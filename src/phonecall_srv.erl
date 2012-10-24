%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       A home-brew finite state machine
%%%            for handling calls
%%%
%%% @end
%%% Created :  6 Apr 2012 by <gordon@hypernumbers.com>
%%%-------------------------------------------------------------------
-module(phonecall_srv).

-behaviour(gen_server).

-record(contact_log,
        {
          from     = [],
          to       = [],
          call_sid = [],
          type     = []
         }).

%% API
-export([
         start_link/3
        ]).

% utilities for using state stuff
-export([
         get_log/1,
         get_hypertag/1,
         get_site/1
        ]).

% export for tidying up
-export([
         delete_self/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("twilio.hrl").
-include("twilio_web.hrl").
-include("phonecall_srv.hrl").

%%%===================================================================
%%% API
%%%===================================================================
delete_self(CallSID) ->
    ok = supervisor:terminate_child(phonecall_sup, CallSID),
    ok = supervisor:delete_child(phonecall_sup, CallSID).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Type, Params, TwiML) ->
    gen_server:start_link(?MODULE, [Type, Params, TwiML], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Type, Params, TwiML_EXT]) ->
    FSM = case twiml:is_valid(TwiML_EXT) of
              false ->
                  Resp = twiml:validate(TwiML_EXT),
                  io:format("TwiML_EXT is ~p~n-~p~n", [TwiML_EXT, Resp]),
                  Tw = [#say{text = "sorry, this call has been setup "
                                  ++ "incorrectly"}],
                  orddict:from_list(twiml:compile(Tw));
              true ->
                  orddict:from_list(twiml:compile(TwiML_EXT))
          end,
    io:format("About to make log~n"),
    Log = make_log(Type, Params),
    io:format("Log is ~p~n", [Log]),
    {ok, #pc_state{twiml_ext = TwiML_EXT, initial_params = Params,
               log = Log, fsm = FSM, history = [{init, now(), Params}]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
    {Reply, NewS}
        = case Request of
               {call_complete, Rec} ->
                  % we do nothing, but you might want to squirrel away the
                  % duration for billing purposes
                  % apply the completion callback fns
                  ok = run_callbacks(Rec, State, completion),
                  spawn(timer, apply_after, [1000, phonecall_srv,
                                             delete_self,
                                             [Rec#twilio.call_sid]]),
                  {ok, State};
              {start_call, _Type, Rec, Callbacks} ->
                  {R, NS} = execute(State, {start_call, now(), Rec}),
                  #pc_state{eventcallbacks = ECB} = NS,
                  {R, NS#pc_state{eventcallbacks = lists:merge(ECB, Callbacks)}};
              {recording_notification, Rec} ->
                  % apply the recording callback fns
                  ok = run_callbacks(Rec, State, recording),
                  {ok, State};
              {gather_response, Rec} ->
                 respond(State, Rec);
              {goto_state, Rec, Goto} ->
                  NewState = State#pc_state{currentstate = Goto},
                  execute(NewState, {"goto " ++ Goto, now(), Rec});
              {Other, _Rec} ->
                   io:format("Got ~p call in phonecall_srv~n", [Other]),
                   {ok, State}
           end,
    {reply, Reply, NewS}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
execute(State, Action) ->
    #pc_state{history = Hist} = State,
    {NewCS, NewState, Msg} = exec2(next, State, Action, []),
    Reply = "<?xml version=\"1.0\"?><Response>"
        ++ lists:flatten(Msg) ++ "</Response>",
    NewState2 = NewState#pc_state{currentstate = NewCS,
                               history = [Action | Hist]},
    {Reply, NewState2}.

exec2(wait, State, _Action, Acc) ->
    #pc_state{currentstate = CS} = State,
    Msg = lists:flatten(lists:reverse(Acc)),
    {CS, State, Msg};
exec2(next, State, Action, Acc) ->
    #pc_state{currentstate = CS, fsm = FSM} = State,
    case orddict:find(CS, FSM) of
        error ->
            exit("invalid state in exec2");
        % these are the terminal clauses
        {ok, {{xml, X}, exit}} ->
            Reply = lists:reverse([X | Acc]),
            {CS, State, Reply};
        {ok, {{xml, X}, gather}} ->
            NewCS = get_next(CS, FSM, fun twiml:bump/1,
                             fun twiml:unbump_on/1),
            NewS = State#pc_state{currentstate = NewCS},
            {_, Default} = get_next_default(NewS, Action, []),
            Reply = lists:reverse([Default, X | Acc]),
            {CS, State, Reply};
        {ok, {{xml, X}, wait}} ->
            Reply = lists:reverse([X | Acc]),
            {CS, State, Reply};
        {ok, {{xml, X}, next}} ->
            Next = get_next(CS, FSM, fun twiml:bump/1, fun twiml:unbump_on/1),
            NewS = State#pc_state{currentstate = Next},
            exec2(next, NewS, Action, [X | Acc]);
        {ok, {{xml, X}, into}} ->
            Into = get_next(CS, FSM, fun twiml:incr/1, fun twiml:bump/1),
            NewS = State#pc_state{currentstate = Into},
            exec2(next, NewS, Action, [X | Acc]);
        {ok, {#response_EXT{}, into}} ->
            InProg = Action#twilio.inprogress,
            #twilio_inprogress{digits = D} = InProg,
            match(State, Action, D, Acc);
        {ok, {#function_EXT{module = M, fn = F}, next}} ->
            NewState = apply_function(M, F, State),
            % just execute the new FSM
            exec2(next, NewState, Action, []);
         {ok, {#goto_EXT{}, Goto}} ->
            {Goto, State, "<Redirect>./" ++ Goto ++ "</Redirect>"}
    end.

get_next(CS, FSM, Fun1, Fun2) ->
    Next = Fun1(CS),
    case orddict:find(Next, FSM) of
        error ->
            Next2 = Fun2(CS),
            case orddict:find(Next2, FSM) of
                error  -> exit("invalid state in get_next");
                {ok, {_, _}} -> Next2
            end;
        {ok, {_, _}} -> Next
    end.

respond(State, Rec) ->
    #pc_state{currentstate = CS, fsm = FSM} = State,
    case orddict:find(CS, FSM) of
        error ->
            exit("invalid state in respond");
        {ok, {_, Type}} when Type == wait orelse Type == gather ->
            NewCS = get_next(CS, FSM, fun twiml:bump/1,
                             fun twiml:unbump_on/1),
            NewS = State#pc_state{currentstate = NewCS},
            execute(NewS, Rec)
    end.

match(State, Action, D, Acc) ->
    #pc_state{currentstate = CS, fsm = FSM} = State,
    case orddict:find(CS, FSM) of
        error ->
            exit("invalid state in match");
        {ok, {#response_EXT{response = R} = _Resp, _}} ->
            case D of
                R -> NewCS = get_next(CS, FSM, fun twiml:incr/1,
                                      fun twiml:bump/1),
                     NewS = State#pc_state{currentstate = NewCS},
                     exec2(next, NewS, Action, []);
                _ -> NewCS = get_next(CS, FSM, fun twiml:bump/1,
                                      fun twiml:unbump_on/1),
                     NewS = State#pc_state{currentstate = NewCS},
                     match(NewS, Action, D, Acc)
            end
    end.

get_next_default(State, Action, Acc) ->
    #pc_state{currentstate = CS, fsm = FSM} = State,
    case orddict:find(CS, FSM) of
        error ->
            exit("invalid state in get_next_default");
        {ok, {#default_EXT{}, _}} ->
            NewCS = get_next(CS, FSM, fun twiml:incr/1,
                             fun twiml:bump/1),
            NewS = State#pc_state{currentstate = NewCS},
            {NewCS2, _NewState2, Msg} = exec2(next, NewS, Action, []),
            {NewCS2, Msg};
        {ok, {#goto_EXT{}, Goto}} ->
            {Goto, "<Redirect>/" ++ Goto ++ "</Redirect>"};
        _Other ->
            NewCS = get_next(CS, FSM, fun twiml:bump/1,
                             fun twiml:unbump_on/1),
            NewS = State#pc_state{currentstate = NewCS},
            get_next_default(NewS, Action, Acc)
    end.

apply_function(Module, Function, State) ->
    #pc_state{currentstate = CS, fsm = FSM, history = History,
          eventcallbacks = CBs} = State,
    NewHist = [{"calling " ++ to_list(Module) ++ ":" ++ to_list(Function)
                ++ "/3"} | History],
    {NewTwiML, CBs2} = apply(to_atom(Module), to_atom(Function), [State]),
    {BumpedState, NewCState} = twiml:compile(NewTwiML, fsm, CS),
    NewFSM1  = orddict:erase(CS, FSM),
    NewFSM2  = shift(NewFSM1, CS, BumpedState, []),
    NewFSM3  = store(NewCState, NewFSM2),
    NewCBs   = lists:merge(CBs, CBs2),
    NewState = State#pc_state{fsm = NewFSM3, history = NewHist,
                              eventcallbacks = NewCBs},
    NewState.

store([], Orddict)           -> Orddict;
store([{K, V} | T], Orddict) -> NewOrddict = orddict:store(K, V, Orddict),
                                store(T, NewOrddict).

to_atom(X) when is_atom(X) -> X;
to_atom(X) when is_list(X) -> list_to_existing_atom(X).

to_list(X) when is_list(X) -> X;
to_list(X) when is_atom(X) -> atom_to_list(X).

shift([], _, _, Acc) -> lists:reverse(Acc);
shift([{State, Rec} | T], CurrentState, BumpedState, Acc) ->
    NewState = shift2(State, CurrentState, BumpedState),
    Rec2 = shift_rec(Rec, CurrentState, BumpedState),
    NewAcc = [{NewState, Rec2} | Acc],
    shift(T, CurrentState, BumpedState, NewAcc).

shift2(State, CurrentState, BumpedState) ->
    if
        State >  CurrentState -> bump(State, CurrentState, BumpedState);
        State =< CurrentState -> State
    end.

shift_rec({#goto_EXT{}, Goto}, CS, BS) ->
    NewGoto = shift2(Goto, CS, BS),
    {#goto_EXT{goto = NewGoto}, NewGoto};
shift_rec(Rec,  _, _) ->
    Rec.

bump(State, CurrentState, BumpedState) ->
    Diff = diff(CurrentState, BumpedState),
    NState = state_to_num(State),
    LenD = length(Diff),
    LenNS = length(NState),
    case LenD of
        LenNS -> bump2(NState, Diff, []);
        _     -> State
    end.

bump2([], [], Acc) ->
    Cca = lists:reverse(Acc),
    string:join([integer_to_list(X) || X <- Cca], ".");
bump2([H1 | T1], [H2 | T2], Acc) ->
    bump2(T1, T2, [H1 + H2 | Acc]).

diff(A, B) -> [A2 | A3]  = lists:reverse(state_to_num(A)),
              [B2 | _B3] = lists:reverse(state_to_num(B)),
              Len = length(A3),
              lists:reverse([B2 - A2 - 1 | lists:duplicate(Len, 0)]).

state_to_num(A) -> [list_to_integer(X) || X <- string:tokens(A, ".")].

run_callbacks(Rec, State, Event) ->
    Callbacks = State#pc_state.eventcallbacks,
    [Fun(Rec, State) || {Ev, Fun} <- Callbacks, Ev == Event],
    ok.

make_log(Type, #twilio{} = Rec) ->
    From = Rec#twilio.from,
    To = Rec#twilio.to,
    F1 = case From of
             null -> "no from yet";
             _    -> From#twilio_from.number
         end,
    T1 = case To of
             null -> "no to yet";
             _    -> To#twilio_to.number
         end,
    #contact_log{type = Type, call_sid = Rec#twilio.call_sid,
                 from = F1, to = T1}.

get_log(#pc_state{} = State) -> State#pc_state.log.

get_hypertag(#pc_state{} = State) -> get_(State, "hypertag").

get_site(#pc_state{} = State) -> get_(State, "site").

get_(State, Key) ->
    InitParam = State#pc_state.initial_params,
    case proplists:lookup(Key, InitParam#twilio.custom_params) of
        none       -> none;
        {Key, Val} -> Val
    end.
