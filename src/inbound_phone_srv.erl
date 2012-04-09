%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       A home-brew finite state machine
%%%            for handling calls
%%%
%%% @end
%%% Created :  6 Apr 2012 by <gordon@hypernumbers.com>
%%%-------------------------------------------------------------------
-module(inbound_phone_srv).

-behaviour(gen_server).

%% API
-export([
         start_link/2
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

-record(state, {twiml_ext = null, initial_params = null, fsm = null,
               currentstate = "1", history = []}).

%%%===================================================================
%%% API
%%%===================================================================
delete_self(CallSID) ->
    ok = supervisor:terminate_child(inbound_phone_sup, CallSID),
    ok = supervisor:delete_child(inbound_phone_sup, CallSID),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Params, TwiML) ->
    gen_server:start_link(?MODULE, [Params, TwiML], []).

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
init([Params, TwiML_EXT]) ->
    FSM = case twiml:is_valid(TwiML_EXT) of
                   false ->
                       Tw = [#say{text = "sorry, this call has been setup "
                                  ++ "incorrectly"}],
                      twiml:compile(Tw);
                   true ->
                       twiml:compile(TwiML_EXT)
               end,
    {ok, #state{twiml_ext = TwiML_EXT, initial_params = Params,
               fsm = FSM, history = [{init, now(), Params}]}}.

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
                   % we do nothing, but you might want to squirrell away the
                   % duration for bill purposes
                   spawn(timer, apply_after, [1000, inbound_phone_srv,
                                              delete_self,
                                              [Rec#twilio.call_sid]]),
                   {ok, State};
               {start_call, Rec} ->
                   execute(State, {start_call, now(), Rec});
              {recording_notification, _Params} ->
                  io:format("Got details of a recording that has been made. "
                            ++ "You should do something with it mebbies?~n"),
                  {ok, State};
              {gather_response, Rec} ->
                 respond(State, Rec);
              {Other, _Rec} ->
                   io:format("Got ~p call in inbound_phone_srv~n", [Other]),
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
    #state{history = Hist} = State,
    {NewS, Reply} = exec2(next, State, Action, []),
    {Reply, State#state{currentstate = NewS,
                       history = [Action | Hist]}}.

exec2(wait, State, _Action, Acc) ->
    #state{currentstate = CS} = State,
    Msg = lists:flatten(lists:reverse(Acc)),
    Reply = "<?xml version=\"1.0\"?><Response>"
        ++ Msg ++ "</Response>",
    {CS, Reply};
exec2(next, State, Action, Acc) ->
    #state{currentstate = CS, fsm = FSM} = State,
    io:format("In exec2 FSM is ~p~nCS is ~p~n", [FSM, CS]),
    case lists:keyfind(CS, 1, FSM) of
        false ->
            exit("invalid state in exec2");
        % these are the terminal clauses
        {CS, {xml, X}, exit} ->
            Reply = wrap(lists:reverse([X | Acc])),
            {CS, Reply};
        {CS, {xml, X}, wait} ->
            Reply = wrap(lists:reverse([X | Acc])),
            {CS, Reply};
        {CS, {xml, X}, next} ->
            Next = get_next(CS, FSM, fun twiml:bump/1, fun twiml:unbump/1),
            NewS = State#state{currentstate = Next},
            exec2(next, NewS, Action, [X | Acc]);
        {CS, {xml, X}, into} ->
            Into = get_next(CS, FSM, fun incr/1, fun twiml:decr/1),
            NewS = State#state{currentstate = Into},
            exec2(next, NewS, Action, [X | Acc]);
        {CS, {xml, X}, repeat} ->
            Into = get_next(CS, FSM, fun twiml:unbump/1, fun twiml:decr/1),
            NewS = State#state{currentstate = Into},
            exec2(next, NewS, Action, [X | Acc]);
        {CS, #response_EXT{}, into} ->
            InProg = Action#twilio.inprogress,
            #twilio_inprogress{digits = D} = InProg,
            match(State, Action, D, Acc);
        {_CS, #goto_EXT{}, _Goto}  -> exit("fix me...")
    end.

get_next(CS, FSM, Fun1, Fun2) ->
    Next = Fun1(CS),
    case lists:keyfind(Next, 1, FSM) of
        false ->
            Next2 = Fun2(CS),
            case lists:keyfind(Next2, 1, FSM) of
                false         -> exit("invalid state in get_next");
                {State, _, _} -> State
            end;
        {State2, _, _} -> State2
    end.

% the twiml compiler passes in a new state like "1.0" and then increments it
% on the pass - we don't, so we have to increment and bump in a oner
incr(CS) -> twiml:bump(twiml:incr(CS)).

wrap(List) -> lists:flatten(lists:append(["<?xml version=\"1.0\"?><Response>",
                                          List, "</Response>"])).

respond(State, Rec) ->
    #state{currentstate = CS, fsm = FSM, history = _Hist} = State,
    case lists:keyfind(CS, 1, FSM) of
        false         -> exit("invalid state in respond");
        {CS, _, wait} -> NewCS = get_next(CS, FSM, fun twiml:bump/1,
                                          fun twiml:unbump/1),
                         NewS = State#state{currentstate = NewCS},
                         execute(NewS, Rec)
    end.

match(State, Action, D, Acc) ->
    #state{currentstate = CS, fsm = FSM} = State,
    case lists:keyfind(CS, 1, FSM) of
        false ->
            exit("invalid state in match");
        {CS, #response_EXT{response = R} = _Resp, _} ->
            io:format("D is ~p R is ~p~n", [D, R]),
            case D of
                R -> NewCS = get_next(CS, FSM, fun incr/1,
                                      fun twiml:decr/1),
                     NewS = State#state{currentstate = NewCS},
                     exec2(next, NewS, Action, []);
               _  -> NewCS = get_next(CS, FSM, fun twiml:bump/1,
                                      fun twiml:umbump/1),
                     NewS = State#state{currentstate = NewCS},
                     match(NewS, Action, D, Acc)
            end
    end.
