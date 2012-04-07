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
                history = []}).

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
    io:format("Starting inbound_phone_srv for ~p~n-with ~p~n",
              [Params, TwiML]),
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
    io:format("In inbound_phone_srv:init~n"),
    {ok, #state{twiml_ext = TwiML_EXT, initial_params = Params,
               fsm = FSM, history = [{init, Params}]}}.

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
    case Request of
        {call_complete, Rec} ->
            % we do nothing, but you might want to squirrell away the
            % duration for bill purposes
            spawn(timer, apply_after, [1000, inbound_phone_srv, delete_self,
                                       [Rec#twilio.call_sid]]);
        {Other, _Rec} ->
            io:format("Got ~p call in inbound_phone_srv~n", [Other])
    end,
    Reply = ok,
    {reply, Reply, State}.

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
