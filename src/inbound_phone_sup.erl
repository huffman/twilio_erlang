%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       Inbound phone supervisor for handling incomging calls
%%%
%%% @end
%%% Created :  6 Mar 2012 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(inbound_phone_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0,
         answer_phone/1
         %call_in_progress/2
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec answer_phone(string()) -> pid() | string().
answer_phone(CallId) ->
    ChildSpec = gen_child_spec(CallId),
    case supervisor:start_child({local, CallId}, ChildSpec) of
        {ok, Pid}                       -> Pid;
        {error, {already_started, Pid}} -> Pid;
        Else                            -> Else
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) -> {ok,{{one_for_one,1,30}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
gen_child_spec(S) ->
    {S, {inbound_phone_srv, start_link, [S]},
     transient, 2000, worker, dynamic}.
