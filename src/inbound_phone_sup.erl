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
         start_link/1,
         answer_phone/2
         %call_in_progress/2
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec answer_phone(string(), string()) -> pid() | string().
answer_phone(Site, CallId) ->
    ChildSpec = gen_child_spec(CallId),
    Id = hn_util:site_to_atom(Site, "_inbound_phone"),
    case supervisor:start_child({global, Id}, ChildSpec) of
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
start_link(Site) ->
    case application:get_env(hypernumbers, startup_debug) of
        {ok, true} -> io:format("...starting inbound_phone_srv for ~p~n",
                                [Site]);
        _Other     -> ok
    end,
    Id = hn_util:site_to_atom(Site, "_inbound_phone"),
    supervisor:start_link({global, Id}, ?MODULE, []).

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
    {S, {inbound_phone_fsm, start_link, [S]},
     transient, 2000, worker, dynamic}.
