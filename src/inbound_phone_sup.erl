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

-include("twilio.hrl").
-include("twilio_web.hrl").

%% API
-export([
         start_link/0,
         answer_phone/2,
         call_complete/1,
         recording_notification/2,
         gather_response/1,
         goto_state/2
         %call_in_progress/2
        ]).

%% Supervisor callbacks
-export([
         init/1
        ]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec answer_phone(#twilio{}, list()) -> pid() | string().
answer_phone(Params, TwiML_EXT) ->
    ChildSpec = gen_child_spec(Params, TwiML_EXT),
    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok, Pid} ->
            io:format("call started...~n"),
            gen_server:call(Pid, {start_call, Params});
        Other ->
            {error, Other}
    end.

-spec call_complete(#twilio{}) -> pid() | string().
call_complete(Params) ->
    Call = Params#twilio.call_sid,
    Pid = get_pid(Call),
    gen_server:call(Pid, {call_complete, Params}).

-spec recording_notification(#twilio{}, list()) -> pid() | string().
recording_notification(Params, Path) ->
    Call = Params#twilio.call_sid,
    Pid = get_pid(Call),
    gen_server:call(Pid, {recording_notification, Params, Path}).

-spec gather_response(#twilio{}) -> pid() | string().
gather_response(Params) ->
    Call = Params#twilio.call_sid,
    Pid = get_pid(Call),
    gen_server:call(Pid, {gather_response, Params}).

-spec goto_state(#twilio{}, list()) -> pid() | string().
goto_state(Params, State) ->
    Call = Params#twilio.call_sid,
    Pid = get_pid(Call),
    gen_server:call(Pid, {goto_state, Params, State}).

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
gen_child_spec(S, TwiML_EXT) ->
    #twilio{call_sid = CallSID} = S,
    {CallSID, {inbound_phone_srv, start_link, [S, TwiML_EXT]},
     transient, brutal_kill, worker, [inbound_phone_srv]}.

get_pid(Call) ->
    Servers = supervisor:which_children(inbound_phone_sup),
    case lists:keyfind(Call, 1, Servers) of
        false             -> exit("server doesn't exist");
        {Call, Pid, _, _} -> case Pid of
                                 undefined -> exit("server has been closed");
                                 Pid       -> Pid
                             end
    end.
