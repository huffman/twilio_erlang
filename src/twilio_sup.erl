-module(twilio_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Twilio      = {twilio_web,
                   {twilio_web, start, []},
                   permanent,
                   2000,
                   worker,
                   [twilio_web]},
    PhoneCall = {phonecall_sup,
                    {phonecall_sup, start_link, []},
                    permanent,
                    infinity,
                    supervisor,
                    [phonecall_sup]},
    {ok, {{one_for_one, 1, 10}, [Twilio, PhoneCall]}}.

