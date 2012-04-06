-module(twilio_app).

-behaviour(application).

-export([init/0]).

%% Application callbacks
-export([start/2, stop/1]).

init() -> application:start(twilio).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    [application:start(A) || A <- [inets, crypto, public_key, ssl]],
    twilio_sup:start_link().

stop(_State) ->
    ok.
