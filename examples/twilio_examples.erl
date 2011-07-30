%%%-------------------------------------------------------------------
%%% @author Ryan Huffman <ryanhuffman@gmail.com>
%%% @copyright 2011, Ryan Huffman
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(twilio_examples).

-export([start/0]).
-export([example_call/1, twilio_spawnfest_sms/2, twilio_spawnfest/2]).

-include("twilio.hrl").

start() ->
    application:start(twilio).

%% @doc Makes an example call that will use the router
%% `twilio_rt_examples'.
example_call(To) ->
    AccountSID = "AC82a27c43c50e5f2716b49c7735bf4e01",
    AuthToken = "78e4c455045fd557974295a512ab9efa",
    From = "4154130718",
    Params = [{"Url", "http://huffman.spawnfest.com:8080/examples"},
        {"FallbackUrl", "http://huffman.spawnfest.com:8080/error"}],
    twilio:make_call(AccountSID, AuthToken, From, To, Params).

twilio_spawnfest_sms(_Path, _Params) ->
    [#sms{text=example_utils:spawnfest_complete_string()}].
    
twilio_spawnfest(_Path, _Params) ->
    [#say{text=example_utils:spawnfest_complete_string()}].
    

