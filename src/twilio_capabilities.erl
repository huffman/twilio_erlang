%%%-------------------------------------------------------------------
%%% @author Ryan Huffman <ryanhuffman@gmail.com>
%%% @copyright 2012, Ryan Huffman
%%% @doc This module generate JSON-JWT Twilio Capability Tokens.
%%% At the time of this writing there doesn't appear to be documentation
%%% from Twilio other than the code they've written for their
%%% own official libraries.  This implementation is based off of those.
%%% The protocol may be subject to change, and thus this implementation
%%% may break.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(twilio_capabilities).

-export([generate/4]).

-type auth_token()     :: string().
-type account_sid()     :: string().
-type application_sid() :: string().
-type client_name()     :: string().
-type stream_arg()      :: {string(), string()}.
-type capability()      :: {client_incoming, client_name()}
                           | {client_outgoing, application_sid()}
                           | {event_stream, [stream_arg()]}.
-type capability_opt()  :: {expires_after, integer()}.

%% @doc Generates a twilio capabilities token.
-spec generate(account_sid(), auth_token(), [capability()], [capability_opt()]) -> binary().
generate(AccountSID, AuthToken, Capabilities, Opts) ->
    % Pull out the client name to be used for other capabilities.  Based
    % on what's seen in other implementations, this parameter needs
    % to be added to client_outgoing requests as well.
    ClientName = proplists:get_value(client_incoming, Capabilities),

    ScopeStrings = [capability_to_scope_string(Cap, ClientName) || Cap <- Capabilities],

    FullScopeString = string:join(ScopeStrings, " "),

    Claims = [
        {"scope", list_to_binary(FullScopeString)},
        {"iss", list_to_binary(AccountSID)},
        {"exp", get_expiration(Opts)}
    ],

    jsonjwt:encode(Claims, AuthToken, "HS256").

%% @doc Generates the expiration date for a token.
get_expiration(Opts) ->
    ExpiresAfter = proplists:get_value(expires_after, Opts, 3600),
    {Megaseconds, Seconds, _Microseconds} = erlang:now(),
    (Megaseconds * 1000000) + Seconds + ExpiresAfter.

%% @doc Formats a capability as a scope string.
capability_to_scope_string({client_incoming, ClientName}, _ClientName) ->
    build_scope_string("client", "incoming", [{"clientName", ClientName}]);
capability_to_scope_string({client_outgoing, ApplicationSID}, ClientName) ->
    case ClientName of
        undefined ->
            Params = [];
        _ ->
            Params = [{"clientName", ClientName}]
    end,
    build_scope_string("client", "outgoing", [{"appSid", ApplicationSID} | Params]);
capability_to_scope_string({event_stream, Params}, _ClientName) ->
    build_scope_string("stream", "subscribe", [{"path", "/2010-04-01/Events"} | Params]).
    
%% @doc Builds a scope string of the form `scope:service:privilege?key=value&key2=value2'.
-spec build_scope_string(string(), string(), [{string(), string()}]) -> string().
build_scope_string(Service, Privilege, Params) ->
    ParamsString = 
    case Params of
        [] ->
            "";
        _ ->
            Params2 = [edoc_lib:escape_uri(Key) ++ "=" ++ edoc_lib:escape_uri(Value)
                || {Key, Value} <- Params],
            "?" ++ string:join(Params2, "&")
    end,

    "scope:" ++ Service ++ ":" ++ Privilege ++ ParamsString.



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

build_scope_string_test() ->
    ?assertEqual(
        "scope:someService:somePrivilege",
        build_scope_string("someService", "somePrivilege", [])),
    ?assertEqual(
        "scope:someService:somePrivilege?KEY=vAlue",
        build_scope_string("someService", "somePrivilege", [{"KEY", "vAlue"}])),
    ?assertEqual(
        "scope:someService:somePrivilege?KEY=vAlue&key2=value2",
        build_scope_string("someService", "somePrivilege",
            [{"KEY", "vAlue"}, {"key2", "value2"}])),
    ok.

capability_to_scope_string_test() ->
    ?assertEqual(
        "scope:client:incoming?clientName=CLIENT", 
        capability_to_scope_string({client_incoming, "CLIENT"}, undefined)),
    ?assertEqual(
        "scope:client:incoming?clientName=CLIENT%3c", 
        capability_to_scope_string({client_incoming, "CLIENT<"}, "asdfasdf")),
    ?assertEqual(
        "scope:client:outgoing?appSid=1234&clientName=CLIENT%3c", 
        capability_to_scope_string({client_outgoing, "1234"}, "CLIENT<")),
    ?assertEqual(
        "scope:client:outgoing?appSid=1234&clientName=CLIENT%3c", 
        capability_to_scope_string({client_outgoing, "1234"}, "CLIENT<")),
    ?assertEqual(
        "scope:client:outgoing?appSid=1234",
        capability_to_scope_string({client_outgoing, "1234"}, undefined)),
    ?assertEqual(
        "scope:stream:subscribe?path=%2f2010-04-01%2fEvents",
        capability_to_scope_string({event_stream, []}, undefined)),
    ?assertEqual(
        "scope:stream:subscribe?path=%2f2010-04-01%2fEvents&channel=5",
        capability_to_scope_string({event_stream, [{"channel", "5"}]}, undefined)),
    ok.

-endif.


