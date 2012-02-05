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
-module(hello_monkey).

-export([build/3, build/0]).


build() ->
    code:add_patha("../deps/erlsha2/ebin"),
    code:add_patha("../deps/mochiweb/ebin"),
    build("/media/sf_virtualbox/twilio/index.html",
                   "AC7a076e30da6d49119b335d3a6de43844",
                   "9248c9a2a25f6914fad9c9fb5b30e69c").

build(FileName, AccountSID, AuthToken) ->
    Token = twilio_capabilities:generate(AccountSID, AuthToken,
                                         [{client_outgoing, "APabe7650f654fc34655fc81ae71caa3ff", []}],
                     [{expires_after, 3600}]),
    io:format("Token is ~p~n", [Token]),
    Page = "<html>" ++
        "<head>" ++
        "<title>Hello Client Monkey 2</title>" ++
        "<script type='text/javascript' src='http://static.twilio.com/libs/twiliojs/1.0/twilio.min.js'></script>" ++
        "<script type='text/javascript' src='https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js'></script>" ++
        "<link href='http://static0.twilio.com/packages/quickstart/client.css' type='text/css' rel='stylesheet' />" ++
        get_script(Token) ++
        "</head>" ++
        "<body>" ++
        "<h1>Twilio Test Page 2</h1>" ++
        "<button class='call' onclick='call();'>Call</button>" ++

        "<div id='log'>Loading pigeons...</div>" ++
        "</body>" ++
        "</html>",
    filelib:ensure_dir(FileName),
    case file:open(FileName, [write]) of
        {ok, Id} ->
            io:fwrite(Id, "~s~n", [Page]);
        Err ->
            Err
    end.

get_script(Token) ->
    Script = "<script type='text/javascript'>~n" ++
        "Twilio.Device.setup('" ++ binary_to_list(Token) ++ "');~n" ++
        "Twilio.Device.ready(function (device) {~n" ++
        "  $('#log').text(\"Ready\");~n"
        "});~n" ++
        "Twilio.Device.error(function (error) {~n" ++
        "  $('#log').text(\"Error: \" + error.message);~n" ++
        "});~n" ++
        "Twilio.Device.connect(function (conn) {~n" ++
        "  $('#log').text(\"Successfully established call\");~n" ++
        "});~n" ++
        "function call() {~n" ++
        "  $('#log').text(\"call fired\");~n" ++
        "  var Status = Twilio.Device.status();" ++
        "  console.log(Status);" ++
        "  var Ret2 = Twilio.Device.connect();~n" ++
        "  console.log(Ret2);" ++
        "  var Status2 = Twilio.Device.status();" ++
        "  console.log(Status2);" ++
        "}~n" ++
        "</script>",
    io_lib:format(Script, []).
