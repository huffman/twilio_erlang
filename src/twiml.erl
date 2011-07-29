%%%-------------------------------------------------------------------
%%% @author Ryan Huffman <ryanhuffman@gmail.com>
%%% @copyright 2011,
%%% @doc Encodes twilio records into an XML document.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(twiml).

-export([encode/1]).

-include("twilio.hrl").

%% @doc Encodes a set of twiml records as an XML document.
encode(Elements) ->
    Content = [{'Response', [], [to_xmerl_element(El) || El <- Elements]}],
    xmerl:export_simple(Content, xmerl_xml).

%% @doc Converts a twiml record to an xmerl XML element.
%% @TODO Conference support for #dial
%% @TODO Secondary verbs: hangup, reject
to_xmerl_element(#say{} = Say) ->
    Attrs = [{voice, Say#say.voice},
             {language, Say#say.language},
             {loop, Say#say.loop}],
    CleanAttrs = remove_undefined(Attrs),
    {'Say', CleanAttrs, [Say#say.text]};
to_xmerl_element(#play{} = Play) ->
    Attrs = [{loop, Play#play.loop}],
    CleanAttrs = remove_undefined(Attrs),
    {'Play', CleanAttrs, [Play#play.url]};
to_xmerl_element(#gather{} = Gather) ->
    Attrs = [{action, Gather#gather.action},
             {method, Gather#gather.method},
             {timeout, Gather#gather.timeout},
             {finishOnKey, Gather#gather.finish_on_key},
             {numDigits, Gather#gather.num_digits}],
    CleanAttrs = remove_undefined(Attrs),
    Body = [to_xmerl_element(Element) || Element <- Gather#gather.body],
    {'Gather', CleanAttrs, Body};
to_xmerl_element(#record{} = Record) ->
    Attrs = [{action, Record#record.action},
             {method, Record#record.method},
             {timeout, Record#record.timeout},
             {finishOnKey, Record#record.finish_on_key},
             {maxLength, Record#record.max_length},
             {transcribe, Record#record.transcribe},
             {transcribeCallback, Record#record.transcribe_callback},
             {playBeep, Record#record.play_beep}],
    CleanAttrs = remove_undefined(Attrs),
    {'Record', CleanAttrs, []};
to_xmerl_element(#sms{} = Sms) ->
    Attrs = [{to, Sms#sms.to},
             {from, Sms#sms.from},
             {action, Sms#sms.action},
             {method, Sms#sms.method},
             {statusCallback, Sms#sms.status_callback}],
    CleanAttrs = remove_undefined(Attrs),
    {'Sms', CleanAttrs, [Sms#sms.text]};
to_xmerl_element(#dial{} = Dial) ->
    Attrs = [{action, Dial#dial.action},
             {method, Dial#dial.method},
             {timeout, Dial#dial.timeout},
             {hangupOnStar, Dial#dial.hangup_on_star},
             {timeLimit, Dial#dial.time_limit},
             {callerId, Dial#dial.caller_id}],
    CleanAttrs = remove_undefined(Attrs),

    case is_list(Dial#dial.body) of
        true ->
            Body = [Dial#dial.body];
        false ->
            Body = [to_xmerl_element(Dial#dial.body)]
    end,
    {'Dial', CleanAttrs, Body};
to_xmerl_element(#number{} = Number) ->
    Attrs = [{sendDigits, Number#number.send_digits},
             {url, Number#number.url}],
    CleanAttrs = remove_undefined(Attrs),
    {'Number', CleanAttrs, [Number#number.number]};
to_xmerl_element(#redirect{} = Redirect) ->
    Attrs = [{method, Redirect#redirect.method}],
    CleanAttrs = remove_undefined(Attrs),
    {'Redirect', CleanAttrs, [Redirect#redirect.url]};
to_xmerl_element(#pause{} = Pause) ->
    Attrs = [{length, Pause#pause.length}],
    CleanAttrs = remove_undefined(Attrs),
    {'Pause', CleanAttrs, []}.

%% @doc Removes any undefined attributes.
remove_undefined(Attrs) ->
    [Attr || {_, Value} = Attr <- Attrs, Value =/= undefined].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(XML(D), "<?xml version=\"1.0\"?><Response>"D"</Response>").

encode_test_() ->
    [{"say twiml",
            fun() ->
                    ?assertEqual(
                        ?XML("<Say></Say>"),
                        lists:flatten(encode([#say{}]))),
                    ?assertEqual(
                        ?XML("<Say>Hello!</Say>"),
                        lists:flatten(encode([#say{text="Hello!"}]))),
                    ?assertEqual(
                        ?XML("<Say language=\"en\" loop=\"2\">Hello!</Say>"),
                        lists:flatten(encode([#say{text="Hello!", loop=2, language="en"}])))
            end},
        {"play twiml",
            fun() ->
                    ?assertEqual(
                        ?XML("<Play></Play>"),
                        lists:flatten(encode([#play{}]))),
                    ?assertEqual(
                        ?XML("<Play loop=\"2\"></Play>"),
                        lists:flatten(encode([#play{loop=2}]))),
                    ?assertEqual(
                        ?XML("<Play loop=\"2\">https://someurlhere.com/blah</Play>"),
                        lists:flatten(encode([#play{url="https://someurlhere.com/blah", loop=2}]))),
                    ?assertEqual(
                        ?XML("<Play loop=\"2\">https://someurlhere.com/blah</Play>"),
                        lists:flatten(encode([#play{url="https://someurlhere.com/blah", loop=2}])))
            end},
        {"gather twiml",
            fun() ->
                    ?assertEqual(
                        ?XML("<Gather/>"),
                        lists:flatten(encode([#gather{}]))),
                    ?assertEqual(
                        ?XML("<Gather action=\"eval_gather\"/>"),
                        lists:flatten(encode([#gather{action="eval_gather"}]))),
                    ?assertEqual(
                        ?XML("<Gather action=\"eval_gather\" timeout=\"60\" numDigits=\"10\"/>"),
                        lists:flatten(encode([#gather{action="eval_gather", num_digits=10, timeout=60}]))),
                    ?assertEqual(
                        ?XML("<Gather action=\"eval_gather\"><Say>Hello</Say></Gather>"),
                        lists:flatten(encode([#gather{action="eval_gather", body=[#say{text="Hello"}]}])))
            end},
        {"record twiml",
            fun() ->
                    ?assertEqual(
                        ?XML("<Record/>"),
                        lists:flatten(encode([#record{}]))),
                    ?assertEqual(
                        ?XML("<Record action=\"eval_reCOrd\"/>"),
                        lists:flatten(encode([#record{action="eval_reCOrd"}]))),
                    ?assertEqual(
                        ?XML("<Record action=\"eval_record\" method=\"GET\" timeout=\"10\"/>"),
                        lists:flatten(encode([#record{action="eval_record", timeout=10, method='GET'}])))
            end},
        {"sms twiml",
            fun() ->
                    ?assertEqual(
                        ?XML("<Sms></Sms>"),
                        lists:flatten(encode([#sms{}]))),
                    ?assertEqual(
                        ?XML("<Sms>Hello!</Sms>"),
                        lists:flatten(encode([#sms{text="Hello!"}]))),
                    ?assertEqual(
                        ?XML("<Sms action=\"default/sms\">Hello!</Sms>"),
                        lists:flatten(encode([#sms{text="Hello!", action="default/sms"}])))
            end},
        {"dial twiml",
            fun() ->
                    ?assertEqual(
                        ?XML("<Dial></Dial>"),
                        lists:flatten(encode([#dial{}]))),
                    ?assertEqual(
                        ?XML("<Dial action=\"/do_stuff\">1234833</Dial>"),
                        lists:flatten(encode([#dial{action="/do_stuff", body="1234833"}]))),
                    ?assertEqual(
                        ?XML("<Dial action=\"/do_stuff\"><Number>1234833</Number></Dial>"),
                        lists:flatten(encode([#dial{action="/do_stuff", body=#number{number="1234833"}}]))),
                    ?assertEqual(
                        ?XML("<Dial action=\"/do_stuff\"><Number sendDigits=\"9528\">1234833</Number></Dial>"),
                        lists:flatten(encode([#dial{action="/do_stuff",
                                        body=#number{send_digits="9528", number="1234833"}}])))
            end},
        {"composite twiml",
            fun() ->
                    ?assertEqual(
                        ?XML("<Say>Hello! This is a voice message</Say><Say>This is another message</Say><Dial>48321523</Dial>"),
                        lists:flatten(encode([#say{text="Hello! This is a voice message"},
                                              #say{text="This is another message"},
                                              #dial{body="48321523"}])))
            end},
        {"redirect twiml",
            fun() ->
                    ?assertEqual(
                        ?XML("<Redirect></Redirect>"),
                        lists:flatten(encode([#redirect{}]))),
                    ?assertEqual(
                        ?XML("<Redirect>/do_stuff</Redirect>"),
                        lists:flatten(encode([#redirect{url="/do_stuff"}])))
            end},
        {"pause twiml",
            fun() ->
                    ?assertEqual(
                        ?XML("<Pause/>"),
                        lists:flatten(encode([#pause{}]))),
                    ?assertEqual(
                        ?XML("<Pause length=\"10\"/>"),
                        lists:flatten(encode([#pause{length=10}])))
            end}
    ].

-endif.

