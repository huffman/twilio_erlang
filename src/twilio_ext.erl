%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       Examples of using extended twiml
%%%
%%% @end
%%% Created :  23 Mar 2012 by gordon@hypernumbers.com

-module(twilio_ext).

-export([
         handle/1,
         get_twiml_ext/1
        ]).

% debugging exports
-export([
         debug/0,
         debug2/0,
         hangup/1,
         recording/0,
         log_terms/2
        ]).

-include("twilio.hrl").
-include("twilio_web.hrl").

handle(Params) ->
    log_terms(Params, "twilio.params.log"),
    Records = twilio_web_util:process_proplist(Params),
    case Records#twilio.call_status of
        "ringing" ->
            io:format("phone ringing...~n"),
            {A, B, C} = now(),
            random:seed(A, B, C),
            _N = random:uniform(9),
            TwiML_EXT = twilio_ext:get_twiml_ext(9),
            inbound_phone_sup:answer_phone(Records, TwiML_EXT);
        "completed" ->
            case Records#twilio.recording of
                null ->
                    io:format("call completed...~n"),
                    ok = inbound_phone_sup:call_complete(Records),
                    ok;
                _ ->
                    io:format("notification of recording...~n"),
                    ok = inbound_phone_sup:recording_notification(Records),
                    ok
            end;
        Other ->
            io:format("Other with ~p~n", [Other]),
             "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Response>"
                ++ "<Say>banjo</Say></Response>"
    end.

get_twiml_ext(N) ->
    TwiML = get_twiml2(N),
    io:format("TwiML is ~p~n", [TwiML]),
    case twiml:is_valid(TwiML) of
        false -> exit("invalid TwiML");
        true -> TwiML
    end.

get_twiml2(1) -> [#say{text = "yowza"}];
get_twiml2(2) -> [#say{text = "bonza, dogface",
                       language = "fr",
                       voice = "woman"},
                  #say{text = "now piss aff!"}];
get_twiml2(3) -> [#say{text = "sorting out this"},
                  #sms{text = "sending Gordon an SMS message",
                       to = "+447776251669",
                       from = "+441315101897"}];
get_twiml2(4) -> [#say{text = "hot diggity",
                       language = "de",
                       voice = "woman"},
                  #pause{length = 10},
                  #say{text = "lover boy!",
                       voice= "woman"}];
get_twiml2(5) -> [#play{url = "http://files.hypernumbers.com/music/"
                        ++ "RockyMountainMedleyPart1.mp3"}];
get_twiml2(6) -> [#say{text = "forwarding to Gordon"},
                  #dial{body = [#number{number = "+447776251669"}]}];
get_twiml2(7) -> [#say{text="welcome to a simple conference call. "
                       ++ "Have someone else call this number"},
                  #dial{body = [#conference{muted = false, beep = true,
                                            startConferenceOnEnter = true,
                                            endConferenceOnExit = true,
                                            conference = "bingo master"}]}];
get_twiml2(8) -> [#say{text = "leave a message after the tone"},
                  #record{playBeep = true, maxLength = 60}];
get_twiml2(9) -> SAY1 = #say{text="erken"},
                 RESPONSE1 = #response_EXT{title = "erken", response = "1",
                                           body = [SAY1]},
                 SAY2 = #say{text="jerken"},
                 RESPONSE2 = #response_EXT{title = "jerken", response = "2",
                                           body = [SAY2]},
                 [#gather{numDigits = "1", autoMenu_EXT = true,
                          after_EXT = [RESPONSE1, RESPONSE2]}].

log_terms(Terms, File) ->
    Str = lists:flatten(io_lib:format("~p.~n", [Terms])),
    _Return = filelib:ensure_dir(File),
    case file:open(File, [append]) of
        {ok, Id} ->
            io:fwrite(Id, "~s~n", [Str]),
            file:close(Id);
        _ ->
            error
    end.

debug() ->

    Params = [{"AccountSid","AC7a076e30da6d49119b335d3a6de43844"},
              {"ToZip",[]},
              {"FromState",[]},
              {"Called","+441315101897"},
              {"FromCountry","GB"},
              {"CallerCountry","GB"},
              {"CalledZip",[]},
              {"Direction","inbound"},
              {"FromCity",[]},
              {"CalledCountry","GB"},
              {"CallerState",[]},
              {"CallSid","CAb78d22107b73765976c1d8b3eafcb4d8"},
              {"CalledState","Edinburgh"},
              {"From","+447776251669"},
              {"CallerZip",[]},
              {"FromZip",[]},
              {"CallStatus","ringing"},
              {"ToCity",[]},
              {"ToState","Edinburgh"},
              {"To","+441315101897"},
              {"ToCountry","GB"},
              {"CallerCity",[]},
              {"ApiVersion","2010-04-01"},
              {"Caller","+447776251669"},
              {"CalledCity",[]}],
    handle(Params).

debug2() ->

    {A, B, C} = now(),
    CallSID = integer_to_list(A) ++ integer_to_list(B) ++ integer_to_list(C),
    io:format("Spoofing a call with CallSID of ~p~n", [CallSID]),
    Params = [{"AccountSid","AC7a076e30da6d49119b335d3a6de43844"},
              {"ToZip",[]},
              {"FromState",[]},
              {"Called","+441315101897"},
              {"FromCountry","GB"},
              {"CallerCountry","GB"},
              {"CalledZip",[]},
              {"Direction","inbound"},
              {"FromCity",[]},
              {"CalledCountry","GB"},
              {"CallerState",[]},
              {"CallSid", CallSID},
              {"CalledState","Edinburgh"},
              {"From","+447776251669"},
              {"CallerZip",[]},
              {"FromZip",[]},
              {"CallStatus","ringing"},
              {"ToCity",[]},
              {"ToState","Edinburgh"},
              {"To","+441315101897"},
              {"ToCountry","GB"},
              {"CallerCity",[]},
              {"ApiVersion","2010-04-01"},
              {"Caller","+447776251669"},
              {"CalledCity",[]}],
    handle(Params).

hangup(CallSID) ->
    Params = [{"AccountSid","AC7a076e30da6d49119b335d3a6de43844"},
              {"ToZip",[]},
              {"FromState",[]},
              {"Called","+441315101897"},
              {"FromCountry","GB"},
              {"CallerCountry","GB"},
              {"CalledZip",[]},
              {"Direction","inbound"},
              {"FromCity",[]},
              {"CalledCountry","GB"},
              {"Duration","1"},
              {"CallerState",[]},
              {"CallSid",CallSID},
              {"CalledState","Edinburgh"},
              {"From","+447776251669"},
              {"CallerZip",[]},
              {"FromZip",[]},
              {"CallStatus","completed"},
              {"ToCity",[]},
              {"ToState","Edinburgh"},
              {"To","+441315101897"},
              {"CallDuration","1"},
              {"ToCountry","GB"},
              {"CallerCity",[]},
              {"ApiVersion","2010-04-01"},
              {"Caller","+447776251669"},
              {"CalledCity",[]}],
    handle(Params).

recording() ->
    {A, B, C} = now(),
    CallSID = integer_to_list(A) ++ integer_to_list(B) ++ integer_to_list(C),
    io:format("Spoofing a call with CallSID of ~p~n", [CallSID]),
    Params1 = [{"AccountSid","AC7a076e30da6d49119b335d3a6de43844"},
              {"ToZip",[]},
              {"FromState",[]},
              {"Called","+441315101897"},
              {"FromCountry","GB"},
              {"CallerCountry","GB"},
              {"CalledZip",[]},
              {"Direction","inbound"},
              {"FromCity",[]},
              {"CalledCountry","GB"},
              {"CallerState",[]},
              {"CallSid", CallSID},
              {"CalledState","Edinburgh"},
              {"From","+447776251669"},
              {"CallerZip",[]},
              {"FromZip",[]},
              {"CallStatus","ringing"},
              {"ToCity",[]},
              {"ToState","Edinburgh"},
              {"To","+441315101897"},
              {"ToCountry","GB"},
              {"CallerCity",[]},
              {"ApiVersion","2010-04-01"},
              {"Caller","+447776251669"},
              {"CalledCity",[]}],
    handle(Params1),

    Params2 = [{"AccountSid","AC7a076e30da6d49119b335d3a6de43844"},
              {"ToZip",[]},
              {"FromState",[]},
              {"Called","+441315101897"},
              {"FromCountry","GB"},
              {"CallerCountry","GB"},
              {"CalledZip",[]},
              {"Direction","inbound"},
              {"FromCity",[]},
              {"CalledCountry","GB"},
              {"CallerState",[]},
              {"CallSid", CallSID},
              {"CalledState","Edinburgh"},
              {"From","+447776251669"},
              {"CallerZip",[]},
              {"FromZip",[]},
              {"CallStatus","completed"},
              {"ToCity",[]},
              {"ToState","Edinburgh"},
              {"RecordingUrl",
               "http://api.twilio.com/2010-04-01/Accounts/AC7a076e30da6d49119b335d3a6de43844/Recordings/REf11666b40c423726fcf26f15635373b6"},
              {"To","+441315101897"},
              {"Digits","hangup"},
              {"ToCountry","GB"},
              {"RecordingDuration","3"},
              {"CallerCity",[]},
              {"ApiVersion","2010-04-01"},
              {"Caller","+447776251669"},
              {"CalledCity",[]},
              {"RecordingSid","REf11666b40c423726fcf26f15635373b6"}],
    handle(Params2).
