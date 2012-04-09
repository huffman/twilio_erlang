%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       Examples of using extended twiml
%%%
%%% @end
%%% Created :  23 Mar 2012 by gordon@hypernumbers.com

-module(twilio_ext).

-export([
         handle/2
        ]).

% debugging exports
-export([
         debug/0,
         debug2/0,
         hangup/1,
         recording/0,
         gather/0,
         log_terms/2
        ]).

-include("twilio.hrl").
-include("twilio_web.hrl").

handle(Params, Path) ->
    log_terms(Params, "twilio.params.log"),
    Records = twilio_web_util:process_proplist(Params),
    case Records#twilio.call_status of
        "ringing" ->
            io:format("phone ringing...~n"),
            TwiML_EXT = twiml_EXT_recipies:recipe(12),
            inbound_phone_sup:answer_phone(Records, TwiML_EXT);
        "completed" ->
            case Records#twilio.recording of
                null ->
                    case Path of
                        [] ->
                            io:format("call completed...~n"),
                            ok = inbound_phone_sup:call_complete(Records),
                            ok;
                        Sub ->
                            io:format("goto ~p segment of call completed...~n",
                                      [Sub]),
                            % do nothing here
                            ok
                    end;
                _ ->
                    io:format("notification of recording...~n"),
                    ok = inbound_phone_sup:recording_notification(Records, Path),
                    ok
            end;
        "in-progress" ->
            case Path of
                [] ->
                    io:format("response to gather...~n"),
                    inbound_phone_sup:gather_response(Records);
                [State | _] ->
                    io:format("return to state ~p~n", [State]),
                    inbound_phone_sup:goto_state(Records, State)
            end;
        Other ->
            twilio_web_util:pretty_print(Records),
            io:format("Other with ~p~n", [Other]),
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Response>"
                ++ "<Say>banjo</Say></Response>"
    end.

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
    handle(Params, []).

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
    handle(Params, []).

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
    handle(Params, []).

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
    handle(Params1, []),

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
    handle(Params2, []).

gather() ->
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
    handle(Params1, []),

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
               {"CallSid",CallSID},
               {"CalledState","Edinburgh"},
               {"From","+447776251669"},
               {"CallerZip",[]},
               {"FromZip",[]},
               {"CallStatus","in-progress"},
               {"ToCity",[]},
               {"ToState","Edinburgh"},
               {"To","+441315101897"},
               {"Digits","2"},
               {"ToCountry","GB"},
               {"msg","Gather End"},
               {"CallerCity",[]},
               {"ApiVersion","2010-04-01"},
               {"Caller","+447776251669"},
               {"CalledCity",[]}],
    handle(Params2, []).
