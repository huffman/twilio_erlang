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
         log_terms/2
        ]).

-include("twilio.hrl").
-include("twilio_web.hrl").

handle(Params) ->
    log_terms(Params, "twilio.params.logx"),
    Records = twilio_web_util:process_proplist(Params),
    case Records#twilio.call_status of
        "ringing" ->
            N = random:uniform(2),
            TwiML_EXT = twilio_ext:get_twiml_ext(N),
            inbound_phone_sup:answer_phone(Records, TwiML_EXT);
        "completed" ->
            ok = inbound_phone_sup:call_complete(Records),
            ok
    end.

get_twiml_ext(1) -> [#say{text="yowza"}];
get_twiml_ext(2) -> [#say{text="bonza, dogface", language = "fr",
                         voice = "woman"},
                    #say{text = "now piss aff!"}].

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
