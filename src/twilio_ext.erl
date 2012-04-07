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
         log_terms/2
        ]).

-include("twilio.hrl").
-include("twilio_web.hrl").

handle(Params) ->
    inbound_phone_sup:answer_phone(Params),
    ok.

get_twiml_ext(1) -> [#say{text="yowza"}].

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
    Records = twilio_web_util:process_proplist(Params),
    handle(Records).

debug2() ->

    {A, B, C} = now(),
    CallSID = integer_to_list(A) ++ integer_to_list(B) ++ integer_to_list(C),
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
    Records = twilio_web_util:process_proplist(Params),
    handle(Records),
    supervisor:which_children(inbound_phone_sup).
