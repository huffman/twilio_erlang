%%%-------------------------------------------------------------------
%%% @author Gordon Guthrie (gordon@hypernumbers.com)
%%% @copyright 2011, Hypernumbers Ltd
%%% @doc Utilities for processing Twilio requests
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(twilio_web_util).

-export([process_body/1]).

-include("twilio_web.hrl").

-define(UQ, mochiweb_util:unquote).

process_body(Binary) ->
    List = binary_to_list(Binary),
    Elements = string:tokens(List, "&"),
    PropList = process(Elements, []),
    make_record(PropList).

make_record(PropList) ->
    make_r(PropList, #twilio{}, #twilio_called{}, #twilio_caller{},
           #twilio_from{}, #twilio_to{}).

make_r([], Twilio, Called, Caller, From, To) ->
    Twilio#twilio{called = fix_up_number(Called),
                  caller = fix_up_number(Caller),
                  from = fix_up_number(From),
                  to = fix_up_number(To)};
% main twilio record
make_r([{"AccountSid", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw#twilio{account_sid = ?UQ(Val)}, Cd, Cr, Fr, To);
make_r([{"ApplicationSid", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw#twilio{application_sid = ?UQ(Val)}, Cd, Cr, Fr, To);
make_r([{"ApiVersion", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw#twilio{api_version = ?UQ(Val)}, Cd, Cr, Fr, To);
make_r([{"Direction", Val}  | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw#twilio{direction = ?UQ(Val)},   Cd, Cr, Fr, To);
make_r([{"CallSid", Val}    | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw#twilio{call_sid = ?UQ(Val)},    Cd, Cr, Fr, To);
make_r([{"CallStatus", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw#twilio{call_status = ?UQ(Val)}, Cd, Cr, Fr, To);
% called records
make_r([{"Called", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw, Cd#twilio_called{called_number = ?UQ(Val)}, Cr, Fr, To);
make_r([{"CalledCity", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw, Cd#twilio_called{called_city = ?UQ(Val)}, Cr, Fr, To);
make_r([{"CalledCountry", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw, Cd#twilio_called{called_country_code = ?UQ(Val)}, Cr, Fr, To);
make_r([{"CalledState", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw, Cd#twilio_called{called_state = ?UQ(Val)}, Cr, Fr, To);
make_r([{"CalledZip", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw, Cd#twilio_called{called_zip = ?UQ(Val)}, Cr, Fr, To);
% caller records
make_r([{"Caller", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw, Cd, Cr#twilio_caller{caller_number = ?UQ(Val)}, Fr, To);
make_r([{"CallerCity", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw, Cd, Cr#twilio_caller{caller_city = ?UQ(Val)}, Fr, To);
make_r([{"CallerCountry", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw, Cd, Cr#twilio_caller{caller_country_code = ?UQ(Val)}, Fr, To);
make_r([{"CallerState", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw, Cd, Cr#twilio_caller{caller_state = ?UQ(Val)}, Fr, To);
make_r([{"CallerZip", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw, Cd, Cr#twilio_caller{caller_zip = ?UQ(Val)}, Fr, To);
% from records
make_r([{"From", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw, Cd, Cr, Fr#twilio_from{from_number = ?UQ(Val)}, To);
make_r([{"FromCity", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw, Cd, Cr, Fr#twilio_from{from_city = ?UQ(Val)}, To);
make_r([{"FromCountry", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw, Cd, Cr, Fr#twilio_from{from_country_code = ?UQ(Val)}, To);
make_r([{"FromState", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw, Cd, Cr, Fr#twilio_from{from_state = ?UQ(Val)}, To);
make_r([{"FromZip", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw, Cd, Cr, Fr#twilio_from{from_zip = ?UQ(Val)}, To);
% To records
make_r([{"To", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw, Cd, Cr, Fr, To#twilio_to{to_number = ?UQ(Val)});
make_r([{"ToCity", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw, Cd, Cr, Fr, To#twilio_to{to_city = ?UQ(Val)});
make_r([{"ToCountry", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw, Cd, Cr, Fr, To#twilio_to{to_country_code = ?UQ(Val)});
make_r([{"ToState", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw, Cd, Cr, Fr, To#twilio_to{to_state = ?UQ(Val)});
make_r([{"ToZip", Val} | T], Tw, Cd, Cr, Fr, To) ->
    make_r(T, Tw, Cd, Cr, Fr, To#twilio_to{to_zip = ?UQ(Val)}).

% these records all have the same structure so use that
fix_up_number({Rec, Number, City, Zip, State, [], [], []} = R) ->
    R;
fix_up_number({Rec, Number, City, Zip, State, [], CC, []}) ->
    {Country, CC, Prefix} = lists:keyfind(CC, 2, ?CCLOOKUP),
    NewPrefix = integer_to_list(Prefix),
    {Rec, fix(Number, NewPrefix), City, Zip, State,
     Country, CC, "+" ++ NewPrefix}.

fix("+" ++ Number, CC) ->
    "0" ++ re:replace(Number, "^" ++ CC, "", [{return, list}]).

process([], Acc) -> Acc;
process([H | T], Acc) ->
    NewAcc = case string:tokens(H, "=") of
                 [Key]      -> {Key, ""};
                 [Key, Val] -> {Key, Val}
             end,
    process(T, [NewAcc | Acc]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

unpack_test() ->
    Bin = <<"AccountSid=yabollocks&ToZip=&FromState=&Called=%2B441315101875&FromCountry=GB&CallerCountry=GB&CalledZip=&Direction=inbound&FromCity=&CalledCountry=GB&CallerState=&CallSid=yahoor&CalledState=Edinburgh&From=%2B447776251669&CallerZip=&FromZip=&CallStatus=ringing&ToCity=&ToState=Edinburgh&To=%2B441315101875&ToCountry=GB&CallerCity=&ApiVersion=2010-04-01&Caller=%2B447776251669&CalledCity=">>,
    OutPut = {twilio,"yabollocks","inbound","ringing","yahoor",
        "2010-04-01",
        {twilio_called,"01315101875",[],[],"Edinburgh",
                       "United Kingdom","GB","+44"},
        {twilio_caller,"07776251669",[],[],[],"United Kingdom","GB",
                       "+44"},
        {twilio_from,"07776251669",[],[],[],"United Kingdom","GB",
                     "+44"},
        {twilio_to,"01315101875",[],[],"Edinburgh","United Kingdom",
                   "GB","+44"}},
    ?assertEqual(OutPut, process_body(Bin)).

-endif.
