%%%-------------------------------------------------------------------
%%% @author Gordon Guthrie (gordon@hypernumbers.com)
%%% @copyright 2011, Hypernumbers Ltd
%%% @doc Utilities for processing Twilio requests
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(twilio_web_util).

-export([
         process_body/1,
         country_code_to_prefix/1,
         country_code_to_country/1,
         prefix_to_country/1,
         prefix_to_country_code/1
         ]).

-include("twilio_web.hrl").

-define(UQ, mochiweb_util:unquote).

country_code_to_prefix(CC) ->
    CC2 = string:to_upper(CC),
    {_Country, CC2, Prefix} = lists:keyfind(CC2, 2, ?CCLOOKUP),
    Prefix.

country_code_to_country(CC) ->
    CC2 = string:to_upper(CC),
    {Country, CC2, _Prefix} = lists:keyfind(CC2, 2, ?CCLOOKUP),
    Country.

prefix_to_country("00" ++ Prefix) ->
    prefix_to_country(Prefix);
prefix_to_country("+" ++ Prefix) ->
    prefix_to_country(Prefix);
prefix_to_country(Prefix) when is_integer(Prefix) ->
    prefix_to_country(integer_to_list(Prefix));
prefix_to_country(Prefix) ->
    {Country, _CC, Prefix} = lists:keyfind(Prefix, 3, ?CCLOOKUP),
    Country.

prefix_to_country_code("00" ++ Prefix) ->
    prefix_to_country_code(Prefix);
prefix_to_country_code("+" ++ Prefix) ->
    prefix_to_country_code(Prefix);
prefix_to_country_code(Prefix) when is_integer(Prefix) ->
    prefix_to_country_code(integer_to_list(Prefix));
prefix_to_country_code(Prefix) ->
    {_Country, CC, Prefix} = lists:keyfind(Prefix, 3, ?CCLOOKUP),
    CC.

process_body(Binary) ->
    List = binary_to_list(Binary),
    Elements = string:tokens(List, "&"),
    PropList = process(Elements, []),
    make_record(PropList).

make_record(PropList) ->
    make_r(PropList, #twilio{}, #twilio_called{}, #twilio_caller{},
           #twilio_from{}, #twilio_to{}, #twilio_duration{}).

make_r([], Twilio, Called, Caller, From, To, Dr) ->
    Twilio#twilio{called = normalise(Called, #twilio_called{}),
                  caller = normalise(Caller, #twilio_caller{}),
                  from   = normalise(From,   #twilio_from{}),
                  to     = normalise(To,     #twilio_to{}),
                  call_duration = normalise2(Dr, #twilio_duration{})};
% main twilio record
make_r([{"AccountSid", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw#twilio{account_sid = ?UQ(Val)}, Cd, Cr, Fr, To, Dr);
make_r([{"ApplicationSid", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw#twilio{application_sid = ?UQ(Val)}, Cd, Cr, Fr, To, Dr);
make_r([{"ApiVersion", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw#twilio{api_version = ?UQ(Val)}, Cd, Cr, Fr, To, Dr);
make_r([{"Direction", Val}  | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw#twilio{direction = ?UQ(Val)},   Cd, Cr, Fr, To, Dr);
make_r([{"CallSid", Val}    | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw#twilio{call_sid = ?UQ(Val)},    Cd, Cr, Fr, To, Dr);
make_r([{"CallStatus", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw#twilio{call_status = ?UQ(Val)}, Cd, Cr, Fr, To, Dr);
    % called records
make_r([{"Called", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd#twilio_called{number = ?UQ(Val)}, Cr, Fr, To, Dr);
make_r([{"CalledCity", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd#twilio_called{city = ?UQ(Val)}, Cr, Fr, To, Dr);
make_r([{"CalledCountry", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd#twilio_called{country_code = ?UQ(Val)}, Cr, Fr, To, Dr);
make_r([{"CalledState", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd#twilio_called{state = ?UQ(Val)}, Cr, Fr, To, Dr);
make_r([{"CalledZip", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd#twilio_called{zip = ?UQ(Val)}, Cr, Fr, To, Dr);
% caller records
make_r([{"Caller", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd, Cr#twilio_caller{number = ?UQ(Val)}, Fr, To, Dr);
make_r([{"CallerCity", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd, Cr#twilio_caller{city = ?UQ(Val)}, Fr, To, Dr);
make_r([{"CallerCountry", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd, Cr#twilio_caller{country_code = ?UQ(Val)}, Fr, To, Dr);
make_r([{"CallerState", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd, Cr#twilio_caller{state = ?UQ(Val)}, Fr, To, Dr);
make_r([{"CallerZip", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd, Cr#twilio_caller{zip = ?UQ(Val)}, Fr, To, Dr);
% from records
make_r([{"From", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd, Cr, Fr#twilio_from{number = ?UQ(Val)}, To, Dr);
make_r([{"FromCity", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd, Cr, Fr#twilio_from{city = ?UQ(Val)}, To, Dr);
make_r([{"FromCountry", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd, Cr, Fr#twilio_from{country_code = ?UQ(Val)}, To, Dr);
make_r([{"FromState", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd, Cr, Fr#twilio_from{state = ?UQ(Val)}, To, Dr);
make_r([{"FromZip", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd, Cr, Fr#twilio_from{zip = ?UQ(Val)}, To, Dr);
% To records
make_r([{"To", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd, Cr, Fr, To#twilio_to{number = ?UQ(Val)}, Dr);
make_r([{"ToCity", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd, Cr, Fr, To#twilio_to{city = ?UQ(Val)}, Dr);
make_r([{"ToCountry", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd, Cr, Fr, To#twilio_to{country_code = ?UQ(Val)}, Dr);
make_r([{"ToState", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd, Cr, Fr, To#twilio_to{state = ?UQ(Val)}, Dr);
make_r([{"ToZip", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd, Cr, Fr, To#twilio_to{zip = ?UQ(Val)}, Dr);
% call duration record
make_r([{"Duration", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd, Cr, Fr, To, Dr#twilio_duration{duration = Val});
make_r([{"CallDuration", Val} | T], Tw, Cd, Cr, Fr, To, Dr) ->
    make_r(T, Tw, Cd, Cr, Fr, To, Dr#twilio_duration{call_duration = Val});
% user supplied parameters
make_r([H | T], Tw, Cd, Cr, Fr, To, Dr) ->
    #twilio{custom_params = CP} = Tw,
    make_r(T, Tw#twilio{custom_params = [H | CP]}, Cd, Cr, Fr, To, Dr).

% if the record is the same as the empty record null it out
normalise(Rec, Rec) -> null;
normalise(Rec, _)   -> fix_up(Rec).

% if the record is the same as the empty record null it out
normalise2(Rec, Rec) -> null;
normalise2(Rec, _)   -> Rec.


% these records all have the same structure so use that
fix_up({_Rec, _Number, _City, _Zip, _State, [], [], []} = R) ->
    R;
fix_up({Rec, Number, City, Zip, State, [], CC, []}) ->
    {Country, CC, Prefix} = lists:keyfind(CC, 2, ?CCLOOKUP),
    NewPrefix = integer_to_list(Prefix),
    {Rec, fix_number(Number, NewPrefix), City, Zip, State,
     Country, CC, "+" ++ NewPrefix}.

fix_number("+" ++ Number, CC) ->
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
