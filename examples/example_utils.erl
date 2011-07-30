%%%-------------------------------------------------------------------
%%% @author Ryan Huffman <ryanhuffman@gmail.com>
%%% @copyright 2011, Ryan Huffman
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_utils).

-export([spawnfest_complete_string/0]).

-define(DAY, (24 * 60 * 60)).
-define(HOUR, (60 * 60)).
-define(MINUTE, 60).

spawnfest_complete_string() ->
    SpawnfestEndTime = calendar:datetime_to_gregorian_seconds({{2011, 7, 11}, {0, 0, 0}}),
    CurrentTime = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),

    Diff = SpawnfestEndTime - CurrentTime,

    case Diff < 0 of
        true ->
            "Spawnfest is already over, dude";
        false ->
            Days = Diff div ?DAY,
            Diff2 = Diff rem ?DAY,

            Hours = Diff2 div ?HOUR,
            Diff3 = Diff2 rem ?HOUR,

            Minutes = Diff3 div ?MINUTE,
            Seconds = Diff3 rem ?MINUTE,

            Times = [{Days, "day"}, {Hours, "hour"}, {Minutes, "minute"}, {Seconds, "second"}],

            TimeLeft = [format_date(D, U) || {D, U} <- Times],
            TimeLeftSpaces = string:join(TimeLeft, ", "),

            "There are " ++ TimeLeftSpaces ++ " until Spawnfest is over"
    end.

format_date(1, Unit) ->
    "1 " ++ Unit;
format_date(D, Unit) ->
    integer_to_list(D) ++ " " ++ Unit ++ "s".
