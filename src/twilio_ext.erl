%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       Examples of using extended twiml
%%%
%%% @end
%%% Created :  23 Mar 2012 by gordon@hypernumbers.com

-module(twilio_ext).

-export([
         handle/1
         ]).

-include("twilio.hrl").

handle(Params) ->
    log_terms(Params, "terms.log"),
    Say = #say{text="yowza"},
    twiml:encode([Say]).

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
