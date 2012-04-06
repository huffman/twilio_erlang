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

handle(Params) ->
    io:format("Params is ~p~n", [Params]),
    Say = #say{text="yowza"},
    twiml:encode(Say).

