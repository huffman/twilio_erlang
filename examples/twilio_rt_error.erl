%%%-------------------------------------------------------------------
%%% @author Ryan Huffman <ryanhuffman@gmail.com>
%%% @copyright 2011, Ryan Huffman
%%% @doc
%%%
%%% @end
%%% Created : 10 Jul 2011
%%%-------------------------------------------------------------------

-module(twilio_rt_error).

-export([handle_request/2]).

-include("twilio.hrl").

handle_request(["sms"], _Params) ->
    [#sms{text="Oops, something went wrong. Damn."}];
handle_request(_Path, _Params) ->
    [#say{text="Oops, something went wrong. Damn."}].

