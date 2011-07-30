%%%-------------------------------------------------------------------
%%% @author Ryan Huffman <ryanhuffman@gmail.com>
%%% @copyright 2011, Ryan Huffman
%%% @doc Routes adhoc twilio requests.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(twilio_rt_adhoc).

-export([handle_request/2, build_action/2]).

%% @doc Routes a twilio request for "/adhoc".  Uses specified
%% module and function names in the path to handle the request.
%% Function names are prefixed with "twilio_" to at least add
%% some safety :) (allowing people to call destructive
%% module:function's would be a bad idea)
handle_request([PreModule, PreFunction | Rest], Params) ->
    Module = list_to_existing_atom(PreModule),
    Function = list_to_existing_atom("twilio_" ++ PreFunction),
    Module:Function(Rest, Params).

%% @doc An easy way to build a twilio action string for
%% adhoc requests.
build_action(Module, Function) ->
    ModuleString = atom_to_list(Module),
    "twilio_" ++ FunctionString = atom_to_list(Function),
    "adhoc/" ++ ModuleString ++ "/" ++ FunctionString.

