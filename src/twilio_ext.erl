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
%-export([
%         log_terms/2
%        ]).

-include("twilio.hrl").
-include("twilio_web.hrl").

handle(Params, Path) ->
    % log_terms(Params, "twilio.params.log"),
    Records = twilio_web_util:process_proplist(Params),
    case Records#twilio.call_status of
        "ringing" ->
            io:format("phone ringing...~n"),
            TwiML_EXT = twiml_EXT_recipies:random(),
            phonecall_sup:answer_phone(Records, TwiML_EXT);
        "completed" ->
            case Records#twilio.recording of
                null ->
                    case Path of
                        [] ->
                            io:format("call completed...~n"),
                            ok = phonecall_sup:call_complete(Records),
                            ok;
                        Sub ->
                            io:format("goto ~p segment of call completed...~n",
                                      [Sub]),
                            % do nothing here
                            ok
                    end;
                _ ->
                    io:format("notification of recording...~n"),
                    ok = phonecall_sup:recording_notification(Records, Path),
                    ok
            end;
        "in-progress" ->
            case Path of
                [] ->
                    io:format("response to gather...~n"),
                    phonecall_sup:gather_response(Records);
                [State | _] ->
                    io:format("return to state ~p~n", [State]),
                    phonecall_sup:goto_state(Records, State)
            end;
        Other ->
            twilio_web_util:pretty_print(Records),
            io:format("Other with ~p~n", [Other]),
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Response>"
                ++ "<Say>something has gone wrong, folks.</Say></Response>"
    end.

%% log_terms(Terms, File) ->
%%     Str = lists:flatten(io_lib:format("~p.~n", [Terms])),
%%     _Return = filelib:ensure_dir(File),
%%     case file:open(File, [append]) of
%%         {ok, Id} ->
%%             io:fwrite(Id, "~s~n", [Str]),
%%             file:close(Id);
%%         _ ->
%%             error
%%     end.

