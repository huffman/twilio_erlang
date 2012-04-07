%%%-------------------------------------------------------------------
%%% @author Ryan Huffman <ryanhuffman@gmail.com>
%%% @copyright 2011,
%%% @doc Encodes twilio records into an XML document.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(twiml).

-export([
         encode/1,
         validate/1,
         is_valid/1,
         compile/1,
         compile/2,
         compile/3
        ]).

-export([
         testing/0,
         testing2/0,
         testing3/0
        ]).

-include("twilio.hrl").

-define(DOLLAR, "$").

%% @doc Encodes a set of twiml records as an XML document.
encode(Elements) ->
    Content = [{'Response', [], [to_xmerl_element(El) || El <- Elements]}],
    xmerl:export_simple(Content, xmerl_xml).

encode_record(Record) ->
    El = xmerl:export_simple([to_xmerl_element(Record)], xmerl_xml),
    "<?xml version=\"1.0\"?>" ++ XML = lists:flatten(El),
    {xml, XML}.

%% @doc Converts a twiml record to an xmerl XML element.
to_xmerl_element(#say{} = Say) ->
    Attrs = [{voice,    Say#say.voice},
             {language, Say#say.language},
             {loop,     Say#say.loop}],
    CleanAttrs = remove_undefined(Attrs),
    {'Say', CleanAttrs, [Say#say.text]};
to_xmerl_element(#play{} = Play) ->
    Attrs = [{loop, Play#play.loop}],
    CleanAttrs = remove_undefined(Attrs),
    {'Play', CleanAttrs, [Play#play.url]};
to_xmerl_element(#gather{} = Gather) ->
    Attrs = [{action,      Gather#gather.action},
             {method,      Gather#gather.method},
             {timeout,     Gather#gather.timeout},
             {finishOnKey, Gather#gather.finish_on_key},
             {numDigits,   Gather#gather.num_digits}],
    CleanAttrs = remove_undefined(Attrs),
    Body = [to_xmerl_element(Element) || Element <- Gather#gather.body],
    {'Gather', CleanAttrs, Body};
to_xmerl_element(#record{} = Record) ->
    Attrs = [{action,             Record#record.action},
             {method,             Record#record.method},
             {timeout,            Record#record.timeout},
             {finishOnKey,        Record#record.finish_on_key},
             {maxLength,          Record#record.max_length},
             {transcribe,         Record#record.transcribe},
             {transcribeCallback, Record#record.transcribe_callback},
             {playBeep,           Record#record.play_beep}],
    CleanAttrs = remove_undefined(Attrs),
    {'Record', CleanAttrs, []};
to_xmerl_element(#sms{} = Sms) ->
    Attrs = [{to,             Sms#sms.to},
             {from,           Sms#sms.from},
             {action,         Sms#sms.action},
             {method,         Sms#sms.method},
             {statusCallback, Sms#sms.status_callback}],
    CleanAttrs = remove_undefined(Attrs),
    {'Sms', CleanAttrs, [Sms#sms.text]};
to_xmerl_element(#dial{} = Dial) ->
    Attrs = [{action,       Dial#dial.action},
             {method,       Dial#dial.method},
             {timeout,      Dial#dial.timeout},
             {hangupOnStar, Dial#dial.hangup_on_star},
             {timeLimit,    Dial#dial.time_limit},
             {callerId,     Dial#dial.caller_id},
             {record,       Dial#dial.record}],
    CleanAttrs = remove_undefined(Attrs),

    case is_list(Dial#dial.body) of
        true ->
            Body = [Dial#dial.body];
        false ->
            Body = [to_xmerl_element(Dial#dial.body)]
    end,
    {'Dial', CleanAttrs, Body};
to_xmerl_element(#number{} = Number) ->
    Attrs = [{sendDigits, Number#number.send_digits},
             {url, Number#number.url}],
    CleanAttrs = remove_undefined(Attrs),
    {'Number', CleanAttrs, [Number#number.number]};
to_xmerl_element(#redirect{} = Redirect) ->
    Attrs = [{method, Redirect#redirect.method}],
    CleanAttrs = remove_undefined(Attrs),
    {'Redirect', CleanAttrs, [Redirect#redirect.url]};
to_xmerl_element(#pause{} = Pause) ->
    Attrs = [{length, Pause#pause.length}],
    CleanAttrs = remove_undefined(Attrs),
    {'Pause', CleanAttrs, []};
to_xmerl_element(#hangup{}) ->
    {'Hangup', [], []};
to_xmerl_element(#reject{} = Reject) ->
    Attrs = [{reason, Reject#reject.reason}],
    CleanAttrs = remove_undefined(Attrs),
    {'Reject', CleanAttrs, []};
to_xmerl_element(#client{} = C) ->
    {'Client', [], [C#client.client]};
to_xmerl_element(#conference{} = C) ->
    Attrs = [{muted,                  C#conference.muted},
             {beep,                   C#conference.beep},
             {startConferenceOnEnter, C#conference.startConferenceOnEnter},
             {endConferenceOnExit,    C#conference.endConferenceOnExit},
             {waitUrl,                C#conference.waitUrl},
             {waitMethod,             C#conference.waitMethod},
             {maxParticipants,        C#conference.maxParticipants}],
    CleanAttrs = remove_undefined(Attrs),
    {'Conference', CleanAttrs, [C#conference.conference]}.

%% @doc Removes any undefined attributes.
remove_undefined(Attrs) ->
    [Attr || {_, Value} = Attr <- Attrs, Value =/= undefined].

compile(Elements) ->
    compile(Elements, "0", fsm).

compile(Elements, Type) ->
    compile(Elements, "0", Type).

compile(Elements, Rank, html) ->
    comp2(Elements, Rank, fun print_html/2);
compile(Elements, Rank, ascii) ->
    comp2(Elements, Rank, fun print_ascii/2);
compile(Elements, Rank, fsm) ->
    comp2(Elements, Rank, fun make_fsm/2).

comp2(Elements, Rank, Fun) ->
    {_NewType,  _NewRank,  Output} = comp3(Elements, hangup,
                                           'no-state', Fun, Rank, []),
    Output.

% these records are terminals as well
% nothing can happen after them, so chuck the tail away
% and terminate here
comp3([H | _T], _ExitType, Type, Fun, Rank, Acc)
  when is_record(H, hangup)
       orelse is_record(H, reject)
       orelse is_record(H, goto_EXT) ->
    Rank2 = bump(Rank),
    {Type, Rank2, lists:flatten(lists:reverse([Fun(H, Rank2) | Acc]))};
% gather is also a terminal - our gathers always have a default or a repeat
% value in them so they never drop through (hard to reason about for most
% folk IMHO - YMMV but hell mend ye...
comp3([#gather{} = G | _T], _ExitType, _Type, Fun, Rank, Acc) ->
    Rank2 = bump(Rank),
    NewRank = incr(Rank2),
    {Resp, Def, Repeat} = split_out(G#gather.after_EXT, [], [], []),
    % no hangup on body
    {_, NewRank1,  NewBody1} = comp3(G#gather.body, nohangup, state,
                                     Fun, NewRank, []),
    {_, NewRank2,  Menu} = make_menu(Resp, Def, G#gather.autoMenu_EXT,
                                     Fun, NewRank1),
    NewRank3 = bump(NewRank2),
    CloseGather = Fun("Gather", NewRank3),
    % no hangup on response - they get a hangup internally
    {_, NewRank4,  NewBody2} = comp3(Resp,   nohangup, state,
                                     Fun, NewRank3, []),
    {_, NewRank5,  NewBody3} = comp3(Def,    nohangup, state,
                                     Fun, NewRank4, []),
    {_, _NewRank6, NewBody4} = comp3(Repeat, nohangup, state,
                                     Fun, NewRank5, []),
    % drop the tail here (and drop the hangup!)
    % also make a close for the <gather> xml
    comp3([], nohangup, state, Fun, Rank2,
          [NewBody4, NewBody3, NewBody2, CloseGather, Menu,
           NewBody1, Fun(G, Rank2) | Acc]);
% otherwise we want all calls to end so we finalise with a HANGUP
% if they need a hangup
comp3([], hangup, Type, Fun, Rank, Acc) ->
    {NewType, NewRank, Hangup} = comp3([#hangup{}], hangup, Type, Fun,
                                       Rank, []),
    {NewType, NewRank, lists:flatten(lists:reverse([Hangup | Acc]))};
% and if they don't we don't give 'em a hangup
comp3([], nohangup, Type, _Fun, Rank, Acc) ->
    {Type, Rank, lists:flatten(lists:reverse(Acc))};
comp3([H | T], ExitType, Type, Fun, Rank, Acc)
  when is_record(H, say)
       orelse is_record(H, play)
       orelse is_record(H, record)
       orelse is_record(H, sms)
       orelse is_record(H, pause)
       orelse is_record(H, number)
       orelse is_record(H, client)
       orelse is_record(H, conference)
       orelse is_record(H, function_EXT)
       orelse is_record(H, chainload_EXT) ->
    Rank2 = bump(Rank),
    comp3(T, ExitType, Type, Fun, Rank2, [Fun(H, Rank2) | Acc]);
comp3([#dial{} = D | T], ExitType, _Type, Fun, Rank, Acc) ->
    Rank2 = bump(Rank),
    NewRank = incr(Rank2),
    % we don't want dial to terminate with a hangup
    {_, _NewRank2, NewBody} = comp3(D#dial.body, nohangup, state, Fun,
                                    NewRank, []),
    NewRank2 = bump(NewRank),
    CloseDial = Fun("Dial", NewRank2),
    NewRec = Fun(D, Rank2),
    comp3(T, ExitType, state, Fun, Rank2,
          [CloseDial, NewBody, NewRec | Acc]);
comp3([#response_EXT{} = R | T], ExitType, _Type, Fun, Rank, Acc) ->
    Rank2 = bump(Rank),
    NewRank = incr(Rank2),
    % we want response to end in a hangup{} if they terminate
    {_, _NewRank2, NewBody} = comp3(R#response_EXT.body, hangup, state,
                                    Fun, NewRank, []),
    comp3(T, ExitType, state, Fun, Rank2,
          [NewBody, Fun(R, Rank2) | Acc]);
comp3([#default_EXT{} = D | T], ExitType, _Type, Fun, Rank, Acc) ->
    Rank2 = bump(Rank),
    % we want default to end in a hangup{} if they terminate
    {_, _NewRank2, NewBody} = comp3(D#default_EXT.body, hangup, state,
                                    Fun, Rank2, []),
    comp3(T, ExitType, state, Fun, Rank2,
          [NewBody, Fun(D, Rank2) | Acc]);
% convert repeat into a #goto_EXT{} record
comp3([#repeat_EXT{} | T], ExitType, _Type, Fun, Rank, Acc) ->
    Rank2 = bump(Rank),
    G = #goto_EXT{goto = unbump(Rank)},
    comp3(T, ExitType, state, Fun, Rank2, [Fun(G, Rank2) | Acc]).

make_menu(_Resp, _Default, undefined, _Fun, Rank) ->
    {state, Rank, []};
make_menu(_Resp, _Default, false, _Fun, Rank) ->
    {state, Rank, []};
make_menu(Resp,   Default, true, Fun, Rank) ->
    Menu = make_m2(Resp, Default, []),
    comp3([Menu], nohangup, state, Fun, Rank, []).

make_m2([], [], Acc) ->
    #say{text = lists:flatten(lists:reverse(Acc))};
make_m2([], [Default], Acc) ->
    Title = Default#default_EXT.title,
    Msg = io_lib:format("Do nothing for ~s", [string:to_upper(Title)]),
    #say{text = lists:flatten(lists:reverse([Msg | Acc]))};
make_m2([#response_EXT{response = R, title = Tt} | T], Default, Acc) ->
    NewAcc = io_lib:format("Press ~s for ~s. ", [R, string:to_upper(Tt)]),
    make_m2(T, Default, [NewAcc | Acc]).

% either a default or a repeat
split_out([], Acc1, [], []) ->
    {sort(Acc1), [], [#repeat_EXT{}]};
split_out([], Acc1, [], Acc3) ->
    {sort(Acc1), [], [hd(lists:reverse(Acc3))]};
split_out([], Acc1, Acc2, []) ->
    {sort(Acc1), [hd(lists:reverse(Acc2))], []};
split_out([], Acc1, Acc2, _Acc3) ->
    {sort(Acc1), [hd(lists:reverse(Acc2))], []};
split_out([#response_EXT{} = R | T], Acc1, Acc2, Acc3) ->
    split_out(T, [R | Acc1], Acc2, Acc3);
split_out([#default_EXT{} = D | T], Acc1, Acc2, Acc3) ->
    split_out(T, Acc1, [D | Acc2], Acc3);
split_out([#repeat_EXT{} = R | T], Acc1, Acc2, Acc3) ->
    split_out(T, Acc1, Acc2, [R | Acc3]).

sort(List) ->
    Fun = fun(A, B) ->
                  if
                      A#response_EXT.response > B#response_EXT.response ->
                          false;
                      A#response_EXT.response < B#response_EXT.response ->
                          true
                  end
          end,
    lists:sort(Fun, List).

make_fsm(Rec, Rank) when is_record(Rec, hangup) ->
    {Rank, encode_record(Rec), exit};
make_fsm(Rec, Rank) when is_record(Rec, say)
                         orelse is_record(Rec, play)
                         orelse is_record(Rec, record)
                         orelse is_record(Rec, number)
                         orelse is_record(Rec, sms)
                         orelse is_record(Rec, pause)
                         orelse is_record(Rec, reject)
                         orelse is_record(Rec, client)
                         orelse is_record(Rec, conference) ->
        {Rank, encode_record(Rec), bump(Rank)};
make_fsm(Rec, Rank) when is_record(Rec, function_EXT) ->
        {Rank, Rec, bump(Rank)};
make_fsm(Rec, Rank) when is_record(Rec, gather) ->
        {Rank, fix_up(encode_record(Rec#gather{body = []})),
         bump(incr(Rank))};
make_fsm(Rec, Rank) when is_record(Rec, dial) ->
    {Rank, fix_up(encode_record(Rec#dial{body = []})),
     bump(incr(Rank))};
make_fsm(Rec, Rank) when is_record(Rec, response_EXT) ->
        {Rank, Rec#response_EXT{body = []}, bump(incr(Rank))};
make_fsm(Rec, Rank) when is_record(Rec, default_EXT) ->
        {Rank, Rec#default_EXT{body = []}, bump(incr(Rank))};
make_fsm(Rec, Rank) when is_record(Rec, chainload_EXT) ->
        {Rank, Rec, null};
make_fsm(Rec, Rank) when is_record(Rec, goto_EXT) ->
    {Rank, Rec, Rec#goto_EXT.goto};
make_fsm(List, Rank) when is_list(List) ->
    {Rank,{xml, "</" ++ List ++ ">"}, bump(Rank)}.

fix_up({xml, XML}) -> {xml, re:replace(XML, "<\/[a-zA-Z]+>" ++ ?DOLLAR, "",
 [{return, list}])}.

print_html(_Element, _Rank) ->
    ok.

print_ascii(Element, Rank) ->
    Indent = trunc(((length(Rank) - 1)/2) * 4),
    print_2(Element, Rank, Indent, "", "~n").

print_2(#say{text = T, voice = V, language = L}, Rank, Indent,
        Prefix, Postfix) ->
    io_lib:format("~s~s~s - SAY \"~s\" ~s ~s~s",
                  [Prefix, pad(Indent), Rank, T, e(V), e(L), Postfix]);
print_2(#play{url = U}, Rank, Indent, Prefix, Postfix) ->
    io_lib:format("~s~s~s - PLAY ~s ~s",
                  [Prefix, pad(Indent), Rank, U, Postfix]);
print_2(#gather{} = G, Rank, Indent, Prefix, Postfix) ->
    Key = case G#gather.finish_on_key of
              undefined -> "";
              K         -> "(finish with: " ++ K ++ ")"
          end,
    io_lib:format("~s~s~s - GATHER (request Keypad Input) ~s~s",
                  [Prefix, pad(Indent), Rank, Key, Postfix]);
print_2(#record{} = R, Rank, Indent, Prefix, Postfix) ->
    Beep = case R#record.play_beep of
               undefined -> "";
               false     -> "";
               true      -> "(beep)"
           end,
    Transcribe = case R#record.transcribe of
                     undefined -> "";
                     false     -> "";
                     true      -> "(transcribe)"
                 end,
    io_lib:format("~s~s~s - RECORD ~s ~s~s",
                  [Prefix, pad(Indent), Rank, Beep, Transcribe, Postfix]);
print_2(#number{number = N}, Rank, Indent, Prefix, Postfix) ->
    io_lib:format("~s~s~s - NUMBER ~s~s",
                  [Prefix, pad(Indent), Rank, N, Postfix]);
print_2(#dial{} = D, Rank, Indent, Prefix, Postfix) ->
    Record = case D#dial.record of
                 undefined -> "";
                 false     -> "";
                 true      -> "(to be recorded)"
             end,
    io_lib:format("~s~s~s - DIAL ~s~s",
                  [Prefix, pad(Indent), Rank, Record, Postfix]);
print_2(#sms{} = S, Rank, Indent, Prefix, Postfix) ->
    io_lib:format("~s~s~s - SMS \"~s\" - to ~s~s",
                  [Prefix, pad(Indent), Rank, S#sms.text, S#sms.to, Postfix]);
print_2(#pause{length = N}, Rank, Indent, Prefix, Postfix) ->
    io_lib:format("~s~s~s - PAUSE for ~p seconds~s",
                  [Prefix, pad(Indent), Rank, N, Postfix]);
print_2(#hangup{}, Rank, Indent, Prefix, Postfix) ->
    io_lib:format("~s~s~s - HANGUP ~s",
                  [Prefix, pad(Indent), Rank, Postfix]);
print_2(#reject{reason = R}, Rank, Indent, Prefix, Postfix) ->
    R2 = case R of
             undefined -> "";
             _         -> "because " ++ R
         end,
    io_lib:format("~s~s~s - REJECT ~s~s",
                  [Prefix, pad(Indent), Rank, R2, Postfix]);
print_2(#client{client = C}, Rank, Indent, Prefix, Postfix) ->
    io_lib:format("~s~s~s - CLIENT ~s~s",
                  [Prefix, pad(Indent), Rank, C, Postfix]);
print_2(#conference{} = C, Rank, Indent, Prefix, Postfix) ->
    Conf = C#conference.conference,
    Mute = case C#conference.muted of
               true -> "(caller can't speak)";
               _    -> ""
           end,
    Beep = case C#conference.beep of
               undefined -> "";
               false     -> "";
               true      -> "(beep)"
           end,
    Role = case C#conference.startConferenceOnEnter of
               undefined -> "";
               false     -> "";
               true      -> "(wait for conf admin)"
           end,
    End = case C#conference.endConferenceOnExit of
              undefined -> "";
              false     -> "";
              true      -> "(conf dies when this person leaves)"
          end,
    io_lib:format("~s~s~s - CONFERENCE ~s ~s ~s ~s ~s~s",
                  [Prefix, pad(Indent), Rank, Conf, Mute, Beep, Role,
                   End, Postfix]);
print_2(#response_EXT{response = R, title = T}, Rank, Indent,
        Prefix, Postfix) ->
    io_lib:format("~s~s~s - Response ~s : ~s  ~s",
                  [Prefix, pad(Indent), Rank, R, string:to_upper(T), Postfix]);
print_2(#function_EXT{title = T, module = M, fn = F}, Rank, Indent,
        Prefix, Postfix) ->
    io_lib:format("~s~s~s - Call out to ~s (~s:~s)  ~s",
                  [Prefix, pad(Indent), Rank, string:to_upper(T),
                   M, F, Postfix]);
print_2(#chainload_EXT{title = T, module = M, fn = F}, Rank, Indent,
        Prefix, Postfix) ->
    io_lib:format("~s~s~s - Dynamically Load Menu ~s from ~s:~s  ~s",
                  [Prefix, pad(Indent), Rank, string:to_upper(T),
                   M, F, Postfix]);
print_2(#goto_EXT{goto = G}, Rank, Indent, Prefix, Postfix) ->
    io_lib:format("~s~s~s - GOTO ~s  ~s",
                  [Prefix, pad(Indent), Rank, G, Postfix]);
print_2(List, Rank, Indent, Prefix, Postfix) when is_list(List) ->
    io_lib:format("~s~s~s - end of ~s (wait for response)~s",
                  [Prefix, pad(Indent), Rank, List, Postfix]).

pad(N) when is_integer(N) -> lists:flatten(lists:duplicate(N, " ")).

e(undefined) -> "";
e(X)         -> X.

check(#say{} = Say, Acc) ->
    NewAcc = is_member("#say{}", [{voice,    Say#say.voice,    ?SAYVoices},
                                  {language, Say#say.language, ?SAYLanguages}],
                       Acc),
    check_int(Say#say.loop, ?SAYLoopMin, "loop", "#say{}", NewAcc);
% should really check the url as well
check(#play{} = Play, Acc) ->
    check_int(Play#play.loop, ?PLAYLoopMin, "loop", "#play{}", Acc);
check(#gather{} = G, Acc) ->
    NAcc = is_member("#gather{}",
                     [{gather, G#gather.method,        ?GATHERMethod},
                      {gather, G#gather.finish_on_key, ?GATHERFOnKey}],
                     Acc),
    NAcc2 = check_int(G#gather.timeout, ?GATHERTimeoutMin, "timeout",
                      "#gather{}", NAcc),
    NAcc3 = check_int(G#gather.num_digits, ?GATHERTimeoutMin, "num_digits",
                      "#gather{}", NAcc2),
    NAcc4 = lists:foldl(fun check_gather/2, NAcc3, G#gather.body),
    NAcc5 = check_bool(G#gather.autoMenu_EXT, "autoMenu_EXT", "#gather{}", NAcc4),
    check_after_EXT(G#gather.after_EXT, G#gather.num_digits, NAcc5);
check(#record{} = R, Acc) ->
    NAcc = is_member("#record{}",
                     [{record, R#record.method,        ?RECORDMethod},
                      {record, R#record.finish_on_key, ?RECORDFOnKey}],
                     Acc),
    NAcc2 = check_bool(R#record.transcribe, "transcribe", "#record{}", NAcc),
    NAcc3 = check_bool(R#record.play_beep,  "play_beeb",  "#record{}", NAcc2),
    NAcc4 = check_int(R#record.timeout, ?RECORDTimeoutMin, "timeout",
                      "#record{}", NAcc3),
    check_int(R#record.max_length, ?RECORDMaxLen, "max_length",
              "#record{}", NAcc4);
check(#dial{} = D, Acc) ->
    NAcc = is_member("#dial{}", [{method, D#dial.method, ?DIALMethod}], Acc),
    NAcc2 = check_int(D#dial.timeout, ?DIALTimeoutMin, "timeout",
                      "#dial{}", NAcc),
    NAcc3 = check_int(D#dial.time_limit, ?DIALTimeLimitMin, "time_limit",
                      "#dial{}", NAcc2),
    NAcc4 = check_bool(D#dial.hangup_on_star, "hangup_on_star",
                       "#dial{}", NAcc3),
    NAcc5 = check_bool(D#dial.record, "#record{}",
                       "#dial{}", NAcc4),
    case D#dial.body of
        [] ->
            io_lib:format("Body of #dial must not be blank~sn", ["~"]);
        _ ->
            lists:foldl(fun check_dial/2, NAcc5, D#dial.body)
    end;
% we assume that the phone numbers (to and from) are properly formatted
% even though they yanks at twilio accept 'Merican formatted
% numbers as well
check(#sms{} = S, Acc) ->
    NAcc = is_member("#sms{}", [{method, S#sms.method, ?SMSMethod}], Acc),
    NAcc2 = check_phone_no(S#sms.to, "to", "#sms{}", NAcc),
    check_phone_no(S#sms.from, "from", "#sms{}", NAcc2);
check(#redirect{}, Acc) ->
    [io_lib:format("#redirect{} is not supported~n", []) | Acc];
check(#pause{} = P, Acc) ->
    check_int(P#pause.length, ?PAUSELengthMin, "length", "#pause{}", Acc);
check(#hangup{}, Acc) ->
    Acc;
check(#reject{} = R, Acc) ->
    is_member("#reject{}", [{method, R#reject.reason, ?REJECTReason}], Acc);
check(#response_EXT{}, Acc) ->
    [io_lib:format("#response_EXT{} can only be a in #gather{}.after_EXT~sn",
                   ["~"])
     | Acc];
check(#default_EXT{}, Acc) ->
    [io_lib:format("#default_EXT{} can only be a in #gather{}.after_EXT~sn",
                   ["~"])
     | Acc];
check(#function_EXT{} = A, Acc) ->
    NAcc = case A#function_EXT.title of
               undefined ->
                   [io_lib:format("title can't be blank in #function_EXT{}~sn",
                                  ["~"])
                    | Acc];
               _ ->
                   Acc
           end,
    NAcc2 = case A#function_EXT.module of
                undefined ->
                    [io_lib:format("module can't be blank in #function_EXT{}~sn",
                                   ["~"])
                     | NAcc];
                _ ->
                    NAcc
            end,
    case A#function_EXT.fn of
        undefined ->
            [io_lib:format("fn can't be blank in #function_EXT{}~sn", ["~"])
             | NAcc2];
        _ ->
            Acc
    end;
check(#chainload_EXT{} = C, Acc) ->
    NAcc = case C#chainload_EXT.module of
               undefined ->
                   [io_lib:format("module can't be blank in #chainload_EXT{}~sn",
                                  ["~"])
                    | Acc];
               _ ->
                   Acc
           end,
    case C#chainload_EXT.fn of
        undefined ->
            [io_lib:format("fn can't be blank in #chainload_EXT{}~sn", ["~"])
             | NAcc];
        _ ->
            NAcc
    end;
check(#goto_EXT{goto = G}, Acc) ->
    case G of
        X when X == undefined orelse X == "" ->
            [io_lib:format("#goto{} can't be blank~sn", ["~"]) | Acc];
        _ ->
            Acc
    end;
check(#repeat_EXT{}, Acc) ->
    [io_lib:format("#repeat_EXT{} can only be a in #gather{}.after_EXT~sn",
                   ["~"])
     | Acc];
check(Rec, Acc) when is_record(Rec, number)
                     orelse is_record(Rec, client)
                     orelse is_record(Rec, conference) ->
    El = element(1, Rec),
    [io_lib:format("#~p{} records are nouns and must be nested in #dial{}~sn",
                   [El, "~"]) | Acc].

% we assume that the phone number is properly formatted
% even though they yanks at twilio accept 'Merican formatted
% numbers as well
check_noun(#number{} = N, Acc) ->
    NAcc = check_send_digits(N#number.send_digits, Acc),
    check_phone_no(N#number.number, "number", "#number{}", NAcc);
% client is a noun with no attributes
check_noun(#client{} = C, Acc) ->
    if
        is_list(C#client.client) ->
            Acc;
        true ->
            [io_lib:format("Invalid client in #client{} ~p~sn",
                           [C#client.client, "~"]) | Acc]
    end;
check_noun(#conference{} = C, Acc) ->
    NAcc = is_member("#conference", [{conference, C#conference.waitMethod,
                                      ?CONFERENCEWaitMethod}],
                     Acc),
    NAcc2 = check_bool(C#conference.muted, "muted", "#conference{}", NAcc),
    NAcc3 = check_bool(C#conference.beep, "beep", "#conference{}", NAcc2),
    NAcc4 = check_bool(C#conference.startConferenceOnEnter,
                       "startConferenceOnEnter", "#conference{}", NAcc3),
    NAcc5 = check_bool(C#conference.endConferenceOnExit,
                       "endConferenceOnExit", "#conference{}", NAcc4),
    MaxP = C#conference.maxParticipants,
    NAcc6 = if MaxP == undefined ->
                    NAcc5;
               MaxP =< ?CONFERENCEMaxParticipants
               andalso MaxP >= ?CONFERENCEMinParticipants ->
                    NAcc5;
               true ->
                    [io_lib:format("Invalid maxPartipants in #conference{} ~p~sn",
                                   [MaxP, "~"]) | Acc]
            end,
    if
        is_list(C#conference.conference) ->
            NAcc6;
        true ->
            [io_lib:format("Invalid conference in #conference{} ~p~sn",
                           [C#conference.conference, "~"]) | Acc]
    end.

check_after_EXT(List, NumD, Acc) ->
    {NumD, HasResp, NAcc} = lists:foldl(fun check_a2/2, {NumD, false, Acc},
                                        List),
    case HasResp of
        true  ->
            NAcc;
        false ->
            [io_lib:format("#gather.after_EXT must contain a "
                           ++ "#response_EXT or #default_EXT record~sn",
                           ["~"]) | NAcc]
    end.

% the after record can be a #repeat_EXT{} record
check_a2(Rec, {NumD, Acc1, Acc2}) when  is_record(Rec, repeat_EXT)->
    {NumD, Acc1, Acc2};
% the after record can be a #default_EXT{} record
check_a2(Rec, {NumD, _Acc1, Acc2}) when  is_record(Rec, default_EXT)->
    {NumD, true, lists:foldl(fun check/2, Acc2, Rec#default_EXT.body)};
% response_EXT can only be in a #gather{}.after_EXT
check_a2(#response_EXT{} = R, {NumD, _Acc1, Acc2}) ->
    NAcc = check_int_as_str(R#response_EXT.response, "response",
                            "response_EXT", Acc2),
    NAcc2 = case NumD of
                undefined ->
                    NAcc;
                _N ->
                    Resp = R#response_EXT.response,
                    case length(Resp) of
                        NumD ->
                            NAcc;
                        _ ->
                            [io_lib:format("Response ~s must be ~p digits "
                                           ++ "long in #response_EXT{}~sn",
                                           [Resp, NumD, "~"]) | NAcc]
                    end
            end,
    NAcc3 = lists:foldl(fun check/2, NAcc2, R#response_EXT.body),
    % 'cos we are here then there is a response so return true
    {NumD, true, NAcc3};
% anything else is wrong
check_a2(Rec, {NumD, Acc1, Acc2}) ->
    El = element(1, Rec),
    {NumD, Acc1, [io_lib:format("Can't ~p as after_EXT in #gather{}~sn",
                                [El, "~"]) | Acc2]}.

% can gather #say, #play and #pause
check_gather(Rec, Acc) when is_record(Rec, say)
                            orelse is_record(Rec, play)
                            orelse is_record(Rec, pause) ->
    check(Rec, Acc);
% anything else is wrong
check_gather(Rec, Acc) ->
    El = element(1, Rec),
    [io_lib:format("Can't #gather ~p~sn", [El, "~"]) | Acc].

% can dial #number, #client and #conference
check_dial(#number{}     = S, Acc) -> check_noun(S, Acc);
check_dial(#client{}     = P, Acc) -> check_noun(P, Acc);
check_dial(#conference{} = P, Acc) -> check_noun(P, Acc);
% #function_EXT and #chainload_EXT can come back as nouns so let them through
check_dial(#function_EXT{}  = P, Acc) -> check(P, Acc);
check_dial(#chainload_EXT{} = P, Acc) -> check(P, Acc);
% anything else is wrong
check_dial(Rec, Acc) ->
    El = element(1, Rec),
    [io_lib:format("Can't #dial ~p~sn", [El, "~"]) | Acc].

check_send_digits(undefined, Acc) ->
    Acc;
check_send_digits(String, Acc) ->
    S2 = string:strip(String, left, $w),
    check_int_as_str(S2, "send_digit", "#number{}", Acc).

check_phone_no("+" ++ Num, Fld, Rec, Acc) ->
    check_int_as_str(Num, Fld, Rec, Acc);
check_phone_no(Val, Fld, Rec, Acc) ->
    [io_lib:format("Invalid ~p in ~p ~p~sn", [Fld, Rec, Val, "~"]) | Acc].

check_int_as_str(undefined, _, _, Acc) ->
    Acc;
check_int_as_str(Str, Fld, Rec, Acc) ->
    IsValid = try list_to_integer(Str)
              catch
                  error: _ ->
                      false
              end,
    case IsValid of
        false ->
            [io_lib:format("Invalid ~p in ~p ~p~sn", [Fld, Rec, Str, "~"])
             | Acc];
        _ ->
            Acc
    end.

check_bool(undefined, _Fld, _Rec, Acc) ->
    Acc;
check_bool(Val, Fld, Rec, Acc) ->
    case string:to_lower(Val) of
        "true" ->
            Acc;
        "false" ->
            Acc;
        _Other ->
            [io_lib:format("Invalid ~p in ~p ~p~sn", [Fld, Rec, Val, "~"]) | Acc]
    end.

check_int(undefined, _Min, _Fld, _Rec, Acc) ->
    Acc;
check_int(Val, Min, Fld, Rec, Acc) ->
    if
        is_integer(Val) andalso Val >= Min ->
            Acc;
        true ->
            [io_lib:format("Invalid ~p in ~p ~p~sn", [Fld, Rec, Val, "~"]) | Acc]
    end.

is_member(_, [], Acc) ->
    Acc;
is_member(Label, [{_SubLab, undefined, _Vs} | T], Acc) ->
    is_member(Label, T, Acc);
is_member(Label, [{SubLab, K, Vs} | T], Acc) ->
    NewAcc = case lists:member(string:to_lower(K), Vs) of
                 true  -> Acc;
                 false -> [io_lib:format("Invalid ~s in ~s ~p~sn",
                                         [SubLab, Label, K, "~"]) | Acc]
             end,
    is_member(Label, T, NewAcc).

is_valid(Elements) ->
    try
        case validate(Elements) of
            {ok, ok} -> true;
            _        -> false
        end
    catch
        _What:_How ->
             false
    end.

validate(Elements) ->
    case lists:flatten(lists:reverse(lists:foldl(fun check/2, [],
                                                 Elements))) of
        []  -> {ok, ok};
        Err -> {error, Err}
    end.

bump(Rank) ->
    [T | H] = lists:reverse(string:tokens(Rank, ".")),
    NewT = integer_to_list(list_to_integer(T) + 1),
    string:join(lists:reverse([NewT | H]), ".").

unbump(Rank) ->
    [_T | H] = lists:reverse(string:tokens(Rank, ".")),
    string:join(lists:reverse(H), ".").

incr(Rank) -> Rank ++ ".0".

%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(XML(D), "<?xml version=\"1.0\"?><Response>"D"</Response>").

encode_test_() ->
    [{"say twiml",
      fun() ->
              ?assertEqual(
                 ?XML("<Say></Say>"),
                 lists:flatten(encode([#say{}]))),
              ?assertEqual(
                 ?XML("<Say>Hello!</Say>"),
                 lists:flatten(encode([#say{text="Hello!"}]))),
              ?assertEqual(
                 ?XML("<Say language=\"en\" loop=\"2\">Hello!</Say>"),
                 lists:flatten(encode([#say{text="Hello!", loop=2, language="en"}])))
      end},
     {"play twiml",
      fun() ->
              ?assertEqual(
                 ?XML("<Play></Play>"),
                 lists:flatten(encode([#play{}]))),
              ?assertEqual(
                 ?XML("<Play loop=\"2\"></Play>"),
                 lists:flatten(encode([#play{loop=2}]))),
              ?assertEqual(
                 ?XML("<Play loop=\"2\">https://someurlhere.com/blah</Play>"),
                 lists:flatten(encode([#play{url="https://someurlhere.com/blah", loop=2}]))),
              ?assertEqual(
                 ?XML("<Play loop=\"2\">https://someurlhere.com/blah</Play>"),
                 lists:flatten(encode([#play{url="https://someurlhere.com/blah", loop=2}])))
      end},
     {"gather twiml",
      fun() ->
              ?assertEqual(
                 ?XML("<Gather/>"),
                 lists:flatten(encode([#gather{}]))),
              ?assertEqual(
                 ?XML("<Gather action=\"eval_gather\"/>"),
                 lists:flatten(encode([#gather{action="eval_gather"}]))),
              ?assertEqual(
                 ?XML("<Gather action=\"eval_gather\" timeout=\"60\" numDigits=\"10\"/>"),
                 lists:flatten(encode([#gather{action="eval_gather", num_digits=10, timeout=60}]))),
              ?assertEqual(
                 ?XML("<Gather action=\"eval_gather\"><Say>Hello</Say></Gather>"),
                 lists:flatten(encode([#gather{action="eval_gather", body=[#say{text="Hello"}]}])))
      end},
     {"record twiml",
      fun() ->
              ?assertEqual(
                 ?XML("<Record/>"),
                 lists:flatten(encode([#record{}]))),
              ?assertEqual(
                 ?XML("<Record action=\"eval_reCOrd\"/>"),
                 lists:flatten(encode([#record{action="eval_reCOrd"}]))),
              ?assertEqual(
                 ?XML("<Record action=\"eval_record\" method=\"GET\" timeout=\"10\"/>"),
                 lists:flatten(encode([#record{action="eval_record", timeout=10, method='GET'}])))
      end},
     {"sms twiml",
      fun() ->
              ?assertEqual(
                 ?XML("<Sms></Sms>"),
                 lists:flatten(encode([#sms{}]))),
              ?assertEqual(
                 ?XML("<Sms>Hello!</Sms>"),
                 lists:flatten(encode([#sms{text="Hello!"}]))),
              ?assertEqual(
                 ?XML("<Sms action=\"default/sms\">Hello!</Sms>"),
                 lists:flatten(encode([#sms{text="Hello!", action="default/sms"}])))
      end},
     {"dial twiml",
      fun() ->
              ?assertEqual(
                 ?XML("<Dial></Dial>"),
                 lists:flatten(encode([#dial{}]))),
              ?assertEqual(
                 ?XML("<Dial action=\"/do_stuff\">1234833</Dial>"),
                 lists:flatten(encode([#dial{action="/do_stuff", body="1234833"}]))),
              ?assertEqual(
                 ?XML("<Dial action=\"/do_stuff\"><Number>1234833</Number></Dial>"),
                 lists:flatten(encode([#dial{action="/do_stuff", body=#number{number="1234833"}}]))),
              ?assertEqual(
                 ?XML("<Dial action=\"/do_stuff\"><Number sendDigits=\"9528\">1234833</Number></Dial>"),
                 lists:flatten(encode([#dial{action="/do_stuff",
                                             body=#number{send_digits="9528", number="1234833"}}])))
      end},
     {"composite twiml",
      fun() ->
              ?assertEqual(
                 ?XML("<Say>Hello! This is a voice message</Say><Say>This is another message</Say><Dial>48321523</Dial>"),
                 lists:flatten(encode([#say{text="Hello! This is a voice message"},
                                       #say{text="This is another message"},
                                       #dial{body="48321523"}])))
      end},
     {"redirect twiml",
      fun() ->
              ?assertEqual(
                 ?XML("<Redirect></Redirect>"),
                 lists:flatten(encode([#redirect{}]))),
              ?assertEqual(
                 ?XML("<Redirect>/do_stuff</Redirect>"),
                 lists:flatten(encode([#redirect{url="/do_stuff"}])))
      end},
     {"pause twiml",
      fun() ->
              ?assertEqual(
                 ?XML("<Pause/>"),
                 lists:flatten(encode([#pause{}]))),
              ?assertEqual(
                 ?XML("<Pause length=\"10\"/>"),
                 lists:flatten(encode([#pause{length=10}])))
      end}
    ].

validate_test_() ->
    [
     % SAY passing
     ?_assertEqual(true, is_valid([#say{}])),
     ?_assertEqual(true, is_valid([#say{loop = 3}])),
     ?_assertEqual(true, is_valid([#say{language = "de"}])),
     ?_assertEqual(true, is_valid([#say{language = "En"}])),
     % SAY failing
     ?_assertEqual(false, is_valid([#say{voice = "benny"}])),
     ?_assertEqual(false, is_valid([#say{language = "klingon"}])),
     ?_assertEqual(false, is_valid([#say{loop = "3"}])),
     ?_assertEqual(false, is_valid([#say{loop = "dd"}])),
     ?_assertEqual(false, is_valid([#say{loop = 1.1}])),
     ?_assertEqual(false, is_valid([#say{loop = -9}])),

     % PLAY passing
     ?_assertEqual(true, is_valid([#play{}])),
     ?_assertEqual(true, is_valid([#play{loop = 7}])),
     % PLAY failing
     ?_assertEqual(false, is_valid([#play{loop = "fsfs"}])),

     % GATHER passing
     % handled later on
     % GATHER failing
     ?_assertEqual(false, is_valid([#gather{}])),
     ?_assertEqual(false, is_valid([#gather{method = "pOsT"}])),
     ?_assertEqual(false, is_valid([#gather{timeout = 3}])),
     ?_assertEqual(false, is_valid([#gather{finish_on_key = "*"}])),
     ?_assertEqual(false, is_valid([#gather{num_digits = 333}])),
     ?_assertEqual(false, is_valid([#gather{body = [#play{}, #say{},
                                                    #pause{}]}])),
     ?_assertEqual("can you have finish_on_key and num_digits?",
                   "dinnae ken"),
     % RECORD passing
     ?_assertEqual(true, is_valid([#record{}])),
     ?_assertEqual(true, is_valid([#record{method = "Post"}])),
     ?_assertEqual(true, is_valid([#record{timeout = 2}])),
     ?_assertEqual(true, is_valid([#record{max_length = 123}])),
     ?_assertEqual(true, is_valid([#record{finish_on_key = "*"}])),
     ?_assertEqual(true, is_valid([#record{transcribe = "tRue"}])),
     ?_assertEqual(true, is_valid([#record{play_beep = "False"}])),
     % RECORD failing
     ?_assertEqual(false, is_valid([#record{method = "Pongo"}])),
     ?_assertEqual(false, is_valid([#record{timeout = 0}])),
     ?_assertEqual(false, is_valid([#record{max_length = 123.44}])),
     ?_assertEqual(false, is_valid([#record{finish_on_key = "&"}])),
     ?_assertEqual(false, is_valid([#record{transcribe = "tRuth"}])),
     ?_assertEqual(false, is_valid([#record{play_beep = "Farts"}])),

     % DIAL passing
     ?_assertEqual(true, is_valid([#dial{method = "GeT",
                                         body = [#number{number = "+123"}]}])),
     ?_assertEqual(true, is_valid([#dial{timeout = 3,
                                         body = [#number{number = "+123"}]}])),
     ?_assertEqual(true, is_valid([#dial{hangup_on_star = "trUE",
                                         body = [#number{number = "+123"}]}])),
     ?_assertEqual(true, is_valid([#dial{time_limit = 4,
                                         body = [#number{number = "+123"}]}])),
     ?_assertEqual(true, is_valid([#dial{record = "fAlse",
                                         body = [#number{number = "+123"}]}])),
     % DIAL failing
     ?_assertEqual(false, is_valid([#dial{}])),
     ?_assertEqual(false, is_valid([#dial{method = "GeT=rund"}])),
     ?_assertEqual(false, is_valid([#dial{timeout = "33"}])),
     ?_assertEqual(false, is_valid([#dial{hangup_on_star = "banjo"}])),
     ?_assertEqual(false, is_valid([#dial{time_limit = -4}])),
     ?_assertEqual(false, is_valid([#dial{record = "fAlsies"}])),

     % SMS passiing
     ?_assertEqual(true, is_valid([#sms{from = "+123", to = "+345"}])),
     ?_assertEqual(true, is_valid([#sms{method = "GEt",
                                        from = "+123", to = "+345"}])),
     % SMS failing
     ?_assertEqual(false, is_valid([#sms{}])),
     ?_assertEqual(false, is_valid([#sms{method = "GEt",
                                         from = "123", to = "+345"}])),
     ?_assertEqual(false, is_valid([#sms{method = "GEt",
                                         from = "+123", to = "345"}])),
     ?_assertEqual(false, is_valid([#sms{method = "Git",
                                         from = "+123", to = "+345"}])),

     % REDIRECT failing
     % in this schema REDIRECT is not used at all
     ?_assertEqual(false, is_valid([#redirect{method = "Git"}])),

     % PAUSE passing
     ?_assertEqual(true, is_valid([#pause{}])),
     ?_assertEqual(true, is_valid([#pause{length = 3}])),
     % PAUSE failing
     ?_assertEqual(false, is_valid([#pause{length = -3}])),

     % HANGUP passing
     ?_assertEqual(true, is_valid([#hangup{}])),

     % REJECT passing
     ?_assertEqual(true, is_valid([#reject{}])),
     ?_assertEqual(true, is_valid([#reject{reason = "BuSy"}])),
     % REJECT failing
     ?_assertEqual(false, is_valid([#reject{reason = "BuSTy"}])),

     % NUMBER passing
     ?_assertEqual(true, is_valid([#dial{body = [#number{send_digits = "ww234",
                                                         number = "+123"}]}])),
     ?_assertEqual(true, is_valid([#dial{body = [#number{number = "+234"}]}])),
     % NUMBER failing
     ?_assertEqual(false, is_valid([#number{}])),
     ?_assertEqual(false, is_valid([#dial{body = [#number{}]}])),
     ?_assertEqual(false, is_valid([#dial{body = [#number{send_digits = "dww234",
                                                          number = "+123"}]}])),
     ?_assertEqual(false, is_valid([#dial{body = [#number{number = "234"}]}])),

     % CLIENT passing
     ?_assertEqual(true, is_valid([#dial{body = [#client{client = "adb"}]}])),

     % CLIENT failing
     ?_assertEqual(false, is_valid([#client{}])),
     ?_assertEqual(false, is_valid([#dial{body = [#client{}]}])),
     ?_assertEqual(false, is_valid([#dial{body = [#client{client = 33}]}])),

     % CONFERENCE passing
     ?_assertEqual(true, is_valid([#dial{body =
                                         [#conference{conference = "a32"}]}])),
     ?_assertEqual(true, is_valid([#dial{body =
                                         [#conference{conference = "a32",
                                                      muted = "tRUe"}]}])),
     ?_assertEqual(true, is_valid([#dial{body =
                                         [#conference{conference = "a32",
                                                      beep = "fALse"}]}])),
     ?_assertEqual(true, is_valid([#dial{body =
                                         [#conference{conference = "a32",
                                                      startConferenceOnEnter =
                                                      "trUe"}]}])),
     ?_assertEqual(true, is_valid([#dial{body =
                                         [#conference{conference = "a32",
                                                      endConferenceOnExit =
                                                      "FAlSE"}]}])),
     ?_assertEqual(true, is_valid([#dial{body =
                                         [#conference{conference = "a32",
                                                      waitMethod = "pOst"}]}])),
     ?_assertEqual(true, is_valid([#dial{body =
                                         [#conference{conference = "a32",
                                                      maxParticipants = 34}]}])),

     % CONFERENCE failing
     ?_assertEqual(false,
                   is_valid([#conference{}])),
     ?_assertEqual(false,
                   is_valid([#dial{body = [#conference{}]}])),
     ?_assertEqual(false,
                   is_valid([#dial{body =
                                   [#conference{conference = "a32",
                                                muted = "filibuters"}]}])),
     ?_assertEqual(false,
                   is_valid([#dial{body =
                                   [#conference{conference = "a32",
                                                beep = "Trudy"}]}])),
     ?_assertEqual(false,
                   is_valid([#dial{body =
                                   [#conference{conference = "a32",
                                                startConferenceOnEnter =
                                                "f"}]}])),
     ?_assertEqual(false,
                   is_valid([#dial{body =
                                   [#conference{conference = "a32",
                                                endConferenceOnExit =
                                                "erk"}]}])),
     ?_assertEqual(false,
                   is_valid([#dial{body =
                                   [#conference{conference = "a32",
                                                waitMethod =
                                                "pOster boy"}]}])),
     ?_assertEqual(false,
                   is_valid([#dial{body =
                                   [#conference{conference = "a32",
                                                maxParticipants = 1}]}])),
     ?_assertEqual(false,
                   is_valid([#dial{body =
                                   [#conference{conference = "a32",
                                                maxParticipants = 41}]}]))
    ].

nesting_test_() ->
    % nesting verbs
    GATHER = #gather{},
    DIAL   = #dial{},

    % non-nesting verbs
    SAY      = #say{},
    PLAY     = #play{},
    PAUSE    = #pause{},
    SMS      = #sms{from = "+123", to = "+345"},
    REDIRECT = #redirect{method = "GEt"},
    RECORD   = #record{},
    HANGUP   = #hangup{},
    REJECT   = #reject{},

    % nouns
    NUMBER     = #number{send_digits = "ww234", number = "+123"},
    CLIENT     = #client{client = "yeah"},
    CONFERENCE = #conference{ conference = "hoot"},

    [
     % Nested GATHER passing
     % these are checked in later tests
     % Nested GATHER failing
     % these are checked in later tests

     % Nested DIAL passing
     ?_assertEqual(true, is_valid([#dial{body = [NUMBER]}])),
     ?_assertEqual(true, is_valid([#dial{body = [CLIENT]}])),
     ?_assertEqual(true, is_valid([#dial{body = [CONFERENCE]}])),
     ?_assertEqual(true, is_valid([#dial{body = [NUMBER, CLIENT, CONFERENCE]}])),
     % Nested DIAL failing
     ?_assertEqual(false, is_valid([#dial{body = [GATHER]}])),
     ?_assertEqual(false, is_valid([#dial{body = [DIAL]}])),
     ?_assertEqual(false, is_valid([#dial{body = [SAY]}])),
     ?_assertEqual(false, is_valid([#dial{body = [PLAY]}])),
     ?_assertEqual(false, is_valid([#dial{body = [RECORD]}])),
     ?_assertEqual(false, is_valid([#dial{body = [SMS]}])),
     ?_assertEqual(false, is_valid([#dial{body = [REDIRECT]}])),
     ?_assertEqual(false, is_valid([#dial{body = [PAUSE]}])),
     ?_assertEqual(false, is_valid([#dial{body = [HANGUP]}])),
     ?_assertEqual(false, is_valid([#dial{body = [REJECT]}])),
     ?_assertEqual(false, is_valid([#dial{body = [GATHER, DIAL, SAY, PLAY,
                                                  RECORD, SMS, REDIRECT,
                                                  PAUSE, HANGUP, REJECT]}]))
     % there are more nested dial tests with EXTENDED verbs later on
    ].

proper_gather_test_() ->
    SAY      = #say{},
    PLAY     = #play{},
    RESPONSE = #response_EXT{title = "schmacy", response = "1",
                             body = [SAY, PLAY]},
    [
     % GATHER passing
     ?_assertEqual(true, is_valid([#gather{method = "pOsT",
                                           after_EXT = [RESPONSE]}])),
     ?_assertEqual(true, is_valid([#gather{timeout = 3,
                                           after_EXT = [RESPONSE]}])),
     ?_assertEqual(true, is_valid([#gather{finish_on_key = "*",
                                           after_EXT = [RESPONSE]}])),
     ?_assertEqual(true, is_valid([#gather{num_digits = 1,
                                           after_EXT = [RESPONSE]}])),
     ?_assertEqual(true, is_valid([#gather{body = [#play{}, #say{},
                                                   #pause{}],
                                           after_EXT = [RESPONSE]}])),
     % GATHER failing
     ?_assertEqual(false, is_valid([#gather{method = "panda",
                                            after_EXT = [RESPONSE]}])),
     ?_assertEqual(false, is_valid([#gather{timeout = -3,
                                            after_EXT = [RESPONSE]}])),
     ?_assertEqual(false, is_valid([#gather{finish_on_key = "^",
                                            after_EXT = [RESPONSE]}])),
     ?_assertEqual(false, is_valid([#gather{num_digits = 333.45,
                                            after_EXT = [RESPONSE]}])),
     ?_assertEqual(false, is_valid([#gather{body = [#dial{}, #number{}],
                                            after_EXT = [RESPONSE]}])),
     ?_assertEqual(false, is_valid([#gather{body = [#play{loop = "erk"},
                                                    #say{voice = "bandy"}],
                                            after_EXT = [RESPONSE]}]))
    ].

menu_consistency_check_test_() ->
    % checks that the responses match the key pad input
    SAY        = #say{},
    PLAY       = #play{},
    RESPONSE1  = #response_EXT{title = "schmacy", response = "1",
                               body = [SAY, PLAY]},
    RESPONSE11 = #response_EXT{title = "schmacy", response = "11",
                               body = [SAY, PLAY]},
    DEFAULT    = #default_EXT{title = "plerk", body = [PLAY, SAY]},

    [
     % GATHER passing
     ?_assertEqual(true, is_valid([#gather{num_digits = 1, method = "pOsT",
                                           after_EXT = [RESPONSE1]}])),
     ?_assertEqual(true, is_valid([#gather{num_digits = 2, method = "pOsT",
                                           after_EXT = [RESPONSE11]}])),
     ?_assertEqual(true, is_valid([#gather{num_digits = 2, method = "pOsT",
                                           after_EXT = [DEFAULT]}])),
     ?_assertEqual(true, is_valid([#gather{finish_on_key = "#", method = "pOsT",
                                           after_EXT = [DEFAULT]}])),

     % GATHER failing
     ?_assertEqual(false, is_valid([#gather{num_digits = 1, method = "pOsT",
                                            after_EXT = [RESPONSE11]}])),
     ?_assertEqual(false, is_valid([#gather{num_digits = 2, method = "pOsT",
                                            after_EXT = [RESPONSE1]}]))
    ].

% dial should be able to include apply and chainload records
extended_dial_test_() ->
    SAY       = #say{},
    PLAY      = #play{},
    FUNCTION  = #function_EXT{title = "hey!",  module = bish, fn = bash},
    CHAINLOAD = #chainload_EXT{title = "ho!", module = bosh, fn = berk},
    RESPONSE  = #response_EXT{title = "schmacy", response = "1",
                              body = [SAY, PLAY]},
    DEFAULT   = #default_EXT{title = "plerk", body = [PLAY, SAY]},
    GOTO      = #goto_EXT{goto = "dddd"},
    [
     % DIAL passing
     ?_assertEqual(true, is_valid([#dial{method = "GeT",
                                         body = [FUNCTION]}])),
     ?_assertEqual(true, is_valid([#dial{timeout = 3,
                                         body = [CHAINLOAD]}])),
     ?_assertEqual(true, is_valid([#dial{hangup_on_star = "trUE",
                                         body = [FUNCTION, CHAINLOAD]}])),
     % DIAL failing tests
     ?_assertEqual(false, is_valid([#dial{time_limit = 4,
                                          body = [RESPONSE]}])),
     ?_assertEqual(false, is_valid([#dial{record = "fAlse",
                                          body = [DEFAULT]}])),
     ?_assertEqual(false, is_valid([#dial{record = "fAlse",
                                          body = [GOTO]}])),
     ?_assertEqual(false, is_valid([#dial{hangup_on_star = "trUE",
                                          body = [FUNCTION, CHAINLOAD,
                                                  RESPONSE, DEFAULT, GOTO]}]))
    ].

nested_gather_test() ->
    GATHER = #gather{},
    DIAL   = #dial{},

    % non-nesting verbs
    SAY      = #say{},
    PLAY     = #play{},
    PAUSE    = #pause{},
    SMS      = #sms{from = "+123", to = "+345"},
    REDIRECT = #redirect{method = "GEt"},
    RECORD   = #record{},
    HANGUP   = #hangup{},
    REJECT   = #reject{},

    % nouns
    NUMBER     = #number{send_digits = "ww234", number = "+123"},
    CLIENT     = #client{client = "yeah"},
    CONFERENCE = #conference{ conference = "hoot"},

    RESPONSE   = #response_EXT{response = "1", body = [SAY]},
    [
     ?_assertEqual(false, is_valid([#gather{body = [SAY],
                                            after_EXT = [RESPONSE]}])),
     ?_assertEqual(false, is_valid([#gather{body = [PLAY],
                                            after_EXT = [RESPONSE]}])),
     ?_assertEqual(false, is_valid([#gather{body = [PAUSE],
                                            after_EXT = [RESPONSE]}])),
     ?_assertEqual(false, is_valid([#gather{body = [SAY, PLAY, PAUSE],
                                            after_EXT = [RESPONSE]}])),
     % non nesting verbs will fail in GATHER
     ?_assertEqual(false, is_valid([#gather{body = [GATHER],
                                            after_EXT = [RESPONSE]}])),
     ?_assertEqual(false, is_valid([#gather{body = [DIAL],
                                            after_EXT = [RESPONSE]}])),
     ?_assertEqual(false, is_valid([#gather{body = [SMS],
                                            after_EXT = [RESPONSE]}])),
     ?_assertEqual(false, is_valid([#gather{body = [REDIRECT],
                                            after_EXT = [RESPONSE]}])),
     ?_assertEqual(false, is_valid([#gather{body = [RECORD],
                                            after_EXT = [RESPONSE]}])),
     ?_assertEqual(false, is_valid([#gather{body = [HANGUP],
                                            after_EXT = [RESPONSE]}])),
     ?_assertEqual(false, is_valid([#gather{body = [REJECT],
                                            after_EXT = [RESPONSE]}])),
     ?_assertEqual(false, is_valid([#gather{body = [GATHER, DIAL, SMS,
                                                    REDIRECT, PAUSE,
                                                    HANGUP, REJECT],
                                            after_EXT = [RESPONSE]}])),
     % non-nesting nouns will fail in GATHER
     ?_assertEqual(false, is_valid([#gather{body = [NUMBER],
                                            after_EXT = [RESPONSE]}])),
     ?_assertEqual(false, is_valid([#gather{body = [CLIENT],
                                            after_EXT = [RESPONSE]}])),
     ?_assertEqual(false, is_valid([#gather{body = [CONFERENCE],
                                            after_EXT = [RESPONSE]}])),
     ?_assertEqual(false, is_valid([#gather{body = [NUMBER, CLIENT,
                                                    CONFERENCE],
                                            after_EXT = [RESPONSE]}]))
    ].

% these tests also includes nested gathers
part_extension_test_() ->
    GATHER    = #gather{},
    SAY       = #say{},
    PLAY      = #play{},
    PAUSE     = #pause{},
    SMS       = #sms{from = "+123", to = "+345"},
    NUMBER    = #number{number = "+123"},
    DIAL      = #dial{body = [NUMBER]},
    FUNCTION  = #function_EXT{title = "hey!",  module = bish, fn = bash},
    CHAINLOAD = #chainload_EXT{title = "ho!", module = bosh, fn = berk},
    REPEAT    = #repeat_EXT{},
    GOTO      = #goto_EXT{goto = "dddd"},
    [
     % FUNCTION_EXT passing
     ?_assertEqual(true, is_valid([#function_EXT{module = bish, fn = bash}])),
     % FUNCTION_EXT failing
     ?_assertEqual(false, is_valid([#function_EXT{}])),

     % RESPONSE_EXT passing
     ?_assertEqual(true,
                   is_valid([#gather{after_EXT =
                                     [#response_EXT{response = "1",
                                                    body = [SAY, SMS]}]}])),
     ?_assertEqual(true,
                   is_valid([#gather{after_EXT =
                                     [#response_EXT{response = "1",
                                                    body = [FUNCTION]}]}])),
     ?_assertEqual(true,
                   is_valid([#gather{after_EXT =
                                     [#response_EXT{response = "1",
                                                    body = [CHAINLOAD]}]}])),
     ?_assertEqual(true,
                   is_valid([#gather{after_EXT =
                                     [#response_EXT{response = "1",
                                                    body = [GOTO]}]}])),
     ?_assertEqual(true,
                   is_valid([#gather{after_EXT =
                                     [#response_EXT{response = "1",
                                                    body = [SAY, CHAINLOAD,
                                                            PLAY, PAUSE,
                                                            GOTO, DIAL]}]}])),
     % RESPONSE_EXT failing
     ?_assertEqual(false, is_valid([#response_EXT{response = "1",
                                                  body = [GATHER]}])),
     ?_assertEqual(false, is_valid([#response_EXT{response = "1",
                                                  body = [SAY]}])),
     ?_assertEqual(false, is_valid([#response_EXT{response = "1",
                                                  body = [FUNCTION]}])),
     ?_assertEqual(false, is_valid([#response_EXT{response = "1",
                                                  body = [CHAINLOAD]}])),
     ?_assertEqual(false, is_valid([#response_EXT{response = "1",
                                                  body = [GOTO]}])),
     ?_assertEqual(false, is_valid([#response_EXT{response = "aa",
                                                  body = [CHAINLOAD]}])),
     ?_assertEqual(false, is_valid([#response_EXT{response = "1",
                                                  body = [REPEAT]}])),

     % DEFAULT_EXT passing
     ?_assertEqual(true, is_valid([#gather{after_EXT =
                                           [#default_EXT{body =
                                                         [PLAY, SAY,
                                                          FUNCTION]}]}])),
     % There are no DEFAULT_EXT failing tests
     ?_assertEqual(false, is_valid([#default_EXT{body = [PLAY, SAY,
                                                         FUNCTION]}])),

     % CHAINLOAD_EXT passing
     ?_assertEqual(true, is_valid([#chainload_EXT{module = bosh, fn = berk}])),
     % CHAINLOAD_EXT failing
     ?_assertEqual(false, is_valid([#chainload_EXT{}])),

     % GOTO passing
     ?_assertEqual(true, is_valid([#goto_EXT{goto = "abc"}])),
     % GOTO failing
     ?_assertEqual(false, is_valid([#goto_EXT{}])),

     % REPEAT_EXT can't pass on its own
     % REPEAT_EXT failing
     ?_assertEqual(false, is_valid([#repeat_EXT{}]))
    ].

full_extension_test_() ->
    % extension nesting verb

    % verb that doesn't nest in the extension
    DIAL   = #dial{},

    % non-nesting verbs
    SAY      = #say{},
    PLAY     = #play{},
    PAUSE    = #pause{},
    SMS      = #sms{from = "+123", to = "+345"},
    REDIRECT = #redirect{method = "GEt"},
    RECORD   = #record{},
    HANGUP   = #hangup{},
    REJECT   = #reject{},

    % nouns
    NUMBER     = #number{send_digits = "ww234", number = "+123"},
    CLIENT     = #client{client = "yeah"},
    CONFERENCE = #conference{ conference = "hoot"},

    % extensions
    FUNCTION  = #function_EXT{title = "fancy", module = bish, fn = bash},
    CHAINLOAD = #chainload_EXT{title = "bonzo", module = bosh, fn = berk},
    RESPONSE1 = #response_EXT{title = "schmacy", response = "1",
                              body = [SAY, PLAY]},
    RESPONSE2 = #response_EXT{title = "clancy", response = "2",
                              body = [PAUSE, SAY, PLAY]},
    GATHER = #gather{body = [PLAY, SAY],
                     after_EXT = [RESPONSE1, RESPONSE2]},
    RESPONSE3 = #response_EXT{title = "glancy", response = "3",
                              body = [PAUSE, SAY, PLAY, GATHER]},
    RESPONSE4 = #response_EXT{title = "ooh, err!", response = "4",
                              body = [CHAINLOAD]},
    DEFAULT = #default_EXT{title = "deffo", body = [PLAY, SAY, FUNCTION]},
    GATHER2 = #gather{body = [SAY, PLAY, FUNCTION, CHAINLOAD],
                      after_EXT = [DIAL, SAY, PLAY, FUNCTION, CHAINLOAD, PAUSE,
                                   SMS, REDIRECT, RECORD, REJECT, HANGUP]},
    [
     % Extended GATHER passing
     ?_assertEqual(true, is_valid([#gather{body = [SAY],
                                           after_EXT = [RESPONSE1, RESPONSE2,
                                                        RESPONSE3, RESPONSE4,
                                                        DEFAULT]}])),
     % Extended GATHER failing
     ?_assertEqual(false, is_valid([#gather{body = [SAY],
                                            after_EXT = [NUMBER, CLIENT,
                                                         CONFERENCE]}])),
     ?_assertEqual(false, is_valid([#gather{body = [SAY],
                                            after_EXT = [GATHER2, DIAL]}])),
     ?_assertEqual(false, is_valid([#gather{body = [SAY],
                                            after_EXT = [SAY, PLAY, PAUSE,
                                                         SMS, RECORD,
                                                         REJECT]}])),
     % we don't support redirect records
     ?_assertEqual(false, is_valid([#gather{body = [SAY],
                                            after_EXT = [REDIRECT]}]))
    ].
%-endif.

testing() ->
    CHAINLOAD  = #chainload_EXT{title = "mimsy", module = "erk", fn = "berk"},
    SAY        = #say{text = "burb"},
    PLAY       = #play{url = "some file"},
    FUNCTION   = #function_EXT{title = "dandy", module = "bish", fn = "bash"},
    _RECORD     = #record{play_beep = true, transcribe = true},
    NUMBER1    = #number{number = "+998877"},
    NUMBER2    = #number{number = "+445566"},
    NUMBER3    = #number{number = "+220044"},
    CONFERENCE = #conference{muted = true, beep = true,
                             startConferenceOnEnter = true,
                             endConferenceOnExit = true,
                             conference = "ya dancer"},
    CLIENT     = #client{client = "banjo"},
    _DIAL       = #dial{record = true, body = [NUMBER1, NUMBER2, NUMBER3,
                                               CLIENT, CONFERENCE]},
    _SMS        = #sms{to = "+443399",
                       text = "now is the winter of our discontent"},
    PAUSE      = #pause{length = 123},
    _REJECT     = #reject{reason = "yo banger"},
    REPEAT     = #repeat_EXT{},
    RESPONSE1  = #response_EXT{title = "berk", response = "1",
                               body = [SAY, PLAY]},
    RESPONSE2  = #response_EXT{title = "jerk", response = "2",
                               body = [PAUSE, SAY, PLAY]},
    GATHER     = #gather{finish_on_key = "#", body = [SAY, PLAY],
                         after_EXT = [RESPONSE1, RESPONSE2]},
    RESPONSE3  = #response_EXT{title = "snerk", response = "3",
                               body = [PAUSE, SAY, PLAY,
                                       GATHER]},
    _RESPONSE4  = #response_EXT{title = "sperk", response = "4",
                               body = [CHAINLOAD]},
    _DEFAULT    = #default_EXT{title = "plerk", body = [PLAY, SAY, FUNCTION]},
    _GOTO       = #goto_EXT{goto = "1.2"},
    GATHER2    = #gather{autoMenu_EXT = true, body = [SAY, PLAY],
                         after_EXT = [RESPONSE3, REPEAT]},
    %compile([SAY, PLAY, RECORD, FUNCTION, DIAL, SMS, GATHER2, PAUSE, REJECT, GOTO],
    %        ascii).
    io:format(compile([GATHER2], ascii)),
    compile([GATHER2]).

testing2() ->
    _GATHER    = #gather{},
    SAY       = #say{},
    PLAY      = #play{},
    PAUSE     = #pause{},
    _SMS       = #sms{from = "+123", to = "+345"},
    NUMBER    = #number{number = "+123"},
    DIAL      = #dial{body = [NUMBER]},
    _FUNCTION  = #function_EXT{title = "hey!",  module = bish, fn = bash},
    CHAINLOAD = #chainload_EXT{title = "ho!", module = bosh, fn = berk},
    _REPEAT    = #repeat_EXT{},
    GOTO      = #goto_EXT{goto = "dddd"},
    validate([#gather{after_EXT = [#response_EXT{response = "1", title = "erk",
                                                 body = [SAY, CHAINLOAD,
                                                         PLAY, PAUSE,
                                                         GOTO, DIAL]}]}]).

testing3() ->
    DIAL = #dial{body=[#number{number="+4455222"}]},
    case is_valid([DIAL]) of
        false ->
            io:format("~p is not valid~n", [DIAL]);
        true  ->
            io:format("Does ~p validate? ~p~n",
                      [DIAL, validate([DIAL])]),
            compile([DIAL])
    end.
