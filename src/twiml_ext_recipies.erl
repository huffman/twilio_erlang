%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       Some simple Extended TwiML Recipies
%%%
%%% @end
%%% Created :  9 Apr 2012 by gordon <gordon@gordon.dev>

-module(twiml_ext_recipies).

-define(MYPHONE, "+447776301539").

-include("twilio.hrl").
-include("twilio_acc.hrl").

-export([
         recipe/1,
         random/0,
         external_function/1,
         external_callback/2
        ]).

random() ->
    {A, B, C} = now(),
    random:seed(A, B, C),
    N = random:uniform(15),
    recipe(N).

recipe(N) ->
    TwiML = recipe2(N),
    case twiml:is_valid(TwiML) of
        false -> io:format("Invalid TwiML ~p~n", [TwiML]),
                 {error, Msg} = twiml:validate(TwiML),
                 io:format(Msg),
                 exit("invalid TwiML");
        true -> TwiML
    end.

% say something
recipe2(1) ->
    [#say{text = "yowza"}];

% say a couple of things and change the voice
recipe2(2) ->
    [#say{text = "bonza, dogface", language = "fr", voice = "woman"},
     #say{text = "now piss aff!"}];

% send an SMS message
recipe2(3) ->
    [#say{text = "gonnae send someone an SMS"},
     #sms{text = "howdy", to = ?MYPHONE, from = ?PhoneNo}];

% pregnant pause
recipe2(4) ->
    [#say{text = "hot diggity", language = "de", voice = "woman"},
     #pause{length = 10}, #say{text = "lover boy!", voice= "woman"}];

% play a file
recipe2(5) ->
    [#play{url = "http://files.hypernumbers.com/music/"
           ++ "RockyMountainMedleyPart1.mp3"}];

% forward a call
recipe2(6) ->
    [#say{text = "forwarding to someone"},
     #dial{body = [#number{number = ?MYPHONE}]}];

% primitive conference call
recipe2(7) ->
    [#say{text="welcome to a simple conference call. "
          ++ "Have someone else call this number"},
     #dial{body = [#conference{muted = false, beep = true,
                               startConferenceOnEnter = true,
                               endConferenceOnExit = true,
                               conference = "bingo master"}]}];

% primitive answerphone
recipe2(8) ->
    [#say{text = "leave a message after the tone"},
     #record{playBeep = true, maxLength = 60}];

% IVR that repeats by default
recipe2(9) ->
    % get yer bits sorted out
    SAY1 = #say{text = "My you are looking swish"},
    RESPONSE1 = #response_EXT{title = "Praise", response = "1", body = [SAY1]},

    SAY2 = #say{text = "What you looking at, fannybaws?", language = "en-gb"},
    RESPONSE2 = #response_EXT{title = "Abuse", response = "2", body = [SAY2]},

    SAYD = #say{text = "can you no use a phone, bawbag?", language = "de"},
    DEFAULT = #default_EXT{title = "a slagging", body = [SAYD]},

    % now put them all together
    [#gather{numDigits = 1, autoMenu_EXT = true, after_EXT = [RESPONSE1,
                                                              RESPONSE2,
                                                              DEFAULT]}];
% IVR with a repeat
recipe2(10) ->
    % get yer bits sorted out
    SAY1 = #say{text = "My you are looking swish"},
    RESPONSE1 = #response_EXT{title = "Praise", response = "1", body = [SAY1]},

    SAY2 = #say{text = "What you looking at, fannybaws?", language = "es"},
    RESPONSE2 = #response_EXT{title = "Abuse", response = "2", body = [SAY2]},

    % now put them all together
    [#gather{numDigits = 1, autoMenu_EXT = true, after_EXT = [RESPONSE1,
                                                              RESPONSE2,
                                                              #repeat_EXT{}]}];

% IVR with an explicit repeat
recipe2(11) ->
    % get yer bits sorted out
    SAY1 = #say{text = "My you are looking swish"},
    RESPONSE1 = #response_EXT{title = "Praise", response = "1",
                              body = [SAY1]},

    SAY2 = #say{text = "What you looking at, fannybaws?", language = "fr"},
    RESPONSE2 = #response_EXT{title = "Abuse", response = "2",
                              body = [SAY2]},

    % now put them all together
    [#gather{numDigits = 1, autoMenu_EXT = true, after_EXT = [RESPONSE1,
                                                              RESPONSE2]}];

% call out to a function
recipe2(12) ->
    [#function_EXT{title = "call out to function",
                   module = 'twiml_ext_recipies',
                   fn = 'external_function'}];

% add a call out into a list of things
recipe2(13) ->
    [#say{text = "gonnae send someone an SMS"},
     #function_EXT{title = "call out to function",
                   module = 'twiml_ext_recipies',
                   fn = 'external_function'},
     #sms{text = "howdy", to = ?MYPHONE, from = ?MYPHONE}];

% add a call out which is on a sub-menu
recipe2(14) ->
    EXT = #function_EXT{title = "call out to function",
                            module = 'twiml_ext_recipies',
                            fn = 'external_function'},

    [#say{text="welcome to a simple conference call. "
          ++ "Have someone else call this number"},
     #dial{body = [EXT]}];

% add a call out which is on a sub-menu
recipe2(15) ->
    SAY = #say{text="Smoot"},
    EXT = #function_EXT{title = "call out to function",
                        module = 'twiml_ext_recipies',
                        fn = 'external_function'},
    GOTO = #goto_EXT{goto = "4"},
    SAY2 = #say{text="Backflip"},
    [SAY, EXT, GOTO, SAY2].

% the function that is called gets the whole phonecall_srv state
%
% a function that is called must return 2 parameters
% the 1st if a list of valid extended TwiML records
% the 2nd is a list of event callbacks. These take the form
% {type(), fun Fun/1} where type() -> notification | complete
% Other types which will be added later will be
% 'transcribe' and  'partcomplete'
% The funs in the event call have an arity of 2. The two arguments are
% the record which triggers the callback and the state of the phonecall
% They return ok.
external_function(_State) ->
    TwiML = [#say{text = "shag off"},
             #pause{length = 5},
             #say{text = "ratboy"}],
    {TwiML, [{complete, fun twiml_ext_recipies:external_callback/2}]}.

external_callback(_Rec, _State) ->
    % twilio_web_util:pretty_print(Rec),
    io:format("Callback called...~n"),
    ok.
