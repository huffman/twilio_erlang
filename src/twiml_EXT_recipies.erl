%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       Some simple Extended TwiML Recipies
%%%
%%% @end
%%% Created :  9 Apr 2012 by gordon <gordon@gordon.dev>

-module(twiml_EXT_recipies).

-define(MYPHONE, "+447776251669").

-include("twilio.hrl").
-include("twilio_acc.hrl").

-export([
         recipe/1,
         random/0,
         function/1
        ]).

random() ->
    {A, B, C} = now(),
    random:seed(A, B, C),
    N = random:uniform(11),
    recipe(N).

recipe(N) ->
    TwiML = recipy2(N),
    io:format("Reciple ~p returns ~p~n", [N, TwiML]),
    case twiml:is_valid(TwiML) of
        false -> exit("invalid TwiML");
        true -> TwiML
    end.

% say something
recipy2(1) ->
    [#say{text = "yowza"}];

% say a couple of things and change the voice
recipy2(2) ->
    [#say{text = "bonza, dogface", language = "fr", voice = "woman"},
     #say{text = "now piss aff!"}];

% send an SMS message
recipy2(3) ->
    [#say{text = "gonnae send someone an SMS"},
     #sms{text = "howdy", to = ?MYPHONE, from = ?PhoneNo}];

% pregnant pause
recipy2(4) ->
    [#say{text = "hot diggity", language = "de", voice = "woman"},
     #pause{length = 10}, #say{text = "lover boy!", voice= "woman"}];

% play a file
recipy2(5) ->
    [#play{url = "http://files.hypernumbers.com/music/"
           ++ "RockyMountainMedleyPart1.mp3"}];

% forward a call
recipy2(6) ->
    [#say{text = "forwarding to someone"},
     #dial{body = [#number{number = ?MYPHONE}]}];

% primitive conference call
recipy2(7) ->
    [#say{text="welcome to a simple conference call. "
          ++ "Have someone else call this number"},
     #dial{body = [#conference{muted = false, beep = true,
                               startConferenceOnEnter = true,
                               endConferenceOnExit = true,
                               conference = "bingo master"}]}];

% primite answerphone
recipy2(8) ->
    [#say{text = "leave a message after the tone"},
     #record{playBeep = true, maxLength = 60}];

% IVR that repeats by default
recipy2(9) ->
    % get yer bits sorted out
    SAY1 = #say{text = "My you are looking swish"},
    RESPONSE1 = #response_EXT{title = "Praise", response = "1", body = [SAY1]},

    SAY2 = #say{text = "What you looking at, fannybaws?"},
    RESPONSE2 = #response_EXT{title = "Abuse", response = "2", body = [SAY2]},

    SAYD = #say{text = "can you no use a phone, bawbag?"},
    DEFAULT = #default_EXT{title = "a slagging", body = [SAYD]},

    % now put them all together
    [#gather{numDigits = 1, autoMenu_EXT = true, after_EXT = [RESPONSE1,
                                                              RESPONSE2,
                                                              DEFAULT]}];
% IVR with a default
recipy2(10) ->
    % get yer bits sorted out
    SAY1 = #say{text = "My you are looking swish"},
    RESPONSE1 = #response_EXT{title = "Praise", response = "1", body = [SAY1]},

    SAY2 = #say{text = "What you looking at, fannybaws?"},
    RESPONSE2 = #response_EXT{title = "Abuse", response = "2", body = [SAY2]},

    % now put them all together
    [#gather{numDigits = 1, autoMenu_EXT = true, after_EXT = [RESPONSE1,
                                                              RESPONSE2,
                                                              #repeat_EXT{}]}];

% IVR with an explicit repeat
recipy2(11) ->
    % get yer bits sorted out
    SAY1 = #say{text = "My you are looking swish"},
    RESPONSE1 = #response_EXT{title = "Praise", response = "1",
                              body = [SAY1]},

    SAY2 = #say{text = "What you looking at, fannybaws?"},
    RESPONSE2 = #response_EXT{title = "Abuse", response = "2",
                              body = [SAY2]},

    % now put them all together
    [#gather{numDigits = 1, autoMenu_EXT = true, after_EXT = [RESPONSE1,
                                                              RESPONSE2]}];

% call out to a function
recipy2(12) ->
    [#function_EXT{title = "call out to function", module = 'twiml_EXT_recipies',
                   fn = "function"}].

function(State) ->
    io:format("State is ~p~n", [State]),
    random().
