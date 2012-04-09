%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       Some simple Extended TwiML Recipies
%%%
%%% @end
%%% Created :  9 Apr 2012 by gordon <gordon@gordon.dev>

-module(twiml_EXT_recipies).

-define(MYPHONE, "+447776301539").

-include("twilio.hrl").
-include("twilio_acc.hrl").

-export([
         recipe/1,
         random/0,
         external_function/1
        ]).

random() ->
    {A, B, C} = now(),
    random:seed(A, B, C),
    N = random:uniform(12),
    recipe(N).

recipe(N) ->
    TwiML = recipy2(N),
    case twiml:is_valid(TwiML) of
        false -> io:format("Invalid TwiML ~p~n", [TwiML]),
                 exit("invalid TwiML");
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

    SAY2 = #say{text = "What you looking at, fannybaws?", language = "en-gb"},
    RESPONSE2 = #response_EXT{title = "Abuse", response = "2", body = [SAY2]},

    SAYD = #say{text = "can you no use a phone, bawbag?", language = "de"},
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

    SAY2 = #say{text = "What you looking at, fannybaws?", language = "es"},
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

    SAY2 = #say{text = "What you looking at, fannybaws?", language = "fr"},
    RESPONSE2 = #response_EXT{title = "Abuse", response = "2",
                              body = [SAY2]},

    % now put them all together
    [#gather{numDigits = 1, autoMenu_EXT = true, after_EXT = [RESPONSE1,
                                                              RESPONSE2]}];

% call out to a function
recipy2(12) ->
    [#function_EXT{title = "call out to function", module = 'twiml_EXT_recipies',
                   fn = 'external_function'}].

% the function that is called gets the whole inbound_phone_srv state
%
% a function that is called must return 3 parameters
% the 1st if a list of valid extended TwiML records
% the 2nd is a list of Fns of arity 1 to be executed when any
% recording notification messages come in
% the 3rd is a list of Fns of arity 1 to be executed when the
% master call complete notification message comes in
% the will be passed the appropriate #twilio{} record in
% the paramater
%
% most of the time just return two empty lists
external_function(_State) ->
    {recipe(10), [], []}.
