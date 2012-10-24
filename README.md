Twilio
======

[![Build Status](https://secure.travis-ci.org/huffman/twilio_erlang.png)](http://travis-ci.org/huffman/twilio_erlang])


This library provides an easy way to work with Twilio
cloud telephony service (http://www.twilio.com/), including initiating calls,
sending SMSs, generating TwiML, and handling
callbacks from the twilio server itself.

If you already have a web server setup and/or your own routing mechanisms you can
still use the `twilio` and `twiml` modules to access the twilio API and
also format twiml responses.

Usage
=====

There are two ways to use twilio_erlang:

* traditional
* extended TwiML

Traditional Usage
=================

## Core

There are two core API modules:

### twilio

Contains functions to access the twilio REST API.  For instance, to make a Twilio call
you can call the function 'twilio:request()' to make an HTTP request to the twilio API:

```erlang
twilio:request("ACCOUNT_SID", "AUTH_TOKEN", post, "/2010-04-01/Account/ACCOUNT_SID/Calls.json",
               [{"From", "5551234"}, {"To", "5554321"}, {"Url", "http://YOUR_URL.com/"}])
```

which will make a call to the specified phone number.  API Reference: [http://www.twilio.com/docs/api/rest/making_calls]

You can 'twilio:request()' provides base level access for making HTTP requests to the Twilio API.

Using 'twilio:make_call()' does the same thing but handles most of the details:

```erlang
twilio:make_call("ACCOUNT_SID", "AUTH_TOKEN", "5551234", "5554321", [{"Url", "http://YOUR_URL.com/"}])
```

Much better.


### twiml

Contains functions to encode a list of twiml erlang records to an XML document.

For instance,

```erlang
twiml:encode([#say{text="Hello, World!"}, #dial{body="5551234"}])
```

produces:

```xml
<?xml version=\"1.0\"?><Response><Say>Hello, World!</Say><Dial>5551234</Dial></Response>
```


### twilio capabilities

*BETA*

Twilio Capability tokens tokens are described at
[http://www.twilio.com/docs/client/capability-tokens]

```erlang
AccountSID = "ACXXXXXXXXXXXXXXX",
AuthToken = "secret",
Capabilities = [{client_incoming, "tommy"}, {client_outgoing, "JJJJ"}],
Opts = [{expires_after, 3600}], % 6 minutes
Token = twilio_capabilities:generate(AccountSID, AuthToken, Capabilities,
Opts).
<<"eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzY29wZSI6WzExNSw5OSwxMTEsMTEyLDEwMSw1OCw5OSwxMDgsMTA1LDEwMSwxMTAsMTE2LDU4LDEwNSwxMTAsOTksMTExLDEwOSwxMDUsMTEwLDEwMyw2Myw5OSwxMDgsMTA1LDEwMSwxMTAsMTE2LDc4LDk3LDEwOSwxMDEsNjEsMTE2LDExMSwxMDksMTA5LDEyMSwzMiwxMTUsOTksMTExLDExMiwxMDEsNTgsOTksMTA4LDEwNSwxMDEsMTEwLDExNiw1OCwxMTEsMTE3LDExNiwxMDMsMTExLDEwNSwxMTAsMTAzLDYzLDk3LDExMiwxMTIsODMsMTA1LDEwMCw2MSw3NCw3NCw3NCw3NCwzOCw5OSwxMDgsMTA1LDEwMSwxMTAsMTE2LDc4LDk3LDEwOSwxMDEsNjEsMTE2LDExMSwxMDksMTA5LDEyMV0sImlzcyI6WzY1LDY3LDg4LDg4LDg4LDg4LDg4LDg4LDg4LDg4LDg4LDg4LDg4LDg4LDg4LDg4LDg4XSwiZXhwIjoxMzI3OTk3NzIxfQ==.l6HkGKuiPahjEJpRrqfG5ZLTdcqwHpfA5UFwKVPEEuE=">>
```

Look at the documentation and source for more information.

## Server

This application optionally starts an HTTP server to handle incoming requests from
Twilio.  You can start the web server by calling:

```erlang
twilio_web:start(_Port = 8080)
```

If you want to handle incoming calls to one of your Twilio phone numbers, you will
need to make sure you have the number setup on Twilio to point to your server.

### Routing

The web server will automatically route requests to a module based on the incoming request path.

An arbitrary path `/ROOT/...` will call the function `twilio_rt_ROOT:handle_request(RestOfPath, Props)`
where `Props` contains the HTTP request parameters.

Here is an example from the machine router `twilio_rt_machine`:

```erlang
%% @doc Handle incoming twilio requests on "/machine".
handle_request(["start"], Params) ->
    % these are values sent in from twilio
    City = proplists:get_value("ToCity", Params),
    State = proplists:get_value("ToState", Params),
    [
        #say{text="Hello!  Welcome to the spawnfest call center."},
        #say{text="It appears you are calling from " ++ City ++
            ", " ++ State},
        #redirect{url="options"}
    ];
```



This handles the route `/start` and passes the URL query or POST parameters, depending on the
HTTP method, as `Params'.  The function must return a list of TwiML records.

### Utilities

*BETA*

There are a number of utilities for handling parameters more easily by parsing them into Erlang records:

```erlang
%% @doc Handle incoming twilio requests on "/machine".
handle_request(["start"], Params) ->
    % these are values sent in from twilio
    Tw = twilio_web_util:process_body(Params),
    twilio_web_util:pretty_print(Tw),
```

This will parse and print out the incoming record giving something like this in the shell:

```
***Start of Twilio Record***********************
Account Sid is     "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
Application Sid is []
Direction is       "inbound"
Call Status is     "in-progress"
Call Sid is        "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy"
API Version is     "2010-04-01"
Custom Params is   []

called Number is       "01315101875"
called City is         []
called Zip is          []
called State is        "Edinburgh"

(...)

```
**NOTE** This work is not complete. the various Twilio parameters are being implemented gradually. Parameters not currently handled are returned as a list in the field `#twilio.custom_params`. If you see parameters in that field consider adding clauses to the function `twilio_web_util:make_r/7` and adding information to the records in `twilio_web.hrl`.

Why Extended TwiML?
===================

*BETA*

This is a major change to the operation of twilio_erlang - it handles incoming calls - but not yet inbound SMS.

TwiML is a set of Twilio facing XML commands that have been implemented in records.

Extended TwiML allows you to mix Erlang facing records with the Twilio-facing ones and makes it trivially easy to build Twilio applications.

Extended TwiML has a number of additions made to it:

* the record #gather{} has 2 additional fields
  * autoMenu_EXT
  * after_EXT
* there are new records
  * #response_EXT{}
  * #repeat_EXT{}
  * #default_EXT{}
  * #function_EXT{}
  * #goto_EXT{}

The problems that Extended TwiML is solving are:

* it is a pain to set up IVR in tradtional Twilio because you have to
  faff around with a **compile-time** state machine
  * Extended TwiML lets you build a **run-time** state machine by managing the responses for you
* speaking to Twilio is easy with TwiML - speaking to Erlang is off-topic
  * Extended TwiML makes it trivial to call out to Erlang
* there is the whole palaver with Twilio making multiple posts to you some of them containing notification that recordings have been made, some that calls are finished
  * Extended TwiML takes care of that for you
* there is no compile-time correctness checks for TwiML
  * Extended Twiml adds a validation step which checks TwiML for correctness
* you are responsible for making sure that the IVR system you build is well-behaved from a user's perspective
  * Extended TwiML does this at compile time
* it is hard to reason about what a TwiML-based system is doing without poring over the code
  * Extended TwiML compiles not only to an executable Finite State Machine but aslo an ascii or html representation of it that makes understanding what is going on a cinch

This is a piece of extended TwiML from the file recipe 1 in ``twiml_ext_recipies.erl``:

```erlang
    [#say{text = "yowza"}];
```

The ascii compilation of this is:

```
1 - SAY "yowza"
2 - HANGUP
```

It compiles to the following state machine:

```erlang
[{"1",{{xml,"<Say>yowza</Say>"},next}},
 {"2",{{xml,"<Hangup/>"},exit}}]
```

Which executes in a finite state machine inside ``phonecall_srv.erl``

This example is not so interesting. Recipe 9 is more interesting:

```erlang
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
```

The ascii compilation of this is:

```
1 - GATHER (request Keypad Input)
    1.1 - SAY "Press 1 for PRAISE. Press 2 for ABUSE. Do nothing for A SLAGGING"
    1.2 - end of Gather (wait for response)
    1.3 - Response 1 : PRAISE
        1.3.1 - SAY "My you are looking swish"
        1.3.2 - HANGUP
    1.4 - Response 2 : ABUSE
        1.4.1 - SAY "What you looking at, fannybaws?"  en-gb
        1.4.2 - HANGUP
    1.5 - Default : A SLAGGING
        1.5.1 - SAY "can you no use a phone, bawbag?"  de
        1.5.2 - HANGUP
```

Notice that by setting the option ``autoMenu_EXT`` to ``true`` an auto-menu is generated from the responses you have specified.

and the FSM version is:

```erlang
[{"1",{{xml,"<Gather numDigits=\"1\">"},into}},
 {"1.1",
  {{xml,"<Say>Press 1 for PRAISE. Press 2 for ABUSE. Do nothing for A SLAGGING</Say>"},
   next}},
 {"1.2",{{xml,"</Gather>"},gather}},
 {"1.3",{{response_EXT,"1","Praise",[]},into}},
 {"1.3.1",{{xml,"<Say>My you are looking swish</Say>"},next}},
 {"1.3.2",{{xml,"<Hangup/>"},exit}},
 {"1.4",{{response_EXT,"2","Abuse",[]},into}},
 {"1.4.1",
  {{xml,"<Say language=\"en-gb\">What you looking at, fannybaws?</Say>"}, next}},
 {"1.4.2",{{xml,"<Hangup/>"},exit}},
 {"1.5",{{default_EXT,"a slagging",[]},next}},
 {"1.5.1",
  {{xml,"<Say language=\"de\">can you no use a phone, bawbag?</Say>"}, next}},
 {"1.5.2",{{xml,"<Hangup/>"},exit}}]
```

The part that makes it super-interesting is the ability to call out to Erlang fuctions natively. Recipe 12 gives the simplest version of this:

```Erlang
    [#function_EXT{title = "call out to function", module = 'twiml_ext_recipies',
                   fn = 'external_function'}]
```

This compiles to ascii as:

```
1 - Call out to CALL OUT TO FUNCTION (twiml_ext_recipies:external_function)
2 - HANGUP
```

At run time this calls the function ``twiml_ext_recipies:external_function/1`` passing in the ``#state{}`` record from ``phonecall_srv.erl``

The signature of the external function is very straightforward:

```erlang
external_function(_State) ->
    {#twiml{}, [{Type, Fun/2}]}.
```

where Type is one of the following atoms:

```erlang
recording
completion
```

It takes a single parameter and it returns a two part tuple, the most important part is the first element - some valid Extended TwiML.

This is compiled and inserted into the Finite State Machine replacing the call-out entry. The FSM is then revaluated.

The other parameter to be returned is a list of tuples that subscribe functions to callback events. The first paramater is the event that is being subscribed to, the second is a function of arity 2 to be called when the event happens.

Extended TwiML Usage
====================

There are a number of steps to be gone through before using Extended TwiML:

* set up your Twilio Account and populate the .hrl file ```twilio_acc.hrl```
   * Extended TwiML users a worker process per call in progress and uses the status callbacks to delete them when they are finished. You need to set the *Status Callback URL* of your twilio app and number to be the same as the *Voice Request URL* and *SMS Request URL*.
   * Extended TwiML only accepts POST's and not GET's.
* the file ```twilio_EXT_recipies.erl``` contains a macro ``?MYPHONE`` which specifies a phone to send text messages to or do call forwarding to for some of the recipies - you will need to set it. Bear in mind you will need a minimum of two phones to test some of the recipes (call forwarding, conference calls, etc).
* Extended TwiML is commented out in ``twilio_web.hrl``- you will need to uncomment it
* NOTE Extended TwiML (mostly) avoids you needing to build State Machines in URLs - with one exception. There is a ``#goto_EXT{}`` record which forces a goto on to Twilio - it tells Twilio to ask for a particular state of the FSM. If you have bound your application to ``http://example.com/some/page`` all the POST's will come to it - with the exception of GOTO's. If there is a record ``#goto_EXT{goto = "1.2.3"}`` the next twilio request will be to ``http://example.com/some/page/1.2.3`` The example in ``twilio_ext.hrl`` is set up for Twilio being bound to the root (ie ``http://example.com/``). It you bind it elsewhere you will need to handle those paths yourself.

### Phonecall_srv.erl

The phone call supervision tree has a phonecall supervisor which spawns a process for all phone calls. This behaves slightly differently for inbound and outbound phone calls.

Inbound phone numbers **MUST** be set up with the callback status URL enabled in Twilio. This means that when a call is completed the application is notified by Twilio and the phonecall_srv can be sent a ``call complete`` message to terminate itself and clean up.

Outbound calls **DO NOT** receive a ``call complete`` or ``recording`` message and must clean up after themselves. An outbound call receives allready 'knows' the URL of its recording - so the simplest way to clean up after an outbound call is just to do a ``call complete`` call on it immediately after serving up the TwiML that makes the call.

### Developers' Notes

Extended TwiML has the following api:

* ``twiml:encode/1`` takes a list of TwiML records - silently discards any Twilio Extensions
* ``twiml:validate/1`` takes a list of TwiML records and validates them as Extended TwimL returning a list of error messages
* ``twiml:is_valid/1`` like ``validate/1`` except returns ``true`` or ``false``.
* ``twiml:compile/1`` takes a list of TwiML records and compiles them to the FSM
* ``twiml:compile/2`` takes a list of TwiML records and a atom which describes the compilation target - can be one of ``html``, ``ascii`` or ``fsm``

## Other Material

Slides of a talk about Extended Twiml at the Erlang User Confernce 2012
http://www.docstoc.com/docs/document-preview.aspx?doc_id=128095466

Presentation at TechMeetup in Glasgow
http://vimeo.com/47162182

License
=======

Check out LICENSE.

Other
=====

This library was written during [Spawnfest](http://spawnfest.com/), a 48 hour Erlang development competition.

