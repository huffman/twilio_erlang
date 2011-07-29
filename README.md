Twilio
======

This library provides an easy way to work with Twilio 
cloud telephony service (http://www.twilio.com/), including initiating calls,
sending SMSs, generating TwiML, and handling 
callbacks from the twilio server itself.

If you already have a web server setup and/or your own routing mechanisms you can
still use the `twilio` and `twiml` modules to access the twilio API and
also format twiml responses.

Usage
=====

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

License
=======

Check out LICENSE.

Other
=====

This library was written during [SpawnFest](http://spawnfest.com/).

