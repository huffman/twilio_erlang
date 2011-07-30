-record(say, {
    voice     :: string(),
    language  :: string(),
    loop      :: integer(),
    text = "" :: string()
}).

-record(play, {
    loop     :: integer(),
    url = "" :: string()
}).

-record(gather, {
        action        :: string(),
        method        :: atom(),
        timeout       :: integer(),
        finish_on_key :: string(),
        num_digits    :: integer(),
        body = []     :: [tuple()]
    }).

-record(record, {
        action              :: string(),
        method              :: atom(),
        timeout             :: integer(),
        finish_on_key       :: string(),
        max_length          :: integer(),
        transcribe          :: boolean(),
        transcribe_callback :: string(),
        play_beep           :: boolean()
    }).

-record(number, {
        send_digits :: string(),
        url         :: string(),
        number = "" :: string()
    }).

-record(dial, {
        action         :: string(),
        method         :: atom(),
        timeout        :: integer(),
        hangup_on_star :: boolean(),
        time_limit     :: integer(),
        caller_id      :: string(),
        body = ""      :: string() | #number{}
    }).


-record(sms, {
        to              :: string(),
        from            :: string(),
        action          :: string(),
        method          :: atom(),
        status_callback :: string(),
        text = ""       :: string()
    }).

-record(redirect, {
        method   :: atom(),
        url = "" :: string()
    }).

-record(pause, {
        length :: integer()
    }).
