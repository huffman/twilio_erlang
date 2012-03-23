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

% gather has been EXTtended with 2 extra fields
-record(gather, {
          action         :: string(),
          method         :: atom(),
          timeout        :: integer(),
          finish_on_key  :: string(),
          num_digits     :: integer(),
          body = []      :: [tuple()],
          autoMenu_EXT   :: boolean(),
          after_EXT = [] :: [tuple()]
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
          record         :: boolean(),
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

% redirect record is not required or supported in Extended TwiML
-record(redirect, {
          method   :: atom(),
          url = "" :: string()
         }).

-record(pause, {
          length :: integer()
         }).

-record(hangup, {}). % has no fields

-record(reject,
        {
          reason :: string()
         }).

-record(client, {
          client :: string()
         }).

-record(conference,
        {
          muted                  :: boolean(),
          beep                   :: boolean(),
          startConferenceOnEnter :: boolean(),
          endConferenceOnExit    :: boolean(),
          waitUrl                :: string(),
          waitMethod             :: string(),
          maxParticipants        :: integer(),
          conference             :: string()
         }).

% these are the EXTension records needed to make TWIML compilable
-record(response_EXT, {
          response  :: string(),
          title     :: string(),
          body = [] :: [tuple()]
         }).

-record(default_EXT, {
          title     :: string(),
          body = [] :: [tuple()]
         }).

-record(function_EXT, {
          title  :: string(),
          module :: string(),
          fn     :: string()
         }).

-record(chainload_EXT, {
          title  :: string(),
          module :: string(),
          fn     :: string()
         }).

-record(goto_EXT, {
          goto :: string()
          }).

-record(repeat_EXT, {}).

% definitions used in validation

% SAY record
-define(SAYLanguages, ["en", "en-gb", "es", "fr", "de"]).
-define(SAYVoices,    ["man", "woman" ]).
-define(SAYLoopMin,   0).
-define(SAYLength, 4000).

%GATHER record
-define(GATHERMethod,     ["get", "post"]).
-define(GATHERFOnKey,     ["0", "1", "2", "3", "4", "5",
                           "6", "7", "8", "9", "*", "#"]).
-define(GATHERTimeoutMin, 1).

% PLAY record
-define(PLAYLoopMin, 0).

% RECORD record
-define(RECORDMethod,     ["get", "post"]).
-define(RECORDFOnKey,     ["0", "1", "2", "3", "4", "5",
                           "6", "7", "8", "9", "*", "#"]).
-define(RECORDTimeoutMin, 1).
-define(RECORDMaxLen,     1).

% DIAL record
-define(DIALMethod,     ["get", "post"]).
-define(DIALTimeoutMin, 1).
-define(DIALTimeLimitMin, 1).

% SMS record
-define(SMSMethod, ["get", "post"]).
-define(SMSLength, 160).

% REDIRECT record
-define(REDIRECTMethod, ["get", "post"]).

% PAUSE record
-define(PAUSELengthMin, 1).

% REJECT record
-define(REJECTReason, ["busy", "rejected"]).

% CONFERENCE record
-define(CONFERENCEWaitMethod,      ["get", "post"]).
-define(CONFERENCEMinParticipants, 2).
-define(CONFERENCEMaxParticipants, 40).
