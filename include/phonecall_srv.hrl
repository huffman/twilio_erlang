%% the state record of the phonecall_srv
-record(pc_state, {twiml_ext = null, initial_params = null, log = null, fsm = null,
               currentstate = "1", history = [], eventcallbacks = []}).
