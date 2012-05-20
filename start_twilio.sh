#! /bin/bash

erl -pa ./deps/mochiweb/ebin ./deps erlsha2/ebin ./ebin -run twilio_app init
