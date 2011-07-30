%%%-------------------------------------------------------------------
%%% @author Ryan Huffman <ryanhuffman@gmail.com>
%%% @copyright 2011, Ryan Huffman
%%% @doc Routes example twilio requests.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(twilio_rt_examples).

-export([handle_request/2]).

-include("twilio.hrl").

handle_request([], _Props) ->
    [
        #gather{
            action="examples/pressed_key",
            timeout=2,
            num_digits=1,
            body=[
                #say{text="Hi, you have 10 seconds to push a button."}
            ]
        },
        #say{text="You actually only had 2 seconds.  Too late.  Ha Ha."}
    ];
handle_request(["pressed_key"], Props) ->
    Digits = proplists:get_value("Digits", Props),
    [
        #say{text="Good job, you pressed " ++ Digits ++ "."},
        #say{text="You win"},
        #pause{length=3},
        #say{text="nothing."}
    ].

