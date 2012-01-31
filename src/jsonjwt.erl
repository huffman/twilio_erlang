%%%-------------------------------------------------------------------
%%% @author Ryan Huffman <ryanhuffman@gmail.com>
%%% @copyright 2012, Ryan Huffman
%%% @doc Really basic implementation of JSON Web Tokens as specified at
%%% http://self-issued.info/docs/draft-jones-json-web-token.html
%%% Only implements JWS tokens for Twilio Capabilities usage.  If
%%% completed, this should be pulled out into a standalone application.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(jsonjwt).

-export([encode/3]).

-type claim() :: {string(), string() | integer() | atom()}.

%% @doc Encodes a JWT.  Algorithm should be "HS256" or "none".
-spec encode([claim()], string(), string()) -> string().
encode(Claims, Key, Algorithm) ->
    Header = [{<<"typ">>, <<"JWT">>}, {<<"alg">>, list_to_binary(Algorithm)}],
    HeaderJSON = iolist_to_binary(mochijson2:encode({struct, Header})),
    HeaderEncoded = base64:encode(HeaderJSON),

    MessageJSON = iolist_to_binary(mochijson2:encode({struct, Claims})),
    MessageEncoded = base64:encode(MessageJSON),

    case Algorithm of
        "none" ->
            Signature = <<"">>;
        _ ->
            SigningInput = <<HeaderEncoded/binary, ".", MessageEncoded/binary>>,
            Signature = hmac:hmac256(Key, SigningInput)
    end,

    SignatureEncoded = base64:encode(Signature),
    <<HeaderEncoded/binary, ".", MessageEncoded/binary, ".", SignatureEncoded/binary>>.

