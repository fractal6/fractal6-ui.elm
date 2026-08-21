module Elm.SearchPatternTest exposing (..)

import Expect
import Test exposing (..)
import Utils.String exposing (parseSearchPattern)


suite : Test
suite =
    describe "parseSearchPattern"
        [ test "no quotes: all unquoted" <|
            \_ ->
                parseSearchPattern "feature x hey"
                    |> Expect.equal ( Just "feature x hey", [] )
        , test "mixed quoted and unquoted" <|
            \_ ->
                parseSearchPattern "feature x \"alice@gmail.com\" hey"
                    |> Expect.equal ( Just "feature x hey", [ "alice@gmail.com" ] )
        , test "several quoted spans" <|
            \_ ->
                parseSearchPattern "\"a b\" \"c d\""
                    |> Expect.equal ( Nothing, [ "a b", "c d" ] )
        , test "single quotes also delimit" <|
            \_ ->
                parseSearchPattern "hey 'exact match'"
                    |> Expect.equal ( Just "hey", [ "exact match" ] )
        , test "unmatched quote: rest is unquoted, quote char dropped" <|
            \_ ->
                parseSearchPattern "don't panic"
                    |> Expect.equal ( Just "don t panic", [] )
        , test "empty quoted span is dropped" <|
            \_ ->
                parseSearchPattern "a \"\" b"
                    |> Expect.equal ( Just "a b", [] )
        , test "whitespace-only span is dropped" <|
            \_ ->
                parseSearchPattern "\"  \""
                    |> Expect.equal ( Nothing, [] )
        , test "empty input" <|
            \_ ->
                parseSearchPattern ""
                    |> Expect.equal ( Nothing, [] )
        , test "quoted-only" <|
            \_ ->
                parseSearchPattern "\"alice@gmail.com\""
                    |> Expect.equal ( Nothing, [ "alice@gmail.com" ] )
        , test "span content is trimmed" <|
            \_ ->
                parseSearchPattern "\" spaced out \""
                    |> Expect.equal ( Nothing, [ "spaced out" ] )
        ]
