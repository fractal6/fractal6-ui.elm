-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.ScalarCodecs exposing (..)

import Fractal.Scalar exposing (defaultCodecs)
import Json.Decode as Decode exposing (Decoder)


type alias DateTime =
    Fractal.Scalar.DateTime


type alias Id =
    Fractal.Scalar.Id


codecs : Fractal.Scalar.Codecs DateTime Id
codecs =
    Fractal.Scalar.defineCodecs
        { codecDateTime = defaultCodecs.codecDateTime
        , codecId = defaultCodecs.codecId
        }
