module Extra exposing (ternary, withDefaultData, withMaybeData)

import Dict exposing (Dict)
import ModelSchema exposing (RequestResult(..))


ternary test positive negative =
    case test of
        True ->
            positive

        False ->
            negative


withDefaultData : a -> RequestResult e a -> a
withDefaultData default result =
    case result of
        Success d ->
            d

        _ ->
            default


withMaybeData : RequestResult e a -> Maybe a
withMaybeData result =
    case result of
        Success d ->
            Just d

        _ ->
            Nothing
