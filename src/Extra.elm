module Extra exposing (ternary, withDefaultData, withMaybeData, withStateString)

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


withStateString : String -> RequestResult e a -> RequestResult e String
withStateString newres result =
    case result of
        Success _ ->
            Success newres

        Failure err ->
            Failure err

        Loading ->
            Loading

        LoadingSlowly ->
            LoadingSlowly

        NotAsked ->
            NotAsked
