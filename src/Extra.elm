module Extra exposing (..)

import Dict exposing (Dict)
import ModelSchema exposing (RequestResult(..))
import String
import String.Extra as SE


ternary test positive negative =
    case test of
        True ->
            positive

        False ->
            negative



-- String


toUp : String -> String
toUp s =
    SE.toTitleCase s


toUp1 : String -> String
toUp1 s =
    SE.toSentenceCase s


toUpAll : String -> String
toUpAll t =
    String.toUpper t



--- Utils


toMapOfList : List ( String, a ) -> Dict String (List a)
toMapOfList parameters =
    List.foldl
        (\( k, v ) dict -> Dict.update k (addParam_ v) dict)
        Dict.empty
        parameters


addParam_ : a -> Maybe (List a) -> Maybe (List a)
addParam_ value maybeValues =
    case maybeValues of
        Just values ->
            Just (value :: values)

        Nothing ->
            Just [ value ]



-- RequestResult / Data methods


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


withMaybeDataMap : (a -> b) -> RequestResult e a -> Maybe b
withMaybeDataMap resMap result =
    case result of
        Success d ->
            Just (resMap d)

        _ ->
            Nothing


withMapData : (a -> b) -> RequestResult e a -> RequestResult e b
withMapData resMap result =
    case result of
        Success d ->
            Success (resMap d)

        Failure err ->
            Failure err

        Loading ->
            Loading

        LoadingSlowly ->
            LoadingSlowly

        NotAsked ->
            NotAsked
