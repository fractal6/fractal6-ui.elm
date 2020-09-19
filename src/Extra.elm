module Extra exposing (ternary, toMapOfList, withDefaultData, withMapData, withMaybeData)

import Dict exposing (Dict)
import ModelSchema exposing (RequestResult(..))


ternary test positive negative =
    case test of
        True ->
            positive

        False ->
            negative



--- Utils


toMapOfList : List ( String, a ) -> Dict String (List a)
toMapOfList parameters =
    List.foldl
        (\( k, v ) dict -> Dict.update k (addParam v) dict)
        Dict.empty
        parameters


addParam : a -> Maybe (List a) -> Maybe (List a)
addParam value maybeValues =
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


withMapData : (b -> a) -> RequestResult e b -> RequestResult e a
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
