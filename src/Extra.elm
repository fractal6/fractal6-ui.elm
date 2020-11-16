module Extra exposing (..)

import Dict exposing (Dict)
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
