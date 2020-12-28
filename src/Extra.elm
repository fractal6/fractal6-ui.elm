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


up0 : String -> String
up0 s =
    SE.toSentenceCase s


up1 : String -> String
up1 s =
    SE.toTitleCase s


upAll : String -> String
upAll t =
    String.toUpper t



-- Utils


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



-- Colors


colorToTextColor : String -> String
colorToTextColor color =
    if List.member color [ "#7FDBFF", "#39CCCC", "#01FF70", "#FFDC00", "#FF851B", "#F012BE", "#AAAAAA", "#DDDDDD" ] then
        "#000"

    else
        "#fff"
