{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2024 Fractale Co

   This file is part of Fractale.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
-}


module Extra exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, span, text)
import Html.Attributes exposing (attribute)
import List.Extra as LE
import Regex exposing (Regex)
import String
import String.Extra as SE



-- Utils


ternary : Bool -> a -> a -> a
ternary test pos neg =
    -- Save som lines ;)
    if test then
        pos

    else
        neg


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


insertAt : Int -> a -> List a -> List a
insertAt index element list =
    --List.take pos l ++ (x :: List.drop pos l)
    let
        ( before, after ) =
            LE.splitAt index list
    in
    before ++ [ element ] ++ after



-- String


upH : String -> String
upH s =
    SE.toSentenceCase s


upT : String -> String
upT s =
    SE.toTitleCase s


upA : String -> String
upA t =
    String.toUpper t


decap : String -> String
decap t =
    SE.decapitalize t


toText : List String -> Html msg
toText l =
    l
        |> List.intersperse " "
        |> List.map (\x -> text x)
        |> span []


textH : String -> Html msg
textH s =
    s |> upH |> text


textT : String -> Html msg
textT s =
    s |> upT |> text


textA : String -> Html msg
textA s =
    s |> upA |> text


textD : String -> Html msg
textD s =
    s |> decap |> text


space_ : String
space_ =
    "\u{00A0}"


regexFromString : String -> Regex
regexFromString =
    Regex.fromString >> Maybe.withDefault Regex.never


regexContains : String -> String -> Bool
regexContains pattern text =
    Regex.contains (regexFromString pattern) text


regexfirstMatchLength : String -> String -> Maybe Int
regexfirstMatchLength pattern text =
    case Regex.findAtMost 1 (regexFromString pattern) text of
        [] ->
            Nothing

        firstMatch :: _ ->
            Just (String.length firstMatch.match)


cleanDup : String -> String -> String
cleanDup c s =
    -- Remove any repetition of the character c in string s
    s |> Regex.replace (regexFromString (c ++ c ++ "+")) (always c)


emojis : List ( Int, String, String )
emojis =
    [ ( 1, String.fromChar '👍', "thumbsup" )
    , ( 2, String.fromChar '👎', "thumbsdown" )
    , ( 3, String.fromChar '🙂', "happy" )
    , ( 4, String.fromChar '🎉', " hooray" )
    , ( 5, String.fromChar '😕', "confused" )
    , ( 6, "❤️", "heart" )
    , ( 7, String.fromChar '🙏', "clap" )

    --, String.fromChar '❓'
    , ( 8, String.fromChar '👀', "eyes" )

    --, String.fromChar '🚜'
    ]


getEmoji : Int -> String
getEmoji type_ =
    case LE.getAt (type_ - 1) emojis of
        Just ( _, b, _ ) ->
            b

        Nothing ->
            ""


getEmojiName : Int -> String
getEmojiName type_ =
    case LE.getAt (type_ - 1) emojis of
        Just ( _, _, c ) ->
            c

        Nothing ->
            ""



-- Maybe


listToMaybe : List a -> Maybe (List a)
listToMaybe l =
    case l of
        [] ->
            Nothing

        _ ->
            Just l


{-| Returns the first value that is present, like the boolean `||`.
-}
mor : Maybe a -> Maybe a -> Maybe a
mor ma mb =
    case ma of
        Nothing ->
            mb

        Just _ ->
            ma


{-| Like using `Maybe.map f a |> withDefault default`
-}
unwrap : b -> (a -> b) -> Maybe a -> b
unwrap default f m =
    case m of
        Nothing ->
            default

        Just a ->
            f a


{-| Like using `Maybe.map f a |> withDefault Nothing |> withDefault default`
-}
unwrap2 : b -> (a -> Maybe b) -> Maybe a -> b
unwrap2 default f m =
    case m of
        Nothing ->
            default

        Just a ->
            f a |> Maybe.withDefault default



-- Colors


{-|

    Get Color style from string color

-}
colorAttr : String -> Html.Attribute msg
colorAttr color =
    let
        c =
            if String.startsWith "#" color then
                color

            else
                "var(--" ++ color ++ ")"

        c_h =
            if String.startsWith "#" color then
                color ++ "cc"

            else
                "var(--" ++ color ++ "-hover)"
    in
    attribute "style"
        ("background-color:"
            ++ c
            ++ "; color:"
            ++ colorToTextColor color
            ++ ";"
            ++ "--hover-color:"
            ++ c_h
            ++ ";"
        )


{-|

    Adjust the text color for dark background color

-}
colorToTextColor : String -> String
colorToTextColor color =
    if
        List.member (String.toUpper color)
            [ "#7FDBFF"
            , "#39CCCC"
            , "#01FF70"
            , "#FFDC00"
            , "#AAAAAA"
            , "#DDDDDD"
            ]
    then
        "#000"

    else if String.startsWith "white" color then
        "#000"

    else
        "#fff"



-- HTML


showIf : Bool -> Html msg -> Html msg
showIf a v =
    if a then
        v

    else
        text ""


showMaybe : Maybe a -> (a -> Html msg) -> Html msg
showMaybe a f =
    case a of
        Just x ->
            f x

        Nothing ->
            text ""
