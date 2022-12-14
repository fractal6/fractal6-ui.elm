-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Enum.ReactionOrderable exposing (..)

import Json.Decode as Decode exposing (Decoder)


type ReactionOrderable
    = Reactionid
    | Type_


list : List ReactionOrderable
list =
    [ Reactionid, Type_ ]


decoder : Decoder ReactionOrderable
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "reactionid" ->
                        Decode.succeed Reactionid

                    "type_" ->
                        Decode.succeed Type_

                    _ ->
                        Decode.fail ("Invalid ReactionOrderable type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : ReactionOrderable -> String
toString enum____ =
    case enum____ of
        Reactionid ->
            "reactionid"

        Type_ ->
            "type_"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe ReactionOrderable
fromString enumString____ =
    case enumString____ of
        "reactionid" ->
            Just Reactionid

        "type_" ->
            Just Type_

        _ ->
            Nothing
