-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Enum.ProjectFieldValueHasFilter exposing (..)

import Json.Decode as Decode exposing (Decoder)


type ProjectFieldValueHasFilter
    = Field
    | Value
    | Pos


list : List ProjectFieldValueHasFilter
list =
    [ Field, Value, Pos ]


decoder : Decoder ProjectFieldValueHasFilter
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "field" ->
                        Decode.succeed Field

                    "value" ->
                        Decode.succeed Value

                    "pos" ->
                        Decode.succeed Pos

                    _ ->
                        Decode.fail ("Invalid ProjectFieldValueHasFilter type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : ProjectFieldValueHasFilter -> String
toString enum____ =
    case enum____ of
        Field ->
            "field"

        Value ->
            "value"

        Pos ->
            "pos"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe ProjectFieldValueHasFilter
fromString enumString____ =
    case enumString____ of
        "field" ->
            Just Field

        "value" ->
            Just Value

        "pos" ->
            Just Pos

        _ ->
            Nothing
