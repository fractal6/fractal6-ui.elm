-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Enum.ProjectColumnType exposing (..)

import Json.Decode as Decode exposing (Decoder)


type ProjectColumnType
    = NormalColumn
    | NoStatusColumn


list : List ProjectColumnType
list =
    [ NormalColumn, NoStatusColumn ]


decoder : Decoder ProjectColumnType
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "NormalColumn" ->
                        Decode.succeed NormalColumn

                    "NoStatusColumn" ->
                        Decode.succeed NoStatusColumn

                    _ ->
                        Decode.fail ("Invalid ProjectColumnType type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : ProjectColumnType -> String
toString enum____ =
    case enum____ of
        NormalColumn ->
            "NormalColumn"

        NoStatusColumn ->
            "NoStatusColumn"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe ProjectColumnType
fromString enumString____ =
    case enumString____ of
        "NormalColumn" ->
            Just NormalColumn

        "NoStatusColumn" ->
            Just NoStatusColumn

        _ ->
            Nothing
