-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Enum.NodeMode exposing (..)

import Json.Decode as Decode exposing (Decoder)


type NodeMode
    = Coordinated
    | Agile


list : List NodeMode
list =
    [ Coordinated, Agile ]


decoder : Decoder NodeMode
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "Coordinated" ->
                        Decode.succeed Coordinated

                    "Agile" ->
                        Decode.succeed Agile

                    _ ->
                        Decode.fail ("Invalid NodeMode type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : NodeMode -> String
toString enum____ =
    case enum____ of
        Coordinated ->
            "Coordinated"

        Agile ->
            "Agile"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe NodeMode
fromString enumString____ =
    case enumString____ of
        "Coordinated" ->
            Just Coordinated

        "Agile" ->
            Just Agile

        _ ->
            Nothing
