-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Enum.ProjectCardHasFilter exposing (..)

import Json.Decode as Decode exposing (Decoder)


type ProjectCardHasFilter
    = Pos
    | Card
    | Pc
    | Values


list : List ProjectCardHasFilter
list =
    [ Pos, Card, Pc, Values ]


decoder : Decoder ProjectCardHasFilter
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "pos" ->
                        Decode.succeed Pos

                    "card" ->
                        Decode.succeed Card

                    "pc" ->
                        Decode.succeed Pc

                    "values" ->
                        Decode.succeed Values

                    _ ->
                        Decode.fail ("Invalid ProjectCardHasFilter type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : ProjectCardHasFilter -> String
toString enum____ =
    case enum____ of
        Pos ->
            "pos"

        Card ->
            "card"

        Pc ->
            "pc"

        Values ->
            "values"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe ProjectCardHasFilter
fromString enumString____ =
    case enumString____ of
        "pos" ->
            Just Pos

        "card" ->
            Just Card

        "pc" ->
            Just Pc

        "values" ->
            Just Values

        _ ->
            Nothing
