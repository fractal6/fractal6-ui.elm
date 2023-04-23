-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Enum.ProjectTensionHasFilter exposing (..)

import Json.Decode as Decode exposing (Decoder)


type ProjectTensionHasFilter
    = Tension
    | Pos
    | Pc


list : List ProjectTensionHasFilter
list =
    [ Tension, Pos, Pc ]


decoder : Decoder ProjectTensionHasFilter
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "tension" ->
                        Decode.succeed Tension

                    "pos" ->
                        Decode.succeed Pos

                    "pc" ->
                        Decode.succeed Pc

                    _ ->
                        Decode.fail ("Invalid ProjectTensionHasFilter type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : ProjectTensionHasFilter -> String
toString enum____ =
    case enum____ of
        Tension ->
            "tension"

        Pos ->
            "pos"

        Pc ->
            "pc"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe ProjectTensionHasFilter
fromString enumString____ =
    case enumString____ of
        "tension" ->
            Just Tension

        "pos" ->
            Just Pos

        "pc" ->
            Just Pc

        _ ->
            Nothing