-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Enum.ProjectColumnHasFilter exposing (..)

import Json.Decode as Decode exposing (Decoder)


type ProjectColumnHasFilter
    = Name
    | Description
    | Color
    | Pos
    | Col_type
    | Cards
    | Project
    | Tensions
    | Drafts


list : List ProjectColumnHasFilter
list =
    [ Name, Description, Color, Pos, Col_type, Cards, Project, Tensions, Drafts ]


decoder : Decoder ProjectColumnHasFilter
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "name" ->
                        Decode.succeed Name

                    "description" ->
                        Decode.succeed Description

                    "color" ->
                        Decode.succeed Color

                    "pos" ->
                        Decode.succeed Pos

                    "col_type" ->
                        Decode.succeed Col_type

                    "cards" ->
                        Decode.succeed Cards

                    "project" ->
                        Decode.succeed Project

                    "tensions" ->
                        Decode.succeed Tensions

                    "drafts" ->
                        Decode.succeed Drafts

                    _ ->
                        Decode.fail ("Invalid ProjectColumnHasFilter type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : ProjectColumnHasFilter -> String
toString enum____ =
    case enum____ of
        Name ->
            "name"

        Description ->
            "description"

        Color ->
            "color"

        Pos ->
            "pos"

        Col_type ->
            "col_type"

        Cards ->
            "cards"

        Project ->
            "project"

        Tensions ->
            "tensions"

        Drafts ->
            "drafts"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe ProjectColumnHasFilter
fromString enumString____ =
    case enumString____ of
        "name" ->
            Just Name

        "description" ->
            Just Description

        "color" ->
            Just Color

        "pos" ->
            Just Pos

        "col_type" ->
            Just Col_type

        "cards" ->
            Just Cards

        "project" ->
            Just Project

        "tensions" ->
            Just Tensions

        "drafts" ->
            Just Drafts

        _ ->
            Nothing
