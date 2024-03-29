-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Enum.ProjectFieldHasFilter exposing (..)

import Json.Decode as Decode exposing (Decoder)


type ProjectFieldHasFilter
    = Field_type
    | IsVisible
    | Values


list : List ProjectFieldHasFilter
list =
    [ Field_type, IsVisible, Values ]


decoder : Decoder ProjectFieldHasFilter
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "field_type" ->
                        Decode.succeed Field_type

                    "isVisible" ->
                        Decode.succeed IsVisible

                    "values" ->
                        Decode.succeed Values

                    _ ->
                        Decode.fail ("Invalid ProjectFieldHasFilter type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : ProjectFieldHasFilter -> String
toString enum____ =
    case enum____ of
        Field_type ->
            "field_type"

        IsVisible ->
            "isVisible"

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
fromString : String -> Maybe ProjectFieldHasFilter
fromString enumString____ =
    case enumString____ of
        "field_type" ->
            Just Field_type

        "isVisible" ->
            Just IsVisible

        "values" ->
            Just Values

        _ ->
            Nothing
