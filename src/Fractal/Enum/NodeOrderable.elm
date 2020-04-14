-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Enum.NodeOrderable exposing (..)

import Json.Decode as Decode exposing (Decoder)


type NodeOrderable
    = CreatedAt
    | Name
    | Nameid
    | Rootnameid
    | N_tensions_out
    | N_tensions_in
    | N_children
    | Skills


list : List NodeOrderable
list =
    [ CreatedAt, Name, Nameid, Rootnameid, N_tensions_out, N_tensions_in, N_children, Skills ]


decoder : Decoder NodeOrderable
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "createdAt" ->
                        Decode.succeed CreatedAt

                    "name" ->
                        Decode.succeed Name

                    "nameid" ->
                        Decode.succeed Nameid

                    "rootnameid" ->
                        Decode.succeed Rootnameid

                    "n_tensions_out" ->
                        Decode.succeed N_tensions_out

                    "n_tensions_in" ->
                        Decode.succeed N_tensions_in

                    "n_children" ->
                        Decode.succeed N_children

                    "skills" ->
                        Decode.succeed Skills

                    _ ->
                        Decode.fail ("Invalid NodeOrderable type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : NodeOrderable -> String
toString enum =
    case enum of
        CreatedAt ->
            "createdAt"

        Name ->
            "name"

        Nameid ->
            "nameid"

        Rootnameid ->
            "rootnameid"

        N_tensions_out ->
            "n_tensions_out"

        N_tensions_in ->
            "n_tensions_in"

        N_children ->
            "n_children"

        Skills ->
            "skills"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe NodeOrderable
fromString enumString =
    case enumString of
        "createdAt" ->
            Just CreatedAt

        "name" ->
            Just Name

        "nameid" ->
            Just Nameid

        "rootnameid" ->
            Just Rootnameid

        "n_tensions_out" ->
            Just N_tensions_out

        "n_tensions_in" ->
            Just N_tensions_in

        "n_children" ->
            Just N_children

        "skills" ->
            Just Skills

        _ ->
            Nothing
