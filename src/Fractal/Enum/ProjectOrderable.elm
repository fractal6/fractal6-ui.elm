-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Enum.ProjectOrderable exposing (..)

import Json.Decode as Decode exposing (Decoder)


type ProjectOrderable
    = CreatedAt
    | UpdatedAt
    | Rootnameid
    | Parentnameid
    | Nameid
    | Name
    | Description


list : List ProjectOrderable
list =
    [ CreatedAt, UpdatedAt, Rootnameid, Parentnameid, Nameid, Name, Description ]


decoder : Decoder ProjectOrderable
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "createdAt" ->
                        Decode.succeed CreatedAt

                    "updatedAt" ->
                        Decode.succeed UpdatedAt

                    "rootnameid" ->
                        Decode.succeed Rootnameid

                    "parentnameid" ->
                        Decode.succeed Parentnameid

                    "nameid" ->
                        Decode.succeed Nameid

                    "name" ->
                        Decode.succeed Name

                    "description" ->
                        Decode.succeed Description

                    _ ->
                        Decode.fail ("Invalid ProjectOrderable type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : ProjectOrderable -> String
toString enum____ =
    case enum____ of
        CreatedAt ->
            "createdAt"

        UpdatedAt ->
            "updatedAt"

        Rootnameid ->
            "rootnameid"

        Parentnameid ->
            "parentnameid"

        Nameid ->
            "nameid"

        Name ->
            "name"

        Description ->
            "description"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe ProjectOrderable
fromString enumString____ =
    case enumString____ of
        "createdAt" ->
            Just CreatedAt

        "updatedAt" ->
            Just UpdatedAt

        "rootnameid" ->
            Just Rootnameid

        "parentnameid" ->
            Just Parentnameid

        "nameid" ->
            Just Nameid

        "name" ->
            Just Name

        "description" ->
            Just Description

        _ ->
            Nothing
