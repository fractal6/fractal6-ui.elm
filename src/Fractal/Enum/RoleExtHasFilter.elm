-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Enum.RoleExtHasFilter exposing (..)

import Json.Decode as Decode exposing (Decoder)


type RoleExtHasFilter
    = Rootnameid
    | Name
    | About
    | Role_type
    | Color
    | Mandate
    | Roles
    | Nodes


list : List RoleExtHasFilter
list =
    [ Rootnameid, Name, About, Role_type, Color, Mandate, Roles, Nodes ]


decoder : Decoder RoleExtHasFilter
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "rootnameid" ->
                        Decode.succeed Rootnameid

                    "name" ->
                        Decode.succeed Name

                    "about" ->
                        Decode.succeed About

                    "role_type" ->
                        Decode.succeed Role_type

                    "color" ->
                        Decode.succeed Color

                    "mandate" ->
                        Decode.succeed Mandate

                    "roles" ->
                        Decode.succeed Roles

                    "nodes" ->
                        Decode.succeed Nodes

                    _ ->
                        Decode.fail ("Invalid RoleExtHasFilter type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : RoleExtHasFilter -> String
toString enum____ =
    case enum____ of
        Rootnameid ->
            "rootnameid"

        Name ->
            "name"

        About ->
            "about"

        Role_type ->
            "role_type"

        Color ->
            "color"

        Mandate ->
            "mandate"

        Roles ->
            "roles"

        Nodes ->
            "nodes"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe RoleExtHasFilter
fromString enumString____ =
    case enumString____ of
        "rootnameid" ->
            Just Rootnameid

        "name" ->
            Just Name

        "about" ->
            Just About

        "role_type" ->
            Just Role_type

        "color" ->
            Just Color

        "mandate" ->
            Just Mandate

        "roles" ->
            Just Roles

        "nodes" ->
            Just Nodes

        _ ->
            Nothing
