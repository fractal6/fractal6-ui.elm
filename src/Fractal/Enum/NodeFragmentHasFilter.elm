-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Enum.NodeFragmentHasFilter exposing (..)

import Json.Decode as Decode exposing (Decoder)


type NodeFragmentHasFilter
    = Nameid
    | Name
    | About
    | Mandate
    | Skills
    | Children
    | Visibility
    | Mode
    | Type_
    | First_link
    | Second_link
    | Role_ext
    | Role_type
    | Color


list : List NodeFragmentHasFilter
list =
    [ Nameid, Name, About, Mandate, Skills, Children, Visibility, Mode, Type_, First_link, Second_link, Role_ext, Role_type, Color ]


decoder : Decoder NodeFragmentHasFilter
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "nameid" ->
                        Decode.succeed Nameid

                    "name" ->
                        Decode.succeed Name

                    "about" ->
                        Decode.succeed About

                    "mandate" ->
                        Decode.succeed Mandate

                    "skills" ->
                        Decode.succeed Skills

                    "children" ->
                        Decode.succeed Children

                    "visibility" ->
                        Decode.succeed Visibility

                    "mode" ->
                        Decode.succeed Mode

                    "type_" ->
                        Decode.succeed Type_

                    "first_link" ->
                        Decode.succeed First_link

                    "second_link" ->
                        Decode.succeed Second_link

                    "role_ext" ->
                        Decode.succeed Role_ext

                    "role_type" ->
                        Decode.succeed Role_type

                    "color" ->
                        Decode.succeed Color

                    _ ->
                        Decode.fail ("Invalid NodeFragmentHasFilter type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : NodeFragmentHasFilter -> String
toString enum____ =
    case enum____ of
        Nameid ->
            "nameid"

        Name ->
            "name"

        About ->
            "about"

        Mandate ->
            "mandate"

        Skills ->
            "skills"

        Children ->
            "children"

        Visibility ->
            "visibility"

        Mode ->
            "mode"

        Type_ ->
            "type_"

        First_link ->
            "first_link"

        Second_link ->
            "second_link"

        Role_ext ->
            "role_ext"

        Role_type ->
            "role_type"

        Color ->
            "color"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe NodeFragmentHasFilter
fromString enumString____ =
    case enumString____ of
        "nameid" ->
            Just Nameid

        "name" ->
            Just Name

        "about" ->
            Just About

        "mandate" ->
            Just Mandate

        "skills" ->
            Just Skills

        "children" ->
            Just Children

        "visibility" ->
            Just Visibility

        "mode" ->
            Just Mode

        "type_" ->
            Just Type_

        "first_link" ->
            Just First_link

        "second_link" ->
            Just Second_link

        "role_ext" ->
            Just Role_ext

        "role_type" ->
            Just Role_type

        "color" ->
            Just Color

        _ ->
            Nothing
