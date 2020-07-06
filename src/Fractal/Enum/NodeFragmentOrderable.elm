-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Enum.NodeFragmentOrderable exposing (..)

import Json.Decode as Decode exposing (Decoder)


type NodeFragmentOrderable
    = Name
    | Nameid
    | About
    | First_link
    | Second_link
    | Skills


list : List NodeFragmentOrderable
list =
    [ Name, Nameid, About, First_link, Second_link, Skills ]


decoder : Decoder NodeFragmentOrderable
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "name" ->
                        Decode.succeed Name

                    "nameid" ->
                        Decode.succeed Nameid

                    "about" ->
                        Decode.succeed About

                    "first_link" ->
                        Decode.succeed First_link

                    "second_link" ->
                        Decode.succeed Second_link

                    "skills" ->
                        Decode.succeed Skills

                    _ ->
                        Decode.fail ("Invalid NodeFragmentOrderable type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : NodeFragmentOrderable -> String
toString enum =
    case enum of
        Name ->
            "name"

        Nameid ->
            "nameid"

        About ->
            "about"

        First_link ->
            "first_link"

        Second_link ->
            "second_link"

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
fromString : String -> Maybe NodeFragmentOrderable
fromString enumString =
    case enumString of
        "name" ->
            Just Name

        "nameid" ->
            Just Nameid

        "about" ->
            Just About

        "first_link" ->
            Just First_link

        "second_link" ->
            Just Second_link

        "skills" ->
            Just Skills

        _ ->
            Nothing
