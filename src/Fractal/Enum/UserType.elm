-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Enum.UserType exposing (..)

import Json.Decode as Decode exposing (Decoder)


type UserType
    = Regular
    | Pro
    | Root


list : List UserType
list =
    [ Regular, Pro, Root ]


decoder : Decoder UserType
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "Regular" ->
                        Decode.succeed Regular

                    "Pro" ->
                        Decode.succeed Pro

                    "Root" ->
                        Decode.succeed Root

                    _ ->
                        Decode.fail ("Invalid UserType type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : UserType -> String
toString enum____ =
    case enum____ of
        Regular ->
            "Regular"

        Pro ->
            "Pro"

        Root ->
            "Root"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe UserType
fromString enumString____ =
    case enumString____ of
        "Regular" ->
            Just Regular

        "Pro" ->
            Just Pro

        "Root" ->
            Just Root

        _ ->
            Nothing