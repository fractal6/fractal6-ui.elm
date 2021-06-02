-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Enum.ContractType exposing (..)

import Json.Decode as Decode exposing (Decoder)


type ContractType
    = AnyCoordoDual
    | AnyParticipants
    | AnyCoordoSource
    | AnyCoordoTarget


list : List ContractType
list =
    [ AnyCoordoDual, AnyParticipants, AnyCoordoSource, AnyCoordoTarget ]


decoder : Decoder ContractType
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "AnyCoordoDual" ->
                        Decode.succeed AnyCoordoDual

                    "AnyParticipants" ->
                        Decode.succeed AnyParticipants

                    "AnyCoordoSource" ->
                        Decode.succeed AnyCoordoSource

                    "AnyCoordoTarget" ->
                        Decode.succeed AnyCoordoTarget

                    _ ->
                        Decode.fail ("Invalid ContractType type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : ContractType -> String
toString enum =
    case enum of
        AnyCoordoDual ->
            "AnyCoordoDual"

        AnyParticipants ->
            "AnyParticipants"

        AnyCoordoSource ->
            "AnyCoordoSource"

        AnyCoordoTarget ->
            "AnyCoordoTarget"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe ContractType
fromString enumString =
    case enumString of
        "AnyCoordoDual" ->
            Just AnyCoordoDual

        "AnyParticipants" ->
            Just AnyParticipants

        "AnyCoordoSource" ->
            Just AnyCoordoSource

        "AnyCoordoTarget" ->
            Just AnyCoordoTarget

        _ ->
            Nothing