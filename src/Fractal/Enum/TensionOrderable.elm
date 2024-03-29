-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Enum.TensionOrderable exposing (..)

import Json.Decode as Decode exposing (Decoder)


type TensionOrderable
    = CreatedAt
    | UpdatedAt
    | Message
    | Emitterid
    | Receiverid
    | Title
    | N_comments


list : List TensionOrderable
list =
    [ CreatedAt, UpdatedAt, Message, Emitterid, Receiverid, Title, N_comments ]


decoder : Decoder TensionOrderable
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "createdAt" ->
                        Decode.succeed CreatedAt

                    "updatedAt" ->
                        Decode.succeed UpdatedAt

                    "message" ->
                        Decode.succeed Message

                    "emitterid" ->
                        Decode.succeed Emitterid

                    "receiverid" ->
                        Decode.succeed Receiverid

                    "title" ->
                        Decode.succeed Title

                    "n_comments" ->
                        Decode.succeed N_comments

                    _ ->
                        Decode.fail ("Invalid TensionOrderable type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : TensionOrderable -> String
toString enum____ =
    case enum____ of
        CreatedAt ->
            "createdAt"

        UpdatedAt ->
            "updatedAt"

        Message ->
            "message"

        Emitterid ->
            "emitterid"

        Receiverid ->
            "receiverid"

        Title ->
            "title"

        N_comments ->
            "n_comments"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe TensionOrderable
fromString enumString____ =
    case enumString____ of
        "createdAt" ->
            Just CreatedAt

        "updatedAt" ->
            Just UpdatedAt

        "message" ->
            Just Message

        "emitterid" ->
            Just Emitterid

        "receiverid" ->
            Just Receiverid

        "title" ->
            Just Title

        "n_comments" ->
            Just N_comments

        _ ->
            Nothing
