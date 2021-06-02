-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Enum.TensionHasFilter exposing (..)

import Json.Decode as Decode exposing (Decoder)


type TensionHasFilter
    = CreatedBy
    | CreatedAt
    | UpdatedAt
    | Message
    | Emitterid
    | Emitter
    | Receiverid
    | Receiver
    | Nth
    | Title
    | Type_
    | Status
    | Assignees
    | Labels
    | Comments
    | Action
    | Blobs
    | Contracts
    | History
    | N_comments
    | N_blobs


list : List TensionHasFilter
list =
    [ CreatedBy, CreatedAt, UpdatedAt, Message, Emitterid, Emitter, Receiverid, Receiver, Nth, Title, Type_, Status, Assignees, Labels, Comments, Action, Blobs, Contracts, History, N_comments, N_blobs ]


decoder : Decoder TensionHasFilter
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "createdBy" ->
                        Decode.succeed CreatedBy

                    "createdAt" ->
                        Decode.succeed CreatedAt

                    "updatedAt" ->
                        Decode.succeed UpdatedAt

                    "message" ->
                        Decode.succeed Message

                    "emitterid" ->
                        Decode.succeed Emitterid

                    "emitter" ->
                        Decode.succeed Emitter

                    "receiverid" ->
                        Decode.succeed Receiverid

                    "receiver" ->
                        Decode.succeed Receiver

                    "nth" ->
                        Decode.succeed Nth

                    "title" ->
                        Decode.succeed Title

                    "type_" ->
                        Decode.succeed Type_

                    "status" ->
                        Decode.succeed Status

                    "assignees" ->
                        Decode.succeed Assignees

                    "labels" ->
                        Decode.succeed Labels

                    "comments" ->
                        Decode.succeed Comments

                    "action" ->
                        Decode.succeed Action

                    "blobs" ->
                        Decode.succeed Blobs

                    "contracts" ->
                        Decode.succeed Contracts

                    "history" ->
                        Decode.succeed History

                    "n_comments" ->
                        Decode.succeed N_comments

                    "n_blobs" ->
                        Decode.succeed N_blobs

                    _ ->
                        Decode.fail ("Invalid TensionHasFilter type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : TensionHasFilter -> String
toString enum =
    case enum of
        CreatedBy ->
            "createdBy"

        CreatedAt ->
            "createdAt"

        UpdatedAt ->
            "updatedAt"

        Message ->
            "message"

        Emitterid ->
            "emitterid"

        Emitter ->
            "emitter"

        Receiverid ->
            "receiverid"

        Receiver ->
            "receiver"

        Nth ->
            "nth"

        Title ->
            "title"

        Type_ ->
            "type_"

        Status ->
            "status"

        Assignees ->
            "assignees"

        Labels ->
            "labels"

        Comments ->
            "comments"

        Action ->
            "action"

        Blobs ->
            "blobs"

        Contracts ->
            "contracts"

        History ->
            "history"

        N_comments ->
            "n_comments"

        N_blobs ->
            "n_blobs"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe TensionHasFilter
fromString enumString =
    case enumString of
        "createdBy" ->
            Just CreatedBy

        "createdAt" ->
            Just CreatedAt

        "updatedAt" ->
            Just UpdatedAt

        "message" ->
            Just Message

        "emitterid" ->
            Just Emitterid

        "emitter" ->
            Just Emitter

        "receiverid" ->
            Just Receiverid

        "receiver" ->
            Just Receiver

        "nth" ->
            Just Nth

        "title" ->
            Just Title

        "type_" ->
            Just Type_

        "status" ->
            Just Status

        "assignees" ->
            Just Assignees

        "labels" ->
            Just Labels

        "comments" ->
            Just Comments

        "action" ->
            Just Action

        "blobs" ->
            Just Blobs

        "contracts" ->
            Just Contracts

        "history" ->
            Just History

        "n_comments" ->
            Just N_comments

        "n_blobs" ->
            Just N_blobs

        _ ->
            Nothing