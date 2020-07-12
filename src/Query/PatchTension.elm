module Query.PatchTension exposing (patchComment, pushTensionComment)

import Dict exposing (Dict)
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.Label
import Fractal.Object.Tension
import Fractal.Object.UpdateCommentPayload
import Fractal.Object.UpdateTensionPayload
import Fractal.Query as Query
import Fractal.Scalar
import Fractal.ScalarCodecs
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Maybe exposing (withDefault)
import ModelCommon exposing (CommentPatchForm, TensionPatchForm)
import ModelSchema exposing (..)
import Query.QueryTension exposing (commentPayload)
import RemoteData exposing (RemoteData)



{-
   Update a comment
-}


type alias PatchCommentPayload =
    { comment : Maybe (List (Maybe Comment)) }


commentPatchDecoder : Maybe PatchCommentPayload -> Maybe Comment
commentPatchDecoder data =
    case data of
        Just d ->
            d.comment
                |> Maybe.map
                    (\items ->
                        List.filterMap identity items
                    )
                |> withDefault []
                |> List.head

        Nothing ->
            Nothing


patchComment url form msg =
    makeGQLMutation url
        (Mutation.updateComment
            (patchCommentInputEncoder form)
            (SelectionSet.map PatchCommentPayload <|
                Fractal.Object.UpdateCommentPayload.comment identity commentPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse commentPatchDecoder >> msg)


patchCommentInputEncoder : CommentPatchForm -> Mutation.UpdateCommentRequiredArguments
patchCommentInputEncoder form =
    let
        -- new comment
        updatedAt =
            Dict.get "updatedAt" form.post |> Maybe.map (\x -> Fractal.Scalar.DateTime x)

        message =
            Dict.get "message" form.post

        patchRequired =
            { filter =
                Input.buildCommentFilter
                    (\f ->
                        { f | id = Present [ encodeId form.id ] }
                    )
            }

        patchOpts =
            \x ->
                { set =
                    Input.buildCommentPatch
                        (\s ->
                            { s | message = fromMaybe message }
                        )
                        |> Present
                , remove = Absent
                }
    in
    { input =
        Input.buildUpdateCommentInput patchRequired patchOpts
    }



{-
   Patch a single tension
-}


type alias PatchTensionIdPayload =
    { tension : Maybe (List (Maybe IdPayload)) }


tensionPatchDecoder : Maybe PatchTensionIdPayload -> Maybe IdPayload
tensionPatchDecoder data =
    case data of
        Just d ->
            d.tension
                |> Maybe.map
                    (\items ->
                        List.filterMap identity items
                    )
                |> withDefault []
                -- Ignore the message_action id
                |> List.head

        Nothing ->
            Nothing


pushTensionComment url form msg =
    makeGQLMutation url
        (Mutation.updateTension
            (pushTensionCommentInputEncoder form)
            (SelectionSet.map PatchTensionIdPayload <|
                Fractal.Object.UpdateTensionPayload.tension identity <|
                    (SelectionSet.succeed IdPayload
                        |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
                    )
            )
        )
        (RemoteData.fromResult >> decodeResponse tensionPatchDecoder >> msg)


pushTensionCommentInputEncoder : TensionPatchForm -> Mutation.UpdateTensionRequiredArguments
pushTensionCommentInputEncoder form =
    let
        -- new comment
        createdAt =
            Dict.get "createdAt" form.post |> Maybe.map (\x -> Fractal.Scalar.DateTime x)

        msg1 =
            Dict.get "message" form.post

        msg2 =
            Dict.get "message_action" form.post

        messages =
            if msg1 /= Nothing || msg2 /= Nothing then
                Just ( msg1, msg2 )

            else
                Nothing

        patchRequired =
            { filter =
                Input.buildTensionFilter
                    (\f ->
                        { f | id = Present [ encodeId form.id ] }
                    )
            }

        patchOpts =
            \x ->
                { set =
                    Input.buildTensionPatch
                        (\s ->
                            { s
                                | status = fromMaybe form.status
                                , comments =
                                    messages
                                        |> Maybe.map
                                            (\( maybeMsg1, maybeMsg2 ) ->
                                                [ Maybe.map
                                                    (\message ->
                                                        Input.buildCommentRef
                                                            (\c ->
                                                                { c
                                                                    | createdAt = fromMaybe createdAt
                                                                    , createdBy =
                                                                        Input.buildUserRef
                                                                            (\u -> { u | username = Present form.uctx.username })
                                                                            |> Present
                                                                    , message = Present message
                                                                }
                                                            )
                                                    )
                                                    maybeMsg1
                                                , Maybe.map
                                                    (\message_action ->
                                                        Input.buildCommentRef
                                                            (\c ->
                                                                { c
                                                                    | createdAt = fromMaybe createdAt
                                                                    , createdBy =
                                                                        Input.buildUserRef
                                                                            (\u -> { u | username = Present form.uctx.username })
                                                                            |> Present
                                                                    , message = Present message_action
                                                                }
                                                            )
                                                    )
                                                    maybeMsg2
                                                ]
                                                    |> List.filterMap identity
                                            )
                                        |> fromMaybe
                            }
                        )
                        |> Present
                , remove = Absent
                }
    in
    { input =
        Input.buildUpdateTensionInput patchRequired patchOpts
    }
