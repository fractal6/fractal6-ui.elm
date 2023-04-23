{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2023 Fractale Co

   This file is part of Fractale.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
-}


module Query.PatchTension exposing
    ( actionRequest
    , moveTension
    , patchComment
    , patchLiteral
    , publishBlob
    , pushCommentFilter
    , pushTensionPatch
    , setAssignee
    , setLabel
    )

import Bulk exposing (ActionForm, AssigneeForm, CommentPatchForm, Ev, LabelForm, TensionForm)
import Dict
import Extra exposing (ternary)
import Fractal.Enum.BlobOrderable as BlobOrderable
import Fractal.Enum.CommentOrderable as CommentOrderable
import Fractal.Enum.ContractOrderable as ContractOrderable
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.Blob
import Fractal.Object.Tension
import Fractal.Object.UpdateCommentPayload
import Fractal.Object.UpdateTensionPayload
import Fractal.Scalar
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (with)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Query.AddTension exposing (buildBlob, buildComment, buildEvents)
import Query.QueryContract exposing (contractPayload)
import Query.QueryNode exposing (tidPayload)
import Query.QueryTension exposing (blobPayload, commentPayload)
import RemoteData exposing (RemoteData)



{-
   Patch a single tension
-}


type alias PatchTensionPayload =
    { tension : Maybe (List (Maybe PatchTensionPayloadID)) }


tensionPushDecoder : Maybe PatchTensionPayload -> Maybe PatchTensionPayloadID
tensionPushDecoder data =
    data
        |> Maybe.andThen
            (\d ->
                d.tension
                    |> Maybe.map (List.filterMap identity)
                    |> withDefault []
                    |> List.head
            )


pushTensionPatch url form msg =
    makeGQLMutation url
        (Mutation.updateTension
            (patchTensionInputEncoder form)
            (SelectionSet.map PatchTensionPayload <|
                Fractal.Object.UpdateTensionPayload.tension identity
                    (SelectionSet.succeed PatchTensionPayloadID
                        |> with
                            (Fractal.Object.Tension.comments pushCommentFilter
                                commentPayload
                            )
                        |> with
                            (Fractal.Object.Tension.blobs pushBlobFilter
                                blobPayload
                            )
                    )
            )
        )
        (RemoteData.fromResult >> decodeResponse tensionPushDecoder >> msg)


pushCommentFilter a =
    { a
        | first = Present 1
        , order =
            Input.buildCommentOrder
                (\b -> { b | desc = Present CommentOrderable.CreatedAt })
                |> Present

        -- @debug: cant filter by createdBy for now !?
    }


pushBlobFilter : Fractal.Object.Tension.BlobsOptionalArguments -> Fractal.Object.Tension.BlobsOptionalArguments
pushBlobFilter a =
    { a
        | first = Present 1
        , order =
            Input.buildBlobOrder
                (\b -> { b | desc = Present BlobOrderable.CreatedAt })
                |> Present

        -- @debug: cant filter by createdBy for now !?
    }


patchTensionInputEncoder : TensionForm -> Mutation.UpdateTensionRequiredArguments
patchTensionInputEncoder f =
    let
        createdAt =
            Dict.get "createdAt" f.post |> withDefault "" |> Fractal.Scalar.DateTime

        updatedAt =
            Dict.get "updatedAt" f.post |> Maybe.map (\x -> Fractal.Scalar.DateTime x)

        message =
            -- new comment
            Dict.get "message" f.post |> Maybe.map String.trim |> (\x -> ternary (x == Just "") Nothing x)

        pce_m =
            if message == Nothing then
                []

            else
                [ Ev TensionEvent.CommentPushed "" "" ]

        inputReq =
            { filter =
                Input.buildTensionFilter
                    (\ft ->
                        { ft | id = Present [ encodeId f.id ] }
                    )
            }

        inputOpt =
            \_ ->
                { set =
                    Input.buildTensionPatch
                        (\s ->
                            { s
                                | updatedAt = fromMaybe updatedAt
                                , comments = buildComment createdAt f.uctx.username message
                                , history = buildEvents createdAt f.uctx.username (f.events ++ pce_m)
                                , blobs = buildBlob createdAt f.uctx.username f.blob_type f.users f.node f.post
                            }
                        )
                        |> Present
                , remove = Absent
                }
    in
    { input = Input.buildUpdateTensionInput inputReq inputOpt }



{-
   Update a tension title
-}


patchLiteral url form msg =
    -- Use the even old and new value for the updates...
    -- Can patch more than literal in reality...
    makeGQLMutation url
        (Mutation.updateTension
            (patchTensionInputEncoder form)
            (SelectionSet.map TensionIdPayload <|
                Fractal.Object.UpdateTensionPayload.tension identity tidPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse tensionIdDecoder >> msg)



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
patchCommentInputEncoder f =
    let
        -- new comment
        updatedAt =
            Dict.get "updatedAt" f.post |> Maybe.map (\x -> Fractal.Scalar.DateTime x)

        message =
            Dict.get "message" f.post |> Maybe.map String.trim

        inputReq =
            { filter =
                Input.buildCommentFilter
                    (\x ->
                        { x | id = Present [ encodeId f.id ] }
                    )
            }

        inputOpt =
            \_ ->
                { set =
                    Input.buildCommentPatch
                        (\s ->
                            { s
                                | updatedAt = fromMaybe updatedAt
                                , message = fromMaybe message
                            }
                        )
                        |> Present
                , remove = Absent
                }
    in
    { input = Input.buildUpdateCommentInput inputReq inputOpt }



{-
   Set assignee
-}


type alias TensionIdPayload =
    { tension : Maybe (List (Maybe IdPayload)) }


tensionIdDecoder : Maybe TensionIdPayload -> Maybe IdPayload
tensionIdDecoder data =
    case data of
        Just d ->
            d.tension
                |> Maybe.map
                    (\items ->
                        List.filterMap identity items
                    )
                |> withDefault []
                |> List.head

        Nothing ->
            Nothing


setAssignee url form msg =
    makeGQLMutation url
        (Mutation.updateTension
            (setAssigneeEncoder form)
            (SelectionSet.map TensionIdPayload <|
                Fractal.Object.UpdateTensionPayload.tension identity tidPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse tensionIdDecoder >> msg)


setAssigneeEncoder : AssigneeForm -> Mutation.UpdateTensionRequiredArguments
setAssigneeEncoder f =
    let
        createdAt =
            Dict.get "createdAt" f.post |> withDefault "" |> Fractal.Scalar.DateTime

        events =
            buildEvents createdAt f.uctx.username f.events

        inputReq =
            { filter =
                Input.buildTensionFilter
                    (\ft ->
                        { ft | id = Present [ encodeId f.tid ] }
                    )
            }

        patch =
            Input.buildTensionPatch
                (\s ->
                    { s
                        | assignees =
                            Present
                                [ Input.buildUserRef
                                    (\u -> { u | username = Present f.assignee.username })
                                ]
                        , history = ternary f.isNew events Absent
                    }
                )
                |> Present

        historyPatch =
            Input.buildTensionPatch (\s -> { s | history = events }) |> Present

        inputOpt =
            \_ ->
                { set = ternary f.isNew patch historyPatch
                , remove = ternary (not f.isNew) patch Absent
                }
    in
    { input = Input.buildUpdateTensionInput inputReq inputOpt }



{-
   Set label
-}


setLabel url form msg =
    makeGQLMutation url
        (Mutation.updateTension
            (setLabelEncoder form)
            (SelectionSet.map TensionIdPayload <|
                Fractal.Object.UpdateTensionPayload.tension identity tidPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse tensionIdDecoder >> msg)


setLabelEncoder : LabelForm -> Mutation.UpdateTensionRequiredArguments
setLabelEncoder f =
    let
        createdAt =
            Dict.get "createdAt" f.post |> withDefault "" |> Fractal.Scalar.DateTime

        events =
            buildEvents createdAt f.uctx.username f.events

        inputReq =
            { filter =
                Input.buildTensionFilter
                    (\ft ->
                        { ft | id = Present [ encodeId f.tid ] }
                    )
            }

        patch =
            Input.buildTensionPatch
                (\s ->
                    { s
                        | labels =
                            Present
                                [ Input.buildLabelRef
                                    (\u -> { u | id = Present (encodeId f.label.id) })
                                ]
                        , history = ternary f.isNew events Absent
                    }
                )
                |> Present

        historyPatch =
            Input.buildTensionPatch (\s -> { s | history = events }) |> Present

        inputOpt =
            \_ ->
                { set = ternary f.isNew patch historyPatch
                , remove = ternary (not f.isNew) patch Absent
                }
    in
    { input = Input.buildUpdateTensionInput inputReq inputOpt }



{-
   Move tension
-}


type alias TensionIdPayload2 =
    { tension : Maybe (List (Maybe TensionId)) }


tensionIdDecoder2 : Maybe TensionIdPayload2 -> Maybe TensionId
tensionIdDecoder2 data =
    case data of
        Just d ->
            d.tension
                |> Maybe.map
                    (\items ->
                        List.filterMap identity items
                    )
                |> withDefault []
                |> List.head

        Nothing ->
            Nothing


moveTension url form msg =
    makeGQLMutation url
        (Mutation.updateTension
            (setMoveEncoder form)
            (SelectionSet.map TensionIdPayload2 <|
                Fractal.Object.UpdateTensionPayload.tension identity <|
                    SelectionSet.map2 TensionId
                        (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
                        (Fractal.Object.Tension.contracts
                            (\args ->
                                { args
                                    | first = Present 1
                                    , order =
                                        Input.buildContractOrder
                                            (\x -> { x | desc = Present ContractOrderable.CreatedAt })
                                            |> Present
                                }
                            )
                            contractPayload
                        )
            )
        )
        (RemoteData.fromResult >> decodeResponse tensionIdDecoder2 >> msg)


{-| setMoveEncoder : MoveForm -> Mutation.UpdateTensionRequiredArguments
-}
setMoveEncoder f =
    let
        createdAt =
            Dict.get "createdAt" f.post |> withDefault "" |> Fractal.Scalar.DateTime

        message =
            -- new comment
            Dict.get "message" f.post |> Maybe.map String.trim |> (\x -> ternary (x == Just "") Nothing x)

        pce_m =
            if message == Nothing then
                []

            else
                [ Ev TensionEvent.CommentPushed "" "" ]

        inputReq =
            { filter =
                Input.buildTensionFilter
                    (\ft -> { ft | id = Present [ encodeId f.tid ] })
            }

        patch =
            Input.buildTensionPatch
                (\s ->
                    { s
                        | history = buildEvents createdAt f.uctx.username (f.events ++ pce_m)
                        , comments = buildComment createdAt f.uctx.username message
                    }
                )
                |> Present

        inputOpt =
            \_ ->
                { set = patch, remove = Absent }
    in
    { input = Input.buildUpdateTensionInput inputReq inputOpt }



{-
   Publish Blob
-}


type alias TensionBlobFlagPayload =
    { tension : Maybe (List (Maybe TensionBlobFlag)) }


publishBlobDecoder : Maybe TensionBlobFlagPayload -> Maybe TensionBlobFlag
publishBlobDecoder data =
    case data of
        Just d ->
            d.tension
                |> Maybe.map
                    (\items ->
                        List.filterMap identity items
                    )
                |> withDefault []
                |> List.head

        Nothing ->
            Nothing


publishBlob url bid form msg =
    makeGQLMutation url
        (Mutation.updateTension
            (publishBlobInputEncoder bid form)
            (SelectionSet.map TensionBlobFlagPayload <|
                Fractal.Object.UpdateTensionPayload.tension identity <|
                    SelectionSet.map2 TensionBlobFlag
                        Fractal.Object.Tension.title
                        (Fractal.Object.Tension.blobs (bidFilter bid) <|
                            SelectionSet.map BlobFlag (Fractal.Object.Blob.pushedFlag |> SelectionSet.map (Maybe.map (\x -> decodedTime x)))
                        )
            )
        )
        (RemoteData.fromResult >> decodeResponse publishBlobDecoder >> msg)


bidFilter : String -> Fractal.Object.Tension.BlobsOptionalArguments -> Fractal.Object.Tension.BlobsOptionalArguments
bidFilter bid a =
    { a
        | filter =
            Input.buildBlobFilter
                (\f ->
                    { f | id = Present [ encodeId bid ] }
                )
                |> Present
    }


publishBlobInputEncoder : String -> TensionForm -> Mutation.UpdateTensionRequiredArguments
publishBlobInputEncoder bid f =
    --@Debug: receiver and receiverid
    let
        createdAt =
            Dict.get "createdAt" f.post |> withDefault "" |> Fractal.Scalar.DateTime

        inputReq =
            { filter =
                Input.buildTensionFilter
                    (\ft ->
                        { ft | id = Present [ encodeId f.id ] }
                    )
            }

        inputOpt =
            \_ ->
                { set =
                    Input.buildTensionPatch
                        (\s ->
                            { s
                                | blobs =
                                    [ Input.buildBlobRef
                                        (\b ->
                                            --@debug: check if directive alter_RO works and if deep update works with dgraph
                                            --{ b | id = Present (encodeId bid), pushedFlag = createdAt |> Fractal.Scalar.DateTime |> Present }
                                            { b | id = Present (encodeId bid) }
                                        )
                                    ]
                                        |> Present
                                , history = buildEvents createdAt f.uctx.username f.events
                            }
                        )
                        |> Present
                , remove = Absent
                }
    in
    { input = Input.buildUpdateTensionInput inputReq inputOpt }



{-
   Doc Action request (see also ActionPanel)
   * archive doc
   * leave role
-}


{-| Just push a new event accompagned of a maybe

  - a comment
  - a blob id that point the blob to work on

-}
actionRequest url form msg =
    makeGQLMutation url
        (Mutation.updateTension
            (actionInputEncoder form)
            (SelectionSet.map TensionIdPayload <|
                Fractal.Object.UpdateTensionPayload.tension identity tidPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse tensionIdDecoder >> msg)


actionInputEncoder : ActionForm -> Mutation.UpdateTensionRequiredArguments
actionInputEncoder f =
    let
        createdAt =
            Dict.get "createdAt" f.post |> withDefault "" |> Fractal.Scalar.DateTime

        message =
            -- new comment
            Dict.get "message" f.post |> Maybe.map String.trim |> (\x -> ternary (x == Just "") Nothing x)

        pce_m =
            if message == Nothing then
                []

            else
                [ Ev TensionEvent.CommentPushed "" "" ]

        inputReq =
            { filter =
                Input.buildTensionFilter
                    (\ft -> { ft | id = Present [ encodeId f.tid ] })
            }

        inputOpt =
            \_ ->
                { set =
                    Input.buildTensionPatch
                        (\s ->
                            { s
                                | comments = buildComment createdAt f.uctx.username message
                                , history = buildEvents createdAt f.uctx.username (f.events ++ pce_m)
                                , blobs =
                                    if f.bid /= "" then
                                        [ Input.buildBlobRef (\b -> { b | id = Present (encodeId f.bid) }) ] |> Present

                                    else
                                        -- @debug: Get the last blob by default
                                        Absent
                            }
                        )
                        |> Present
                , remove = Absent
                }
    in
    { input = Input.buildUpdateTensionInput inputReq inputOpt }
