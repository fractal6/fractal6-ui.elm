module Query.PatchTension exposing
    ( archiveDoc
    , patchComment
    , patchTitle
    , publishBlob
    , pushTensionPatch
    , setAssignee
    )

import Components.ActionPanel exposing (ActionForm)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Fractal.Enum.BlobOrderable as BlobOrderable
import Fractal.Enum.CommentOrderable as CommentOrderable
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.Blob
import Fractal.Object.Comment
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
import ModelCommon exposing (AssigneeForm, CommentPatchForm, TensionPatchForm)
import ModelSchema exposing (..)
import Query.AddTension exposing (buildBlob, buildComment, buildEvent)
import Query.QueryTension exposing (blobPayload, commentPayload)
import RemoteData exposing (RemoteData)



{-
   Patch a single tension
-}


type alias PatchTensionPayload =
    { tension : Maybe (List (Maybe PatchTensionPayloadID)) }


tensionPushDecoder : Maybe PatchTensionPayload -> Maybe PatchTensionPayloadID
tensionPushDecoder data =
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


pushTensionPatch url form msg =
    makeGQLMutation url
        (Mutation.updateTension
            (patchTensionInputEncoder form)
            (SelectionSet.map PatchTensionPayload <|
                Fractal.Object.UpdateTensionPayload.tension identity <|
                    (SelectionSet.succeed PatchTensionPayloadID
                        |> with
                            (Fractal.Object.Tension.comments (pushCommentFilter form) <|
                                commentPayload
                            )
                        |> with
                            (Fractal.Object.Tension.blobs (pushBlobFilter form) <|
                                blobPayload
                            )
                    )
            )
        )
        (RemoteData.fromResult >> decodeResponse tensionPushDecoder >> msg)


pushCommentFilter : TensionPatchForm -> Fractal.Object.Tension.CommentsOptionalArguments -> Fractal.Object.Tension.CommentsOptionalArguments
pushCommentFilter f a =
    { a
        | first = Present 1
        , order =
            Input.buildCommentOrder
                (\b -> { b | desc = Present CommentOrderable.CreatedAt })
                |> Present

        -- @debug: cant filter by createdBy for now !?
    }


pushBlobFilter : TensionPatchForm -> Fractal.Object.Tension.BlobsOptionalArguments -> Fractal.Object.Tension.BlobsOptionalArguments
pushBlobFilter f a =
    { a
        | first = Present 1
        , order =
            Input.buildBlobOrder
                (\b -> { b | desc = Present BlobOrderable.CreatedAt })
                |> Present

        -- @debug: cant filter by createdBy for now !?
    }


patchTensionInputEncoder : TensionPatchForm -> Mutation.UpdateTensionRequiredArguments
patchTensionInputEncoder f =
    --@Debug: receiver and receiverid
    let
        createdAt =
            Dict.get "createdAt" f.post |> withDefault ""

        updatedAt =
            Dict.get "updatedAt" f.post |> Maybe.map (\x -> Fractal.Scalar.DateTime x)

        title =
            Dict.get "title" f.post

        message =
            -- new comment
            Dict.get "message" f.post

        inputReq =
            { filter =
                Input.buildTensionFilter
                    (\ft ->
                        { ft | id = Present [ encodeId f.id ] }
                    )
            }

        inputOpt =
            \x ->
                { set =
                    Input.buildTensionPatch
                        (\s ->
                            { s
                                | updatedAt = fromMaybe updatedAt
                                , title = fromMaybe title
                                , status = fromMaybe f.status
                                , action = fromMaybe f.action
                                , type_ = fromMaybe f.tension_type
                                , comments = buildComment createdAt f.uctx.username message
                                , history = buildEvent createdAt f.uctx.username f.events_type f.post
                                , blobs = buildBlob createdAt f.uctx.username f.blob_type f.users f.node f.post
                            }
                        )
                        |> Present
                , remove = Absent
                }
    in
    { input =
        Input.buildUpdateTensionInput inputReq inputOpt
    }



{-
   Update a tension title
-}


type alias PatchTitlePayload =
    { tension : Maybe (List (Maybe TensionTitle)) }


type alias TensionTitle =
    { title : String }


titlePatchDecoder : Maybe PatchTitlePayload -> Maybe String
titlePatchDecoder data =
    case data of
        Just d ->
            d.tension
                |> Maybe.map
                    (\items ->
                        List.filterMap identity items
                    )
                |> withDefault []
                |> List.head
                |> Maybe.map (\t -> t.title)

        Nothing ->
            Nothing


patchTitle url form msg =
    makeGQLMutation url
        (Mutation.updateTension
            (patchTensionInputEncoder form)
            (SelectionSet.map PatchTitlePayload <|
                Fractal.Object.UpdateTensionPayload.tension identity <|
                    SelectionSet.map TensionTitle Fractal.Object.Tension.title
            )
        )
        (RemoteData.fromResult >> decodeResponse titlePatchDecoder >> msg)



{-
   set assignee
-}


type alias AssigneePayload =
    { tension : Maybe (List (Maybe IdPayload)) }


assigneeDecoder : Maybe AssigneePayload -> Maybe IdPayload
assigneeDecoder data =
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
            (SelectionSet.map AssigneePayload <|
                Fractal.Object.UpdateTensionPayload.tension identity <|
                    SelectionSet.map IdPayload
                        (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
            )
        )
        (RemoteData.fromResult >> decodeResponse assigneeDecoder >> msg)


setAssigneeEncoder : AssigneeForm -> Mutation.UpdateTensionRequiredArguments
setAssigneeEncoder f =
    let
        createdAt =
            Dict.get "createdAt" f.post |> withDefault ""

        events =
            buildEvent createdAt f.uctx.username f.events_type f.post

        inputReq =
            { filter =
                Input.buildTensionFilter
                    (\ft ->
                        { ft | id = Present [ encodeId f.tid ] }
                    )
            }

        userPatch =
            Input.buildTensionPatch
                (\s ->
                    { s
                        | assignees =
                            Present
                                [ Input.buildUserRef
                                    (\u -> { u | username = Present f.assignee.username })
                                ]
                        , history =
                            if f.isNew then
                                events

                            else
                                Absent
                    }
                )
                |> Present

        historyPatch =
            Input.buildTensionPatch (\s -> { s | history = events }) |> Present

        inputOpt =
            \x ->
                { set = ternary f.isNew userPatch historyPatch
                , remove = ternary (f.isNew == False) userPatch Absent
                }
    in
    { input =
        Input.buildUpdateTensionInput inputReq inputOpt
    }



{-
   Publish Blob
-}


type alias TensionBlobFlagPayload =
    { tension : Maybe (List (Maybe TensionBlobFlag)) }


type alias TensionBlobFlag =
    { blobs : Maybe (List BlobFlag) }


publishBlobDecoder : Maybe TensionBlobFlagPayload -> Maybe BlobFlag
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
                |> Maybe.map (\t -> t.blobs)
                |> withDefault Nothing
                |> withDefault []
                |> List.head
                |> Maybe.map
                    (\b ->
                        { pushedFlag = b.pushedFlag }
                    )

        Nothing ->
            Nothing


publishBlob url bid form msg =
    makeGQLMutation url
        (Mutation.updateTension
            (publishBlobInputEncoder bid form)
            (SelectionSet.map TensionBlobFlagPayload <|
                Fractal.Object.UpdateTensionPayload.tension identity <|
                    SelectionSet.map TensionBlobFlag
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


publishBlobInputEncoder : String -> TensionPatchForm -> Mutation.UpdateTensionRequiredArguments
publishBlobInputEncoder bid f =
    --@Debug: receiver and receiverid
    let
        createdAt =
            Dict.get "createdAt" f.post |> withDefault ""

        inputReq =
            { filter =
                Input.buildTensionFilter
                    (\ft ->
                        { ft | id = Present [ encodeId f.id ] }
                    )
            }

        inputOpt =
            \x ->
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
                                , history = buildEvent createdAt f.uctx.username f.events_type f.post
                            }
                        )
                        |> Present
                , remove = Absent
                }
    in
    { input =
        Input.buildUpdateTensionInput inputReq inputOpt
    }



{-
   Archive Doc
-}


type alias TensionArchivePayload =
    { tension : Maybe (List (Maybe ActionResult)) }


archiveDocDecoder : Maybe TensionArchivePayload -> Maybe ActionResult
archiveDocDecoder data =
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


archiveDoc url form msg =
    makeGQLMutation url
        (Mutation.updateTension
            (archiveDocInputEncoder form)
            (SelectionSet.map TensionArchivePayload <|
                Fractal.Object.UpdateTensionPayload.tension identity <|
                    SelectionSet.map ActionResult Fractal.Object.Tension.action
            )
        )
        (RemoteData.fromResult >> decodeResponse archiveDocDecoder >> msg)


archiveDocInputEncoder : ActionForm -> Mutation.UpdateTensionRequiredArguments
archiveDocInputEncoder f =
    let
        createdAt =
            Dict.get "createdAt" f.post |> withDefault ""

        inputReq =
            { filter =
                Input.buildTensionFilter
                    (\ft ->
                        { ft | id = Present [ encodeId f.tid ] }
                    )
            }

        inputOpt =
            \x ->
                { set =
                    Input.buildTensionPatch
                        (\s ->
                            { s
                                | history = buildEvent createdAt f.uctx.username f.events_type f.post
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
    { input =
        Input.buildUpdateTensionInput inputReq inputOpt
    }



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

        inputReq =
            { filter =
                Input.buildCommentFilter
                    (\f ->
                        { f | id = Present [ encodeId form.id ] }
                    )
            }

        inputOpt =
            \x ->
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
    { input =
        Input.buildUpdateCommentInput inputReq inputOpt
    }
