module Query.PatchTension exposing
    ( patchComment
    , patchTitle
    , publishBlob
    , pushTensionPatch
    )

import Dict exposing (Dict)
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
import ModelCommon exposing (CommentPatchForm, TensionPatchForm)
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

        patchRequired =
            { filter =
                Input.buildTensionFilter
                    (\ft ->
                        { ft | id = Present [ encodeId f.id ] }
                    )
            }

        patchOpts =
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
                                , blobs = buildBlob createdAt f.uctx.username f.blob_type f.node f.post
                                , history = buildEvent createdAt f.uctx.username f.events_type f.post
                            }
                        )
                        |> Present
                , remove = Absent
                }
    in
    { input =
        Input.buildUpdateTensionInput patchRequired patchOpts
    }



{-
   Publish Blob
-}


type alias TensionPayloadID =
    { tension : Maybe (List (Maybe IdPayload)) }


publishBlobDecoder : Maybe TensionPayloadID -> Maybe IdPayload
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
                |> Maybe.map
                    (\t ->
                        { id = t.id }
                    )

        Nothing ->
            Nothing


publishBlob url form msg =
    makeGQLMutation url
        (Mutation.updateTension
            (publishBlobInputEncoder form)
            (SelectionSet.map TensionPayloadID <|
                Fractal.Object.UpdateTensionPayload.tension identity <|
                    SelectionSet.map IdPayload (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
            )
        )
        (RemoteData.fromResult >> decodeResponse publishBlobDecoder >> msg)


publishBlobInputEncoder : TensionPatchForm -> Mutation.UpdateTensionRequiredArguments
publishBlobInputEncoder f =
    --@Debug: receiver and receiverid
    let
        createdAt =
            Dict.get "createdAt" f.post |> withDefault ""

        blobid =
            Dict.get "blobid" f.post |> withDefault ""

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
                                            { b | id = Present (encodeId blobid), pushedFlag = createdAt |> Fractal.Scalar.DateTime |> Present }
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
        Input.buildUpdateCommentInput patchRequired patchOpts
    }
