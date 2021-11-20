module Query.PatchTension exposing
    ( actionRequest
    , moveTension
    , patchComment
    , patchLiteral
    , publishBlob
    , pushTensionPatch
    , setAssignee
    , setLabel
    )

import Dict exposing (Dict)
import Extra exposing (ternary)
import Fractal.Enum.BlobOrderable as BlobOrderable
import Fractal.Enum.CommentOrderable as CommentOrderable
import Fractal.Enum.ContractOrderable as ContractOrderable
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.Blob
import Fractal.Object.Comment
import Fractal.Object.Contract
import Fractal.Object.Label
import Fractal.Object.Node
import Fractal.Object.Tension
import Fractal.Object.UpdateCommentPayload
import Fractal.Object.UpdateTensionPayload
import Fractal.Object.Vote
import Fractal.Query as Query
import Fractal.Scalar
import Fractal.ScalarCodecs
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Maybe exposing (withDefault)
import ModelCommon exposing (ActionForm, AssigneeForm, CommentPatchForm, LabelForm, TensionPatchForm)
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
                                , type_ = fromMaybe f.type_
                                , comments = buildComment createdAt f.uctx.username message
                                , history = buildEvents createdAt f.uctx.username f.events_type f.post
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
    { input = Input.buildUpdateCommentInput inputReq inputOpt }



{-
   set assignee
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
            Dict.get "createdAt" f.post |> withDefault ""

        events =
            buildEvents createdAt f.uctx.username f.events_type f.post

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
            \x ->
                { set = ternary f.isNew patch historyPatch
                , remove = ternary (f.isNew == False) patch Absent
                }
    in
    { input = Input.buildUpdateTensionInput inputReq inputOpt }



{-
   set label
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
            Dict.get "createdAt" f.post |> withDefault ""

        events =
            buildEvents createdAt f.uctx.username f.events_type f.post

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
            \x ->
                { set = ternary f.isNew patch historyPatch
                , remove = ternary (f.isNew == False) patch Absent
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
            Dict.get "createdAt" f.post |> withDefault ""

        message =
            -- new comment
            Dict.get "message" f.post

        events =
            buildEvents createdAt f.uctx.username f.events_type f.post

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
                        | history = events
                        , comments = buildComment createdAt f.uctx.username message
                    }
                )
                |> Present

        inputOpt =
            \x ->
                { set = patch, remove = Absent }
    in
    { input = Input.buildUpdateTensionInput inputReq inputOpt }



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
                                , history = buildEvents createdAt f.uctx.username f.events_type f.post
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


type alias TensionActionPayload =
    { tension : Maybe (List (Maybe ActionResult)) }


actionDecoder : Maybe TensionActionPayload -> Maybe ActionResult
actionDecoder data =
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


actionRequest url form msg =
    makeGQLMutation url
        (Mutation.updateTension
            (actionInputEncoder form)
            (SelectionSet.map TensionActionPayload <|
                Fractal.Object.UpdateTensionPayload.tension identity <|
                    SelectionSet.map ActionResult Fractal.Object.Tension.action
            )
        )
        (RemoteData.fromResult >> decodeResponse actionDecoder >> msg)


actionInputEncoder : ActionForm -> Mutation.UpdateTensionRequiredArguments
actionInputEncoder f =
    let
        createdAt =
            Dict.get "createdAt" f.post |> withDefault ""

        message =
            -- new comment
            Dict.get "message" f.post

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
                                | comments = buildComment createdAt f.uctx.username message
                                , history = buildEvents createdAt f.uctx.username f.events_type f.post
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
