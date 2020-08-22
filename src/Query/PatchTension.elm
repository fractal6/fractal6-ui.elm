module Query.PatchTension exposing
    ( patchComment
    , patchTitle
    , pushTensionComment
    )

import Dict exposing (Dict)
import Fractal.Enum.CommentOrderable as CommentOrderable
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
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
import Query.QueryTension exposing (commentPayload)
import RemoteData exposing (RemoteData)



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
            (pushTensionCommentInputEncoder form)
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



{-
   Patch a single tension
-}


type alias PatchTensionIdPayload =
    { tension : Maybe (List (Maybe CommentId)) }


type alias CommentId =
    { comments : Maybe (List IdPayload) }


tensionPushDecoder : Maybe PatchTensionIdPayload -> Maybe IdPayload
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
                |> Maybe.map
                    (\t ->
                        t.comments |> Maybe.map (\cs -> List.head cs)
                    )
                |> withDefault Nothing
                |> withDefault Nothing

        Nothing ->
            Nothing


pushTensionComment url form msg =
    makeGQLMutation url
        (Mutation.updateTension
            (pushTensionCommentInputEncoder form)
            (SelectionSet.map PatchTensionIdPayload <|
                Fractal.Object.UpdateTensionPayload.tension identity <|
                    (SelectionSet.succeed CommentId
                        |> with
                            (Fractal.Object.Tension.comments (pushCommentFilter form) <|
                                SelectionSet.map IdPayload (Fractal.Object.Comment.id |> SelectionSet.map decodedId)
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


pushTensionCommentInputEncoder : TensionPatchForm -> Mutation.UpdateTensionRequiredArguments
pushTensionCommentInputEncoder f =
    let
        -- new comment
        createdAt =
            -- Maybe.map (\x -> Fractal.Scalar.DateTime x) ???
            Dict.get "createdAt" f.post |> withDefault ""

        updatedAt =
            Dict.get "updatedAt" f.post |> Maybe.map (\x -> Fractal.Scalar.DateTime x)

        title =
            Dict.get "title" f.post

        message =
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
