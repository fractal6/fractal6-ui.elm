module Query.AddTension exposing
    ( addCircleTension
    , addOneTension
    , buildMandate
    , tensionFromForm
    )

import Dict exposing (Dict)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.AddTensionPayload
import Fractal.Object.Label
import Fractal.Object.Tension
import Fractal.Query as Query
import Fractal.Scalar
import Fractal.ScalarCodecs
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Maybe exposing (withDefault)
import ModelCommon exposing (TensionForm)
import ModelSchema exposing (..)
import Query.QueryTension exposing (tensionPayload)
import RemoteData exposing (RemoteData)



{-
   Add a single tension
-}


type alias AddTensionPayload =
    { tension : Maybe (List (Maybe Tension)) }


type alias AddLabelPayload =
    { label : Maybe (List (Maybe IdPayload)) }



-- Response Decoder


tensionDecoder : Maybe AddTensionPayload -> Maybe Tension
tensionDecoder a =
    case a of
        Just b ->
            b.tension
                |> Maybe.map (\x -> List.head x)
                |> Maybe.withDefault Nothing
                |> Maybe.withDefault Nothing

        Nothing ->
            Nothing


addOneTension url form msg =
    --@DEBUG: Infered type...
    makeGQLMutation url
        (Mutation.addTension
            (addTensionInputEncoder form)
            (SelectionSet.map AddTensionPayload <|
                Fractal.Object.AddTensionPayload.tension identity tensionPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse tensionDecoder >> msg)



-- input Encoder


addTensionInputEncoder : TensionForm -> Mutation.AddTensionRequiredArguments
addTensionInputEncoder f =
    let
        title =
            Dict.get "title" f.post |> withDefault ""

        createdAt =
            Dict.get "createdAt" f.post |> withDefault ""

        message =
            Dict.get "message" f.post

        tensionRequired =
            { createdAt = createdAt |> Fractal.Scalar.DateTime
            , createdBy =
                Input.buildUserRef
                    (\x -> { x | username = Present f.uctx.username })
            , title = title
            , type_ = f.tension_type
            , status = TensionStatus.Open
            , emitter =
                Input.buildNodeRef (\n -> { n | nameid = Present f.source.nameid })
            , receiver =
                Input.buildNodeRef (\n -> { n | nameid = Present f.target.nameid })
            , emitterid = f.source.nameid
            , receiverid = f.target.nameid
            }

        tensionOpts =
            \t -> { t | comments = buildComment createdAt f.uctx.username message }
    in
    { input =
        [ Input.buildAddTensionInput tensionRequired tensionOpts ]
    }



{-
   Add a tension with with New Circle/Role Action
-}


addCircleTension url form msg =
    --@DEBUG: Infered type...
    makeGQLMutation url
        (Mutation.addTension
            (addCircleInputEncoder form)
            (SelectionSet.map AddTensionPayload
                (Fractal.Object.AddTensionPayload.tension identity tensionPayload)
            )
        )
        (RemoteData.fromResult >> decodeResponse tensionDecoder >> msg)


addCircleInputEncoder : TensionForm -> Mutation.AddTensionRequiredArguments
addCircleInputEncoder f =
    let
        title =
            Dict.get "title" f.post |> withDefault ""

        createdAt =
            Dict.get "createdAt" f.post |> withDefault ""

        status =
            Dict.get "status" f.post |> withDefault "" |> TensionStatus.fromString |> withDefault TensionStatus.Open

        message =
            Dict.get "message" f.post

        tensionRequired =
            { createdAt = createdAt |> Fractal.Scalar.DateTime
            , createdBy =
                Input.buildUserRef
                    (\x -> { x | username = Present f.uctx.username })
            , title = title
            , type_ = f.tension_type
            , status = status
            , emitterid = f.source.nameid
            , receiverid = f.target.nameid
            , emitter =
                Input.buildNodeRef (\x -> { x | nameid = Present f.source.nameid })
            , receiver =
                Input.buildNodeRef (\x -> { x | nameid = Present f.target.nameid })
            }

        tensionOpts =
            \t ->
                { t
                    | action = f.action |> fromMaybe
                    , comments = buildComment createdAt f.uctx.username message
                    , blobs = buildBlob createdAt f.uctx.username f.blob_type f.node f.post
                }
    in
    { input =
        [ Input.buildAddTensionInput tensionRequired tensionOpts ]
    }



--- Utils


tensionFromForm : TensionForm -> (Input.TensionRefOptionalFields -> Input.TensionRefOptionalFields)
tensionFromForm f =
    let
        title =
            Dict.get "title" f.post |> withDefault ""

        createdAt =
            Dict.get "createdAt" f.post |> withDefault ""

        status =
            Dict.get "status" f.post |> withDefault "" |> TensionStatus.fromString |> withDefault TensionStatus.Open

        message =
            Dict.get "message" f.post
    in
    \t ->
        { t
            | createdAt = createdAt |> Fractal.Scalar.DateTime |> Present
            , createdBy =
                Input.buildUserRef
                    (\x -> { x | username = Present f.uctx.username })
                    |> Present
            , title = title |> Present
            , type_ = f.tension_type |> Present
            , status = status |> Present
            , action = f.action |> fromMaybe
            , emitterid = f.source.nameid |> Present
            , receiverid = f.target.nameid |> Present
            , emitter =
                Input.buildNodeRef (\x -> { x | nameid = Present f.source.nameid }) |> Present
            , receiver =
                Input.buildNodeRef (\x -> { x | nameid = Present f.target.nameid }) |> Present
            , comments = buildComment createdAt f.uctx.username message
            , blobs = buildBlob createdAt f.uctx.username f.blob_type f.node f.post
        }


buildComment : String -> String -> Maybe String -> OptionalArgument (List Input.CommentRef)
buildComment createdAt username message_m =
    let
        message =
            message_m |> withDefault ""
    in
    Present
        [ Input.buildCommentRef
            (\x ->
                { x
                    | createdAt = createdAt |> Fractal.Scalar.DateTime |> Present
                    , createdBy =
                        Input.buildUserRef
                            (\u -> { u | username = Present username })
                            |> Present
                    , message = Present message
                }
            )
        ]


buildBlob : String -> String -> Maybe BlobType.BlobType -> NodeFragment -> Post -> OptionalArgument (List Input.BlobRef)
buildBlob createdAt username blob_type_m node post =
    blob_type_m
        |> Maybe.map
            (\blob_type ->
                [ Input.buildBlobRef
                    (\x ->
                        { x
                            | createdAt = createdAt |> Fractal.Scalar.DateTime |> Present
                            , createdBy =
                                Input.buildUserRef
                                    (\u -> { u | username = Present username })
                                    |> Present
                            , node = buildNodeFragmentRef node
                            , md = Dict.get "md" post |> fromMaybe
                        }
                    )
                ]
            )
        |> fromMaybe


buildNodeFragmentRef : NodeFragment -> OptionalArgument Input.NodeFragmentRef
buildNodeFragmentRef nf =
    Input.buildNodeFragmentRef
        (\n ->
            { n
                | name = fromMaybe nf.name
                , nameid = fromMaybe nf.nameid
                , type_ = fromMaybe nf.type_
                , role_type = fromMaybe nf.role_type
                , about = fromMaybe nf.about
                , mandate = buildMandate nf.mandate
                , charac = nf.charac |> Maybe.map (\c -> { userCanJoin = Present c.userCanJoin, mode = Present c.mode, id = Absent }) |> fromMaybe
                , first_link = fromMaybe nf.first_link
            }
        )
        |> Present


buildMandate : Maybe Mandate -> OptionalArgument Input.MandateRef
buildMandate maybeMandate =
    maybeMandate
        |> Maybe.map
            (\mandate ->
                Input.buildMandateRef
                    (\m ->
                        { m
                            | purpose = mandate.purpose |> Present
                            , responsabilities = mandate.responsabilities |> fromMaybe
                            , domains = mandate.domains |> fromMaybe
                            , policies = mandate.policies |> fromMaybe
                        }
                    )
            )
        |> fromMaybe
