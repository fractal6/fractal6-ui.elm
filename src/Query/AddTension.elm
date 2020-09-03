module Query.AddTension exposing
    ( addOneTension
    , buildBlob
    , buildComment
    , buildEvent
    , buildMandate
    , tensionFromForm
    )

import Dict exposing (Dict)
import Form.NewCircle exposing (getFirstLinks)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
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

        status =
            Dict.get "status" f.post |> withDefault "" |> TensionStatus.fromString |> withDefault TensionStatus.Open

        message =
            Dict.get "message" f.post |> withDefault ""

        tensionRequired =
            { createdAt = createdAt |> Fractal.Scalar.DateTime
            , createdBy =
                Input.buildUserRef
                    (\x -> { x | username = Present f.uctx.username })
            , title = title
            , type_ = f.tension_type
            , status = status
            , emitter =
                Input.buildNodeRef (\n -> { n | nameid = Present f.source.nameid })
            , receiver =
                Input.buildNodeRef (\n -> { n | nameid = Present f.target.nameid })
            , emitterid = f.source.nameid
            , receiverid = f.target.nameid
            , history =
                f.events_type
                    |> withDefault [ TensionEvent.Created ]
                    |> List.map
                        (\event_type ->
                            Input.buildEventRef
                                (\x ->
                                    { x
                                        | createdAt = createdAt |> Fractal.Scalar.DateTime |> Present
                                        , createdBy =
                                            Input.buildUserRef
                                                (\u -> { u | username = Present f.uctx.username })
                                                |> Present
                                        , event_type = Present event_type
                                        , old = Present ""
                                        , new = Present ""
                                    }
                                )
                        )
            }

        tensionOpts =
            \t ->
                { t
                    | action = f.action |> fromMaybe
                    , comments = buildComment createdAt f.uctx.username (Just message)
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
            Dict.get "message" f.post |> withDefault ""
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
            , emitter =
                Input.buildNodeRef (\x -> { x | nameid = Present f.source.nameid }) |> Present
            , receiver =
                Input.buildNodeRef (\x -> { x | nameid = Present f.target.nameid }) |> Present
            , emitterid = f.source.nameid |> Present
            , receiverid = f.target.nameid |> Present
            , comments = buildComment createdAt f.uctx.username (Just message)
            , blobs = buildBlob createdAt f.uctx.username f.blob_type f.node f.post
            , history = buildEvent createdAt f.uctx.username f.events_type f.post
        }


buildComment : String -> String -> Maybe String -> OptionalArgument (List Input.CommentRef)
buildComment createdAt username message_m =
    message_m
        |> Maybe.map
            (\message ->
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
            )
        |> fromMaybe


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
                            , blob_type = Present blob_type
                            , node = buildNodeFragmentRef node
                            , md = Dict.get "md" post |> fromMaybe
                        }
                    )
                ]
            )
        |> fromMaybe


buildEvent : String -> String -> Maybe (List TensionEvent.TensionEvent) -> Post -> OptionalArgument (List Input.EventRef)
buildEvent createdAt username events_type_m post =
    events_type_m
        |> Maybe.map
            (\events_type ->
                events_type
                    |> List.map
                        (\event_type ->
                            Input.buildEventRef
                                (\x ->
                                    { x
                                        | createdAt = createdAt |> Fractal.Scalar.DateTime |> Present
                                        , createdBy =
                                            Input.buildUserRef
                                                (\u -> { u | username = Present username })
                                                |> Present
                                        , event_type = Present event_type
                                        , old = Present ""
                                        , new = Present ""
                                    }
                                )
                        )
            )
        |> fromMaybe


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


buildNodeFragmentRef : NodeFragment -> OptionalArgument Input.NodeFragmentRef
buildNodeFragmentRef nf =
    let
        type_ =
            nf.type_ |> withDefault NodeType.Role

        first_links =
            getFirstLinks nf
    in
    Input.buildNodeFragmentRef
        (\n ->
            let
                commonFields =
                    { n
                        | name = fromMaybe nf.name
                        , nameid = fromMaybe nf.nameid
                        , type_ = fromMaybe nf.type_
                        , role_type = fromMaybe nf.role_type
                        , about = fromMaybe nf.about
                        , mandate = buildMandate nf.mandate
                        , charac = nf.charac |> Maybe.map (\c -> { userCanJoin = Present c.userCanJoin, mode = Present c.mode, id = Absent }) |> fromMaybe
                    }
            in
            case type_ of
                NodeType.Role ->
                    -- Role
                    { commonFields | first_link = fromMaybe nf.first_link }

                NodeType.Circle ->
                    -- Circle
                    { commonFields
                        | children =
                            first_links
                                |> List.indexedMap
                                    (\i uname ->
                                        Input.buildNodeFragmentRef
                                            (\c -> { c | first_link = fromMaybe (Just uname) })
                                    )
                                |> Present
                    }
        )
        |> Present
