module Query.AddTension exposing
    ( addOneTension
    , buildBlob
    , buildComment
    , buildEvent
    , buildMandate
    , tensionFromForm
    )

import Dict exposing (Dict)
import Extra exposing (ternary)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.NodeMode as NodeMode
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
import ModelCommon exposing (TensionForm, UserForm)
import ModelSchema exposing (..)
import Query.QueryTension exposing (tensionPayload)
import RemoteData exposing (RemoteData)



{-
   Add a single tension
-}


type alias TensionsPayload =
    { tension : Maybe (List (Maybe Tension)) }



-- Response Decoder


tensionDecoder : Maybe TensionsPayload -> Maybe Tension
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
            (SelectionSet.map TensionsPayload <|
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
            Dict.get "message" f.post |> withDefault ""

        inputReq =
            { createdAt = createdAt |> Fractal.Scalar.DateTime
            , createdBy =
                Input.buildUserRef
                    (\x -> { x | username = Present f.uctx.username })
            , title = title
            , type_ = f.tension_type
            , status = f.status
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

        inputOpt =
            \x ->
                { x
                    | action = f.action |> fromMaybe
                    , comments = buildComment createdAt f.uctx.username (Just message)
                    , blobs = buildBlob createdAt f.uctx.username f.blob_type f.users f.node f.post
                    , labels = buildLabels f
                }
    in
    { input =
        [ Input.buildAddTensionInput inputReq inputOpt ]
    }



--- Utils


tensionFromForm : TensionForm -> (Input.TensionRefOptionalFields -> Input.TensionRefOptionalFields)
tensionFromForm f =
    let
        title =
            Dict.get "title" f.post |> withDefault ""

        createdAt =
            Dict.get "createdAt" f.post |> withDefault ""

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
            , status = f.status |> Present
            , action = f.action |> fromMaybe
            , emitter =
                Input.buildNodeRef (\x -> { x | nameid = Present f.source.nameid }) |> Present
            , receiver =
                Input.buildNodeRef (\x -> { x | nameid = Present f.target.nameid }) |> Present
            , emitterid = f.source.nameid |> Present
            , receiverid = f.target.nameid |> Present
            , comments = buildComment createdAt f.uctx.username (Just message)
            , blobs = buildBlob createdAt f.uctx.username f.blob_type f.users f.node f.post
            , history = buildEvent createdAt f.uctx.username f.events_type f.post
        }


buildLabels : TensionForm -> OptionalArgument (List Input.LabelRef)
buildLabels form =
    form.labels
        |> List.map
            (\label ->
                Input.buildLabelRef
                    (\x ->
                        { x | id = Present (encodeId label.id) }
                    )
            )
        |> Present


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


buildBlob : String -> String -> Maybe BlobType.BlobType -> List UserForm -> NodeFragment -> Post -> OptionalArgument (List Input.BlobRef)
buildBlob createdAt username blob_type_m users node post =
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
                            , node = buildNodeFragmentRef users node
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
                                        , old = Dict.get "old" post |> fromMaybe
                                        , new = Dict.get "new" post |> fromMaybe
                                    }
                                )
                        )
            )
        |> fromMaybe


buildNodeFragmentRef : List UserForm -> NodeFragment -> OptionalArgument Input.NodeFragmentRef
buildNodeFragmentRef users nf =
    let
        type_ =
            nf.type_ |> withDefault NodeType.Role

        g1 =
            Debug.log "heu" type_
    in
    Input.buildNodeFragmentRef
        (\n ->
            let
                commonFields =
                    { n
                        | name = fromMaybe nf.name
                        , nameid = fromMaybe (Maybe.map (\nid -> String.toLower nid) nf.nameid)
                        , type_ = fromMaybe nf.type_
                        , about = fromMaybe nf.about
                        , mandate = buildMandate nf.mandate
                        , charac = nf.charac |> Maybe.map (\c -> { userCanJoin = Present c.userCanJoin, mode = Present c.mode, id = Absent }) |> fromMaybe
                    }
            in
            case type_ of
                NodeType.Role ->
                    -- Role
                    { commonFields
                        | first_link = users |> List.filter (\u -> u.username /= "") |> List.head |> Maybe.map (\us -> us.username) |> fromMaybe
                        , role_type = users |> List.head |> Maybe.map (\us -> us.role_type) |> fromMaybe
                    }

                NodeType.Circle ->
                    -- Circle
                    { commonFields
                        | children =
                            users
                                |> List.indexedMap
                                    (\i us ->
                                        Input.buildNodeFragmentRef
                                            (\c ->
                                                { c
                                                    | first_link = ternary (us.username /= "") (Present us.username) Absent
                                                    , role_type = Present us.role_type
                                                }
                                            )
                                    )
                                |> Present
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
