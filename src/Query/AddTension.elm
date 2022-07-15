module Query.AddTension exposing
    ( addOneTension
    , buildBlob
    , buildComment
    , buildEvents
    , buildMandate
    )

import Dict exposing (Dict)
import Extra exposing (listToMaybe, ternary)
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
import ModelCommon exposing (Ev, TensionForm, UserForm, encodeLabel)
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
    a
        |> Maybe.andThen
            (\b ->
                b.tension
                    |> Maybe.map (\x -> List.head x)
                    |> Maybe.withDefault Nothing
                    |> Maybe.withDefault Nothing
            )


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
            Dict.get "createdAt" f.post |> withDefault "" |> Fractal.Scalar.DateTime

        message =
            Dict.get "message" f.post |> withDefault ""

        events =
            f.events
                ++ List.map
                    (\l ->
                        Ev TensionEvent.LabelAdded "" (encodeLabel l)
                    )
                    f.labels

        inputReq =
            { createdAt = createdAt
            , createdBy =
                Input.buildUserRef
                    (\x -> { x | username = Present f.uctx.username })
            , title = title
            , type_ = withDefault TensionType.Operational f.type_
            , status = withDefault TensionStatus.Open f.status
            , emitter =
                Input.buildNodeRef (\n -> { n | nameid = Present f.source.nameid })
            , receiver =
                Input.buildNodeRef (\n -> { n | nameid = Present f.target.nameid })
            , emitterid = f.source.nameid
            , receiverid = f.target.nameid
            }

        inputOpt =
            \x ->
                { x
                    | action = fromMaybe f.action
                    , comments = buildComment createdAt f.uctx.username (Just message)
                    , blobs = buildBlob createdAt f.uctx.username f.blob_type f.users f.node f.post
                    , labels = buildLabels f
                    , history = buildEvents createdAt f.uctx.username events
                }
    in
    { input = [ Input.buildAddTensionInput inputReq inputOpt ] }



--- Utils


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
        |> listToMaybe
        |> fromMaybe


buildComment : Fractal.Scalar.DateTime -> String -> Maybe String -> OptionalArgument (List Input.CommentRef)
buildComment createdAt username message_m =
    message_m
        |> Maybe.map
            (\message ->
                [ Input.buildCommentRef
                    (\x ->
                        { x
                            | createdAt = Present createdAt
                            , createdBy = Input.buildUserRef (\u -> { u | username = Present username }) |> Present
                            , message = Present message
                        }
                    )
                ]
            )
        |> fromMaybe


buildBlob : Fractal.Scalar.DateTime -> String -> Maybe BlobType.BlobType -> List UserForm -> NodeFragment -> Post -> OptionalArgument (List Input.BlobRef)
buildBlob createdAt username blob_type_m users node post =
    blob_type_m
        |> Maybe.map
            (\blob_type ->
                [ Input.buildBlobRef
                    (\x ->
                        { x
                            | createdAt = Present createdAt
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


buildEvents : Fractal.Scalar.DateTime -> String -> List Ev -> OptionalArgument (List Input.EventRef)
buildEvents createdAt username events =
    events
        |> List.map
            (\{ event_type, old, new } ->
                Input.buildEventRef
                    (\x ->
                        { x
                            | createdAt = createdAt |> Present
                            , createdBy =
                                Input.buildUserRef
                                    (\u -> { u | username = Present username })
                                    |> Present
                            , event_type = Present event_type
                            , old = Present old
                            , new = Present new
                        }
                    )
            )
        |> Present


buildNodeFragmentRef : List UserForm -> NodeFragment -> OptionalArgument Input.NodeFragmentRef
buildNodeFragmentRef users nf =
    let
        type_ =
            withDefault NodeType.Role nf.type_
    in
    Input.buildNodeFragmentRef
        (\n ->
            let
                commonFields =
                    { n
                        | name = fromMaybe (Maybe.map String.trim nf.name)
                        , nameid = fromMaybe (Maybe.map (String.toLower >> String.trim) nf.nameid)
                        , type_ = fromMaybe nf.type_
                        , about = fromMaybe nf.about
                        , mandate = Maybe.map buildMandate nf.mandate |> fromMaybe
                        , visibility = fromMaybe nf.visibility
                        , mode = fromMaybe nf.mode
                        , color = fromMaybe nf.color
                    }
            in
            case type_ of
                NodeType.Role ->
                    -- Role
                    { commonFields
                        | first_link = users |> List.filter (\u -> u.username /= "") |> List.head |> Maybe.map (\us -> us.username) |> fromMaybe
                        , role_type = fromMaybe nf.role_type
                        , role_ext = fromMaybe nf.role_ext
                    }

                NodeType.Circle ->
                    -- Circle
                    { commonFields | children = Absent }
         -- Children not implemented for now
         --        users
         --            |> List.map
         --                (\us ->
         --                    Input.buildNodeFragmentRef
         --                        (\c ->
         --                            { c
         --                                | first_link = ternary (us.username /= "") (Present us.username) Absent
         --                                , role_type = Present us.role_type
         --                            }
         --                        )
         --                )
         --            |> Present
         --}
        )
        |> Present


buildMandate : Mandate -> Input.MandateRef
buildMandate mandate =
    Input.buildMandateRef
        (\m ->
            { m
                | purpose = mandate.purpose |> Present
                , responsabilities = mandate.responsabilities |> fromMaybe
                , domains = mandate.domains |> fromMaybe
                , policies = mandate.policies |> fromMaybe
            }
        )
