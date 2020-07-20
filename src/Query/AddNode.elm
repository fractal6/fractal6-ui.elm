module Query.AddNode exposing
    ( addNewMember
    , addOneCircle
    , buildComment
    , buildMandate
    , buildNodeFragmentRef
    , tensionFromForm
    )

import Dict exposing (Dict)
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.AddNodePayload
import Fractal.Object.Node
import Fractal.Object.NodeCharac
import Fractal.Object.User
import Fractal.Scalar
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Maybe exposing (withDefault)
import ModelCommon exposing (JoinOrgaForm, TensionForm)
import ModelCommon.Uri exposing (guestIdCodec, nodeIdCodec)
import ModelSchema exposing (..)
import Query.QueryNode exposing (nodeOrgaPayload)
import RemoteData exposing (RemoteData)



{-
   Add a New member
-}


type alias AddNodePayload =
    { node : Maybe (List (Maybe Node)) }



--- Response Decoder


nodeDecoder : Maybe AddNodePayload -> Maybe Node
nodeDecoder a =
    case a of
        Just b ->
            b.node
                |> Maybe.map
                    (\x ->
                        List.head x
                    )
                |> Maybe.withDefault Nothing
                |> Maybe.withDefault Nothing

        Nothing ->
            Nothing



--- Query


addNewMember url form msg =
    --@DEBUG: Infered type...
    makeGQLMutation url
        (Mutation.addNode
            (newMemberInputEncoder form)
            (SelectionSet.map AddNodePayload
                (Fractal.Object.AddNodePayload.node identity nodeOrgaPayload)
            )
        )
        (RemoteData.fromResult >> decodeResponse nodeDecoder >> msg)



-- Input Encoder


newMemberInputEncoder : JoinOrgaForm -> Mutation.AddNodeRequiredArguments
newMemberInputEncoder { uctx, rootnameid, id, post } =
    let
        createdAt =
            Dict.get "createdAt" post |> withDefault ""

        nodeRequired =
            { createdAt = createdAt |> Fractal.Scalar.DateTime
            , createdBy =
                Input.buildUserRef
                    (\u -> { u | username = Present uctx.username })
            , type_ = NodeType.Role
            , nameid = guestIdCodec rootnameid uctx.username
            , name = "Guest"
            , rootnameid = rootnameid
            , isRoot = False
            , charac = { userCanJoin = Present False, mode = Present NodeMode.Coordinated, id = Absent }
            , isPrivate = False
            }

        nodeOptional =
            \n ->
                { n
                    | role_type = Present RoleType.Guest
                    , parent =
                        Input.buildNodeRef
                            (\p -> { p | nameid = Present rootnameid })
                            |> Present
                    , first_link =
                        Input.buildUserRef
                            (\u -> { u | username = Present uctx.username })
                            |> Present
                }
    in
    { input =
        [ Input.buildAddNodeInput nodeRequired nodeOptional ]
    }



{-
   Add a new Circle/Role
-}


type alias Circle =
    { id : String
    , createdAt : String
    , name : String
    , nameid : String
    , rootnameid : String
    , parent : Maybe NodeId -- see issue with recursive structure
    , children : Maybe (List Node)
    , type_ : NodeType.NodeType
    , role_type : Maybe RoleType.RoleType
    , first_link : Maybe Username
    , charac : NodeCharac
    , isPrivate : Bool
    }


type alias AddCirclePayload =
    { node : Maybe (List (Maybe Circle)) }



--- Response Decoder


circleDecoder : Maybe AddCirclePayload -> Maybe (List Node)
circleDecoder a =
    case a of
        Just b ->
            b.node
                |> Maybe.map
                    (\x ->
                        case List.head x of
                            Just (Just n) ->
                                let
                                    children =
                                        n.children |> withDefault []

                                    node =
                                        { id = .id n
                                        , createdAt = .createdAt n
                                        , name = .name n
                                        , nameid = .nameid n
                                        , rootnameid = .rootnameid n
                                        , parent = .parent n
                                        , type_ = .type_ n
                                        , role_type = .role_type n
                                        , first_link = .first_link n
                                        , charac = .charac n
                                        , isPrivate = .isPrivate n
                                        }
                                in
                                [ node ]
                                    ++ children
                                    |> Just

                            _ ->
                                Nothing
                    )
                |> Maybe.withDefault Nothing

        Nothing ->
            Nothing



--- Query


addOneCircle url form msg =
    --@DEBUG: Infered type...
    makeGQLMutation url
        (Mutation.addNode
            (addCircleInputEncoder form)
            (SelectionSet.map AddCirclePayload <|
                Fractal.Object.AddNodePayload.node identity addOneCirclePayload
            )
        )
        (RemoteData.fromResult >> decodeResponse circleDecoder >> msg)


addOneCirclePayload : SelectionSet Circle Fractal.Object.Node
addOneCirclePayload =
    SelectionSet.succeed Circle
        |> with (Fractal.Object.Node.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Node.createdAt |> SelectionSet.map decodedTime)
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with Fractal.Object.Node.rootnameid
        |> with
            (Fractal.Object.Node.parent identity (SelectionSet.map NodeId Fractal.Object.Node.nameid))
        |> with (Fractal.Object.Node.children identity nodeOrgaPayload)
        |> with Fractal.Object.Node.type_
        |> with Fractal.Object.Node.role_type
        |> with (Fractal.Object.Node.first_link identity (SelectionSet.map Username Fractal.Object.User.username))
        |> with
            (Fractal.Object.Node.charac identity <|
                SelectionSet.map2 NodeCharac
                    Fractal.Object.NodeCharac.userCanJoin
                    Fractal.Object.NodeCharac.mode
            )
        |> with Fractal.Object.Node.isPrivate



-- Input Encoder


addCircleInputEncoder : TensionForm -> Mutation.AddNodeRequiredArguments
addCircleInputEncoder f =
    let
        createdAt =
            Dict.get "createdAt" f.post |> withDefault ""

        type_ =
            f.data.type_ |> withDefault NodeType.Role

        nameid =
            f.data.nameid |> Maybe.map (\nid -> nodeIdCodec f.target.nameid nid type_) |> withDefault ""

        name =
            f.data.name |> withDefault ""

        charac =
            f.data.charac |> withDefault (NodeCharac False NodeMode.Coordinated)

        nodeRequired =
            { createdAt = createdAt |> Fractal.Scalar.DateTime
            , createdBy =
                Input.buildUserRef
                    (\u -> { u | username = Present f.uctx.username })
            , isRoot = False
            , type_ = type_
            , name = name
            , nameid = nameid
            , rootnameid = f.target.rootnameid
            , isPrivate = f.target.isPrivate
            , charac = { userCanJoin = Present charac.userCanJoin, mode = Present charac.mode, id = Absent }
            }

        nodeOptional =
            getAddCircleOptionals f
    in
    { input =
        [ Input.buildAddNodeInput nodeRequired nodeOptional ]
    }


getAddCircleOptionals : TensionForm -> (Input.AddNodeInputOptionalFields -> Input.AddNodeInputOptionalFields)
getAddCircleOptionals f =
    let
        createdAt =
            Dict.get "createdAt" f.post |> withDefault ""

        type_ =
            f.data.type_ |> withDefault NodeType.Role

        nameid =
            f.data.nameid |> Maybe.map (\nid -> nodeIdCodec f.target.nameid nid type_) |> withDefault ""

        first_links =
            f.data.first_link
                |> withDefault ""
                |> String.split "@"
                |> List.filter (\x -> x /= "")
    in
    \n ->
        let
            commonFields =
                { n
                    | parent =
                        Input.buildNodeRef
                            (\p -> { p | nameid = Present f.target.nameid })
                            |> Present
                    , about = fromMaybe f.data.about
                    , mandate = buildMandate f.data.mandate
                    , tensions_in =
                        [ Input.buildTensionRef (tensionFromForm f) ] |> Present
                }
        in
        case type_ of
            NodeType.Circle ->
                { commonFields
                    | children =
                        first_links
                            |> List.indexedMap
                                (\i uname ->
                                    Input.buildNodeRef
                                        (\c ->
                                            { c
                                                | createdAt = createdAt |> Fractal.Scalar.DateTime |> Present
                                                , createdBy =
                                                    Input.buildUserRef (\u -> { u | username = Present f.uctx.username }) |> Present
                                                , first_link =
                                                    Input.buildUserRef (\u -> { u | username = uname |> Present }) |> Present
                                                , isRoot = False |> Present
                                                , isPrivate = f.target.isPrivate |> Present
                                                , type_ = NodeType.Role |> Present
                                                , role_type = RoleType.Coordinator |> Present
                                                , name = "Coordinator" |> Present
                                                , nameid = (nameid ++ "#" ++ "coordo" ++ String.fromInt i) |> Present
                                                , rootnameid = f.target.rootnameid |> Present
                                                , charac = f.data.charac |> Maybe.map (\ch -> { userCanJoin = Present ch.userCanJoin, mode = Present ch.mode, id = Absent }) |> fromMaybe
                                            }
                                        )
                                )
                            |> Present
                }

            NodeType.Role ->
                let
                    first_link =
                        first_links |> List.head
                in
                { commonFields
                    | role_type = f.data.role_type |> fromMaybe
                    , first_link =
                        first_links
                            |> List.head
                            |> Maybe.map
                                (\uname ->
                                    Input.buildUserRef
                                        (\u -> { u | username = uname |> Present })
                                )
                            |> fromMaybe
                }


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
            , data = buildNodeFragmentRef f
        }


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


buildNodeFragmentRef : TensionForm -> OptionalArgument Input.NodeFragmentRef
buildNodeFragmentRef f =
    let
        nf =
            f.data
    in
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
                , first_link =
                    fromMaybe nf.first_link
            }
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
