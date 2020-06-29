module Query.AddNode exposing (addNewMember, addOneCircle)

import Dict exposing (Dict)
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
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
import Query.QueryNodes exposing (nodeOrgaPayload)
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


addNewMember form msg =
    --@DEBUG: Infered type...
    makeGQLMutation
        (Mutation.addNode
            (newMemberInputEncoder form)
            (SelectionSet.map AddNodePayload <|
                Fractal.Object.AddNodePayload.node identity nodeOrgaPayload
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
            , charac = { userCanJoin = Present False, mode = Present NodeMode.Coordinated }
            , isPrivate = False
            }

        nodeOptional =
            \n ->
                { n
                    | role_type = Present RoleType.Guest
                    , parent =
                        Input.buildNodeRef
                            (\p -> { p | nameid = Present rootnameid, id = id |> Maybe.map (\i -> encodeId i) |> fromMaybe })
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
   Add a Circle
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


addOneCircle form msg =
    --@DEBUG: Infered type...
    makeGQLMutation
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
            (Fractal.Object.Node.parent identity <| SelectionSet.map NodeId Fractal.Object.Node.nameid)
        |> with (Fractal.Object.Node.children identity nodeOrgaPayload)
        |> with Fractal.Object.Node.type_
        |> with Fractal.Object.Node.role_type
        |> with (Fractal.Object.Node.first_link identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with
            (Fractal.Object.Node.charac <|
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

        nameid =
            Dict.get "nameid" f.post |> Maybe.map (\nid -> nodeIdCodec f.target.nameid nid f.type_) |> withDefault ""

        name =
            Dict.get "name" f.post |> withDefault ""

        nodeMode =
            -- @DEBUG: Ignored from now, we inherit from the root mode
            Dict.get "node_mode" f.post |> withDefault "" |> NodeMode.fromString |> withDefault NodeMode.Coordinated

        nodeRequired =
            { createdAt = createdAt |> Fractal.Scalar.DateTime
            , createdBy =
                Input.buildUserRef
                    (\u -> { u | username = Present f.uctx.username })
            , isRoot = False
            , type_ = f.type_
            , name = name
            , nameid = nameid
            , rootnameid = f.target.rootnameid
            , charac = { userCanJoin = Present False, mode = Present nodeMode }
            , isPrivate = f.target.isPrivate
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

        nameid =
            Dict.get "nameid" f.post |> Maybe.map (\nid -> nodeIdCodec f.target.nameid nid f.type_) |> withDefault ""

        nodeMode =
            -- @DEBUG: Ignored from now, we inherit from the root mode
            Dict.get "node_mode" f.post |> withDefault "" |> NodeMode.fromString |> withDefault NodeMode.Coordinated

        first_links =
            Dict.get "first_links" f.post
                |> withDefault ""
                |> String.split "@"
                |> List.filter (\x -> x /= "")
    in
    case f.type_ of
        NodeType.Circle ->
            \n ->
                { n
                    | parent =
                        Input.buildNodeRef
                            (\p -> { p | nameid = Present f.target.nameid, id = encodeId f.target.id |> Present })
                            |> Present
                    , mandate =
                        Input.buildMandateRef
                            (\m ->
                                { m
                                    | purpose = Dict.get "purpose" f.post |> fromMaybe
                                    , responsabilities = Dict.get "responsabilities" f.post |> fromMaybe
                                    , domains = Dict.get "domains" f.post |> fromMaybe
                                    , policies = Dict.get "policies" f.post |> fromMaybe
                                    , tensions =
                                        [ Input.buildTensionRef (tensionFromForm f) ]
                                            |> Present
                                }
                            )
                            |> Present
                    , children =
                        first_links
                            |> List.indexedMap
                                (\i uname ->
                                    Input.buildNodeRef
                                        (\c ->
                                            { c
                                                | createdAt = createdAt |> Fractal.Scalar.DateTime |> Present
                                                , createdBy =
                                                    Input.buildUserRef
                                                        (\u -> { u | username = Present f.uctx.username })
                                                        |> Present
                                                , isRoot = False |> Present
                                                , isPrivate = f.target.isPrivate |> Present
                                                , type_ = NodeType.Role |> Present
                                                , role_type = RoleType.Coordinator |> Present
                                                , name = "Coordinator" |> Present
                                                , nameid = (nameid ++ "#" ++ "coordo" ++ String.fromInt i) |> Present
                                                , rootnameid = f.target.rootnameid |> Present
                                                , charac =
                                                    { userCanJoin = Present False, mode = Present nodeMode }
                                                        |> Present
                                                , first_link =
                                                    Input.buildUserRef
                                                        (\u -> { u | username = uname |> Present })
                                                        |> Present
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
            \n ->
                { n
                    | parent =
                        Input.buildNodeRef
                            (\p -> { p | nameid = Present f.target.nameid, id = encodeId f.target.id |> Present })
                            |> Present
                    , mandate =
                        Input.buildMandateRef
                            (\m ->
                                { m
                                    | purpose = Dict.get "purpose" f.post |> fromMaybe
                                    , responsabilities = Dict.get "responsabilities" f.post |> fromMaybe
                                    , domains = Dict.get "domains" f.post |> fromMaybe
                                    , policies = Dict.get "policies" f.post |> fromMaybe
                                    , tensions =
                                        [ Input.buildTensionRef (tensionFromForm f) ]
                                            |> Present
                                }
                            )
                            |> Present
                    , role_type = f.role_type |> Present
                    , first_link =
                        first_link
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
            , message = fromMaybe message
            , emitterid = Present f.source.nameid
            , receiverid = Present f.target.nameid
            , emitter =
                Input.buildNodeRef
                    (\x ->
                        { x
                            | nameid = Present f.source.nameid
                            , rootnameid = Present f.source.rootnameid
                        }
                    )
                    |> Present
            , receiver =
                Input.buildNodeRef
                    (\x ->
                        { x
                            | nameid = Present f.target.nameid
                            , rootnameid = Present f.target.rootnameid
                        }
                    )
                    |> Present
            , mandate =
                Input.buildMandateRef
                    (\m ->
                        { m
                            | purpose = Dict.get "purpose" f.post |> withDefault "" |> Present
                            , responsabilities = Dict.get "responsabilities" f.post |> withDefault "" |> Present
                            , domains = Dict.get "domains" f.post |> withDefault "" |> Present
                        }
                    )
                    |> Present
        }
