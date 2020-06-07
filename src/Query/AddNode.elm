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
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Maybe exposing (withDefault)
import ModelCommon.Uri exposing (circleIdCodec, guestIdCodec)
import ModelSchema exposing (..)
import Query.QueryNodesOrga exposing (nodeOrgaPayload)
import RemoteData exposing (RemoteData)



{-
   Add a New member
-}


type alias AddNodeIDPayload =
    { node : Maybe (List (Maybe IdPayload)) }


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


addNewMember uctx post targetid msg =
    --@DEBUG: Infered type...
    makeGQLMutation
        (Mutation.addNode
            (newMemberInputEncoder uctx post targetid)
            (SelectionSet.map AddNodePayload <|
                Fractal.Object.AddNodePayload.node identity nodeOrgaPayload
            )
         --(SelectionSet.map AddNodeIDPayload <|
         --    Fractal.Object.AddNodePayload.node identity <|
         --        (SelectionSet.succeed IdPayload
         --            |> with (Fractal.Object.Node.id |> SelectionSet.map decodedId)
         --        )
         --)
        )
        (RemoteData.fromResult >> decodeResponse nodeDecoder >> msg)



-- Input Encoder


newMemberInputEncoder : UserCtx -> Post -> String -> Mutation.AddNodeRequiredArguments
newMemberInputEncoder uctx post targetid =
    let
        createdAt =
            Dict.get "createdAt" post |> withDefault ""

        nodeRequired =
            { createdAt = createdAt |> Fractal.Scalar.DateTime
            , createdBy =
                Input.buildUserRef
                    (\u -> { u | username = OptionalArgument.Present uctx.username })
            , type_ = NodeType.Role
            , nameid = guestIdCodec targetid uctx.username
            , name = "Guest"
            , rootnameid = targetid
            , isRoot = False
            , charac = { userCanJoin = OptionalArgument.Present False, mode = OptionalArgument.Present NodeMode.Coordinated }
            }

        nodeOptional =
            \n ->
                { n
                    | role_type = OptionalArgument.Present RoleType.Guest
                    , parent =
                        Input.buildNodeRef
                            (\p -> { p | nameid = OptionalArgument.Present targetid })
                            |> OptionalArgument.Present
                    , first_link =
                        Input.buildUserRef
                            (\u -> { u | username = OptionalArgument.Present uctx.username })
                            |> OptionalArgument.Present
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
    , parent : Maybe ParentNode -- see issue with recursive structure
    , children : Maybe (List Node)
    , type_ : NodeType.NodeType
    , role_type : Maybe RoleType.RoleType
    , first_link : Maybe Username
    , charac : NodeCharac
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


addOneCircle uctx post source target msg =
    --@DEBUG: Infered type...
    makeGQLMutation
        (Mutation.addNode
            (addCircleInputEncoder uctx post source target)
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
            --(Fractal.Object.Node.parent identity (SelectionSet.map (ParentNode << decodedId) Fractal.Object.Node.id))
            (Fractal.Object.Node.parent identity <| SelectionSet.map ParentNode Fractal.Object.Node.nameid)
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



-- Input Encoder


addCircleInputEncoder : UserCtx -> Post -> UserRole -> Node -> Mutation.AddNodeRequiredArguments
addCircleInputEncoder uctx post source target =
    let
        createdAt =
            Dict.get "createdAt" post |> withDefault ""

        nameid =
            Dict.get "nameid" post |> Maybe.map (\nid -> circleIdCodec target.nameid nid) |> withDefault ""

        name =
            Dict.get "name" post |> withDefault ""

        nodeMode =
            -- @DEBUG: Ignored from now, we inherit from the root mode
            Dict.get "node_mode" post |> withDefault "" |> NodeMode.fromString |> withDefault NodeMode.Coordinated

        first_links =
            Dict.get "first_links" post |> withDefault ""

        nodeRequired =
            { createdAt = createdAt |> Fractal.Scalar.DateTime
            , createdBy =
                Input.buildUserRef
                    (\u -> { u | username = OptionalArgument.Present uctx.username })
            , isRoot = False
            , type_ = NodeType.Circle
            , name = name
            , nameid = nameid
            , rootnameid = target.rootnameid
            , charac = { userCanJoin = OptionalArgument.Present False, mode = OptionalArgument.Present nodeMode }
            }

        nodeOptional =
            \n ->
                { n
                    | parent =
                        Input.buildNodeRef
                            (\p -> { p | nameid = OptionalArgument.Present target.nameid })
                            |> OptionalArgument.Present
                    , mandate =
                        Input.buildMandateRef
                            (\m ->
                                { m
                                    | purpose = Dict.get "purpose" post |> withDefault "" |> OptionalArgument.Present
                                    , responsabilities = Dict.get "responsabilities" post |> withDefault "" |> OptionalArgument.Present
                                    , domains = Dict.get "domains" post |> withDefault "" |> OptionalArgument.Present
                                    , tensions =
                                        [ Input.buildTensionRef (tensionFromForm uctx post source target)
                                        ]
                                            |> OptionalArgument.Present
                                }
                            )
                            |> OptionalArgument.Present
                    , children =
                        first_links
                            |> String.split "@"
                            |> List.filter (\x -> x /= "")
                            |> List.indexedMap
                                (\i uname ->
                                    Input.buildNodeRef
                                        (\c ->
                                            { c
                                                | createdAt = createdAt |> Fractal.Scalar.DateTime |> OptionalArgument.Present
                                                , createdBy =
                                                    Input.buildUserRef
                                                        (\u -> { u | username = OptionalArgument.Present uctx.username })
                                                        |> OptionalArgument.Present
                                                , isRoot = False |> OptionalArgument.Present
                                                , type_ = NodeType.Role |> OptionalArgument.Present
                                                , role_type = RoleType.Coordinator |> OptionalArgument.Present
                                                , name = "Coordinator" |> OptionalArgument.Present
                                                , nameid = (nameid ++ "#" ++ "coordo" ++ String.fromInt i) |> OptionalArgument.Present
                                                , rootnameid = target.rootnameid |> OptionalArgument.Present
                                                , charac =
                                                    { userCanJoin = OptionalArgument.Present False, mode = OptionalArgument.Present nodeMode }
                                                        |> OptionalArgument.Present
                                                , first_link =
                                                    Input.buildUserRef
                                                        (\u -> { u | username = uname |> OptionalArgument.Present })
                                                        |> OptionalArgument.Present
                                            }
                                        )
                                )
                            |> OptionalArgument.Present
                }
    in
    { input =
        [ Input.buildAddNodeInput nodeRequired nodeOptional ]
    }


tensionFromForm : UserCtx -> Post -> UserRole -> Node -> (Input.TensionRefOptionalFields -> Input.TensionRefOptionalFields)
tensionFromForm uctx post source target =
    let
        title =
            Dict.get "title" post |> withDefault ""

        createdAt =
            Dict.get "createdAt" post |> withDefault ""

        status =
            Dict.get "status" post |> withDefault "" |> TensionStatus.fromString |> withDefault TensionStatus.Open
    in
    \t ->
        { t
            | createdAt = createdAt |> Fractal.Scalar.DateTime |> OptionalArgument.Present
            , createdBy =
                Input.buildUserRef
                    (\x -> { x | username = OptionalArgument.Present uctx.username })
                    |> OptionalArgument.Present
            , title = title |> OptionalArgument.Present
            , type_ = TensionType.Governance |> OptionalArgument.Present
            , status = status |> OptionalArgument.Present
            , emitter =
                Input.buildNodeRef
                    (\x ->
                        { x
                            | nameid = OptionalArgument.Present source.nameid
                            , rootnameid = OptionalArgument.Present source.rootnameid
                        }
                    )
                    |> OptionalArgument.Present
            , receiver =
                Input.buildNodeRef
                    (\x ->
                        { x
                            | nameid = OptionalArgument.Present target.nameid
                            , rootnameid = OptionalArgument.Present target.rootnameid
                        }
                    )
                    |> OptionalArgument.Present
            , action = TensionAction.NewCircle |> OptionalArgument.Present
            , mandate =
                Input.buildMandateRef
                    (\m ->
                        { m
                            | purpose = Dict.get "purpose" post |> withDefault "" |> OptionalArgument.Present
                            , responsabilities = Dict.get "responsabilities" post |> withDefault "" |> OptionalArgument.Present
                            , domains = Dict.get "domains" post |> withDefault "" |> OptionalArgument.Present
                        }
                    )
                    |> OptionalArgument.Present
        }
