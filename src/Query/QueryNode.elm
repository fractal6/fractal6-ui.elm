{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2024 Fractale Co

   This file is part of Fractale.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
-}


module Query.QueryNode exposing
    ( MemberNode
    , blobIdPayload
    , cidPayload
    , contractEventPayload
    , emiterOrReceiverPayload
    , emiterOrReceiverWithPinPayload
    , fetchNode
    , fetchNode2
    , fetchNodeData
    , getCircleRights
    , getLabels
    , getNodeId
    , getOrgaInfo
    , getProjects
    , getRoles
    , labelFullPayload
    , labelPayload
    , mandatePayload
    , membersNodeDecoder
    , nidFilter
    , nodeDecoder
    , nodeIdPayload
    , nodeOrgaFilter
    , nodeOrgaPayload
    , notifEventPayload
    , pNodePayload
    , pinPayload
    , projectFullPayload
    , queryJournal
    , queryLabels
    , queryLabelsDown
    , queryLocalGraph
    , queryMembers
    , queryMembersLocal
    , queryNodeExt
    , queryNodesSub
    , queryOrgaNode
    , queryOrgaTree
    , queryProjects
    , queryPublicOrga
    , queryRolesFull
    , roleFullPayload
    , searchUserFilter
    , tensionEventPayload
    , tidPayload
    , userPayload
    )

import Bulk.Codecs exposing (activeMembershipRoleTypes, membershipRoleTypes, nid2rootid)
import Dict exposing (Dict)
import Extra exposing (ternary, unwrap, unwrap2)
import Fractal.Enum.ContractStatus as ContractStatus
import Fractal.Enum.LabelOrderable as LabelOrderable
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeOrderable as NodeOrderable
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.NodeVisibility as NodeVisibility
import Fractal.Enum.ProjectOrderable as ProjectOrderable
import Fractal.Enum.ProjectStatus as ProjectStatus
import Fractal.Enum.RoleExtOrderable as RoleExtOrderable
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.InputObject as Input
import Fractal.Object
import Fractal.Object.Blob
import Fractal.Object.BuildInfo
import Fractal.Object.Contract
import Fractal.Object.ContractAggregateResult
import Fractal.Object.Event
import Fractal.Object.EventFragment
import Fractal.Object.Label
import Fractal.Object.Mandate
import Fractal.Object.Node
import Fractal.Object.NodeAggregateResult
import Fractal.Object.NodeFragment
import Fractal.Object.Notif
import Fractal.Object.Project
import Fractal.Object.ProjectAggregateResult
import Fractal.Object.RoleExt
import Fractal.Object.Tension
import Fractal.Object.TensionAggregateResult
import Fractal.Object.User
import Fractal.Object.UserAggregateResult
import Fractal.Query as Query
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import List.Extra as LE
import Loading exposing (RequestResult(..))
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import RemoteData
import String.Extra as SE



--
-- Query Public Orga / Explore
--


nodeDecoder : Maybe (List (Maybe node)) -> Maybe node
nodeDecoder data =
    data
        |> Maybe.map
            (\d ->
                if List.length d == 0 then
                    Nothing

                else
                    d
                        |> List.filterMap identity
                        |> List.head
            )
        |> withDefault Nothing


nodesDecoder : Maybe (List (Maybe node)) -> Maybe (List node)
nodesDecoder data =
    data
        |> Maybe.map
            (\d ->
                if List.length d == 0 then
                    Nothing

                else
                    d
                        |> List.filterMap identity
                        |> Just
            )
        |> withDefault Nothing


queryPublicOrga url msg =
    makeGQLQuery url
        (Query.queryNode
            publicOrgaFilter
            nodeOrgaExtPayload
        )
        (RemoteData.fromResult >> decodeResponse nodesDecoder >> msg)


publicOrgaFilter : Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
publicOrgaFilter a =
    { a
        | filter =
            Input.buildNodeFilter
                (\b ->
                    { b
                        | isRoot = Present True
                        , visibility = Present { eq = Present NodeVisibility.Public, in_ = Absent }

                        --, not = Input.buildNodeFilter (\c -> { c | isPersonal = Present True }) |> Present
                    }
                )
                |> Present
        , order =
            Input.buildNodeOrder
                (\b -> { b | desc = Present NodeOrderable.CreatedAt })
                |> Present
    }


nodeOrgaExtPayload : SelectionSet NodeExt Fractal.Object.Node
nodeOrgaExtPayload =
    SelectionSet.succeed NodeExt
        |> with (Fractal.Object.Node.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Node.createdAt |> SelectionSet.map decodedTime)
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with (Fractal.Object.Node.parent identity nodeIdPayload)
        |> with Fractal.Object.Node.type_
        |> with Fractal.Object.Node.role_type
        |> with (Fractal.Object.Node.first_link identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with Fractal.Object.Node.visibility
        |> with Fractal.Object.Node.about
        |> with
            (SelectionSet.map (unwrap2 0 .count) <|
                Fractal.Object.Node.childrenAggregate
                    (\a ->
                        { a
                            | filter =
                                Present <|
                                    Input.buildNodeFilter
                                        (\x ->
                                            { x
                                                | type_ = Present { eq = Present NodeType.Role, in_ = Absent }
                                                , nameid = Present { regexp = Present "/^.*##@/", eq = Absent, in_ = Absent }
                                                , not = Present <| Input.buildNodeFilter (\y -> { y | role_type = Present { in_ = Present [ Just RoleType.Retired, Just RoleType.Pending ], eq = Absent } })
                                            }
                                        )
                        }
                    )
                <|
                    SelectionSet.map Count Fractal.Object.NodeAggregateResult.count
            )
        |> with
            (SelectionSet.map (unwrap2 0 .count) <|
                Fractal.Object.Node.watchersAggregate identity <|
                    SelectionSet.map Count Fractal.Object.UserAggregateResult.count
            )



--
-- Query Node Ext / Profile
--


queryNodeExt url nameids msg =
    makeGQLQuery url
        (Query.queryNode
            (nodeExtFilter nameids)
            nodeOrgaExtPayload
        )
        (RemoteData.fromResult >> decodeResponse nodesDecoder >> msg)


queryOrgaNode url nameids msg =
    makeGQLQuery url
        (Query.queryNode
            (nodeExtFilter nameids)
            (SelectionSet.map2 OrgaNode
                Fractal.Object.Node.name
                Fractal.Object.Node.nameid
            )
        )
        (RemoteData.fromResult >> decodeResponse nodesDecoder >> msg)


nodeExtFilter : List String -> Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
nodeExtFilter nameids a =
    { a
        | filter =
            Input.buildNodeFilter
                (\b ->
                    { b
                        | nameid = { regexp = Absent, eq = Absent, in_ = List.map Just nameids |> Present } |> Present
                    }
                )
                |> Present
        , order =
            Input.buildNodeOrder (\b -> { b | desc = Present NodeOrderable.UpdatedAt })
                |> Present
    }



--
-- Query Node and Sub Nodes / FetchNodes
--


queryNodesSub url nameid msg =
    makeGQLQuery url
        (Query.queryNode
            (nodesSubFilter nameid)
            nodeOrgaPayload
        )
        (RemoteData.fromResult >> decodeResponse nodesDecoder >> msg)


nodesSubFilter : String -> Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
nodesSubFilter nameid a =
    let
        nameidRegxp =
            "/^" ++ nameid ++ "/"
    in
    { a
        | filter =
            Input.buildNodeFilter
                (\b ->
                    { b
                        | nameid = { eq = Absent, in_ = Absent, regexp = Present nameidRegxp } |> Present
                    }
                )
                |> Present
    }



--
-- Query Organisation Nodes / GraphPack
--


nodeOrgaDecoder : Maybe (List (Maybe Node)) -> Maybe (Dict String Node)
nodeOrgaDecoder data =
    data
        |> Maybe.map
            (\d ->
                if List.length d == 0 then
                    Nothing

                else
                    d
                        |> List.filterMap (Maybe.map (\n -> ( n.nameid, n )))
                        |> Dict.fromList
                        |> Just
            )
        |> withDefault Nothing


queryOrgaTree url rootid msg =
    makeGQLQuery url
        (Query.queryNode
            (nodeOrgaFilter rootid membershipRoleTypes)
            nodeOrgaPayload
        )
        (RemoteData.fromResult >> decodeResponse nodeOrgaDecoder >> msg)


nodeOrgaFilter : String -> List RoleType.RoleType -> Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
nodeOrgaFilter rootid alls a =
    { a
        | filter =
            Input.buildNodeFilter
                (\b ->
                    { b
                        | rootnameid = Present { eq = Present rootid, in_ = Absent, regexp = Absent }
                        , not =
                            Input.buildNodeFilter (\sd -> { sd | isArchived = Present True, or = matchAnyRoleType alls })
                                |> Present
                    }
                )
                |> Present
    }


matchAnyRoleType : List RoleType.RoleType -> OptionalArgument (List (Maybe Input.NodeFilter))
matchAnyRoleType alls =
    --List.foldl
    --    (\x filter ->
    --        Input.buildNodeFilter
    --            (\d ->
    --                { d
    --                    | role_type = Present { eq = Present x }
    --                    , or = filter
    --                }
    --            )
    --            |> Present
    --    )
    --    Absent
    --    alls
    Present
        [ Input.buildNodeFilter
            (\d ->
                { d
                    | role_type = Present { eq = Absent, in_ = alls |> List.map Just |> Present }
                }
            )
            |> Just
        ]


nodeOrgaPayload : SelectionSet Node Fractal.Object.Node
nodeOrgaPayload =
    SelectionSet.succeed Node
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with (Fractal.Object.Node.parent identity nodeIdPayload)
        |> with Fractal.Object.Node.type_
        |> with Fractal.Object.Node.role_type
        |> with Fractal.Object.Node.color
        |> with (Fractal.Object.Node.first_link identity userPayload)
        |> with Fractal.Object.Node.visibility
        |> with Fractal.Object.Node.mode
        |> with (Fractal.Object.Node.source identity blobIdPayload)
        |> with Fractal.Object.Node.userCanJoin
        |> with
            (SelectionSet.map (unwrap2 0 .count) <|
                Fractal.Object.Node.tensions_inAggregate (\a -> { a | filter = Present <| Input.buildTensionFilter (\x -> { x | status = Present { eq = Present TensionStatus.Open, in_ = Absent } }) }) <|
                    SelectionSet.map Count Fractal.Object.TensionAggregateResult.count
            )
        |> with
            (SelectionSet.map (\x -> withDefault Nothing x |> unwrap2 0 .count) <|
                Fractal.Object.Node.source identity
                    (SelectionSet.map identity
                        (Fractal.Object.Blob.tension identity
                            (SelectionSet.map identity <|
                                Fractal.Object.Tension.contractsAggregate (\a -> { a | filter = Present <| Input.buildContractFilter (\x -> { x | status = Present { eq = Present ContractStatus.Open, in_ = Absent } }) }) <|
                                    SelectionSet.map Count Fractal.Object.ContractAggregateResult.count
                            )
                        )
                    )
            )


{-| With blob id
-}
nodeOrgaPayload2 : SelectionSet Node Fractal.Object.Node
nodeOrgaPayload2 =
    SelectionSet.succeed Node
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with (Fractal.Object.Node.parent identity nodeIdPayload2)
        |> with Fractal.Object.Node.type_
        |> with Fractal.Object.Node.role_type
        |> with Fractal.Object.Node.color
        |> with (Fractal.Object.Node.first_link identity userPayload)
        |> with Fractal.Object.Node.visibility
        |> with Fractal.Object.Node.mode
        |> with (Fractal.Object.Node.source identity blobIdPayload)
        |> with Fractal.Object.Node.userCanJoin
        |> hardcoded 0
        |> hardcoded 0


nodeIdPayload : SelectionSet NodeId Fractal.Object.Node
nodeIdPayload =
    SelectionSet.succeed NodeId
        |> with Fractal.Object.Node.nameid
        |> hardcoded Nothing


nodeIdPayload2 : SelectionSet NodeId Fractal.Object.Node
nodeIdPayload2 =
    SelectionSet.succeed NodeId
        |> with Fractal.Object.Node.nameid
        |> with (Fractal.Object.Node.source identity blobIdPayload)


blobIdPayload : SelectionSet BlobId Fractal.Object.Blob
blobIdPayload =
    SelectionSet.succeed BlobId
        |> with (Fractal.Object.Blob.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Blob.tension identity tidPayload)


userPayload : SelectionSet User Fractal.Object.User
userPayload =
    SelectionSet.map2 User
        Fractal.Object.User.username
        Fractal.Object.User.name


pNodePayload : SelectionSet PNode Fractal.Object.Node
pNodePayload =
    SelectionSet.succeed PNode
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> hardcoded Nothing


tidPayload : SelectionSet IdPayload Fractal.Object.Tension
tidPayload =
    SelectionSet.map IdPayload
        (SelectionSet.map decodedId Fractal.Object.Tension.id)


cidPayload : SelectionSet IdPayload Fractal.Object.Contract
cidPayload =
    SelectionSet.map IdPayload
        (SelectionSet.map decodedId Fractal.Object.Contract.id)



--
-- Get the node data /about, mandate, etc)
--


type alias NodeDataSource =
    { source : Maybe { node : Maybe { about : Maybe String, mandate : Maybe Mandate } } }


nodeDataSourceDecoder : Maybe NodeDataSource -> Maybe NodeData
nodeDataSourceDecoder data =
    data
        |> unwrap Nothing .source
        |> Maybe.map
            (\x ->
                { about = unwrap Nothing .about x.node
                , mandate = unwrap Nothing .mandate x.node
                }
            )


fetchNodeData url nameid msg =
    makeGQLQuery url
        (Query.getNode
            (nidFilter nameid)
            nodeDataPayload
        )
        (RemoteData.fromResult >> decodeResponse nodeDataSourceDecoder >> msg)


nodeDataPayload : SelectionSet NodeDataSource Fractal.Object.Node
nodeDataPayload =
    SelectionSet.succeed NodeDataSource
        |> with
            (Fractal.Object.Node.source identity
                (SelectionSet.map (\x -> { node = x })
                    (Fractal.Object.Blob.node identity
                        (SelectionSet.map2 (\xx yy -> { about = xx, mandate = yy })
                            Fractal.Object.NodeFragment.about
                            (Fractal.Object.NodeFragment.mandate identity mandatePayload)
                        )
                    )
                )
            )


mandatePayload : SelectionSet Mandate Fractal.Object.Mandate
mandatePayload =
    SelectionSet.succeed Mandate
        |> with Fractal.Object.Mandate.purpose
        |> with Fractal.Object.Mandate.responsabilities
        |> with Fractal.Object.Mandate.domains
        |> with Fractal.Object.Mandate.policies



--
-- Get Node
--


fetchNode url nid msg =
    makeGQLQuery url
        (Query.getNode
            (nidFilter nid)
            nodeOrgaPayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


fetchNode2 url nid msg =
    makeGQLQuery url
        (Query.getNode
            (nidFilter nid)
            nodeOrgaPayload2
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


getNodeId url nid msg =
    makeGQLQuery url
        (Query.getNode
            (nidFilter nid)
            nodeIdPayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


nidFilter : String -> Query.GetNodeOptionalArguments -> Query.GetNodeOptionalArguments
nidFilter nid a =
    { a | nameid = Present nid }


nidsFilter : List String -> Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
nidsFilter nids a =
    { a
        | filter =
            Input.buildNodeFilter
                (\c ->
                    { c | nameid = Present { eq = Absent, regexp = Absent, in_ = List.map Just nids |> Present } }
                )
                |> Present
    }


nidsDownFilter : List String -> Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
nidsDownFilter nids a =
    let
        nameidsRegxp =
            nids
                |> List.map (\n -> "^" ++ n)
                |> String.join "|"
                |> SE.surround "/"
    in
    { a
        | filter =
            Input.buildNodeFilter
                (\c ->
                    { c | nameid = Present { eq = Absent, in_ = Absent, regexp = Present nameidsRegxp } }
                )
                |> Present
    }



--
-- Query Local Graph / Path Data
--


type alias LocalNode =
    { name : String
    , nameid : String
    , type_ : NodeType.NodeType
    , visibility : NodeVisibility.NodeVisibility
    , mode : NodeMode.NodeMode
    , userCanJoin : Maybe Bool
    , source : Maybe BlobId
    , parent : Maybe LocalRootNode
    , children : Maybe (List EmitterOrReceiver)
    , pinned : Maybe (List PinTension)
    }


type alias LocalRootNode =
    { isRoot : Bool
    , name : String
    , nameid : String
    , userCanJoin : Maybe Bool
    , mode : NodeMode.NodeMode
    , source : Maybe BlobId
    }


ln2fn : LocalNode -> FocusNode
ln2fn n =
    FocusNode n.name n.nameid n.type_ n.visibility n.mode n.source (withDefault [] n.children) (Success n.pinned)


lgDecoder : Maybe LocalNode -> Maybe LocalGraph
lgDecoder data =
    data
        |> Maybe.map
            (\n ->
                case n.parent of
                    Just p ->
                        if p.isRoot then
                            { root = RNode p.name p.nameid p.userCanJoin p.mode |> Just
                            , path = [ shrinkNode p, shrinkNode n ]
                            , focus = ln2fn n
                            }

                        else
                            -- partial path
                            { root = Nothing
                            , path = [ shrinkNode p, shrinkNode n ]
                            , focus = ln2fn n
                            }

                    Nothing ->
                        -- Assume Root node
                        { root = RNode n.name n.nameid n.userCanJoin n.mode |> Just
                        , path = [ shrinkNode n ]
                        , focus = ln2fn n
                        }
            )


queryLocalGraph url nid isInit msg =
    makeGQLQuery url
        (Query.getNode
            (nidFilter nid)
            (lgPayload isInit)
        )
        (RemoteData.fromResult >> decodeResponse lgDecoder >> msg)


lgPayload : Bool -> SelectionSet LocalNode Fractal.Object.Node
lgPayload isInit =
    SelectionSet.succeed LocalNode
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with Fractal.Object.Node.type_
        |> with Fractal.Object.Node.visibility
        |> with Fractal.Object.Node.mode
        |> with Fractal.Object.Node.userCanJoin
        |> with (Fractal.Object.Node.source identity blobIdPayload)
        |> with (Fractal.Object.Node.parent identity lg2Payload)
        |> (\x ->
                if isInit then
                    x
                        |> with (Fractal.Object.Node.children lgChildrenFilter emiterOrReceiverPayload)
                        |> with (Fractal.Object.Node.pinned identity pinPayload |> SelectionSet.map (\y -> ternary (y == Just []) Nothing y))

                else
                    x
                        |> hardcoded Nothing
                        |> hardcoded Nothing
           )


lg2Payload : SelectionSet LocalRootNode Fractal.Object.Node
lg2Payload =
    SelectionSet.succeed LocalRootNode
        |> with Fractal.Object.Node.isRoot
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with Fractal.Object.Node.userCanJoin
        |> with Fractal.Object.Node.mode
        |> with (Fractal.Object.Node.source identity blobIdPayload)


mbChildrenFilter : Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
mbChildrenFilter a =
    { a
        | filter =
            Input.buildNodeFilter
                (\b ->
                    { b
                        | not = Input.buildNodeFilter (\sd -> { sd | isArchived = Present True, or = matchAnyRoleType [ RoleType.Retired ] }) |> Present
                    }
                )
                |> Present
    }


lgChildrenFilter : Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
lgChildrenFilter a =
    { a
        | filter =
            Input.buildNodeFilter
                (\b ->
                    { b
                        | not = Input.buildNodeFilter (\sd -> { sd | isArchived = Present True, or = matchAnyRoleType [ RoleType.Member, RoleType.Guest, RoleType.Pending, RoleType.Retired ] }) |> Present
                    }
                )
                |> Present
    }


emiterOrReceiverPayload : SelectionSet EmitterOrReceiver Fractal.Object.Node
emiterOrReceiverPayload =
    SelectionSet.succeed EmitterOrReceiver
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with Fractal.Object.Node.role_type
        |> with Fractal.Object.Node.color


emiterOrReceiverWithPinPayload : String -> SelectionSet (NodeWithPin EmitterOrReceiver) Fractal.Object.Node
emiterOrReceiverWithPinPayload tid =
    SelectionSet.succeed (\a b c d e -> { name = a, nameid = b, role_type = c, color = d, pinned = e })
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with Fractal.Object.Node.role_type
        |> with Fractal.Object.Node.color
        |> with
            (Fractal.Object.Node.pinned
                (\a ->
                    { a
                        | first = Present 1
                        , filter =
                            Input.buildTensionFilter
                                (\b -> { b | id = Present [ encodeId tid ] })
                                |> Present
                    }
                )
                tidPayload
            )


pinPayload : SelectionSet PinTension Fractal.Object.Tension
pinPayload =
    SelectionSet.succeed PinTension
        |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
        |> with Fractal.Object.Tension.title
        |> with (Fractal.Object.Tension.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.Tension.createdBy identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with Fractal.Object.Tension.type_
        |> with Fractal.Object.Tension.status



--
-- Query Orga rights
--


getCircleRights url nameid msg =
    makeGQLQuery url
        (Query.getNode
            (nidFilter nameid)
            nodeRightsPayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


nodeRightsPayload : SelectionSet NodeRights Fractal.Object.Node
nodeRightsPayload =
    SelectionSet.succeed NodeRights
        |> with Fractal.Object.Node.visibility
        |> with Fractal.Object.Node.userCanJoin
        |> with Fractal.Object.Node.guestCanCreateTension



--
-- Query Members
--


type alias NodeMembers =
    { first_link : Maybe User }


membersDecoder : Maybe (List (Maybe NodeMembers)) -> Maybe (List User)
membersDecoder data =
    data
        |> Maybe.map
            (\d ->
                if List.length d == 0 then
                    Nothing

                else
                    d
                        |> List.filterMap identity
                        |> List.filterMap .first_link
                        |> Just
            )
        |> withDefault Nothing


queryMembers url nids msg =
    let
        rootid =
            nids |> LE.last |> withDefault "" |> nid2rootid
    in
    makeGQLQuery url
        (Query.queryNode
            (membersFilter rootid)
            membersPayload
        )
        (RemoteData.fromResult >> decodeResponse membersDecoder >> msg)


membersFilter : String -> Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
membersFilter rootid a =
    { a
        | filter =
            Input.buildNodeFilter
                (\c ->
                    { c
                        | rootnameid = Present { eq = Present rootid, in_ = Absent, regexp = Absent }
                        , and = matchAnyRoleType activeMembershipRoleTypes

                        -- @todo pending members
                    }
                )
                |> Present
    }


membersPayload : SelectionSet NodeMembers Fractal.Object.Node
membersPayload =
    SelectionSet.succeed NodeMembers
        |> with (Fractal.Object.Node.first_link identity userPayload)



--
-- Query Local Members
--


type alias LocalMemberNode =
    { createdAt : String
    , name : String
    , nameid : String
    , role_type : Maybe RoleType.RoleType
    , color : Maybe String
    , first_link : Maybe User
    , parent : Maybe NodeId
    , children : Maybe (List MemberNode)
    }


type alias MemberNode =
    { createdAt : String
    , name : String
    , nameid : String
    , role_type : Maybe RoleType.RoleType
    , color : Maybe String
    , first_link : Maybe User
    , parent : Maybe NodeId
    }


membersLocalDecoder : Maybe LocalMemberNode -> Maybe (List Member)
membersLocalDecoder data =
    data
        |> Maybe.map
            (\n ->
                case n.first_link of
                    Just first_link ->
                        Just [ Member first_link.username first_link.name [ node2role n ] ]

                    Nothing ->
                        case n.children of
                            Just children ->
                                Just <| membersNodeDecoder children

                            Nothing ->
                                Nothing
            )
        |> withDefault Nothing


node2role n =
    -- n -> UserRoleExtended
    UserRoleExtended n.name n.nameid (withDefault RoleType.Guest n.role_type) n.color n.createdAt n.parent


membersNodeDecoder : List MemberNode -> List Member
membersNodeDecoder nodes =
    let
        toTuples : MemberNode -> List ( String, Member )
        toTuples m =
            case m.first_link of
                Just fs ->
                    [ ( fs.username, Member fs.username fs.name [ node2role m ] ) ]

                Nothing ->
                    []

        toDict : List ( String, Member ) -> Dict String Member
        toDict inputs =
            List.foldl
                (\( k, v ) dict -> Dict.update k (addParam v) dict)
                Dict.empty
                inputs

        addParam : Member -> Maybe Member -> Maybe Member
        addParam m maybeMember =
            case maybeMember of
                Just member ->
                    Just { member | roles = member.roles ++ m.roles }

                Nothing ->
                    Just m
    in
    List.concatMap toTuples nodes
        |> toDict
        |> Dict.values


queryMembersLocal url nid pattern msg =
    makeGQLQuery url
        (Query.getNode
            (nidFilter nid)
            (membersLocalPayload pattern)
        )
        (RemoteData.fromResult >> decodeResponse membersLocalDecoder >> msg)


membersLocalPayload : Maybe String -> SelectionSet LocalMemberNode Fractal.Object.Node
membersLocalPayload pattern =
    SelectionSet.succeed LocalMemberNode
        |> with (Fractal.Object.Node.createdAt |> SelectionSet.map decodedTime)
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with Fractal.Object.Node.role_type
        |> with Fractal.Object.Node.color
        |> with (Fractal.Object.Node.first_link identity userPayload)
        |> hardcoded Nothing
        |> with
            (Fractal.Object.Node.children mbChildrenFilter
                (SelectionSet.succeed MemberNode
                    |> with (Fractal.Object.Node.createdAt |> SelectionSet.map decodedTime)
                    |> with Fractal.Object.Node.name
                    |> with Fractal.Object.Node.nameid
                    |> with Fractal.Object.Node.role_type
                    |> with Fractal.Object.Node.color
                    |> with (Fractal.Object.Node.first_link (searchUserFilter pattern) userPayload)
                    |> hardcoded Nothing
                )
            )



--searchUserFilter : Maybe String -> Fractal.Object.Node.FirstLinkOptionalArguments -> Fractal.Object.Node.FirstLinkOptionalArguments


searchUserFilter pattern a =
    { a
        | filter =
            Maybe.map
                (\p ->
                    Input.buildUserFilter
                        (\b ->
                            { b
                                | username = { regexp = Present ("/" ++ p ++ "/"), eq = Absent, in_ = Absent } |> Present
                                , or =
                                    Present
                                        [ Input.buildUserFilter
                                            (\c ->
                                                { c | name = { regexp = Present ("/" ++ p ++ "/") } |> Present }
                                            )
                                            |> Just
                                        ]
                            }
                        )
                )
                pattern
                |> fromMaybe
    }



--
-- Query RoleExt (Full)
--


type alias NodeRolesFull =
    { roles : Maybe (List RoleExtFull) }


rolesFullDecoder : Maybe NodeRolesFull -> Maybe (List RoleExtFull)
rolesFullDecoder data =
    data
        |> Maybe.map (\d -> withDefault [] d.roles)


getRoles url nid msg =
    -- Fetch on the given node
    makeGQLQuery url
        (Query.getNode
            (nidFilter nid)
            nodeRolesFullPayload
        )
        (RemoteData.fromResult >> decodeResponse rolesFullDecoder >> msg)


rolesFullDecoder2 : Maybe (List (Maybe NodeRolesFull)) -> Maybe (List RoleExtFull)
rolesFullDecoder2 data =
    data
        |> Maybe.map
            (\d ->
                if List.length d == 0 then
                    Nothing

                else
                    d
                        |> List.filterMap identity
                        |> List.concatMap (\x -> withDefault [] x.roles)
                        |> LE.uniqueBy .id
                        |> Just
            )
        |> withDefault Nothing


queryRolesFull url nids msg =
    -- Fetch on the given group of nodes
    makeGQLQuery url
        (Query.queryNode
            (nidsFilter nids)
            nodeRolesFullPayload
        )
        (RemoteData.fromResult >> decodeResponse rolesFullDecoder2 >> msg)


nodeRolesFullPayload : SelectionSet NodeRolesFull Fractal.Object.Node
nodeRolesFullPayload =
    SelectionSet.map NodeRolesFull
        (Fractal.Object.Node.roles
            (\args ->
                { args
                    | order =
                        Input.buildRoleExtOrder (\b -> { b | asc = Present RoleExtOrderable.Name })
                            |> Present
                }
            )
            roleFullPayload
        )


roleFullPayload : SelectionSet RoleExtFull Fractal.Object.RoleExt
roleFullPayload =
    SelectionSet.map8 RoleExtFull
        (Fractal.Object.RoleExt.id |> SelectionSet.map decodedId)
        Fractal.Object.RoleExt.name
        Fractal.Object.RoleExt.color
        Fractal.Object.RoleExt.role_type
        Fractal.Object.RoleExt.about
        (Fractal.Object.RoleExt.mandate identity mandatePayload)
        (SelectionSet.map (unwrap Nothing .count) <|
            Fractal.Object.RoleExt.nodesAggregate identity <|
                SelectionSet.map Count Fractal.Object.NodeAggregateResult.count
        )
        (SelectionSet.map (unwrap Nothing .count) <|
            Fractal.Object.RoleExt.rolesAggregate identity <|
                SelectionSet.map Count Fractal.Object.NodeAggregateResult.count
        )



--
-- Query Labels (Full)
--


type alias NodeLabelsFull =
    { labels : Maybe (List LabelFull) }


labelsFullDecoder : Maybe NodeLabelsFull -> Maybe (List LabelFull)
labelsFullDecoder data =
    data
        |> Maybe.map (\d -> withDefault [] d.labels)


getLabels url nid msg =
    -- Fetch on the given node
    makeGQLQuery url
        (Query.getNode
            (nidFilter nid)
            nodeLabelsFullPayload
        )
        (RemoteData.fromResult >> decodeResponse labelsFullDecoder >> msg)


nodeLabelsFullPayload : SelectionSet NodeLabelsFull Fractal.Object.Node
nodeLabelsFullPayload =
    SelectionSet.map NodeLabelsFull
        (Fractal.Object.Node.labels
            (\args ->
                { args
                    | order =
                        Input.buildLabelOrder (\b -> { b | asc = Present LabelOrderable.Name })
                            |> Present
                }
            )
            labelFullPayload
        )


labelFullPayload : SelectionSet LabelFull Fractal.Object.Label
labelFullPayload =
    SelectionSet.map5 LabelFull
        (Fractal.Object.Label.id |> SelectionSet.map decodedId)
        Fractal.Object.Label.name
        Fractal.Object.Label.color
        Fractal.Object.Label.description
        (SelectionSet.map (unwrap Nothing .count) <|
            Fractal.Object.Label.nodesAggregate identity <|
                SelectionSet.map Count Fractal.Object.NodeAggregateResult.count
        )



--
-- Query Project (Full)
--


type alias NodeProjectsFull =
    { projects : Maybe (List ProjectFull), open : Maybe Int, closed : Maybe Int }


projectsFullDecoder : NodeProjectsFull -> Maybe { projects : List ProjectFull, counts : ProjectsCount }
projectsFullDecoder d =
    Just
        { projects = withDefault [] d.projects
        , counts = { open = withDefault 0 d.open, closed = withDefault 0 d.closed }
        }


getProjects url nid pattern status msg =
    -- Fetch on the given node
    makeGQLQuery url
        (SelectionSet.map3 NodeProjectsFull
            (Query.getNode (nidFilter nid) (nodeProjectsFullPayload pattern status))
            (Query.aggregateProject
                (\args ->
                    { args
                        | filter =
                            Input.buildProjectFilter
                                (\c ->
                                    { c
                                        | parentnameid = Present { eq = Present nid, in_ = Absent }
                                        , name = fromMaybe <| Maybe.map (\x -> { anyoftext = Present x, alloftext = Absent }) pattern
                                        , status = Present { eq = Present ProjectStatus.Open, in_ = Absent }
                                    }
                                )
                                |> Present
                    }
                )
                (SelectionSet.map (withDefault 0) Fractal.Object.ProjectAggregateResult.count)
            )
            (Query.aggregateProject
                (\args ->
                    { args
                        | filter =
                            Input.buildProjectFilter
                                (\c ->
                                    { c
                                        | parentnameid = Present { eq = Present nid, in_ = Absent }
                                        , name = fromMaybe <| Maybe.map (\x -> { anyoftext = Present x, alloftext = Absent }) pattern
                                        , status = Present { eq = Present ProjectStatus.Closed, in_ = Absent }
                                    }
                                )
                                |> Present
                    }
                )
                (SelectionSet.map (withDefault 0) Fractal.Object.ProjectAggregateResult.count)
            )
        )
        (RemoteData.fromResult >> decodeResponse projectsFullDecoder >> msg)


nodeProjectsFullPayload : Maybe String -> ProjectStatus.ProjectStatus -> SelectionSet (List ProjectFull) Fractal.Object.Node
nodeProjectsFullPayload pattern status =
    SelectionSet.map (withDefault [])
        (Fractal.Object.Node.projects
            (\args ->
                { args
                    | filter =
                        Input.buildProjectFilter
                            (\c ->
                                { c
                                    | name = fromMaybe <| Maybe.map (\x -> { anyoftext = Present x, alloftext = Absent }) pattern
                                    , status = Present <| { eq = Present status, in_ = Absent }
                                }
                            )
                            |> Present
                    , order =
                        Input.buildProjectOrder (\b -> { b | desc = Present ProjectOrderable.CreatedAt })
                            |> Present
                }
            )
            projectFullPayload
        )


projectFullPayload : SelectionSet ProjectFull Fractal.Object.Project
projectFullPayload =
    SelectionSet.map4 ProjectFull
        (Fractal.Object.Project.id |> SelectionSet.map decodedId)
        (Fractal.Object.Project.updatedAt |> SelectionSet.map decodedTime)
        Fractal.Object.Project.name
        Fractal.Object.Project.description



--
-- Query Labels
--


type alias NodeLabels =
    { labels : Maybe (List Label) }


labelsDecoder : Maybe (List (Maybe NodeLabels)) -> Maybe (List Label)
labelsDecoder data =
    data
        |> Maybe.map
            (\d ->
                if List.length d == 0 then
                    Nothing

                else
                    d
                        |> List.filterMap identity
                        |> List.concatMap (\x -> withDefault [] x.labels)
                        |> LE.uniqueBy .id
                        |> Just
            )
        |> withDefault Nothing


queryLabels url nids msg =
    -- Fetch on the given group of nodes
    makeGQLQuery url
        (Query.queryNode
            (nidsFilter nids)
            nodeLabelsPayload
        )
        (RemoteData.fromResult >> decodeResponse labelsDecoder >> msg)


queryLabelsDown url nids msg =
    -- Fetch on all children nodes
    makeGQLQuery url
        (Query.queryNode
            (nidsDownFilter nids)
            nodeLabelsPayload
        )
        (RemoteData.fromResult >> decodeResponse labelsDecoder >> msg)


nodeLabelsPayload : SelectionSet NodeLabels Fractal.Object.Node
nodeLabelsPayload =
    SelectionSet.map NodeLabels
        (Fractal.Object.Node.labels
            (\args ->
                { args
                    | order =
                        Input.buildLabelOrder (\b -> { b | asc = Present LabelOrderable.Name })
                            |> Present
                }
            )
            labelPayload
        )


labelPayload : SelectionSet Label Fractal.Object.Label
labelPayload =
    SelectionSet.succeed Label
        |> with (Fractal.Object.Label.id |> SelectionSet.map decodedId)
        |> with Fractal.Object.Label.name
        |> with Fractal.Object.Label.color
        |> hardcoded []



--
-- Query Projects
--


type alias NodeProjects =
    { projects : Maybe (List Project) }


projectsDecoder : Maybe (List (Maybe NodeProjects)) -> Maybe (List Project)
projectsDecoder data =
    data
        |> Maybe.map
            (\d ->
                if List.length d == 0 then
                    Nothing

                else
                    d
                        |> List.filterMap identity
                        |> List.concatMap (\x -> withDefault [] x.projects)
                        |> LE.uniqueBy .id
                        |> Just
            )
        |> withDefault Nothing


queryProjects url nids msg =
    -- Fetch on the given group of nodes
    makeGQLQuery url
        (Query.queryNode
            (nidsFilter nids)
            nodeProjectsPayload
        )
        (RemoteData.fromResult >> decodeResponse projectsDecoder >> msg)


nodeProjectsPayload : SelectionSet NodeProjects Fractal.Object.Node
nodeProjectsPayload =
    SelectionSet.map NodeProjects
        (Fractal.Object.Node.projects
            (\args ->
                { args
                    | order =
                        Input.buildProjectOrder (\b -> { b | asc = Present ProjectOrderable.Name })
                            |> Present
                }
            )
            projectPayload
        )


projectPayload : SelectionSet Project Fractal.Object.Project
projectPayload =
    SelectionSet.succeed Project
        |> with (Fractal.Object.Project.id |> SelectionSet.map decodedId)
        |> with Fractal.Object.Project.name



--
-- Query journal
--


type alias JournalNode =
    { nameid : String, event_history : Maybe (List EventNotif) }


journalDecoder : Maybe JournalNode -> Maybe (List EventNotif)
journalDecoder data =
    data
        |> Maybe.map
            (\x ->
                withDefault [] x.event_history
            )


queryJournal url nameid msg =
    makeGQLQuery url
        (Query.getNode
            (nidFilter nameid)
            (SelectionSet.map2 JournalNode
                Fractal.Object.Node.nameid
                (Fractal.Object.Node.events_history identity tensionEventPayload)
            )
        )
        (RemoteData.fromResult >> decodeResponse journalDecoder >> msg)


tensionEventPayload : SelectionSet EventNotif Fractal.Object.Event
tensionEventPayload =
    SelectionSet.succeed EventNotif
        |> with (Fractal.Object.Event.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.Event.createdBy identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with Fractal.Object.Event.event_type
        |> with
            (Fractal.Object.Event.tension identity
                (SelectionSet.map4 (\a b c d -> { id = a, emitterid = b, receiver = c, title = d })
                    (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
                    Fractal.Object.Tension.emitterid
                    (Fractal.Object.Tension.receiver identity pNodePayload)
                    Fractal.Object.Tension.title
                )
            )
        |> with Fractal.Object.Event.new


contractEventPayload : SelectionSet ContractNotif Fractal.Object.Contract
contractEventPayload =
    SelectionSet.succeed ContractNotif
        |> with (Fractal.Object.Contract.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Contract.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.Contract.createdBy identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with Fractal.Object.Contract.contract_type
        |> with (Fractal.Object.Contract.event identity <| SelectionSet.map (\x -> { event_type = x }) Fractal.Object.EventFragment.event_type)
        |> with
            (Fractal.Object.Contract.tension identity
                (SelectionSet.map2 (\a b -> { id = a, receiver = b })
                    (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
                    (Fractal.Object.Tension.receiver identity pNodePayload)
                )
            )


notifEventPayload : SelectionSet NotifNotif Fractal.Object.Notif
notifEventPayload =
    SelectionSet.succeed NotifNotif
        |> with (Fractal.Object.Notif.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.Notif.createdBy identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with Fractal.Object.Notif.message
        |> with
            (Fractal.Object.Notif.tension_ identity
                (SelectionSet.map2 (\a b -> { id = a, receiver = b })
                    (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
                    (Fractal.Object.Tension.receiver identity pNodePayload)
                )
            )
        |> with (Fractal.Object.Notif.contract identity (SelectionSet.map IdPayload (SelectionSet.map decodedId Fractal.Object.Contract.id)))
        |> with Fractal.Object.Notif.link



--
-- Get an organisation info/stats
--


getOrgaInfo url username nameid msg =
    makeGQLQuery url
        (SelectionSet.map3
            (\x y versions ->
                Maybe.map (\oi -> { oi | n_projects = unwrap2 0 .count y, client_version = versions |> withDefault [] |> List.head |> withDefault Nothing |> withDefault "" }) x
            )
            (Query.getNode (nidFilter nameid) (orgaInfoPayload username))
            (Query.aggregateProject (\a -> { a | filter = Present <| Input.buildProjectFilter (\x -> { x | rootnameid = Present { eq = Present nameid, in_ = Absent }, status = Present { eq = Present ProjectStatus.Open, in_ = Absent } }) })
                (SelectionSet.map Count Fractal.Object.ProjectAggregateResult.count)
            )
            (Query.queryBuildInfo identity (SelectionSet.map identity Fractal.Object.BuildInfo.client_version))
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


orgaInfoPayload : String -> SelectionSet OrgaInfo Fractal.Object.Node
orgaInfoPayload username =
    SelectionSet.succeed OrgaInfo
        |> hardcoded 0
        |> with
            (SelectionSet.map (unwrap2 0 .count) <|
                Fractal.Object.Node.childrenAggregate (\a -> { a | filter = Present <| Input.buildNodeFilter (\x -> { x | role_type = Present { in_ = Present <| List.map Just <| activeMembershipRoleTypes, eq = Absent } }) }) <|
                    SelectionSet.map Count Fractal.Object.NodeAggregateResult.count
            )
        |> hardcoded 0
        |> with
            (SelectionSet.map (unwrap2 0 .count) <|
                Fractal.Object.Node.watchersAggregate identity <|
                    SelectionSet.map Count Fractal.Object.UserAggregateResult.count
            )
        |> with
            (SelectionSet.map (Maybe.map (\y -> List.length y > 0))
                (Fractal.Object.Node.watchers (\a -> { a | filter = Present <| Input.buildUserFilter (\x -> { x | username = Present { eq = Present username, in_ = Absent, regexp = Absent } }) })
                    (SelectionSet.map NameidPayload Fractal.Object.User.username)
                )
            )
        |> hardcoded ""
