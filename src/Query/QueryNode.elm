{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2026 Fractale Co

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
    , getOpenProjectsForPanel
    , getOrgaInfo
    , getProjects
    , getServerVersion
    , getRoles
    , projectColumnLitePayload
    , projectWithColumnsPayload
    , getProjectTemplateById
    , getProjectTemplates
    , getTensionTemplateById
    , getTensionTemplates
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
    , queryPinnedTensionsSub
    , queryOrgaTree
    , queryProjects
    , queryPublicOrga
    , queryRolesFull
    , roleFullPayload
    , searchUserFilter
    , projectTemplateFullPayload
    , projectTemplateLitePayload
    , tensionEventPayload
    , tensionTemplateFullPayload
    , tensionTemplateLitePayload
    , tidPayload
    , userPayload
    )

import Codecs exposing (decodeColumnsJson)
import Fractale.Graph exposing (maxPinnedTensions)
import Fractale.Codecs exposing (activeMembershipRoleTypes, membershipRoleTypes, nid2rootid)
import Dict exposing (Dict)
import Utils.Bool exposing (ternary)
import Utils.Maybe exposing (unwrap, unwrap2)
import Utils.String exposing (parseSearchPattern)
import Schema.Enum.ContractStatus as ContractStatus
import Schema.Enum.LabelOrderable as LabelOrderable
import Schema.Enum.NodeMode as NodeMode
import Schema.Enum.NodeOrderable as NodeOrderable
import Schema.Enum.NodeType as NodeType
import Schema.Enum.NodeVisibility as NodeVisibility
import Schema.Enum.ProjectColumnOrderable as ProjectColumnOrderable
import Schema.Enum.ProjectOrderable as ProjectOrderable
import Schema.Enum.ProjectStatus as ProjectStatus
import Schema.Enum.ProjectTemplateOrderable as ProjectTemplateOrderable
import Schema.Enum.RoleExtOrderable as RoleExtOrderable
import Schema.Enum.RoleType as RoleType
import Schema.Enum.TensionStatus as TensionStatus
import Schema.Enum.TensionTemplateOrderable as TensionTemplateOrderable
import Schema.InputObject as Input
import Schema.Object
import Schema.Object.Blob
import Schema.Object.BuildInfo
import Schema.Object.Contract
import Schema.Object.ContractAggregateResult
import Schema.Object.Event
import Schema.Object.EventFragment
import Schema.Object.Label
import Schema.Object.Mandate
import Schema.Object.Node
import Schema.Object.NodeAggregateResult
import Schema.Object.NodeFragment
import Schema.Object.Notif
import Schema.Object.Project
import Schema.Object.ProjectAggregateResult
import Schema.Object.ProjectColumn
import Schema.Object.ProjectTemplate
import Schema.Object.RoleExt
import Schema.Object.Tension
import Schema.Object.TensionAggregateResult
import Schema.Object.TensionTemplate
import Schema.Object.TensionTemplateAggregateResult
import Schema.Object.User
import Schema.Object.UserAggregateResult
import Schema.Query as Query
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

                        -- `not: {eq: true}` rather than `eq: false`: the flag is null on orgs never archived.
                        , not = Input.buildNodeFilter (\c -> { c | isRootArchived = Present True }) |> Present

                        --, not = Input.buildNodeFilter (\c -> { c | isPersonal = Present True }) |> Present
                    }
                )
                |> Present
        , order =
            Input.buildNodeOrder
                (\b -> { b | desc = Present NodeOrderable.CreatedAt })
                |> Present
    }


nodeOrgaExtPayload : SelectionSet NodeExt Schema.Object.Node
nodeOrgaExtPayload =
    SelectionSet.succeed NodeExt
        |> with (Schema.Object.Node.id |> SelectionSet.map decodedId)
        |> with (Schema.Object.Node.createdAt |> SelectionSet.map decodedTime)
        |> with Schema.Object.Node.name
        |> with Schema.Object.Node.nameid
        |> with (Schema.Object.Node.parent identity nodeIdPayload)
        |> with Schema.Object.Node.type_
        |> with Schema.Object.Node.role_type
        |> with (Schema.Object.Node.first_link identity <| SelectionSet.map Username Schema.Object.User.username)
        |> with Schema.Object.Node.visibility
        |> with Schema.Object.Node.about
        |> with Schema.Object.Node.isRootArchived
        |> with
            (SelectionSet.map (unwrap2 0 .count) <|
                Schema.Object.Node.childrenAggregate
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
                    SelectionSet.map Count Schema.Object.NodeAggregateResult.count
            )
        |> with
            (SelectionSet.map (unwrap2 0 .count) <|
                Schema.Object.Node.watchersAggregate identity <|
                    SelectionSet.map Count Schema.Object.UserAggregateResult.count
            )



--
-- Query Node Ext / Profile
--


queryNodeExt url nameids orderBy msg =
    makeGQLQuery url
        (Query.queryNode
            (nodeExtFilter nameids orderBy False)
            nodeOrgaExtPayload
        )
        (RemoteData.fromResult >> decodeResponse nodesDecoder >> msg)


queryOrgaNode url nameids msg =
    makeGQLQuery url
        (Query.queryNode
            (nodeExtFilter nameids NodeOrderable.UpdatedAt True)
            (SelectionSet.map2 OrgaNode
                Schema.Object.Node.name
                Schema.Object.Node.nameid
            )
        )
        (RemoteData.fromResult >> decodeResponse nodesDecoder >> msg)


nodeExtFilter : List String -> NodeOrderable.NodeOrderable -> Bool -> Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
nodeExtFilter nameids orderBy excludeArchived a =
    { a
        | filter =
            Input.buildNodeFilter
                (\b ->
                    { b
                        | nameid = { regexp = Absent, eq = Absent, in_ = List.map Just nameids |> Present } |> Present

                        -- `not` (instead of isRootArchived=false) to also catch nodes where the flag is unset
                        , not =
                            ternary excludeArchived
                                (Input.buildNodeFilter (\c -> { c | isRootArchived = Present True }) |> Present)
                                Absent
                    }
                )
                |> Present
        , order =
            Input.buildNodeOrder (\b -> { b | desc = Present orderBy })
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


nodeOrgaPayload : SelectionSet Node Schema.Object.Node
nodeOrgaPayload =
    SelectionSet.succeed Node
        |> with Schema.Object.Node.name
        |> with Schema.Object.Node.nameid
        |> with (Schema.Object.Node.parent identity nodeIdPayload)
        |> with Schema.Object.Node.type_
        |> with Schema.Object.Node.role_type
        |> with Schema.Object.Node.color
        |> with (Schema.Object.Node.first_link identity userPayload)
        |> with Schema.Object.Node.visibility
        |> with Schema.Object.Node.mode
        |> with (Schema.Object.Node.source identity blobIdPayload)
        |> with Schema.Object.Node.userCanJoin
        |> with Schema.Object.Node.isRootArchived
        |> with
            (SelectionSet.map (unwrap2 0 .count) <|
                Schema.Object.Node.tensions_inAggregate (\a -> { a | filter = Present <| Input.buildTensionFilter (\x -> { x | status = Present { eq = Present TensionStatus.Open, in_ = Absent } }) }) <|
                    SelectionSet.map Count Schema.Object.TensionAggregateResult.count
            )
        |> with
            (SelectionSet.map (\x -> withDefault Nothing x |> unwrap2 0 .count) <|
                Schema.Object.Node.source identity
                    (SelectionSet.map identity
                        (Schema.Object.Blob.tension identity
                            (SelectionSet.map identity <|
                                Schema.Object.Tension.contractsAggregate (\a -> { a | filter = Present <| Input.buildContractFilter (\x -> { x | status = Present { eq = Present ContractStatus.Open, in_ = Absent } }) }) <|
                                    SelectionSet.map Count Schema.Object.ContractAggregateResult.count
                            )
                        )
                    )
            )


{-| With blob id
-}
nodeOrgaPayload2 : SelectionSet Node Schema.Object.Node
nodeOrgaPayload2 =
    SelectionSet.succeed Node
        |> with Schema.Object.Node.name
        |> with Schema.Object.Node.nameid
        |> with (Schema.Object.Node.parent identity nodeIdPayload2)
        |> with Schema.Object.Node.type_
        |> with Schema.Object.Node.role_type
        |> with Schema.Object.Node.color
        |> with (Schema.Object.Node.first_link identity userPayload)
        |> with Schema.Object.Node.visibility
        |> with Schema.Object.Node.mode
        |> with (Schema.Object.Node.source identity blobIdPayload)
        |> with Schema.Object.Node.userCanJoin
        |> with Schema.Object.Node.isRootArchived
        |> hardcoded 0
        |> hardcoded 0


nodeIdPayload : SelectionSet NodeId Schema.Object.Node
nodeIdPayload =
    SelectionSet.succeed NodeId
        |> with Schema.Object.Node.nameid
        |> hardcoded Nothing


nodeIdPayload2 : SelectionSet NodeId Schema.Object.Node
nodeIdPayload2 =
    SelectionSet.succeed NodeId
        |> with Schema.Object.Node.nameid
        |> with (Schema.Object.Node.source identity blobIdPayload)


blobIdPayload : SelectionSet BlobId Schema.Object.Blob
blobIdPayload =
    SelectionSet.succeed BlobId
        |> with (Schema.Object.Blob.id |> SelectionSet.map decodedId)
        |> with (Schema.Object.Blob.tension identity tidPayload)


userPayload : SelectionSet User Schema.Object.User
userPayload =
    SelectionSet.map2 User
        Schema.Object.User.username
        Schema.Object.User.name


pNodePayload : SelectionSet PNode Schema.Object.Node
pNodePayload =
    SelectionSet.succeed PNode
        |> with Schema.Object.Node.name
        |> with Schema.Object.Node.nameid
        |> hardcoded Nothing


tidPayload : SelectionSet IdPayload Schema.Object.Tension
tidPayload =
    SelectionSet.map IdPayload
        (SelectionSet.map decodedId Schema.Object.Tension.id)


cidPayload : SelectionSet IdPayload Schema.Object.Contract
cidPayload =
    SelectionSet.map IdPayload
        (SelectionSet.map decodedId Schema.Object.Contract.id)



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


nodeDataPayload : SelectionSet NodeDataSource Schema.Object.Node
nodeDataPayload =
    SelectionSet.succeed NodeDataSource
        |> with
            (Schema.Object.Node.source identity
                (SelectionSet.map (\x -> { node = x })
                    (Schema.Object.Blob.node identity
                        (SelectionSet.map2 (\xx yy -> { about = xx, mandate = yy })
                            Schema.Object.NodeFragment.about
                            (Schema.Object.NodeFragment.mandate identity mandatePayload)
                        )
                    )
                )
            )


mandatePayload : SelectionSet Mandate Schema.Object.Mandate
mandatePayload =
    SelectionSet.succeed Mandate
        |> with Schema.Object.Mandate.purpose
        |> with Schema.Object.Mandate.responsabilities
        |> with Schema.Object.Mandate.domains
        |> with Schema.Object.Mandate.policies



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
    , isTemplateTensionOnly : Maybe Bool
    , isPinnedTensionfetchRecursively : Maybe Bool
    , isRootArchived : Maybe Bool
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
    , isTemplateTensionOnly : Maybe Bool
    , isPinnedTensionfetchRecursively : Maybe Bool
    , isRootArchived : Maybe Bool
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
                            { root = RNode p.name p.nameid p.userCanJoin p.mode p.isTemplateTensionOnly p.isPinnedTensionfetchRecursively p.isRootArchived |> Just
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
                        { root = RNode n.name n.nameid n.userCanJoin n.mode n.isTemplateTensionOnly n.isPinnedTensionfetchRecursively n.isRootArchived |> Just
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


lgPayload : Bool -> SelectionSet LocalNode Schema.Object.Node
lgPayload isInit =
    SelectionSet.succeed LocalNode
        |> with Schema.Object.Node.name
        |> with Schema.Object.Node.nameid
        |> with Schema.Object.Node.type_
        |> with Schema.Object.Node.visibility
        |> with Schema.Object.Node.mode
        |> with Schema.Object.Node.userCanJoin
        |> with Schema.Object.Node.isTemplateTensionOnly
        |> with Schema.Object.Node.isPinnedTensionfetchRecursively
        |> with Schema.Object.Node.isRootArchived
        |> with (Schema.Object.Node.source identity blobIdPayload)
        |> with (Schema.Object.Node.parent identity lg2Payload)
        |> (\x ->
                if isInit then
                    x
                        |> with (Schema.Object.Node.children lgChildrenFilter emiterOrReceiverPayload)
                        |> with (Schema.Object.Node.pinned identity pinPayload |> SelectionSet.map (\y -> ternary (y == Just []) Nothing y))

                else
                    x
                        |> hardcoded Nothing
                        |> hardcoded Nothing
           )


lg2Payload : SelectionSet LocalRootNode Schema.Object.Node
lg2Payload =
    SelectionSet.succeed LocalRootNode
        |> with Schema.Object.Node.isRoot
        |> with Schema.Object.Node.name
        |> with Schema.Object.Node.nameid
        |> with Schema.Object.Node.userCanJoin
        |> with Schema.Object.Node.mode
        |> with Schema.Object.Node.isTemplateTensionOnly
        |> with Schema.Object.Node.isPinnedTensionfetchRecursively
        |> with Schema.Object.Node.isRootArchived
        |> with (Schema.Object.Node.source identity blobIdPayload)


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


emiterOrReceiverPayload : SelectionSet EmitterOrReceiver Schema.Object.Node
emiterOrReceiverPayload =
    SelectionSet.succeed EmitterOrReceiver
        |> with Schema.Object.Node.name
        |> with Schema.Object.Node.nameid
        |> with Schema.Object.Node.role_type
        |> with Schema.Object.Node.color


emiterOrReceiverWithPinPayload : String -> SelectionSet (NodeWithPin EmitterOrReceiver) Schema.Object.Node
emiterOrReceiverWithPinPayload tid =
    SelectionSet.succeed (\a b c d e -> { name = a, nameid = b, role_type = c, color = d, pinned = e })
        |> with Schema.Object.Node.name
        |> with Schema.Object.Node.nameid
        |> with Schema.Object.Node.role_type
        |> with Schema.Object.Node.color
        |> with
            (Schema.Object.Node.pinned
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


pinPayload : SelectionSet PinTension Schema.Object.Tension
pinPayload =
    SelectionSet.succeed PinTension
        |> with (Schema.Object.Tension.id |> SelectionSet.map decodedId)
        |> with Schema.Object.Tension.title
        |> with (Schema.Object.Tension.createdAt |> SelectionSet.map decodedTime)
        |> with (Schema.Object.Tension.createdBy identity <| SelectionSet.map Username Schema.Object.User.username)
        |> with Schema.Object.Tension.type_
        |> with Schema.Object.Tension.status
        |> with (Schema.Object.Tension.labels identity labelPayload)



--
-- Query Pinned Tensions in sub-circles (recursive)
--
-- @TODO: this is the only direct-Dgraph recursive sub-fetch in the codebase
-- (besides queryNodesSub, which legitimately needs the full tree for graphpack).
-- Every other recursive *Sub fetch — fetchChildren, fetchMembersSub, fetchLabelsSub,
-- fetchRolesSub, fetchTensionTemplatesSub, fetchProjectsSub — goes through a Go
-- backend `/sub` endpoint where bounds, ordering, and authz are enforced uniformly.
-- The bounds below (subNodesCap, maxPinnedTensions per node, deterministic order) are
-- a tactical safeguard. When this area is touched again, migrate to a REST endpoint
-- (e.g. `/tensions/pinned/sub`) for consistency with the rest of the *Sub family.
--


{-| Hard cap on the number of descendant circles inspected per recursive pinned
fetch. Combined with maxPinnedTensions per node, the worst-case response is
bounded at subNodesCap * maxPinnedTensions tension entries.
-}
subNodesCap : Int
subNodesCap =
    50


queryPinnedTensionsSub url nameid msg =
    makeGQLQuery url
        (Query.queryNode
            (subPinnedFilter nameid)
            nodeWithPinsPayload
        )
        (RemoteData.fromResult >> decodeResponse (subPinnedDecoder nameid) >> msg)


subPinnedFilter : String -> Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
subPinnedFilter nameid a =
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
                        , not =
                            Input.buildNodeFilter (\sd -> { sd | isArchived = Present True, or = matchAnyRoleType [ RoleType.Member, RoleType.Guest, RoleType.Pending, RoleType.Retired ] })
                                |> Present
                    }
                )
                |> Present
        , first = Present subNodesCap
        , order =
            Input.buildNodeOrder
                (\b -> { b | desc = Present NodeOrderable.CreatedAt })
                |> Present
    }


{-| Cap each descendant's pinned list at maxPinnedTensions so the response
stays bounded; the merged result is then capped again client-side.
-}
nodeWithPinsPayload : SelectionSet NodeWithPins Schema.Object.Node
nodeWithPinsPayload =
    SelectionSet.succeed NodeWithPins
        |> with Schema.Object.Node.name
        |> with Schema.Object.Node.nameid
        |> with Schema.Object.Node.role_type
        |> with Schema.Object.Node.color
        |> with (Schema.Object.Node.pinned (\a -> { a | first = Present maxPinnedTensions }) pinPayload)


subPinnedDecoder : String -> Maybe (List (Maybe NodeWithPins)) -> Maybe (List NodeWithPins)
subPinnedDecoder focusNameid data =
    data
        |> Maybe.map
            (List.filterMap identity
                >> List.filter (\n -> n.nameid /= focusNameid)
                >> List.filterMap
                    (\n ->
                        case n.pinned of
                            Just (_ :: _) ->
                                Just n

                            _ ->
                                Nothing
                    )
            )



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


nodeRightsPayload : SelectionSet NodeRights Schema.Object.Node
nodeRightsPayload =
    SelectionSet.succeed NodeRights
        |> with Schema.Object.Node.visibility
        |> with Schema.Object.Node.userCanJoin
        |> with Schema.Object.Node.guestCanCreateTension
        |> with Schema.Object.Node.isTemplateTensionOnly
        |> with Schema.Object.Node.isPinnedTensionfetchRecursively



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


membersPayload : SelectionSet NodeMembers Schema.Object.Node
membersPayload =
    SelectionSet.succeed NodeMembers
        |> with (Schema.Object.Node.first_link identity userPayload)



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


membersLocalPayload : Maybe String -> SelectionSet LocalMemberNode Schema.Object.Node
membersLocalPayload pattern =
    SelectionSet.succeed LocalMemberNode
        |> with (Schema.Object.Node.createdAt |> SelectionSet.map decodedTime)
        |> with Schema.Object.Node.name
        |> with Schema.Object.Node.nameid
        |> with Schema.Object.Node.role_type
        |> with Schema.Object.Node.color
        |> with (Schema.Object.Node.first_link identity userPayload)
        |> hardcoded Nothing
        |> with
            (Schema.Object.Node.children mbChildrenFilter
                (SelectionSet.succeed MemberNode
                    |> with (Schema.Object.Node.createdAt |> SelectionSet.map decodedTime)
                    |> with Schema.Object.Node.name
                    |> with Schema.Object.Node.nameid
                    |> with Schema.Object.Node.role_type
                    |> with Schema.Object.Node.color
                    |> with (Schema.Object.Node.first_link (searchUserFilter pattern) userPayload)
                    |> hardcoded Nothing
                )
            )



--searchUserFilter : Maybe String -> Schema.Object.Node.FirstLinkOptionalArguments -> Schema.Object.Node.FirstLinkOptionalArguments


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


nodeRolesFullPayload : SelectionSet NodeRolesFull Schema.Object.Node
nodeRolesFullPayload =
    SelectionSet.map NodeRolesFull
        (Schema.Object.Node.roles
            (\args ->
                { args
                    | order =
                        Input.buildRoleExtOrder (\b -> { b | asc = Present RoleExtOrderable.Name })
                            |> Present
                }
            )
            roleFullPayload
        )


roleFullPayload : SelectionSet RoleExtFull Schema.Object.RoleExt
roleFullPayload =
    SelectionSet.map8 RoleExtFull
        (Schema.Object.RoleExt.id |> SelectionSet.map decodedId)
        Schema.Object.RoleExt.name
        Schema.Object.RoleExt.color
        Schema.Object.RoleExt.role_type
        Schema.Object.RoleExt.about
        (Schema.Object.RoleExt.mandate identity mandatePayload)
        (SelectionSet.map (unwrap Nothing .count) <|
            Schema.Object.RoleExt.nodesAggregate identity <|
                SelectionSet.map Count Schema.Object.NodeAggregateResult.count
        )
        (SelectionSet.map (unwrap Nothing .count) <|
            Schema.Object.RoleExt.rolesAggregate identity <|
                SelectionSet.map Count Schema.Object.NodeAggregateResult.count
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


nodeLabelsFullPayload : SelectionSet NodeLabelsFull Schema.Object.Node
nodeLabelsFullPayload =
    SelectionSet.map NodeLabelsFull
        (Schema.Object.Node.labels
            (\args ->
                { args
                    | order =
                        Input.buildLabelOrder (\b -> { b | asc = Present LabelOrderable.Name })
                            |> Present
                }
            )
            labelFullPayload
        )


labelFullPayload : SelectionSet LabelFull Schema.Object.Label
labelFullPayload =
    SelectionSet.map5 LabelFull
        (Schema.Object.Label.id |> SelectionSet.map decodedId)
        Schema.Object.Label.name
        Schema.Object.Label.color
        Schema.Object.Label.description
        (SelectionSet.map (unwrap Nothing .count) <|
            Schema.Object.Label.nodesAggregate identity <|
                SelectionSet.map Count Schema.Object.NodeAggregateResult.count
        )



--
-- Query Tension Templates (Full)
--


type alias NodeTensionTemplatesFull =
    { tension_templates : Maybe (List TensionTemplateFull) }


tensionTemplatesFullDecoder : Maybe NodeTensionTemplatesFull -> Maybe (List TensionTemplateFull)
tensionTemplatesFullDecoder data =
    data
        |> Maybe.map (\d -> withDefault [] d.tension_templates)


getTensionTemplates url nid msg =
    -- Fetch on the given node
    makeGQLQuery url
        (Query.getNode
            (nidFilter nid)
            nodeTensionTemplatesFullPayload
        )
        (RemoteData.fromResult >> decodeResponse tensionTemplatesFullDecoder >> msg)


getTensionTemplateById url tid msg =
    makeGQLQuery url
        (Query.getTensionTemplate
            { id = encodeId tid }
            tensionTemplateFullPayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


nodeTensionTemplatesFullPayload : SelectionSet NodeTensionTemplatesFull Schema.Object.Node
nodeTensionTemplatesFullPayload =
    SelectionSet.map NodeTensionTemplatesFull
        (Schema.Object.Node.tension_templates
            (\args ->
                { args
                    | order =
                        Input.buildTensionTemplateOrder (\b -> { b | asc = Present TensionTemplateOrderable.Name })
                            |> Present
                }
            )
            tensionTemplateFullPayload
        )


tensionTemplateFullPayload : SelectionSet TensionTemplateFull Schema.Object.TensionTemplate
tensionTemplateFullPayload =
    SelectionSet.succeed TensionTemplateFull
        |> with (Schema.Object.TensionTemplate.id |> SelectionSet.map decodedId)
        |> with Schema.Object.TensionTemplate.name
        |> with Schema.Object.TensionTemplate.description
        |> with Schema.Object.TensionTemplate.title
        |> with Schema.Object.TensionTemplate.comment
        |> with Schema.Object.TensionTemplate.type_
        |> with Schema.Object.TensionTemplate.is_recursive
        |> with (Schema.Object.TensionTemplate.labels identity labelPayload)
        |> with (Schema.Object.TensionTemplate.assignees identity userPayload)
        |> with
            (SelectionSet.map (unwrap Nothing .count) <|
                Schema.Object.TensionTemplate.nodesAggregate identity <|
                    SelectionSet.map Count Schema.Object.NodeAggregateResult.count
            )


tensionTemplateLitePayload : SelectionSet TensionTemplateLite Schema.Object.TensionTemplate
tensionTemplateLitePayload =
    SelectionSet.map5 TensionTemplateLite
        (Schema.Object.TensionTemplate.id |> SelectionSet.map decodedId)
        Schema.Object.TensionTemplate.name
        Schema.Object.TensionTemplate.description
        Schema.Object.TensionTemplate.is_recursive
        (SelectionSet.map (withDefault []) <| Schema.Object.TensionTemplate.nodes identity (SelectionSet.map NameidPayload Schema.Object.Node.nameid))


type alias NodeProjectTemplatesFull =
    { project_templates : Maybe (List ProjectTemplateFull) }


projectTemplatesFullDecoder : Maybe NodeProjectTemplatesFull -> Maybe (List ProjectTemplateFull)
projectTemplatesFullDecoder data =
    data
        |> Maybe.map (\d -> withDefault [] d.project_templates)


getProjectTemplates url nid msg =
    -- Fetch on the given node
    makeGQLQuery url
        (Query.getNode
            (nidFilter nid)
            nodeProjectTemplatesFullPayload
        )
        (RemoteData.fromResult >> decodeResponse projectTemplatesFullDecoder >> msg)


getProjectTemplateById url tid msg =
    makeGQLQuery url
        (Query.getProjectTemplate
            { id = encodeId tid }
            projectTemplateFullPayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


nodeProjectTemplatesFullPayload : SelectionSet NodeProjectTemplatesFull Schema.Object.Node
nodeProjectTemplatesFullPayload =
    SelectionSet.map NodeProjectTemplatesFull
        (Schema.Object.Node.project_templates
            (\args ->
                { args
                    | order =
                        Input.buildProjectTemplateOrder (\b -> { b | asc = Present ProjectTemplateOrderable.Name })
                            |> Present
                }
            )
            projectTemplateFullPayload
        )


projectTemplateFullPayload : SelectionSet ProjectTemplateFull Schema.Object.ProjectTemplate
projectTemplateFullPayload =
    SelectionSet.succeed ProjectTemplateFull
        |> with (Schema.Object.ProjectTemplate.id |> SelectionSet.map decodedId)
        |> with Schema.Object.ProjectTemplate.name
        |> with Schema.Object.ProjectTemplate.description
        |> with Schema.Object.ProjectTemplate.is_recursive
        |> with (Schema.Object.ProjectTemplate.columns_json |> SelectionSet.map decodeColumnsJson)
        |> with
            (SelectionSet.map (unwrap Nothing .count) <|
                Schema.Object.ProjectTemplate.nodesAggregate identity <|
                    SelectionSet.map Count Schema.Object.NodeAggregateResult.count
            )


projectTemplateLitePayload : SelectionSet ProjectTemplateLite Schema.Object.ProjectTemplate
projectTemplateLitePayload =
    SelectionSet.map5 ProjectTemplateLite
        (Schema.Object.ProjectTemplate.id |> SelectionSet.map decodedId)
        Schema.Object.ProjectTemplate.name
        Schema.Object.ProjectTemplate.description
        Schema.Object.ProjectTemplate.is_recursive
        (SelectionSet.map (withDefault []) <| Schema.Object.ProjectTemplate.nodes identity (SelectionSet.map NameidPayload Schema.Object.Node.nameid))



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


{-| Fulltext filter for a search pattern: anyoftext on the unquoted part, alloftext
on the quoted spans (joined: alloftext of joined spans == AND of per-span alloftext).
-}
searchNameFilter : Maybe String -> OptionalArgument { alloftext : OptionalArgument String, anyoftext : OptionalArgument String }
searchNameFilter pattern =
    case Maybe.map parseSearchPattern pattern of
        Just ( unquoted, span :: spans ) ->
            Present
                { anyoftext = fromMaybe unquoted
                , alloftext = Present (String.join " " (span :: spans))
                }

        Just ( Just unquoted, [] ) ->
            Present { anyoftext = Present unquoted, alloftext = Absent }

        _ ->
            Absent


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
                                        , name = searchNameFilter pattern
                                        , status = Present { eq = Present ProjectStatus.Open, in_ = Absent }
                                    }
                                )
                                |> Present
                    }
                )
                (SelectionSet.map (withDefault 0) Schema.Object.ProjectAggregateResult.count)
            )
            (Query.aggregateProject
                (\args ->
                    { args
                        | filter =
                            Input.buildProjectFilter
                                (\c ->
                                    { c
                                        | parentnameid = Present { eq = Present nid, in_ = Absent }
                                        , name = searchNameFilter pattern
                                        , status = Present { eq = Present ProjectStatus.Closed, in_ = Absent }
                                    }
                                )
                                |> Present
                    }
                )
                (SelectionSet.map (withDefault 0) Schema.Object.ProjectAggregateResult.count)
            )
        )
        (RemoteData.fromResult >> decodeResponse projectsFullDecoder >> msg)


nodeProjectsFullPayload : Maybe String -> ProjectStatus.ProjectStatus -> SelectionSet (List ProjectFull) Schema.Object.Node
nodeProjectsFullPayload pattern status =
    SelectionSet.map (withDefault [])
        (Schema.Object.Node.projects
            (\args ->
                { args
                    | filter =
                        Input.buildProjectFilter
                            (\c ->
                                { c
                                    | name = searchNameFilter pattern
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


projectFullPayload : SelectionSet ProjectFull Schema.Object.Project
projectFullPayload =
    SelectionSet.succeed ProjectFull
        |> with (Schema.Object.Project.id |> SelectionSet.map decodedId)
        |> with (Schema.Object.Project.updatedAt |> SelectionSet.map decodedTime)
        |> with Schema.Object.Project.name
        |> with Schema.Object.Project.description
        |> with (Schema.Object.Project.parentnameid |> SelectionSet.map Just)
        |> with (Schema.Object.Project.nodes identity emiterOrReceiverPayload |> SelectionSet.map (withDefault []))
        |> with (Schema.Object.Project.collaborators identity (SelectionSet.map Username Schema.Object.User.username) |> SelectionSet.map (withDefault []))
        |> with Schema.Object.Project.peerCanEditProject
        |> with Schema.Object.Project.guestCanEditProject



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


nodeLabelsPayload : SelectionSet NodeLabels Schema.Object.Node
nodeLabelsPayload =
    SelectionSet.map NodeLabels
        (Schema.Object.Node.labels
            (\args ->
                { args
                    | order =
                        Input.buildLabelOrder (\b -> { b | asc = Present LabelOrderable.Name })
                            |> Present
                }
            )
            labelWithNodesPayload
        )


labelPayload : SelectionSet Label Schema.Object.Label
labelPayload =
    SelectionSet.succeed Label
        |> with (Schema.Object.Label.id |> SelectionSet.map decodedId)
        |> with Schema.Object.Label.name
        |> with Schema.Object.Label.color
        |> hardcoded []


labelWithNodesPayload : SelectionSet Label Schema.Object.Label
labelWithNodesPayload =
    SelectionSet.succeed Label
        |> with (Schema.Object.Label.id |> SelectionSet.map decodedId)
        |> with Schema.Object.Label.name
        |> with Schema.Object.Label.color
        |> with
            (Schema.Object.Label.nodes identity (SelectionSet.map NameidPayload Schema.Object.Node.nameid)
                |> SelectionSet.map (withDefault [])
            )



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


nodeProjectsPayload : SelectionSet NodeProjects Schema.Object.Node
nodeProjectsPayload =
    SelectionSet.map NodeProjects
        (Schema.Object.Node.projects
            (\args ->
                { args
                    | order =
                        Input.buildProjectOrder (\b -> { b | asc = Present ProjectOrderable.Name })
                            |> Present
                }
            )
            projectPayload
        )


projectPayload : SelectionSet Project Schema.Object.Project
projectPayload =
    SelectionSet.succeed Project
        |> with (Schema.Object.Project.id |> SelectionSet.map decodedId)
        |> with Schema.Object.Project.name



--
-- Query Open Projects (with columns) for the side-panel picker
--


type alias NodeOpenProjects =
    { projects : Maybe (List ProjectWithColumns) }


openProjectsDecoder : Maybe (List (Maybe NodeOpenProjects)) -> Maybe (List ProjectWithColumns)
openProjectsDecoder data =
    data
        |> Maybe.map
            (\d ->
                d
                    |> List.filterMap identity
                    |> List.concatMap (\x -> withDefault [] x.projects)
                    |> LE.uniqueBy .id
            )


getOpenProjectsForPanel url nameids pattern_m msg =
    -- Fetch open projects on the given group of nodes (no recursion)
    makeGQLQuery url
        (Query.queryNode
            (nidsFilter nameids)
            (nodeOpenProjectsPayload pattern_m)
        )
        (RemoteData.fromResult >> decodeResponse openProjectsDecoder >> msg)


nodeOpenProjectsPayload : Maybe String -> SelectionSet NodeOpenProjects Schema.Object.Node
nodeOpenProjectsPayload pattern_m =
    SelectionSet.map NodeOpenProjects
        (Schema.Object.Node.projects
            (\args ->
                { args
                    | filter =
                        Input.buildProjectFilter
                            (\f ->
                                { f
                                    | status = Present { eq = Present ProjectStatus.Open, in_ = Absent }
                                    , name = searchNameFilter pattern_m
                                }
                            )
                            |> Present
                    , order =
                        Input.buildProjectOrder (\b -> { b | asc = Present ProjectOrderable.Name })
                            |> Present
                }
            )
            projectWithColumnsPayload
        )


projectWithColumnsPayload : SelectionSet ProjectWithColumns Schema.Object.Project
projectWithColumnsPayload =
    SelectionSet.succeed ProjectWithColumns
        |> with (Schema.Object.Project.id |> SelectionSet.map decodedId)
        |> with Schema.Object.Project.name
        |> with
            (Schema.Object.Project.columns
                (\args ->
                    { args
                        | order =
                            Input.buildProjectColumnOrder (\b -> { b | asc = Present ProjectColumnOrderable.Pos })
                                |> Present
                    }
                )
                projectColumnLitePayload
                |> SelectionSet.map (withDefault [])
            )
        |> with
            (Schema.Object.Project.nodes identity (SelectionSet.map NameidPayload Schema.Object.Node.nameid)
                |> SelectionSet.map (withDefault [])
            )


projectColumnLitePayload : SelectionSet ProjectColumnLite Schema.Object.ProjectColumn
projectColumnLitePayload =
    SelectionSet.succeed ProjectColumnLite
        |> with (Schema.Object.ProjectColumn.id |> SelectionSet.map decodedId)
        |> with Schema.Object.ProjectColumn.name
        |> with Schema.Object.ProjectColumn.color
        |> with Schema.Object.ProjectColumn.pos
        |> with Schema.Object.ProjectColumn.col_type



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


queryJournal url nameid textQuery msg =
    makeGQLQuery url
        (Query.getNode
            (nidFilter nameid)
            (SelectionSet.map2 JournalNode
                Schema.Object.Node.nameid
                (Schema.Object.Node.events_history
                    (\args -> { args | query = fromMaybe textQuery })
                    tensionEventPayload
                )
            )
        )
        (RemoteData.fromResult >> decodeResponse journalDecoder >> msg)


tensionEventPayload : SelectionSet EventNotif Schema.Object.Event
tensionEventPayload =
    SelectionSet.succeed EventNotif
        |> with (Schema.Object.Event.createdAt |> SelectionSet.map decodedTime)
        |> with (Schema.Object.Event.createdBy identity <| SelectionSet.map Username Schema.Object.User.username)
        |> with Schema.Object.Event.event_type
        |> with
            (Schema.Object.Event.tension identity
                (SelectionSet.map4 (\a b c d -> { id = a, emitterid = b, receiver = c, title = d })
                    (Schema.Object.Tension.id |> SelectionSet.map decodedId)
                    Schema.Object.Tension.emitterid
                    (Schema.Object.Tension.receiver identity pNodePayload)
                    Schema.Object.Tension.title
                )
            )
        |> with Schema.Object.Event.new


contractEventPayload : SelectionSet ContractNotif Schema.Object.Contract
contractEventPayload =
    SelectionSet.succeed ContractNotif
        |> with (Schema.Object.Contract.id |> SelectionSet.map decodedId)
        |> with (Schema.Object.Contract.createdAt |> SelectionSet.map decodedTime)
        |> with (Schema.Object.Contract.createdBy identity <| SelectionSet.map Username Schema.Object.User.username)
        |> with Schema.Object.Contract.contract_type
        |> with (Schema.Object.Contract.event identity <| SelectionSet.map (\x -> { event_type = x }) Schema.Object.EventFragment.event_type)
        |> with
            (Schema.Object.Contract.tension identity
                (SelectionSet.map2 (\a b -> { id = a, receiver = b })
                    (Schema.Object.Tension.id |> SelectionSet.map decodedId)
                    (Schema.Object.Tension.receiver identity pNodePayload)
                )
            )


notifEventPayload : SelectionSet NotifNotif Schema.Object.Notif
notifEventPayload =
    SelectionSet.succeed NotifNotif
        |> with (Schema.Object.Notif.createdAt |> SelectionSet.map decodedTime)
        |> with (Schema.Object.Notif.createdBy identity <| SelectionSet.map Username Schema.Object.User.username)
        |> with Schema.Object.Notif.message
        |> with
            (Schema.Object.Notif.tension_ identity
                (SelectionSet.map2 (\a b -> { id = a, receiver = b })
                    (Schema.Object.Tension.id |> SelectionSet.map decodedId)
                    (Schema.Object.Tension.receiver identity pNodePayload)
                )
            )
        |> with (Schema.Object.Notif.contract identity (SelectionSet.map IdPayload (SelectionSet.map decodedId Schema.Object.Contract.id)))
        |> with Schema.Object.Notif.link



--
-- Get an organisation info/stats
--


getOrgaInfo url username nameid msg =
    makeGQLQuery url
        (SelectionSet.map2
            (\x y ->
                Maybe.map (\oi -> { oi | n_projects = unwrap2 0 .count y }) x
            )
            (Query.getNode (nidFilter nameid) (orgaInfoPayload username))
            (Query.aggregateProject (\a -> { a | filter = Present <| Input.buildProjectFilter (\x -> { x | rootnameid = Present { eq = Present nameid, in_ = Absent }, status = Present { eq = Present ProjectStatus.Open, in_ = Absent } }) })
                (SelectionSet.map Count Schema.Object.ProjectAggregateResult.count)
            )
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


-- Standalone so the version banner fires on non-org pages.


getServerVersion url msg =
    makeGQLQuery url
        (Query.queryBuildInfo identity
            (SelectionSet.map2 ServerBuild
                Schema.Object.BuildInfo.client_version
                (SelectionSet.map parseReloadMode Schema.Object.BuildInfo.reload_mode)
            )
            |> SelectionSet.map (withDefault [] >> List.filterMap identity >> List.head >> Just)
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


orgaInfoPayload : String -> SelectionSet OrgaInfo Schema.Object.Node
orgaInfoPayload username =
    SelectionSet.succeed OrgaInfo
        |> hardcoded 0
        |> with
            (SelectionSet.map (unwrap2 0 .count) <|
                Schema.Object.Node.childrenAggregate (\a -> { a | filter = Present <| Input.buildNodeFilter (\x -> { x | role_type = Present { in_ = Present <| List.map Just <| activeMembershipRoleTypes, eq = Absent } }) }) <|
                    SelectionSet.map Count Schema.Object.NodeAggregateResult.count
            )
        |> hardcoded 0
        |> with
            (SelectionSet.map (unwrap2 0 .count) <|
                Schema.Object.Node.watchersAggregate identity <|
                    SelectionSet.map Count Schema.Object.UserAggregateResult.count
            )
        |> with
            (SelectionSet.map (Maybe.map (\y -> List.length y > 0))
                (Schema.Object.Node.watchers (\a -> { a | filter = Present <| Input.buildUserFilter (\x -> { x | username = Present { eq = Present username, in_ = Absent, regexp = Absent } }) })
                    (SelectionSet.map NameidPayload Schema.Object.User.username)
                )
            )
        |> with Schema.Object.Node.lexicon
