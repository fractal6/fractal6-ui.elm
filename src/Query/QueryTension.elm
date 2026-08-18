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


module Query.QueryTension exposing
    ( blobPayload
    , commentPayload
    , draftNodeTypePayload
    , eventPayload
    , getTensionBlobs
    , getTensionComments
    , getTensionHead
    , getTensionPanel
    , getTensionProjects
    , governedNodePayload
    , nodeFragmentLightPayload
    , nodeFragmentPayload
    , queryAllTension
    , queryAssignedTensions
    , queryCircleTension
    , queryExtTension
    , queryIntTension
    , orgNodesFilter
    , orgTensionPayload
    , queryOrgTensions
    , queryPinnedTensions
    , tensionPayload
    )

import Dict exposing (Dict)
import Fractale.Codecs exposing (membershipRoleTypes, nid2rootid)
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import List.Extra exposing (uniqueBy)
import Maybe exposing (withDefault)
import ModelSchema exposing (Blob, Comment, CommentFile, Count, EmitterOrReceiver, Event, GovernedNode, IdPayload, Label, MentionedTension, NodeFragment, NodeFragmentLight, PinTension, ProjectCardLite, ProjectColumnLite, ProjectWithColumns, Reaction, Tension, TensionBlobs, TensionComments, TensionHead, TensionPanel, TensionProject, User, UserCtx, Username, decodeResponse, decodedId, decodedTime, encodeId)
import Query.QueryNode exposing (emiterOrReceiverPayload, emiterOrReceiverWithPinPayload, labelPayload, mandatePayload, nidFilter, pinPayload, projectColumnLitePayload, projectWithColumnsPayload, userPayload)
import RemoteData
import Schema.Enum.BlobOrderable as BlobOrderable
import Schema.Enum.ContractStatus as ContractStatus
import Schema.Enum.NodeHasFilter as NodeHasFilter
import Schema.Enum.NodeOrderable as NodeOrderable
import Schema.Enum.NodeType as NodeType
import Schema.Enum.TensionEvent as TensionEvent
import Schema.Enum.TensionOrderable as TensionOrderable
import Schema.Enum.TensionStatus as TensionStatus
import Schema.Enum.TensionType as TensionType
import Schema.InputObject as Input
import Schema.Object
import Schema.Object.Blob
import Schema.Object.Comment
import Schema.Object.CommentAggregateResult
import Schema.Object.Contract
import Schema.Object.ContractAggregateResult
import Schema.Object.Event
import Schema.Object.File
import Schema.Object.Node
import Schema.Object.NodeFragment
import Schema.Object.ProjectCard
import Schema.Object.ProjectColumn
import Schema.Object.Reaction
import Schema.Object.Tension
import Schema.Object.User
import Schema.Query as Query
import Schema.Union
import Schema.Union.CardKind
import String.Extra as SE
import Utils.Bool exposing (ternary)
import Utils.Maybe exposing (unwrap, unwrap2)



--
-- Get one tension Head/Comments/Blobs/History
--


nCommentPerTension : Int
nCommentPerTension =
    250


nBlobPerTension : Int
nBlobPerTension =
    50


getTensionHead url uctx tensionid msg =
    makeGQLQuery url
        (Query.getTension { id = encodeId tensionid }
            (tensionHeadPayload tensionid uctx)
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


getTensionPanel url uctx tensionid msg =
    makeGQLQuery url
        (Query.getTension { id = encodeId tensionid }
            (tensionPanelPayload uctx)
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


getTensionComments url tensionid msg =
    makeGQLQuery url
        (Query.getTension { id = encodeId tensionid }
            tensionCommentsPayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


getTensionBlobs url tensionid msg =
    makeGQLQuery url
        (Query.getTension { id = encodeId tensionid }
            tensionBlobsPayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


getTensionProjects url tensionid msg =
    makeGQLQuery url
        (Query.getTension { id = encodeId tensionid }
            (Schema.Object.Tension.project_statuses identity (tensionProjectStatusPayload tensionid)
                |> SelectionSet.map (withDefault [] >> List.filterMap identity)
            )
        )
        (RemoteData.fromResult >> decodeResponse (Maybe.withDefault [] >> Just) >> msg)


tensionProjectStatusPayload : String -> SelectionSet (Maybe TensionProject) Schema.Object.ProjectColumn
tensionProjectStatusPayload tid =
    SelectionSet.succeed
        (\colId colName colColor colPos colType project cards ->
            let
                column =
                    { id = colId, name = colName, color = colColor, pos = colPos, col_type = colType }

                matchedCard =
                    cards
                        |> List.filter (\c -> c.tensionId == Just tid)
                        |> List.head
            in
            Maybe.map
                (\c -> { card = { id = c.id, pos = c.pos }, column = column, project = project })
                matchedCard
        )
        |> with (Schema.Object.ProjectColumn.id |> SelectionSet.map decodedId)
        |> with Schema.Object.ProjectColumn.name
        |> with Schema.Object.ProjectColumn.color
        |> with Schema.Object.ProjectColumn.pos
        |> with Schema.Object.ProjectColumn.col_type
        |> with (Schema.Object.ProjectColumn.project identity projectWithColumnsPayload)
        |> with
            (Schema.Object.ProjectColumn.cards identity projectCardWithTensionIdPayload
                |> SelectionSet.map (withDefault [])
            )


projectCardWithTensionIdPayload : SelectionSet { id : String, pos : Int, tensionId : Maybe String } Schema.Object.ProjectCard
projectCardWithTensionIdPayload =
    SelectionSet.succeed (\a b c -> { id = a, pos = b, tensionId = c })
        |> with (Schema.Object.ProjectCard.id |> SelectionSet.map decodedId)
        |> with Schema.Object.ProjectCard.pos
        |> with
            (Schema.Object.ProjectCard.card identity
                (Schema.Union.CardKind.fragments
                    { onTension = Schema.Object.Tension.id |> SelectionSet.map (decodedId >> Just)
                    , onProjectDraft = SelectionSet.empty |> SelectionSet.map (\_ -> Nothing)
                    }
                )
            )


tensionHeadPayload : String -> UserCtx -> SelectionSet TensionHead Schema.Object.Tension
tensionHeadPayload tid uctx =
    SelectionSet.succeed
        (\a b c d e f g receiver i j k l n o history q ->
            let
                eor =
                    EmitterOrReceiver receiver.name receiver.nameid receiver.role_type receiver.color

                isPinned =
                    receiver.pinned == Just [ { id = tid } ]

                draftNodeType =
                    n |> Maybe.andThen .node |> Maybe.andThen .type_
            in
            TensionHead a b c d e f g eor i j k isPinned n draftNodeType o history q
        )
        |> with (Schema.Object.Tension.id |> SelectionSet.map decodedId)
        |> with (Schema.Object.Tension.createdAt |> SelectionSet.map decodedTime)
        |> with (Schema.Object.Tension.createdBy identity <| SelectionSet.map Username Schema.Object.User.username)
        |> with Schema.Object.Tension.title
        |> with Schema.Object.Tension.type_
        |> with (Schema.Object.Tension.labels identity labelPayload)
        |> with (Schema.Object.Tension.assignees identity userPayload)
        --|> with (Schema.Object.Tension.emitter identity emiterOrReceiverPayload)
        |> with (Schema.Object.Tension.receiver identity (emiterOrReceiverWithPinPayload tid))
        |> with Schema.Object.Tension.status
        |> with (Schema.Object.Tension.governed_node identity governedNodePayload)
        |> (\x ->
                case uctx.username of
                    "" ->
                        hardcoded False x

                    username ->
                        with
                            (Schema.Object.Tension.subscribers
                                (\a ->
                                    { a
                                        | filter =
                                            Input.buildUserFilter
                                                (\d ->
                                                    { d | username = Present { eq = Present username, regexp = Absent, in_ = Absent } }
                                                )
                                                |> Present
                                    }
                                )
                                (SelectionSet.map identity Schema.Object.User.username)
                                |> SelectionSet.map (Maybe.map (\y -> List.length y > 0) >> withDefault False)
                            )
                            x
           )
        |> hardcoded False
        |> with
            (Schema.Object.Tension.blobs
                (\args ->
                    { args
                        | first = Present 1
                        , order =
                            Input.buildBlobOrder
                                (\x -> { x | desc = Present BlobOrderable.CreatedAt })
                                |> Present
                    }
                )
                blobPayload
                |> SelectionSet.map (Maybe.andThen List.head)
            )
        |> with
            (Schema.Object.Tension.contracts
                (\args ->
                    { args
                        | first = Present 1
                        , filter =
                            Input.buildContractFilter
                                (\x -> { x | status = Present { eq = Present ContractStatus.Open, in_ = Absent } })
                                |> Present
                    }
                )
                contractPayloadId
            )
        |> with
            (Schema.Object.Tension.history
                (\args ->
                    { args
                        | filter =
                            Input.buildEventFilter
                                (\x ->
                                    { x | not = Input.buildEventFilter (\e -> { e | event_type = Present { eq = Present TensionEvent.CommentPushed, in_ = Absent } }) |> Present }
                                )
                                |> Present
                    }
                )
                eventPayload
            )
        -- Aggregate
        |> with
            (SelectionSet.map (unwrap2 0 .count) <|
                Schema.Object.Tension.contractsAggregate (\a -> { a | filter = Present <| Input.buildContractFilter (\x -> { x | status = Present { eq = Present ContractStatus.Open, in_ = Absent } }) }) <|
                    SelectionSet.map Count Schema.Object.ContractAggregateResult.count
            )


tensionPanelPayload : UserCtx -> SelectionSet TensionPanel Schema.Object.Tension
tensionPanelPayload uctx =
    SelectionSet.succeed TensionPanel
        |> with (Schema.Object.Tension.id |> SelectionSet.map decodedId)
        |> with (Schema.Object.Tension.createdAt |> SelectionSet.map decodedTime)
        |> with (Schema.Object.Tension.createdBy identity <| SelectionSet.map Username Schema.Object.User.username)
        |> with Schema.Object.Tension.title
        |> with Schema.Object.Tension.type_
        |> with (Schema.Object.Tension.labels identity labelPayload)
        |> with (Schema.Object.Tension.assignees identity userPayload)
        --|> with (Schema.Object.Tension.emitter identity emiterOrReceiverPayload)
        |> with (Schema.Object.Tension.receiver identity emiterOrReceiverPayload)
        |> with Schema.Object.Tension.status
        |> with (Schema.Object.Tension.governed_node identity governedNodePayload)
        |> (\x ->
                case uctx.username of
                    "" ->
                        hardcoded False x

                    username ->
                        with
                            (Schema.Object.Tension.subscribers
                                (\a ->
                                    { a
                                        | filter =
                                            Input.buildUserFilter
                                                (\d ->
                                                    { d | username = Present { eq = Present username, regexp = Absent, in_ = Absent } }
                                                )
                                                |> Present
                                    }
                                )
                                (SelectionSet.map identity Schema.Object.User.username)
                                |> SelectionSet.map (Maybe.map (\y -> List.length y > 0) >> withDefault False)
                            )
                            x
           )
        |> with draftNodeTypePayload
        |> with
            (Schema.Object.Tension.history
                (\args ->
                    { args
                        | filter =
                            Input.buildEventFilter
                                (\x ->
                                    { x | not = Input.buildEventFilter (\e -> { e | event_type = Present { eq = Present TensionEvent.CommentPushed, in_ = Absent } }) |> Present }
                                )
                                |> Present
                    }
                )
                eventPayload
            )
        |> with
            (Schema.Object.Tension.comments
                (\args -> { args | first = Present nCommentPerTension })
                commentPayload
            )


tensionBlobsPayload : SelectionSet TensionBlobs Schema.Object.Tension
tensionBlobsPayload =
    SelectionSet.succeed TensionBlobs
        |> with (Schema.Object.Tension.id |> SelectionSet.map decodedId)
        |> with
            (Schema.Object.Tension.blobs
                (\args ->
                    { args
                        | first = Present nBlobPerTension
                        , order =
                            Input.buildBlobOrder
                                (\b -> { b | desc = Present BlobOrderable.CreatedAt })
                                |> Present
                    }
                )
                blobPayload
            )


tensionCommentsPayload : SelectionSet TensionComments Schema.Object.Tension
tensionCommentsPayload =
    SelectionSet.succeed TensionComments
        |> with (Schema.Object.Tension.id |> SelectionSet.map decodedId)
        |> with
            (Schema.Object.Tension.comments
                (\args -> { args | first = Present nCommentPerTension })
                commentPayload
            )


commentPayload : SelectionSet Comment Schema.Object.Comment
commentPayload =
    SelectionSet.succeed Comment
        |> with (Schema.Object.Comment.id |> SelectionSet.map decodedId)
        |> with (Schema.Object.Comment.createdAt |> SelectionSet.map decodedTime)
        |> with (Schema.Object.Comment.updatedAt |> SelectionSet.map (Maybe.map decodedTime))
        |> with (Schema.Object.Comment.createdBy identity (SelectionSet.map Username Schema.Object.User.username))
        |> with Schema.Object.Comment.message
        |> with
            -- Aggregate Reactions
            (Schema.Object.Comment.reactions identity
                (SelectionSet.map2 (\x y -> { type_ = x, user = y.username })
                    Schema.Object.Reaction.type_
                    (Schema.Object.Reaction.user identity (SelectionSet.map Username Schema.Object.User.username))
                )
                |> SelectionSet.map
                    (\x ->
                        let
                            addParam : { type_ : Int, user : String } -> Maybe Reaction -> Maybe Reaction
                            addParam { type_, user } maybeValue =
                                case maybeValue of
                                    Just value ->
                                        Just { value | users = user :: value.users }

                                    Nothing ->
                                        Just { type_ = type_, users = [ user ] }
                        in
                        withDefault [] x
                            |> List.foldr (\r dict -> Dict.update r.type_ (addParam r) dict)
                                Dict.empty
                            |> Dict.values
                    )
            )
        |> with
            (Schema.Object.Comment.files identity commentFilePayload
                |> SelectionSet.map (withDefault [])
            )


commentFilePayload : SelectionSet CommentFile Schema.Object.File
commentFilePayload =
    SelectionSet.succeed CommentFile
        |> with (Schema.Object.File.id |> SelectionSet.map decodedId)
        |> with Schema.Object.File.filename
        |> with Schema.Object.File.contentType
        |> with Schema.Object.File.size
        |> with (Schema.Object.File.embedded |> SelectionSet.map (Maybe.withDefault False))
        |> with (Schema.Object.File.createdBy identity (SelectionSet.map Username Schema.Object.User.username))


blobPayload : SelectionSet Blob Schema.Object.Blob
blobPayload =
    SelectionSet.succeed Blob
        |> with (Schema.Object.Blob.id |> SelectionSet.map decodedId)
        |> with (Schema.Object.Blob.createdAt |> SelectionSet.map decodedTime)
        |> with (Schema.Object.Blob.createdBy identity <| SelectionSet.map Username Schema.Object.User.username)
        |> with Schema.Object.Blob.blob_type
        |> with (Schema.Object.Blob.node identity nodeFragmentPayload)
        |> with Schema.Object.Blob.md
        |> with (Schema.Object.Blob.pushedFlag |> SelectionSet.map (Maybe.map decodedTime))


governedNodePayload : SelectionSet GovernedNode Schema.Object.Node
governedNodePayload =
    SelectionSet.succeed GovernedNode
        |> with Schema.Object.Node.nameid
        |> with Schema.Object.Node.type_
        |> with Schema.Object.Node.isArchived


{-| Latest blob's Node kind for unpublished drafts. Selected on every list tension but only
read when `governed_node` is null; move to a backend computed field if list latency suffers.
-}
draftNodeTypePayload : SelectionSet (Maybe NodeType.NodeType) Schema.Object.Tension
draftNodeTypePayload =
    Schema.Object.Tension.blobs
        (\args ->
            { args
                | first = Present 1
                , order = Input.buildBlobOrder (\x -> { x | desc = Present BlobOrderable.CreatedAt }) |> Present
            }
        )
        (Schema.Object.Blob.node identity Schema.Object.NodeFragment.type_
            |> SelectionSet.map (Maybe.andThen identity)
        )
        |> SelectionSet.map (Maybe.andThen List.head >> Maybe.andThen identity)


eventPayload : SelectionSet Event Schema.Object.Event
eventPayload =
    SelectionSet.succeed Event
        |> with (Schema.Object.Event.id |> SelectionSet.map decodedId)
        |> with (Schema.Object.Event.createdAt |> SelectionSet.map decodedTime)
        |> with (Schema.Object.Event.createdBy identity <| SelectionSet.map Username Schema.Object.User.username)
        |> with Schema.Object.Event.event_type
        |> with Schema.Object.Event.old
        |> with Schema.Object.Event.new
        |> with
            (Schema.Object.Event.mentioned identity <|
                SelectionSet.map4 MentionedTension
                    (Schema.Object.Tension.id |> SelectionSet.map decodedId)
                    Schema.Object.Tension.status
                    Schema.Object.Tension.title
                    Schema.Object.Tension.receiverid
            )


contractPayloadId : SelectionSet IdPayload Schema.Object.Contract
contractPayloadId =
    SelectionSet.map IdPayload (Schema.Object.Contract.id |> SelectionSet.map decodedId)


nodeFragmentPayload : SelectionSet NodeFragment Schema.Object.NodeFragment
nodeFragmentPayload =
    SelectionSet.succeed NodeFragment
        |> with Schema.Object.NodeFragment.name
        |> with Schema.Object.NodeFragment.nameid
        |> with Schema.Object.NodeFragment.type_
        |> with Schema.Object.NodeFragment.role_type
        |> with Schema.Object.NodeFragment.role_ext
        |> with Schema.Object.NodeFragment.color
        |> with Schema.Object.NodeFragment.visibility
        |> with Schema.Object.NodeFragment.mode
        |> with Schema.Object.NodeFragment.about
        |> with (Schema.Object.NodeFragment.mandate identity mandatePayload)
        |> with (Schema.Object.NodeFragment.first_link |> SelectionSet.map (Maybe.map (\x -> ternary (x == "") Nothing (Just x)) >> withDefault Nothing))


nodeFragmentLightPayload : SelectionSet NodeFragmentLight Schema.Object.NodeFragment
nodeFragmentLightPayload =
    SelectionSet.succeed NodeFragmentLight
        |> with Schema.Object.NodeFragment.name
        |> with Schema.Object.NodeFragment.nameid
        |> with Schema.Object.NodeFragment.type_
        |> with Schema.Object.NodeFragment.role_type



--
-- Query Circle Tension (all tension at depth 0 or 1 of a given node)
--


nCircleTensionPpg : Int
nCircleTensionPpg =
    10


type alias NodeTensions =
    { nameid : String
    , tensions_in : Maybe (List Tension)
    , tensions_out : Maybe (List Tension)
    , children : Maybe (List SubNodeTensions)
    }


type alias SubNodeTensions =
    { tensions_in : Maybe (List Tension)
    , tensions_out : Maybe (List Tension)
    }


circleTensionDecoder : Maybe NodeTensions -> Maybe (List Tension)
circleTensionDecoder data =
    data
        |> Maybe.map
            (\node ->
                let
                    tin =
                        node.tensions_in |> withDefault []

                    tout =
                        -- Empty for now (automatic tensions ?)
                        node.tensions_out |> withDefault []

                    tchild =
                        node.children |> withDefault [] |> List.concatMap subCircleTensionDecoder
                in
                (tin ++ tout ++ tchild)
                    |> uniqueBy .id
                    |> List.take nCircleTensionPpg
                    |> Just
            )
        |> Maybe.withDefault Nothing


subCircleTensionDecoder : SubNodeTensions -> List Tension
subCircleTensionDecoder child =
    let
        tin =
            child.tensions_in |> withDefault []

        tout =
            child.tensions_out |> withDefault []
    in
    tin ++ tout


queryCircleTension url targetid msg =
    --@DEBUG: Archived Nodes are not filtered
    makeGQLQuery url
        (Query.getNode
            (nidFilter targetid)
            circleTensionPayload
        )
        (RemoteData.fromResult >> decodeResponse circleTensionDecoder >> msg)


circleTensionFilter : Schema.Object.Node.TensionsInOptionalArguments -> Schema.Object.Node.TensionsInOptionalArguments
circleTensionFilter a =
    { a
        | first = Present (nCircleTensionPpg + nCircleTensionPpg // 2)
        , filter = Input.buildTensionFilter (\x -> { x | status = Present { eq = Present TensionStatus.Open, in_ = Absent } }) |> Present
        , order =
            Input.buildTensionOrder
                (\b -> { b | desc = Present TensionOrderable.CreatedAt })
                |> Present
    }


circleTensionPayload : SelectionSet NodeTensions Schema.Object.Node
circleTensionPayload =
    SelectionSet.succeed NodeTensions
        |> with Schema.Object.Node.nameid
        |> with (Schema.Object.Node.tensions_in circleTensionFilter tensionPayload)
        |> with (Schema.Object.Node.tensions_out circleTensionFilter tensionPayload)
        |> with
            (Schema.Object.Node.children identity
                (SelectionSet.succeed SubNodeTensions
                    |> with (Schema.Object.Node.tensions_in circleTensionFilter tensionPayload)
                    |> with (Schema.Object.Node.tensions_out circleTensionFilter tensionPayload)
                )
            )


tensionPayload : SelectionSet Tension Schema.Object.Tension
tensionPayload =
    SelectionSet.succeed Tension
        |> with (Schema.Object.Tension.id |> SelectionSet.map decodedId)
        |> with (Schema.Object.Tension.createdAt |> SelectionSet.map decodedTime)
        |> with (Schema.Object.Tension.createdBy identity <| SelectionSet.map Username Schema.Object.User.username)
        |> with Schema.Object.Tension.title
        |> with Schema.Object.Tension.type_
        |> with (Schema.Object.Tension.labels identity labelPayload)
        --|> with (Schema.Object.Tension.emitter identity emiterOrReceiverPayload)
        |> with (Schema.Object.Tension.receiver identity emiterOrReceiverPayload)
        |> with (Schema.Object.Tension.governed_node identity governedNodePayload)
        |> with draftNodeTypePayload
        |> with Schema.Object.Tension.status
        |> with
            (SelectionSet.map (unwrap Nothing .count) <|
                Schema.Object.Tension.commentsAggregate identity <|
                    SelectionSet.map Count Schema.Object.CommentAggregateResult.count
            )
        |> hardcoded Nothing


tensionPayloadFiltered : List User -> List Label -> SelectionSet Tension Schema.Object.Tension
tensionPayloadFiltered authors labels =
    SelectionSet.succeed Tension
        |> with (Schema.Object.Tension.id |> SelectionSet.map decodedId)
        |> with (Schema.Object.Tension.createdAt |> SelectionSet.map decodedTime)
        |> with (Schema.Object.Tension.createdBy (usersFilter authors) <| SelectionSet.map Username Schema.Object.User.username)
        |> with Schema.Object.Tension.title
        |> with Schema.Object.Tension.type_
        |> with (Schema.Object.Tension.labels identity labelPayload)
        --|> with (Schema.Object.Tension.emitter identity emiterOrReceiverPayload)
        |> with (Schema.Object.Tension.receiver identity emiterOrReceiverPayload)
        |> with (Schema.Object.Tension.governed_node identity governedNodePayload)
        |> with draftNodeTypePayload
        |> with Schema.Object.Tension.status
        |> with
            (SelectionSet.map (unwrap Nothing .count) <|
                Schema.Object.Tension.commentsAggregate identity <|
                    SelectionSet.map Count Schema.Object.CommentAggregateResult.count
            )
        |> hardcoded Nothing



{- Match all users -}


usersFilter : List User -> Schema.Object.Tension.CreatedByOptionalArguments -> Schema.Object.Tension.CreatedByOptionalArguments
usersFilter authors a =
    { a | filter = matchAllUsers authors }


matchAllUsers : List User -> OptionalArgument Input.UserFilter
matchAllUsers alls =
    List.foldl
        (\x filter ->
            Input.buildUserFilter
                (\d ->
                    let
                        f =
                            case filter of
                                Present y ->
                                    Just y

                                _ ->
                                    Nothing
                    in
                    { d
                        | username = Present { eq = Present x.username, regexp = Absent, in_ = Absent }
                        , and = Present [ f ]
                    }
                )
                |> Present
        )
        Absent
        alls



--
-- Query Regexp Tension
--


type alias SubNodeTensions2 =
    { tensions_in : Maybe (List Tension)
    , tensions_out : Maybe (List Tension)
    , children : Maybe (List NodeTensions)
    }


subTensionDecoder : Maybe (List (Maybe Tension)) -> Maybe (List Tension)
subTensionDecoder data =
    data
        |> Maybe.map
            (\ts ->
                List.filterMap identity ts
            )


queryAllTension url targetids first offset query_ status_ type_ msg =
    makeGQLQuery url
        (Query.queryTension
            (subTensionAllFilterByDate targetids first offset query_ status_ type_)
            tensionPayload
        )
        (RemoteData.fromResult >> decodeResponse subTensionDecoder >> msg)



{- queryIntTension and queryExtTension should support directive query to work with Dgraph....
   https://github.com/dillonkearns/elm-graphql/issues/482
-}


queryIntTension url targetids first offset query_ status_ authors labels type_ msg =
    makeGQLQuery url
        (Query.queryTension
            (subTensionIntFilterByDate targetids first offset query_ status_ type_)
            (tensionPayloadFiltered authors labels)
        )
        (RemoteData.fromResult >> decodeResponse subTensionDecoder >> msg)


queryExtTension url targetids first offset query_ status_ authors labels type_ msg =
    makeGQLQuery url
        (Query.queryTension
            (subTensionExtFilterByDate targetids first offset query_ status_ type_)
            (tensionPayloadFiltered authors labels)
        )
        (RemoteData.fromResult >> decodeResponse subTensionDecoder >> msg)



--
-- Org filters: list the governance tensions behind the nodes of a subtree
-- (roles/circles, archived or not). Tension filters can't reach node
-- properties, so we query Node and walk back through `source` (the published
-- blob) to its tension.
--


type alias OrgNodeQuery =
    { nameid : String -- subtree root, matched by nameid prefix
    , type_ : NodeType.NodeType
    , isArchived : Bool
    , noFirstLink : Bool -- True: vacant role (no first_link)
    , sort : Maybe String -- newest (default) | oldest | activity
    , first : Int
    , offset : Int
    }


queryOrgTensions url q msg =
    makeGQLQuery url
        (Query.queryNode (orgNodesFilter q) orgTensionPayload)
        (RemoteData.fromResult >> decodeResponse orgTensionDecoder >> msg)


orgTensionPayload : SelectionSet (Maybe Tension) Schema.Object.Node
orgTensionPayload =
    Schema.Object.Node.source identity (Schema.Object.Blob.tension identity tensionPayload)


orgTensionDecoder : Maybe (List (Maybe (Maybe Tension))) -> Maybe (List Tension)
orgTensionDecoder data =
    Maybe.map (List.filterMap (Maybe.andThen identity)) data


orgNodesFilter : OrgNodeQuery -> Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
orgNodesFilter q a =
    { a
        | first = Present q.first
        , offset = Present q.offset
        , order = Present (orgNodesOrder q.sort)
        , filter =
            Input.buildNodeFilter
                (\c ->
                    { c
                        | nameid = Present { eq = Absent, in_ = Absent, regexp = Present ("/^" ++ q.nameid ++ "(#|$)/") }
                        , type_ = Present { eq = Present q.type_, in_ = Absent }
                        , isArchived = Present q.isArchived

                        -- Exclude membership nodes (@username roles) and the Owner role:
                        -- they have no governance tension, hence no `source` blob.
                        -- Open roles also exclude nodes that already have a first_link.
                        , not = Present (orgNodesNot q.noFirstLink)
                    }
                )
                |> Present
    }


{-| Membership nodes (and Owner) have no source tension. Open roles also drop
nodes that already have a first_link — `not: { or: [role_type, has] }`.
-}
orgNodesNot : Bool -> Input.NodeFilter
orgNodesNot noFirstLink =
    let
        excludeMembership =
            Input.buildNodeFilter
                (\d ->
                    { d
                        | role_type =
                            Present
                                { eq = Absent
                                , in_ = Present (List.map Just membershipRoleTypes)
                                }
                    }
                )
    in
    if noFirstLink then
        Input.buildNodeFilter
            (\d ->
                { d
                    | or =
                        Present
                            [ Just excludeMembership
                            , Just (Input.buildNodeFilter (\e -> { e | has = Present [ Just NodeHasFilter.First_link ] }))
                            ]
                }
            )

    else
        excludeMembership


{-| Mirror of the tension sort keys on the node, since we order on Node here.
-}
orgNodesOrder : Maybe String -> Input.NodeOrder
orgNodesOrder sort =
    case sort of
        Just "oldest" ->
            Input.buildNodeOrder (\b -> { b | asc = Present NodeOrderable.CreatedAt })

        Just "activity" ->
            Input.buildNodeOrder (\b -> { b | desc = Present NodeOrderable.UpdatedAt })

        _ ->
            Input.buildNodeOrder (\b -> { b | desc = Present NodeOrderable.CreatedAt })


subTensionAllFilterByDate : List String -> Int -> Int -> Maybe String -> Maybe TensionStatus.TensionStatus -> Maybe TensionType.TensionType -> Query.QueryTensionOptionalArguments -> Query.QueryTensionOptionalArguments
subTensionAllFilterByDate nameids first offset query_ status_ type_ a =
    { a
        | first = Present first
        , offset = Present offset
        , order =
            Input.buildTensionOrder
                (\b -> { b | desc = Present TensionOrderable.CreatedAt })
                |> Present
        , filter =
            Input.buildTensionFilter
                (\c ->
                    { c
                        | status = status_ |> Maybe.map (\s -> { eq = Present s, in_ = Absent }) |> fromMaybe
                        , type_ = type_ |> Maybe.map (\t -> { eq = Present t, in_ = Absent }) |> fromMaybe
                        , and =
                            Present
                                ([ Input.buildTensionFilter
                                    (\d1 ->
                                        { d1
                                            | receiverid = Present { eq = Absent, regexp = Absent, in_ = List.map Just nameids |> Present }
                                            , or =
                                                Present
                                                    [ Input.buildTensionFilter
                                                        (\d2 ->
                                                            { d2 | emitterid = Present { eq = Absent, regexp = Absent, in_ = List.map Just nameids |> Present } }
                                                        )
                                                        |> Just
                                                    ]
                                        }
                                    )
                                    |> Just
                                 ]
                                    ++ (query_
                                            |> Maybe.map
                                                (\q ->
                                                    [ Input.buildTensionFilter
                                                        (\d3 ->
                                                            { d3
                                                                | title = { alloftext = Absent, anyoftext = Present q } |> Present
                                                                , or =
                                                                    Present
                                                                        [ Input.buildTensionFilter
                                                                            (\d4 ->
                                                                                { d4
                                                                                    | message = { alloftext = Absent, anyoftext = Present q } |> Present
                                                                                }
                                                                            )
                                                                            |> Just
                                                                        ]
                                                            }
                                                        )
                                                        |> Just
                                                    ]
                                                )
                                            |> withDefault []
                                       )
                                )
                    }
                )
                |> Present
    }


subTensionIntFilterByDate : List String -> Int -> Int -> Maybe String -> Maybe TensionStatus.TensionStatus -> Maybe TensionType.TensionType -> Query.QueryTensionOptionalArguments -> Query.QueryTensionOptionalArguments
subTensionIntFilterByDate nameids first offset query_ status_ type_ a =
    { a
        | first = Present first
        , offset = Present offset
        , order =
            Input.buildTensionOrder
                (\b -> { b | desc = Present TensionOrderable.CreatedAt })
                |> Present
        , filter =
            Input.buildTensionFilter
                (\c ->
                    { c
                        | status = status_ |> Maybe.map (\s -> { eq = Present s, in_ = Absent }) |> fromMaybe
                        , type_ = type_ |> Maybe.map (\t -> { eq = Present t, in_ = Absent }) |> fromMaybe
                        , emitterid = { eq = Absent, regexp = Absent, in_ = List.map Just nameids |> Present } |> Present
                        , receiverid = { eq = Absent, regexp = Absent, in_ = List.map Just nameids |> Present } |> Present
                        , and =
                            query_
                                |> Maybe.map
                                    (\q ->
                                        [ Input.buildTensionFilter
                                            (\d2 ->
                                                { d2
                                                    | title = { alloftext = Absent, anyoftext = Present q } |> Present
                                                    , or =
                                                        Present
                                                            [ Input.buildTensionFilter
                                                                (\d3 ->
                                                                    { d3
                                                                        | message = { alloftext = Absent, anyoftext = Present q } |> Present
                                                                    }
                                                                )
                                                                |> Just
                                                            ]
                                                }
                                            )
                                            |> Just
                                        ]
                                    )
                                |> fromMaybe
                    }
                )
                |> Present
    }


subTensionExtFilterByDate : List String -> Int -> Int -> Maybe String -> Maybe TensionStatus.TensionStatus -> Maybe TensionType.TensionType -> Query.QueryTensionOptionalArguments -> Query.QueryTensionOptionalArguments
subTensionExtFilterByDate nameids first offset query_ status_ type_ a =
    { a
        | first = Present first
        , offset = Present offset
        , order =
            Input.buildTensionOrder
                (\b ->
                    { b | desc = Present TensionOrderable.CreatedAt }
                )
                |> Present
        , filter =
            Input.buildTensionFilter
                (\c ->
                    { c
                        | status = status_ |> Maybe.map (\s -> { eq = Present s, in_ = Absent }) |> fromMaybe
                        , type_ = type_ |> Maybe.map (\t -> { eq = Present t, in_ = Absent }) |> fromMaybe
                        , title = query_ |> Maybe.map (\q -> { alloftext = Absent, anyoftext = Present q }) |> fromMaybe
                        , or =
                            Present
                                [ Input.buildTensionFilter
                                    (\d3 ->
                                        { d3
                                            | message = query_ |> Maybe.map (\q -> { alloftext = Absent, anyoftext = Present q }) |> fromMaybe
                                        }
                                    )
                                    |> Just
                                ]
                        , and =
                            Present
                                [ Input.buildTensionFilter
                                    (\d ->
                                        { d
                                            | receiverid = { eq = Absent, regexp = Absent, in_ = List.map Just nameids |> Present } |> Present
                                            , not =
                                                Input.buildTensionFilter
                                                    (\e ->
                                                        { e
                                                            | emitterid = { eq = Absent, regexp = Absent, in_ = List.map Just nameids |> Present } |> Present
                                                        }
                                                    )
                                                    |> Present
                                            , or =
                                                Present
                                                    [ Input.buildTensionFilter
                                                        (\d1 ->
                                                            { d1
                                                                | emitterid = { eq = Absent, regexp = Absent, in_ = List.map Just nameids |> Present } |> Present
                                                                , not =
                                                                    Input.buildTensionFilter
                                                                        (\e1 ->
                                                                            { e1
                                                                                | receiverid = { eq = Absent, regexp = Absent, in_ = List.map Just nameids |> Present } |> Present
                                                                            }
                                                                        )
                                                                        |> Present
                                                            }
                                                        )
                                                        |> Just
                                                    ]
                                        }
                                    )
                                    |> Just
                                ]
                    }
                )
                |> Present
    }



--
-- Query tension assigned to user
--


type alias AssignedTensions =
    { username : String
    , tensions_assigned : Maybe (List Tension)
    }


assignedTensionDecoder : Maybe AssignedTensions -> Maybe (Dict String (List Tension))
assignedTensionDecoder data =
    --
    -- Convert a list of tension into a Dict of tension by Receiverid
    --
    let
        addParam : Tension -> Maybe (List Tension) -> Maybe (List Tension)
        addParam value maybeValues =
            case maybeValues of
                Just values ->
                    Just (values ++ [ value ])

                Nothing ->
                    Just [ value ]

        toDict2 : List ( String, Tension ) -> Dict String (List Tension)
        toDict2 parameters =
            List.foldl
                (\( k, v ) dict -> Dict.update k (addParam v) dict)
                Dict.empty
                parameters
    in
    data
        |> Maybe.map
            (\x ->
                withDefault [] x.tensions_assigned
                    --|> List.sortBy .createdAt
                    |> List.map (\y -> ( nid2rootid y.receiver.nameid, y ))
                    |> toDict2
            )


queryAssignedTensions url form msg =
    --@DEBUG: Archived Nodes are not filtered
    makeGQLQuery url
        (Query.getUser
            (\a -> { a | username = Present form.uctx.username })
            (assignedTensionsPayload form.first)
        )
        (RemoteData.fromResult >> decodeResponse assignedTensionDecoder >> msg)


assignedTensionsPayload : Int -> SelectionSet AssignedTensions Schema.Object.User
assignedTensionsPayload first =
    SelectionSet.succeed AssignedTensions
        |> with Schema.Object.User.username
        |> with
            (Schema.Object.User.tensions_assigned
                (\a ->
                    { a
                        | first = Present first
                        , filter = Input.buildTensionFilter (\x -> { x | status = Present { eq = Present TensionStatus.Open, in_ = Absent } }) |> Present
                        , order =
                            Input.buildTensionOrder
                                (\b -> { b | desc = Present TensionOrderable.CreatedAt })
                                |> Present
                    }
                )
                tensionPayload
            )



--
-- Query tension pinned
--


type alias PinnedTensions =
    { pinned : Maybe (List PinTension)
    }


pinnedTensionDecoder : Maybe PinnedTensions -> Maybe (Maybe (List PinTension))
pinnedTensionDecoder data =
    data
        |> Maybe.map .pinned
        |> Maybe.map
            (Maybe.map
                (\y ->
                    if List.length y > 0 then
                        Just y

                    else
                        Nothing
                )
            )
        |> withDefault Nothing


queryPinnedTensions url nameid msg =
    makeGQLQuery url
        (Query.getNode
            (\a -> { a | nameid = Present nameid })
            pinnedTensionsPayload
        )
        (RemoteData.fromResult >> decodeResponse pinnedTensionDecoder >> msg)


pinnedTensionsPayload : SelectionSet PinnedTensions Schema.Object.Node
pinnedTensionsPayload =
    SelectionSet.map PinnedTensions
        (Schema.Object.Node.pinned identity pinPayload)
