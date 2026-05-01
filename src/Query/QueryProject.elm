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


module Query.QueryProject exposing
    ( addProjectCard
    , addProjectColumn
    , deleteProjectColumns
    , getNoStatusCol
    , getProject
    , getProjectColumn
    , moveProjectCard
    , moveProjectColumn
    , removeProjectCards
    , setProjectDraftAssignee
    , setProjectDraftLabel
    , updateProjectColumn
    , updateProjectDraft
    )

import Bulk exposing (AssigneeForm, LabelForm)
import Dict
import Extra exposing (ternary, unwrap, unwrap2)
import Schema.Enum.ProjectColumnType as ProjectColumnType
import Schema.Enum.RoleType as RoleType
import Schema.InputObject as Input
import Schema.Mutation as Mutation
import Schema.Object
import Schema.Object.AddProjectCardPayload
import Schema.Object.AddProjectColumnPayload
import Schema.Object.Comment
import Schema.Object.CommentAggregateResult
import Schema.Object.DeleteProjectCardPayload
import Schema.Object.DeleteProjectColumnPayload
import Schema.Object.Project
import Schema.Object.ProjectCard
import Schema.Object.ProjectCardAggregateResult
import Schema.Object.ProjectColumn
import Schema.Object.ProjectDraft
import Schema.Object.ProjectField
import Schema.Object.Tension
import Schema.Object.UpdateProjectCardPayload
import Schema.Object.UpdateProjectColumnPayload
import Schema.Object.UpdateProjectDraftPayload
import Schema.Object.UpdateProjectPayload
import Schema.Object.User
import Schema.Query as Query
import Schema.Scalar
import Schema.Union
import Schema.Union.CardKind
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Query.QueryNode exposing (emiterOrReceiverPayload, labelPayload, userPayload)
import Query.QueryTension exposing (tensionPayload)
import RemoteData
import String.Extra as SE



--
-- Query Project
--


getProject url projectid msg =
    makeGQLQuery url
        (Query.getProject
            { id = encodeId projectid }
            projectDataPayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


projectDataPayload : SelectionSet ProjectData Schema.Object.Project
projectDataPayload =
    SelectionSet.succeed ProjectData
        |> with (Schema.Object.Project.id |> SelectionSet.map decodedId)
        |> with Schema.Object.Project.name
        |> with Schema.Object.Project.description
        |> with Schema.Object.Project.status
        |> with (Schema.Object.Project.nodes identity emiterOrReceiverPayload |> SelectionSet.map (withDefault []))
        |> with
            (Schema.Object.Project.columns identity columnPayload
                |> withDefaultSelectionMap []
                |> SelectionSet.map
                    (List.map (\x -> { x | cards = List.map (\y -> { y | colid = x.id }) x.cards |> List.sortBy .pos })
                        >> List.sortBy .pos
                    )
            )
        |> with (Schema.Object.Project.collaborators identity (SelectionSet.map Username Schema.Object.User.username) |> SelectionSet.map (withDefault []))
        |> with Schema.Object.Project.peerCanEditProject
        |> with Schema.Object.Project.guestCanEditProject


getProjectColumn url colid msg =
    makeGQLQuery url
        (Query.getProjectColumn
            { id = encodeId colid }
            columnPayloadEdit
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


getNoStatusCol url projectid msg =
    makeGQLQuery url
        (Query.getProject
            { id = encodeId projectid }
            (SelectionSet.map identity
                (Schema.Object.Project.columns
                    (\args ->
                        { args
                            | first = Present 1
                            , filter =
                                Input.buildProjectColumnFilter
                                    (\x -> { x | col_type = Present { eq = Present ProjectColumnType.NoStatusColumn, in_ = Absent } })
                                    |> Present
                        }
                    )
                    (SelectionSet.map2 (\a b -> { id = a, cards_len = unwrap2 0 .count b })
                        (Schema.Object.ProjectColumn.id |> SelectionSet.map decodedId)
                        (Schema.Object.ProjectColumn.cardsAggregate identity (SelectionSet.map Count Schema.Object.ProjectCardAggregateResult.count))
                    )
                    |> withDefaultSelectionMap []
                    |> SelectionSet.map List.head
                )
            )
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)



--
-- Patch Project items
--


moveProjectCard url id_ pos colid msg =
    makeGQLMutation url
        (Mutation.updateProjectCard
            { input =
                Input.buildUpdateProjectCardInput { filter = Input.buildProjectCardFilter (oneId id_) }
                    (\_ ->
                        { set =
                            Input.buildProjectCardPatch
                                (\a ->
                                    { a
                                        | pos = Present pos
                                        , pc = Input.buildProjectColumnRef (\b -> { b | id = Present (encodeId colid) }) |> Present
                                    }
                                )
                                |> Present
                        , remove = Absent
                        }
                    )
            }
            (SelectionSet.map (\a -> withDefault [] a |> List.head |> withDefault Nothing)
                (Schema.Object.UpdateProjectCardPayload.projectCard identity
                    (SelectionSet.map IdPayload
                        (SelectionSet.map decodedId Schema.Object.ProjectCard.id)
                    )
                )
            )
        )
        (RemoteData.fromResult >> decodeResponse (withDefault Nothing) >> msg)


moveProjectColumn url id_ pos msg =
    let
        form =
            { colid = id_
            , post = Dict.fromList [ ( "pos", String.fromInt pos ) ]
            }
    in
    updateProjectColumn url form msg


updateProjectColumn url form msg =
    makeGQLMutation url
        (Mutation.updateProjectColumn
            { input =
                Input.buildUpdateProjectColumnInput { filter = Input.buildProjectColumnFilter (oneId form.colid) }
                    (\_ ->
                        { set =
                            Input.buildProjectColumnPatch
                                (\a ->
                                    { a
                                        | name = fromMaybe (Dict.get "name" form.post)
                                        , description = fromMaybe (Dict.get "description" form.post)
                                        , color = fromMaybe (Dict.get "color" form.post)
                                        , pos = fromMaybe (Dict.get "pos" form.post |> unwrap Nothing String.toInt)
                                    }
                                )
                                |> Present
                        , remove = Absent
                        }
                    )
            }
            (SelectionSet.map (\a -> withDefault [] a |> List.head |> withDefault Nothing)
                (Schema.Object.UpdateProjectColumnPayload.projectColumn identity
                    (SelectionSet.map IdPayload
                        (SelectionSet.map decodedId Schema.Object.ProjectColumn.id)
                    )
                )
            )
        )
        (RemoteData.fromResult >> decodeResponse (withDefault Nothing) >> msg)


updateProjectDraft url form msg =
    makeGQLMutation url
        (Mutation.updateProjectDraft
            { input =
                Input.buildUpdateProjectDraftInput { filter = Input.buildProjectDraftFilter (oneId form.id) }
                    (\_ ->
                        { set =
                            Input.buildProjectDraftPatch
                                (\a ->
                                    { a
                                        | title = fromMaybe (Dict.get "title" form.post)
                                        , message = fromMaybe (Dict.get "message" form.post)
                                    }
                                )
                                |> Present
                        , remove = Absent
                        }
                    )
            }
            (SelectionSet.map (\a -> withDefault [] a |> List.head |> withDefault Nothing)
                (Schema.Object.UpdateProjectDraftPayload.projectDraft identity
                    (SelectionSet.map IdPayload
                        (SelectionSet.map decodedId Schema.Object.ProjectDraft.id)
                    )
                )
            )
        )
        (RemoteData.fromResult >> decodeResponse (withDefault Nothing) >> msg)



--
-- Add projects items
--


addProjectColumn url form msg =
    makeGQLMutation url
        (Mutation.addProjectColumn
            { input =
                [ Input.buildAddProjectColumnInput
                    { name = Dict.get "name" form.post |> withDefault ""
                    , pos = form.pos |> withDefault 0
                    , col_type = form.col_type |> withDefault ProjectColumnType.NormalColumn
                    , project = Input.buildProjectRef (\a -> { a | id = Present (encodeId form.projectid) })
                    }
                    (\x ->
                        { x
                            | description = fromMaybe (Dict.get "description" form.post)
                            , color = fromMaybe (Dict.get "color" form.post)
                        }
                    )
                ]
            }
            --RequestResult (List String) (Maybe.Maybe (List (Maybe.Maybe ProjectColumn)))
            (SelectionSet.map (unwrap2 Nothing List.head)
                (Schema.Object.AddProjectColumnPayload.projectColumn identity columnPayload)
            )
        )
        (RemoteData.fromResult >> decodeResponse (withDefault Nothing >> Maybe.map (\x -> { x | cards = List.map (\y -> { y | colid = x.id }) x.cards })) >> msg)


addProjectCard url form msg =
    makeGQLMutation url
        (Mutation.addProjectCard
            { input =
                form.tids
                    |> List.indexedMap
                        (\i tid_m ->
                            Input.buildAddProjectCardInput
                                { pc = Input.buildProjectColumnRef (\a -> { a | id = Present (encodeId form.colid) })
                                , pos = form.pos + i
                                , card =
                                    case tid_m of
                                        Just tid ->
                                            -- add tension card
                                            Input.buildCardKindRef (\a -> { a | tensionRef = Input.buildTensionRef (\b -> { b | id = Present (encodeId tid) }) |> Present })

                                        Nothing ->
                                            -- add draft card
                                            Input.buildCardKindRef
                                                (\a ->
                                                    { a
                                                        | projectDraftRef =
                                                            Input.buildProjectDraftRef
                                                                (\b ->
                                                                    { b
                                                                        | title = Present form.title
                                                                        , message = fromMaybe (Dict.get "message" form.post)
                                                                        , createdAt = Dict.get "createdAt" form.post |> withDefault "" |> Schema.Scalar.DateTime |> Present
                                                                        , createdBy = Input.buildUserRef (\u -> { u | username = Present form.uctx.username }) |> Present
                                                                        , project_status = Input.buildProjectColumnRef (\c -> { c | id = Present (encodeId form.colid) }) |> Present
                                                                    }
                                                                )
                                                                |> Present
                                                    }
                                                )
                                }
                                identity
                        )
            }
            (SelectionSet.map (unwrap [] (List.filterMap identity)) <|
                Schema.Object.AddProjectCardPayload.projectCard identity
                    projectCardPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse (Maybe.map (List.map (\b -> { b | colid = form.colid }))) >> msg)


removeProjectCards url uids msg =
    makeGQLMutation url
        (Mutation.deleteProjectCard
            { filter =
                Input.buildProjectCardFilter (\i -> { i | id = Present <| List.map encodeId uids })
            }
            (SelectionSet.map (unwrap [] (List.filterMap identity)) <|
                Schema.Object.DeleteProjectCardPayload.projectCard identity
                    (SelectionSet.map decodedId Schema.Object.ProjectCard.id)
            )
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


deleteProjectColumns url uids msg =
    makeGQLMutation url
        (Mutation.deleteProjectColumn
            { filter =
                Input.buildProjectColumnFilter (\i -> { i | id = Present <| List.map encodeId uids })
            }
            (SelectionSet.map (unwrap [] (List.filterMap identity)) <|
                Schema.Object.DeleteProjectColumnPayload.projectColumn identity
                    (SelectionSet.map decodedId Schema.Object.ProjectColumn.id)
            )
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)



---
--- PAYLOAD
---


columnPayload : SelectionSet ProjectColumn Schema.Object.ProjectColumn
columnPayload =
    SelectionSet.succeed ProjectColumn
        |> with (Schema.Object.ProjectColumn.id |> SelectionSet.map decodedId)
        |> with Schema.Object.ProjectColumn.name
        |> with Schema.Object.ProjectColumn.color
        |> with Schema.Object.ProjectColumn.pos
        |> with Schema.Object.ProjectColumn.col_type
        |> with (Schema.Object.ProjectColumn.cards identity projectCardPayload |> withDefaultSelectionMap [])


columnPayloadEdit : SelectionSet ProjectColumnEdit Schema.Object.ProjectColumn
columnPayloadEdit =
    SelectionSet.succeed ProjectColumnEdit
        |> with (Schema.Object.ProjectColumn.id |> SelectionSet.map decodedId)
        |> with Schema.Object.ProjectColumn.name
        |> with Schema.Object.ProjectColumn.description
        |> with Schema.Object.ProjectColumn.color
        |> with Schema.Object.ProjectColumn.pos


projectCardPayload : SelectionSet ProjectCard Schema.Object.ProjectCard
projectCardPayload =
    SelectionSet.succeed ProjectCard
        |> with (Schema.Object.ProjectCard.id |> SelectionSet.map decodedId)
        |> hardcoded ""
        |> with Schema.Object.ProjectCard.pos
        |> with (Schema.Object.ProjectCard.card identity cardPayload)


cardPayload : SelectionSet CardKind Schema.Union.CardKind
cardPayload =
    Schema.Union.CardKind.fragments
        -- @DEBUG: agregate subquery doesn seems to work !!!
        --{ onTension = SelectionSet.map CardTension tensionPayload
        { onTension = SelectionSet.map CardTension tensionPayload2
        , onProjectDraft = SelectionSet.map CardDraft draftPayload
        }


draftPayload : SelectionSet ProjectDraft Schema.Object.ProjectDraft
draftPayload =
    SelectionSet.succeed ProjectDraft
        |> with (Schema.Object.ProjectDraft.id |> SelectionSet.map decodedId)
        |> with Schema.Object.ProjectDraft.title
        |> with Schema.Object.ProjectDraft.message
        |> with (Schema.Object.ProjectDraft.createdAt |> SelectionSet.map decodedTime)
        |> with (Schema.Object.ProjectDraft.createdBy identity <| SelectionSet.map Username Schema.Object.User.username)
        |> with (Schema.Object.ProjectDraft.labels identity labelPayload)
        |> with (Schema.Object.ProjectDraft.assignees identity userPayload)
        |> hardcoded ""
        |> hardcoded ""
        |> hardcoded 0



--
-- Utils
--
--
-- Patch ProjectDraft labels / assignees
--


setProjectDraftLabel url form msg =
    makeGQLMutation url
        (Mutation.updateProjectDraft
            (setProjectDraftLabelEncoder form)
            (SelectionSet.map (\a -> withDefault [] a |> List.head |> withDefault Nothing)
                (Schema.Object.UpdateProjectDraftPayload.projectDraft identity
                    (SelectionSet.map IdPayload
                        (SelectionSet.map decodedId Schema.Object.ProjectDraft.id)
                    )
                )
            )
        )
        (RemoteData.fromResult >> decodeResponse (withDefault Nothing) >> msg)


setProjectDraftLabelEncoder : LabelForm -> Mutation.UpdateProjectDraftRequiredArguments
setProjectDraftLabelEncoder f =
    let
        inputReq =
            { filter = Input.buildProjectDraftFilter (oneId f.tid) }

        patch =
            Input.buildProjectDraftPatch
                (\s ->
                    { s
                        | labels =
                            Present
                                [ Input.buildLabelRef
                                    (\u -> { u | id = Present (encodeId f.label.id) })
                                ]
                    }
                )
                |> Present

        inputOpt =
            \_ ->
                { set = ternary f.isNew patch Absent
                , remove = ternary (not f.isNew) patch Absent
                }
    in
    { input = Input.buildUpdateProjectDraftInput inputReq inputOpt }


setProjectDraftAssignee url form msg =
    makeGQLMutation url
        (Mutation.updateProjectDraft
            (setProjectDraftAssigneeEncoder form)
            (SelectionSet.map (\a -> withDefault [] a |> List.head |> withDefault Nothing)
                (Schema.Object.UpdateProjectDraftPayload.projectDraft identity
                    (SelectionSet.map IdPayload
                        (SelectionSet.map decodedId Schema.Object.ProjectDraft.id)
                    )
                )
            )
        )
        (RemoteData.fromResult >> decodeResponse (withDefault Nothing) >> msg)


setProjectDraftAssigneeEncoder : AssigneeForm -> Mutation.UpdateProjectDraftRequiredArguments
setProjectDraftAssigneeEncoder f =
    let
        inputReq =
            { filter = Input.buildProjectDraftFilter (oneId f.tid) }

        patch =
            Input.buildProjectDraftPatch
                (\s ->
                    { s
                        | assignees =
                            Present
                                [ Input.buildUserRef
                                    (\u -> { u | username = Present f.assignee.username })
                                ]
                    }
                )
                |> Present

        inputOpt =
            \_ ->
                { set = ternary f.isNew patch Absent
                , remove = ternary (not f.isNew) patch Absent
                }
    in
    { input = Input.buildUpdateProjectDraftInput inputReq inputOpt }


tensionPayload2 : SelectionSet Tension Schema.Object.Tension
tensionPayload2 =
    SelectionSet.succeed Tension
        |> with (Schema.Object.Tension.id |> SelectionSet.map decodedId)
        |> with (Schema.Object.Tension.createdAt |> SelectionSet.map decodedTime)
        |> with (Schema.Object.Tension.createdBy identity <| SelectionSet.map Username Schema.Object.User.username)
        |> with Schema.Object.Tension.title
        |> with Schema.Object.Tension.type_
        |> with (Schema.Object.Tension.labels identity labelPayload)
        --|> with (Schema.Object.Tension.emitter identity emiterOrReceiverPayload)
        |> with (Schema.Object.Tension.receiver identity emiterOrReceiverPayload)
        |> with Schema.Object.Tension.action
        |> with Schema.Object.Tension.status
        -- Aggreate doesn not seem to work with enum...
        --|> with
        --    (SelectionSet.map (unwrap Nothing .count) <|
        --        Schema.Object.Tension.commentsAggregate identity <|
        --            SelectionSet.map Count Schema.Object.CommentAggregateResult.count
        --    )
        |> hardcoded Nothing
        |> hardcoded Nothing
