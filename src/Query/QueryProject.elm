{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2023 Fractale Co

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
    , updateProjectColumn
    , updateProjectDraft
    )

import Dict
import Extra exposing (ternary, unwrap, unwrap2)
import Fractal.Enum.ProjectColumnType as ProjectColumnType
import Fractal.Enum.RoleType as RoleType
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.AddProjectCardPayload
import Fractal.Object.AddProjectColumnPayload
import Fractal.Object.DeleteProjectCardPayload
import Fractal.Object.DeleteProjectColumnPayload
import Fractal.Object.Project
import Fractal.Object.ProjectCard
import Fractal.Object.ProjectCardAggregateResult
import Fractal.Object.ProjectColumn
import Fractal.Object.ProjectDraft
import Fractal.Object.ProjectField
import Fractal.Object.Tension
import Fractal.Object.UpdateProjectCardPayload
import Fractal.Object.UpdateProjectColumnPayload
import Fractal.Object.UpdateProjectDraftPayload
import Fractal.Object.UpdateProjectPayload
import Fractal.Object.User
import Fractal.Query as Query
import Fractal.Scalar
import Fractal.Union
import Fractal.Union.CardKind
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Query.QueryNode exposing (emiterOrReceiverPayload, labelPayload)
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


projectDataPayload : SelectionSet ProjectData Fractal.Object.Project
projectDataPayload =
    SelectionSet.succeed ProjectData
        |> with (Fractal.Object.Project.id |> SelectionSet.map decodedId)
        |> with Fractal.Object.Project.name
        |> with
            (Fractal.Object.Project.columns identity columnPayload
                |> withDefaultSelectionMap []
                |> SelectionSet.map
                    (List.map (\x -> { x | cards = List.map (\y -> { y | colid = x.id }) x.cards |> List.sortBy .pos })
                        >> List.sortBy .pos
                    )
            )


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
                (Fractal.Object.Project.columns
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
                        (Fractal.Object.ProjectColumn.id |> SelectionSet.map decodedId)
                        (Fractal.Object.ProjectColumn.cardsAggregate identity (SelectionSet.map Count Fractal.Object.ProjectCardAggregateResult.count))
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
                (Fractal.Object.UpdateProjectCardPayload.projectCard identity
                    (SelectionSet.map IdPayload
                        (SelectionSet.map decodedId Fractal.Object.ProjectCard.id)
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
                (Fractal.Object.UpdateProjectColumnPayload.projectColumn identity
                    (SelectionSet.map IdPayload
                        (SelectionSet.map decodedId Fractal.Object.ProjectColumn.id)
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
                (Fractal.Object.UpdateProjectDraftPayload.projectDraft identity
                    (SelectionSet.map IdPayload
                        (SelectionSet.map decodedId Fractal.Object.ProjectDraft.id)
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
                (Fractal.Object.AddProjectColumnPayload.projectColumn identity columnPayload)
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
                                                                        , createdAt = Dict.get "createdAt" form.post |> withDefault "" |> Fractal.Scalar.DateTime |> Present
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
                Fractal.Object.AddProjectCardPayload.projectCard identity
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
                Fractal.Object.DeleteProjectCardPayload.projectCard identity
                    (SelectionSet.map decodedId Fractal.Object.ProjectCard.id)
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
                Fractal.Object.DeleteProjectColumnPayload.projectColumn identity
                    (SelectionSet.map decodedId Fractal.Object.ProjectColumn.id)
            )
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)



---
--- PAYLOAD
---


columnPayload : SelectionSet ProjectColumn Fractal.Object.ProjectColumn
columnPayload =
    SelectionSet.succeed ProjectColumn
        |> with (Fractal.Object.ProjectColumn.id |> SelectionSet.map decodedId)
        |> with Fractal.Object.ProjectColumn.name
        |> with Fractal.Object.ProjectColumn.color
        |> with Fractal.Object.ProjectColumn.pos
        |> with Fractal.Object.ProjectColumn.col_type
        |> with (Fractal.Object.ProjectColumn.cards identity projectCardPayload |> withDefaultSelectionMap [])


columnPayloadEdit : SelectionSet ProjectColumnEdit Fractal.Object.ProjectColumn
columnPayloadEdit =
    SelectionSet.succeed ProjectColumnEdit
        |> with (Fractal.Object.ProjectColumn.id |> SelectionSet.map decodedId)
        |> with Fractal.Object.ProjectColumn.name
        |> with Fractal.Object.ProjectColumn.description
        |> with Fractal.Object.ProjectColumn.color
        |> with Fractal.Object.ProjectColumn.pos


projectCardPayload : SelectionSet ProjectCard Fractal.Object.ProjectCard
projectCardPayload =
    SelectionSet.succeed ProjectCard
        |> with (Fractal.Object.ProjectCard.id |> SelectionSet.map decodedId)
        |> hardcoded ""
        |> with Fractal.Object.ProjectCard.pos
        |> with (Fractal.Object.ProjectCard.card identity cardPayload)


cardPayload : SelectionSet CardKind Fractal.Union.CardKind
cardPayload =
    Fractal.Union.CardKind.fragments
        -- @DEBUG: agregate subquery doesn seems to work !!!
        --{ onTension = SelectionSet.map CardTension tensionPayload
        { onTension = SelectionSet.map CardTension tensionPayload2
        , onProjectDraft = SelectionSet.map CardDraft draftPayload
        }


draftPayload : SelectionSet ProjectDraft Fractal.Object.ProjectDraft
draftPayload =
    SelectionSet.succeed ProjectDraft
        |> with (Fractal.Object.ProjectDraft.id |> SelectionSet.map decodedId)
        |> with Fractal.Object.ProjectDraft.title
        |> with Fractal.Object.ProjectDraft.message
        |> with (Fractal.Object.ProjectDraft.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.ProjectDraft.createdBy identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> hardcoded ""
        |> hardcoded ""
        |> hardcoded 0



--
-- Utils
--


tensionPayload2 : SelectionSet Tension Fractal.Object.Tension
tensionPayload2 =
    SelectionSet.succeed Tension
        |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Tension.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.Tension.createdBy identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with Fractal.Object.Tension.title
        |> with Fractal.Object.Tension.type_
        |> with (Fractal.Object.Tension.labels identity labelPayload)
        --|> with (Fractal.Object.Tension.emitter identity emiterOrReceiverPayload)
        |> with (Fractal.Object.Tension.receiver identity emiterOrReceiverPayload)
        |> with Fractal.Object.Tension.action
        |> with Fractal.Object.Tension.status
        |> hardcoded Nothing
        |> hardcoded Nothing
