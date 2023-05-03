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
    , getProject
    , moveProjectCard
    )

import Dict
import Extra exposing (unwrap, unwrap2)
import Fractal.Enum.ProjectColumnType as ProjectColumnType
import Fractal.Enum.RoleType as RoleType
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.AddProjectCardPayload
import Fractal.Object.AddProjectColumnPayload
import Fractal.Object.Project
import Fractal.Object.ProjectCard
import Fractal.Object.ProjectColumn
import Fractal.Object.ProjectDraft
import Fractal.Object.ProjectField
import Fractal.Object.Tension
import Fractal.Object.UpdateProjectCardPayload
import Fractal.Object.UpdateProjectPayload
import Fractal.Query as Query
import Fractal.Scalar
import Fractal.Union
import Fractal.Union.CardKind
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
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
        |> with (Fractal.Object.Project.columns identity columnPayload |> withDefaultSelectionMap [])



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

                                        -- @todo: update cardKind (edit title / ard from separate view)
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
                    , col_type = ProjectColumnType.NormalColumn
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
            (SelectionSet.map (unwrap2 Nothing List.head) <|
                Fractal.Object.AddProjectColumnPayload.projectColumn identity
                    columnPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse (withDefault Nothing) >> msg)


addProjectCard url form msg =
    makeGQLMutation url
        (Mutation.addProjectCard
            { input =
                form.tids
                    |> List.map
                        (\tid_m ->
                            Input.buildAddProjectCardInput
                                { pc = Input.buildProjectColumnRef (\a -> { a | id = Present (encodeId form.colid) })
                                , pos = form.pos |> withDefault 0
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
                                                                        | title = fromMaybe (Dict.get "title" form.post)
                                                                        , message = fromMaybe (Dict.get "message" form.post)
                                                                        , createdAt = Dict.get "createdAt" form.post |> withDefault "" |> Fractal.Scalar.DateTime |> Present
                                                                        , createdBy = Input.buildUserRef (\u -> { u | username = Present form.uctx.username }) |> Present
                                                                    }
                                                                )
                                                                |> Present
                                                    }
                                                )
                                }
                                identity
                        )
            }
            (SelectionSet.map (unwrap2 Nothing List.head) <|
                Fractal.Object.AddProjectCardPayload.projectCard identity
                    projectCardPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse (withDefault Nothing) >> msg)



---
--- PAYLOAD
---


columnPayload : SelectionSet ProjectColumn Fractal.Object.ProjectColumn
columnPayload =
    SelectionSet.succeed ProjectColumn
        |> with (Fractal.Object.ProjectColumn.id |> SelectionSet.map decodedId)
        |> with Fractal.Object.ProjectColumn.name
        |> with Fractal.Object.ProjectColumn.pos
        |> with (Fractal.Object.ProjectColumn.cards identity projectCardPayload |> withDefaultSelectionMap [])


projectCardPayload : SelectionSet ProjectCard Fractal.Object.ProjectCard
projectCardPayload =
    SelectionSet.succeed ProjectCard
        |> with (Fractal.Object.ProjectCard.id |> SelectionSet.map decodedId)
        |> with Fractal.Object.ProjectCard.pos
        |> with (Fractal.Object.ProjectCard.card identity cardPayload)


cardPayload : SelectionSet CardKind Fractal.Union.CardKind
cardPayload =
    Fractal.Union.CardKind.fragments
        { onTension = SelectionSet.map CardTension tensionPayload
        , onProjectDraft = SelectionSet.map CardDraft draftPayload
        }


draftPayload : SelectionSet ProjectDraft Fractal.Object.ProjectDraft
draftPayload =
    SelectionSet.succeed ProjectDraft
        |> with (Fractal.Object.ProjectDraft.id |> SelectionSet.map decodedId)
        |> with Fractal.Object.ProjectDraft.title
        |> with Fractal.Object.ProjectDraft.message
