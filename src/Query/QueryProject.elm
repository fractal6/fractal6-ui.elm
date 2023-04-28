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
    ( addProjectColumn
    , getProject
    , moveProjectTension
    )

import Dict
import Extra exposing (unwrap, unwrap2)
import Fractal.Enum.RoleType as RoleType
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.Project
import Fractal.Object.ProjectColumn
import Fractal.Object.ProjectField
import Fractal.Object.ProjectTension
import Fractal.Object.Tension
import Fractal.Object.UpdateProjectPayload
import Fractal.Object.UpdateProjectTensionPayload
import Fractal.Query as Query
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


columnPayload : SelectionSet ProjectColumn Fractal.Object.ProjectColumn
columnPayload =
    SelectionSet.succeed ProjectColumn
        |> with (Fractal.Object.ProjectColumn.id |> SelectionSet.map decodedId)
        |> with Fractal.Object.ProjectColumn.name
        |> with Fractal.Object.ProjectColumn.pos
        |> with
            (Fractal.Object.ProjectColumn.tensions identity
                (SelectionSet.succeed ProjectTension
                    |> with (Fractal.Object.ProjectTension.id |> SelectionSet.map decodedId)
                    |> with Fractal.Object.ProjectTension.pos
                    |> with (Fractal.Object.ProjectTension.tension identity tensionPayload)
                )
                |> withDefaultSelectionMap []
            )



--
-- Patch Project
--


moveProjectTension url id_ pos colid msg =
    makeGQLMutation url
        (Mutation.updateProjectTension
            { input =
                Input.buildUpdateProjectTensionInput { filter = Input.buildProjectTensionFilter (oneId id_) }
                    (\_ ->
                        { set = Absent
                        , remove = Absent
                        }
                    )
            }
            (SelectionSet.map (\a -> withDefault [] a |> List.head |> withDefault Nothing)
                (Fractal.Object.UpdateProjectTensionPayload.projectTension identity
                    (SelectionSet.map IdPayload
                        (SelectionSet.map decodedId Fractal.Object.ProjectTension.id)
                    )
                )
            )
        )
        (RemoteData.fromResult >> decodeResponse (withDefault Nothing) >> msg)


addProjectColumn url form msg =
    let
        name_m =
            Dict.get "name" form.post

        column =
            Input.buildProjectColumnRef
                (\i ->
                    { i
                        | name = fromMaybe name_m
                        , description = fromMaybe (Dict.get "description" form.post)
                        , color = fromMaybe (Dict.get "color" form.post)
                        , pos = fromMaybe form.pos
                    }
                )
    in
    makeGQLMutation url
        (Mutation.updateProject
            { input =
                Input.buildUpdateProjectInput { filter = Input.buildProjectFilter (oneId form.projectid) }
                    (\_ ->
                        { set = Input.buildProjectPatch (\i -> { i | columns = Present [ column ] }) |> Present
                        , remove = Absent
                        }
                    )
            }
            (SelectionSet.map (\a -> withDefault [] a |> List.head |> withDefault Nothing |> withDefault Nothing)
                (Fractal.Object.UpdateProjectPayload.project identity
                    (SelectionSet.map (withDefault [] >> List.head)
                        (Fractal.Object.Project.columns
                            (\b ->
                                { b
                                    | filter =
                                        Input.buildProjectColumnFilter (\c -> { c | name = fromMaybe <| Maybe.map (\name -> { eq = Present name, in_ = Absent }) name_m })
                                            |> Present
                                }
                            )
                            columnPayload
                        )
                    )
                )
            )
        )
        (RemoteData.fromResult >> decodeResponse (withDefault Nothing) >> msg)
