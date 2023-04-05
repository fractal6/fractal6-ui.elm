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


module Query.QueryProject exposing (getProjects)

import Bulk.Codecs exposing (nid2rootid)
import Dict exposing (Dict)
import Extra exposing (unwrap)
import Fractal.Enum.ProjectOrderable as ProjectOrderable
import Fractal.InputObject as Input
import Fractal.Object
import Fractal.Object.Label
import Fractal.Object.Node
import Fractal.Object.Project
import Fractal.Object.Tension
import Fractal.Object.TensionAggregateResult
import Fractal.Object.User
import Fractal.Object.UserAggregateResult
import Fractal.Query as Query
import Fractal.Scalar
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import List.Extra as LE
import Loading exposing (RequestResult(..))
import Maybe exposing (withDefault)
import ModelSchema exposing (Project, decodeResponse, decodedId)
import RemoteData exposing (RemoteData)
import String.Extra as SE



--
-- Query Roles
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
                        |> Just
            )
        |> withDefault Nothing


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


{-| Fetch on the given nodes only
-}
getProjects url nids msg =
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
