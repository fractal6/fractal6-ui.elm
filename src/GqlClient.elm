{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2022 Fractale Co

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


module GqlClient exposing (..)

import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import RemoteData exposing (RemoteData)
import Session exposing (Apis)



{- GraphQL client

   Gql request are defined in Query/

-}


setAuthHeader : String -> (Graphql.Http.Request decodesTo -> Graphql.Http.Request decodesTo)
setAuthHeader token =
    Graphql.Http.withHeader "Authorization" ("Bearer " ++ token)


setVersionHeader : Apis -> (Graphql.Http.Request decodesTo -> Graphql.Http.Request decodesTo)
setVersionHeader api =
    Graphql.Http.withHeader "X-Client-Version" api.version


makeGQLQuery : Apis -> SelectionSet decodesTo RootQuery -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg) -> Cmd msg
makeGQLQuery api query decodesTo =
    query
        |> Graphql.Http.queryRequest api.gql
        {-
           queryRequest signature is of the form
               String -> SelectionSet decodesTo RootQuery -> Request decodesTo
               url    -> SelectionSet TasksWUser RootQuery -> Request TasksWUser
        -}
        --|> setAuthHeader authToken
        --|> setVersionHeader api
        |> Graphql.Http.withCredentials
        |> Graphql.Http.send decodesTo


makeGQLMutation : Apis -> SelectionSet decodesTo RootMutation -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg) -> Cmd msg
makeGQLMutation api query decodesTo =
    query
        |> Graphql.Http.mutationRequest api.gql
        --|> setAuthHeader authToken
        --|> setVersionHeader api
        |> Graphql.Http.withCredentials
        |> Graphql.Http.send decodesTo
