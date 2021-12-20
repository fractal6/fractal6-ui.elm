module GqlClient exposing (..)

import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import RemoteData exposing (RemoteData)
import Session exposing (Apis)



{-
   GraphQL client
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
