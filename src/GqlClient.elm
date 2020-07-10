module GqlClient exposing (..)

import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import RemoteData exposing (RemoteData)



{-
   GraphQL client
-}


getAuthHeader : String -> (Graphql.Http.Request decodesTo -> Graphql.Http.Request decodesTo)
getAuthHeader token =
    Graphql.Http.withHeader "Authorization" ("Bearer " ++ token)


makeGQLQuery : String -> SelectionSet decodesTo RootQuery -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg) -> Cmd msg
makeGQLQuery url query decodesTo =
    query
        |> Graphql.Http.queryRequest url
        {-
           queryRequest signature is of the form
               String -> SelectionSet decodesTo RootQuery -> Request decodesTo
               url    -> SelectionSet TasksWUser RootQuery -> Request TasksWUser
        -}
        --|> getAuthHeader authToken
        |> Graphql.Http.withCredentials
        |> Graphql.Http.send decodesTo


makeGQLMutation : String -> SelectionSet decodesTo RootMutation -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg) -> Cmd msg
makeGQLMutation url query decodesTo =
    query
        |> Graphql.Http.mutationRequest url
        --|> getAuthHeader authToken
        |> Graphql.Http.withCredentials
        |> Graphql.Http.send decodesTo
