module GqlClient exposing (GQLResponse, decodeGQLResponse, makeGQLMutation, makeGQLQuery)

import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import RemoteData exposing (RemoteData)



{-
   GraphQL client
-}


graphql_url : String
graphql_url =
    "https://localhost:8888/api"


getAuthHeader : String -> (Graphql.Http.Request decodesTo -> Graphql.Http.Request decodesTo)
getAuthHeader token =
    Graphql.Http.withHeader "Authorization" ("Bearer " ++ token)


makeGQLQuery : SelectionSet decodesTo RootQuery -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg) -> Cmd msg
makeGQLQuery query decodesTo =
    --makeGQLQuery authToken query decodesTo =
    query
        |> Graphql.Http.queryRequest graphql_url
        {-
           queryRequest signature is of the form
               String -> SelectionSet decodesTo RootQuery -> Request decodesTo
               url    -> SelectionSet TasksWUser RootQuery -> Request TasksWUser
        -}
        --|> getAuthHeader authToken
        |> Graphql.Http.send decodesTo


makeGQLMutation : SelectionSet decodesTo RootMutation -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg) -> Cmd msg
makeGQLMutation query decodesTo =
    --makeGQLMutation authToken query decodesTo =
    query
        |> Graphql.Http.mutationRequest graphql_url
        {-
           mutationRequest signature is of the form
               String -> SelectionSet decodesTo RootMutation -> Request decodesTo
               url    -> SelectionSet TasksWUser RootMutation -> Request TasksWUser
        -}
        --|> getAuthHeader authToken
        |> Graphql.Http.send decodesTo


type GQLResponse decodesTo
    = GQLResponse (RemoteData (Graphql.Http.Error decodesTo) decodesTo)


decodeGQLResponse : GQLResponse decodesTo -> RemoteData (Graphql.Http.Error decodesTo) decodesTo
decodeGQLResponse (GQLResponse value) =
    value
