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
    "http://localhost:8888/api"


getAuthHeader : String -> (Graphql.Http.Request decodesTo -> Graphql.Http.Request decodesTo)
getAuthHeader token =
    Graphql.Http.withHeader "Authorization" ("Bearer " ++ token)


makeGQLQuery : SelectionSet decodesTo RootQuery -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg) -> Cmd msg
makeGQLQuery query decodesTo =
    --makeGQLQuery authToken query decodesTo =
    query
        |> Graphql.Http.queryRequest graphql_url
        --|> Graphql.Http.queryRequestWithHttpGet graphql_url Graphql.Http.AlwaysGet
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
           queryRequest signature is of the form
               String -> SelectionSet decodesTo RootQuery -> Request decodesTo
               url    -> SelectionSet TasksWUser RootQuery -> Request TasksWUser
        -}
        --|> getAuthHeader authToken
        |> Graphql.Http.send decodesTo


type GQLResponse decodesTo
    = GQLResponse (RemoteData (Graphql.Http.Error decodesTo) decodesTo)


decodeGQLResponse : Maybe (List (Maybe a)) -> List a
decodeGQLResponse data =
    --decodeGQLResponse : GQLResponse decodesTo -> RemoteData (Graphql.Http.Error decodesTo) decodesTo
    -- Convert empty data to empty list
    case data of
        Just d ->
            if List.length d == 0 then
                []

            else
                List.filterMap identity d

        Nothing ->
            []
