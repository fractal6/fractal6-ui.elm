module Model exposing (..)

import Debug
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.TensionOrderable as TensionOrderable
import Fractal.Enum.TensionType as TensionType
import Fractal.InputObject as Input
import Fractal.Object
import Fractal.Object.Label
import Fractal.Object.Node
import Fractal.Object.Tension
import Fractal.Query as Query
import Fractal.Scalar
import Fractal.ScalarCodecs
import GqlClient exposing (..)
import Graphql.Http
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import RemoteData exposing (RemoteData)



{------------------------------------------------}
-- Frontend Data structure
{------------------------------------------------}


type alias ErrorData =
    String


type RequestResult errors data
    = Success data
    | Failure errors
    | RemoteLoading
    | NotAsked



{-
   Schema Data Structure
-}


type alias Node =
    { id : String
    , name : String
    , nameid : String
    , rootnameid : String
    , type_ : NodeType.NodeType
    }


type alias Label =
    { name : String }


type alias Tension =
    { id : String
    , title : String
    , type_ : TensionType.TensionType
    , labels : Maybe (List Label)
    , n_comments : Maybe Int

    --, emitter : String
    --, receivers : String
    }



{------------------------------------------------}
-- Data Responses
{------------------------------------------------}


type alias NodesResponse =
    Maybe (List (Maybe Node))


type alias NodesData =
    List Node


type alias TensionsResponse =
    Maybe (List (Maybe Tension))


type alias TensionsData =
    List Tension



{------------------------------------------------}
-- Request decoder
{------------------------------------------------}
--
-- Nodes
--


nodeOrgaFilter : String -> Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
nodeOrgaFilter nameid a =
    { a
        | filter =
            OptionalArgument.Present
                (Input.buildNodeFilter
                    (\b ->
                        { b | rootnameid = OptionalArgument.Present { eq = OptionalArgument.Present nameid } }
                    )
                )
    }


nodePayload : SelectionSet Node Fractal.Object.Node
nodePayload =
    SelectionSet.succeed Node
        |> with (Fractal.Object.Node.id |> SelectionSet.map decodedId)
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with Fractal.Object.Node.rootnameid
        |> with Fractal.Object.Node.type_


fetchNodesOrga nameid msg =
    makeGQLQuery
        (Query.queryNode
            (nodeOrgaFilter nameid)
            nodePayload
        )
        (RemoteData.fromResult >> decodeQueryResponse >> msg)



--
-- Tensions
--


tensionPgFilter : Query.QueryTensionOptionalArguments -> Query.QueryTensionOptionalArguments
tensionPgFilter a =
    { a
        | first = OptionalArgument.Present 10
        , order =
            OptionalArgument.Present
                (Input.buildTensionOrder
                    (\b ->
                        { b | desc = OptionalArgument.Present TensionOrderable.CreatedAt }
                    )
                )
    }


tensionPgPayload : SelectionSet Tension Fractal.Object.Tension
tensionPgPayload =
    SelectionSet.succeed Tension
        |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
        |> with Fractal.Object.Tension.title
        |> with Fractal.Object.Tension.type_
        |> with
            (Fractal.Object.Tension.labels
                (\args -> { args | first = OptionalArgument.Present 3 })
                (SelectionSet.succeed Label
                    |> with Fractal.Object.Label.name
                )
            )
        |> with Fractal.Object.Tension.n_comments


fetchTensionsBunch msg =
    --fetchTensionsBunch : Cmd Msg
    makeGQLQuery
        (Query.queryTension
            tensionPgFilter
            tensionPgPayload
        )
        (RemoteData.fromResult >> decodeQueryResponse >> msg)



{------------------------------------------------}
-- Response decoder
{------------------------------------------------}
--decodeQueryResponse : RemoteData (Graphql.Http.Error TensionsResponse) TensionsResponse -> RequestResult ErrorData TensionsData
{-
   This decoder take a generic *Response type that is used for
   all gql query (get, query).
   @DEBUG: how to set the type in the function signature.
-}


decodeQueryResponse response =
    case response of
        RemoteData.Failure errors ->
            case errors of
                Graphql.Http.GraphqlError maybeParsedData err ->
                    "GraphQL errors: \n"
                        ++ Debug.toString err
                        |> Failure

                Graphql.Http.HttpError httpError ->
                    "Http error "
                        ++ Debug.toString httpError
                        |> Failure

        RemoteData.Loading ->
            RemoteLoading

        RemoteData.NotAsked ->
            NotAsked

        RemoteData.Success data ->
            gqlQueryDecoder data |> Success



--decodedId : Fractal.ScalarCodecs.Id -> String


decodedId codecId =
    case codecId of
        Fractal.Scalar.Id id ->
            id


gqlQueryDecoder : Maybe (List (Maybe a)) -> List a
gqlQueryDecoder data =
    -- Convert empty data to empty list
    case data of
        Just d ->
            if List.length d == 0 then
                []

            else
                List.filterMap identity d

        Nothing ->
            []
