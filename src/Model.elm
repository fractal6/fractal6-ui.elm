module Model exposing (..)

import Debug
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.TensionOrderable as TensionOrderable
import Fractal.Enum.TensionType as TensionType
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.AddTensionPayload
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



--
-- Constants
--


nLabelPerTension : Int
nLabelPerTension =
    3


nTensionPpg : Int
nTensionPpg =
    20



--
-- Frontend Data structure
--


type alias ErrorData =
    String


type RequestResult errors data
    = Success data
    | Failure errors
    | RemoteLoading
    | NotAsked



{-
   Schema Data Structure (fetch/Query)
-}


type alias Node =
    { id : String
    , name : String
    , nameid : String
    , parent : Maybe ParentNode -- see issue with recursive structure
    , type_ : NodeType.NodeType
    }


type alias ParentNode =
    { id : String }


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



--
-- Data Query Responses
--


type alias NodesResponse =
    Maybe (List (Maybe Node))


type alias NodesData =
    List Node


type alias TensionsResponse =
    Maybe (List (Maybe Tension))


type alias TensionsData =
    List Tension



--
-- Data Mutation Response
--


type alias MutationResponse a =
    Maybe a



--
-- Query decoder
--
{-
   QueryNode
-}


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
        |> with
            (Fractal.Object.Node.parent identity (SelectionSet.map (ParentNode << decodedId) Fractal.Object.Node.id))
        |> with Fractal.Object.Node.type_


fetchNodesOrga nameid msg =
    makeGQLQuery
        (Query.queryNode
            (nodeOrgaFilter nameid)
            nodePayload
        )
        (RemoteData.fromResult >> decodeQueryResponse >> msg)



{-
   QueryTension
-}


tensionPgFilter : Query.QueryTensionOptionalArguments -> Query.QueryTensionOptionalArguments
tensionPgFilter a =
    { a
        | first = OptionalArgument.Present nTensionPpg
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
                (\args -> { args | first = OptionalArgument.Present nLabelPerTension })
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



--
-- Query Response decoder
--
--decodeQueryResponse : RemoteData (Graphql.Http.Error dataResponse) dataResponse -> RequestResult ErrorData dataDecoded


decodeQueryResponse response =
    {-
       This decoder take two generic type of data:
       * `dataResponse` which is directlty related to Graphql data returned by the server.
       * `dataDecoded` which is the data Model used in Elm code.
       @DEBUG: how to set the universal type in the function signature.
    -}
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



--
-- Mutation decoder
--


type alias IdPayload =
    { id : String }


type alias AddTensionPayload =
    { tension : Maybe (List (Maybe IdPayload)) }


addOneTension msg tension =
    makeGQLMutation
        (Mutation.addTension
            tensionInputEncoder
            --(tensionInputEncoder tension)
            (SelectionSet.map AddTensionPayload <|
                Fractal.Object.AddTensionPayload.tension identity <|
                    (SelectionSet.succeed IdPayload
                        |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
                    )
            )
        )
        (RemoteData.fromResult >> decodeMutationResponse >> msg)


tensionInputEncoder : Mutation.AddTensionRequiredArguments
tensionInputEncoder =
    -- This wiil take the TensionPost !
    let
        tension =
            { createdAt = Fractal.Scalar.DateTime "Fri, 11 Jan 2013 08:11:00 GMT"
            , createdBy =
                Input.buildUserRef
                    (\x -> { x | username = OptionalArgument.Present "clara" })
            , title = "Tension from elm-graphql"
            , type_ = TensionType.Operational
            , emitter =
                Input.buildNodeRef
                    (\x -> { x | nameid = OptionalArgument.Present "SKU" })
            }
    in
    { input =
        [ Input.buildAddTensionInput tension identity ]
    }


gqlMutationDecoder : MutationResponse a -> Maybe a
gqlMutationDecoder data =
    -- Convert empty data to empty list
    case data of
        Just d ->
            Just d

        Nothing ->
            Nothing


decodeMutationResponse response =
    {-
       This decoder take two generic type of data:
       * `dataResponse` which is directlty related to Graphql data returned by the server.
       * `dataDecoded` which is the data Model used in Elm code.
       @DEBUG: how to set the universal type in the function signature.
    -}
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
            gqlMutationDecoder data |> Success



--
-- Utils
--


decodedId : Fractal.ScalarCodecs.Id -> String
decodedId (Fractal.Scalar.Id id) =
    id
