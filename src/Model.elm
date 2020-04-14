module Model exposing (..)

import Debug
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.TensionOrderable as TensionOrderable
import Fractal.Enum.TensionType as TensionType
import Fractal.InputObject as Input
import Fractal.Object
import Fractal.Object.Label
import Fractal.Object.Tension
import Fractal.Query as Query
import Fractal.ScalarCodecs
import GqlClient exposing (..)
import Graphql.Http
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import RemoteData exposing (RemoteData)



{-
   Frontend Data structure
-}


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
    { id : Fractal.ScalarCodecs.Id
    , nameid : String
    , name : String
    , type_ : NodeType.NodeType
    }


type alias Label =
    { name : String }


type alias Tension =
    { id : Fractal.ScalarCodecs.Id
    , title : String
    , type_ : TensionType.TensionType
    , labels : Maybe (List Label)
    , n_comments : Maybe Int

    --, emitter : String
    --, receivers : String
    }



{-
   Data Responses
-}


type alias OrgaResponse =
    Maybe (List (Maybe Node))


type alias OrgaData =
    List Node


type alias TensionsResponse =
    Maybe (List (Maybe Tension))


type alias TensionsData =
    List Tension



{-
   Response Decoder
-}


tensionsResponse : RemoteData (Graphql.Http.Error TensionsResponse) TensionsResponse -> RequestResult ErrorData TensionsData
tensionsResponse response =
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



{-

   Request decoder

-}


fetchTensionsBunch msg =
    --fetchTensionsBunch : Cmd Msg
    makeGQLQuery
        (Query.queryTension
            tensionPgFilter
            tensionPgPayload
        )
        (RemoteData.fromResult >> tensionsResponse >> msg)


tensionPgFilter : Query.QueryTensionOptionalArguments -> Query.QueryTensionOptionalArguments
tensionPgFilter a =
    { a
        | first = OptionalArgument.Present 10
        , order =
            OptionalArgument.Present
                (Input.buildTensionOrder
                    (\b ->
                        { b
                            | desc = OptionalArgument.Present TensionOrderable.CreatedAt
                        }
                    )
                )
    }


tensionPgPayload : SelectionSet Tension Fractal.Object.Tension
tensionPgPayload =
    SelectionSet.succeed Tension
        |> with Fractal.Object.Tension.id
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



{-
   Utils decoder
-}


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
