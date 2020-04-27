module ModelOrg exposing (..)

import Debug
import Dict exposing (Dict)
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
import Iso8601 exposing (fromTime)
import RemoteData exposing (RemoteData)



--
-- Constants
--


nLabelPerTension : Int
nLabelPerTension =
    3


nTensionPpg : Int
nTensionPpg =
    15



--
-- Frontend Data structure
--


type alias ErrorData =
    String


type RequestResult errors data
    = Success data
    | Failure errors
    | Loading -- we don't make the difference between RemoteLoading and Loading here
    | LoadingSlowly
    | NotAsked


type alias Post =
    Dict String String



{-
   Schema Data Structure (fetch/Query)
-}
-- Node interface


type alias Node =
    { id : String
    , name : String
    , nameid : String
    , parent : Maybe ParentNode -- see issue with recursive structure
    , type_ : NodeType.NodeType
    }


type alias ParentNode =
    { id : String }



--


type alias NodesData =
    List Node


type alias NodesResponse =
    Maybe (List (Maybe Node))



--- Tension interface


type alias NodeTensions =
    { tensions_in : Maybe (List Tension)
    , tensions_out : Maybe (List Tension)
    }


type alias Tension =
    { id : String
    , title : String
    , type_ : TensionType.TensionType
    , labels : Maybe (List Label)
    , emitter : Emitter
    , receiver : Receiver
    , createdAt : String
    , n_comments : Maybe Int
    }


type alias Label =
    { name : String }


type alias Emitter =
    { name : String
    , nameid : String
    }


type alias Receiver =
    { name : String
    , nameid : String
    }



--


type alias TensionsData =
    List Tension


type alias TensionsResponse =
    Maybe (List (Maybe Tension))



--
-- Query decoder
--
{-
   Query Organisation Nodes
-}


nodeOrgaFilter : String -> Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
nodeOrgaFilter rootid a =
    { a
        | filter =
            OptionalArgument.Present
                (Input.buildNodeFilter
                    (\b ->
                        { b | rootnameid = OptionalArgument.Present { eq = OptionalArgument.Present rootid } }
                    )
                )
    }


nodeOrgaPayload : SelectionSet Node Fractal.Object.Node
nodeOrgaPayload =
    SelectionSet.succeed Node
        |> with (Fractal.Object.Node.id |> SelectionSet.map decodedId)
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with
            (Fractal.Object.Node.parent identity (SelectionSet.map (ParentNode << decodedId) Fractal.Object.Node.id))
        |> with Fractal.Object.Node.type_


fetchNodesOrga rootid msg =
    makeGQLQuery
        (Query.queryNode
            (nodeOrgaFilter rootid)
            nodeOrgaPayload
        )
        (RemoteData.fromResult >> decodeResponse queryDecoder >> msg)



{-
   Query Circle Tension
-}


circleTensionFilter : String -> Query.GetNodeOptionalArguments -> Query.GetNodeOptionalArguments
circleTensionFilter nid a =
    { a | nameid = OptionalArgument.Present nid }


tensionPgFilter : Fractal.Object.Node.TensionsInOptionalArguments -> Fractal.Object.Node.TensionsInOptionalArguments
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
        |> with
            (Fractal.Object.Tension.emitter identity
                (SelectionSet.succeed Emitter
                    |> with Fractal.Object.Node.name
                    |> with Fractal.Object.Node.nameid
                )
            )
        |> with
            (Fractal.Object.Tension.receiver identity
                (SelectionSet.succeed Receiver
                    |> with Fractal.Object.Node.name
                    |> with Fractal.Object.Node.nameid
                )
            )
        |> with (Fractal.Object.Tension.createdAt |> SelectionSet.map decodedTime)
        |> with Fractal.Object.Tension.n_comments


circleTensionPayload : SelectionSet NodeTensions Fractal.Object.Node
circleTensionPayload =
    SelectionSet.succeed NodeTensions
        |> with
            (Fractal.Object.Node.tensions_in tensionPgFilter tensionPgPayload)
        |> with (Fractal.Object.Node.tensions_out tensionPgFilter tensionPgPayload)


fetchCircleTension targetid msg =
    makeGQLQuery
        (Query.getNode
            (circleTensionFilter targetid)
            circleTensionPayload
        )
        (RemoteData.fromResult >> decodeResponse circleTensionDecoder >> msg)



{-
   Query Tension Page
-}
--tensionPgFilter : Query.QueryTensionOptionalArguments -> Query.QueryTensionOptionalArguments
--tensionPgFilter a =
--    { a
--        | first = OptionalArgument.Present nTensionPpg
--        , order =
--            OptionalArgument.Present
--                (Input.buildTensionOrder
--                    (\b ->
--                        { b | desc = OptionalArgument.Present TensionOrderable.CreatedAt }
--                    )
--                )
--    }
--
--
--tensionPgPayload : SelectionSet Tension Fractal.Object.Tension
--tensionPgPayload =
--    SelectionSet.succeed Tension
--        |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
--        |> with Fractal.Object.Tension.title
--        |> with Fractal.Object.Tension.type_
--        |> with
--            (Fractal.Object.Tension.labels
--                (\args -> { args | first = OptionalArgument.Present nLabelPerTension })
--                (SelectionSet.succeed Label
--                    |> with Fractal.Object.Label.name
--                )
--            )
--        |> with Fractal.Object.Tension.n_comments
--
--
--fetchTensionsPg msg =
--    makeGQLQuery
--        (Query.queryTension
--            tensionPgFilter
--            tensionPgPayload
--        )
--        (RemoteData.fromResult >> decodeResponse queryDecoder >> msg)
--
-- Mutation decoder
--


type alias IdPayload =
    { id : String }


type alias AddTensionPayload =
    { tension : Maybe (List (Maybe IdPayload)) }


addOneTension source target tension msg =
    makeGQLMutation
        (Mutation.addTension
            (tensionInputEncoder source target tension)
            (SelectionSet.map AddTensionPayload <|
                Fractal.Object.AddTensionPayload.tension identity <|
                    (SelectionSet.succeed IdPayload
                        |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
                    )
            )
        )
        (RemoteData.fromResult >> decodeResponse mutationDecoder >> msg)


assertString x =
    case x of
        Just y ->
            y

        Nothing ->
            ""


assertTensionType x =
    case x of
        Just y ->
            y

        Nothing ->
            TensionType.Operational


tensionInputEncoder : Node -> Node -> Post -> Mutation.AddTensionRequiredArguments
tensionInputEncoder source target post =
    let
        time =
            Dict.get "createdAt" post |> assertString

        title =
            Dict.get "title" post |> assertString

        type_ =
            Dict.get "type_" post |> assertString |> TensionType.fromString |> assertTensionType

        createdby =
            Dict.get "username" post |> assertString

        emitterid =
            source.nameid

        receiverid =
            target.nameid

        tensionRequired =
            { createdAt = Fractal.Scalar.DateTime time
            , createdBy =
                Input.buildUserRef
                    (\x -> { x | username = OptionalArgument.Present createdby })
            , title = title
            , type_ = type_
            , emitter =
                Input.buildNodeRef
                    (\x -> { x | nameid = OptionalArgument.Present emitterid })
            , receiver =
                Input.buildNodeRef
                    (\x -> { x | nameid = OptionalArgument.Present receiverid })
            }
    in
    { input =
        [ Input.buildAddTensionInput tensionRequired identity ]
    }



--
-- Response decoder
--


decodeResponse decoder response =
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
            Loading

        RemoteData.NotAsked ->
            NotAsked

        RemoteData.Success data ->
            decoder data |> Success


circleTensionDecoder : Maybe NodeTensions -> List Tension
circleTensionDecoder data =
    case data of
        Just node ->
            let
                tin =
                    case node.tensions_in of
                        Just a ->
                            a

                        Nothing ->
                            []

                tout =
                    case node.tensions_out of
                        Just a ->
                            a

                        Nothing ->
                            []
            in
            List.sortBy .createdAt (tin ++ List.filter (\x -> x.emitter.nameid /= x.receiver.nameid) tout) |> List.reverse

        Nothing ->
            []


queryDecoder : Maybe (List (Maybe a)) -> List a
queryDecoder data =
    -- Convert empty data to empty list
    case data of
        Just d ->
            if List.length d == 0 then
                []

            else
                List.filterMap identity d

        Nothing ->
            []


mutationDecoder : Maybe a -> Maybe a
mutationDecoder =
    identity



--
-- Utils
--


decodedId : Fractal.ScalarCodecs.Id -> String
decodedId (Fractal.Scalar.Id id) =
    id


decodedTime : Fractal.ScalarCodecs.DateTime -> String
decodedTime (Fractal.Scalar.DateTime time) =
    time
