module Query.QueryTension exposing (queryCircleTension, tensionPgPayload)

import Dict exposing (Dict)
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionOrderable as TensionOrderable
import Fractal.Enum.TensionType as TensionType
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.Label
import Fractal.Object.Node
import Fractal.Object.Tension
import Fractal.Object.User
import Fractal.Query as Query
import Fractal.Scalar
import Fractal.ScalarCodecs
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import List.Extra exposing (uniqueBy)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import RemoteData exposing (RemoteData)



{-
   Query Circle Tension
-}


nLabelPerTension : Int
nLabelPerTension =
    3


nTensionPpg : Int
nTensionPpg =
    15


queryCircleTension targetid msg =
    --@DEBUG: Infered type...
    makeGQLQuery
        (Query.getNode
            (circleTensionFilter targetid)
            circleTensionPayload
        )
        (RemoteData.fromResult >> decodeResponse circleTensionDecoder >> msg)


circleTensionFilter : String -> Query.GetNodeOptionalArguments -> Query.GetNodeOptionalArguments
circleTensionFilter nid a =
    { a | nameid = OptionalArgument.Present nid }


tensionPgFilter : Fractal.Object.Node.TensionsInOptionalArguments -> Fractal.Object.Node.TensionsInOptionalArguments
tensionPgFilter a =
    { a
        | first = OptionalArgument.Present nTensionPpg

        -- we reorder it anyway !
        --, order = OptionalArgument.Present (Input.buildTensionOrder (\b -> { b | desc = OptionalArgument.Present TensionOrderable.CreatedAt }))
    }


tensionPgPayload : SelectionSet Tension Fractal.Object.Tension
tensionPgPayload =
    SelectionSet.succeed Tension
        |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Tension.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.Tension.createdBy identity <| SelectionSet.map Username Fractal.Object.User.username)
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
                (SelectionSet.succeed EmitterOrReceiver
                    |> with Fractal.Object.Node.name
                    |> with Fractal.Object.Node.nameid
                    |> with Fractal.Object.Node.type_
                )
            )
        |> with
            (Fractal.Object.Tension.receiver identity
                (SelectionSet.succeed EmitterOrReceiver
                    |> with Fractal.Object.Node.name
                    |> with Fractal.Object.Node.nameid
                    |> with Fractal.Object.Node.type_
                )
            )
        |> with Fractal.Object.Tension.n_comments
        |> with Fractal.Object.Tension.action


circleTensionPayload : SelectionSet NodeTensions Fractal.Object.Node
circleTensionPayload =
    SelectionSet.succeed NodeTensions
        |> with (Fractal.Object.Node.tensions_in tensionPgFilter tensionPgPayload)
        |> with (Fractal.Object.Node.tensions_out tensionPgFilter tensionPgPayload)
        |> with
            (Fractal.Object.Node.children identity
                (SelectionSet.succeed SubNodeTensions
                    |> with (Fractal.Object.Node.tensions_in tensionPgFilter tensionPgPayload)
                    |> with (Fractal.Object.Node.tensions_out tensionPgFilter tensionPgPayload)
                )
            )



{- Response Decoder -}


circleTensionDecoder : Maybe NodeTensions -> List Tension
circleTensionDecoder data =
    case data of
        Just node ->
            let
                tin =
                    node.tensions_in |> withDefault []

                tout =
                    -- Empty for now (automatic tensions ?)
                    node.tensions_out |> withDefault []

                tchild =
                    node.children |> withDefault [] |> List.map subCircleTensionDecoder |> List.concat
            in
            List.sortBy .createdAt (tchild ++ tin ++ List.filter (\t -> t.emitter.nameid /= t.receiver.nameid) tout)
                |> List.reverse
                |> uniqueBy (\t -> t.id)
                |> List.take nTensionPpg

        Nothing ->
            []


subCircleTensionDecoder : SubNodeTensions -> List Tension
subCircleTensionDecoder child =
    let
        tin =
            child.tensions_in |> withDefault []

        tout =
            child.tensions_out |> withDefault []
    in
    tin ++ List.filter (\t -> t.emitter.nameid /= t.receiver.nameid) tout



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
--queryDecoder : Maybe (List (Maybe a)) -> List a
--queryDecoder data =
--    -- Convert empty data to empty list
--    -- Standard decoder to get list of result from a gql query
--    case data of
--        Just d ->
--            if List.length d == 0 then
--                []
--
--            else
--                List.filterMap identity d
--
--        Nothing ->
--            []
