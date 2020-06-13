module Query.QueryTension exposing (queryCircleTension, queryPageTension, tensionPgPayload)

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


nCircleTensionPpg : Int
nCircleTensionPpg =
    15


type alias NodeTensions =
    { tensions_in : Maybe (List Tension)
    , tensions_out : Maybe (List Tension)
    , children : Maybe (List SubNodeTensions)
    }


type alias SubNodeTensions =
    { tensions_in : Maybe (List Tension)
    , tensions_out : Maybe (List Tension)
    }



-- Response Decoder


circleTensionDecoder : Maybe NodeTensions -> Maybe (List Tension)
circleTensionDecoder data =
    data
        |> Maybe.map
            (\node ->
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
                    |> List.take nCircleTensionPpg
                    |> Just
            )
        |> Maybe.withDefault Nothing


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


circleTensionPgFilter : Fractal.Object.Node.TensionsInOptionalArguments -> Fractal.Object.Node.TensionsInOptionalArguments
circleTensionPgFilter a =
    { a
        | first = OptionalArgument.Present nCircleTensionPpg

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
        |> with (Fractal.Object.Node.tensions_in circleTensionPgFilter tensionPgPayload)
        |> with (Fractal.Object.Node.tensions_out circleTensionPgFilter tensionPgPayload)
        |> with
            (Fractal.Object.Node.children identity
                (SelectionSet.succeed SubNodeTensions
                    |> with (Fractal.Object.Node.tensions_in circleTensionPgFilter tensionPgPayload)
                    |> with (Fractal.Object.Node.tensions_out circleTensionPgFilter tensionPgPayload)
                )
            )


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


nTensionPpg : Int
nTensionPpg =
    25



-- Response decoder


tensionDecoder : Maybe (List (Maybe Tension)) -> Maybe (List Tension)
tensionDecoder data =
    data
        |> Maybe.map
            (\ts ->
                List.filterMap identity ts
            )


queryPageTension targetid msg =
    makeGQLQuery
        (Query.queryTension
            (tensionPgFilter targetid)
            tensionPgPayload
        )
        (RemoteData.fromResult >> decodeResponse tensionDecoder >> msg)


tensionPgFilter : String -> Query.QueryTensionOptionalArguments -> Query.QueryTensionOptionalArguments
tensionPgFilter nid a =
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
