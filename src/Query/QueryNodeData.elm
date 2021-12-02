module Query.QueryNodeData exposing (mandatePayload, queryNodeData)

import Dict exposing (Dict)
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Fractal.InputObject as Input
import Fractal.Object
import Fractal.Object.AddNodePayload
import Fractal.Object.Mandate
import Fractal.Object.Node
import Fractal.Object.Tension
import Fractal.Object.User
import Fractal.Query as Query
import Fractal.Scalar
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Query.QueryNode exposing (nidFilter, nodeDecoder, nodeOrgaPayload)
import RemoteData exposing (RemoteData)



{-
   Query the node data /about, mandate, etc)
-}


queryNodeData url nameid msg =
    makeGQLQuery url
        (Query.getNode
            (nidFilter nameid)
            nodeDataPayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


nodeDataPayload : SelectionSet NodeData Fractal.Object.Node
nodeDataPayload =
    SelectionSet.succeed NodeData
        |> with Fractal.Object.Node.about
        |> with
            (Fractal.Object.Node.mandate identity mandatePayload)


mandatePayload : SelectionSet Mandate Fractal.Object.Mandate
mandatePayload =
    SelectionSet.succeed Mandate
        |> with Fractal.Object.Mandate.purpose
        |> with Fractal.Object.Mandate.responsabilities
        |> with Fractal.Object.Mandate.domains
        |> with Fractal.Object.Mandate.policies
