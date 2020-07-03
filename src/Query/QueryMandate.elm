module Query.QueryMandate exposing (mandatePayload, queryMandate)

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
import Fractal.Object.NodeCharac
import Fractal.Object.User
import Fractal.Query as Query
import Fractal.Scalar
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Query.QueryNode exposing (nodeOrgaPayload)
import RemoteData exposing (RemoteData)



{-
   Query the mandate of a node
-}


type alias TopMandate =
    { nameid : String
    , mandate : Maybe Mandate
    }


mandateDecoder : Maybe TopMandate -> Maybe Mandate
mandateDecoder a =
    a
        |> Maybe.map (\n -> n.mandate)
        |> withDefault Nothing


queryMandate nameid msg =
    makeGQLQuery
        (Query.getNode
            (mandateFilter nameid)
            nodeMandatePayload
        )
        (RemoteData.fromResult >> decodeResponse mandateDecoder >> msg)


mandateFilter : String -> Query.GetNodeOptionalArguments -> Query.GetNodeOptionalArguments
mandateFilter nid a =
    { a | nameid = Present nid }


nodeMandatePayload : SelectionSet TopMandate Fractal.Object.Node
nodeMandatePayload =
    SelectionSet.succeed TopMandate
        |> with Fractal.Object.Node.nameid
        |> with
            (Fractal.Object.Node.mandate identity mandatePayload)


mandatePayload : SelectionSet Mandate Fractal.Object.Mandate
mandatePayload =
    SelectionSet.succeed Mandate
        |> with Fractal.Object.Mandate.about
        |> with Fractal.Object.Mandate.purpose
        |> with Fractal.Object.Mandate.responsabilities
        |> with Fractal.Object.Mandate.domains
        |> with Fractal.Object.Mandate.policies
