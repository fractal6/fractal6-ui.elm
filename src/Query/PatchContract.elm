module Query.PatchContract exposing (Todo)

import Dict exposing (Dict)
import Extra exposing (ternary)
import Fractal.Enum.ContractStatus as ContractStatus
import Fractal.Enum.ContractType as ContractType
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.Contract
import Fractal.Object.DeleteContractPayload
import Fractal.Query as Query
import Fractal.Scalar
import Fractal.ScalarCodecs
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Maybe exposing (withDefault)
import ModelCommon exposing (TensionForm, UserForm)
import ModelSchema exposing (..)
import Query.QueryContract exposing (contractPayload)
import Query.QueryNode exposing (tidPayload)
import RemoteData exposing (RemoteData)



{-
   update contract
-}


type alias Todo =
    String
