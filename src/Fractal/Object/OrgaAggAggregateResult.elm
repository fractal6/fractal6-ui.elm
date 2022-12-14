-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Object.OrgaAggAggregateResult exposing (..)

import Fractal.InputObject
import Fractal.Interface
import Fractal.Object
import Fractal.Scalar
import Fractal.ScalarCodecs
import Fractal.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


count : SelectionSet (Maybe Int) Fractal.Object.OrgaAggAggregateResult
count =
    Object.selectionForField "(Maybe Int)" "count" [] (Decode.int |> Decode.nullable)


n_membersMin : SelectionSet (Maybe Int) Fractal.Object.OrgaAggAggregateResult
n_membersMin =
    Object.selectionForField "(Maybe Int)" "n_membersMin" [] (Decode.int |> Decode.nullable)


n_membersMax : SelectionSet (Maybe Int) Fractal.Object.OrgaAggAggregateResult
n_membersMax =
    Object.selectionForField "(Maybe Int)" "n_membersMax" [] (Decode.int |> Decode.nullable)


n_membersSum : SelectionSet (Maybe Int) Fractal.Object.OrgaAggAggregateResult
n_membersSum =
    Object.selectionForField "(Maybe Int)" "n_membersSum" [] (Decode.int |> Decode.nullable)


n_membersAvg : SelectionSet (Maybe Float) Fractal.Object.OrgaAggAggregateResult
n_membersAvg =
    Object.selectionForField "(Maybe Float)" "n_membersAvg" [] (Decode.float |> Decode.nullable)


n_guestsMin : SelectionSet (Maybe Int) Fractal.Object.OrgaAggAggregateResult
n_guestsMin =
    Object.selectionForField "(Maybe Int)" "n_guestsMin" [] (Decode.int |> Decode.nullable)


n_guestsMax : SelectionSet (Maybe Int) Fractal.Object.OrgaAggAggregateResult
n_guestsMax =
    Object.selectionForField "(Maybe Int)" "n_guestsMax" [] (Decode.int |> Decode.nullable)


n_guestsSum : SelectionSet (Maybe Int) Fractal.Object.OrgaAggAggregateResult
n_guestsSum =
    Object.selectionForField "(Maybe Int)" "n_guestsSum" [] (Decode.int |> Decode.nullable)


n_guestsAvg : SelectionSet (Maybe Float) Fractal.Object.OrgaAggAggregateResult
n_guestsAvg =
    Object.selectionForField "(Maybe Float)" "n_guestsAvg" [] (Decode.float |> Decode.nullable)
