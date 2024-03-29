-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Object.NodeFragmentAggregateResult exposing (..)

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


count : SelectionSet (Maybe Int) Fractal.Object.NodeFragmentAggregateResult
count =
    Object.selectionForField "(Maybe Int)" "count" [] (Decode.int |> Decode.nullable)


nameidMin : SelectionSet (Maybe String) Fractal.Object.NodeFragmentAggregateResult
nameidMin =
    Object.selectionForField "(Maybe String)" "nameidMin" [] (Decode.string |> Decode.nullable)


nameidMax : SelectionSet (Maybe String) Fractal.Object.NodeFragmentAggregateResult
nameidMax =
    Object.selectionForField "(Maybe String)" "nameidMax" [] (Decode.string |> Decode.nullable)


nameMin : SelectionSet (Maybe String) Fractal.Object.NodeFragmentAggregateResult
nameMin =
    Object.selectionForField "(Maybe String)" "nameMin" [] (Decode.string |> Decode.nullable)


nameMax : SelectionSet (Maybe String) Fractal.Object.NodeFragmentAggregateResult
nameMax =
    Object.selectionForField "(Maybe String)" "nameMax" [] (Decode.string |> Decode.nullable)


aboutMin : SelectionSet (Maybe String) Fractal.Object.NodeFragmentAggregateResult
aboutMin =
    Object.selectionForField "(Maybe String)" "aboutMin" [] (Decode.string |> Decode.nullable)


aboutMax : SelectionSet (Maybe String) Fractal.Object.NodeFragmentAggregateResult
aboutMax =
    Object.selectionForField "(Maybe String)" "aboutMax" [] (Decode.string |> Decode.nullable)


first_linkMin : SelectionSet (Maybe String) Fractal.Object.NodeFragmentAggregateResult
first_linkMin =
    Object.selectionForField "(Maybe String)" "first_linkMin" [] (Decode.string |> Decode.nullable)


first_linkMax : SelectionSet (Maybe String) Fractal.Object.NodeFragmentAggregateResult
first_linkMax =
    Object.selectionForField "(Maybe String)" "first_linkMax" [] (Decode.string |> Decode.nullable)


role_extMin : SelectionSet (Maybe String) Fractal.Object.NodeFragmentAggregateResult
role_extMin =
    Object.selectionForField "(Maybe String)" "role_extMin" [] (Decode.string |> Decode.nullable)


role_extMax : SelectionSet (Maybe String) Fractal.Object.NodeFragmentAggregateResult
role_extMax =
    Object.selectionForField "(Maybe String)" "role_extMax" [] (Decode.string |> Decode.nullable)


colorMin : SelectionSet (Maybe String) Fractal.Object.NodeFragmentAggregateResult
colorMin =
    Object.selectionForField "(Maybe String)" "colorMin" [] (Decode.string |> Decode.nullable)


colorMax : SelectionSet (Maybe String) Fractal.Object.NodeFragmentAggregateResult
colorMax =
    Object.selectionForField "(Maybe String)" "colorMax" [] (Decode.string |> Decode.nullable)
