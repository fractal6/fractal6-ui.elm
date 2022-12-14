-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Object.NodeAggregateResult exposing (..)

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


count : SelectionSet (Maybe Int) Fractal.Object.NodeAggregateResult
count =
    Object.selectionForField "(Maybe Int)" "count" [] (Decode.int |> Decode.nullable)


createdAtMin : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.NodeAggregateResult
createdAtMin =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "createdAtMin" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


createdAtMax : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.NodeAggregateResult
createdAtMax =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "createdAtMax" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


updatedAtMin : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.NodeAggregateResult
updatedAtMin =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "updatedAtMin" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


updatedAtMax : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.NodeAggregateResult
updatedAtMax =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "updatedAtMax" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


nameidMin : SelectionSet (Maybe String) Fractal.Object.NodeAggregateResult
nameidMin =
    Object.selectionForField "(Maybe String)" "nameidMin" [] (Decode.string |> Decode.nullable)


nameidMax : SelectionSet (Maybe String) Fractal.Object.NodeAggregateResult
nameidMax =
    Object.selectionForField "(Maybe String)" "nameidMax" [] (Decode.string |> Decode.nullable)


rootnameidMin : SelectionSet (Maybe String) Fractal.Object.NodeAggregateResult
rootnameidMin =
    Object.selectionForField "(Maybe String)" "rootnameidMin" [] (Decode.string |> Decode.nullable)


rootnameidMax : SelectionSet (Maybe String) Fractal.Object.NodeAggregateResult
rootnameidMax =
    Object.selectionForField "(Maybe String)" "rootnameidMax" [] (Decode.string |> Decode.nullable)


nameMin : SelectionSet (Maybe String) Fractal.Object.NodeAggregateResult
nameMin =
    Object.selectionForField "(Maybe String)" "nameMin" [] (Decode.string |> Decode.nullable)


nameMax : SelectionSet (Maybe String) Fractal.Object.NodeAggregateResult
nameMax =
    Object.selectionForField "(Maybe String)" "nameMax" [] (Decode.string |> Decode.nullable)


aboutMin : SelectionSet (Maybe String) Fractal.Object.NodeAggregateResult
aboutMin =
    Object.selectionForField "(Maybe String)" "aboutMin" [] (Decode.string |> Decode.nullable)


aboutMax : SelectionSet (Maybe String) Fractal.Object.NodeAggregateResult
aboutMax =
    Object.selectionForField "(Maybe String)" "aboutMax" [] (Decode.string |> Decode.nullable)


rightsMin : SelectionSet (Maybe Int) Fractal.Object.NodeAggregateResult
rightsMin =
    Object.selectionForField "(Maybe Int)" "rightsMin" [] (Decode.int |> Decode.nullable)


rightsMax : SelectionSet (Maybe Int) Fractal.Object.NodeAggregateResult
rightsMax =
    Object.selectionForField "(Maybe Int)" "rightsMax" [] (Decode.int |> Decode.nullable)


rightsSum : SelectionSet (Maybe Int) Fractal.Object.NodeAggregateResult
rightsSum =
    Object.selectionForField "(Maybe Int)" "rightsSum" [] (Decode.int |> Decode.nullable)


rightsAvg : SelectionSet (Maybe Float) Fractal.Object.NodeAggregateResult
rightsAvg =
    Object.selectionForField "(Maybe Float)" "rightsAvg" [] (Decode.float |> Decode.nullable)


colorMin : SelectionSet (Maybe String) Fractal.Object.NodeAggregateResult
colorMin =
    Object.selectionForField "(Maybe String)" "colorMin" [] (Decode.string |> Decode.nullable)


colorMax : SelectionSet (Maybe String) Fractal.Object.NodeAggregateResult
colorMax =
    Object.selectionForField "(Maybe String)" "colorMax" [] (Decode.string |> Decode.nullable)
