-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Object.EventAggregateResult exposing (..)

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


count : SelectionSet (Maybe Int) Fractal.Object.EventAggregateResult
count =
    Object.selectionForField "(Maybe Int)" "count" [] (Decode.int |> Decode.nullable)


createdAtMin : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.EventAggregateResult
createdAtMin =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "createdAtMin" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


createdAtMax : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.EventAggregateResult
createdAtMax =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "createdAtMax" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


updatedAtMin : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.EventAggregateResult
updatedAtMin =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "updatedAtMin" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


updatedAtMax : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.EventAggregateResult
updatedAtMax =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "updatedAtMax" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


messageMin : SelectionSet (Maybe String) Fractal.Object.EventAggregateResult
messageMin =
    Object.selectionForField "(Maybe String)" "messageMin" [] (Decode.string |> Decode.nullable)


messageMax : SelectionSet (Maybe String) Fractal.Object.EventAggregateResult
messageMax =
    Object.selectionForField "(Maybe String)" "messageMax" [] (Decode.string |> Decode.nullable)


oldMin : SelectionSet (Maybe String) Fractal.Object.EventAggregateResult
oldMin =
    Object.selectionForField "(Maybe String)" "oldMin" [] (Decode.string |> Decode.nullable)


oldMax : SelectionSet (Maybe String) Fractal.Object.EventAggregateResult
oldMax =
    Object.selectionForField "(Maybe String)" "oldMax" [] (Decode.string |> Decode.nullable)


newMin : SelectionSet (Maybe String) Fractal.Object.EventAggregateResult
newMin =
    Object.selectionForField "(Maybe String)" "newMin" [] (Decode.string |> Decode.nullable)


newMax : SelectionSet (Maybe String) Fractal.Object.EventAggregateResult
newMax =
    Object.selectionForField "(Maybe String)" "newMax" [] (Decode.string |> Decode.nullable)
