-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Object.TensionAggregateResult exposing (..)

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


count : SelectionSet (Maybe Int) Fractal.Object.TensionAggregateResult
count =
    Object.selectionForField "(Maybe Int)" "count" [] (Decode.int |> Decode.nullable)


createdAtMin : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.TensionAggregateResult
createdAtMin =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "createdAtMin" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


createdAtMax : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.TensionAggregateResult
createdAtMax =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "createdAtMax" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


updatedAtMin : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.TensionAggregateResult
updatedAtMin =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "updatedAtMin" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


updatedAtMax : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.TensionAggregateResult
updatedAtMax =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "updatedAtMax" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


messageMin : SelectionSet (Maybe String) Fractal.Object.TensionAggregateResult
messageMin =
    Object.selectionForField "(Maybe String)" "messageMin" [] (Decode.string |> Decode.nullable)


messageMax : SelectionSet (Maybe String) Fractal.Object.TensionAggregateResult
messageMax =
    Object.selectionForField "(Maybe String)" "messageMax" [] (Decode.string |> Decode.nullable)


emitteridMin : SelectionSet (Maybe String) Fractal.Object.TensionAggregateResult
emitteridMin =
    Object.selectionForField "(Maybe String)" "emitteridMin" [] (Decode.string |> Decode.nullable)


emitteridMax : SelectionSet (Maybe String) Fractal.Object.TensionAggregateResult
emitteridMax =
    Object.selectionForField "(Maybe String)" "emitteridMax" [] (Decode.string |> Decode.nullable)


receiveridMin : SelectionSet (Maybe String) Fractal.Object.TensionAggregateResult
receiveridMin =
    Object.selectionForField "(Maybe String)" "receiveridMin" [] (Decode.string |> Decode.nullable)


receiveridMax : SelectionSet (Maybe String) Fractal.Object.TensionAggregateResult
receiveridMax =
    Object.selectionForField "(Maybe String)" "receiveridMax" [] (Decode.string |> Decode.nullable)


titleMin : SelectionSet (Maybe String) Fractal.Object.TensionAggregateResult
titleMin =
    Object.selectionForField "(Maybe String)" "titleMin" [] (Decode.string |> Decode.nullable)


titleMax : SelectionSet (Maybe String) Fractal.Object.TensionAggregateResult
titleMax =
    Object.selectionForField "(Maybe String)" "titleMax" [] (Decode.string |> Decode.nullable)


n_commentsMin : SelectionSet (Maybe Int) Fractal.Object.TensionAggregateResult
n_commentsMin =
    Object.selectionForField "(Maybe Int)" "n_commentsMin" [] (Decode.int |> Decode.nullable)


n_commentsMax : SelectionSet (Maybe Int) Fractal.Object.TensionAggregateResult
n_commentsMax =
    Object.selectionForField "(Maybe Int)" "n_commentsMax" [] (Decode.int |> Decode.nullable)


n_commentsSum : SelectionSet (Maybe Int) Fractal.Object.TensionAggregateResult
n_commentsSum =
    Object.selectionForField "(Maybe Int)" "n_commentsSum" [] (Decode.int |> Decode.nullable)


n_commentsAvg : SelectionSet (Maybe Float) Fractal.Object.TensionAggregateResult
n_commentsAvg =
    Object.selectionForField "(Maybe Float)" "n_commentsAvg" [] (Decode.float |> Decode.nullable)
