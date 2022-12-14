-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Object.BlobAggregateResult exposing (..)

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


count : SelectionSet (Maybe Int) Fractal.Object.BlobAggregateResult
count =
    Object.selectionForField "(Maybe Int)" "count" [] (Decode.int |> Decode.nullable)


createdAtMin : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.BlobAggregateResult
createdAtMin =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "createdAtMin" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


createdAtMax : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.BlobAggregateResult
createdAtMax =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "createdAtMax" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


updatedAtMin : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.BlobAggregateResult
updatedAtMin =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "updatedAtMin" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


updatedAtMax : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.BlobAggregateResult
updatedAtMax =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "updatedAtMax" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


messageMin : SelectionSet (Maybe String) Fractal.Object.BlobAggregateResult
messageMin =
    Object.selectionForField "(Maybe String)" "messageMin" [] (Decode.string |> Decode.nullable)


messageMax : SelectionSet (Maybe String) Fractal.Object.BlobAggregateResult
messageMax =
    Object.selectionForField "(Maybe String)" "messageMax" [] (Decode.string |> Decode.nullable)


pushedFlagMin : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.BlobAggregateResult
pushedFlagMin =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "pushedFlagMin" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


pushedFlagMax : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.BlobAggregateResult
pushedFlagMax =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "pushedFlagMax" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


archivedFlagMin : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.BlobAggregateResult
archivedFlagMin =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "archivedFlagMin" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


archivedFlagMax : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.BlobAggregateResult
archivedFlagMax =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "archivedFlagMax" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


mdMin : SelectionSet (Maybe String) Fractal.Object.BlobAggregateResult
mdMin =
    Object.selectionForField "(Maybe String)" "mdMin" [] (Decode.string |> Decode.nullable)


mdMax : SelectionSet (Maybe String) Fractal.Object.BlobAggregateResult
mdMax =
    Object.selectionForField "(Maybe String)" "mdMax" [] (Decode.string |> Decode.nullable)
