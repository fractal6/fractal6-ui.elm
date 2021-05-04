-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Object.Event exposing (..)

import Fractal.Enum.TensionEvent
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


type alias TensionOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.TensionFilter }


tension :
    (TensionOptionalArguments -> TensionOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Tension
    -> SelectionSet decodesTo Fractal.Object.Event
tension fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { filter = Absent }

        optionalArgs =
            [ Argument.optional "filter" filledInOptionals.filter Fractal.InputObject.encodeTensionFilter ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "tension" optionalArgs object_ identity


event_type : SelectionSet Fractal.Enum.TensionEvent.TensionEvent Fractal.Object.Event
event_type =
    Object.selectionForField "Enum.TensionEvent.TensionEvent" "event_type" [] Fractal.Enum.TensionEvent.decoder


old : SelectionSet (Maybe String) Fractal.Object.Event
old =
    Object.selectionForField "(Maybe String)" "old" [] (Decode.string |> Decode.nullable)


new : SelectionSet (Maybe String) Fractal.Object.Event
new =
    Object.selectionForField "(Maybe String)" "new" [] (Decode.string |> Decode.nullable)


id : SelectionSet Fractal.ScalarCodecs.Id Fractal.Object.Event
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecId |> .decoder)


type alias CreatedByOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.UserFilter }


createdBy :
    (CreatedByOptionalArguments -> CreatedByOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.User
    -> SelectionSet decodesTo Fractal.Object.Event
createdBy fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { filter = Absent }

        optionalArgs =
            [ Argument.optional "filter" filledInOptionals.filter Fractal.InputObject.encodeUserFilter ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "createdBy" optionalArgs object_ identity


createdAt : SelectionSet Fractal.ScalarCodecs.DateTime Fractal.Object.Event
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


updatedAt : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.Event
updatedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "updatedAt" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


message : SelectionSet (Maybe String) Fractal.Object.Event
message =
    Object.selectionForField "(Maybe String)" "message" [] (Decode.string |> Decode.nullable)
