-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Object.Tension exposing (..)

import Fractal.Enum.TensionType
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


{-| -}
nth : SelectionSet (Maybe Int) Fractal.Object.Tension
nth =
    Object.selectionForField "(Maybe Int)" "nth" [] (Decode.int |> Decode.nullable)


{-| -}
type_ : SelectionSet Fractal.Enum.TensionType.TensionType Fractal.Object.Tension
type_ =
    Object.selectionForField "Enum.TensionType.TensionType" "type_" [] Fractal.Enum.TensionType.decoder


{-| -}
emitter : SelectionSet decodesTo Fractal.Interface.Node -> SelectionSet (Maybe decodesTo) Fractal.Object.Tension
emitter object_ =
    Object.selectionForCompositeField "emitter" [] object_ (identity >> Decode.nullable)


{-| -}
receivers : SelectionSet decodesTo Fractal.Interface.Node -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Tension
receivers object_ =
    Object.selectionForCompositeField "receivers" [] object_ (identity >> Decode.list >> Decode.nullable)


{-| -}
severity : SelectionSet (Maybe Int) Fractal.Object.Tension
severity =
    Object.selectionForField "(Maybe Int)" "severity" [] (Decode.int |> Decode.nullable)


{-| -}
isAnonymous : SelectionSet (Maybe Bool) Fractal.Object.Tension
isAnonymous =
    Object.selectionForField "(Maybe Bool)" "isAnonymous" [] (Decode.bool |> Decode.nullable)


{-| -}
id : SelectionSet Fractal.ScalarCodecs.Id Fractal.Object.Tension
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| -}
title : SelectionSet String Fractal.Object.Tension
title =
    Object.selectionForField "String" "title" [] Decode.string


{-| -}
message : SelectionSet (Maybe String) Fractal.Object.Tension
message =
    Object.selectionForField "(Maybe String)" "message" [] (Decode.string |> Decode.nullable)


{-| -}
createdAt : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.Tension
createdAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "createdAt" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)
