-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Object.Notif exposing (..)

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


tension_ :
    (TensionOptionalArguments -> TensionOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Tension
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Notif
tension_ fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeTensionFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "tension_" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias ContractOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.ContractFilter }


contract :
    (ContractOptionalArguments -> ContractOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Contract
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Notif
contract fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeContractFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "contract" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


link : SelectionSet (Maybe String) Fractal.Object.Notif
link =
    Object.selectionForField "(Maybe String)" "link" [] (Decode.string |> Decode.nullable)


id : SelectionSet Fractal.ScalarCodecs.Id Fractal.Object.Notif
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecId |> .decoder)


type alias CreatedByOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.UserFilter }


createdBy :
    (CreatedByOptionalArguments -> CreatedByOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.User
    -> SelectionSet decodesTo Fractal.Object.Notif
createdBy fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeUserFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "createdBy" optionalArgs____ object____ Basics.identity


createdAt : SelectionSet Fractal.ScalarCodecs.DateTime Fractal.Object.Notif
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


updatedAt : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.Notif
updatedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "updatedAt" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


message : SelectionSet (Maybe String) Fractal.Object.Notif
message =
    Object.selectionForField "(Maybe String)" "message" [] (Decode.string |> Decode.nullable)
