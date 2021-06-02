-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Object.DeleteSharedNodePayload exposing (..)

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


type alias SharedNodeOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.SharedNodeFilter
    , order : OptionalArgument Fractal.InputObject.SharedNodeOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


sharedNode :
    (SharedNodeOptionalArguments -> SharedNodeOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.SharedNode
    -> SelectionSet (Maybe (List (Maybe decodesTo))) Fractal.Object.DeleteSharedNodePayload
sharedNode fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs =
            [ Argument.optional "filter" filledInOptionals.filter Fractal.InputObject.encodeSharedNodeFilter, Argument.optional "order" filledInOptionals.order Fractal.InputObject.encodeSharedNodeOrder, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "sharedNode" optionalArgs object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


msg : SelectionSet (Maybe String) Fractal.Object.DeleteSharedNodePayload
msg =
    Object.selectionForField "(Maybe String)" "msg" [] (Decode.string |> Decode.nullable)


numUids : SelectionSet (Maybe Int) Fractal.Object.DeleteSharedNodePayload
numUids =
    Object.selectionForField "(Maybe Int)" "numUids" [] (Decode.int |> Decode.nullable)