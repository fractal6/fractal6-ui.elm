-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Object.DeletePostPayload exposing (..)

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


type alias PostOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.PostFilter
    , order : OptionalArgument Fractal.InputObject.PostOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


post :
    (PostOptionalArguments -> PostOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Post
    -> SelectionSet (Maybe (List (Maybe decodesTo))) Fractal.Object.DeletePostPayload
post fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs =
            [ Argument.optional "filter" filledInOptionals.filter Fractal.InputObject.encodePostFilter, Argument.optional "order" filledInOptionals.order Fractal.InputObject.encodePostOrder, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "post" optionalArgs object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


msg : SelectionSet (Maybe String) Fractal.Object.DeletePostPayload
msg =
    Object.selectionForField "(Maybe String)" "msg" [] (Decode.string |> Decode.nullable)


numUids : SelectionSet (Maybe Int) Fractal.Object.DeletePostPayload
numUids =
    Object.selectionForField "(Maybe Int)" "numUids" [] (Decode.int |> Decode.nullable)
