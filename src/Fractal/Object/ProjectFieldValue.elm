-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Object.ProjectFieldValue exposing (..)

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


type alias FieldOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.ProjectFieldFilter }


field :
    (FieldOptionalArguments -> FieldOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.ProjectField
    -> SelectionSet decodesTo Fractal.Object.ProjectFieldValue
field fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeProjectFieldFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "field" optionalArgs____ object____ Basics.identity


value : SelectionSet String Fractal.Object.ProjectFieldValue
value =
    Object.selectionForField "String" "value" [] Decode.string
