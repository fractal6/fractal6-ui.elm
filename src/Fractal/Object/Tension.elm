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


nth : SelectionSet (Maybe String) Fractal.Object.Tension
nth =
    Object.selectionForField "(Maybe String)" "nth" [] (Decode.string |> Decode.nullable)


title : SelectionSet String Fractal.Object.Tension
title =
    Object.selectionForField "String" "title" [] Decode.string


type_ : SelectionSet Fractal.Enum.TensionType.TensionType Fractal.Object.Tension
type_ =
    Object.selectionForField "Enum.TensionType.TensionType" "type_" [] Fractal.Enum.TensionType.decoder


type alias EmitterOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.NodeFilter }


emitter : (EmitterOptionalArguments -> EmitterOptionalArguments) -> SelectionSet decodesTo Fractal.Object.Node -> SelectionSet decodesTo Fractal.Object.Tension
emitter fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { filter = Absent }

        optionalArgs =
            [ Argument.optional "filter" filledInOptionals.filter Fractal.InputObject.encodeNodeFilter ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "emitter" optionalArgs object_ identity


type alias ReceiverOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.NodeFilter }


receiver : (ReceiverOptionalArguments -> ReceiverOptionalArguments) -> SelectionSet decodesTo Fractal.Object.Node -> SelectionSet decodesTo Fractal.Object.Tension
receiver fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { filter = Absent }

        optionalArgs =
            [ Argument.optional "filter" filledInOptionals.filter Fractal.InputObject.encodeNodeFilter ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "receiver" optionalArgs object_ identity


type alias CommentsOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.CommentFilter
    , order : OptionalArgument Fractal.InputObject.CommentOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


comments : (CommentsOptionalArguments -> CommentsOptionalArguments) -> SelectionSet decodesTo Fractal.Object.Comment -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Tension
comments fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs =
            [ Argument.optional "filter" filledInOptionals.filter Fractal.InputObject.encodeCommentFilter, Argument.optional "order" filledInOptionals.order Fractal.InputObject.encodeCommentOrder, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "comments" optionalArgs object_ (identity >> Decode.list >> Decode.nullable)


type alias LabelsOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.LabelFilter
    , order : OptionalArgument Fractal.InputObject.LabelOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


labels : (LabelsOptionalArguments -> LabelsOptionalArguments) -> SelectionSet decodesTo Fractal.Object.Label -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Tension
labels fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs =
            [ Argument.optional "filter" filledInOptionals.filter Fractal.InputObject.encodeLabelFilter, Argument.optional "order" filledInOptionals.order Fractal.InputObject.encodeLabelOrder, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "labels" optionalArgs object_ (identity >> Decode.list >> Decode.nullable)


n_comments : SelectionSet (Maybe Int) Fractal.Object.Tension
n_comments =
    Object.selectionForField "(Maybe Int)" "n_comments" [] (Decode.int |> Decode.nullable)


id : SelectionSet Fractal.ScalarCodecs.Id Fractal.Object.Tension
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecId |> .decoder)


createdAt : SelectionSet Fractal.ScalarCodecs.DateTime Fractal.Object.Tension
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


type alias CreatedByOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.UserFilter }


createdBy : (CreatedByOptionalArguments -> CreatedByOptionalArguments) -> SelectionSet decodesTo Fractal.Object.User -> SelectionSet decodesTo Fractal.Object.Tension
createdBy fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { filter = Absent }

        optionalArgs =
            [ Argument.optional "filter" filledInOptionals.filter Fractal.InputObject.encodeUserFilter ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "createdBy" optionalArgs object_ identity


message : SelectionSet (Maybe String) Fractal.Object.Tension
message =
    Object.selectionForField "(Maybe String)" "message" [] (Decode.string |> Decode.nullable)


items : SelectionSet (Maybe (List String)) Fractal.Object.Tension
items =
    Object.selectionForField "(Maybe (List String))" "items" [] (Decode.string |> Decode.list |> Decode.nullable)
