-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Object.ProjectColumn exposing (..)

import Fractal.Enum.ProjectColumnType
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


id : SelectionSet Fractal.ScalarCodecs.Id Fractal.Object.ProjectColumn
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecId |> .decoder)


name : SelectionSet String Fractal.Object.ProjectColumn
name =
    Object.selectionForField "String" "name" [] Decode.string


description : SelectionSet (Maybe String) Fractal.Object.ProjectColumn
description =
    Object.selectionForField "(Maybe String)" "description" [] (Decode.string |> Decode.nullable)


color : SelectionSet (Maybe String) Fractal.Object.ProjectColumn
color =
    Object.selectionForField "(Maybe String)" "color" [] (Decode.string |> Decode.nullable)


pos : SelectionSet Int Fractal.Object.ProjectColumn
pos =
    Object.selectionForField "Int" "pos" [] Decode.int


col_type : SelectionSet Fractal.Enum.ProjectColumnType.ProjectColumnType Fractal.Object.ProjectColumn
col_type =
    Object.selectionForField "Enum.ProjectColumnType.ProjectColumnType" "col_type" [] Fractal.Enum.ProjectColumnType.decoder


type alias CardsOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.ProjectCardFilter
    , order : OptionalArgument Fractal.InputObject.ProjectCardOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


cards :
    (CardsOptionalArguments -> CardsOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.ProjectCard
    -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.ProjectColumn
cards fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeProjectCardFilter, Argument.optional "order" filledInOptionals____.order Fractal.InputObject.encodeProjectCardOrder, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "cards" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


type alias ProjectOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.ProjectFilter }


project :
    (ProjectOptionalArguments -> ProjectOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Project
    -> SelectionSet decodesTo Fractal.Object.ProjectColumn
project fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeProjectFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "project" optionalArgs____ object____ Basics.identity


type alias TensionsOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.TensionFilter
    , order : OptionalArgument Fractal.InputObject.TensionOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


tensions :
    (TensionsOptionalArguments -> TensionsOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Tension
    -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.ProjectColumn
tensions fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeTensionFilter, Argument.optional "order" filledInOptionals____.order Fractal.InputObject.encodeTensionOrder, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "tensions" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


type alias DraftsOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.ProjectDraftFilter
    , order : OptionalArgument Fractal.InputObject.ProjectDraftOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


drafts :
    (DraftsOptionalArguments -> DraftsOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.ProjectDraft
    -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.ProjectColumn
drafts fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeProjectDraftFilter, Argument.optional "order" filledInOptionals____.order Fractal.InputObject.encodeProjectDraftOrder, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "drafts" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


type alias CardsAggregateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.ProjectCardFilter }


cardsAggregate :
    (CardsAggregateOptionalArguments -> CardsAggregateOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.ProjectCardAggregateResult
    -> SelectionSet (Maybe decodesTo) Fractal.Object.ProjectColumn
cardsAggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeProjectCardFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "cardsAggregate" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias TensionsAggregateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.TensionFilter }


tensionsAggregate :
    (TensionsAggregateOptionalArguments -> TensionsAggregateOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.TensionAggregateResult
    -> SelectionSet (Maybe decodesTo) Fractal.Object.ProjectColumn
tensionsAggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeTensionFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "tensionsAggregate" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias DraftsAggregateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.ProjectDraftFilter }


draftsAggregate :
    (DraftsAggregateOptionalArguments -> DraftsAggregateOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.ProjectDraftAggregateResult
    -> SelectionSet (Maybe decodesTo) Fractal.Object.ProjectColumn
draftsAggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeProjectDraftFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "draftsAggregate" optionalArgs____ object____ (Basics.identity >> Decode.nullable)
