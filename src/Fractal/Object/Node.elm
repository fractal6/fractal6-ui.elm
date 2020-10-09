-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Object.Node exposing (..)

import Fractal.Enum.NodeType
import Fractal.Enum.RoleType
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


id : SelectionSet Fractal.ScalarCodecs.Id Fractal.Object.Node
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecId |> .decoder)


createdAt : SelectionSet Fractal.ScalarCodecs.DateTime Fractal.Object.Node
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


type alias CreatedByOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.UserFilter }


createdBy : (CreatedByOptionalArguments -> CreatedByOptionalArguments) -> SelectionSet decodesTo Fractal.Object.User -> SelectionSet decodesTo Fractal.Object.Node
createdBy fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { filter = Absent }

        optionalArgs =
            [ Argument.optional "filter" filledInOptionals.filter Fractal.InputObject.encodeUserFilter ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "createdBy" optionalArgs object_ identity


name : SelectionSet String Fractal.Object.Node
name =
    Object.selectionForField "String" "name" [] Decode.string


nameid : SelectionSet String Fractal.Object.Node
nameid =
    Object.selectionForField "String" "nameid" [] Decode.string


rootnameid : SelectionSet String Fractal.Object.Node
rootnameid =
    Object.selectionForField "String" "rootnameid" [] Decode.string


type alias ParentOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.NodeFilter }


parent : (ParentOptionalArguments -> ParentOptionalArguments) -> SelectionSet decodesTo Fractal.Object.Node -> SelectionSet (Maybe decodesTo) Fractal.Object.Node
parent fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { filter = Absent }

        optionalArgs =
            [ Argument.optional "filter" filledInOptionals.filter Fractal.InputObject.encodeNodeFilter ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "parent" optionalArgs object_ (identity >> Decode.nullable)


type alias ChildrenOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.NodeFilter
    , order : OptionalArgument Fractal.InputObject.NodeOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


children : (ChildrenOptionalArguments -> ChildrenOptionalArguments) -> SelectionSet decodesTo Fractal.Object.Node -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Node
children fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs =
            [ Argument.optional "filter" filledInOptionals.filter Fractal.InputObject.encodeNodeFilter, Argument.optional "order" filledInOptionals.order Fractal.InputObject.encodeNodeOrder, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "children" optionalArgs object_ (identity >> Decode.list >> Decode.nullable)


type_ : SelectionSet Fractal.Enum.NodeType.NodeType Fractal.Object.Node
type_ =
    Object.selectionForField "Enum.NodeType.NodeType" "type_" [] Fractal.Enum.NodeType.decoder


type alias TensionsOutOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.TensionFilter
    , order : OptionalArgument Fractal.InputObject.TensionOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


tensions_out : (TensionsOutOptionalArguments -> TensionsOutOptionalArguments) -> SelectionSet decodesTo Fractal.Object.Tension -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Node
tensions_out fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs =
            [ Argument.optional "filter" filledInOptionals.filter Fractal.InputObject.encodeTensionFilter, Argument.optional "order" filledInOptionals.order Fractal.InputObject.encodeTensionOrder, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "tensions_out" optionalArgs object_ (identity >> Decode.list >> Decode.nullable)


type alias TensionsInOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.TensionFilter
    , order : OptionalArgument Fractal.InputObject.TensionOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


tensions_in : (TensionsInOptionalArguments -> TensionsInOptionalArguments) -> SelectionSet decodesTo Fractal.Object.Tension -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Node
tensions_in fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs =
            [ Argument.optional "filter" filledInOptionals.filter Fractal.InputObject.encodeTensionFilter, Argument.optional "order" filledInOptionals.order Fractal.InputObject.encodeTensionOrder, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "tensions_in" optionalArgs object_ (identity >> Decode.list >> Decode.nullable)


about : SelectionSet (Maybe String) Fractal.Object.Node
about =
    Object.selectionForField "(Maybe String)" "about" [] (Decode.string |> Decode.nullable)


type alias MandateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.MandateFilter }


mandate : (MandateOptionalArguments -> MandateOptionalArguments) -> SelectionSet decodesTo Fractal.Object.Mandate -> SelectionSet (Maybe decodesTo) Fractal.Object.Node
mandate fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { filter = Absent }

        optionalArgs =
            [ Argument.optional "filter" filledInOptionals.filter Fractal.InputObject.encodeMandateFilter ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "mandate" optionalArgs object_ (identity >> Decode.nullable)


type alias DocsOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.TensionFilter
    , order : OptionalArgument Fractal.InputObject.TensionOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


docs : (DocsOptionalArguments -> DocsOptionalArguments) -> SelectionSet decodesTo Fractal.Object.Tension -> SelectionSet (Maybe (List (Maybe decodesTo))) Fractal.Object.Node
docs fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs =
            [ Argument.optional "filter" filledInOptionals.filter Fractal.InputObject.encodeTensionFilter, Argument.optional "order" filledInOptionals.order Fractal.InputObject.encodeTensionOrder, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "docs" optionalArgs object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias SourceOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.TensionFilter }


source : (SourceOptionalArguments -> SourceOptionalArguments) -> SelectionSet decodesTo Fractal.Object.Tension -> SelectionSet (Maybe decodesTo) Fractal.Object.Node
source fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { filter = Absent }

        optionalArgs =
            [ Argument.optional "filter" filledInOptionals.filter Fractal.InputObject.encodeTensionFilter ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "source" optionalArgs object_ (identity >> Decode.nullable)


n_tensions_out : SelectionSet (Maybe Int) Fractal.Object.Node
n_tensions_out =
    Object.selectionForField "(Maybe Int)" "n_tensions_out" [] (Decode.int |> Decode.nullable)


n_tensions_in : SelectionSet (Maybe Int) Fractal.Object.Node
n_tensions_in =
    Object.selectionForField "(Maybe Int)" "n_tensions_in" [] (Decode.int |> Decode.nullable)


n_children : SelectionSet (Maybe Int) Fractal.Object.Node
n_children =
    Object.selectionForField "(Maybe Int)" "n_children" [] (Decode.int |> Decode.nullable)


stats : SelectionSet decodesTo Fractal.Object.NodeStats -> SelectionSet (Maybe decodesTo) Fractal.Object.Node
stats object_ =
    Object.selectionForCompositeField "stats" [] object_ (identity >> Decode.nullable)


isRoot : SelectionSet Bool Fractal.Object.Node
isRoot =
    Object.selectionForField "Bool" "isRoot" [] Decode.bool


isPrivate : SelectionSet Bool Fractal.Object.Node
isPrivate =
    Object.selectionForField "Bool" "isPrivate" [] Decode.bool


isArchived : SelectionSet Bool Fractal.Object.Node
isArchived =
    Object.selectionForField "Bool" "isArchived" [] Decode.bool


type alias CharacOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.NodeCharacFilter }


charac : (CharacOptionalArguments -> CharacOptionalArguments) -> SelectionSet decodesTo Fractal.Object.NodeCharac -> SelectionSet decodesTo Fractal.Object.Node
charac fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { filter = Absent }

        optionalArgs =
            [ Argument.optional "filter" filledInOptionals.filter Fractal.InputObject.encodeNodeCharacFilter ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "charac" optionalArgs object_ identity


type alias FirstLinkOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.UserFilter }


first_link : (FirstLinkOptionalArguments -> FirstLinkOptionalArguments) -> SelectionSet decodesTo Fractal.Object.User -> SelectionSet (Maybe decodesTo) Fractal.Object.Node
first_link fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { filter = Absent }

        optionalArgs =
            [ Argument.optional "filter" filledInOptionals.filter Fractal.InputObject.encodeUserFilter ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "first_link" optionalArgs object_ (identity >> Decode.nullable)


type alias SecondLinkOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.UserFilter }


second_link : (SecondLinkOptionalArguments -> SecondLinkOptionalArguments) -> SelectionSet decodesTo Fractal.Object.User -> SelectionSet (Maybe decodesTo) Fractal.Object.Node
second_link fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { filter = Absent }

        optionalArgs =
            [ Argument.optional "filter" filledInOptionals.filter Fractal.InputObject.encodeUserFilter ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "second_link" optionalArgs object_ (identity >> Decode.nullable)


skills : SelectionSet (Maybe (List String)) Fractal.Object.Node
skills =
    Object.selectionForField "(Maybe (List String))" "skills" [] (Decode.string |> Decode.list |> Decode.nullable)


role_type : SelectionSet (Maybe Fractal.Enum.RoleType.RoleType) Fractal.Object.Node
role_type =
    Object.selectionForField "(Maybe Enum.RoleType.RoleType)" "role_type" [] (Fractal.Enum.RoleType.decoder |> Decode.nullable)
