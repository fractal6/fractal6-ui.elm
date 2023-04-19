-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Object.Tension exposing (..)

import Fractal.Enum.TensionAction
import Fractal.Enum.TensionStatus
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


type alias EmitterOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.NodeFilter }


emitter :
    (EmitterOptionalArguments -> EmitterOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Node
    -> SelectionSet decodesTo Fractal.Object.Tension
emitter fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeNodeFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "emitter" optionalArgs____ object____ Basics.identity


emitterid : SelectionSet String Fractal.Object.Tension
emitterid =
    Object.selectionForField "String" "emitterid" [] Decode.string


type alias ReceiverOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.NodeFilter }


receiver :
    (ReceiverOptionalArguments -> ReceiverOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Node
    -> SelectionSet decodesTo Fractal.Object.Tension
receiver fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeNodeFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "receiver" optionalArgs____ object____ Basics.identity


receiverid : SelectionSet String Fractal.Object.Tension
receiverid =
    Object.selectionForField "String" "receiverid" [] Decode.string


title : SelectionSet String Fractal.Object.Tension
title =
    Object.selectionForField "String" "title" [] Decode.string


type_ : SelectionSet Fractal.Enum.TensionType.TensionType Fractal.Object.Tension
type_ =
    Object.selectionForField "Enum.TensionType.TensionType" "type_" [] Fractal.Enum.TensionType.decoder


status : SelectionSet Fractal.Enum.TensionStatus.TensionStatus Fractal.Object.Tension
status =
    Object.selectionForField "Enum.TensionStatus.TensionStatus" "status" [] Fractal.Enum.TensionStatus.decoder


action : SelectionSet (Maybe Fractal.Enum.TensionAction.TensionAction) Fractal.Object.Tension
action =
    Object.selectionForField "(Maybe Enum.TensionAction.TensionAction)" "action" [] (Fractal.Enum.TensionAction.decoder |> Decode.nullable)


type alias AssigneesOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.UserFilter
    , order : OptionalArgument Fractal.InputObject.UserOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


assignees :
    (AssigneesOptionalArguments -> AssigneesOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.User
    -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Tension
assignees fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeUserFilter, Argument.optional "order" filledInOptionals____.order Fractal.InputObject.encodeUserOrder, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "assignees" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


type alias LabelsOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.LabelFilter
    , order : OptionalArgument Fractal.InputObject.LabelOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


labels :
    (LabelsOptionalArguments -> LabelsOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Label
    -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Tension
labels fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeLabelFilter, Argument.optional "order" filledInOptionals____.order Fractal.InputObject.encodeLabelOrder, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "labels" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


type alias CommentsOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.CommentFilter
    , order : OptionalArgument Fractal.InputObject.CommentOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


comments :
    (CommentsOptionalArguments -> CommentsOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Comment
    -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Tension
comments fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeCommentFilter, Argument.optional "order" filledInOptionals____.order Fractal.InputObject.encodeCommentOrder, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "comments" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


type alias BlobsOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.BlobFilter
    , order : OptionalArgument Fractal.InputObject.BlobOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


blobs :
    (BlobsOptionalArguments -> BlobsOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Blob
    -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Tension
blobs fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeBlobFilter, Argument.optional "order" filledInOptionals____.order Fractal.InputObject.encodeBlobOrder, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "blobs" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


type alias HistoryOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.EventFilter
    , order : OptionalArgument Fractal.InputObject.EventOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


history :
    (HistoryOptionalArguments -> HistoryOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Event
    -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Tension
history fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeEventFilter, Argument.optional "order" filledInOptionals____.order Fractal.InputObject.encodeEventOrder, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "history" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


type alias MentionsOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.EventFilter
    , order : OptionalArgument Fractal.InputObject.EventOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


mentions :
    (MentionsOptionalArguments -> MentionsOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Event
    -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Tension
mentions fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeEventFilter, Argument.optional "order" filledInOptionals____.order Fractal.InputObject.encodeEventOrder, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "mentions" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


type alias ContractsOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.ContractFilter
    , order : OptionalArgument Fractal.InputObject.ContractOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


contracts :
    (ContractsOptionalArguments -> ContractsOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Contract
    -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Tension
contracts fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeContractFilter, Argument.optional "order" filledInOptionals____.order Fractal.InputObject.encodeContractOrder, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "contracts" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


type alias SubscribersOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.UserFilter
    , order : OptionalArgument Fractal.InputObject.UserOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


subscribers :
    (SubscribersOptionalArguments -> SubscribersOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.User
    -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Tension
subscribers fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeUserFilter, Argument.optional "order" filledInOptionals____.order Fractal.InputObject.encodeUserOrder, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "subscribers" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


type alias ProjectsOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.ProjectTensionFilter
    , order : OptionalArgument Fractal.InputObject.ProjectTensionOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


projects :
    (ProjectsOptionalArguments -> ProjectsOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.ProjectTension
    -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Tension
projects fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeProjectTensionFilter, Argument.optional "order" filledInOptionals____.order Fractal.InputObject.encodeProjectTensionOrder, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "projects" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


n_open_contracts : SelectionSet (Maybe Int) Fractal.Object.Tension
n_open_contracts =
    Object.selectionForField "(Maybe Int)" "n_open_contracts" [] (Decode.int |> Decode.nullable)


n_comments : SelectionSet (Maybe Int) Fractal.Object.Tension
n_comments =
    Object.selectionForField "(Maybe Int)" "n_comments" [] (Decode.int |> Decode.nullable)


id : SelectionSet Fractal.ScalarCodecs.Id Fractal.Object.Tension
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecId |> .decoder)


type alias CreatedByOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.UserFilter }


createdBy :
    (CreatedByOptionalArguments -> CreatedByOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.User
    -> SelectionSet decodesTo Fractal.Object.Tension
createdBy fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeUserFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "createdBy" optionalArgs____ object____ Basics.identity


createdAt : SelectionSet Fractal.ScalarCodecs.DateTime Fractal.Object.Tension
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


updatedAt : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.Tension
updatedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "updatedAt" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


message : SelectionSet (Maybe String) Fractal.Object.Tension
message =
    Object.selectionForField "(Maybe String)" "message" [] (Decode.string |> Decode.nullable)


type alias AssigneesAggregateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.UserFilter }


assigneesAggregate :
    (AssigneesAggregateOptionalArguments -> AssigneesAggregateOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.UserAggregateResult
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Tension
assigneesAggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeUserFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "assigneesAggregate" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias LabelsAggregateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.LabelFilter }


labelsAggregate :
    (LabelsAggregateOptionalArguments -> LabelsAggregateOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.LabelAggregateResult
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Tension
labelsAggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeLabelFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "labelsAggregate" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias CommentsAggregateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.CommentFilter }


commentsAggregate :
    (CommentsAggregateOptionalArguments -> CommentsAggregateOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.CommentAggregateResult
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Tension
commentsAggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeCommentFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "commentsAggregate" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias BlobsAggregateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.BlobFilter }


blobsAggregate :
    (BlobsAggregateOptionalArguments -> BlobsAggregateOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.BlobAggregateResult
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Tension
blobsAggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeBlobFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "blobsAggregate" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias HistoryAggregateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.EventFilter }


historyAggregate :
    (HistoryAggregateOptionalArguments -> HistoryAggregateOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.EventAggregateResult
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Tension
historyAggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeEventFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "historyAggregate" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias MentionsAggregateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.EventFilter }


mentionsAggregate :
    (MentionsAggregateOptionalArguments -> MentionsAggregateOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.EventAggregateResult
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Tension
mentionsAggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeEventFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "mentionsAggregate" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias ContractsAggregateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.ContractFilter }


contractsAggregate :
    (ContractsAggregateOptionalArguments -> ContractsAggregateOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.ContractAggregateResult
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Tension
contractsAggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeContractFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "contractsAggregate" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias SubscribersAggregateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.UserFilter }


subscribersAggregate :
    (SubscribersAggregateOptionalArguments -> SubscribersAggregateOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.UserAggregateResult
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Tension
subscribersAggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeUserFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "subscribersAggregate" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias ProjectsAggregateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.ProjectTensionFilter }


projectsAggregate :
    (ProjectsAggregateOptionalArguments -> ProjectsAggregateOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.ProjectTensionAggregateResult
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Tension
projectsAggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeProjectTensionFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "projectsAggregate" optionalArgs____ object____ (Basics.identity >> Decode.nullable)
