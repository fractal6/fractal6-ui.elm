-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Object.Node exposing (..)

import Fractal.Enum.NodeMode
import Fractal.Enum.NodeType
import Fractal.Enum.NodeVisibility
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


type alias CreatedByOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.UserFilter }


createdBy :
    (CreatedByOptionalArguments -> CreatedByOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.User
    -> SelectionSet decodesTo Fractal.Object.Node
createdBy fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeUserFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "createdBy" optionalArgs____ object____ Basics.identity


createdAt : SelectionSet Fractal.ScalarCodecs.DateTime Fractal.Object.Node
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


updatedAt : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Object.Node
updatedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "updatedAt" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


nameid : SelectionSet String Fractal.Object.Node
nameid =
    Object.selectionForField "String" "nameid" [] Decode.string


rootnameid : SelectionSet String Fractal.Object.Node
rootnameid =
    Object.selectionForField "String" "rootnameid" [] Decode.string


type alias SourceOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.BlobFilter }


source :
    (SourceOptionalArguments -> SourceOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Blob
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Node
source fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeBlobFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "source" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


name : SelectionSet String Fractal.Object.Node
name =
    Object.selectionForField "String" "name" [] Decode.string


about : SelectionSet (Maybe String) Fractal.Object.Node
about =
    Object.selectionForField "(Maybe String)" "about" [] (Decode.string |> Decode.nullable)


skills : SelectionSet (Maybe (List String)) Fractal.Object.Node
skills =
    Object.selectionForField "(Maybe (List String))" "skills" [] (Decode.string |> Decode.list |> Decode.nullable)


isRoot : SelectionSet Bool Fractal.Object.Node
isRoot =
    Object.selectionForField "Bool" "isRoot" [] Decode.bool


type alias ParentOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.NodeFilter }


parent :
    (ParentOptionalArguments -> ParentOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Node
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Node
parent fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeNodeFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "parent" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type_ : SelectionSet Fractal.Enum.NodeType.NodeType Fractal.Object.Node
type_ =
    Object.selectionForField "Enum.NodeType.NodeType" "type_" [] Fractal.Enum.NodeType.decoder


type alias TensionsOutOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.TensionFilter
    , order : OptionalArgument Fractal.InputObject.TensionOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


tensions_out :
    (TensionsOutOptionalArguments -> TensionsOutOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Tension
    -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Node
tensions_out fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeTensionFilter, Argument.optional "order" filledInOptionals____.order Fractal.InputObject.encodeTensionOrder, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "tensions_out" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


type alias TensionsInOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.TensionFilter
    , order : OptionalArgument Fractal.InputObject.TensionOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


tensions_in :
    (TensionsInOptionalArguments -> TensionsInOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Tension
    -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Node
tensions_in fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeTensionFilter, Argument.optional "order" filledInOptionals____.order Fractal.InputObject.encodeTensionOrder, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "tensions_in" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


visibility : SelectionSet Fractal.Enum.NodeVisibility.NodeVisibility Fractal.Object.Node
visibility =
    Object.selectionForField "Enum.NodeVisibility.NodeVisibility" "visibility" [] Fractal.Enum.NodeVisibility.decoder


mode : SelectionSet Fractal.Enum.NodeMode.NodeMode Fractal.Object.Node
mode =
    Object.selectionForField "Enum.NodeMode.NodeMode" "mode" [] Fractal.Enum.NodeMode.decoder


rights : SelectionSet Int Fractal.Object.Node
rights =
    Object.selectionForField "Int" "rights" [] Decode.int


isArchived : SelectionSet Bool Fractal.Object.Node
isArchived =
    Object.selectionForField "Bool" "isArchived" [] Decode.bool


isPersonal : SelectionSet (Maybe Bool) Fractal.Object.Node
isPersonal =
    Object.selectionForField "(Maybe Bool)" "isPersonal" [] (Decode.bool |> Decode.nullable)


userCanJoin : SelectionSet (Maybe Bool) Fractal.Object.Node
userCanJoin =
    Object.selectionForField "(Maybe Bool)" "userCanJoin" [] (Decode.bool |> Decode.nullable)


guestCanCreateTension : SelectionSet (Maybe Bool) Fractal.Object.Node
guestCanCreateTension =
    Object.selectionForField "(Maybe Bool)" "guestCanCreateTension" [] (Decode.bool |> Decode.nullable)


type alias WatchersOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.UserFilter
    , order : OptionalArgument Fractal.InputObject.UserOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


watchers :
    (WatchersOptionalArguments -> WatchersOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.User
    -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Node
watchers fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeUserFilter, Argument.optional "order" filledInOptionals____.order Fractal.InputObject.encodeUserOrder, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "watchers" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


type alias ChildrenOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.NodeFilter
    , order : OptionalArgument Fractal.InputObject.NodeOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


children :
    (ChildrenOptionalArguments -> ChildrenOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Node
    -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Node
children fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeNodeFilter, Argument.optional "order" filledInOptionals____.order Fractal.InputObject.encodeNodeOrder, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "children" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


type alias LabelsOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.LabelFilter
    , order : OptionalArgument Fractal.InputObject.LabelOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


labels :
    (LabelsOptionalArguments -> LabelsOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Label
    -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Node
labels fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeLabelFilter, Argument.optional "order" filledInOptionals____.order Fractal.InputObject.encodeLabelOrder, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "labels" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


type alias RolesOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.RoleExtFilter
    , order : OptionalArgument Fractal.InputObject.RoleExtOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


roles :
    (RolesOptionalArguments -> RolesOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.RoleExt
    -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Node
roles fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeRoleExtFilter, Argument.optional "order" filledInOptionals____.order Fractal.InputObject.encodeRoleExtOrder, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "roles" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


type alias PinnedOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.TensionFilter
    , order : OptionalArgument Fractal.InputObject.TensionOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


pinned :
    (PinnedOptionalArguments -> PinnedOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Tension
    -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Node
pinned fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeTensionFilter, Argument.optional "order" filledInOptionals____.order Fractal.InputObject.encodeTensionOrder, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "pinned" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


type alias RoleExtOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.RoleExtFilter }


role_ext :
    (RoleExtOptionalArguments -> RoleExtOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.RoleExt
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Node
role_ext fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeRoleExtFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "role_ext" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


role_type : SelectionSet (Maybe Fractal.Enum.RoleType.RoleType) Fractal.Object.Node
role_type =
    Object.selectionForField "(Maybe Enum.RoleType.RoleType)" "role_type" [] (Fractal.Enum.RoleType.decoder |> Decode.nullable)


color : SelectionSet (Maybe String) Fractal.Object.Node
color =
    Object.selectionForField "(Maybe String)" "color" [] (Decode.string |> Decode.nullable)


type alias FirstLinkOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.UserFilter }


first_link :
    (FirstLinkOptionalArguments -> FirstLinkOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.User
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Node
first_link fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeUserFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "first_link" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias SecondLinkOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.UserFilter }


second_link :
    (SecondLinkOptionalArguments -> SecondLinkOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.User
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Node
second_link fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeUserFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "second_link" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias ContractsOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.VoteFilter
    , order : OptionalArgument Fractal.InputObject.VoteOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


contracts :
    (ContractsOptionalArguments -> ContractsOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Vote
    -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Node
contracts fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeVoteFilter, Argument.optional "order" filledInOptionals____.order Fractal.InputObject.encodeVoteOrder, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "contracts" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


type alias OrgaAggOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.OrgaAggFilter }


orga_agg :
    (OrgaAggOptionalArguments -> OrgaAggOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.OrgaAgg
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Node
orga_agg fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeOrgaAggFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "orga_agg" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias EventsHistoryOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.EventFilter
    , order : OptionalArgument Fractal.InputObject.EventOrder
    , first : OptionalArgument Int
    , offset : OptionalArgument Int
    }


events_history :
    (EventsHistoryOptionalArguments -> EventsHistoryOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.Event
    -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.Node
events_history fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent, order = Absent, first = Absent, offset = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeEventFilter, Argument.optional "order" filledInOptionals____.order Fractal.InputObject.encodeEventOrder, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "events_history" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


type alias TensionsOutAggregateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.TensionFilter }


tensions_outAggregate :
    (TensionsOutAggregateOptionalArguments -> TensionsOutAggregateOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.TensionAggregateResult
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Node
tensions_outAggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeTensionFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "tensions_outAggregate" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias TensionsInAggregateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.TensionFilter }


tensions_inAggregate :
    (TensionsInAggregateOptionalArguments -> TensionsInAggregateOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.TensionAggregateResult
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Node
tensions_inAggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeTensionFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "tensions_inAggregate" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias WatchersAggregateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.UserFilter }


watchersAggregate :
    (WatchersAggregateOptionalArguments -> WatchersAggregateOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.UserAggregateResult
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Node
watchersAggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeUserFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "watchersAggregate" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias ChildrenAggregateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.NodeFilter }


childrenAggregate :
    (ChildrenAggregateOptionalArguments -> ChildrenAggregateOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.NodeAggregateResult
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Node
childrenAggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeNodeFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "childrenAggregate" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias LabelsAggregateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.LabelFilter }


labelsAggregate :
    (LabelsAggregateOptionalArguments -> LabelsAggregateOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.LabelAggregateResult
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Node
labelsAggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeLabelFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "labelsAggregate" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias RolesAggregateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.RoleExtFilter }


rolesAggregate :
    (RolesAggregateOptionalArguments -> RolesAggregateOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.RoleExtAggregateResult
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Node
rolesAggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeRoleExtFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "rolesAggregate" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias PinnedAggregateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.TensionFilter }


pinnedAggregate :
    (PinnedAggregateOptionalArguments -> PinnedAggregateOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.TensionAggregateResult
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Node
pinnedAggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeTensionFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "pinnedAggregate" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias ContractsAggregateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.VoteFilter }


contractsAggregate :
    (ContractsAggregateOptionalArguments -> ContractsAggregateOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.VoteAggregateResult
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Node
contractsAggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeVoteFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "contractsAggregate" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias EventsHistoryAggregateOptionalArguments =
    { filter : OptionalArgument Fractal.InputObject.EventFilter }


events_historyAggregate :
    (EventsHistoryAggregateOptionalArguments -> EventsHistoryAggregateOptionalArguments)
    -> SelectionSet decodesTo Fractal.Object.EventAggregateResult
    -> SelectionSet (Maybe decodesTo) Fractal.Object.Node
events_historyAggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { filter = Absent }

        optionalArgs____ =
            [ Argument.optional "filter" filledInOptionals____.filter Fractal.InputObject.encodeEventFilter ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "events_historyAggregate" optionalArgs____ object____ (Basics.identity >> Decode.nullable)
