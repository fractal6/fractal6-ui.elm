{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2023 Fractale Co

   This file is part of Fractale.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
-}


module ModelSchema exposing (..)

import Dict exposing (Dict)
import Extra exposing (mor)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.ContractStatus as ContractStatus
import Fractal.Enum.ContractType as ContractType
import Fractal.Enum.Lang as Lang
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.NodeVisibility as NodeVisibility
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Fractal.Enum.UserType as UserType
import Fractal.Scalar
import Fractal.ScalarCodecs
import Graphql.Http
import Json.Encode as JE
import Loading exposing (RequestResult(..), errorGraphQLHttpToString)
import Maybe exposing (withDefault)
import RemoteData exposing (RemoteData)
import Text as T



{-
   @WARNING: changing field order may break Json decoder.
   The decoder's field have to be ordered following the
   alias type definition.
-}
--
-- Frontend Data Structure
--


type alias NodesDict =
    -- @codefactor: rename to OrgaData ?
    Dict String Node


type alias TensionsDict =
    Dict String (List Tension)


type alias UsersDict =
    Dict String (List User)


type alias Post =
    Dict String String



--
-- Node
--


type alias Node =
    { name : String
    , nameid : String
    , parent : Maybe NodeId
    , type_ : NodeType.NodeType
    , role_type : Maybe RoleType.RoleType
    , color : Maybe String
    , first_link : Maybe User
    , visibility : NodeVisibility.NodeVisibility
    , mode : NodeMode.NodeMode
    , source : Maybe BlobId

    -- Only here to build LocalGraph from OrgaData
    , userCanJoin : Maybe Bool

    -- Aggregate
    , n_tensions : Int
    }


type alias NodeExt =
    { id : String
    , createdAt : String
    , name : String
    , nameid : String
    , parent : Maybe NodeId
    , type_ : NodeType.NodeType
    , role_type : Maybe RoleType.RoleType
    , first_link : Maybe Username
    , visibility : NodeVisibility.NodeVisibility
    , about : Maybe String

    -- aggregate
    , orga_agg : Maybe OrgaAgg
    }


type alias OrgaAgg =
    { n_members : Maybe Int
    , n_guests : Maybe Int
    }


type alias NodeId =
    -- User fetchNode2 to fetch the source, else it is hardcoded to Nothing
    { nameid : String, source : Maybe BlobId }


type alias LocalGraph =
    { root : Maybe RNode
    , path : List PNode
    , focus : FocusNode
    }


type alias FocusNode =
    { name : String
    , nameid : String
    , type_ : NodeType.NodeType
    , visibility : NodeVisibility.NodeVisibility
    , mode : NodeMode.NodeMode
    , children : List EmitterOrReceiver
    , source : Maybe BlobId
    }


type alias RNode =
    { name : String
    , nameid : String
    , userCanJoin : Maybe Bool
    }


{-| From root to focus node (both included)
-}
type alias PNode =
    { name : String
    , nameid : String

    -- optional
    , source : Maybe BlobId
    }


type alias PNode_ n =
    { n
        | name : String
        , nameid : String
        , source : Maybe BlobId
    }


getSourceTid : PNode_ n -> String
getSourceTid n =
    case n.source of
        Just blob ->
            blob.tension.id

        Nothing ->
            ""


type alias EmitterOrReceiver =
    -- similar to UserRole
    { name : String
    , nameid : String
    , role_type : Maybe RoleType.RoleType
    , color : Maybe String
    }


type alias NodeWithPin a =
    { a
        | pinned : Maybe (List IdPayload)
    }


type alias NodeRights =
    { visibility : NodeVisibility.NodeVisibility
    , userCanJoin : Maybe Bool
    , guestCanCreateTension : Maybe Bool
    }


type alias OrgaNode =
    { name : String
    , nameid : String
    }


type alias OrgaInfo =
    { -- total numbers of open tensions
      n_tensions : Int
    , -- total numbers of members
      n_members : Int
    , -- total numbers of watching users
      n_watchers : Int
    , -- is logged user watching the orga
      isWatching : Maybe Bool
    }


type alias NotifCount =
    { unread_events : Int
    , pending_contracts : Int
    , assigned_tensions : Int
    }


initNotifCount : NotifCount
initNotifCount =
    { unread_events = 0, pending_contracts = 0, assigned_tensions = 0 }


notifCountEncoder : NotifCount -> JE.Value
notifCountEncoder notif =
    JE.object
        [ ( "unread_events", JE.int notif.unread_events )
        , ( "pending_contracts", JE.int notif.pending_contracts )
        , ( "assigned_tensions", JE.int notif.assigned_tensions )
        ]



--
-- Tension
--


type alias Tension =
    { id : String
    , createdAt : String
    , createdBy : Username
    , title : String
    , type_ : TensionType.TensionType
    , labels : Maybe (List Label)

    --, emitter : EmitterOrReceiver
    , receiver : EmitterOrReceiver
    , action : Maybe TensionAction.TensionAction
    , status : TensionStatus.TensionStatus

    -- aggregate
    , n_comments : Maybe Int

    -- Extra / Might be hardocded / use with TensionsAll
    , assignees : Maybe (List User)
    }


type alias TensionsCount =
    { open : Int, closed : Int }


type alias Count =
    { count : Maybe Int }


type alias Label =
    { id : String
    , name : String
    , color : Maybe String

    -- nodes are hardcoded in Query/, not in Request...
    , nodes : List NameidPayload
    }


type alias LabelFull =
    { id : String
    , name : String
    , color : Maybe String
    , description : Maybe String
    , n_nodes : Maybe Int

    --, n_tensions : Maybe Int
    }


type alias RoleExt =
    { id : String
    , name : String
    , color : Maybe String
    , role_type : RoleType.RoleType
    , nodes : List NameidPayload
    }


type alias RoleExtFull =
    { id : String
    , name : String
    , color : Maybe String
    , role_type : RoleType.RoleType
    , about : Maybe String
    , mandate : Maybe Mandate
    , n_nodes : Maybe Int
    , n_roles : Maybe Int
    }


type alias RoleExtCommon a =
    { a
        | name : String
        , color : Maybe String
        , role_type : RoleType.RoleType
    }


type alias TensionHead =
    { id : String
    , createdAt : String
    , createdBy : Username
    , title : String
    , type_ : TensionType.TensionType
    , labels : Maybe (List Label)
    , assignees : Maybe (List User)

    --, emitter : EmitterOrReceiver
    , receiver : EmitterOrReceiver
    , action : Maybe TensionAction.TensionAction
    , status : TensionStatus.TensionStatus

    -- Computed
    , isSubscribed : Bool
    , isPinned : Bool

    -- List and Head
    , blobs : Maybe (List Blob) -- head / len() == 1
    , contracts : Maybe (List IdPayload) -- head / len() == 1
    , history : Maybe (List Event)

    -- Count @debug: filter Open contract
    , n_open_contracts : Maybe Int
    }


type alias TensionComments =
    { id : String
    , comments : Maybe (List Comment)
    }


type alias TensionBlobs =
    { id : String
    , blobs : Maybe (List Blob)
    }


type alias Comment =
    { id : String
    , createdAt : String
    , updatedAt : Maybe String
    , createdBy : Username
    , message : String

    -- Reaction Results are agglomerated in the query decoder
    , reactions : List Reaction
    }


type alias Reaction =
    { type_ : Int
    , users : List String
    }


type alias ReactionResponse =
    { cid : String
    , type_ : Int
    }


type alias Blob =
    { id : String
    , createdAt : String
    , createdBy : Username
    , blob_type : BlobType.BlobType
    , node : Maybe NodeFragment
    , md : Maybe String
    , pushedFlag : Maybe String
    }


type alias TensionBlobFlag =
    { title : String, blobs : Maybe (List BlobFlag) }


type alias BlobFlag =
    { pushedFlag : Maybe String }


type alias Event =
    { id : String
    , createdAt : String
    , createdBy : Username
    , event_type : TensionEvent.TensionEvent
    , old : Maybe String
    , new : Maybe String
    , mentioned : Maybe MentionedTension
    }


type alias MentionedTension =
    { id : String
    , status : TensionStatus.TensionStatus
    , title : String
    , receiverid : String
    }


type alias EventFragment =
    { event_type : TensionEvent.TensionEvent
    , old : Maybe String
    , new : Maybe String
    }


type alias Contract =
    { id : String
    , createdAt : String
    , closedAt : Maybe String
    , createdBy : Username
    , tension : IdPayload
    , event : EventFragment
    , status : ContractStatus.ContractStatus
    , contract_type : ContractType.ContractType

    --, candidate:
    , candidates : Maybe (List Username)
    , participants : List Vote

    -- Maybe hardcoded
    , comments : Maybe (List Comment)
    }


type alias ContractFull =
    { id : String
    , createdAt : String
    , closedAt : Maybe String
    , createdBy : Username
    , tension : TensionForContract
    , event : EventFragment
    , status : ContractStatus.ContractStatus
    , contract_type : ContractType.ContractType

    --, candidate:
    , candidates : Maybe (List Username)
    , participants : List Vote

    -- Full
    , isValidator : Maybe Bool
    , comments : Maybe (List Comment)
    }


type alias TensionForContract =
    { id : String
    , blobs : Maybe (List { node : Maybe NodeFragment })
    }


type alias ContractResult =
    { id : String
    , status : ContractStatus.ContractStatus
    }


type alias ContractComments =
    { tension : IdPayload
    , comments : Maybe (List Comment)
    }


type alias Vote =
    { voteid : String
    , node : NameidPayload
    , data : VoteData
    }


type alias VoteData =
    List Int


type alias NodeFragment =
    { name : Maybe String
    , nameid : Maybe String
    , type_ : Maybe NodeType.NodeType
    , role_type : Maybe RoleType.RoleType
    , role_ext : Maybe String
    , color : Maybe String
    , visibility : Maybe NodeVisibility.NodeVisibility
    , mode : Maybe NodeMode.NodeMode
    , about : Maybe String
    , mandate : Maybe Mandate
    , first_link : Maybe String
    }


node2NodeFragment : Maybe Node -> Maybe NodeData -> NodeFragment
node2NodeFragment node_m nData =
    { name = Maybe.map .name node_m
    , nameid = Maybe.map .nameid node_m
    , type_ = Maybe.map .type_ node_m
    , visibility = Maybe.map .visibility node_m
    , mode = Maybe.map .mode node_m
    , role_type = Maybe.map .role_type node_m |> withDefault Nothing
    , role_ext = Nothing
    , color = Maybe.map .color node_m |> withDefault Nothing
    , about = Maybe.map .about nData |> withDefault Nothing
    , mandate = Maybe.map .mandate nData |> withDefault Nothing
    , first_link = Maybe.map (\n -> Maybe.map .username n.first_link) node_m |> withDefault Nothing
    }


nodeFragmentUpdate : Maybe NodeFragment -> NodeFragment -> NodeFragment
nodeFragmentUpdate n_m n =
    Maybe.map
        (\nf ->
            { name = mor n.name nf.name
            , nameid = mor n.nameid nf.nameid
            , type_ = mor n.type_ nf.type_
            , visibility = mor n.visibility nf.visibility
            , mode = mor n.mode nf.mode
            , role_type = mor n.role_type nf.role_type
            , role_ext = Nothing
            , color = mor n.color nf.color
            , about = mor n.about nf.about
            , mandate = mor n.mandate nf.mandate
            , first_link = mor n.first_link nf.first_link
            }
        )
        n_m
        |> withDefault n


type alias PatchTensionPayloadID =
    { comments : Maybe (List Comment)
    , blobs : Maybe (List Blob)
    }



--
-- Mandate
--


type alias NodeData =
    { about : Maybe String
    , mandate : Maybe Mandate
    }


type alias Mandate =
    { purpose : String
    , responsabilities : Maybe String
    , domains : Maybe String
    , policies : Maybe String

    --, tensions : List { id : String, title : String }
    }


initUserctx : UserCtx
initUserctx =
    UserCtx Nothing "" Lang.En initUserRights [] "" ""


initNodeData : NodeData
initNodeData =
    NodeData Nothing Nothing


initNode : Node
initNode =
    { name = ""
    , nameid = ""
    , parent = Nothing
    , type_ = NodeType.Circle
    , role_type = Nothing
    , color = Nothing
    , first_link = Nothing
    , visibility = NodeVisibility.Public
    , mode = NodeMode.Coordinated
    , source = Nothing
    , userCanJoin = Nothing
    , n_tensions = 0
    }


initPNode : PNode
initPNode =
    shrinkNode initNode


shrinkNode : PNode_ n -> PNode
shrinkNode n =
    { name = n.name
    , nameid = n.nameid
    , source = n.source
    }


initMandate : Mandate
initMandate =
    Mandate "" Nothing Nothing Nothing


initNodeFragment : Maybe NodeType.NodeType -> NodeFragment
initNodeFragment nt =
    NodeFragment Nothing Nothing nt Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


initEventFragment : EventFragment
initEventFragment =
    { event_type = TensionEvent.Created, old = Nothing, new = Nothing }



--
-- User/Members
--


type alias Username =
    { username : String }


type alias Email =
    { email : String }


type alias User =
    { username : String
    , name : Maybe String

    -- avatar ?
    }


type alias Member =
    { username : String
    , name : Maybe String
    , roles : List UserRoleExtended
    }


type alias UserEvents =
    List UserEvent


type alias UserEvent =
    { id : String
    , isRead : Bool
    , event : List EventKind
    }


type EventKind
    = TensionEvent EventNotif
    | ContractEvent ContractNotif
    | NotifEvent NotifNotif


type alias EventNotif =
    { createdAt : String
    , createdBy : Username
    , event_type : TensionEvent.TensionEvent
    , tension : { id : String, emitterid : String, receiver : PNode, title : String }
    }


type alias ContractNotif =
    { id : String
    , createdAt : String
    , createdBy : Username
    , contract_type : ContractType.ContractType
    , event : { event_type : TensionEvent.TensionEvent }
    , tension : { id : String, receiver : PNode }
    }


type alias NotifNotif =
    { createdAt : String
    , createdBy : Username
    , message : Maybe String
    , tension : Maybe { id : String, receiver : PNode }
    , contract : Maybe { id : String }
    , link : Maybe String
    }


type alias NotificationsForm =
    { uctx : UserCtx
    , first : Int
    }



--
-- User auth
--


type alias UserCtx =
    { name : Maybe String
    , username : String
    , lang : Lang.Lang
    , rights : UserRights
    , roles : List UserRole
    , client_version : String
    , expiresAt : String
    }


type alias UserProfile =
    { username : String
    , name : Maybe String
    , lang : Lang.Lang
    , rights : UserRights
    , roles : List UserRole

    --
    , notifyByEmail : Bool
    , bio : Maybe String
    , location : Maybe String

    --, utc: Maybe String
    --, links : Maybe (List String)
    --, skills : Maybe (List String)
    }


type alias UserFull =
    { username : String
    , name : Maybe String
    , rights : UserRights
    , roles : List UserRole

    --
    , notifyByEmail : Bool
    , lang : Lang.Lang
    , bio : Maybe String
    , location : Maybe String

    --, utc: Maybe String
    --, links : Maybe (List String)
    --, skills : Maybe (List String)
    -- Private
    , email : String
    }


type alias UserCommon a =
    { a
        | username : String
        , name : Maybe String
        , rights : UserRights
        , roles : List UserRole

        --
        , notifyByEmail : Bool
        , lang : Lang.Lang
        , bio : Maybe String
        , location : Maybe String

        --, utc: Maybe String
        --, links : Maybe (List String)
        --, skills : Maybe (List String)
    }


type alias UserRights =
    { canLogin : Bool
    , canCreateRoot : Bool
    , type_ : UserType.UserType
    }


type alias UserRole =
    { name : String
    , nameid : String
    , role_type : RoleType.RoleType
    , color : Maybe String
    }


type alias UserRoleExtended =
    { name : String
    , nameid : String
    , role_type : RoleType.RoleType
    , color : Maybe String

    --
    , createdAt : String
    , parent : Maybe NodeId
    }


type alias UserRoleCommon a =
    { a
        | name : String
        , nameid : String
        , role_type : RoleType.RoleType
        , color : Maybe String
    }


initUserRights : UserRights
initUserRights =
    UserRights False False UserType.Regular



--
-- Response Data for Mutations
--


type alias IdPayload =
    { id : String }


type alias NameidPayload =
    { nameid : String }


type alias TensionId =
    -- To use when there is possibly a contract to catch
    { id : String
    , contracts : Maybe (List Contract)
    }


type alias BlobId =
    { id : String
    , tension : IdPayload
    }



--
-- Response decoder
--


decodeResponse decoder response =
    --@DEBUG: Infered type...
    {-
       This decoder take two generic type of data:
       * `dataResponse` which is directlty related to Graphql data returned by the server.
       * `dataDecoded` which is the data Model used in Elm code.
       @DEBUG: how to set the universal type in the function signature.
    -}
    case response of
        RemoteData.Failure errors ->
            case errors of
                Graphql.Http.GraphqlError maybeParsedData err ->
                    -- Graphql error
                    err
                        |> List.map (\e -> e.message)
                        |> Failure

                Graphql.Http.HttpError httpError ->
                    [ "GraphQL Http Error: " ++ errorGraphQLHttpToString httpError ]
                        |> Failure

        RemoteData.Loading ->
            Loading

        RemoteData.NotAsked ->
            NotAsked

        RemoteData.Success data ->
            case decoder data of
                Just d ->
                    Success d

                Nothing ->
                    Failure [ T.noDataError ]


mutationDecoder : Maybe a -> Maybe a
mutationDecoder =
    identity



--
-- Utils
--


decodedId : Fractal.ScalarCodecs.Id -> String
decodedId (Fractal.Scalar.Id id) =
    id


encodeId : String -> Fractal.ScalarCodecs.Id
encodeId id =
    Fractal.Scalar.Id id


decodedTime : Fractal.ScalarCodecs.DateTime -> String
decodedTime (Fractal.Scalar.DateTime time) =
    time
