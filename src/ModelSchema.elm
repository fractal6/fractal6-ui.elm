module ModelSchema exposing (..)

import Components.Loading exposing (ErrorData, RequestResult(..), errorGraphQLHttpToString)
import Dict exposing (Dict)
import Extra exposing (mor)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.ContractStatus as ContractStatus
import Fractal.Enum.ContractType as ContractType
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
import Maybe exposing (withDefault)
import RemoteData exposing (RemoteData)
import Text as T



--
-- Frontend Data Structure
--


type alias NodesDict =
    Dict String Node


type alias TensionsList =
    List Tension


type alias UsersDict =
    Dict String (List User)


type alias Post =
    Dict String String



--
-- Node
--


type alias Node =
    { createdAt : String
    , name : String
    , nameid : String
    , parent : Maybe NodeId
    , type_ : NodeType.NodeType
    , role_type : Maybe RoleType.RoleType
    , color : Maybe String
    , first_link : Maybe User
    , visibility : NodeVisibility.NodeVisibility
    , mode : NodeMode.NodeMode
    , source : Maybe BlobId
    , userCanJoin : Maybe Bool -- Only here because we reconstruct the LocalGraph from the node list in the graphpack code in assets/js.
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
    { nameid : String }


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


type alias PNode =
    { name : String
    , nameid : String
    }


type alias EmitterOrReceiver =
    { name : String
    , nameid : String
    , role_type : Maybe RoleType.RoleType
    }



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
    , emitter : EmitterOrReceiver
    , receiver : EmitterOrReceiver
    , action : Maybe TensionAction.TensionAction
    , status : TensionStatus.TensionStatus

    -- aggregate
    --, comments_agg : Maybe Count
    , n_comments : Maybe Int
    }


type alias TensionsCount =
    { open : Int, closed : Int }


type alias Count =
    { count : Maybe Int }


type alias Label =
    { id : String, name : String, color : Maybe String }


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


type alias TensionHead =
    { id : String
    , createdAt : String
    , createdBy : Username
    , title : String
    , type_ : TensionType.TensionType
    , labels : Maybe (List Label)
    , assignees : Maybe (List User)
    , emitter : EmitterOrReceiver
    , receiver : EmitterOrReceiver
    , action : Maybe TensionAction.TensionAction
    , status : TensionStatus.TensionStatus

    --
    , isSubscribed : Maybe Bool

    --
    , blobs : Maybe (List Blob) -- head / len() == 1
    , contracts : Maybe (List IdPayload) -- head / len() == 1
    , history : Maybe (List Event)
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


type alias BlobFlag =
    { pushedFlag : Maybe String }


type alias Event =
    { id : String
    , createdAt : String
    , createdBy : Username
    , event_type : TensionEvent.TensionEvent
    , old : Maybe String
    , new : Maybe String
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

    -- Maybe hardocoded
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
    , children : Maybe (List SubNodeFragment)
    }


type alias SubNodeFragment =
    -- @deprecated ?
    { name : Maybe String
    , nameid : Maybe String
    , type_ : Maybe NodeType.NodeType
    , role_type : Maybe RoleType.RoleType
    , visibility : Maybe NodeVisibility.NodeVisibility
    , mode : Maybe NodeMode.NodeMode
    , about : Maybe String
    , mandate : Maybe Mandate
    , first_link : Maybe String
    }


node2NodeFragment : Maybe Node -> Maybe (List SubNodeFragment) -> Maybe NodeData -> NodeFragment
node2NodeFragment node_m children nData =
    { name = Maybe.map (\n -> n.name) node_m
    , nameid = Maybe.map (\n -> n.nameid) node_m
    , type_ = Maybe.map (\n -> n.type_) node_m
    , visibility = Maybe.map (\n -> n.visibility) node_m
    , mode = Maybe.map (\n -> n.mode) node_m
    , role_type = Maybe.map (\n -> n.role_type) node_m |> withDefault Nothing
    , role_ext = Nothing
    , color = Maybe.map (\n -> n.color) node_m |> withDefault Nothing
    , about = Maybe.map (\n -> n.about) nData |> withDefault Nothing
    , mandate = Maybe.map (\n -> n.mandate) nData |> withDefault Nothing
    , first_link = Maybe.map (\n -> n.first_link |> Maybe.map (\u -> u.username)) node_m |> withDefault Nothing
    , children = children
    }


node2SubNodeFragment : Node -> SubNodeFragment
node2SubNodeFragment n =
    { name = Just n.name
    , nameid = Just n.nameid
    , type_ = Just n.type_
    , visibility = Just n.visibility
    , mode = Just n.mode
    , role_type = n.role_type
    , about = Nothing
    , mandate = Nothing
    , first_link = n.first_link |> Maybe.map (\u -> u.username)
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
            , children = mor n.children nf.children
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
    UserCtx "" Nothing initUserRights [] "" ""


initNodeData : NodeData
initNodeData =
    NodeData Nothing Nothing


initNode : Node
initNode =
    { createdAt = ""
    , name = ""
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
    }


initPNode : PNode
initPNode =
    shrinkNode initNode


shrinkNode n =
    --shrinkNode : n -> PNode
    { name = n.name
    , nameid = n.nameid
    }


initMandate : Mandate
initMandate =
    Mandate "" Nothing Nothing Nothing


initNodeFragment : Maybe NodeType.NodeType -> NodeFragment
initNodeFragment nt =
    NodeFragment Nothing Nothing nt Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


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
    , tension : { id : String, receiver : PNode, title : String }
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
    }


type alias NotificationsForm =
    { uctx : UserCtx
    , first : Int
    }



--
-- User auth
--
{-
   @WARNING: changing field order will break user decoder.
   The decoder's field have to be ordered following the
   alias type definition.
-}


type alias UserCtx =
    { username : String
    , name : Maybe String
    , rights : UserRights
    , roles : List UserRole
    , client_version : String
    , expiresAt : String
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
    }


type alias UserRoleExtended =
    { name : String
    , nameid : String
    , role_type : RoleType.RoleType

    --
    , createdAt : String
    , parent : Maybe NodeId
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
                    [ "Http error: " ++ errorGraphQLHttpToString httpError ]
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
