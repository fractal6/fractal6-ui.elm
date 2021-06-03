module ModelSchema exposing (..)

import Components.Loading exposing (ErrorData, RequestResult(..), errorGraphQLHttpToString)
import Dict exposing (Dict)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.ContractStatus as ContractStatus
import Fractal.Enum.ContractType as ContractType
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
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



--
-- Frontend Data Structure
--


type alias NodesData =
    Dict String Node


type alias TensionsData =
    List Tension


type alias UsersData =
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
    , rootnameid : String
    , parent : Maybe NodeId -- see issue with recursive structure
    , type_ : NodeType.NodeType
    , role_type : Maybe RoleType.RoleType
    , first_link : Maybe User
    , charac : NodeCharac
    , isPrivate : Bool
    , source : Maybe BlobId
    }


type alias NodeExt =
    { id : String
    , createdAt : String
    , name : String
    , nameid : String
    , rootnameid : String
    , parent : Maybe NodeId -- see issue with recursive structure
    , type_ : NodeType.NodeType
    , role_type : Maybe RoleType.RoleType
    , first_link : Maybe Username
    , charac : NodeCharac
    , isPrivate : Bool
    , about : Maybe String

    -- aggregate
    , orga_agg : Maybe OrgaAgg
    }


type alias OrgaAgg =
    { n_members : Maybe Int
    , n_guests : Maybe Int
    }


type alias NodeCharac =
    { userCanJoin : Bool
    , mode : NodeMode.NodeMode
    }


type alias NodeId =
    { nameid : String, isPrivate : Bool }


type alias LocalGraph =
    { root : Maybe PNode
    , path : List PNode
    , focus : FocusNode
    }


type alias FocusNode =
    { name : String
    , nameid : String
    , type_ : NodeType.NodeType
    , charac : NodeCharac
    , children : List EmitterOrReceiver
    , isPrivate : Bool
    }


type alias PNode =
    { name : String
    , nameid : String
    , charac : NodeCharac
    , isPrivate : Bool
    }


type alias EmitterOrReceiver =
    { name : String
    , nameid : String
    , role_type : Maybe RoleType.RoleType
    , charac : NodeCharac
    , isPrivate : Bool
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


type alias Count =
    { count : Maybe Int }


type alias ActionResult =
    { action : Maybe TensionAction.TensionAction }


type alias Label =
    { id : String, name : String, color : Maybe String }


type alias LabelFull =
    { id : String, name : String, color : Maybe String, description : Maybe String, n_nodes : Maybe Int }


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
    , blobs : Maybe (List Blob) -- head / len() == 1
    , contracts : Maybe (List IdPayload) -- head / len() == 1
    , history : List Event
    , n_open_contracts : Maybe Int
    }


type alias TensionComments =
    { id : String
    , comments : Maybe (List Comment)
    , receiver : EmitterOrReceiver
    }


type alias TensionBlobs =
    { id : String
    , blobs : Maybe (List Blob)
    , receiver : EmitterOrReceiver
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
    , participants : Maybe (List Vote)

    -- Maybe hardocded
    , comments : Maybe (List Comment)
    }


type alias ContractComments =
    { tension : IdPayload
    , comments : Maybe (List Comment)
    }


type alias Vote =
    { node : NameidPayload
    , data : Maybe VoteData
    }


type alias VoteData =
    List Int


type alias NodeFragment =
    { name : Maybe String
    , nameid : Maybe String
    , type_ : Maybe NodeType.NodeType
    , role_type : Maybe RoleType.RoleType
    , about : Maybe String
    , mandate : Maybe Mandate
    , isPrivate : Maybe Bool
    , charac : Maybe NodeCharac
    , first_link : Maybe String
    , children : Maybe (List SubNodeFragment)
    }


type alias SubNodeFragment =
    { name : Maybe String
    , nameid : Maybe String
    , type_ : Maybe NodeType.NodeType
    , role_type : Maybe RoleType.RoleType
    , about : Maybe String
    , mandate : Maybe Mandate
    , isPrivate : Maybe Bool
    , charac : Maybe NodeCharac
    , first_link : Maybe String
    }


type alias PatchTensionPayloadID =
    { comments : Maybe (List Comment)
    , blobs : Maybe (List Blob)
    }



--
-- Mandate
--


type alias NodeData =
    { nameid : String -- needed for @isPrivate test
    , about : Maybe String
    , mandate : Maybe Mandate
    , isPrivate : Bool
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
    UserCtx "" Nothing initUserRights []


initNodeData : NodeData
initNodeData =
    NodeData "" Nothing Nothing False


initNode : Node
initNode =
    { createdAt = ""
    , name = ""
    , nameid = ""
    , rootnameid = ""
    , parent = Nothing
    , type_ = NodeType.Circle
    , role_type = Nothing
    , first_link = Nothing
    , charac = initCharac
    , isPrivate = False
    , source = Nothing
    }


initPNode : PNode
initPNode =
    shrinkNode initNode


shrinkNode n =
    --shrinkNode : node -> PNode
    { name = n.name
    , nameid = n.nameid
    , charac = n.charac
    , isPrivate = n.isPrivate
    }


initMandate : Mandate
initMandate =
    Mandate "" Nothing Nothing Nothing


initNodeFragment : Maybe NodeType.NodeType -> NodeFragment
initNodeFragment nt =
    NodeFragment Nothing Nothing nt Nothing Nothing Nothing Nothing Nothing Nothing Nothing


initCharac : NodeCharac
initCharac =
    { userCanJoin = False, mode = NodeMode.Coordinated }


initEventFragment : EventFragment
initEventFragment =
    { event_type = TensionEvent.Created, old = Nothing, new = Nothing }



--
-- User/Members
--


type alias Username =
    { username : String }


type alias User =
    { username : String
    , name : Maybe String

    -- logo
    }


type alias Member =
    { username : String
    , name : Maybe String
    , roles : List UserRoleExtended
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
    }


type alias UserRights =
    { canLogin : Bool
    , canCreateRoot : Bool
    , type_ : UserType.UserType
    }


type alias UserRole =
    { name : String
    , nameid : String
    , rootnameid : String
    , role_type : RoleType.RoleType
    }


type alias UserRoleExtended =
    { name : String
    , nameid : String
    , rootnameid : String
    , role_type : RoleType.RoleType

    --
    , createdAt : String
    , parent : Maybe NodeId
    , isPrivate : Bool
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
                    Failure [ "No data returned." ]


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
