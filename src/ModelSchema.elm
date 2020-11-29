module ModelSchema exposing (..)

import Components.Loading exposing (ErrorData, RequestResult(..), errorGraphQLHttpToString)
import Dict exposing (Dict)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
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


type alias NodeCharac =
    { userCanJoin : Bool
    , mode : NodeMode.NodeMode
    }


type alias NodeId =
    { nameid : String, isPrivate : Bool }


type alias RootNode =
    { name : String
    , nameid : String
    , charac : NodeCharac
    , isPrivate : Bool
    }


type alias PNode =
    { name : String, nameid : String, isPrivate : Bool }


type alias FocusNode =
    { name : String
    , nameid : String
    , type_ : NodeType.NodeType
    , charac : NodeCharac
    , children : List EmitterOrReceiver
    , isPrivate : Bool
    }


type alias LocalGraph =
    { root : Maybe RootNode
    , path : List PNode
    , focus : FocusNode
    }


type alias EmitterOrReceiver =
    { name : String
    , nameid : String
    , role_type : Maybe RoleType.RoleType
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
    , n_comments : Maybe Int
    }


type alias ActionResult =
    { action : Maybe TensionAction.TensionAction }


type alias Label =
    { name : String }


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

    --
    , status : TensionStatus.TensionStatus
    , blobs : Maybe (List Blob) -- head
    , history : List Event
    }


type alias TensionComments =
    { id : String
    , comments : Maybe (List Comment)
    , n_comments : Maybe Int
    }


type alias TensionBlobs =
    { id : String
    , blobs : Maybe (List Blob)
    , n_blobs : Maybe Int
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
    UserCtx "" Nothing (UserRights False False) []


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


initMandate : Mandate
initMandate =
    Mandate "" Nothing Nothing Nothing


initNodeFragment : Maybe NodeType.NodeType -> NodeFragment
initNodeFragment nt =
    NodeFragment Nothing Nothing nt Nothing Nothing Nothing Nothing Nothing Nothing Nothing


initCharac : NodeCharac
initCharac =
    { userCanJoin = False, mode = NodeMode.Coordinated }



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



--
-- Response Data for Mutations
--


type alias BlobId =
    { id : String
    , tension : IdPayload
    }


type alias IdPayload =
    { id : String }



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
