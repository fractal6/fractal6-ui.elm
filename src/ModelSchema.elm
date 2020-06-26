module ModelSchema exposing (..)

import Components.Loading exposing (ErrorData, errorGraphQLHttpToString)
import Dict exposing (Dict)
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
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



--
-- Remote Data and Sinks
--


type alias GqlData a =
    RequestResult ErrorData a


type RequestResult errors data
    = Success data
    | Failure errors
    | Loading
    | LoadingSlowly
    | NotAsked


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
    , first_link : Maybe Username
    , charac : NodeCharac
    }


type alias NodeCharac =
    { userCanJoin : Bool
    , mode : NodeMode.NodeMode
    }


type alias NodeId =
    { nameid : String }


type alias RootNode =
    { name : String
    , nameid : String
    , charac : NodeCharac
    }


type alias FocusNode =
    { name : String
    , nameid : String
    , type_ : NodeType.NodeType
    , children : List NodeId
    }


type alias PNode =
    { name : String, nameid : String }


type alias LocalGraph =
    { root : Maybe RootNode
    , path : List PNode
    , focus : FocusNode
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


type alias EmitterOrReceiver =
    { name : String
    , nameid : String
    , role_type : Maybe RoleType.RoleType
    }


type alias Username =
    { username : String }


type alias Label =
    { name : String }


type alias TensionExtended =
    { id : String
    , createdAt : String
    , createdBy : Username
    , title : String
    , type_ : TensionType.TensionType
    , labels : Maybe (List Label)
    , emitter : EmitterOrReceiver
    , receiver : EmitterOrReceiver
    , action : Maybe TensionAction.TensionAction
    , mandate : Maybe Mandate

    --
    , status : TensionStatus.TensionStatus
    , message : Maybe String
    , comments : Maybe (List Comment)
    , n_comments : Maybe Int
    }


type alias Comment =
    { createdAt : String
    , createdBy : Username
    , message : String
    }



--
-- Mandate
--


type alias Mandate =
    { purpose : String
    , responsabilities : Maybe String
    , domains : Maybe String
    , policies : Maybe String

    --, tensions : List { id : String, title : String }
    }



--
-- User/Members
--


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
    }



--
-- Response Data for Mutations
--


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
