module ModelCommon exposing (..)

import Array
import Json.Decode as JD exposing (Value, decodeValue)
import Json.Encode as JE
import ModelOrg exposing (..)



-- Session


type alias Session =
    { user : UserState
    , node_focus : Maybe NodeFocusState
    , orga_data : Maybe OrgaData
    , circle_tensions : Maybe CircleTensionsData
    , node_action : Maybe ActionState
    }



-- User


type UserState
    = LoggedOut
    | LoggedIn UserInfo


type alias UserInfo =
    { username : String
    , display_name : String
    }



-- Focus


type alias NodeFocusState =
    { nidjs : String -- This is redundant with the path[-1]
    , nameid : String --
    , rootid : String --
    , name : String --
    , path : Array.Array { nidjs : String, nameid : String, name : String }
    }



-- Data Structures


type alias ErrorData =
    String


type alias OrgaData =
    RequestResult ErrorData NodesData


type alias CircleTensionsData =
    RequestResult ErrorData TensionsData



-- Action Step and Form Data


type ActionState
    = Ask (ActionStep Node)
    | AskErr String
    | NotAsk


type ActionStep target
    = FirstStep target
    | AddTensionStep TensionForm -- AskNewTension


type TensionStep
    = TensionTypeForm
    | TensionFinalForm
    | TensionValidation


type alias TensionForm =
    { step : TensionStep
    , post : Post
    , result : RequestResult ErrorData (Maybe AddTensionPayload)
    , target : Node
    , source : Node
    }


type alias NodeTarget =
    -- Helper for encoding ActionState
    Result JD.Error Node
