module ModelCommon exposing (..)

import Array
import Json.Decode as JD exposing (Value, decodeValue)
import Json.Encode as JE
import ModelOrg exposing (..)



-- Session


type alias Session =
    { user : UserState
    , node_focus : Maybe NodeFocus
    , node_path : Maybe NodePath
    , orga_data : Maybe NodesData
    , circle_tensions : Maybe TensionsData
    , node_action : Maybe ActionState
    }



-- User


type UserState
    = LoggedOut
    | LoggedIn UserCtx


type alias UserCtx =
    { username : String
    , name : Maybe String
    , roles : List UserRole
    }


type alias UserRole =
    { nameid : String, role_type : String }



-- Focus


type alias NodeFocus =
    { rootid : String
    , nameid : String
    , isRoot : Bool

    --, name : Maybe String // get the name when JS/D3 finished the rendering Job
    }


type alias NodePath =
    Array.Array { nidjs : String, nameid : String, name : String }



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
    = FirstStep target -- AskActions
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
    -- Helper for encoding ActionState / Receiving Node from JS.
    Result JD.Error Node



--
-- Getters
--


uriFromFocus : NodeFocus -> String
uriFromFocus focus =
    if focus.isRoot then
        String.join "/" [ "/org", focus.rootid ]

    else
        String.join "/" [ "/org", focus.rootid, focus.nameid ]
