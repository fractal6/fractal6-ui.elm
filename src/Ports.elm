port module Ports exposing (..)

import Dict exposing (Dict)
import Json.Encode as JE
import ModelCommon exposing (graphPackEncoder, userEncoder)
import ModelSchema exposing (NodesData, UserCtx)



-- Ingoing
--port getBackTimeFrom : (String -> msg) -> Sub msg
-- How to use that ? (dayjs tipically or other use of a general library (updated nested data?)
-- Outgoing
-- Interface


port outgoing : { action : String, data : JE.Value } -> Cmd msg



-- Js Util


port closeModalFromJs : (String -> msg) -> Sub msg



--
-- Utils drivers
--


log : String -> Cmd msg
log message =
    outgoing
        { action = "LOG"
        , data = JE.string message
        }


bulma_driver : String -> Cmd msg
bulma_driver eltId =
    outgoing
        { action = "BULMA"
        , data = JE.string eltId
        }


toggle_theme : Cmd msg
toggle_theme =
    outgoing
        { action = "TOGGLE_TH"
        , data = JE.string ""
        }



--
-- Graphpack
--


initGraphPack : NodesData -> String -> Cmd msg
initGraphPack data focus =
    outgoing
        { action = "INIT_GRAPHPACK"
        , data = graphPackEncoder data focus
        }


focusGraphPack : String -> Cmd msg
focusGraphPack focusid =
    outgoing
        { action = "FOCUS_GRAPHPACK"
        , data = JE.string focusid
        }


clearTooltip : Cmd msg
clearTooltip =
    outgoing
        { action = "CLEAR_TOOLTIP"
        , data = JE.string ""
        }


redrawGraphPack : NodesData -> Cmd msg
redrawGraphPack data =
    outgoing
        { action = "DRAW_GRAPHPACK"
        , data = graphPackEncoder data ""
        }


removeRedrawGraphPack : NodesData -> String -> Cmd msg
removeRedrawGraphPack data nid =
    outgoing
        { action = "REMOVEDRAW_GRAPHPACK"
        , data = graphPackEncoder data nid
        }


drawButtonsGraphPack : Cmd msg
drawButtonsGraphPack =
    outgoing
        { action = "DRAW_BUTTONS_GRAPHPACK"
        , data = JE.string ""
        }



--
-- Session functions
--


saveUserCtx : UserCtx -> Cmd msg
saveUserCtx userCtx =
    let
        -- Stringigy a Dict
        --dataD = Dict.fromList [ ( "key", "user_ctx" ++ userCtx.username ), ( "data", JE.encode 0 <| userEncoder userCtx ) ]
        --datad = JE.dict identity JE.string dataD
        --
        -- Turn the dict into Json string
        data =
            JE.object
                --[ ( "key", JE.string <| "user_ctx" ++ userCtx.username )
                [ ( "key", JE.string "user_ctx" )
                , ( "data", userEncoder userCtx )
                ]
    in
    outgoing
        { action = "SAVE_USERCTX"
        , data = data
        }


loadUserCtx : String -> Cmd msg
loadUserCtx key =
    outgoing
        { action = "LOAD_USERCTX"
        , data = JE.string key
        }


removeUserCtx : UserCtx -> Cmd msg
removeUserCtx userCtx =
    outgoing
        { action = "REMOVE_USERCTX"
        , data = JE.string "user_ctx"

        --, data = JE.string <| "user_ctx" ++ userCtx.username
        }



--- Modal


open_modal : Cmd msg
open_modal =
    outgoing
        { action = "OPEN_MODAL"
        , data = JE.string ""
        }


close_modal : Cmd msg
close_modal =
    outgoing
        { action = "CLOSE_MODAL"
        , data = JE.string ""
        }


open_auth_modal : Cmd msg
open_auth_modal =
    outgoing
        { action = "OPEN_AUTH_MODAL"
        , data = JE.string ""
        }


close_auth_modal : Cmd msg
close_auth_modal =
    outgoing
        { action = "CLOSE_AUTH_MODAL"
        , data = JE.string ""
        }



--- Quick Search


searchNode : String -> Cmd msg
searchNode pattern =
    outgoing
        { action = "SEARCH_NODES"
        , data = JE.string pattern
        }



--- Popups


outsideClickClose : String -> String -> Cmd msg
outsideClickClose msg target =
    outgoing
        { action = "OUTSIDE_CLICK_CLOSE"
        , data =
            JE.object
                [ ( "msg", JE.string msg )
                , ( "target", JE.string target )
                ]
        }
