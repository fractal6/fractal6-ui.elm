port module Ports exposing (..)

import Codecs exposing (LookupResult, labelDecoder, labelsEncoder, nodeDecoder, nodeEncoder, nodesEncoder, userCtxDecoder, userCtxEncoder, userDecoder, userEncoder, usersEncoder)
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import ModelCommon.Codecs exposing (nearestCircleid)
import ModelSchema exposing (Label, Node, NodesData, User, UserCtx)



-- Outgoing Ports


port outgoing : { action : String, data : JE.Value } -> Cmd msg



-- Ingoing
--port getBackTimeFrom : (String -> msg) -> Sub msg


port closeModalFromJs : (String -> msg) -> Sub msg


port closeModalConfirmFromJs : (() -> msg) -> Sub msg


port lookupNodeFromJs_ : (JD.Value -> a) -> Sub a


port lookupUserFromJs_ : (JD.Value -> a) -> Sub a


port lookupLabelFromJs_ : (JD.Value -> a) -> Sub a


port cancelAssigneesFromJs : (() -> msg) -> Sub msg


port cancelLabelsFromJs : (() -> msg) -> Sub msg


port cancelActionFromJs : (() -> msg) -> Sub msg


port cancelLookupFsFromJs : (() -> msg) -> Sub msg


port cancelColorFromJs : (() -> msg) -> Sub msg


port triggerHelpFromJs : (String -> msg) -> Sub msg



--
-- Utils drivers
--


log : String -> Cmd msg
log message =
    outgoing
        { action = "LOG"
        , data = JE.string message
        }


logErr : String -> Cmd msg
logErr message =
    outgoing
        { action = "LOGERR"
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



--
-- Session functions
--


saveUserCtx : UserCtx -> Cmd msg
saveUserCtx userCtx =
    let
        -- Stringigy a Dict
        --dataD = Dict.fromList [ ( "key", "user_ctx" ++ userCtx.username ), ( "data", JE.encode 0 <| userCtxEncoder userCtx ) ]
        --datad = JE.dict identity JE.string dataD
        --
        -- Turn the dict into Json string
        data =
            JE.object
                --[ ( "key", JE.string <| "user_ctx" ++ userCtx.username )
                [ ( "key", JE.string "user_ctx" )
                , ( "data", userCtxEncoder userCtx )
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


initUserSearch : List User -> Cmd msg
initUserSearch data =
    outgoing
        { action = "INIT_USERSEARCH"
        , data = usersEncoder data
        }


initLabelSearch : List Label -> Cmd msg
initLabelSearch data =
    outgoing
        { action = "INIT_LABELSEARCH"
        , data = labelsEncoder data
        }


addQuickSearchNodes : List Node -> Cmd msg
addQuickSearchNodes nodes =
    outgoing
        { action = "ADD_QUICKSEARCH_NODES"
        , data = JE.list JE.object <| List.map nodeEncoder nodes
        }


addQuickSearchUsers : List User -> Cmd msg
addQuickSearchUsers users =
    outgoing
        { action = "ADD_QUICKSEARCH_USERS"
        , data = usersEncoder users
        }


searchNode : String -> Cmd msg
searchNode pattern =
    outgoing
        { action = "SEARCH_NODES"
        , data = JE.string pattern
        }


searchUser : String -> Cmd msg
searchUser pattern =
    outgoing
        { action = "SEARCH_USERS"
        , data = JE.string pattern
        }


searchLabel : String -> Cmd msg
searchLabel pattern =
    outgoing
        { action = "SEARCH_LABELS"
        , data = JE.string pattern
        }



--- Popups


inheritWith : String -> Cmd msg
inheritWith target =
    outgoing
        { action = "INHERIT_WIDTH"
        , data = JE.string target
        }


focusOn : String -> Cmd msg
focusOn target =
    outgoing
        { action = "FOCUS_ON"
        , data = JE.string target
        }


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


click : String -> Cmd msg
click target =
    outgoing
        { action = "CLICK"
        , data = JE.string target
        }



--
-- Encoder
--


graphPackEncoder : NodesData -> String -> JE.Value
graphPackEncoder data focus =
    JE.object
        [ ( "data", nodesEncoder data )
        , ( "focusid", JE.string focus )
        ]



--
-- Decoder
--


lookupNodeFromJs : (LookupResult Node -> msg) -> Sub msg
lookupNodeFromJs object =
    lookupNodeFromJs_
        (object
            << (\x ->
                    case x of
                        Ok n ->
                            Ok n

                        Err err ->
                            Err (JD.errorToString err)
               )
            << JD.decodeValue (JD.list nodeDecoder)
        )


lookupUserFromJs : (LookupResult User -> msg) -> Sub msg
lookupUserFromJs object =
    lookupUserFromJs_
        (object
            << (\x ->
                    case x of
                        Ok n ->
                            Ok n

                        Err err ->
                            Err (JD.errorToString err)
               )
            << JD.decodeValue (JD.list userDecoder)
        )


lookupLabelFromJs : (LookupResult Label -> msg) -> Sub msg
lookupLabelFromJs object =
    lookupLabelFromJs_
        (object
            << (\x ->
                    case x of
                        Ok n ->
                            Ok n

                        Err err ->
                            Err (JD.errorToString err)
               )
            << JD.decodeValue (JD.list labelDecoder)
        )
