{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2022 Fractale Co

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


port module Ports exposing (..)

import Codecs
    exposing
        ( LookupResult
        , WindowPos
        , labelDecoder
        , labelsEncoder
        , modalDataDecoder
        , nodeDecoder
        , nodeEncoder
        , nodesEncoder
        , userCtxDecoder
        , userCtxEncoder
        , userDecoder
        , userEncoder
        , usersEncoder
        , windowEncoder
        )
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Loading exposing (ModalData)
import Maybe exposing (withDefault)
import ModelCommon.Codecs exposing (nearestCircleid)
import ModelSchema
    exposing
        ( Label
        , Node
        , NodesDict
        , NotifCount
        , User
        , UserCtx
        )



{-
   Ingoing Ports
-}
-- Trigger


port triggerHelpFromJs : (() -> msg) -> Sub msg


port triggerJoinFromJs : (() -> msg) -> Sub msg


port triggerJoinPendingFromJs : (() -> msg) -> Sub msg


port triggerInviteFromJs : (() -> msg) -> Sub msg


port triggerMenuOrgaFromJs : (() -> msg) -> Sub msg


port triggerMenuTreeFromJs : (() -> msg) -> Sub msg


port triggerNotifFromJs : (() -> msg) -> Sub msg



-- User


port loggedOutOkFromJs : (() -> msg) -> Sub msg


port updateMenuOrgaFromJs : (Maybe Bool -> msg) -> Sub msg


port updateMenuTreeFromJs : (Maybe Bool -> msg) -> Sub msg


port updateLangFromJs : (String -> msg) -> Sub msg


port updateNotifFromJs : (() -> msg) -> Sub msg


port loadUserCtxFromJs : (JD.Value -> msg) -> Sub msg



-- Modal


port openAuthModalFromJs : (JD.Value -> msg) -> Sub msg


port openAuthNeededFromJs : (() -> msg) -> Sub msg


port closeModalFromJs : (JD.Value -> msg) -> Sub msg


port closeModalConfirmFromJs : (JD.Value -> msg) -> Sub msg


port closeModalTensionFromJs : (JD.Value -> msg) -> Sub msg


port closeActionPanelModalFromJs : (JD.Value -> msg) -> Sub msg



-- Panel


port cancelActionFromJs : (() -> msg) -> Sub msg


port cancelLookupFsFromJs : (() -> msg) -> Sub msg



-- Menus


port requireTreeDataFromJs : (() -> msg) -> Sub msg



-- Lookup and Quicksearch


port lookupNodeFromJs_ : (JD.Value -> a) -> Sub a


port lookupUserFromJs_ : (JD.Value -> a) -> Sub a


port lookupLabelFromJs_ : (JD.Value -> a) -> Sub a



-- Utils


port cancelColorFromJs : (() -> msg) -> Sub msg


port relogErr : (String -> msg) -> Sub msg



{-
   Outgoing Ports
-}


port outgoing : { action : String, data : JE.Value } -> Cmd msg



--
-- Bulma
--


bulma_driver : String -> Cmd msg
bulma_driver eltId =
    outgoing
        { action = "BULMA"
        , data = JE.string eltId
        }



--
-- Graphpack
--


initGraphPack : NodesDict -> String -> Cmd msg
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


redrawGraphPack : NodesDict -> Cmd msg
redrawGraphPack data =
    outgoing
        { action = "DRAW_GRAPHPACK"
        , data = graphPackEncoder data ""
        }


removeRedrawGraphPack : NodesDict -> String -> Cmd msg
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
saveUserCtx uctx =
    outgoing
        { action = "SAVE_USERCTX"
        , data =
            JE.object
                -- @TODO: pass username to support multiple session
                [ ( "data", userCtxEncoder uctx )
                ]
        }


removeSession : UserCtx -> Cmd msg
removeSession uctx =
    outgoing
        { action = "REMOVE_SESSION"
        , data = JE.string ""
        }


saveWindowpos : WindowPos -> Cmd msg
saveWindowpos x =
    outgoing
        { action = "SAVE_SESSION_ITEM"
        , data =
            JE.object
                [ ( "key", JE.string "window_pos" )
                , ( "val", windowEncoder x )
                ]
        }


saveMenuOrga : Bool -> Cmd msg
saveMenuOrga x =
    outgoing
        { action = "SAVE_SESSION_ITEM"
        , data =
            JE.object
                [ ( "key", JE.string "orga_menu" )
                , ( "val", JE.bool x )
                ]
        }


saveMenuTree : Bool -> Cmd msg
saveMenuTree x =
    outgoing
        { action = "SAVE_SESSION_ITEM"
        , data =
            JE.object
                [ ( "key", JE.string "tree_menu" )
                , ( "val", JE.bool x )
                ]
        }



--- Modal


raiseAuthModal : UserCtx -> Cmd msg
raiseAuthModal uctx =
    outgoing
        { action = "RAISE_AUTH_MODAL"
        , data = userCtxEncoder uctx
        }


raiseAuthNeeded : Cmd msg
raiseAuthNeeded =
    outgoing
        { action = "RAISE_AUTH_NEEDED"
        , data = JE.string ""
        }


open_modal : String -> Cmd msg
open_modal modalid =
    outgoing
        { action = "OPEN_MODAL"
        , data = JE.string modalid
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


initUserSearchSeek : List User -> String -> Cmd msg
initUserSearchSeek data pattern =
    outgoing
        { action = "INIT_USERSEARCHSEEK"
        , data =
            JE.object
                [ ( "users", usersEncoder data )
                , ( "pattern", JE.string pattern )
                ]
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



--
-- Menus
--


openOrgaMenu : Cmd msg
openOrgaMenu =
    outgoing
        { action = "OPEN_ORGA_MENU"
        , data = JE.string ""
        }


closeOrgaMenu : Cmd msg
closeOrgaMenu =
    outgoing
        { action = "CLOSE_ORGA_MENU"
        , data = JE.string ""
        }


openTreeMenu : Cmd msg
openTreeMenu =
    outgoing
        { action = "OPEN_TREE_MENU"
        , data = JE.string ""
        }


closeTreeMenu : Cmd msg
closeTreeMenu =
    outgoing
        { action = "CLOSE_TREE_MENU"
        , data = JE.string ""
        }


requireTreeData : Cmd msg
requireTreeData =
    outgoing
        { action = "REQUIRE_TREE_DATA"
        , data = JE.string ""
        }



--
-- Utils
--


log : String -> Cmd msg
log message =
    outgoing
        { action = "LOG"
        , data = JE.string message
        }


show : String -> Cmd msg
show message =
    outgoing
        { action = "SHOW"
        , data = JE.string message
        }


hide : String -> Cmd msg
hide message =
    outgoing
        { action = "HIDE"
        , data = JE.string message
        }


fitHeight : String -> Cmd msg
fitHeight message =
    outgoing
        { action = "FIT_HEIGHT"
        , data = JE.string message
        }


logErr : String -> Cmd msg
logErr message =
    outgoing
        { action = "LOGERR"
        , data = JE.string message
        }


click : String -> Cmd msg
click target =
    outgoing
        { action = "CLICK"
        , data = JE.string target
        }


forceReload : Cmd msg
forceReload =
    outgoing
        { action = "FORCE_RELOAD"
        , data = JE.string ""
        }


reloadLang : String -> Cmd msg
reloadLang lang =
    outgoing
        { action = "RELOAD_LANG"
        , data = JE.string lang
        }


resetScroll : Cmd msg
resetScroll =
    outgoing
        { action = "RESET_SCROLL"
        , data = JE.string ""
        }


send_if_mobile : String -> Cmd msg
send_if_mobile p =
    outgoing
        { action = "SEND_IF_MOBILE"
        , data = JE.string p
        }


richText : String -> String -> Cmd msg
richText targetid command =
    outgoing
        { action = "RICH_TEXT"
        , data =
            JE.object
                [ ( "target", JE.string targetid )
                , ( "command", JE.string command )
                ]
        }



--
-- Encoder
--


graphPackEncoder : NodesDict -> String -> JE.Value
graphPackEncoder data focus =
    JE.object
        [ ( "data", nodesEncoder data )
        , ( "focusid", JE.string focus )
        ]



--
-- Decoder
--
-- Modal data decoder


mcPD : ((JD.Value -> msg) -> Sub msg) -> (String -> msg) -> (ModalData -> msg) -> Sub msg
mcPD sub messageErr message =
    sub
        ((\x ->
            case x of
                Ok n ->
                    message n

                Err err ->
                    messageErr (JD.errorToString err)
         )
            << JD.decodeValue modalDataDecoder
        )



-- uctx Decoder


uctxPD : ((JD.Value -> msg) -> Sub msg) -> (String -> msg) -> (UserCtx -> msg) -> Sub msg
uctxPD sub messageErr message =
    sub
        ((\x ->
            case x of
                Ok n ->
                    message n

                Err err ->
                    messageErr (JD.errorToString err)
         )
            << JD.decodeValue userCtxDecoder
        )


uctxPD2 : ((JD.Value -> msg) -> Sub msg) -> (String -> msg) -> (Bool -> UserCtx -> msg) -> Sub msg
uctxPD2 sub messageErr message =
    sub
        ((\x ->
            case x of
                Ok n ->
                    message (withDefault False n.refresh) n.uctx

                Err err ->
                    messageErr (JD.errorToString err)
         )
            << JD.decodeValue
                (JD.map2
                    (\a b ->
                        { uctx = a, refresh = b }
                    )
                    (JD.field "uctx" userCtxDecoder)
                    (JD.maybe <| JD.field "refresh" JD.bool)
                )
        )



-- Quicksearch/lookup decoder


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
