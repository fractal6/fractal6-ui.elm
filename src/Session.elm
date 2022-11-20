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


module Session exposing (..)

import Array exposing (Array)
import Codecs exposing (WindowPos, userCtxDecoder, windowDecoder)
import Fractal.Enum.Lang as Lang
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeVisibility as NodeVisibility
import Fractal.Enum.RoleType as RoleType
import Json.Decode as JD
import Loading
    exposing
        ( GqlData
        , WebData
        )
import ModelCommon exposing (AssigneeForm, LabelForm, OrgaForm, UserState(..))
import ModelCommon.Codecs exposing (NodeFocus)
import ModelSchema exposing (..)
import Ports
import RemoteData
import Time
import Url exposing (Url)



--
-- Session / Global
--


type alias Apis =
    { auth : String
    , gql : String
    , rest : String
    , doc : String
    , version : String
    }


type alias Screen =
    { w : Int, h : Int }


type alias Conf =
    { screen : Screen
    , lang : Lang.Lang
    , now : Time.Posix
    }


type alias SessionFlags =
    { uctx : Maybe JD.Value
    , lang : Maybe JD.Value
    , window_pos : Maybe JD.Value
    , orga_menu : Maybe Bool
    , tree_menu : Maybe Bool
    , apis : Apis
    , screen : Screen
    }


type alias Session =
    { user : UserState
    , lang : Lang.Lang
    , notif : NotifCount
    , referer : Maybe Url
    , can_referer : Maybe Url
    , token_data : WebData UserCtx
    , node_focus : Maybe NodeFocus
    , path_data : Maybe LocalGraph
    , children : Maybe (List NodeId)
    , node_data : Maybe NodeData
    , tensions_data : Maybe (List Tension)
    , tensions_int : Maybe (List Tension)
    , tensions_ext : Maybe (List Tension)
    , tensions_all : Maybe TensionsDict
    , tensions_count : Maybe TensionsCount
    , tension_head : Maybe TensionHead
    , orgs_data : Maybe (List OrgaNode)
    , tree_data : Maybe NodesDict
    , isAdmin : Maybe Bool
    , node_quickSearch : Maybe NodesQuickSearch
    , apis : Apis
    , window_pos : Maybe WindowPos
    , orga_menu : Maybe Bool
    , tree_menu : Maybe Bool
    , screen : Screen
    , authorsPanel : Maybe UserSearchPanelModel
    , labelsPanel : Maybe LabelSearchPanelModel
    , newOrgaData : Maybe OrgaForm
    , isWatching : Maybe Bool
    }


type
    GlobalCmd
    -- @FIX: Make this in Global to define the mapGlobalOutcmds only once ?!
    -- Or: use only Ports, and add Subscription in Global to trigger update from JS ! (bad for data copy)
    -- Or: use a type to return directly Global.Cmd !
    = -- Global Msg
      DoFocus String
    | DoNavigate String
    | DoReplaceUrl String
    | DoUpdateToken
    | DoUpdateUserSession UserCtx
    | DoUpdatePath (Maybe LocalGraph)
    | DoUpdateTree (Maybe NodesDict)
    | DoUpdateOrgs (Maybe (List OrgaNode))
    | DoToggleWatchOrga String
      -- Components Msg
    | DoCreateTension String
    | DoJoinOrga String
    | DoOpenActionPanel String String (Maybe ( Int, Int ))
    | DoToggleTreeMenu
    | DoFetchNode String
    | DoAddNodes (List Node)
    | DoUpdateNode String (Node -> Node)
    | DoDelNodes (List String)
    | DoMoveNode String String String
      -- App Msg
    | DoPushTension Tension
    | DoModalAsk String String -- Safe close modal


type alias NodesQuickSearch =
    { pattern : String
    , lookup : Array Node
    , idx : Int
    , visible : Bool
    }


resetSession : Session -> SessionFlags -> Session
resetSession session flags =
    { referer = Nothing
    , can_referer = Nothing
    , user = LoggedOut
    , lang = session.lang
    , notif = initNotifCount
    , token_data = RemoteData.NotAsked
    , node_focus = Nothing
    , path_data = Nothing
    , children = Nothing
    , node_data = Nothing
    , tensions_data = Nothing
    , tensions_int = Nothing
    , tensions_ext = Nothing
    , tensions_all = Nothing
    , tensions_count = Nothing
    , tension_head = Nothing
    , orgs_data = Nothing
    , tree_data = Nothing
    , isAdmin = Nothing
    , node_quickSearch = Nothing
    , window_pos = Nothing
    , orga_menu = Nothing
    , tree_menu = session.tree_menu
    , apis = flags.apis
    , screen = flags.screen
    , authorsPanel = Nothing
    , labelsPanel = Nothing
    , newOrgaData = Nothing
    , isWatching = Nothing
    }


fromLocalSession : SessionFlags -> ( Session, List (Cmd msg) )
fromLocalSession flags =
    let
        ( user, cmd1 ) =
            case flags.uctx of
                Just raw ->
                    case JD.decodeValue userCtxDecoder raw of
                        Ok uctx ->
                            ( LoggedIn uctx, Cmd.none )

                        Err err ->
                            ( LoggedOut, Ports.logErr (JD.errorToString err) )

                Nothing ->
                    ( LoggedOut, Cmd.none )

        ( window_pos, cmd2 ) =
            case flags.window_pos of
                Just raw ->
                    case JD.decodeValue windowDecoder raw of
                        Ok v ->
                            ( Just v, Cmd.none )

                        Err err ->
                            ( Nothing, Ports.logErr (JD.errorToString err) )

                Nothing ->
                    ( Nothing, Cmd.none )

        ( lang, cmd3 ) =
            case flags.lang of
                Just raw ->
                    case JD.decodeValue Lang.decoder raw of
                        Ok v ->
                            ( Just v, Cmd.none )

                        Err err ->
                            ( Nothing, Ports.logErr (JD.errorToString err) )

                Nothing ->
                    ( Nothing, Cmd.none )
    in
    ( { referer = Nothing
      , can_referer = Nothing
      , user = user
      , lang = Maybe.withDefault Lang.En lang
      , notif = initNotifCount
      , token_data = RemoteData.NotAsked
      , node_focus = Nothing
      , path_data = Nothing
      , children = Nothing
      , node_data = Nothing
      , tensions_data = Nothing
      , tensions_int = Nothing
      , tensions_ext = Nothing
      , tensions_all = Nothing
      , tensions_count = Nothing
      , tension_head = Nothing
      , orgs_data = Nothing
      , tree_data = Nothing
      , isAdmin = Nothing
      , node_quickSearch = Nothing
      , window_pos = window_pos
      , orga_menu = flags.orga_menu
      , tree_menu = flags.tree_menu
      , apis = flags.apis
      , screen = flags.screen
      , authorsPanel = Nothing
      , labelsPanel = Nothing
      , newOrgaData = Nothing
      , isWatching = Nothing
      }
    , [ cmd1, cmd2, cmd3 ]
    )



--
-- Shared Model
--
{-
   UserSearchPanel
-}


type alias UserSearchPanelModel =
    { isOpen : Bool
    , form : AssigneeForm
    , click_result : GqlData IdPayload
    , action : UserSearchPanelOnClickAction

    -- Lookup
    , lookup : List User
    , pattern : String -- search pattern
    , assignees_data : GqlData (List User)

    --, init_lookup : List User -> Cmd Msg
    --, search_lookup : String -> Cmd Msg
    -- Common
    , refresh_trial : Int
    }


type UserSearchPanelOnClickAction
    = AssignUser
    | SelectUser



{-
   LabelSearchPanel
-}


type alias LabelSearchPanelModel =
    { isOpen : Bool
    , form : LabelForm
    , click_result : GqlData IdPayload
    , action : LabelSearchPanelOnClickAction

    -- Lookup
    , lookup : List Label
    , pattern : String -- search pattern
    , labels_data : GqlData (List Label)

    --, init_lookup : List Label -> Cmd Msg
    --, search_lookup : String -> Cmd Msg
    -- Common
    , refresh_trial : Int
    }


type LabelSearchPanelOnClickAction
    = AssignLabel
    | SelectLabel
