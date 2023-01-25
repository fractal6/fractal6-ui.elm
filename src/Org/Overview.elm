{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2023 Fractale Co

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


port module Org.Overview exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Array
import Assets as A
import Assets.Logo as Logo
import Auth exposing (ErrState(..), parseErr)
import Browser.Events as Events
import Browser.Navigation as Nav
import Bulk exposing (..)
import Bulk.Codecs
    exposing
        ( ActionType(..)
        , DocType(..)
        , Flags_
        , FractalBaseRoute(..)
        , NodeFocus
        , focusFromNameid
        , focusState
        , hasLazyAdminRole
        , nameidFromFlags
        , nearestCircleid
        , nid2rootid
        , tensionCharacFromNode
        , uriFromNameid
        )
import Bulk.Error exposing (viewGqlErrors)
import Bulk.Event exposing (contractToLink, eventToIcon, eventToLink, eventTypeToText, viewContractMedia, viewEventMedia)
import Bulk.View exposing (mediaTension, viewPinnedTensions, viewUsernameLink)
import Codecs exposing (LookupResult, QuickDoc, WindowPos, nodeDecoder)
import Components.ActionPanel as ActionPanel
import Components.AuthModal as AuthModal
import Components.HelperBar as HelperBar
import Components.JoinOrga as JoinOrga
import Components.NodeDoc as NodeDoc
import Components.OrgaMenu as OrgaMenu
import Components.TreeMenu as TreeMenu
import Debug
import Dict exposing (Dict)
import Dom
import Extra exposing (ternary, textH, unwrap, upH)
import Extra.Events exposing (onClickPD, onKeydown)
import Form exposing (isPostSendable)
import Form.Help as Help
import Form.NewTension as NTF exposing (NewTensionInput(..), TensionTab(..))
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.Lang as Lang
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Global exposing (Msg(..), send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, span, table, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, autocomplete, class, classList, disabled, href, id, list, name, placeholder, required, rows, style, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import Json.Decode as JD
import List.Extra as LE
import Loading
    exposing
        ( GqlData
        , ModalData
        , RequestResult(..)
        , WebData
        , fromMaybeData
        , isFailure
        , isSuccess
        , withDefaultData
        , withMapData
        , withMaybeData
        , withMaybeDataMap
        )
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.QueryNode exposing (fetchNodeData, queryJournal, queryNodesSub, queryOrgaTree)
import Query.QueryTension exposing (queryAllTension)
import RemoteData exposing (RemoteData)
import Session exposing (Conf, GlobalCmd(..), NodesQuickSearch, isMobile)
import String
import String.Extra as SE
import Task
import Text as T
import Time
import Url exposing (Url)



---- PROGRAM ----


type alias Flags =
    Flags_


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


mapGlobalOutcmds : List GlobalCmd -> ( List (Cmd Msg), List (Cmd Global.Msg) )
mapGlobalOutcmds gcmds =
    gcmds
        |> List.map
            (\m ->
                case m of
                    -- Global
                    DoFocus nameid ->
                        ( Cmd.none, send (NavigateNode nameid) )

                    DoNavigate link ->
                        ( Cmd.none, send (NavigateRaw link) )

                    DoReplaceUrl url ->
                        ( Cmd.none, send (ReplaceUrl url) )

                    DoUpdateToken ->
                        ( Cmd.none, send UpdateUserToken )

                    DoUpdateUserSession uctx ->
                        ( Cmd.none, send (UpdateUserSession uctx) )

                    DoUpdatePath path ->
                        ( Cmd.none, send (UpdateSessionPath path) )

                    --DoUpdateTree tree ->
                    --    ( Cmd.none, send (UpdateSessionTree tree) )
                    --
                    DoUpdateOrgs orgs ->
                        ( Cmd.none, send (UpdateSessionOrgs orgs) )

                    DoToggleWatchOrga a ->
                        ( Cmd.none, send (ToggleWatchOrga a) )

                    -- Component
                    DoCreateTension a ->
                        ( Cmd.map NewTensionMsg <| send (NTF.OnOpen (FromNameid a)), Cmd.none )

                    DoJoinOrga a ->
                        ( Cmd.map JoinOrgaMsg <| send (JoinOrga.OnOpen a JoinOrga.JoinOne), Cmd.none )

                    DoOpenActionPanel a b c ->
                        ( send <| OpenActionPanel a b c, Cmd.none )

                    DoToggleTreeMenu ->
                        ( Cmd.map TreeMenuMsg <| send TreeMenu.OnToggle, Cmd.none )

                    DoFetchNode nameid ->
                        ( Cmd.map TreeMenuMsg <| send (TreeMenu.FetchNewNode nameid False), Cmd.none )

                    DoAddNodes nodes ->
                        ( Cmd.map TreeMenuMsg <| send (TreeMenu.AddNodes nodes), Cmd.none )

                    DoUpdateNode nameid fun ->
                        ( Cmd.map TreeMenuMsg <| send (TreeMenu.UpdateNode nameid fun), Cmd.none )

                    DoDelNodes nameids ->
                        ( Cmd.map TreeMenuMsg <| send (TreeMenu.DelNodes nameids), Cmd.none )

                    DoMoveNode a b c ->
                        ( Cmd.map TreeMenuMsg <| send (TreeMenu.MoveNode a b c), Cmd.none )

                    -- App
                    DoPushTension tension ->
                        ( send (PushTension tension), Cmd.none )

                    DoUpdateTree tree ->
                        ( send (OnUpdateTree tree), Cmd.none )

                    _ ->
                        ( Cmd.none, Cmd.none )
            )
        |> List.unzip



---- MODEL----


type alias Model =
    { node_focus : NodeFocus
    , path_data : Maybe LocalGraph
    , tree_data : GqlData NodesDict
    , tensions_data : GqlData (List Tension)
    , journal_data : GqlData (List EventNotif)
    , node_data : GqlData NodeData
    , init_tensions : Bool
    , init_data : Bool
    , node_quickSearch : NodesQuickSearch
    , window_pos : WindowPos
    , node_hovered : Maybe Node
    , next_focus : Maybe String
    , activity_tab : ActivityTab
    , depth : Maybe Int
    , legend : Bool

    -- common
    , conf : Conf
    , refresh_trial : Int
    , empty : {}

    -- Components
    , helperBar : HelperBar.State
    , help : Help.State
    , tensionForm : NTF.State
    , actionPanel : ActionPanel.State
    , joinOrga : JoinOrga.State
    , authModal : AuthModal.State
    , orgaMenu : OrgaMenu.State
    , treeMenu : TreeMenu.State
    }


type ActivityTab
    = TensionTab
    | JournalTab


nfirstTensions : Int
nfirstTensions =
    10



---- MSG ----


type Msg
    = PassedSlowLoadTreshold -- timer
    | LoadOrga
    | OnUpdateTree (Maybe NodesDict)
    | PushTension Tension
    | Submit (Time.Posix -> Msg) -- Get the current time
      -- Gql Data Queries
    | GotOrga (GqlData NodesDict)
    | GotTensions (GqlData (List Tension))
    | GotData (GqlData NodeData)
      -- Page
    | SwitchWindow
    | SetLegend Bool
    | UpdatePath
      -- Quick search
    | LookupFocus String
    | ToggleLookup
    | LookupBlur
    | LookupBlur_
    | ChangePattern String
    | ChangeNodeLookup (LookupResult Node)
    | SearchKeyDown Int
    | ChangeActivityTab ActivityTab
    | GotJournal (GqlData (List EventNotif))
      -- Node Action
    | OpenActionPanel String String (Maybe ( Int, Int ))
      -- GP JS Interop
    | FlushGraphpack
    | NodeClicked String
    | NodeHovered String
    | NodeFocused ( String, Int )
    | OnClearTooltip
    | ToggleGraphReverse
      -- Common
    | NoMsg
    | LogErr String
      -- Components
    | HelperBarMsg HelperBar.Msg
    | HelpMsg Help.Msg
    | NewTensionMsg NTF.Msg
    | ActionPanelMsg ActionPanel.Msg
    | JoinOrgaMsg JoinOrga.Msg
    | AuthModalMsg AuthModal.Msg
    | OrgaMenuMsg OrgaMenu.Msg
    | TreeMenuMsg TreeMenu.Msg



---- INIT ----


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        session =
            global.session

        apis =
            session.apis

        conf =
            { screen = global.session.screen, now = global.now, lang = global.session.lang, url = global.url }

        -- Focus
        newFocus =
            flags
                |> nameidFromFlags
                |> focusFromNameid

        -- What has changed
        fs_ =
            focusState OverviewBaseUri session.referer global.url session.node_focus newFocus

        isInit =
            session.tree_data == Nothing || session.path_data == Nothing

        fs =
            { fs_ | isInit = fs_.isInit || isInit }

        --d1 = Debug.log "isInit, orgChange, focuChange, menuChange" [ fs.isInit, fs.orgChange, fs.focusChange, fs.menuChange ]
        --d2 = Debug.log "newfocus" [ newFocus ]
        -- QuickSearch
        qs =
            session.node_quickSearch |> withDefault { pattern = "", lookup = Array.empty, idx = 0, visible = False }

        -- Model init
        model =
            { node_focus = newFocus
            , path_data = ternary fs.orgChange Nothing session.path_data -- Loaded from GraphPack
            , tree_data = fromMaybeData session.tree_data Loading
            , tensions_data = fromMaybeData session.tensions_data Loading
            , journal_data = NotAsked
            , node_data = fromMaybeData session.node_data Loading
            , init_tensions = True
            , init_data = True
            , node_quickSearch = { qs | pattern = "", idx = 0 }
            , window_pos =
                session.window_pos
                    |> withDefault { topRight = "doc", bottomLeft = "activities" }
                    |> (\x ->
                            if isMobile global.session.screen then
                                { x | topRight = x.bottomLeft, bottomLeft = x.topRight }

                            else
                                x
                       )
            , node_hovered = Nothing
            , next_focus = Nothing
            , activity_tab = TensionTab
            , depth = Nothing
            , legend = False

            -- Common
            , conf = conf
            , refresh_trial = 0
            , empty = {}

            -- Components
            , helperBar = HelperBar.init OverviewBaseUri global.url.query newFocus global.session.user
            , help = Help.init session.user conf
            , tensionForm = NTF.init session.user conf
            , actionPanel = ActionPanel.init session.user global.session.screen
            , joinOrga = JoinOrga.init newFocus.nameid session.user session.screen
            , authModal = AuthModal.init session.user Nothing
            , orgaMenu = OrgaMenu.init newFocus global.session.orga_menu global.session.orgs_data global.session.user
            , treeMenu = TreeMenu.init OverviewBaseUri global.url.query newFocus global.session.tree_menu global.session.tree_data global.session.user
            }

        cmds_ =
            if fs.orgChange || isInit then
                [ send LoadOrga
                , Ports.initGraphPack Dict.empty "" -- canvas loading effet

                --, queryCircleTension apis newFocus.nameid GotTensions
                , fetchNodeData apis newFocus.nameid GotData
                ]

            else if fs.focusChange then
                [ fetchNodeData apis newFocus.nameid GotData

                --queryCircleTension apis newFocus.nameid GotTensions
                ]
                    ++ (if fs.menuChange then
                            case session.tree_data of
                                Just _ ->
                                    [ send LoadOrga
                                    , Ports.initGraphPack Dict.empty "" --canvas loading effect
                                    ]

                                Nothing ->
                                    []

                        else
                            [ Ports.focusGraphPack newFocus.nameid ]
                       )

            else if fs.menuChange then
                [ send LoadOrga
                , Ports.initGraphPack Dict.empty "" --canvas loading effect

                --, queryCircleTension apis newFocus.nameid GotTensions
                , fetchNodeData apis newFocus.nameid GotData
                ]

            else
                []

        model2 =
            ternary (cmds_ == []) { model | init_tensions = False, init_data = False } model

        cmds =
            cmds_
                ++ [ sendSleep PassedSlowLoadTreshold 500
                   , Cmd.map OrgaMenuMsg (send OrgaMenu.OnLoad)
                   , Cmd.map TreeMenuMsg (send (TreeMenu.ScrollToElement newFocus.nameid))
                   ]
    in
    ( model2
    , Cmd.batch cmds
    , if fs.refresh then
        send (UpdateSessionFocus (Just newFocus))

      else
        Cmd.none
    )



---- UPDATE ----


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global message model =
    let
        apis =
            global.session.apis
    in
    case message of
        LoadOrga ->
            ( model, queryOrgaTree apis model.node_focus.rootnameid GotOrga, Cmd.none )

        OnUpdateTree tree ->
            case tree of
                Just data ->
                    ( { model | tree_data = Success data }, Cmd.none, send (UpdateSessionTree tree) )

                Nothing ->
                    ( model, Cmd.none, Cmd.none )

        PushTension tension ->
            let
                tensions =
                    hotTensionPush tension model.tensions_data
            in
            ( { model | tensions_data = Success tensions }, Cmd.none, send (UpdateSessionTensions (Just tensions)) )

        PassedSlowLoadTreshold ->
            let
                tree_data =
                    ternary (model.tree_data == Loading) LoadingSlowly model.tree_data

                tensions_data =
                    ternary (model.tensions_data == Loading) LoadingSlowly model.tensions_data

                node_data =
                    ternary (model.node_data == Loading) LoadingSlowly model.node_data
            in
            ( { model | tree_data = tree_data, tensions_data = tensions_data, node_data = node_data }, Cmd.none, Cmd.none )

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        --  Page
        SwitchWindow ->
            let
                win =
                    model.window_pos

                newWin =
                    { win | bottomLeft = win.topRight, topRight = win.bottomLeft }
            in
            ( { model | window_pos = newWin }, Ports.saveWindowpos newWin, send (UpdateSessionWindow (Just newWin)) )

        SetLegend val ->
            ( { model | legend = val }, Cmd.none, Cmd.none )

        UpdatePath ->
            ( { model | path_data = global.session.path_data }, Cmd.none, Cmd.none )

        -- Data queries
        GotOrga result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( model, Ports.raiseAuthModal (uctxFromUser global.session.user), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep LoadOrga 500, send UpdateUserToken )

                OkAuth data ->
                    if Dict.size data > 0 then
                        ( { model | tree_data = Success data }
                        , Cmd.batch [ Ports.initGraphPack data model.node_focus.nameid, Cmd.map TreeMenuMsg (send (TreeMenu.OnSetTree data)) ]
                        , send (UpdateSessionTree (Just data))
                        )

                    else
                        ( { model | tree_data = Failure [ T.nodeNotExist ] }, Cmd.none, Cmd.none )

                _ ->
                    ( { model | tree_data = result }, Cmd.none, Cmd.none )

        GotTensions result ->
            case result of
                Success data ->
                    ( { model | tensions_data = result, init_tensions = False }, Cmd.none, send (UpdateSessionTensions (Just data)) )

                _ ->
                    ( { model | tensions_data = result }, Cmd.none, send (UpdateSessionTensions Nothing) )

        GotData result ->
            case result of
                Success data ->
                    ( { model | node_data = result, init_data = False }, Cmd.none, send (UpdateSessionData (Just data)) )

                _ ->
                    ( { model | node_data = result }, Cmd.none, Cmd.none )

        -- Search
        ToggleLookup ->
            if model.node_quickSearch.visible then
                ( model, send LookupBlur, Cmd.none )

            else
                ( model, send (LookupFocus model.node_quickSearch.pattern), Cmd.none )

        LookupFocus pattern ->
            let
                qs =
                    model.node_quickSearch
            in
            case pattern of
                "" ->
                    case model.path_data of
                        Just path ->
                            case model.tree_data of
                                Success data ->
                                    let
                                        newLookup =
                                            path.focus.children
                                                |> List.filterMap (\n -> Dict.get n.nameid data)
                                                |> Array.fromList
                                    in
                                    ( { model | node_quickSearch = { qs | lookup = newLookup, visible = True, idx = 0 } }, Cmd.none, Cmd.none )

                                _ ->
                                    ( model, Cmd.none, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none, Cmd.none )

                _ ->
                    ( { model | node_quickSearch = { qs | visible = True } }, Cmd.none, Cmd.none )

        LookupBlur ->
            ( model, sendSleep LookupBlur_ 150, Cmd.none )

        LookupBlur_ ->
            let
                qs =
                    model.node_quickSearch
            in
            ( { model | node_quickSearch = { qs | visible = False } }, Cmd.none, Cmd.none )

        ChangePattern pattern ->
            let
                qs =
                    model.node_quickSearch

                cmd =
                    if pattern == "" then
                        LookupFocus pattern |> send

                    else
                        Ports.searchNode pattern
            in
            ( { model | node_quickSearch = { qs | pattern = pattern, visible = True } }
            , cmd
            , Cmd.none
            )

        ChangeNodeLookup nodes_ ->
            let
                qs =
                    model.node_quickSearch
            in
            case nodes_ of
                Ok nodes ->
                    ( { model | node_quickSearch = { qs | lookup = Array.fromList nodes } }, Cmd.none, Cmd.none )

                Err err ->
                    ( model, Ports.logErr err, Cmd.none )

        SearchKeyDown key ->
            let
                qs =
                    model.node_quickSearch

                len =
                    Array.length qs.lookup
            in
            case key of
                13 ->
                    --ENTER
                    case Array.get model.node_quickSearch.idx model.node_quickSearch.lookup of
                        Just n ->
                            ( model, send (NodeClicked n.nameid), Cmd.none )

                        Nothing ->
                            ( model, Cmd.none, Cmd.none )

                27 ->
                    --ESC
                    ( model, send LookupBlur, Cmd.none )

                40 ->
                    --DOWN
                    let
                        newIdx =
                            if len > 0 && qs.idx < len - 1 then
                                qs.idx + 1

                            else
                                0
                    in
                    ( { model | node_quickSearch = { qs | idx = newIdx } }, Cmd.none, Cmd.none )

                38 ->
                    --UP
                    let
                        newIdx =
                            if len > 0 && qs.idx > 0 then
                                qs.idx - 1

                            else if len > 0 && qs.idx == 0 then
                                len - 1

                            else
                                0
                    in
                    ( { model | node_quickSearch = { qs | idx = newIdx } }, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        ChangeActivityTab tab ->
            let
                cmd =
                    if withMaybeData model.journal_data == Nothing then
                        queryJournal apis model.node_focus.nameid GotJournal

                    else
                        Cmd.none
            in
            ( { model | activity_tab = tab, journal_data = LoadingSlowly }, cmd, Cmd.none )

        GotJournal result ->
            ( { model | journal_data = result }, Cmd.none, Cmd.none )

        -- Node Action
        OpenActionPanel domid nameid pos ->
            ( model, Cmd.map ActionPanelMsg (send <| ActionPanel.OnOpen domid nameid (TreeMenu.getOrgaData_ model.treeMenu) pos), Cmd.none )

        -- JS interop
        FlushGraphpack ->
            ( model, Ports.flushGraphPack, Cmd.none )

        NodeClicked nameid ->
            ( model, Cmd.none, send (NavigateNode nameid) )

        NodeHovered nid ->
            let
                ( node, cmd ) =
                    case getNode nid model.tree_data of
                        Just n ->
                            ( Just n, Cmd.none )

                        Nothing ->
                            -- Will close the panel when the a the modal raise
                            ( Nothing, Cmd.map ActionPanelMsg (send ActionPanel.OnClose) )
            in
            ( { model | node_hovered = node }, cmd, Cmd.none )

        NodeFocused ( nameid, maxdepth ) ->
            -- May change the node_focus var
            case localGraphFromOrga nameid model.tree_data of
                Just path ->
                    let
                        nameids =
                            path.focus.children |> List.map (\x -> x.nameid) |> List.append [ path.focus.nameid ]

                        isPathNew =
                            Just path.focus.nameid /= Maybe.map (.focus >> .nameid) global.session.path_data

                        f =
                            path.focus

                        p =
                            global.session.path_data |> Maybe.map (.focus >> .pinned) |> withDefault NotAsked
                    in
                    ( { model | path_data = Just { path | focus = { f | pinned = p } }, depth = Just maxdepth }
                    , Cmd.batch
                        [ Ports.drawButtonsGraphPack
                        , if isPathNew || model.init_tensions then
                            queryAllTension apis nameids nfirstTensions 0 Nothing (Just TensionStatus.Open) Nothing GotTensions

                          else
                            Cmd.none
                        , if not (isFailure model.node_data) && (getNode model.node_focus.nameid model.tree_data == Nothing) then
                            Cmd.map TreeMenuMsg <| send (TreeMenu.FetchNewNode nameid True)

                          else
                            Cmd.none
                        ]
                    , if isPathNew then
                        send (UpdateSessionPath (Just path))

                      else
                        Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none, Cmd.none )

        ToggleGraphReverse ->
            ( model, () |> sendToggleGraphReverse, Cmd.none )

        OnClearTooltip ->
            ( model, Cmd.none, Ports.clearTooltip )

        -- Common
        NoMsg ->
            ( model, Cmd.none, Cmd.none )

        LogErr err ->
            ( model, Ports.logErr err, Cmd.none )

        -- Components
        HelperBarMsg msg ->
            let
                ( data, out ) =
                    HelperBar.update apis msg model.helperBar

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | helperBar = data }, out.cmds |> List.map (\m -> Cmd.map HelperBarMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        HelpMsg msg ->
            let
                ( data, out ) =
                    Help.update apis msg model.help

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | help = data }, out.cmds |> List.map (\m -> Cmd.map HelpMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        NewTensionMsg msg ->
            let
                ( data, out ) =
                    NTF.update apis msg model.tensionForm

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | tensionForm = data }, out.cmds |> List.map (\m -> Cmd.map NewTensionMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        ActionPanelMsg msg ->
            let
                ( data, out ) =
                    ActionPanel.update apis msg model.actionPanel

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | actionPanel = data }, out.cmds |> List.map (\m -> Cmd.map ActionPanelMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        JoinOrgaMsg msg ->
            let
                ( data, out ) =
                    JoinOrga.update apis msg model.joinOrga

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | joinOrga = data }, out.cmds |> List.map (\m -> Cmd.map JoinOrgaMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        AuthModalMsg msg ->
            let
                ( data, out ) =
                    AuthModal.update apis msg model.authModal

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds

                -- reload silently the page if needed
                cmds_extra =
                    out.result
                        |> Maybe.map
                            (\o ->
                                if Tuple.first o then
                                    [ Nav.replaceUrl global.key (Url.toString global.url) ]

                                else
                                    []
                            )
                        |> withDefault []
            in
            ( { model | authModal = data }, out.cmds |> List.map (\m -> Cmd.map AuthModalMsg m) |> List.append (cmds ++ cmds_extra) |> Cmd.batch, Cmd.batch gcmds )

        OrgaMenuMsg msg ->
            let
                ( data, out ) =
                    OrgaMenu.update apis msg model.orgaMenu

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | orgaMenu = data }, out.cmds |> List.map (\m -> Cmd.map OrgaMenuMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        TreeMenuMsg msg ->
            let
                ( data, out ) =
                    TreeMenu.update apis msg model.treeMenu

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | treeMenu = data }, out.cmds |> List.map (\m -> Cmd.map TreeMenuMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ model =
    [ flushGraphPackFromJs (always FlushGraphpack)
    , nodeClickedFromJs NodeClicked

    -- @CODEFACTOR: since node_hovered is know, leftClick and rightClick could be replace by a JS .click() function on #doTension and #doAction...
    -- what would be the advantage of using .click instead of ports ?
    , nodeLeftClickedFromJs
        (\nameid ->
            case localGraphFromOrga nameid model.tree_data of
                Just path ->
                    NewTensionMsg <| NTF.OnOpen (FromPath path)

                Nothing ->
                    NewTensionMsg <| NTF.OnOpen (FromNameid nameid)
        )
    , case model.node_hovered of
        Just node ->
            nodeRightClickedFromJs (\_ -> OpenActionPanel "actionPanelContentTooltip" node.nameid Nothing)

        Nothing ->
            --nodeRightClickedFromJs (\_ -> ActionPanelMsg ActionPanel.OnClose)
            Sub.none
    , if model.legend then
        Events.onMouseUp (Dom.outsideClickClose "canvasLegend" (SetLegend False))

      else
        Sub.none
    , nodeHoveredFromJs NodeHovered
    , nodeFocusedFromJs NodeFocused
    , Ports.lookupNodeFromJs ChangeNodeLookup
    , Ports.reloadPathFromJs (always UpdatePath)
    ]
        ++ (HelperBar.subscriptions |> List.map (\s -> Sub.map HelperBarMsg s))
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        ++ (NTF.subscriptions model.tensionForm |> List.map (\s -> Sub.map NewTensionMsg s))
        ++ (ActionPanel.subscriptions model.actionPanel |> List.map (\s -> Sub.map ActionPanelMsg s))
        ++ (JoinOrga.subscriptions model.joinOrga |> List.map (\s -> Sub.map JoinOrgaMsg s))
        ++ (AuthModal.subscriptions |> List.map (\s -> Sub.map AuthModalMsg s))
        ++ (OrgaMenu.subscriptions |> List.map (\s -> Sub.map OrgaMenuMsg s))
        ++ (TreeMenu.subscriptions |> List.map (\s -> Sub.map TreeMenuMsg s))
        |> Sub.batch



-- Receive from Javascript


port flushGraphPackFromJs : (() -> msg) -> Sub msg


port nodeClickedFromJs : (String -> msg) -> Sub msg


port nodeLeftClickedFromJs : (String -> msg) -> Sub msg


port nodeRightClickedFromJs : (String -> msg) -> Sub msg


port nodeHoveredFromJs : (String -> msg) -> Sub msg


port nodeFocusedFromJs : (( String, Int ) -> msg) -> Sub msg



-- Send to Javascript


port sendToggleGraphReverse : () -> Cmd msg



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    let
        path_data =
            Maybe.map (\x -> Success x) model.path_data |> withDefault Loading

        helperData =
            { path_data = model.path_data
            , isPanelOpen = ActionPanel.isOpen_ "actionPanelHelper" model.actionPanel
            , orgaInfo = global.session.orgaInfo
            }

        panelData =
            { tc = { action = TensionAction.EditRole, action_type = EDIT, doc_type = NODE NodeType.Role }
            , isRight = True
            , domid = "actionPanelHelper"
            , tree_data = model.tree_data
            }
    in
    { title =
        (String.join "/" <| LE.unique [ model.node_focus.rootnameid, model.node_focus.nameid |> String.split "#" |> LE.last |> withDefault "" ])
            ++ " · "
            ++ T.overview
    , body =
        [ div [ class "orgPane" ]
            [ HelperBar.view helperData model.helperBar |> Html.map HelperBarMsg
            , div [ id "mainPane" ] [ view_ global model ]
            ]
        , Help.view model.empty model.help |> Html.map HelpMsg
        , NTF.view { tree_data = model.tree_data, path_data = path_data } model.tensionForm |> Html.map NewTensionMsg
        , JoinOrga.view model.empty model.joinOrga |> Html.map JoinOrgaMsg
        , AuthModal.view model.empty model.authModal |> Html.map AuthModalMsg
        , OrgaMenu.view model.empty model.orgaMenu |> Html.map OrgaMenuMsg
        , TreeMenu.view model.empty model.treeMenu |> Html.map TreeMenuMsg
        , ActionPanel.view panelData model.actionPanel |> Html.map ActionPanelMsg
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    let
        focus_m =
            getNode model.node_focus.nameid model.tree_data

        tid =
            focus_m |> Maybe.map (\nd -> nd.source |> Maybe.map (\b -> b.tension.id)) |> withDefault Nothing |> withDefault ""

        leads =
            model.path_data
                |> Maybe.map (.focus >> .children)
                |> withDefault []
                |> List.map (\x -> getNode x.nameid model.tree_data)
                |> List.filter (\x -> unwrap Nothing .role_type x /= Just RoleType.Owner)
                |> List.filterMap (Maybe.map .first_link >> withDefault Nothing)
                |> LE.uniqueBy .username
                |> List.sortBy .username

        nodeData =
            { focus = model.node_focus
            , tid_r = withMapData (\_ -> tid) model.node_data
            , node = getNode model.node_focus.nameid model.tree_data
            , node_data = withMaybeData model.node_data |> withDefault (NodeData Nothing Nothing)
            , leads = leads
            , isLazy = model.init_data
            , source = OverviewBaseUri
            , hasBeenPushed = True
            , receiver = nearestCircleid model.node_focus.nameid
            , hasInnerToolbar = True
            }

        viewFromPos : String -> Html Msg
        viewFromPos pos =
            case pos of
                "doc" ->
                    NodeDoc.view nodeData Nothing

                "activities" ->
                    div []
                        [ model.path_data
                            |> Maybe.map (.focus >> .pinned >> withMaybeData >> withDefault Nothing)
                            |> withDefault Nothing
                            |> Maybe.map
                                (\x ->
                                    div [ class "mb-4 pb-1" ]
                                        [ viewPinnedTensions 2 model.conf model.node_focus x ]
                                )
                            |> withDefault (text "")
                        , viewActivies model
                        ]

                _ ->
                    text "wrong position"
    in
    div [ id "overview", class "columns is-centered" ]
        [ div [ class "column is-6 is-5-fullhd" ]
            [ --viewSearchBar global.session.user model
              viewCanvas global.session.user model
            , viewFromPos model.window_pos.bottomLeft
            ]
        , div [ class "divider is-vertical is-hidden-mobile", onClick SwitchWindow ] [ text "⇋" ]
        , div [ class "column is-6 is-5-fullhd" ]
            [ div [ id "nextToChart" ]
                [ viewFromPos model.window_pos.topRight ]
            ]
        ]


viewSearchBar : UserState -> Model -> Html Msg
viewSearchBar us model =
    div
        [ id "searchBarOverview" ]
        [ div
            [ class "field has-addons searchBar"

            --, onMouseEnter OnClearTooltip
            ]
            ([ div [ class "control has-icons-left is-expanded" ]
                [ input
                    [ class "input is-small"
                    , type_ "search"
                    , autocomplete False
                    , placeholder T.phQS
                    , value model.node_quickSearch.pattern
                    , onInput ChangePattern
                    , onClick ToggleLookup
                    , onBlur LookupBlur
                    , onKeydown SearchKeyDown

                    --, list "searchList" -- impossible interaction !
                    ]
                    []
                , span [ class "icon is-left" ] [ i [ class "icon-search" ] [] ]
                ]
             ]
                ++ (case model.path_data of
                        Just p ->
                            let
                                node =
                                    getNode p.focus.nameid model.tree_data |> withDefault initNode
                            in
                            [ div [ class "control controlButtons" ]
                                [ span
                                    [ class "button is-small is-link2 is-wrapped"
                                    , attribute "data-modal" "actionModal"
                                    , onClick <| NewTensionMsg (NTF.OnOpen (FromPath p))
                                    ]
                                    [ span [ class "has-text-weight-bold is-wrapped" ] [ text p.focus.name ]
                                    , i [ class "px-1" ] []
                                    ]
                                , viewActionPanel "actionPanelContentSearchBar" us node model.tree_data model.actionPanel
                                ]
                            ]

                        _ ->
                            []
                   )
            )
        , div [ class "control" ]
            [ viewSearchList us model ]
        ]


viewActionPanel : String -> UserState -> Node -> GqlData NodesDict -> ActionPanel.State -> Html Msg
viewActionPanel domid us node o actionPanel =
    case us of
        LoggedIn uctx ->
            let
                isAdmin =
                    List.length (getNodeRights uctx node o) > 0

                hasRole =
                    Just uctx.username == Maybe.map (\fs -> fs.username) node.first_link

                hasConfig =
                    isAdmin || hasRole
            in
            if hasConfig then
                let
                    panelData =
                        { tc = tensionCharacFromNode node
                        , isRight = True
                        , domid = domid
                        , tree_data = o
                        }
                in
                span []
                    [ span [ id domid ]
                        [ span
                            [ class "button is-small clickMe"
                            , classList [ ( "is-light", domid == "actionPanelContentTooltip" ) ]
                            , onClick (OpenActionPanel domid node.nameid Nothing)
                            ]
                            [ i [ class "icon-more-vertical" ] [] ]
                        ]
                    , ActionPanel.view panelData actionPanel |> Html.map ActionPanelMsg
                    ]

            else
                text ""

        LoggedOut ->
            text ""


viewSearchList : UserState -> Model -> Html Msg
viewSearchList _ model =
    let
        qs =
            model.node_quickSearch

        sortedLookup =
            qs.lookup
                |> Array.toList
                |> List.sortWith
                    (\n1 n2 ->
                        case ( n1.type_, n2.type_ ) of
                            ( NodeType.Circle, NodeType.Role ) ->
                                LT

                            ( NodeType.Role, NodeType.Circle ) ->
                                GT

                            _ ->
                                compare n1.name n2.name
                    )

        isHidden =
            (qs.visible == False) || (qs.pattern == "" && sortedLookup == [])
    in
    div
        [ id "searchList", classList [ ( "is-hidden", isHidden ) ] ]
        [ table [ class "table is-fullwidth" ] <|
            if sortedLookup == [] then
                [ tbody [] [ td [] [ text T.noResultsFound ] ] ]

            else
                sortedLookup
                    |> List.indexedMap
                        (\i n ->
                            [ tr
                                [ class "button-light"
                                , classList [ ( "is-active", i == qs.idx ) ]
                                , onClickPD (NodeClicked n.nameid)
                                ]
                              <|
                                [ th [] [ text n.name ] ]
                                    ++ (case n.type_ of
                                            NodeType.Circle ->
                                                [ td [] [ n.parent |> Maybe.map (\p -> p.nameid |> String.split "#" |> LE.last |> withDefault "") |> withDefault "" |> text ]
                                                , td [] [ n.first_link |> Maybe.map (\p -> "@" ++ p.username) |> withDefault "--" |> text ]

                                                --, td [] [ n.first_link |> Maybe.map (\p -> viewUsernameLink p.username) |> withDefault (text "--") ]
                                                ]

                                            NodeType.Role ->
                                                [ td [] [ n.parent |> Maybe.map (\p -> p.nameid |> String.split "#" |> LE.last |> withDefault "") |> withDefault "" |> text ]
                                                , td [] [ n.first_link |> Maybe.map (\p -> "@" ++ p.username) |> withDefault "--" |> text ]
                                                ]
                                       )
                            ]
                                |> List.append
                                    (if i == 0 && n.type_ == NodeType.Circle then
                                        [ td [ class "is-grey is-aligned-center is-size-6" ] [ text T.circle ] ]

                                     else if i == 0 || n.type_ == NodeType.Role && (Array.get (i - 1) (Array.fromList sortedLookup) |> Maybe.map (\x -> x.type_ == NodeType.Circle) |> withDefault False) then
                                        [ td [ class "is-grey is-aligned-center is-size-6" ] [ text T.role ] ]

                                     else
                                        []
                                    )
                        )
                    |> List.concat
                    |> tbody []
                    |> List.singleton
                    |> List.append [ thead [] [ tr [] [ th [] [ text T.name ], th [] [ text T.parent ], th [] [ text T.firstLink ] ] ] ]
        ]


viewCanvas : UserState -> Model -> Html Msg
viewCanvas us model =
    let
        isAdmin =
            case us of
                LoggedIn uctx ->
                    hasLazyAdminRole uctx model.node_focus.rootnameid

                LoggedOut ->
                    False

        isComplex =
            --Maybe.map (\x -> x > 2) model.depth |> withDefault False
            False
    in
    div [ id "canvasParent", classList [ ( "spinner", model.tree_data == LoadingSlowly ) ] ]
        [ case model.tree_data of
            Failure err ->
                viewGqlErrors err

            Success d ->
                if isFailure model.node_data && Dict.get model.node_focus.nameid d == Nothing then
                    viewGqlErrors [ T.nodeNotFound ]

                else
                    text ""

            _ ->
                text ""
        , if model.legend then
            div [ id "canvasLegend", class "box has-background-warning-light has-text-dark" ]
                [ span [ class "is-item-aligned" ] [ i [ attribute "style" "position:relative; bottom:2px; left: -4px;" ] [ Logo.circles ], span [] [ text T.circlesLegend ] ]
                , br [ class "mb-3" ] []
                , span [ class "is-item-aligned" ] [ i [ attribute "style" "position:relative; bottom:2px; left: -4px;" ] [ Logo.focusCircle ], span [] [ text T.focusLegend ] ]
                , br [ class "mb-5" ] []
                , A.icon1 "icon-git-branch icon-lg" T.circleLegend
                , br [ class "mb-2" ] []
                , A.icon1 "icon-leaf icon-lg" T.roleLegend
                , br [ class "mb-2" ] []
                , A.icon1 "icon-queen icon-lg" T.ownerLegend
                , br [ class "mb-2" ] []
                , A.icon1 "icon-king icon-lg" T.coordoLegend
                , br [ class "mb-5" ] []
                , A.icon1 "icon-globe icon-lg" T.visibilityPublicLegend
                , br [ class "mb-2" ] []
                , A.icon1 "icon-lock icon-lg" T.visibilityPrivateLegend
                , br [ class "mb-2" ] []
                , A.icon1 "icon-key icon-lg" T.visibilitySecretLegend
                , br [ class "mb-5" ] []
                , span
                    [ class "button-light has-text-info is-size-7 is-pulled-right"
                    , onClick (HelpMsg Help.OnOpen)
                    ]
                    [ text T.help ]
                ]

          else
            text ""
        , canvas [ id "canvasOrga", class "has-border-light is-invisible" ] []

        {- Hidden classes use in graphpack_d3.js -}
        --
        -- Welcom buttons
        --
        , withMaybeData model.tree_data
            |> withDefault Dict.empty
            |> (\orga ->
                    if isFreshOrga orga then
                        let
                            p =
                                case model.path_data of
                                    Just path ->
                                        FromPath path

                                    Nothing ->
                                        FromNameid model.node_focus.rootnameid
                        in
                        div [ id "welcomeButtons", class "buttons re-small is-invisible" ]
                            [ div
                                [ class "button is-success"
                                , onClick (NewTensionMsg <| NTF.OnOpen p)
                                ]
                                [ text T.createNewTension ]
                            , div [ class "hbar", classList [ ( "is-invisible", not isAdmin ) ] ] []
                            , div
                                [ class "button is-success"
                                , classList [ ( "is-invisible", not isAdmin ) ]
                                , onClick (NewTensionMsg <| NTF.OnOpenCircle p)
                                ]
                                [ text T.createNewCircle ]
                            , div
                                [ class "button is-success"
                                , classList [ ( "is-invisible", not isAdmin ) ]
                                , onClick (NewTensionMsg <| NTF.OnOpenRole p)
                                ]
                                [ text T.createNewRole ]
                            ]

                    else
                        text ""
               )

        --
        -- Graphpack Control buttons
        --
        , div [ id "canvasButtons", class "buttons are-small is-invisible" ]
            ((Maybe.map
                (\path ->
                    [ div
                        [ class "button tooltip has-tooltip-arrow has-tooltip-left"
                        , attribute "data-tooltip" (T.add ++ "...")
                        , onClick <| NewTensionMsg (NTF.OnOpen (FromPath path))
                        ]
                        [ span [ style "padding" "2px" ] [ A.icon "icon-plus icon-xs is-strong" ] ]
                    ]
                )
                model.path_data
                |> withDefault []
             )
                ++ (if isAdmin then
                        [ div
                            [ class "button tooltip has-tooltip-arrow has-tooltip-left"
                            , attribute "data-tooltip" T.inviteMembers
                            , onClick (JoinOrgaMsg (JoinOrga.OnOpen model.node_focus.rootnameid JoinOrga.InviteOne))
                            ]
                            [ span [ style "padding" "2px" ] [ A.icon "icon-user-plus icon-xs" ] ]
                        ]

                    else
                        []
                   )
                ++ [ if (model.node_focus.nameid /= model.node_focus.rootnameid || isComplex) && isAdmin then
                        div [ class "hbar", style "margin-right" "8px" ] []

                     else
                        text ""
                   ]
                ++ (if model.node_focus.nameid == model.node_focus.rootnameid then
                        []

                    else
                        [ div
                            [ class "button tooltip has-tooltip-arrow has-tooltip-left"
                            , attribute "data-tooltip" T.goRoot
                            , onClick (NodeClicked model.node_focus.rootnameid)
                            ]
                            [ A.icon "icon-chevrons-up" ]
                        , div
                            [ class "button tooltip has-tooltip-arrow has-tooltip-left"
                            , attribute "data-tooltip" T.goParent
                            , case model.path_data of
                                Just g ->
                                    LE.getAt 1 (List.reverse g.path)
                                        |> Maybe.map (\x -> x.nameid)
                                        |> withDefault g.focus.nameid
                                        |> NodeClicked
                                        |> onClick

                                Nothing ->
                                    onClick NoMsg
                            ]
                            [ A.icon "icon-chevron-up" ]
                        ]
                   )
                ++ (if isComplex then
                        [ div
                            [ class "button buttonToggle tooltip has-tooltip-arrow has-tooltip-left"
                            , attribute "data-tooltip" T.reverseTooltip
                            , onClick ToggleGraphReverse
                            ]
                            [ span [ style "padding" "2px" ] [ A.icon "icon-sort-amount-desc icon-xs" ] ]
                        ]

                    else
                        []
                   )
                ++ [ div
                        [ class "tag is-rounded has-border-light2 is-light is-info is-small tooltip has-tooltip-arrow has-tooltip-top"

                        -- Pushed to bottom in flex/column parent.
                        , attribute "style" "margin-top:auto; user-select:none;"
                        , attribute "data-tooltip" "Show the legend"
                        , onClick (SetLegend (not model.legend))
                        ]
                        [ text "Legend" ]
                   ]
            )
        , div
            [ id "nodeTooltip"
            , class "is-invisible"

            -- @Warning: data used in graphpack.js
            , attribute "data-event-tension" "doTension"
            , attribute "data-event-action" "doAction"
            ]
            [ span [ id "doTension" ]
                [ span [] [ text "void" ] -- Node name from JS
                , i [ class "icon-plus custom-style" ] []
                ]
            , span [ id "doAction" ]
                [ case model.node_hovered of
                    Just node ->
                        viewActionPanel "actionPanelContentTooltip" us node model.tree_data model.actionPanel

                    Nothing ->
                        text ""
                ]
            ]
        ]


viewActivies : Model -> Html Msg
viewActivies model =
    div
        [ id "activities", class "box is-shrinked2 is-flex-grow" ]
        [ div [ class "title ml-4 mt-2 mb-3" ]
            [ div [ class "level" ]
                [ div [ class "level-left" ]
                    [ div
                        [ class "tooltip has-tooltip-arrow"
                        , case model.path_data of
                            Just p ->
                                attribute "data-tooltip" ([ "Recent activities for the", NodeType.toString p.focus.type_, p.focus.name ] |> List.intersperse " " |> String.join "")

                            Nothing ->
                                class ""
                        ]
                        [ span [ class "help" ] [ text T.recentActivities, text ":" ] ]
                    ]
                , div [ class "level-right" ]
                    [ div [ class "tabs is-small" ]
                        [ ul []
                            [ li [ classList [ ( "is-active", model.activity_tab == TensionTab ) ] ]
                                [ a [ onClickPD (ChangeActivityTab TensionTab), target "_blank", classList [ ( "has-text-grey", model.activity_tab /= TensionTab ) ] ]
                                    [ A.icon1 "icon-exchange icon-sm" T.tensions ]
                                ]
                            , li [ classList [ ( "is-active", model.activity_tab == JournalTab ) ] ]
                                [ a [ onClickPD (ChangeActivityTab JournalTab), target "_blank", classList [ ( "has-text-grey", model.activity_tab /= JournalTab ) ] ]
                                    [ A.icon1 "icon-history icon-sm" T.journal ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , div [ class "content is-size-7", classList [ ( "spinner", model.tensions_data == LoadingSlowly ), ( "is-lazy", model.init_tensions ) ] ]
            [ case model.activity_tab of
                TensionTab ->
                    case model.tensions_data of
                        Success tensions ->
                            if List.length tensions > 0 then
                                List.map (\x -> mediaTension model.conf model.node_focus x False True "is-size-6") tensions
                                    ++ [ div [ class "is-aligned-center mt-1 mb-2" ]
                                            [ a [ class "mx-4 discrete-link", href (uriFromNameid TensionsBaseUri model.node_focus.nameid []) ] [ text T.seeFullList ]
                                            , text "|"
                                            , a [ class "mx-4 discrete-link", href (uriFromNameid TensionsBaseUri model.node_focus.nameid [] ++ "?v=circle") ] [ text T.seeByCircle ]
                                            ]
                                       ]
                                    |> div [ id "tensionsTab" ]

                            else
                                case model.node_focus.type_ of
                                    NodeType.Role ->
                                        div [ class "m-4" ] [ text T.noOpenTensionRole ]

                                    NodeType.Circle ->
                                        div [ class "m-4" ] [ text T.noOpenTensionCircle ]

                        Failure err ->
                            viewGqlErrors err

                        _ ->
                            text ""

                JournalTab ->
                    case model.journal_data of
                        Success events ->
                            List.map (\x -> viewEventNotif model.conf x) events
                                |> div [ id "journalTab" ]

                        Failure err ->
                            viewGqlErrors err

                        LoadingSlowly ->
                            div [ class "spinner" ] []

                        _ ->
                            text ""
            ]
        ]


viewEventNotif : Conf -> EventNotif -> Html Msg
viewEventNotif conf e =
    let
        ue =
            UserEvent "" False []

        link =
            eventToLink ue e

        ev =
            Dict.fromList
                [ ( "id", ue.id )
                , ( "title", e.event_type |> eventTypeToText )
                , ( "title_", e.tension.title )
                , ( "target", e.tension.receiver.name )
                , ( "orga", nid2rootid e.tension.receiver.nameid )
                , ( "date", e.createdAt )
                , ( "author", e.createdBy.username )
                , ( "link", link )
                , ( "icon", eventToIcon e.event_type )
                ]

        node =
            e.tension.receiver
    in
    viewEventMedia conf True ev



-- Utils


nodeFragmentFromOrga : Maybe Node -> GqlData NodeData -> List EmitterOrReceiver -> NodesDict -> NodeFragment
nodeFragmentFromOrga node_m nodeData children_eo ndata =
    node2NodeFragment node_m (withMaybeData nodeData)
