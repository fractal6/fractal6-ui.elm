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


port module Org.Project exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), hasLazyAdminRole)
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Bulk exposing (..)
import Bulk.Board2 exposing (DraftForm, viewBoard)
import Bulk.Codecs exposing (ActionType(..), DocType(..), Flags_, FractalBaseRoute(..), NodeFocus, contractIdCodec, focusFromNameid, focusState, id3Changed, nameidFromFlags, nearestCircleid, uriFromNameid)
import Bulk.Error exposing (viewGqlErrors, viewGqlErrorsLight)
import Bulk.View exposing (viewRole, viewUserFull)
import Components.ActionPanel as ActionPanel
import Components.AuthModal as AuthModal
import Components.HelperBar as HelperBar
import Components.JoinOrga as JoinOrga
import Components.LinkTensionPanel as LinkTensionPanel
import Components.OrgaMenu as OrgaMenu
import Components.ProjectColumnModal as ProjectColumnModal exposing (ModalType(..))
import Components.SearchBar exposing (viewSearchBar)
import Components.TreeMenu as TreeMenu
import Dict
import Extra exposing (insertAt, ternary, unwrap)
import Extra.Url exposing (queryBuilder, queryParser)
import Fifo exposing (Fifo)
import Form.Help as Help
import Form.NewTension as NTF exposing (NewTensionInput(..), TensionTab(..))
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.ProjectColumnType as ProjectColumnType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Generated.Route as Route exposing (toHref)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, button, div, h2, hr, i, input, span, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, classList, href, id, style, type_)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Lazy as Lazy
import List.Extra as LE
import Loading exposing (GqlData, RequestResult(..), fromMaybeData, isFailure, isLoading, withDefaultData, withMapData, withMaybeData, withMaybeMapData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.QueryNode exposing (queryLocalGraph)
import Query.QueryProject exposing (addProjectCard, getProject, moveProjectCard)
import Scroll exposing (scrollToSubBottom)
import Session exposing (Conf, GlobalCmd(..))
import Task
import Text as T
import Time
import Url


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
                        ( [], send (NavigateNode nameid) )

                    DoNavigate link ->
                        ( [], send (NavigateRaw link) )

                    DoReplaceUrl url ->
                        ( [], send (ReplaceUrl url) )

                    DoUpdateToken ->
                        ( [], send UpdateUserToken )

                    DoUpdateUserSession uctx ->
                        ( [], send (UpdateUserSession uctx) )

                    DoUpdatePath path ->
                        ( [], send (UpdateSessionPath path) )

                    DoUpdateTree tree ->
                        ( [], send (UpdateSessionTree tree) )

                    DoUpdateOrgs orgs ->
                        ( [], send (UpdateSessionOrgs orgs) )

                    DoToggleWatchOrga a ->
                        ( [], send (ToggleWatchOrga a) )

                    -- Component
                    DoCreateTension ntm a ->
                        case ntm of
                            Nothing ->
                                ( [ Cmd.map NewTensionMsg <| send (NTF.OnOpen (FromNameid a)) ], Cmd.none )

                            Just NodeType.Circle ->
                                ( [ Cmd.map NewTensionMsg <| send (NTF.OnOpenCircle (FromNameid a)) ], Cmd.none )

                            Just NodeType.Role ->
                                ( [ Cmd.map NewTensionMsg <| send (NTF.OnOpenRole (FromNameid a)) ], Cmd.none )

                    DoJoinOrga a ->
                        ( [ Cmd.map JoinOrgaMsg <| send (JoinOrga.OnOpen a JoinOrga.JoinOne) ], Cmd.none )

                    DoOpenActionPanel a b c ->
                        ( [ send <| OpenActionPanel a b c ], Cmd.none )

                    DoToggleTreeMenu ->
                        ( [ Cmd.map TreeMenuMsg <| send TreeMenu.OnToggle ], Cmd.none )

                    DoFetchNode nameid ->
                        ( [ Cmd.map TreeMenuMsg <| send (TreeMenu.FetchNewNode nameid False) ], Cmd.none )

                    DoAddNodes nodes ->
                        ( [ Cmd.map TreeMenuMsg <| send (TreeMenu.AddNodes nodes) ], Cmd.none )

                    --DoUpdateNode nameid fun ->
                    --    ( Cmd.map TreeMenuMsg <| send (TreeMenu.UpdateNode nameid fun), Cmd.none )
                    DoDelNodes nameids ->
                        ( [ Cmd.map TreeMenuMsg <| send (TreeMenu.DelNodes nameids) ], Cmd.none )

                    DoMoveNode a b c ->
                        ( [ Cmd.map TreeMenuMsg <| send (TreeMenu.MoveNode a b c) ], Cmd.none )

                    -- App
                    DoUpdateNode nameid fun ->
                        ( [ Cmd.map TreeMenuMsg <| send (TreeMenu.UpdateNode nameid fun), send DoLoad ], Cmd.none )

                    _ ->
                        ( [], Cmd.none )
            )
        |> List.unzip
        |> Tuple.mapFirst List.concat



--
-- Model
--


type alias Model =
    { -- Focus
      node_focus : NodeFocus
    , path_data : GqlData LocalGraph

    -- Page
    , projectid : String
    , project_data : GqlData ProjectData
    , isAddingDraft : Maybe DraftForm

    -- Board
    , boardHeight : Maybe Float
    , movingCard : Maybe ProjectCard
    , linkTensionPanel : LinkTensionPanel.State
    , movingHoverCol : Maybe { pos : Int, colid : String, length : Int }
    , movingHoverT : Maybe { pos : Int, cardid : String, colid : String }
    , dragCount : Int
    , draging : Bool
    , projectColumnModal : ProjectColumnModal.State
    , board_result : GqlData String -- track board remote result silently

    -- Common
    , conf : Conf
    , refresh_trial : Int
    , empty : {}

    -- Components
    , helperBar : HelperBar.State
    , actionPanel : ActionPanel.State
    , help : Help.State
    , joinOrga : JoinOrga.State
    , tensionForm : NTF.State
    , authModal : AuthModal.State
    , orgaMenu : OrgaMenu.State
    , treeMenu : TreeMenu.State
    }



--
-- INIT
--


type alias Flags =
    { param1 : String, param2 : String }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        apis =
            global.session.apis

        conf =
            { screen = global.session.screen, now = global.now, lang = global.session.lang, url = global.url }

        -- Query parameters
        query =
            queryParser global.url

        -- Focus
        rootnameid =
            flags.param1 |> Url.percentDecode |> withDefault ""

        projectid =
            "0x" ++ flags.param2

        newFocus =
            NodeFocus rootnameid rootnameid NodeType.Circle

        -- What has changed
        fs =
            focusState ProjectBaseUri global.session.referer global.url global.session.node_focus newFocus

        model =
            { node_focus = newFocus
            , path_data =
                global.session.path_data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , projectid = projectid
            , project_data = ternary fs.orgChange Loading (fromMaybeData global.session.project_data Loading)
            , isAddingDraft = Nothing

            -- Board
            , boardHeight = Nothing
            , movingCard = Nothing
            , movingHoverCol = Nothing
            , movingHoverT = Nothing
            , dragCount = 0
            , draging = False

            --
            , projectColumnModal = ProjectColumnModal.init projectid global.session.user
            , linkTensionPanel = LinkTensionPanel.init projectid global.session.user
            , board_result = NotAsked

            -- Common
            , conf = conf
            , tensionForm = NTF.init global.session.user conf
            , refresh_trial = 0
            , empty = {}
            , helperBar = HelperBar.init ProjectsBaseUri global.url.query newFocus global.session.user
            , help = Help.init global.session.user conf
            , joinOrga = JoinOrga.init newFocus.nameid global.session.user global.session.screen
            , authModal = AuthModal.init global.session.user Nothing
            , orgaMenu = OrgaMenu.init newFocus global.session.orga_menu global.session.orgs_data global.session.user
            , treeMenu = TreeMenu.init ProjectsBaseUri global.url.query newFocus global.session.tree_menu global.session.tree_data global.session.user
            , actionPanel = ActionPanel.init global.session.user global.session.screen
            }

        cmds =
            [ ternary fs.focusChange (queryLocalGraph apis newFocus.nameid True (GotPath True)) Cmd.none
            , send DoLoad
            , Ports.hide "footBar"
            , Task.attempt FitBoard (Dom.getElement "projectView")
            , sendSleep PassedSlowLoadTreshold 500
            , sendSleep (ScrollToElement "projectView") 333
            , Cmd.map OrgaMenuMsg (send OrgaMenu.OnLoad)
            , Cmd.map TreeMenuMsg (send TreeMenu.OnLoad)
            ]

        refresh =
            Maybe.map (\x -> id3Changed x.id global.url) global.session.project_data |> withDefault True
    in
    ( model
    , Cmd.batch cmds
    , if fs.menuChange || refresh then
        send (UpdateSessionFocus (Just newFocus))

      else
        Cmd.none
    )



--
-- Msg
--


type Msg
    = --Loading
      PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
    | GotPath Bool (GqlData LocalGraph) -- GraphQL
      -- Page
    | DoLoad
    | GotProject (GqlData ProjectData) -- Rest
    | OpenTensionPane
      -- Board
    | OnResize Int Int
    | FitBoard (Result Dom.Error Dom.Element)
    | OnMove { pos : Int, colid : String, length : Int } ProjectCard
    | OnCancelHov
    | OnEndMove
    | OnMoveEnterCol { pos : Int, colid : String, length : Int } Bool
    | OnMoveLeaveCol
    | OnMoveLeaveCol_
    | OnMoveEnterT { pos : Int, cardid : String, colid : String }
    | OnMoveDrop String
    | GotCardMoved (GqlData IdPayload)
    | OnCardClick (Maybe ProjectCard)
    | OnCardClick_ (Maybe ProjectCard)
      --
    | OnAddCol
    | OnAddDraft String
    | OnDraftEdit String
    | OnDraftKeydown Int
    | OnDraftCancel
    | OnAddDraftAck (GqlData (List ProjectCard))
    | OnClearBoardResult
      -- Common
    | NoMsg
    | LogErr String
    | OnGoRoot
    | ScrollToElement String
    | OpenActionPanel String String (Maybe ( Int, Int ))
      -- Components
    | HelperBarMsg HelperBar.Msg
    | HelpMsg Help.Msg
    | NewTensionMsg NTF.Msg
    | JoinOrgaMsg JoinOrga.Msg
    | AuthModalMsg AuthModal.Msg
    | OrgaMenuMsg OrgaMenu.Msg
    | TreeMenuMsg TreeMenu.Msg
    | ActionPanelMsg ActionPanel.Msg
    | ProjectColumnModalMsg ProjectColumnModal.Msg
    | LinkTensionPanelMsg LinkTensionPanel.Msg


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global message model =
    let
        apis =
            global.session.apis
    in
    case message of
        PassedSlowLoadTreshold ->
            let
                project_data =
                    ternary (model.project_data == Loading) LoadingSlowly model.project_data
            in
            ( { model | project_data = project_data }, Cmd.none, Cmd.none )

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        GotPath isInit result ->
            case result of
                Success path ->
                    let
                        prevPath =
                            if isInit then
                                { path | path = [] }

                            else
                                withDefaultData path model.path_data
                    in
                    case path.root of
                        Just root ->
                            let
                                newPath =
                                    { prevPath | root = Just root, path = path.path ++ (List.tail prevPath.path |> withDefault []) }
                            in
                            ( { model | path_data = Success newPath }, Cmd.none, send (UpdateSessionPath (Just newPath)) )

                        Nothing ->
                            let
                                newPath =
                                    { prevPath | path = path.path ++ (List.tail prevPath.path |> withDefault []) }

                                nameid =
                                    List.head path.path |> Maybe.map .nameid |> withDefault ""
                            in
                            ( { model | path_data = Success newPath }, queryLocalGraph apis nameid False (GotPath False), Cmd.none )

                _ ->
                    ( { model | path_data = result }, Cmd.none, Cmd.none )

        DoLoad ->
            ( model, Cmd.batch [ getProject apis model.projectid GotProject ], Cmd.none )

        GotProject result ->
            ( { model | project_data = result }
            , Cmd.batch [ Task.attempt FitBoard (Dom.getElement "projectView") ]
            , Cmd.none
            )

        OpenTensionPane ->
            ( model
            , Cmd.batch
                [ Cmd.map LinkTensionPanelMsg (send LinkTensionPanel.OnOpen)
                , Cmd.map TreeMenuMsg (send TreeMenu.OnRequireData)
                ]
            , Cmd.none
            )

        -- Board
        OnResize w h ->
            let
                conf =
                    model.conf

                newScreen =
                    { w = w, h = h }

                newConf =
                    { conf | screen = newScreen }
            in
            ( { model | conf = newConf }, Task.attempt FitBoard (Dom.getElement "projectView"), send (UpdateSessionScreen newScreen) )

        FitBoard elt ->
            case elt of
                Ok e ->
                    let
                        h =
                            if e.viewport.height - e.element.y < 511 then
                                -- allow y-scroll here. Substract the header size.
                                e.viewport.height - 79

                            else
                                e.viewport.height - e.element.y
                    in
                    ( { model | boardHeight = Just h }, Cmd.none, Cmd.none )

                Err _ ->
                    ( model, Cmd.none, Cmd.none )

        OnMove col card ->
            ( { model | draging = True, dragCount = 0, movingHoverCol = Just col, movingCard = Just card }, Cmd.none, Cmd.none )

        OnEndMove ->
            let
                newModel =
                    { model | draging = False }
            in
            Maybe.map3
                (\card { pos, colid } c_hover ->
                    if card.id == c_hover.cardid then
                        ( newModel, sendSleep OnCancelHov 300, Cmd.none )

                    else
                        -- Do not wait the query to success to move the column.
                        let
                            pj =
                                withMapData
                                    (\data ->
                                        let
                                            pos_fixed =
                                                ternary (colid == card.colid && c_hover.pos > card.pos)
                                                    (c_hover.pos - 1)
                                                    c_hover.pos
                                        in
                                        data
                                            -- Remove the card from old pos
                                            |> (\d -> { d | columns = removeCard card d.columns })
                                            -- Add the card in new pos tension to list
                                            |> (\d -> { d | columns = pushCard { card | colid = colid, pos = pos_fixed } d.columns })
                                    )
                                    model.project_data
                        in
                        ( { newModel | project_data = pj, board_result = Loading }
                        , Cmd.batch
                            [ moveProjectCard apis card.id c_hover.pos colid GotCardMoved
                            , send OnCancelHov
                            ]
                        , Cmd.none
                        )
                )
                model.movingCard
                model.movingHoverCol
                model.movingHoverT
                |> withDefault
                    ( newModel, sendSleep OnCancelHov 300, Cmd.none )

        OnCardClick c ->
            -- Highlight the border and show ellipsis on click
            -- or unselect.
            case c of
                Just _ ->
                    ( model, sendSleep (OnCardClick_ c) 50, Cmd.none )

                Nothing ->
                    ( { model | movingCard = Nothing }, Cmd.none, Cmd.none )

        OnCardClick_ c ->
            ( { model | movingCard = c }, Cmd.none, Cmd.none )

        OnCancelHov ->
            --let
            --    l1 =
            --        Debug.log "Cancel hov" ""
            --in
            ( { model | movingHoverCol = Nothing, movingHoverT = Nothing }, Cmd.none, Cmd.none )

        OnMoveEnterCol hover reset ->
            -- @DEBUG: How to optimize / simplify that ?
            -- Does "dragCount" still usefull ??
            let
                ( is_last, c_h ) =
                    Maybe.map2
                        (\ch h ->
                            ( ch.colid == h.colid && ch.pos == h.length - 1, Just { ch | pos = ch.pos + 1 } )
                        )
                        model.movingHoverT
                        model.movingHoverCol
                        |> withDefault ( False, model.movingHoverT )
            in
            if Just hover == model.movingHoverCol && not reset then
                -- ?
                ( { model | dragCount = 1 }, Cmd.none, Cmd.none )

            else if Just hover == model.movingHoverCol && reset && is_last then
                ( { model | movingHoverT = c_h }, Cmd.none, Cmd.none )

            else
                let
                    -- Add a virtual card hover in empty columns in order to be able to move card there.
                    ( last_cardid, n_cards ) =
                        withMaybeMapData
                            (\d ->
                                LE.find (\x -> x.id == hover.colid) d.columns
                                    |> unwrap ( "", -1 )
                                        (\cols -> ( LE.last cols.cards |> unwrap "" .id, List.length cols.cards ))
                            )
                            model.project_data
                            |> withDefault ( "", -1 )

                    mht_ =
                        if Maybe.map .colid model.movingHoverT /= Just hover.colid then
                            Nothing

                        else
                            model.movingHoverT

                    mht =
                        case mht_ of
                            Nothing ->
                                if n_cards == 0 then
                                    Just { pos = 0, cardid = "", colid = hover.colid }

                                else if n_cards > 0 then
                                    Just { pos = n_cards, cardid = last_cardid, colid = hover.colid }

                                else
                                    Nothing

                            Just _ ->
                                model.movingHoverT
                in
                ( { model | dragCount = 1, movingHoverCol = Just hover, movingHoverT = mht }, Cmd.none, Cmd.none )

        OnMoveLeaveCol ->
            ( { model | dragCount = model.dragCount - 1 }, sendSleep OnMoveLeaveCol_ 15, Cmd.none )

        OnMoveLeaveCol_ ->
            if model.dragCount < 0 && model.draging then
                ( model, send OnCancelHov, Cmd.none )

            else
                ( model, Cmd.none, Cmd.none )

        OnMoveEnterT hover ->
            --let
            --    l1 =
            --        Debug.log "On move enter Card" hover.pos
            --in
            ( { model | movingHoverT = Just hover }, Cmd.none, Cmd.none )

        OnMoveDrop nameid ->
            -- @not implemented.
            ( { model | movingCard = Nothing, movingHoverCol = Nothing, movingHoverT = Nothing }
            , Cmd.none
            , Cmd.none
            )

        GotCardMoved result ->
            ( { model | board_result = withMapData .id result }, Cmd.none, Cmd.none )

        OnAddCol ->
            let
                pos =
                    withMaybeData model.project_data |> unwrap [] .columns |> List.length
            in
            ( model, Cmd.map ProjectColumnModalMsg (send (ProjectColumnModal.OnOpenAdd pos)), Cmd.none )

        OnAddDraft colid ->
            let
                title =
                    Maybe.map .title model.isAddingDraft |> withDefault ""

                pos =
                    model.project_data
                        |> withMaybeMapData (\a -> LE.find (\b -> b.id == colid) a.columns |> unwrap [] .cards |> List.length)
                        |> withDefault 0

                uctx =
                    uctxFromUser global.session.user
            in
            ( { model | isAddingDraft = Just { uctx = uctx, tids = [ Nothing ], post = Dict.empty, title = title, colid = colid, pos = pos } }
            , Cmd.batch [ Ports.focusOn "draft-card-editable", scrollToSubBottom colid NoMsg ]
            , Cmd.none
            )

        OnDraftEdit val ->
            let
                form =
                    model.isAddingDraft

                title =
                    String.replace "<br>" "" val
                        |> String.replace "<div>" ""
                        |> String.replace "</div>" ""
            in
            ( { model | isAddingDraft = Maybe.map (\f -> { f | title = title }) form }, Cmd.none, Cmd.none )

        OnDraftKeydown key ->
            case key of
                13 ->
                    --ENTER
                    case model.isAddingDraft of
                        Just form ->
                            ternary (form.title /= "" && not (isLoading model.board_result))
                                ( { model | board_result = Loading }, addProjectCard apis form OnAddDraftAck, Cmd.none )
                                ( model, Cmd.none, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none, Cmd.none )

                27 ->
                    --ESC
                    ( { model | isAddingDraft = Nothing }, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        OnDraftCancel ->
            ( { model | isAddingDraft = Nothing }, Cmd.none, Cmd.none )

        OnAddDraftAck result ->
            case result of
                Success cards ->
                    let
                        project_data =
                            withMapData
                                (\d ->
                                    { d | columns = List.foldl (\c cols -> pushCard c cols) d.columns cards }
                                )
                                model.project_data
                    in
                    ( { model | project_data = project_data, isAddingDraft = Nothing, board_result = NotAsked }
                    , send (OnAddDraft (List.head cards |> unwrap "" .colid))
                    , Cmd.none
                    )

                Failure err ->
                    ( { model | board_result = Failure err }, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        OnClearBoardResult ->
            ( { model | board_result = NotAsked }, Cmd.none, Cmd.none )

        -- Common
        NoMsg ->
            ( model, Cmd.none, Cmd.none )

        LogErr err ->
            ( model, Ports.logErr err, Cmd.none )

        OnGoRoot ->
            let
                query =
                    global.url.query |> Maybe.map (\uq -> "?" ++ uq) |> Maybe.withDefault ""
            in
            ( model, Cmd.none, send (NavigateRaw (uriFromNameid ProjectsBaseUri model.node_focus.rootnameid [] ++ query)) )

        ScrollToElement did ->
            ( model, Scroll.scrollToElement did NoMsg, Cmd.none )

        OpenActionPanel domid nameid pos ->
            ( model, Cmd.map ActionPanelMsg (send <| ActionPanel.OnOpen domid nameid (TreeMenu.getOrgaData_ model.treeMenu) pos), Cmd.none )

        -- Components
        HelperBarMsg msg ->
            let
                ( data, out ) =
                    HelperBar.update apis msg model.helperBar

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | helperBar = data }, out.cmds |> List.map (\m -> Cmd.map HelperBarMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        NewTensionMsg msg ->
            let
                ( tf, out ) =
                    NTF.update apis msg model.tensionForm

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | tensionForm = tf }, out.cmds |> List.map (\m -> Cmd.map NewTensionMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        HelpMsg msg ->
            let
                ( help, out ) =
                    Help.update apis msg model.help

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | help = help }, out.cmds |> List.map (\m -> Cmd.map HelpMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

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

                extra_cmd =
                    if
                        ((out.result == Just ( True, True ))
                            || (out.result == Just ( True, False ))
                        )
                            && not (LinkTensionPanel.hasTargets_ model.linkTensionPanel)
                    then
                        send (LinkTensionPanelMsg (LinkTensionPanel.SetTargets model.path_data <| TreeMenu.getList_ model.node_focus.nameid data))

                    else
                        Cmd.none
            in
            ( { model | treeMenu = data }, out.cmds |> List.map (\m -> Cmd.map TreeMenuMsg m) |> List.append (extra_cmd :: cmds) |> Cmd.batch, Cmd.batch gcmds )

        ActionPanelMsg msg ->
            let
                ( data, out ) =
                    ActionPanel.update apis msg model.actionPanel

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | actionPanel = data }
            , out.cmds
                |> List.map (\m -> Cmd.map ActionPanelMsg m)
                |> List.append cmds
                |> Cmd.batch
            , Cmd.batch gcmds
            )

        ProjectColumnModalMsg msg ->
            let
                ( data, out ) =
                    ProjectColumnModal.update apis msg model.projectColumnModal

                pd =
                    case out.result of
                        Just ( a, b ) ->
                            withMapData
                                (\x ->
                                    case a of
                                        AddColumn ->
                                            { x | columns = x.columns ++ [ b ] }

                                        EditColumn ->
                                            { x
                                                | columns =
                                                    LE.updateIf (\c -> c.id == b.id)
                                                        (\c -> { c | name = b.name, color = b.color, pos = b.pos })
                                                        x.columns
                                            }

                                        _ ->
                                            x
                                )
                                model.project_data

                        Nothing ->
                            model.project_data

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | projectColumnModal = data, project_data = pd }, out.cmds |> List.map (\m -> Cmd.map ProjectColumnModalMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        LinkTensionPanelMsg msg ->
            let
                ( data, out ) =
                    LinkTensionPanel.update apis msg model.linkTensionPanel

                pd =
                    case out.result of
                        Just ( colid, cards ) ->
                            withMapData
                                (\x ->
                                    case LE.findIndex (\c -> c.id == colid) x.columns of
                                        Just i ->
                                            { x
                                                | columns =
                                                    LE.updateAt i
                                                        (\c -> { c | cards = c.cards ++ cards })
                                                        x.columns
                                            }

                                        Nothing ->
                                            let
                                                noStatusCol =
                                                    { id = colid
                                                    , name = "No Status"
                                                    , color = Nothing
                                                    , pos = 0
                                                    , col_type = ProjectColumnType.NoStatusColumn
                                                    , cards = cards
                                                    }
                                            in
                                            { x | columns = noStatusCol :: x.columns }
                                )
                                model.project_data

                        Nothing ->
                            model.project_data

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | linkTensionPanel = data, project_data = pd }, out.cmds |> List.map (\m -> Cmd.map LinkTensionPanelMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ model =
    [ unselectCardFromJs (always (OnCardClick Nothing))
    ]
        ++ (HelperBar.subscriptions |> List.map (\s -> Sub.map HelperBarMsg s))
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        ++ (NTF.subscriptions model.tensionForm |> List.map (\s -> Sub.map NewTensionMsg s))
        ++ (JoinOrga.subscriptions model.joinOrga |> List.map (\s -> Sub.map JoinOrgaMsg s))
        ++ (AuthModal.subscriptions |> List.map (\s -> Sub.map AuthModalMsg s))
        ++ (OrgaMenu.subscriptions |> List.map (\s -> Sub.map OrgaMenuMsg s))
        ++ (TreeMenu.subscriptions |> List.map (\s -> Sub.map TreeMenuMsg s))
        ++ (ActionPanel.subscriptions model.actionPanel |> List.map (\s -> Sub.map ActionPanelMsg s))
        ++ (ProjectColumnModal.subscriptions model.projectColumnModal |> List.map (\s -> Sub.map ProjectColumnModalMsg s))
        ++ (LinkTensionPanel.subscriptions model.linkTensionPanel |> List.map (\s -> Sub.map LinkTensionPanelMsg s))
        |> Sub.batch


port unselectCardFromJs : (() -> msg) -> Sub msg



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    let
        helperData =
            { path_data = withMaybeData model.path_data
            , isPanelOpen = ActionPanel.isOpen_ "actionPanelHelper" model.actionPanel
            , orgaInfo = global.session.orgaInfo
            }

        panelData =
            { tc = { action = TensionAction.EditRole, action_type = EDIT, doc_type = NODE NodeType.Role }
            , isRight = True
            , domid = "actionPanelHelper"
            , tree_data = TreeMenu.getOrgaData_ model.treeMenu
            }

        tree_data =
            TreeMenu.getOrgaData_ model.treeMenu
    in
    { title =
        case model.project_data of
            Success p ->
                T.project ++ " · " ++ p.name

            _ ->
                "Loading..."
    , body =
        [ div [ class "orgPane unselect-card-click-trigger" ]
            [ HelperBar.view helperData model.helperBar |> Html.map HelperBarMsg
            , div [ id "mainPane" ]
                [ view_ global model
                , case model.project_data of
                    Success data ->
                        viewProject data model

                    Failure err ->
                        viewGqlErrors err

                    _ ->
                        div [ class "spinner" ] []
                ]
            , ProjectColumnModal.view {} model.projectColumnModal |> Html.map ProjectColumnModalMsg
            ]
        , Help.view model.empty model.help |> Html.map HelpMsg
        , NTF.view { tree_data = tree_data, path_data = model.path_data } model.tensionForm |> Html.map NewTensionMsg
        , JoinOrga.view model.empty model.joinOrga |> Html.map JoinOrgaMsg
        , AuthModal.view model.empty model.authModal |> Html.map AuthModalMsg
        , OrgaMenu.view model.empty model.orgaMenu |> Html.map OrgaMenuMsg
        , TreeMenu.view model.empty model.treeMenu |> Html.map TreeMenuMsg
        , ActionPanel.view panelData model.actionPanel |> Html.map ActionPanelMsg
        , LinkTensionPanel.view { tree_data = tree_data, path_data = model.path_data } model.linkTensionPanel |> Html.map LinkTensionPanelMsg
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    let
        isAdmin =
            case global.session.user of
                LoggedIn uctx ->
                    --hasAdminRole uctx (withMaybeData model.path_data)
                    hasLazyAdminRole uctx Nothing model.node_focus.rootnameid

                LoggedOut ->
                    False
    in
    div [ class "columns is-centered" ]
        [ div [ class "column is-12 is-11-desktop is-10-fullhd pb-0" ]
            [ div [ class "columns is-centered mb-0" ]
                [ div [ class "column is-tree-quarter pb-1" ]
                    [ case model.project_data of
                        Success p ->
                            viewSearchBar p model

                        _ ->
                            text ""
                    ]
                , div [ class "column is-one-quarter is-flex is-align-self-flex-start pt-0 pb-1" ]
                    [ div [ class "button is-small is-pushed-right", onClick OpenTensionPane ]
                        [ A.icon1 "icon-plus" "Add tensions to project" ]
                    ]
                ]

            -- User notification
            , case model.board_result of
                Failure err ->
                    div [ class "f6-notification notification is-danger is-light" ]
                        [ button [ class "delete", onClick OnClearBoardResult ] []
                        , viewGqlErrorsLight err
                        ]

                _ ->
                    text ""
            ]
        ]


viewSearchBar : ProjectData -> Model -> Html Msg
viewSearchBar project model =
    div [ id "searchBarProject", class "searchBar" ]
        [ h2 [ class "subtitle is-strong" ] [ text project.name ]

        --div [ class "columns mb-0" ] [
        --  h2 [] [text title]
        --, div [ class "column is-5" ]
        --  [ div [ class "field has-addons" ]
        --      [ div [ class "control is-expanded" ]
        --          [ input
        --              [ class "is-rounded input is-small pr-6"
        --              , type_ "search"
        --              , autocomplete False
        --              , autofocus False
        --              , placeholder T.searchTensions
        --              , value model.pattern
        --              , onInput ChangePattern
        --              , onKeydown SearchKeyDown
        --              ]
        --              []
        --          , span [ class "icon-input-flex-right" ]
        --              [ if model.pattern_init /= "" then
        --                  span [ class "delete is-hidden-mobile", onClick (SubmitTextSearch "") ] []
        --                else
        --                  text ""
        --              , span [ class "vbar has-border-color" ] []
        --              , span [ class "button-light is-w px-1", onClick (SearchKeyDown 13) ]
        --                  [ A.icon "icon-search" ]
        --              ]
        --          ]
        --      ]
        --  ]
        ]


viewProject : ProjectData -> Model -> Html Msg
viewProject data model =
    let
        op =
            { hasTaskMove = True
            , hasNewCol = True
            , isAddingDraft = model.isAddingDraft
            , conf = model.conf
            , node_focus = model.node_focus
            , boardId = "projectView"
            , boardHeight = model.boardHeight
            , movingCard = model.movingCard
            , movingHoverCol = model.movingHoverCol
            , movingHoverT = model.movingHoverT

            -- Board Msg
            , onMove = OnMove
            , onCancelHov = OnCancelHov
            , onEndMove = OnEndMove
            , onMoveEnterCol = OnMoveEnterCol
            , onMoveLeaveCol = OnMoveLeaveCol
            , onMoveEnterT = OnMoveEnterT
            , onMoveDrop = OnMoveDrop
            , noMsg = NoMsg
            , onAddCol = OnAddCol
            , onDraftEdit = OnDraftEdit
            , onDraftKeydown = OnDraftKeydown
            , onDraftCancel = OnDraftCancel
            , onCardClick = OnCardClick
            }

        columns =
            data.columns |> List.filter (\x -> not (x.col_type == ProjectColumnType.NoStatusColumn && List.length x.cards == 0))
    in
    viewBoard op viewHeader columns


viewHeader : ProjectColumn -> Maybe ProjectCard -> Html Msg
viewHeader col card =
    span []
        [ div [ class "level" ]
            [ div [ class "level-left ml-3" ] [ span [ class "mr-3", style "color" (withDefault "lightgrey" col.color) ] [ A.icon "icon-circle1 icon-lg" ], text col.name ]
            , span [ class "level-left" ]
                [ span
                    [ class "tag is-rounded-light button-light is-w has-border mx-1"
                    , onClick (OnAddDraft col.id)
                    ]
                    [ A.icon "icon-plus" ]
                , if col.col_type /= ProjectColumnType.NoStatusColumn then
                    div [ class "dropdown mx-2 is-align-self-baseline is-right" ]
                        [ div [ class "dropdown-trigger is-w is-h" ]
                            [ div
                                [ class "ellipsis"
                                , attribute "aria-controls" ("edit-ellipsis-" ++ col.id)
                                , attribute "aria-haspopup" "true"
                                ]
                                [ A.icon "icon-more-horizontal icon-lg" ]
                            ]
                        , div [ id ("edit-ellipsis-" ++ col.id), class "dropdown-menu", attribute "role" "menu" ]
                            [ div [ class "dropdown-content p-0" ] <|
                                [ div [ class "dropdown-item button-light", onClick (ProjectColumnModalMsg (ProjectColumnModal.OnOpenEdit col.id)) ] [ text T.edit ]
                                , hr [ class "dropdown-divider" ] []
                                , div [ class "dropdown-item button-light" ] [ text T.delete ]
                                ]
                            ]
                        ]

                  else
                    text ""
                ]
            ]
        ]



--
-- Utils
--


pushCard : ProjectCard -> List ProjectColumn -> List ProjectColumn
pushCard c columns =
    LE.updateIf
        (\a -> a.id == c.colid)
        (\a ->
            { a
                | cards = insertAt c.pos c a.cards

                -- Not needed, since we work in direct position on the front.
                --|> (\cards ->
                --        -- Increment the position of the elements to take into account the new insertion.
                --        let
                --            ( before, after ) =
                --                LE.splitAt (c.pos + 1) cards
                --        in
                --        before ++ List.map (\b -> { b | pos = b.pos + 1 }) after
                --   )
            }
        )
        columns


removeCard : ProjectCard -> List ProjectColumn -> List ProjectColumn
removeCard c columns =
    LE.updateIf
        (\a -> a.id == c.colid)
        (\a -> { a | cards = LE.removeAt c.pos a.cards })
        columns
