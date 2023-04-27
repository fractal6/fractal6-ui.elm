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


module Org.Project exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), hasLazyAdminRole)
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Bulk exposing (..)
import Bulk.Board exposing (viewBoard)
import Bulk.Codecs exposing (ActionType(..), DocType(..), Flags_, FractalBaseRoute(..), NodeFocus, contractIdCodec, focusFromNameid, focusState, id3Changed, nameidFromFlags, nearestCircleid, uriFromNameid)
import Bulk.Error exposing (viewGqlErrors)
import Bulk.View exposing (viewRole, viewUserFull)
import Components.ActionPanel as ActionPanel
import Components.AuthModal as AuthModal
import Components.HelperBar as HelperBar
import Components.JoinOrga as JoinOrga
import Components.OrgaMenu as OrgaMenu
import Components.SearchBar exposing (viewSearchBar)
import Components.TreeMenu as TreeMenu
import Dict
import Extra exposing (ternary, unwrap)
import Extra.Url exposing (queryBuilder, queryParser)
import Fifo exposing (Fifo)
import Form.Help as Help
import Form.NewTension as NTF exposing (NewTensionInput(..), TensionTab(..))
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Generated.Route as Route exposing (toHref)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, div, h2, i, input, span, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, classList, href, id, style, type_)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Lazy as Lazy
import List.Extra as LE
import Loading exposing (GqlData, RequestResult(..), fromMaybeData, withDefaultData, withMapData, withMaybeData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.QueryNode exposing (queryLocalGraph)
import Query.QueryProject exposing (getProject, moveProjectTension)
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

    -- Board
    , boardHeight : Maybe Float
    , hover_column : Maybe String
    , movingTension : Maybe Tension
    , moveFifo : Fifo ( Int, Tension )
    , movingHoverCol : Maybe { pos : Int, to_receiverid : String }
    , movingHoverT : Maybe { pos : Int, tid : String, to_receiverid : String }
    , dragCount : Int
    , draging : Bool

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

            -- Board
            , boardHeight = Nothing
            , hover_column = Nothing
            , movingTension = Nothing
            , moveFifo = Fifo.empty
            , movingHoverCol = Nothing
            , movingHoverT = Nothing
            , dragCount = 0
            , draging = False

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
      -- Board
    | OnResize Int Int
    | FitBoard (Result Dom.Error Dom.Element)
    | OnColumnHover (Maybe String)
    | OnMove { pos : Int, to_receiverid : String } Tension
    | OnCancelHov
    | OnEndMove
    | OnMoveEnterCol { pos : Int, to_receiverid : String } Bool
    | OnMoveLeaveCol
    | OnMoveLeaveCol_
    | OnMoveEnterT { pos : Int, tid : String, to_receiverid : String }
    | OnMoveDrop String
      --
    | GotCardMoved (GqlData IdPayload)
      -- Common
    | NoMsg
    | LogErr String
    | OnGoRoot
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
            ( model
            , Cmd.batch
                [ getProject apis model.projectid GotProject
                ]
            , Cmd.none
            )

        GotProject result ->
            ( { model | project_data = result }, Cmd.none, Cmd.none )

        -- Board
        OnResize w h ->
            let
                conf =
                    model.conf

                newScreen =
                    { w = w, h = h }

                newConf =
                    { conf | screen = newScreen }

                elmId =
                    "projectView"
            in
            ( { model | conf = newConf }, Task.attempt FitBoard (Dom.getElement elmId), send (UpdateSessionScreen newScreen) )

        FitBoard elt ->
            case elt of
                Ok e ->
                    let
                        h =
                            if e.viewport.height - e.element.y < 511 then
                                -- allow y-scroll here. Substract the header size.
                                e.viewport.height - 50

                            else
                                e.viewport.height - e.element.y
                    in
                    ( { model | boardHeight = Just h }, Cmd.none, Cmd.none )

                Err _ ->
                    ( model, Cmd.none, Cmd.none )

        OnColumnHover v ->
            ( { model | hover_column = v }, Cmd.none, Cmd.none )

        OnMove c t ->
            ( { model | draging = True, dragCount = 0, movingHoverCol = Just c, movingTension = Just t }, Cmd.none, Cmd.none )

        OnEndMove ->
            let
                newModel =
                    { model | draging = False }

                cmds =
                    [ sendSleep OnCancelHov 300 ]
            in
            Maybe.map2
                (\t { pos, to_receiverid } ->
                    --if t.id == tid then
                    if t.receiver.nameid == to_receiverid then
                        ( newModel, Cmd.batch cmds, Cmd.none )

                    else
                        let
                            j =
                                Maybe.map .pos model.movingHoverT |> withDefault -1
                        in
                        ( { newModel | moveFifo = Fifo.insert ( j, t ) model.moveFifo }
                        , moveProjectTension apis t.id pos to_receiverid GotCardMoved
                        , Cmd.none
                        )
                )
                model.movingTension
                model.movingHoverCol
                |> withDefault
                    ( newModel, Cmd.batch cmds, Cmd.none )

        OnCancelHov ->
            ( { model | movingHoverCol = Nothing, movingHoverT = Nothing }, Cmd.none, Cmd.none )

        OnMoveEnterCol hover reset ->
            if Just hover == model.movingHoverCol then
                if reset then
                    ( { model | movingHoverT = Nothing }, Cmd.none, Cmd.none )

                else
                    ( { model | dragCount = 1 }, Cmd.none, Cmd.none )

            else
                ( { model | dragCount = 1, movingHoverCol = Just hover, movingHoverT = Nothing }, Cmd.none, Cmd.none )

        OnMoveLeaveCol ->
            ( { model | dragCount = model.dragCount - 1 }, sendSleep OnMoveLeaveCol_ 15, Cmd.none )

        OnMoveLeaveCol_ ->
            if model.dragCount < 0 && model.draging then
                ( model, send OnCancelHov, Cmd.none )

            else
                ( model, Cmd.none, Cmd.none )

        OnMoveEnterT hover ->
            ( { model | movingHoverT = Just hover }, Cmd.none, Cmd.none )

        OnMoveDrop nameid ->
            ( { model | movingTension = Nothing, movingHoverCol = Nothing, movingHoverT = Nothing }
            , Cmd.none
            , Cmd.none
            )

        --
        GotCardMoved result ->
            -- @TODO: save result and show a popup on error !
            ( model, Cmd.none, Cmd.none )

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
            in
            ( { model | treeMenu = data }, out.cmds |> List.map (\m -> Cmd.map TreeMenuMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        ActionPanelMsg msg ->
            let
                ( data, out ) =
                    ActionPanel.update apis msg model.actionPanel

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | actionPanel = data }, out.cmds |> List.map (\m -> Cmd.map ActionPanelMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ model =
    []
        ++ (HelperBar.subscriptions |> List.map (\s -> Sub.map HelperBarMsg s))
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        ++ (NTF.subscriptions model.tensionForm |> List.map (\s -> Sub.map NewTensionMsg s))
        ++ (JoinOrga.subscriptions model.joinOrga |> List.map (\s -> Sub.map JoinOrgaMsg s))
        ++ (AuthModal.subscriptions |> List.map (\s -> Sub.map AuthModalMsg s))
        ++ (OrgaMenu.subscriptions |> List.map (\s -> Sub.map OrgaMenuMsg s))
        ++ (TreeMenu.subscriptions |> List.map (\s -> Sub.map TreeMenuMsg s))
        ++ (ActionPanel.subscriptions model.actionPanel |> List.map (\s -> Sub.map ActionPanelMsg s))
        |> Sub.batch



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
    in
    { title =
        case model.project_data of
            Success p ->
                T.project ++ " · " ++ p.name

            _ ->
                "Loading..."
    , body =
        [ div [ class "orgPane" ]
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
            ]
        , Help.view model.empty model.help |> Html.map HelpMsg
        , NTF.view { tree_data = TreeMenu.getOrgaData_ model.treeMenu, path_data = model.path_data } model.tensionForm |> Html.map NewTensionMsg
        , JoinOrga.view model.empty model.joinOrga |> Html.map JoinOrgaMsg
        , AuthModal.view model.empty model.authModal |> Html.map AuthModalMsg
        , OrgaMenu.view model.empty model.orgaMenu |> Html.map OrgaMenuMsg
        , TreeMenu.view model.empty model.treeMenu |> Html.map TreeMenuMsg
        , ActionPanel.view panelData model.actionPanel |> Html.map ActionPanelMsg
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ class "columns is-centered" ]
        [ div [ class "column is-12 is-11-desktop is-10-fullhd pb-0" ]
            [ div [ class "columns is-centered mb-0" ]
                [ div [ class "column is-12 pb-1" ]
                    [ viewSearchBar model ]
                ]
            ]
        ]


viewSearchBar : Model -> Html Msg
viewSearchBar model =
    div [ id "searchBarProject", class "searchBar" ]
        [ div [ class "columns mb-0" ]
            [ case model.project_data of
                Success p ->
                    h2 [ class "subtitle is-strong" ] [ text p.name ]

                _ ->
                    text ""

            --div [ class "column is-5" ]
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
        ]


viewProject : ProjectData -> Model -> Html Msg
viewProject data model =
    let
        -- Computation @TODO: optimize/lazy
        keys =
            data.columns |> List.sortBy .pos |> List.map .id

        names =
            data.columns |> List.sortBy .pos |> List.map .name

        dict_data =
            data.columns |> List.map (\x -> ( x.name, List.sortBy .pos x.tensions |> List.map .tension )) |> Dict.fromList

        header : String -> String -> Maybe Tension -> Html Msg
        header key title t_m =
            span []
                [ text title
                , span
                    [ class "tag is-rounded button-light is-w has-border is-pulled-right ml-1"

                    --  It's distracting for for the eyes (works with onMouseEnter below)
                    --, classList [ ( "is-invisible", model.hover_column /= Just n ) ]
                    , onClick (NewTensionMsg (NTF.OnOpen (FromNameid model.node_focus.nameid)))
                    ]
                    [ A.icon "icon-plus" ]
                ]

        op =
            { hasTaskMove = True
            , hasNewCol = True
            , conf = model.conf
            , node_focus = model.node_focus
            , boardId = "projectView"
            , boardHeight = model.boardHeight
            , movingTension = model.movingTension
            , movingHoverCol = model.movingHoverCol
            , movingHoverT = model.movingHoverT

            -- Board Msg
            , onColumnHover = OnColumnHover
            , onMove = OnMove
            , onCancelHov = OnCancelHov
            , onEndMove = OnEndMove
            , onMoveEnterCol = OnMoveEnterCol
            , onMoveLeaveCol = OnMoveLeaveCol
            , onMoveEnterT = OnMoveEnterT
            , onMoveDrop = OnMoveDrop
            , noMsg = NoMsg
            }
    in
    viewBoard op header (LE.zip keys names) dict_data
