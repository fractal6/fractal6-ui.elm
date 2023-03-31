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


module Org.Projects exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Browser.Dom as Dom
import Browser.Events as Events
import Browser.Navigation as Nav
import Bulk exposing (..)
import Bulk.Board exposing (viewBoard)
import Bulk.Codecs exposing (ActionType(..), DocType(..), Flags_, FractalBaseRoute(..), NodeFocus, basePathChanged, focusFromNameid, focusState, hasLazyAdminRole, nameidFromFlags, uriFromNameid)
import Bulk.Error exposing (viewGqlErrors, viewHttpErrors)
import Bulk.View exposing (mediaTension, statusColor, tensionIcon3, tensionStatus2str, tensionType2str, viewPinnedTensions, viewUserFull)
import Codecs exposing (QuickDoc)
import Components.ActionPanel as ActionPanel
import Components.AuthModal as AuthModal
import Components.HelperBar as HelperBar
import Components.JoinOrga as JoinOrga
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.MoveTension as MoveTension
import Components.OrgaMenu as OrgaMenu
import Components.TreeMenu as TreeMenu
import Dict exposing (Dict)
import Dict.Extra as DE
import Extra exposing (space_, ternary, textH, unwrap, upH)
import Extra.Events exposing (onClickPD, onDragEnd, onDragEnter, onDragLeave, onDragStart, onDrop, onEnter, onKeydown, onTab)
import Extra.Url exposing (queryBuilder, queryParser)
import Fifo exposing (Fifo)
import Form exposing (isPostSendable)
import Form.Help as Help
import Form.NewTension as NTF exposing (NewTensionInput(..), TensionTab(..))
import Fractal.Enum.Lang as Lang
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, li, nav, option, p, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, autocomplete, autofocus, class, classList, disabled, href, id, list, placeholder, rows, selected, style, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading
    exposing
        ( GqlData
        , ModalData
        , RequestResult(..)
        , WebData
        , fromMaybeData
        , fromMaybeWebData
        , isSuccess
        , isWebSuccess
        , withDefaultData
        , withDefaultWebData
        , withMapData
        , withMaybeData
        , withMaybeDataMap
        )
import Maybe exposing (withDefault)
import ModelSchema
    exposing
        ( LocalGraph
        , Project
        )
import Page exposing (Document, Page)
import Ports
import Process
import Query.QueryNode exposing (getProjects, queryLocalGraph)
import RemoteData exposing (RemoteData)
import Requests exposing (fetchProjectsSub, fetchProjectsTop)
import Session exposing (Conf, GlobalCmd(..), Screen)
import Task
import Text as T
import Time
import Url exposing (Url)



---- PROGRAM ----


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

                    DoUpdateTree tree ->
                        ( Cmd.none, send (UpdateSessionTree tree) )

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
                    _ ->
                        ( Cmd.none, Cmd.none )
            )
        |> List.unzip



--
-- Model
--


type alias Model =
    { -- Focus
      node_focus : NodeFocus
    , path_data : GqlData LocalGraph

    -- Page
    , hasUnsavedData : Bool
    , artefact_form : ProjectForm

    -- Projects
    , projects : GqlData (List Project)
    , projects_top : WebData (List Project)
    , projects_sub : WebData (List Project)
    , project_add : Bool
    , project_edit : Maybe Project
    , project_result : GqlData Project
    , project_result_del : GqlData Project

    -- Common
    , modal_confirm : ModalConfirm Msg
    , refresh_trial : Int
    , url : Url
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


resetForm : Model -> Model
resetForm model =
    { model
        | project_form = initArtefactNodeForm (LoggedIn model.project_form.uctx) model.node_focus.nameidColorPicker.initColor
        , hasUnsavedData = False
        , project_result = NotAsked
        , project_result_del = NotAsked
    }


type Msg
    = PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
      -- Data Queries
    | GotPath Bool (GqlData LocalGraph)
      -- Page
    | ChangeProjectPost String String
    | SafeEdit Msg
    | SafeSend Msg
      -- Projects
    | GotProjects (GqlData (List Project))
    | GotProjectsTop (WebData (List Project))
    | GotProjectsSub (WebData (List Project))
    | AddProject
    | EditProject Project
    | CancelProject
    | SubmitAddProject Time.Posix
    | SubmitEditProject Time.Posix
    | SubmitDeleteProject String Time.Posix
    | GotProject (GqlData Project)
    | GotProjectDel (GqlData Project)
      -- Common
    | NoMsg
    | LogErr String
    | OnGoRoot
    | OpenActionPanel String String (Maybe ( Int, Int ))
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Components
    | HelperBarMsg HelperBar.Msg
    | HelpMsg Help.Msg
    | NewTensionMsg NTF.Msg
    | JoinOrgaMsg JoinOrga.Msg
    | AuthModalMsg AuthModal.Msg
    | OrgaMenuMsg OrgaMenu.Msg
    | TreeMenuMsg TreeMenu.Msg
    | ActionPanelMsg ActionPanel.Msg



--
-- INIT
--


type alias Flags =
    Flags_


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
        newFocus =
            flags
                |> nameidFromFlags
                |> focusFromNameid

        -- What has changed
        fs =
            focusState SettingsBaseUri global.session.referer global.url global.session.node_focus newFocus

        model =
            { node_focus = newFocus
            , path_data =
                global.session.path_data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , hasUnsavedData = False
            , artefact_form = ProjectForm

            -- Projectss
            , projects = Loading
            , projects_top = RemoteData.Loading
            , projects_sub = RemoteData.Loading
            , project_add = False
            , project_edit = Nothing
            , project_result = NotAsked
            , project_result_del = NotAsked

            -- Common
            , refresh_trial = 0
            , url = global.url
            , empty = {}
            , helperBar = HelperBar.init SettingsBaseUri global.url.query newFocus global.session.user
            , help = Help.init global.session.user conf
            , tensionForm = NTF.init global.session.user conf
            , modal_confirm = ModalConfirm.init NoMsg
            , joinOrga = JoinOrga.init newFocus.nameid global.session.user global.session.screen
            , authModal = AuthModal.init global.session.user (Dict.get "puid" query |> Maybe.map List.head |> withDefault Nothing)
            , orgaMenu = OrgaMenu.init newFocus global.session.orga_menu global.session.orgs_data global.session.user
            , treeMenu = TreeMenu.init SettingsBaseUri global.url.query newFocus global.session.tree_menu global.session.tree_data global.session.user
            , actionPanel = ActionPanel.init global.session.user global.session.screen
            }

        cmds =
            [ ternary fs.focusChange (queryLocalGraph apis newFocus.nameid True (GotPath True)) Cmd.none
            , sendSleep PassedSlowLoadTreshold 500
            , Cmd.map OrgaMenuMsg (send OrgaMenu.OnLoad)
            , Cmd.map TreeMenuMsg (send TreeMenu.OnLoad)
            ]
                ++ [ getProjects apis newFocus.nameid GotProjects
                   , fetchProjectsTop apis newFocus.nameid False GotProjectsTop
                   , fetchProjectsSub apis newFocus.nameid GotProjectsSub
                   ]
    in
    ( model
    , Cmd.batch cmds
    , if fs.refresh then
        send (UpdateSessionFocus (Just newFocus))

      else
        Cmd.none
    )


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global message model =
    let
        apis =
            global.session.apis
    in
    case message of
        PassedSlowLoadTreshold ->
            let
                labels =
                    ternary (model.labels == Loading) LoadingSlowly model.labels
            in
            ( { model | labels = labels }, Cmd.none, Cmd.none )

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        -- Data queries
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

        ChangeProjectPost field value ->
            let
                f =
                    model.project_form

                newForm =
                    { f | post = Dict.insert field value f.post }
            in
            ( { model | project_form = newForm, hasUnsavedData = True }, Cmd.none, Cmd.none )

        SafeEdit msg ->
            if model.hasUnsavedData then
                ( model
                , send <|
                    DoModalConfirmOpen (SafeSend msg)
                        { message = Nothing
                        , txts = [ ( T.confirmUnsafe, "" ) ]
                        }
                , Cmd.none
                )

            else
                ( resetForm model, send msg, Cmd.none )

        SafeSend msg ->
            ( resetForm model, send msg, Cmd.none )

        --
        -- Projects
        --
        GotProjects result ->
            let
                newModel =
                    { model | projects = result }
            in
            ( newModel, Cmd.none, Cmd.none )

        GotProjectsTop result ->
            let
                newModel =
                    { model | projects_top = result }
            in
            ( newModel, Cmd.none, Cmd.none )

        GotProjectsSub result ->
            let
                newModel =
                    { model | projects_sub = result }
            in
            ( newModel, Cmd.none, Cmd.none )

        AddProject ->
            if model.project_add then
                ( model, Cmd.none, Cmd.none )

            else
                -- Toggle Add Project Box
                ( { model
                    | project_add = ternary model.project_add False True
                    , project_edit = Nothing
                  }
                , Cmd.none
                , Cmd.none
                )

        EditProject project ->
            let
                f =
                    model.project_form

                newForm =
                    { f
                        | id = project.id
                        , post =
                            Dict.fromList
                                ([ ( "name", project.name ) ]
                                    ++ (project.color |> Maybe.map (\x -> [ ( "color", x ) ]) |> withDefault [])
                                    ++ (project.description |> Maybe.map (\x -> [ ( "description", x ) ]) |> withDefault [])
                                    ++ [ ( "old_name", project.name ) ]
                                )
                    }
            in
            ( { model
                | project_add = False
                , project_edit = Just project
                , project_form = newForm
              }
            , Cmd.none
            , Cmd.none
            )

        CancelProject ->
            ( { model
                | project_add = False
                , project_edit = Nothing
                , project_result = NotAsked
                , project_result_del = NotAsked
              }
                |> resetForm
            , Cmd.none
            , Cmd.none
            )

        SubmitAddProject _ ->
            ( { model | project_result = LoadingSlowly }, addOneProject apis model.project_form GotProject, Cmd.none )

        SubmitEditProject _ ->
            ( { model | project_result = LoadingSlowly }, updateOneProject apis model.project_form GotProject, Cmd.none )

        SubmitDeleteProject id _ ->
            let
                f =
                    model.project_form

                newForm =
                    { f | id = id }
            in
            ( { model | project_result_del = LoadingSlowly, project_form = newForm }, removeOneProject apis newForm GotProjectDel, Cmd.none )

        GotProject result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | project_result = NotAsked }, Ports.raiseAuthModal model.project_form.uctx, Cmd.none )

                RefreshToken i ->
                    if model.project_add then
                        ( { model | refresh_trial = i }, sendSleep (Submit SubmitAddProject) 500, send UpdateUserToken )

                    else
                        -- assume edit
                        ( { model | refresh_trial = i }, sendSleep (Submit SubmitEditProject) 500, send UpdateUserToken )

                OkAuth project ->
                    let
                        d =
                            withMaybeData model.projects |> withDefault []

                        new =
                            if model.project_add then
                                [ project ] ++ d

                            else
                                -- assume edit
                                List.map
                                    (\x ->
                                        if x.id == project.id then
                                            project

                                        else
                                            x
                                    )
                                    d
                    in
                    ( { model | project_result = result, projects = Success new, project_add = False, project_edit = Nothing } |> resetForm
                    , Cmd.none
                    , Cmd.none
                    )

                DuplicateErr ->
                    let
                        project_name =
                            Dict.get "name" model.project_form.post |> withDefault "" |> String.toLower

                        here name =
                            (withMaybeData model.projects |> withDefault [] |> List.filter (\x -> x.name == name) |> List.length)
                                > 0

                        form =
                            model.project_form
                    in
                    if model.project_add && not (here project_name) then
                        -- set the projects in the node projects list
                        ( { model | project_result = LoadingSlowly, project_form = { form | id = "" } }, send (Submit SubmitEditProject), Cmd.none )

                    else
                        -- trow error if the projects is in the list of projects
                        ( { model | project_result = result }, Cmd.none, Cmd.none )

                _ ->
                    ( { model | project_result = result }, Cmd.none, Cmd.none )

        GotProjectDel result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | project_result_del = NotAsked }, Ports.raiseAuthModal model.project_form.uctx, Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep (Submit <| SubmitDeleteProject model.project_form.id) 500, send UpdateUserToken )

                OkAuth _ ->
                    let
                        d =
                            withMaybeData model.projects |> withDefault []

                        new =
                            List.filter (\x -> x.id /= model.project_form.id) d
                    in
                    ( { model | project_result_del = NotAsked, projects = Success new, project_add = False, project_edit = Nothing } |> resetForm
                    , Cmd.none
                    , Cmd.none
                    )

                _ ->
                    ( { model | project_result_del = result }, Cmd.none, Cmd.none )

        -- Common
        NoMsg ->
            ( model, Cmd.none, Cmd.none )

        LogErr err ->
            ( model, Ports.logErr err, Cmd.none )

        OnGoRoot ->
            let
                query =
                    model.url.query |> Maybe.map (\uq -> "?" ++ uq) |> Maybe.withDefault ""
            in
            ( model, Cmd.none, send (NavigateRaw (uriFromNameid SettingsBaseUri model.node_focus.rootnameid [] ++ query)) )

        OpenActionPanel domid nameid pos ->
            ( model, Cmd.map ActionPanelMsg (send <| ActionPanel.OnOpen domid nameid (TreeMenu.getOrgaData_ model.treeMenu) pos), Cmd.none )

        -- Confirm Modal
        DoModalConfirmOpen msg mess ->
            ( { model | modal_confirm = ModalConfirm.open msg mess model.modal_confirm }, Cmd.none, Cmd.none )

        DoModalConfirmClose _ ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, Cmd.none, Cmd.none )

        DoModalConfirmSend ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, send model.modal_confirm.msg, Cmd.none )

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
                                    [ Nav.replaceUrl global.key (Url.toString model.url) ]

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
    [ Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    ]
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
        (String.join "/" <| LE.unique [ model.node_focus.rootnameid, model.node_focus.nameid |> String.split "#" |> LE.last |> withDefault "" ])
            ++ " · "
            ++ T.settings
    , body =
        [ div [ class "orgPane" ]
            [ HelperBar.view helperData model.helperBar |> Html.map HelperBarMsg
            , div [ id "mainPane" ] [ view_ model.global model ]
            ]
        , Help.view model.empty model.help |> Html.map HelpMsg
        , NTF.view { tree_data = TreeMenu.getOrgaData_ model.treeMenu, path_data = model.path_data } model.tensionForm |> Html.map NewTensionMsg
        , JoinOrga.view model.empty model.joinOrga |> Html.map JoinOrgaMsg
        , AuthModal.view model.empty model.authModal |> Html.map AuthModalMsg
        , OrgaMenu.view model.empty model.orgaMenu |> Html.map OrgaMenuMsg
        , TreeMenu.view model.empty model.treeMenu |> Html.map TreeMenuMsg
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    let
        isAdmin =
            case global.session.user of
                LoggedIn uctx ->
                    hasLazyAdminRole uctx model.node_focus.rootnameid

                LoggedOut ->
                    False
    in
    div [ class "columns is-centered" ]
        [ div [ class "column is-12 is-11-desktop is-9-fullhd mt-5" ]
            [ div [ class "section_ mt-2" ]
                [ if isAdmin then
                    div
                        [ class "button is-primary is-pulled-right"
                        , onClick (JoinOrgaMsg (JoinOrga.OnOpen model.node_focus.rootnameid JoinOrga.InviteOne))
                        ]
                        [ A.icon1 "icon-user-plus" T.inviteMembers ]

                  else
                    text ""
                , div [ class "columns mb-6 px-3" ]
                    [ Lazy.lazy3 viewProjects model.conf "Circle's Projects" model.projects_top ]
                , div [ class "columns mb-6 px-3" ]
                    [ Lazy.lazy3 viewProjects model.conf "Top Circle's Projects" model.projects_sub ]
                , div [ class "columns mb-6 px-3" ]
                    [ Lazy.lazy3 viewProjects model.conf "Sub Circle's Projects" model.projects_top ]
                ]
            ]
        ]


viewProjects : Conf -> String -> WebData (List Project) -> Html Msg
viewProjects conf title data =
    text "no implemented"
