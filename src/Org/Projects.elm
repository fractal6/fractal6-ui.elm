{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2024 Fractale Co

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
import Auth exposing (ErrState(..), getProjectRights, hasLazyAdminRole, parseErr)
import Browser.Dom as Dom
import Browser.Events as Events
import Browser.Navigation as Nav
import Bulk exposing (ProjectForm, UserState(..), initProjectForm)
import Bulk.Codecs exposing (ActionType(..), DocType(..), Flags_, FractalBaseRoute(..), NodeFocus, basePathChanged, focusFromNameid, focusState, nameidEncoder, nameidFromFlags, shortId, toLink)
import Bulk.Error exposing (viewGqlErrors, viewHttpErrors)
import Bulk.View exposing (nodeType2str, projectStatus2str)
import Components.ActionPanel as ActionPanel
import Components.AuthModal as AuthModal
import Components.HelperBar as HelperBar
import Components.JoinOrga as JoinOrga
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.NodeDoc exposing (viewUrlForm)
import Components.OrgaMenu as OrgaMenu
import Components.SearchBar exposing (viewSearchBar)
import Components.TreeMenu as TreeMenu
import Dict exposing (Dict)
import Extra exposing (decap, space_, ternary, textH, textT, unwrap, upH)
import Extra.Date exposing (formatDate)
import Extra.Url exposing (queryBuilder, queryParser)
import Form exposing (isPostSendable)
import Form.Help as Help
import Form.NewTension as NTF
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.ProjectStatus as ProjectStatus
import Fractal.Enum.TensionAction as TensionAction
import Generated.Route as Route exposing (toHref)
import Global exposing (Msg(..), getConf, send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, datalist, div, figcaption, figure, h1, h2, hr, i, img, input, li, nav, option, p, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (alt, attribute, autocomplete, autofocus, class, classList, disabled, href, id, list, placeholder, required, rows, selected, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), RestData, withDefaultData, withMapData, withMaybeData)
import Maybe exposing (withDefault)
import ModelSchema exposing (LocalGraph, NewTensionInput(..), ProjectFull, ProjectsCount)
import Page exposing (Document, Page)
import Ports
import Query.PatchNode exposing (addOneProject, removeOneProject, updateOneProject)
import Query.QueryNode exposing (getProjects, queryLocalGraph)
import RemoteData
import Requests exposing (fetchProjectCount, fetchProjectsSub, fetchProjectsTop)
import Session exposing (Conf, GlobalCmd(..), Screen, Theme(..))
import String.Format as Format
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
                    DoCreateTension a ntm d ->
                        case ntm of
                            Nothing ->
                                ( Cmd.map NewTensionMsg <| send (NTF.OnOpen (FromNameid a) d), Cmd.none )

                            Just NodeType.Circle ->
                                ( Cmd.map NewTensionMsg <| send (NTF.OnOpenCircle (FromNameid a)), Cmd.none )

                            Just NodeType.Role ->
                                ( Cmd.map NewTensionMsg <| send (NTF.OnOpenRole (FromNameid a)), Cmd.none )

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
    , project_form : ProjectForm
    , pattern : String
    , pattern_init : String
    , statusFilter : StatusFilter
    , projects_count : GqlData ProjectsCount
    , hasDuplicate : Bool

    -- Projects
    , projects : GqlData (List ProjectFull)
    , projects_top : RestData (List ProjectFull)
    , projects_sub : RestData (List ProjectFull)
    , project_add : Bool
    , project_edit : Maybe ProjectFull
    , project_result : GqlData ProjectFull
    , project_result_del : GqlData ProjectFull

    -- Common
    , conf : Conf
    , modal_confirm : ModalConfirm Msg
    , refresh_trial : Int
    , url : Url
    , empty : {}

    -- Components
    , actionPanel : ActionPanel.State
    , helperBar : HelperBar.State
    , help : Help.State
    , joinOrga : JoinOrga.State
    , tensionForm : NTF.State
    , authModal : AuthModal.State
    , orgaMenu : OrgaMenu.State
    , treeMenu : TreeMenu.State
    }


type StatusFilter
    = OpenStatus
    | ClosedStatus
    | AllStatus


statusFilterEncoder : StatusFilter -> String
statusFilterEncoder x =
    case x of
        AllStatus ->
            "all"

        OpenStatus ->
            "open"

        ClosedStatus ->
            "closed"


statusFilterDecoder : String -> StatusFilter
statusFilterDecoder x =
    case x of
        "all" ->
            AllStatus

        "closed" ->
            ClosedStatus

        _ ->
            OpenStatus


defaultStatus : String
defaultStatus =
    "open"


defaultStatusFilter =
    OpenStatus


statusFilter2Text : StatusFilter -> String
statusFilter2Text x =
    case x of
        AllStatus ->
            T.all

        OpenStatus ->
            projectStatus2str ProjectStatus.Open

        ClosedStatus ->
            projectStatus2str ProjectStatus.Closed


statusDecoder : StatusFilter -> ProjectStatus.ProjectStatus
statusDecoder sf =
    case sf of
        AllStatus ->
            ProjectStatus.Open

        OpenStatus ->
            ProjectStatus.Open

        ClosedStatus ->
            ProjectStatus.Closed


resetForm : Model -> Model
resetForm model =
    { model
        | project_form = initProjectForm (LoggedIn model.project_form.uctx) model.node_focus.nameid
        , hasUnsavedData = False
        , project_result = NotAsked
        , project_result_del = NotAsked
    }


simpleKanban : List { name : String, description : String, color : Maybe String }
simpleKanban =
    [ { name = "Todo"
      , description = "This item hasn't been started"
      , color = Just "#01FF70"
      }
    , { name = "In Progress"
      , description = "This is actively being worked on"
      , color = Just "#FF851B"
      }
    , { name = "Done"
      , description = "This has been completed"
      , color = Just "#B10DC9"
      }
    ]


type Msg
    = --Loading
      PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
    | GotPath Bool (GqlData LocalGraph)
    | DoLoad
      -- Projects
    | ChangeProjectPost String String
    | SafeEdit Msg
    | SafeSend Msg
    | GotProjects (GqlData { projects : List ProjectFull, counts : ProjectsCount })
    | GotProjectsTop (RestData (List ProjectFull))
    | GotProjectsSub (RestData (List ProjectFull))
    | AddProject
    | EditProject ProjectFull
    | ChangeStatus ProjectStatus.ProjectStatus ProjectFull
    | CancelProject
    | SubmitAddProject Time.Posix
    | SubmitEditProject Time.Posix
    | SubmitDeleteProject String Time.Posix
    | GotProject (GqlData ProjectFull)
    | GotProjectDel (GqlData ProjectFull)
      -- Search
    | ChangePattern String
    | ChangeStatusFilter StatusFilter
    | SearchKeyDown Int
    | ResetData
    | SubmitSearch
    | SubmitTextSearch String
    | SubmitSearchReset
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
            getConf global

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
            focusState ProjectsBaseUri global.session.referer global.url global.session.node_focus newFocus

        model =
            { node_focus = newFocus
            , path_data =
                global.session.path_data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , hasUnsavedData = False
            , project_form = initProjectForm global.session.user newFocus.nameid
            , pattern = Dict.get "q" query |> withDefault [] |> List.head |> withDefault ""
            , pattern_init = Dict.get "q" query |> withDefault [] |> List.head |> withDefault ""
            , statusFilter = Dict.get "s" query |> withDefault [] |> List.head |> withDefault "" |> statusFilterDecoder
            , projects_count = Loading
            , hasDuplicate = False

            -- Projectss
            , projects = Loading
            , projects_top = RemoteData.Loading
            , projects_sub = RemoteData.Loading
            , project_add = False
            , project_edit = Nothing
            , project_result = NotAsked
            , project_result_del = NotAsked

            -- Common
            , conf = conf
            , refresh_trial = 0
            , url = global.url
            , empty = {}
            , tensionForm = NTF.init global.session.user conf
            , helperBar = HelperBar.init ProjectsBaseUri global.url.query newFocus global.session.user
            , help = Help.init global.session.user conf
            , modal_confirm = ModalConfirm.init NoMsg
            , joinOrga = JoinOrga.init newFocus.nameid global.session.user global.session.screen
            , authModal = AuthModal.init global.session.user (Dict.get "puid" query |> Maybe.map List.head |> withDefault Nothing)
            , orgaMenu = OrgaMenu.init newFocus global.session.orga_menu global.session.orgs_data global.session.user
            , treeMenu = TreeMenu.init ProjectsBaseUri global.url.query newFocus global.session.tree_menu global.session.tree_data global.session.user
            , actionPanel = ActionPanel.init global.session.user global.session.screen
            }

        cmds =
            [ ternary fs.focusChange (queryLocalGraph apis newFocus.nameid True (GotPath True)) Cmd.none
            , sendSleep PassedSlowLoadTreshold 500
            , send DoLoad
            , Cmd.map OrgaMenuMsg (send OrgaMenu.OnLoad)
            , Cmd.map TreeMenuMsg (send TreeMenu.OnLoad)
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
                projects =
                    ternary (model.projects == Loading) LoadingSlowly model.projects
            in
            ( { model | projects = projects }, Cmd.none, Cmd.none )

        Submit nextMsg ->
            ( model, sendNow nextMsg, Cmd.none )

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

        DoLoad ->
            let
                status =
                    statusDecoder model.statusFilter

                pattern_m =
                    case model.pattern of
                        "" ->
                            Nothing

                        a ->
                            Just a
            in
            ( model
            , Cmd.batch
                [ getProjects apis model.node_focus.nameid pattern_m status GotProjects

                --, fetchProjectsTop apis model.node_focus.nameid GotProjectsTop
                --, fetchProjectsSub apis model.node_focus.nameid GotProjectsSub
                --, fetchProjectCount apis nameids model.pattern Nothing GotProjectCount
                , Ports.bulma_driver ""
                ]
            , Cmd.none
            )

        ChangeProjectPost field value ->
            let
                form =
                    model.project_form

                newForm =
                    case field of
                        "name" ->
                            { form
                                | post =
                                    form.post
                                        |> Dict.insert field value
                                        |> Dict.insert "nameid" (nameidEncoder value)
                            }

                        "nameid" ->
                            { form | post = Dict.insert field (nameidEncoder value) form.post }

                        _ ->
                            { form | post = Dict.insert field value form.post }
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

        ChangePattern value ->
            ( { model | pattern = value }, Cmd.none, Cmd.none )

        ChangeStatusFilter value ->
            ( { model | statusFilter = value }, send SubmitSearchReset, Cmd.none )

        SearchKeyDown key ->
            case key of
                13 ->
                    --ENTER
                    ( model, send (SubmitTextSearch model.pattern), Cmd.none )

                27 ->
                    --ESC
                    ( model, send (ChangePattern ""), Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        SubmitSearch ->
            let
                query =
                    queryBuilder
                        [ ( "q", model.pattern |> String.trim )
                        , ( "s", statusFilterEncoder model.statusFilter |> (\x -> ternary (x == defaultStatus) "" x) )
                        ]
                        |> (\q -> ternary (q == "") "" ("?" ++ q))
            in
            ( model, Nav.pushUrl global.key (toLink ProjectsBaseUri model.node_focus.nameid [] ++ query), Cmd.none )

        SubmitTextSearch pattern ->
            if String.trim pattern == model.pattern_init then
                ( model, Cmd.none, Cmd.none )

            else
                ( { model | pattern = pattern }, send SubmitSearchReset, Cmd.none )

        SubmitSearchReset ->
            -- Send search and reset the other results
            ( model, Cmd.batch [ send SubmitSearch, send ResetData ], Cmd.none )

        ResetData ->
            ( { model | projects = Loading, projects_sub = RemoteData.Loading, projects_top = RemoteData.Loading, path_data = Loading, projects_count = Loading }
            , Cmd.none
            , Cmd.none
            )

        --
        -- Projects
        --
        GotProjects result ->
            let
                newModel =
                    { model | projects = withMapData .projects result, projects_count = withMapData .counts result }
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
                let
                    form =
                        model.project_form

                    newForm =
                        { form | columns = Just simpleKanban }
                in
                ( { model
                    | project_add = ternary model.project_add False True
                    , project_edit = Nothing
                    , project_form = newForm
                  }
                , Cmd.batch [ Ports.bulma_driver "edit-project" ]
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
                                    ++ (project.description |> Maybe.map (\x -> [ ( "description", x ) ]) |> withDefault [])
                                    ++ [ ( "old_name", project.name ) ]
                                    ++ [ ( "old_nameid", nameidEncoder project.name ) ]
                                )
                    }
            in
            ( { model
                | project_add = False
                , project_edit = Just project
                , project_form = newForm
              }
            , Cmd.batch [ Ports.bulma_driver "edit-project" ]
            , Cmd.none
            )

        ChangeStatus status project ->
            let
                form =
                    model.project_form

                newForm =
                    { form
                        | status = Just status
                        , post = Dict.insert "nameid" (nameidEncoder project.name) form.post
                    }
            in
            ( { model | project_result = LoadingSlowly, project_form = newForm }
            , updateOneProject apis newForm GotProject
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
            , Ports.bulma_driver ""
            , Cmd.none
            )

        SubmitAddProject time ->
            let
                form =
                    model.project_form

                newForm =
                    { form
                        | post =
                            Dict.insert "createdAt" (fromTime time) form.post
                    }
            in
            ( { model | project_result = LoadingSlowly, project_form = newForm }
            , addOneProject apis newForm GotProject
            , Cmd.none
            )

        SubmitEditProject time ->
            let
                form =
                    model.project_form

                newForm =
                    { form
                        | post =
                            Dict.insert "updatedAt" (fromTime time) form.post
                    }
            in
            ( { model | project_result = LoadingSlowly, project_form = newForm }
            , updateOneProject apis newForm GotProject
            , Cmd.none
            )

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
                            withDefaultData [] model.projects

                        c =
                            withDefaultData (ProjectsCount 0 0) model.projects_count

                        ( new_d, new_c ) =
                            if model.project_add then
                                ( [ project ] ++ d
                                , { c | open = c.open + 1 }
                                )

                            else if model.project_form.status == Nothing || model.project_form.status == Just (statusDecoder model.statusFilter) then
                                -- assume edit
                                ( LE.setIf (\x -> x.id == project.id) project d
                                , c
                                )

                            else
                                -- Status Project changed
                                ( LE.filterNot (\x -> x.id == project.id) d
                                , case model.project_form.status of
                                    Just ProjectStatus.Open ->
                                        { c | open = c.open + 1, closed = c.closed - 1 }

                                    Just ProjectStatus.Closed ->
                                        { c | open = c.open - 1, closed = c.closed + 1 }

                                    Nothing ->
                                        c
                                )

                        redirect =
                            if model.project_add then
                                send <| NavigateRaw (toLink ProjectBaseUri model.node_focus.nameid [ project.id ])

                            else
                                Cmd.none
                    in
                    ( { model | project_result = result, projects = Success new_d, projects_count = Success new_c, project_add = False, project_edit = Nothing } |> resetForm
                    , Cmd.batch [ Ports.bulma_driver "" ]
                    , redirect
                    )

                DuplicateErr ->
                    --let
                    --    project_name =
                    --        Dict.get "name" model.project_form.post |> withDefault "" |> String.toLower
                    --    form =
                    --        model.project_form
                    --in
                    -- @TODO: **LINK** project from other circles
                    --( { model | project_result = LoadingSlowly, project_form = { form | id = "" } }, send (Submit SubmitEditProject), Cmd.none )
                    ( { model
                        | project_result = Failure [ T.duplicateNameError ]
                        , hasDuplicate = True
                      }
                    , Cmd.none
                    , Cmd.none
                    )

                NameTooLong ->
                    ( { model
                        | project_result = Failure [ T.nameTooLongError ]
                        , hasDuplicate = True
                      }
                    , Cmd.none
                    , Cmd.none
                    )

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
                            withDefaultData [] model.projects

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
            ( model, Cmd.none, send (NavigateRaw (toLink ProjectsBaseUri model.node_focus.rootnameid [] ++ query)) )

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
            ++ T.projects
    , body =
        [ div [ class "orgPane" ]
            [ HelperBar.view helperData model.helperBar |> Html.map HelperBarMsg
            , div [ id "mainPane" ] [ view_ global model ]
            ]
        , Lazy.lazy2 Help.view model.empty model.help |> Html.map HelpMsg
        , Lazy.lazy3 NTF.view (TreeMenu.getOrgaData_ model.treeMenu) model.path_data model.tensionForm |> Html.map NewTensionMsg
        , Lazy.lazy2 JoinOrga.view model.empty model.joinOrga |> Html.map JoinOrgaMsg
        , Lazy.lazy2 AuthModal.view model.empty model.authModal |> Html.map AuthModalMsg
        , Lazy.lazy2 OrgaMenu.view model.empty model.orgaMenu |> Html.map OrgaMenuMsg
        , Lazy.lazy2 TreeMenu.view model.empty model.treeMenu |> Html.map TreeMenuMsg
        , ActionPanel.view panelData model.actionPanel |> Html.map ActionPanelMsg
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ class "columns is-centered" ]
        [ div [ class "column is-12 is-11-desktop is-9-fullhd" ]
            [ if model.project_add then
                viewNewOrEditProject model.conf True model

              else if model.project_edit /= Nothing then
                viewNewOrEditProject model.conf False model

              else
                viewDefault global.session.user model
            ]
        ]


viewNewOrEditProject : Conf -> Bool -> Model -> Html Msg
viewNewOrEditProject conf isNew model =
    let
        title =
            ternary isNew T.newProject T.editProject

        submit_txt =
            ternary isNew T.create T.edit

        --
        post =
            model.project_form.post

        name =
            Dict.get "name" post |> withDefault ""

        description =
            Dict.get "description" post |> withDefault ""

        --
        isLoading =
            model.project_result == LoadingSlowly

        isSendable =
            if isNew then
                isPostSendable [ "name" ] post

            else
                isPostSendable [ "name" ] post
                    && ((Just name /= Maybe.map .name model.project_edit)
                            || (Just description /= unwrap Nothing .description model.project_edit)
                       )

        submitOrga =
            if isNew then
                onClick (Submit <| SubmitAddProject)

            else
                onClick (Submit <| SubmitEditProject)
    in
    div [ id "edit-project", class "columns submitFocus" ]
        [ div [ class "column is-half" ]
            [ h1 [ class "title" ] [ text title ]
            , div [ class "field" ]
                [ div [ class "label" ] [ text T.name ]
                , div [ class "control" ]
                    [ input
                        [ class "input autofocus followFocus"
                        , attribute "data-nextfocus" "aboutField"
                        , autocomplete False
                        , type_ "text"
                        , placeholder T.name
                        , value name
                        , onInput <| ChangeProjectPost "name"

                        --, onBlur SaveData
                        , required True
                        ]
                        []

                    --, p [ class "help" ] [ text T.orgaNameHelp ]
                    ]
                , if model.hasDuplicate then
                    div [ class "mt-3" ]
                        [ viewUrlForm (Dict.get "nameid" post) (ChangeProjectPost "nameid") model.hasDuplicate ]

                  else
                    text ""
                , if model.hasDuplicate then
                    let
                        nid =
                            Dict.get "nameid" post |> withDefault ""
                    in
                    div [ class "f6-error message is-danger is-light is-small mt-1" ]
                        [ p [ class "message-body" ]
                            (if String.length nid > 42 then
                                [ text T.nameTooLongError ]

                             else
                                [ text T.duplicateNameError ]
                            )
                        ]

                  else
                    text ""
                ]
            , div [ class "field" ]
                [ div [ class "label" ] [ text T.description ]
                , div [ class "control" ]
                    [ textarea
                        [ id "aboutField"
                        , class "textarea"
                        , rows 5
                        , placeholder "Short description (Optional)"
                        , value description
                        , onInput <| ChangeProjectPost "description"

                        --, onBlur SaveData
                        , required True
                        ]
                        []
                    ]

                --, p [ class "help" ] [ text T.purposeHelpOrga ]
                ]
            , div [ class "field pt-3 level is-mobile" ]
                [ div [ class "level-left" ]
                    [ button [ class "button", onClick CancelProject ]
                        [ A.icon0 "icon-chevron-left", text T.cancel ]
                    ]
                , div [ class "level-right" ]
                    [ div [ class "buttons" ]
                        [ button
                            ([ class "button has-text-weight-semibold defaultSubmit"
                             , classList [ ( "is-success", isSendable ), ( "is-loading", isLoading ) ]
                             , disabled (not isSendable)
                             ]
                                ++ ternary (isSendable && not isLoading)
                                    [ submitOrga ]
                                    []
                            )
                            [ text submit_txt ]
                        ]
                    ]
                ]
            , case model.project_result of
                Failure err ->
                    viewGqlErrors err

                _ ->
                    text ""
            ]
        , if isNew then
            div [ class "column is-half" ]
                [ figure [ class "image is-fullwidth has-border-light is-rounded" ]
                    [ case conf.theme of
                        DarkTheme ->
                            img [ src "https://api.fractale.co/assets/screenshots/f6-project-base-template-dark.png" ] []

                        LightTheme ->
                            img [ src "https://api.fractale.co/assets/screenshots/f6-project-base-template-light.png" ] []
                    ]
                , figcaption [] [ text T.projectCaptionSimple ]
                ]

          else
            text ""
        ]


viewDefault : UserState -> Model -> Html Msg
viewDefault user model =
    let
        isAdmin =
            case user of
                LoggedIn uctx ->
                    --hasAdminRole uctx (withMaybeData model.path_data)
                    hasLazyAdminRole uctx Nothing model.node_focus.rootnameid

                LoggedOut ->
                    False

        opSearch =
            { onChangePattern = ChangePattern
            , onSearchKeyDown = SearchKeyDown
            , onSubmitText = SubmitTextSearch
            , id_name = "searchBarProjects"
            , placeholder_txt = T.searchProjects
            }
    in
    div []
        [ div [ class "columns is-centered" ]
            [ div [ class "column is-tree-quarter" ]
                [ viewSearchBar opSearch model.pattern_init model.pattern ]
            , if isAdmin then
                div [ class "column is-one-quarter is-flex is-align-self-flex-start" ]
                    [ button [ class "button is-success is-pushed-right", onClick (SafeEdit AddProject) ] [ textT T.newProject ] ]

              else
                text ""
            ]
        , div [ class "columns is-centered" ]
            [ div [ class "column is-12" ]
                [ viewProjects model ]
            ]
        ]


viewProjects : Model -> Html Msg
viewProjects model =
    div [ class "columns" ]
        [ div [ class "column is-12" ]
            [ viewProjectsListHeader model.node_focus model.projects_count model.statusFilter
            , viewProjectsList model.conf model.node_focus model.pattern_init model.statusFilter model.projects
            ]
        ]


viewProjectsListHeader : NodeFocus -> GqlData ProjectsCount -> StatusFilter -> Html Msg
viewProjectsListHeader focus counts statusFilter =
    let
        checked =
            A.icon1 "icon-check has-text-success" ""

        unchecked =
            A.icon1 "icon-check has-text-success is-invisible" ""
    in
    div
        [ class "pt-3 pb-3 has-border-light has-background-header"
        , attribute "style" "border-top-left-radius: 6px; border-top-right-radius: 6px;"
        ]
        [ div [ class "level is-marginless is-mobile" ]
            [ div [ class "level-left px-3" ]
                [ viewProjectsCount counts statusFilter
                , if focus.nameid /= focus.rootnameid then
                    span
                        [ class "is-hidden-mobile help-label button-light is-h is-discrete px-5 is-align-self-flex-start"
                        , onClick OnGoRoot
                        ]
                        [ A.icon "arrow-up", text T.goRoot ]

                  else
                    text ""
                ]
            , div [ class "level-right px-3" ]
                []
            , if focus.nameid /= focus.rootnameid then
                div [ class "is-hidden-tablet help-label button-light is-h is-discrete px-5", onClick OnGoRoot ] [ A.icon "arrow-up", text T.goRoot ]

              else
                text ""
            ]
        ]


viewProjectsCount : GqlData ProjectsCount -> StatusFilter -> Html Msg
viewProjectsCount counts statusFilter =
    case counts of
        Success c ->
            let
                activeCls =
                    "is-hovered has-text-weight-semibold"

                inactiveCls =
                    "has-background-header"
            in
            div [ class "buttons has-addons mb-0" ]
                [ div
                    [ class "button is-rounded is-small"
                    , classList [ ( activeCls, statusFilter == OpenStatus ), ( inactiveCls, statusFilter /= OpenStatus ) ]
                    , onClick <| ChangeStatusFilter OpenStatus
                    ]
                    [ span [] [ c.open |> String.fromInt |> text ], text (space_ ++ T.openTension) ]
                , div
                    [ class "button is-rounded is-small"
                    , classList [ ( activeCls, statusFilter == ClosedStatus ), ( inactiveCls, statusFilter /= ClosedStatus ) ]
                    , onClick <| ChangeStatusFilter ClosedStatus
                    ]
                    [ c.closed |> String.fromInt |> text, text (space_ ++ T.closedTension) ]
                ]

        LoadingSlowly ->
            div [ class "buttons has-addons m-0" ]
                [ button [ class "button is-rounded is-small" ] [ text T.openTension ]
                , button [ class "button is-rounded is-small" ] [ text T.closedTension ]
                ]

        _ ->
            div [] []


viewProjectsList : Conf -> NodeFocus -> String -> StatusFilter -> GqlData (List ProjectFull) -> Html Msg
viewProjectsList conf focus pattern statusFilter data =
    div
        [ class "box is-shrinked"
        , attribute "style" "border-top-left-radius: 0px; border-top-right-radius: 0px;"
        , classList [ ( "spinner", data == LoadingSlowly ) ]
        ]
        [ case data of
            Success items ->
                if List.length items > 0 then
                    items
                        |> List.map (\x -> Lazy.lazy4 mediaProject conf focus statusFilter x)
                        |> div [ id "tensionsTab" ]

                else if pattern /= "" then
                    div [ class "m-4" ] [ text T.noResultsFor, text ": ", text pattern ]

                else
                    div [ class "m-4" ]
                        [ text
                            (T.noProjects
                                |> Format.namedValue "type" (nodeType2str focus.type_ |> decap)
                                |> Format.namedValue "status" (projectStatus2str (statusDecoder statusFilter) |> decap)
                            )
                        ]

            Failure err ->
                viewGqlErrors err

            _ ->
                div [] []
        ]


mediaProject : Conf -> NodeFocus -> StatusFilter -> ProjectFull -> Html Msg
mediaProject conf focus statusFilter project =
    let
        ( status_new, status_txt ) =
            case statusDecoder statusFilter of
                ProjectStatus.Open ->
                    ( ProjectStatus.Closed, T.close )

                ProjectStatus.Closed ->
                    ( ProjectStatus.Open, T.reopen )
    in
    div
        [ class "media mediaBox is-hoverable" ]
        [ div [ class "media-left" ] []
        , div [ class "media-content " ]
            [ div [ class "columns mb-0" ]
                [ div [ class ("column " ++ ternary (project.description == Nothing) "is-11" "is-4") ]
                    [ a
                        [ class "has-text-weight-semibold is-human discrete-link"
                        , href (Route.Project_Dynamic_Dynamic { param1 = focus.rootnameid, param2 = shortId project.id } |> toHref)
                        ]
                        [ text project.name ]
                    ]
                , case project.description of
                    Just x ->
                        div [ class "column is-8" ]
                            [ span [ class "is-discret is-smaller" ] [ text x ] ]

                    Nothing ->
                        text ""
                ]
            , div [ class "level is-smaller2 is-mobile" ]
                [ div [ class "level-left" ]
                    [ span [ class "is-discrete" ] <|
                        List.intersperse (text " ") <|
                            [ textH T.updated, text (formatDate conf.lang conf.now project.updateAt) ]
                    ]
                , div [ class "level-right" ] []
                ]
            ]
        , div [ class "media-right wrapped-container-33" ]
            [ div [ class "dropdown is-right" ]
                [ div [ class "dropdown-trigger is-w is-h" ]
                    [ div
                        [ class "ellipsis"
                        , attribute "aria-controls" ("edit-ellipsis-" ++ project.id)
                        , attribute "aria-haspopup" "true"
                        ]
                        [ A.icon "icon-more-horizontal icon-lg" ]
                    ]
                , div [ id ("edit-ellipsis-" ++ project.id), class "dropdown-menu", attribute "role" "menu" ]
                    [ div [ class "dropdown-content p-0" ] <|
                        [ div [ class "dropdown-item button-light", onClick (EditProject project) ] [ text T.edit ]
                        , hr [ class "dropdown-divider" ] []
                        , div [ class "dropdown-item button-light", onClick (ChangeStatus status_new project) ] [ text status_txt ]
                        ]
                    ]
                ]
            ]
        ]
