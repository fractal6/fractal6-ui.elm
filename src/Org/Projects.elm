{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2026 Fractale Co

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
import Components.ActionPanel as ActionPanel
import Components.AuthModal as AuthModal
import Components.ColorPicker as ColorPicker exposing (ColorPicker)
import Components.HelperBar as HelperBar
import Components.JoinOrga as JoinOrga
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.OrgaMenu as OrgaMenu
import Components.SearchBar exposing (viewSearchBarCol)
import Components.TreeMenu as TreeMenu exposing (viewSelectorTree)
import Components.UserInput as UserInput
import Dict exposing (Dict)
import Form exposing (isPostSendable)
import Form.Help as Help
import Form.NewTension as NTF
import Fractale.Codecs exposing (ActionType(..), DocType(..), Flags_, FractalBaseRoute(..), NodeFocus, basePathChanged, focusFromNameid, focusState, nameidEncoder, nameidFromFlags, nid2rootid, shortId, toLink)
import Fractale.Error exposing (viewGqlErrors, viewHttpErrors)
import Fractale.Form exposing (ProjectForm, initProjectForm)
import Fractale.User exposing (UserState(..), freshSessionOnOrgaSwitch)
import Fractale.View exposing (nodeType2str, projectStatus2str, viewCircleTarget, viewGoRoot, viewUrlForm)
import Generated.Route as Route exposing (toHref)
import Global exposing (Msg(..))
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, li, nav, option, p, select, span, table, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, autocomplete, autofocus, checked, class, classList, disabled, href, id, list, placeholder, required, rows, selected, style, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), RestData, isDataEmpty, withDefaultData, withMapData, withMaybeData)
import Maybe exposing (withDefault)
import ModelSchema exposing (ColumnDraft, LocalGraph, NewTensionInput(..), Node, ProjectFull, ProjectTemplateFull, ProjectTemplateLite, ProjectsCount)
import Page exposing (Document, Page)
import Ports
import Query.PatchNode exposing (addOneProject, removeOneProject, updateOneProject)
import Query.QueryNode exposing (getProjectTemplateById, getProjects, queryLocalGraph)
import RemoteData
import Requests exposing (fetchProjectCount, fetchProjectTemplatesTop, fetchProjectsSub, fetchProjectsTop)
import Schema.Enum.NodeType as NodeType
import Schema.Enum.ProjectColumnType as ProjectColumnType
import Schema.Enum.ProjectStatus as ProjectStatus
import Schema.Enum.TensionAction as TensionAction
import Schemas.TreeMenu exposing (ExpandedLines)
import Session exposing (CommonMsg, GlobalCmd(..), SessionCommon, Theme(..))
import String.Format as Format
import Text as T
import Time
import Url exposing (Url)
import Utils.Bool exposing (ternary)
import Utils.Bulma as B
import Utils.Cmd exposing (send, sendNow, sendSleep)
import Utils.Date exposing (formatDate)
import Utils.DomEvents exposing (key, onClickPD, onClickSP, outsideClickClose)
import Utils.Html exposing (showIf, textH, textT)
import Utils.Maybe exposing (unwrap)
import Utils.String exposing (decap, space_, upH)
import Utils.Url exposing (queryBuilder, queryParser)



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

                    DoPushSystemNotif a ->
                        ( Cmd.none, send (OnPushSystemNotif a) )

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
                        ( Cmd.map TreeMenuMsg <| sendSleep (TreeMenu.FetchNewNode nameid False) 333, Cmd.none )

                    DoAddNodes nodes ->
                        ( Cmd.map TreeMenuMsg <| send (TreeMenu.AddNodes nodes), Cmd.none )

                    DoUpdateNode nameid fun ->
                        ( Cmd.map TreeMenuMsg <| send (TreeMenu.UpdateNode nameid fun), Cmd.none )

                    DoDelNodes nameids ->
                        ( Cmd.map TreeMenuMsg <| send (TreeMenu.DelNodes nameids), Cmd.none )

                    DoMoveNode a b c ->
                        ( Cmd.map TreeMenuMsg <| send (TreeMenu.MoveNode a b c), Cmd.none )

                    DoUpdateDraft draftUpdate ->
                        ( Cmd.none, send (Global.UpdateDraft draftUpdate) )

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
    , project_result_del : GqlData String

    -- Column color picker (shared, scoped to one column at a time)
    , colorPicker : ColorPicker
    , colorPickerIdx : Maybe Int

    -- Project templates (picker for new project)
    , ptemplates : RestData (List ProjectTemplateLite)
    , ptemplateLoading : Bool
    , showTemplatePicker : Bool
    , selectedTemplateName : String

    -- Move
    , project_move : Maybe ProjectFull
    , move_target : Maybe Node
    , isMoveTargetOpen : Bool
    , move_expanded_lines : ExpandedLines
    , project_result_move : GqlData ProjectFull

    -- Common
    , session : SessionCommon
    , commonOp : CommonMsg Msg
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
    , userInput : UserInput.State
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
        , project_move = Nothing
        , move_target = Nothing
        , isMoveTargetOpen = False
        , move_expanded_lines = Dict.empty
        , project_result_move = NotAsked
        , userInput = UserInput.init [ model.node_focus.nameid ] True True model.session
    }


simpleKanban : List ColumnDraft
simpleKanban =
    [ { name = T.colTodoName
      , description = T.colTodoDesc
      , color = Just "#01FF70"
      , col_type = ProjectColumnType.NormalColumn
      }
    , { name = T.colInProgressName
      , description = T.colInProgressDesc
      , color = Just "#FF851B"
      , col_type = ProjectColumnType.NormalColumn
      }
    , { name = T.colDoneName
      , description = T.colDoneDesc
      , color = Just "#B10DC9"
      , col_type = ProjectColumnType.NormalColumn
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
    | TogglePeerCanEdit
    | ToggleGuestCanEdit
      -- Columns editor (new project)
    | AddColumn
    | RemoveColumn Int
    | ChangeColumnField Int String String
    | MoveColumn Int Int
    | OpenColumnColor Int
    | CloseColumnColor
    | SelectColumnColor String
    | SafeEdit Msg
    | SafeSend Msg
    | GotProjects (GqlData { projects : List ProjectFull, counts : ProjectsCount })
    | GotProjectsTop (RestData (List ProjectFull))
    | GotProjectsSub (RestData (List ProjectFull))
    | AddProject
    | EditProject ProjectFull
    | ChangeStatus ProjectStatus.ProjectStatus ProjectFull
    | CancelProject
      -- Project template picker
    | GotProjectTemplatesLite (RestData (List ProjectTemplateLite))
    | OpenTemplatePicker
    | CloseTemplatePicker
    | OnSelectProjectTemplate ProjectTemplateLite
    | GotProjectTemplateContent (GqlData ProjectTemplateFull)
    | OnSelectSimpleKanbanTemplate
    | SubmitAddProject Time.Posix
    | SubmitEditProject Time.Posix
    | SubmitDeleteProject String String Time.Posix
    | GotProject (GqlData ProjectFull)
    | GotProjectDel (GqlData String)
      -- Move
    | MoveProject ProjectFull
    | OnMoveTargetClick String
    | OnChangeMoveTarget Node
    | OnMoveExpandToggle String
    | CancelMoveProject
    | SubmitMoveProject Time.Posix
    | GotProjectMove (GqlData ProjectFull)
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
    | UserInputMsg UserInput.Msg



--
-- INIT
--


type alias Flags =
    Flags_


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        session =
            global.session

        query =
            session.common.query

        -- Focus
        newFocus =
            flags
                |> nameidFromFlags
                |> focusFromNameid

        -- What has changed
        fs =
            focusState ProjectsBaseUri session.referer global.url session.common.node_focus newFocus

        -- Session snapshot shared by the page model and every component init.
        -- Lexicon is dropped on a real org switch so views fall back to defaults
        -- until GotOrgaInfo lands the new org's lexicon.
        sessionCommon =
            freshSessionOnOrgaSwitch fs session.common

        model =
            { node_focus = newFocus
            , path_data =
                session.common.path_data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , hasUnsavedData = False
            , project_form = initProjectForm sessionCommon.user newFocus.nameid
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
            , colorPicker = ColorPicker.init
            , colorPickerIdx = Nothing
            , ptemplates = RemoteData.NotAsked
            , ptemplateLoading = False
            , showTemplatePicker = False
            , selectedTemplateName = T.simpleKanban

            -- Move
            , project_move = Nothing
            , move_target = Nothing
            , isMoveTargetOpen = False
            , move_expanded_lines = Dict.empty
            , project_result_move = NotAsked

            -- Common
            , session = sessionCommon
            , refresh_trial = 0
            , url = global.url
            , commonOp = CommonMsg NoMsg LogErr
            , empty = {}
            , tensionForm = NTF.init sessionCommon
            , helperBar = HelperBar.init ProjectsBaseUri global.url.query newFocus sessionCommon
            , help = Help.init sessionCommon
            , modal_confirm = ModalConfirm.init NoMsg
            , joinOrga = JoinOrga.init newFocus.nameid sessionCommon
            , authModal = AuthModal.init (Dict.get "puid" query |> Maybe.map List.head |> withDefault Nothing) sessionCommon
            , orgaMenu = OrgaMenu.init newFocus session.data.orga_menu session.data.orgs_data sessionCommon
            , treeMenu = TreeMenu.init ProjectsBaseUri global.url.query newFocus session.data.tree_menu session.data.tree_data sessionCommon
            , actionPanel = ActionPanel.init sessionCommon
            , userInput = UserInput.init [ newFocus.nameid ] True True sessionCommon
            }

        cmds =
            [ ternary fs.focusChange (queryLocalGraph session.apis newFocus.nameid True (GotPath True)) Cmd.none
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
                , fetchProjectsSub apis model.node_focus.nameid False GotProjectsSub

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

        TogglePeerCanEdit ->
            let
                form =
                    model.project_form

                newVal =
                    not (form.peerCanEditProject |> Maybe.withDefault False)

                newForm =
                    { form | peerCanEditProject = Just newVal }
            in
            ( { model | project_form = newForm, hasUnsavedData = True }, Cmd.none, Cmd.none )

        ToggleGuestCanEdit ->
            let
                form =
                    model.project_form

                newVal =
                    not (form.guestCanEditProject |> Maybe.withDefault False)

                newForm =
                    if newVal then
                        -- Enabling guest also enables peer
                        { form | guestCanEditProject = Just True, peerCanEditProject = Just True }

                    else
                        { form | guestCanEditProject = Just False }
            in
            ( { model | project_form = newForm, hasUnsavedData = True }, Cmd.none, Cmd.none )

        AddColumn ->
            let
                form =
                    model.project_form

                cols =
                    form.columns |> withDefault []

                newCol : ColumnDraft
                newCol =
                    { name = ""
                    , description = ""
                    , color = Just ColorPicker.initColor
                    , col_type = ProjectColumnType.NormalColumn
                    }

                newForm =
                    { form | columns = Just (cols ++ [ newCol ]) }

                newIdx =
                    List.length cols
            in
            ( { model | project_form = newForm, hasUnsavedData = True }
            , Ports.focusOn (ColorPicker.columnNameInputId "column-name" newIdx)
            , Cmd.none
            )

        RemoveColumn idx ->
            let
                form =
                    model.project_form

                cols =
                    form.columns |> withDefault []

                newForm =
                    { form | columns = Just (LE.removeAt idx cols) }
            in
            ( { model | project_form = newForm, hasUnsavedData = True }, Cmd.none, Cmd.none )

        ChangeColumnField idx field value ->
            let
                form =
                    model.project_form

                cols =
                    form.columns |> withDefault []

                newCols =
                    LE.updateAt idx
                        (\c ->
                            case field of
                                "name" ->
                                    { c | name = value }

                                "description" ->
                                    { c | description = value }

                                _ ->
                                    c
                        )
                        cols

                newForm =
                    { form | columns = Just newCols }
            in
            ( { model | project_form = newForm, hasUnsavedData = True }, Cmd.none, Cmd.none )

        MoveColumn idx delta ->
            let
                form =
                    model.project_form

                cols =
                    form.columns |> withDefault []

                target =
                    idx + delta
            in
            if target < 0 || target >= List.length cols then
                ( model, Cmd.none, Cmd.none )

            else
                let
                    newForm =
                        { form | columns = Just (LE.swapAt idx target cols) }
                in
                ( { model | project_form = newForm, hasUnsavedData = True }, Cmd.none, Cmd.none )

        OpenColumnColor idx ->
            let
                col_color =
                    model.project_form.columns
                        |> Maybe.andThen (LE.getAt idx)
                        |> Maybe.andThen .color

                newPicker =
                    model.colorPicker
                        |> ColorPicker.setColor col_color
                        |> ColorPicker.open
            in
            ( { model | colorPicker = newPicker, colorPickerIdx = Just idx }
            , Cmd.none
            , Cmd.none
            )

        CloseColumnColor ->
            ( { model
                | colorPicker = ColorPicker.close model.colorPicker
                , colorPickerIdx = Nothing
              }
            , Cmd.none
            , Cmd.none
            )

        SelectColumnColor color ->
            let
                form =
                    model.project_form

                cols =
                    form.columns |> withDefault []

                newCols =
                    case model.colorPickerIdx of
                        Just idx ->
                            LE.updateAt idx (\c -> { c | color = Just color }) cols

                        Nothing ->
                            cols

                newForm =
                    { form | columns = Just newCols }

                newPicker =
                    model.colorPicker
                        |> ColorPicker.setColor (Just color)
                        |> ColorPicker.close
            in
            ( { model
                | project_form = newForm
                , colorPicker = newPicker
                , colorPickerIdx = Nothing
                , hasUnsavedData = True
              }
            , Ports.click "body"
            , Cmd.none
            )

        SafeEdit msg ->
            if model.hasUnsavedData then
                ( model
                , send <|
                    DoModalConfirmOpen (SafeSend msg)
                        { message = Nothing
                        , txts = [ ( T.confirmUnsafe, "" ) ]
                        , confirmClass = "is-success"
                        , confirmLabel = T.confirm
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
            ( newModel, Cmd.none, Ports.bulma_driver "" )

        GotProjectsTop result ->
            let
                newModel =
                    { model | projects_top = result }
            in
            ( newModel, Cmd.none, Ports.bulma_driver "" )

        GotProjectsSub result ->
            let
                newModel =
                    { model | projects_sub = result }
            in
            ( newModel, Cmd.none, Ports.bulma_driver "" )

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
                    , selectedTemplateName = T.simpleKanban
                    , showTemplatePicker = False
                  }
                , Cmd.batch
                    [ Ports.bulma_driver "edit-project"
                    , case model.ptemplates of
                        RemoteData.NotAsked ->
                            fetchProjectTemplatesTop apis model.node_focus.nameid True GotProjectTemplatesLite

                        _ ->
                            Cmd.none
                    ]
                , Cmd.none
                )

        GotProjectTemplatesLite result ->
            ( { model | ptemplates = result }, Cmd.none, Cmd.none )

        OpenTemplatePicker ->
            ( { model | showTemplatePicker = True }, Cmd.none, Cmd.none )

        CloseTemplatePicker ->
            ( { model | showTemplatePicker = False }, Cmd.none, Cmd.none )

        OnSelectProjectTemplate tpl ->
            ( { model | ptemplateLoading = True, showTemplatePicker = False, selectedTemplateName = tpl.name }
            , getProjectTemplateById apis tpl.id GotProjectTemplateContent
            , Cmd.none
            )

        GotProjectTemplateContent result ->
            case result of
                Success tpl ->
                    let
                        form =
                            model.project_form
                    in
                    ( { model
                        | ptemplateLoading = False
                        , project_form = { form | columns = Just tpl.columns }
                        , hasUnsavedData = True
                      }
                    , Cmd.none
                    , Cmd.none
                    )

                Failure _ ->
                    ( { model | ptemplateLoading = False }, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        OnSelectSimpleKanbanTemplate ->
            let
                form =
                    model.project_form
            in
            ( { model
                | project_form = { form | columns = Just simpleKanban }
                , showTemplatePicker = False
                , selectedTemplateName = T.simpleKanban
                , hasUnsavedData = True
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
                        , nameid = project.parentnameid |> withDefault f.nameid
                        , post =
                            Dict.fromList
                                ([ ( "name", project.name ) ]
                                    ++ (project.description |> Maybe.map (\x -> [ ( "description", x ) ]) |> withDefault [])
                                    ++ [ ( "old_name", project.name ), ( "old_nameid", nameidEncoder project.name ) ]
                                )
                        , collaborators_add = List.map .username project.collaborators
                        , peerCanEditProject = Just project.peerCanEditProject
                        , guestCanEditProject = Just project.guestCanEditProject
                    }

                newUserInput =
                    UserInput.init [ model.node_focus.nameid ] True True model.session
            in
            ( { model
                | project_add = False
                , project_edit = Just project
                , project_form = newForm
                , userInput = newUserInput
              }
            , Cmd.batch
                [ Ports.bulma_driver "edit-project"
                , List.map (\u -> Cmd.map UserInputMsg (send (UserInput.OnClickUser { username = u.username, name = Nothing }))) project.collaborators
                    |> Cmd.batch
                ]
            , Cmd.none
            )

        ChangeStatus status project ->
            let
                form =
                    model.project_form

                newForm =
                    { form
                        | id = project.id
                        , nameid = project.parentnameid |> withDefault form.nameid
                        , status = Just status
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

                originalCollabs =
                    model.project_edit
                        |> Maybe.map (.collaborators >> List.map .username)
                        |> withDefault []

                collabsToAdd =
                    List.filter (\u -> not (List.member u originalCollabs)) form.collaborators_add

                collabsToRemove =
                    List.filter (\u -> not (List.member u form.collaborators_add)) originalCollabs

                newForm =
                    { form
                        | post =
                            Dict.insert "updatedAt" (fromTime time) form.post
                        , collaborators_add = collabsToAdd
                        , collaborators_remove = collabsToRemove
                    }
            in
            ( { model | project_result = LoadingSlowly, project_form = newForm }
            , updateOneProject apis newForm GotProject
            , Cmd.none
            )

        SubmitDeleteProject id nodeNameid _ ->
            let
                f =
                    model.project_form

                newForm =
                    { f | id = id, nameid = nodeNameid }
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
                    ( { model
                        | project_result = result
                        , projects = Success new_d
                        , projects_count = Success new_c
                        , projects_sub = RemoteData.map (LE.setIf (\x -> x.id == project.id) project) model.projects_sub
                        , project_add = False
                        , project_edit = Nothing
                      }
                        |> resetForm
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
                    ( { model | refresh_trial = i }, sendSleep (Submit <| SubmitDeleteProject model.project_form.id model.project_form.nameid) 500, send UpdateUserToken )

                OkAuth _ ->
                    let
                        projectId =
                            model.project_form.id

                        newProjects =
                            withDefaultData [] model.projects
                                |> List.filter (\x -> x.id /= projectId)

                        newSub =
                            RemoteData.map (List.filter (\x -> x.id /= projectId)) model.projects_sub
                    in
                    ( { model | project_result_del = NotAsked, projects = Success newProjects, projects_sub = newSub, project_add = False, project_edit = Nothing } |> resetForm
                    , Cmd.none
                    , Cmd.none
                    )

                _ ->
                    ( { model | project_result_del = result }, Cmd.none, Cmd.none )

        -- Move
        MoveProject project ->
            ( { model | project_move = Just project, project_result_move = NotAsked, move_target = Nothing, isMoveTargetOpen = False }
            , Cmd.batch [ Cmd.map TreeMenuMsg (send TreeMenu.OnRequireData), Ports.open_modal "MoveProjectModal" ]
            , Cmd.none
            )

        OnMoveTargetClick _ ->
            ( { model | isMoveTargetOpen = not model.isMoveTargetOpen }, Cmd.none, Cmd.none )

        OnChangeMoveTarget node ->
            ( { model | move_target = Just node, isMoveTargetOpen = False }, Cmd.none, Cmd.none )

        OnMoveExpandToggle nid ->
            ( { model
                | move_expanded_lines =
                    if Dict.member nid model.move_expanded_lines then
                        Dict.remove nid model.move_expanded_lines

                    else
                        Dict.insert nid False model.move_expanded_lines
              }
            , Cmd.none
            , Cmd.none
            )

        CancelMoveProject ->
            ( { model | project_move = Nothing, move_target = Nothing, isMoveTargetOpen = False, move_expanded_lines = Dict.empty, project_result_move = NotAsked }
            , Ports.close_modal
            , Cmd.none
            )

        SubmitMoveProject _ ->
            case ( model.project_move, model.move_target ) of
                ( Just project, Just target ) ->
                    let
                        f =
                            model.project_form

                        oldParent =
                            project.nodes |> List.head |> Maybe.map .nameid |> withDefault model.node_focus.nameid

                        newForm =
                            { f
                                | id = project.id
                                , nameid = target.nameid
                                , post = Dict.insert "move_from" oldParent f.post
                            }
                    in
                    ( { model | project_result_move = LoadingSlowly, project_form = newForm }
                    , updateOneProject apis newForm GotProjectMove
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        GotProjectMove result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | project_result_move = NotAsked }, Ports.raiseAuthModal model.project_form.uctx, Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep (Submit SubmitMoveProject) 500, send UpdateUserToken )

                OkAuth project ->
                    let
                        projectId =
                            model.project_form.id

                        targetNameid =
                            model.project_form.nameid

                        -- Remove from both lists
                        mainList =
                            withDefaultData [] model.projects
                                |> List.filter (\x -> x.id /= projectId)

                        subList =
                            RemoteData.map (List.filter (\x -> x.id /= projectId)) model.projects_sub

                        -- Add to the appropriate list based on target
                        ( newMain, newSub ) =
                            if targetNameid == model.node_focus.nameid then
                                -- Moved to current circle -> add to main list
                                ( project :: mainList, subList )

                            else if String.startsWith model.node_focus.nameid targetNameid then
                                -- Moved to a sub-circle -> add to sub list
                                ( mainList, RemoteData.map (\l -> project :: l) subList )

                            else
                                -- Moved elsewhere -> just remove
                                ( mainList, subList )
                    in
                    ( { model | project_result_move = NotAsked, projects = Success newMain, projects_sub = newSub } |> resetForm
                    , Ports.close_modal
                    , Cmd.none
                    )

                _ ->
                    ( { model | project_result_move = result }, Cmd.none, Cmd.none )

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
                state =
                    case msg of
                        NTF.OnOpen _ _ ->
                            model.tensionForm
                                |> NTF.setCurrentDraft global.session.data.drafts.newTension
                                |> NTF.setSessionTemplates global.session.data.tension_templates

                        _ ->
                            model.tensionForm

                ( tf, out ) =
                    NTF.update apis msg state

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
                state =
                    case msg of
                        JoinOrga.OnOpen _ _ ->
                            JoinOrga.setCurrentDraft global.session.data.drafts.newInvite model.joinOrga

                        _ ->
                            model.joinOrga

                ( data, out ) =
                    JoinOrga.update apis msg state

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

        UserInputMsg msg ->
            let
                ( data, out ) =
                    UserInput.update apis msg model.userInput

                form =
                    model.project_form

                newCollabs =
                    case out.result of
                        Just ( True, users ) ->
                            form.collaborators_add ++ List.map .username users

                        Just ( False, users ) ->
                            List.filter (\u -> not (List.member u (List.map .username users))) form.collaborators_add

                        Nothing ->
                            form.collaborators_add

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | userInput = data, project_form = { form | collaborators_add = newCollabs } }
            , out.cmds |> List.map (\m -> Cmd.map UserInputMsg m) |> List.append cmds |> Cmd.batch
            , Cmd.batch gcmds
            )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ model =
    [ Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    ]
        ++ (if model.colorPicker.isOpen then
                [ Events.onMouseUp (outsideClickClose "colorPicker" CloseColumnColor)
                , Events.onKeyUp (key "Escape" CloseColumnColor)
                ]

            else
                []
           )
        ++ (if model.showTemplatePicker then
                [ Events.onMouseUp (outsideClickClose "ptemplate-picker" CloseTemplatePicker)
                , Events.onKeyUp (key "Escape" CloseTemplatePicker)
                ]

            else
                []
           )
        ++ (if model.project_move /= Nothing then
                [ Ports.mcPD Ports.closeModalFromJs LogErr (\_ -> CancelMoveProject) ]

            else
                []
           )
        ++ (HelperBar.subscriptions |> List.map (\s -> Sub.map HelperBarMsg s))
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        ++ (NTF.subscriptions model.tensionForm |> List.map (\s -> Sub.map NewTensionMsg s))
        ++ (JoinOrga.subscriptions model.joinOrga |> List.map (\s -> Sub.map JoinOrgaMsg s))
        ++ (AuthModal.subscriptions |> List.map (\s -> Sub.map AuthModalMsg s))
        ++ (OrgaMenu.subscriptions |> List.map (\s -> Sub.map OrgaMenuMsg s))
        ++ (TreeMenu.subscriptions |> List.map (\s -> Sub.map TreeMenuMsg s))
        ++ (ActionPanel.subscriptions model.actionPanel |> List.map (\s -> Sub.map ActionPanelMsg s))
        ++ (UserInput.subscriptions model.userInput |> List.map (\s -> Sub.map UserInputMsg s))
        |> Sub.batch



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    let
        helperData =
            { path_data = withMaybeData model.path_data
            , isPanelOpen = ActionPanel.isOpen_ "actionPanelHelper" model.actionPanel
            , orgaInfo = global.session.data.orgaInfo
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
        , viewMoveProjectModal model
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ class "columns is-centered" ]
        [ div [ class "column is-12 is-11-desktop is-9-fullhd" ]
            [ if model.project_add then
                viewNewOrEditProject model.session True model

              else if model.project_edit /= Nothing then
                viewNewOrEditProject model.session False model

              else
                viewDefault global.session.common.user model
            ]
        ]


viewNewOrEditProject : SessionCommon -> Bool -> Model -> Html Msg
viewNewOrEditProject session isNew model =
    let
        title =
            ternary isNew T.newProject T.editProject

        submit_txt =
            ternary isNew T.create T.save

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

        collabsChanged =
            case model.project_edit of
                Just p ->
                    List.sort model.project_form.collaborators_add /= List.sort (List.map .username p.collaborators)

                Nothing ->
                    not (List.isEmpty model.project_form.collaborators_add)

        permsChanged =
            case model.project_edit of
                Just p ->
                    (model.project_form.peerCanEditProject /= Just p.peerCanEditProject)
                        || (model.project_form.guestCanEditProject /= Just p.guestCanEditProject)

                Nothing ->
                    False

        isSendable =
            if isNew then
                isPostSendable [ "name" ] post

            else
                isPostSendable [ "name" ] post
                    && ((Just name /= Maybe.map .name model.project_edit)
                            || (Just description /= unwrap Nothing .description model.project_edit)
                            || collabsChanged
                            || permsChanged
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
                    div [ class "f6-error message is-danger is-small mt-1" ]
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
            , hr [ class "mt-6 mb-3" ] []
            , div [ class "field" ]
                [ div [ class "label" ] [ text T.collaborators ]
                , p [ class "help" ] [ text T.collaboratorsHelp ]
                , UserInput.view { label_text = text "", showEmail = False, placeholder_text = Nothing } model.userInput |> Html.map UserInputMsg
                ]
            , hr [ class "mt-6 mb-3" ] []
            , div [ class "field" ]
                [ div [ class "label" ] [ text T.permissions ]
                , Html.label [ class "checkbox" ]
                    [ input
                        [ type_ "checkbox"
                        , checked (model.project_form.peerCanEditProject |> Maybe.withDefault False)
                        , onClick TogglePeerCanEdit
                        ]
                        []
                    , span [ class "ml-2" ] [ text T.peerCanEditProject ]
                    ]
                , p [ class "help" ] [ text T.peerCanEditProjectHelp ]
                ]
            , div [ class "field" ]
                [ Html.label [ class "checkbox" ]
                    [ input
                        [ type_ "checkbox"
                        , checked (model.project_form.guestCanEditProject |> Maybe.withDefault False)
                        , onClick ToggleGuestCanEdit
                        ]
                        []
                    , span [ class "ml-2" ] [ text T.guestCanEditProject ]
                    ]
                , p [ class "help" ] [ text T.guestCanEditProjectHelp ]
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
                [ viewColumnsEditor model
                , p [ class "is-size-7 has-text-grey mt-3" ]
                    [ text (T.projectCaptionSimple session.lexicon) ]
                ]

          else
            text ""
        ]


viewColumnRow : List String -> Maybe Int -> Int -> Int -> ColumnDraft -> Html Msg
viewColumnRow colors activeIdx nCols idx col =
    ColorPicker.viewColumnRow
        { col = col
        , idx = idx
        , nCols = nCols
        , isPickerActive = activeIdx == Just idx
        , colors = colors
        , inputIdPrefix = "column-name"
        , onOpenColor = OpenColumnColor idx
        , onCloseColor = CloseColumnColor
        , onSelectColor = SelectColumnColor
        , onChangeName = ChangeColumnField idx "name"
        , onChangeDesc = ChangeColumnField idx "description"
        , onMove = MoveColumn idx
        , onRemove = RemoveColumn idx
        }


viewColumnsEditor : Model -> Html Msg
viewColumnsEditor model =
    let
        cols =
            model.project_form.columns |> withDefault []

        nCols =
            List.length cols
    in
    div []
        [ div [ class "level mb-3 is-mobile" ]
            [ div [ class "level-left" ]
                [ div []
                    [ div [ class "label mb-0" ] [ text T.columnLayout ]
                    , p [ class "is-size-7 has-text-grey" ]
                        [ text T.customizeColumns ]
                    ]
                ]
            , div [ class "level-right" ]
                [ viewTemplatePicker model ]
            ]
        , div [] (List.indexedMap (viewColumnRow model.colorPicker.colors model.colorPickerIdx nCols) cols)
        , button
            [ class "button is-fullwidth is-weak mt-2"
            , style "border" "1px dashed var(--bulma-border)"
            , onClick AddColumn
            ]
            [ A.icon1 "icon-plus" T.addColumn ]
        ]


viewTemplatePicker : Model -> Html Msg
viewTemplatePicker model =
    let
        templates =
            case model.ptemplates of
                RemoteData.Success t ->
                    t

                _ ->
                    []
    in
    div
        [ id "ptemplate-picker"
        , class "dropdown is-right"
        , classList [ ( "is-active", model.showTemplatePicker ) ]
        , style "position" "relative"
        ]
        [ div [ class "dropdown-trigger" ]
            [ button
                [ class "tag is-weak button-light"
                , classList [ ( "is-loading", model.ptemplateLoading ) ]
                , attribute "aria-haspopup" "true"
                , onClickPD (ternary model.showTemplatePicker CloseTemplatePicker OpenTemplatePicker)
                ]
                [ text model.selectedTemplateName
                , span [ class "ml-1" ] [ A.icon "icon-chevron-down" ]
                ]
            ]
        , div [ class "dropdown-menu", attribute "role" "menu" ]
            [ div [ class "dropdown-content" ] <|
                button
                    [ class "dropdown-item button-light has-text-left"
                    , classList [ ( "is-active", model.selectedTemplateName == T.simpleKanban ) ]
                    , onClickPD OnSelectSimpleKanbanTemplate
                    ]
                    [ text T.simpleKanban ]
                    :: (if List.isEmpty templates then
                            []

                        else
                            hr [ class "dropdown-divider" ] []
                                :: List.map (viewTemplatePickerItem model.selectedTemplateName) templates
                       )
            ]
        ]


viewTemplatePickerItem : String -> ProjectTemplateLite -> Html Msg
viewTemplatePickerItem selectedName t =
    button
        [ class "dropdown-item button-light has-text-left"
        , classList [ ( "is-active", selectedName == t.name ) ]
        , onClickPD (OnSelectProjectTemplate t)
        ]
        [ text t.name
        , case t.description of
            Just d ->
                span [ class "is-size-7 has-text-grey ml-2" ] [ text d ]

            Nothing ->
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
            , column_class = "is-8"
            , field_class = ""
            , placeholder_txt = T.searchProjects
            }
    in
    div []
        [ div [ class "columns is-centered" ]
            [ div [ class "column is-tree-quarter" ]
                [ viewSearchBarCol opSearch model.pattern_init model.pattern ]
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
    let
        canEditProject project =
            case model.session.user of
                LoggedIn uctx ->
                    getProjectRights uctx project model.path_data

                LoggedOut ->
                    False

        statusFilter =
            statusDecoder model.statusFilter
    in
    div [ class "columns" ]
        [ div [ class "column is-12" ]
            [ viewProjectsListHeader model.node_focus model.projects_count model.statusFilter (isDataEmpty model.projects)
            , viewProjectsList canEditProject model.commonOp model.session model.node_focus model.pattern_init model.statusFilter model.projects
            , showIf (model.statusFilter == OpenStatus) <|
                viewProjectsSub canEditProject model.commonOp model.session model.node_focus model.projects_sub
            ]
        ]


viewProjectsListHeader : NodeFocus -> GqlData ProjectsCount -> StatusFilter -> Bool -> Html Msg
viewProjectsListHeader focus counts statusFilter isEmpty =
    let
        checked =
            A.icon1 "icon-check has-text-success" ""

        unchecked =
            A.icon1 "icon-check has-text-success is-invisible" ""

        showGoRoot =
            focus.nameid /= focus.rootnameid && not isEmpty
    in
    div
        [ class "pt-3 pb-3 has-border-light has-background-header"
        , attribute "style" "border-top-left-radius: var(--bulma-radius-large); border-top-right-radius: var(--bulma-radius-large); border-bottom: 0 !important;"
        ]
        [ div [ class "level m-0 is-mobile" ]
            [ div [ class "level-left px-3" ]
                [ viewProjectsCount counts statusFilter
                , showIf showGoRoot <|
                    viewGoRoot "is-hidden-mobile is-align-self-flex-start px-5" OnGoRoot
                ]
            , div [ class "level-right px-3" ]
                []
            , showIf showGoRoot <|
                viewGoRoot "is-hidden-tablet px-5" OnGoRoot
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


viewProjectsList : (ProjectFull -> Bool) -> CommonMsg Msg -> SessionCommon -> NodeFocus -> String -> StatusFilter -> GqlData (List ProjectFull) -> Html Msg
viewProjectsList canEditProject commonOp session focus pattern statusFilter data =
    div
        [ class "box is-shrinked"
        , attribute "style" "border-top-left-radius: 0px; border-top-right-radius: 0px;"
        , classList [ ( "spinner", data == LoadingSlowly ) ]
        ]
        [ case data of
            Success items ->
                if List.length items > 0 then
                    items
                        |> List.map (\x -> Lazy.lazy6 mediaProject (canEditProject x) commonOp session focus statusFilter x)
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
                        , showIf (focus.nameid /= focus.rootnameid) <|
                            viewGoRoot "" OnGoRoot
                        ]

            Failure err ->
                viewGqlErrors err

            _ ->
                div [] []
        ]


viewProjectsSub : (ProjectFull -> Bool) -> CommonMsg Msg -> SessionCommon -> NodeFocus -> RestData (List ProjectFull) -> Html Msg
viewProjectsSub canEditProject commonOp session focus data =
    case data of
        RemoteData.Success items ->
            if List.length items > 0 then
                div [ class "mt-6" ]
                    [ h2 [ class "subtitle is-size-6 has-text-weight-semibold" ] [ text T.subProjects ]
                    , div [ class "box is-shrinked" ]
                        (List.map (\p -> viewProjectSubRow (canEditProject p) commonOp session focus p) items)
                    ]

            else
                text ""

        RemoteData.Failure _ ->
            text ""

        _ ->
            text ""


viewProjectSubRow : Bool -> CommonMsg Msg -> SessionCommon -> NodeFocus -> ProjectFull -> Html Msg
viewProjectSubRow canEdit commonOp session focus project =
    viewProjectRow canEdit commonOp session focus project <|
        [ hr [ class "dropdown-divider" ] []
        , div [ class "dropdown-item button-light", onClick (ChangeStatus ProjectStatus.Closed project) ] [ text T.close ]
        ]


mediaProject : Bool -> CommonMsg Msg -> SessionCommon -> NodeFocus -> StatusFilter -> ProjectFull -> Html Msg
mediaProject canEdit commonOp session focus statusFilter project =
    let
        ( status_new, status_txt ) =
            case statusDecoder statusFilter of
                ProjectStatus.Open ->
                    ( ProjectStatus.Closed, T.close )

                ProjectStatus.Closed ->
                    ( ProjectStatus.Open, T.reopen )
    in
    viewProjectRow canEdit commonOp session focus project <|
        [ hr [ class "dropdown-divider" ] []
        , div [ class "dropdown-item button-light", onClick (ChangeStatus status_new project) ] [ text status_txt ]
        ]
            ++ (if List.length project.nodes > 1 then
                    [ hr [ class "dropdown-divider" ] []
                    , div
                        [ class "dropdown-item button-light has-text-warning"
                        , onClick
                            (DoModalConfirmOpen (Submit <| SubmitDeleteProject project.id focus.nameid)
                                { message = Nothing
                                , txts = [ ( T.confirmDetachProject, "" ) ]
                                , confirmClass = "is-warning"
                                , confirmLabel = T.confirm
                                }
                            )
                        ]
                        [ text T.unlink ]
                    ]

                else
                    []
               )


viewProjectRow : Bool -> CommonMsg Msg -> SessionCommon -> NodeFocus -> ProjectFull -> List (Html Msg) -> Html Msg
viewProjectRow canEdit commonOp session focus project extraMenuItems =
    div
        [ class "media mediaBox is-hoverable" ]
        [ div [ class "media-left" ] []
        , div [ class "media-content" ]
            [ div [ class "columns mb-1" ]
                [ div [ class ("column pb-0 " ++ ternary (project.description == Nothing) "is-8" "is-4") ]
                    [ a
                        [ class "has-text-weight-semibold is-human discrete-link"
                        , href (Route.Project_Dynamic_Dynamic { param1 = focus.rootnameid, param2 = shortId project.id } |> toHref)
                        ]
                        [ text project.name ]
                    ]
                , case project.description of
                    Just x ->
                        div [ class "column pb-0 is-4" ]
                            [ span [ class "is-discret is-smaller" ] [ text x ] ]

                    Nothing ->
                        text ""
                , div [ class "column pb-0 is-4 has-text-right" ]
                    (project.nodes
                        |> List.map (\node -> viewCircleTarget ProjectsBaseUri commonOp "is-small" node)
                    )
                ]
            , div [ class "level is-smaller2 is-mobile" ]
                [ div [ class "level-left" ]
                    [ span [ class "is-discrete" ] <|
                        List.intersperse (text " ") <|
                            [ textH T.updated, text (formatDate session.lang session.now project.updateAt) ]
                    ]
                ]
            ]
        , if canEdit then
            div [ class "media-right wrapped-container-33" ]
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
                            ]
                                ++ (if List.length project.nodes == 1 then
                                        [ div [ class "dropdown-item button-light", onClick (MoveProject project) ] [ text T.move ] ]

                                    else
                                        []
                                   )
                                ++ extraMenuItems
                        ]
                    ]
                ]

          else
            text ""
        ]


viewMoveProjectModal : Model -> Html Msg
viewMoveProjectModal model =
    case model.project_move of
        Nothing ->
            text ""

        Just project ->
            let
                isTargetOpen =
                    model.isMoveTargetOpen

                tree_data =
                    TreeMenu.getOrgaData_ model.treeMenu

                linkedNodes =
                    List.map .nameid project.nodes

                selected =
                    case model.move_target of
                        Just t ->
                            t.nameid :: linkedNodes

                        Nothing ->
                            linkedNodes

                isSendable =
                    case model.move_target of
                        Just t ->
                            not (List.member t.nameid linkedNodes)

                        Nothing ->
                            False

                isLoading =
                    model.project_result_move == LoadingSlowly
            in
            div [ id "MoveProjectModal", class "modal modal-fx-fadeIn is-active", attribute "data-modal-close" "closeModalFromJs" ]
                [ div [ class "modal-background modal-escape", attribute "data-modal" "MoveProjectModal", onClick CancelMoveProject ] []
                , div [ class "modal-card submitFocus" ]
                    [ div [ class "modal-card-head" ]
                        [ div [ class "modal-card-title is-wrapped is-size-6 has-text-weight-semibold" ]
                            [ span [] [ text (T.moveProject ++ ": "), span [ class "has-text-primary has-text-weight-extrabold" ] [ text project.name ] ]
                            ]
                        ]
                    , div [ class "modal-card-body" ]
                        [ div [ class "level is-flex-inline" ]
                            [ span [ class "level-right" ] [ text (T.newReceiver ++ ":") ]
                            , div [ class "level-item" ]
                                [ B.dropdownLight
                                    { dropdown_id = "move-project-target-menu"
                                    , isOpen = isTargetOpen
                                    , dropdown_cls = ""
                                    , button_cls = ""
                                    , button_html =
                                        case model.move_target of
                                            Nothing ->
                                                span [ class "button" ] [ text T.selectADestination, span [ class "ml-2 icon-chevron-down" ] [] ]

                                            Just t ->
                                                span [ class "button is-rounded has-border" ] [ text t.name, span [ class "ml-2 icon-chevron-down" ] [] ]
                                    , menu_cls = ""
                                    , content_cls = "p-0 has-border-light"
                                    , content_html = viewSelectorTree OnChangeMoveTarget OnMoveExpandToggle selected model.move_expanded_lines tree_data
                                    , msg = ternary isTargetOpen (OnMoveTargetClick "") (OnMoveTargetClick "open")
                                    }
                                ]
                            ]
                        ]
                    , div [ class "modal-card-foot" ]
                        [ case model.project_result_move of
                            Failure err ->
                                div [ class "field" ] [ viewGqlErrors err ]

                            _ ->
                                text ""
                        , div [ class "field level is-mobile" ]
                            [ div [ class "level-left" ]
                                [ button [ class "button", onClick CancelMoveProject ] [ text T.cancel ]
                                ]
                            , div [ class "level-right" ]
                                [ button
                                    [ class "button defaultSubmit is-success"
                                    , classList [ ( "is-loading", isLoading ) ]
                                    , disabled (not isSendable || isLoading)
                                    , onClick (Submit SubmitMoveProject)
                                    ]
                                    [ text T.move ]
                                ]
                            ]
                        ]
                    ]
                ]
