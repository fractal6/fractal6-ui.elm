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


module Org.Settings exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Browser.Navigation as Nav
import Bulk exposing (..)
import Bulk.Codecs exposing (ActionType(..), DocType(..), Flags_, FractalBaseRoute(..), NodeFocus, focusFromNameid, focusState, nameidFromFlags, nid2rootid, toLink)
import Bulk.Error exposing (viewGqlErrors, viewHttpErrors)
import Bulk.View exposing (helperButton, tensionIcon2, tensionIcon3, viewGoRoot, viewLabel, viewRoleExt, viewTensionTypePicker, viewUsers)
import Components.ActionPanel as ActionPanel
import Components.AuthModal as AuthModal
import Components.ColorPicker as ColorPicker exposing (ColorPicker)
import Components.Comments exposing (viewCommentInputHeader)
import Components.HelperBar as HelperBar
import Components.JoinOrga as JoinOrga
import Components.LabelSearchPanel as LabelSearchPanel
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.NodeDoc as NodeDoc exposing (NodeDoc, viewMandateInput, viewMandateSection, viewSelectAuthority)
import Components.OrgaMenu as OrgaMenu
import Components.TreeMenu as TreeMenu
import Components.UserSearchPanel as UserSearchPanel
import Dict
import Extra exposing (showIf, space_, ternary, textT, unwrap, unwrap2, upH)
import Extra.Events exposing (onClickPD)
import Extra.Url exposing (queryBuilder, queryParser)
import Extra.Views exposing (showMsg)
import Form.Help as Help
import Form.NewTension as NTF
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.NodeVisibility as NodeVisibility
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (toHref)
import Global exposing (Msg(..), send, sendNow, sendSleep)
import Html exposing (Html, a, button, div, h2, h3, hr, i, input, label, li, nav, option, p, select, span, table, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, autofocus, checked, class, classList, colspan, disabled, for, href, id, name, placeholder, rows, selected, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy as Lazy
import Json.Encode as JE
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), RestData, withDefaultData, withMapData, withMaybeData)
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.PatchNode exposing (addOneLabel, addOneRole, addOneTensionTemplate, removeOneLabel, removeOneRole, removeOneTensionTemplate, updateOneLabel, updateOneRole, updateOneTensionTemplate)
import Query.QueryNode exposing (getCircleRights, getLabels, getRoles, getTensionTemplates, queryLocalGraph)
import RemoteData
import Requests exposing (fetchLabelsSub, fetchLabelsTop, fetchRolesSub, fetchRolesTop, fetchTensionTemplatesSub, fetchTensionTemplatesTop, setGuestCanCreateTension, setIsPinnedTensionfetchRecursively, setIsTemplateTensionOnly, setLexicon, setUserCanJoin)
import Session exposing (CommonMsg, GlobalCmd(..), LabelSearchPanelOnClickAction(..), UserSearchPanelOnClickAction(..))
import Text as T
import Time
import Url exposing (Url)


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
    , menuFocus : MenuSettings
    , menuList : List MenuSettings
    , colorPicker : ColorPicker
    , artefact_form : ArtefactNodeForm
    , hasUnsavedData : Bool

    -- Labels
    , labels : GqlData (List LabelFull)
    , labels_top : RestData (List Label)
    , labels_sub : RestData (List Label)
    , label_add : Bool
    , label_edit : Maybe LabelFull
    , label_result : GqlData LabelFull
    , label_result_del : GqlData LabelFull
    , label_anim_enter : Maybe String

    -- Roles
    , nodeDoc : NodeDoc
    , showMandate : String
    , roles : GqlData (List RoleExtFull)
    , roles_top : RestData (List RoleExt)
    , roles_sub : RestData (List RoleExt)
    , role_add : Bool
    , role_edit : Maybe RoleExtFull
    , role_result : GqlData RoleExtFull
    , role_result_del : GqlData RoleExtFull
    , role_anim_enter : Maybe String

    -- Templates
    , template_form : TensionTemplateForm
    , templates : GqlData (List TensionTemplateFull)
    , labelsPanel : LabelSearchPanel.State
    , assigneesPanel : UserSearchPanel.State
    , templates_top : RestData (List TensionTemplateLite)
    , templates_sub : RestData (List TensionTemplateLite)
    , template_add : Bool
    , template_edit : Maybe TensionTemplateFull
    , template_result : GqlData TensionTemplateFull
    , template_result_del : GqlData TensionTemplateFull
    , template_anim_enter : Maybe String

    -- Orga
    , orga_rights : GqlData NodeRights
    , switch_result : RestData Bool
    , switch_index : Int
    , lexicon : Dict.Dict String String
    , lexicon_input : String
    , mandate_input : String
    , lexicon_result : RestData Bool

    -- Common
    , modal_confirm : ModalConfirm Msg
    , refresh_trial : Int
    , url : Url
    , empty : {}
    , commonOp : CommonMsg Msg

    -- Components
    , helperBar : HelperBar.State
    , actionPanel : ActionPanel.State
    , help : Help.State
    , tensionForm : NTF.State
    , joinOrga : JoinOrga.State
    , authModal : AuthModal.State
    , orgaMenu : OrgaMenu.State
    , treeMenu : TreeMenu.State
    }


type MenuSettings
    = LabelsMenu
    | RolesMenu
    | TemplatesMenu
    | GlobalMenu
    | EditMenu


menuList : List MenuSettings
menuList =
    [ LabelsMenu, RolesMenu, TemplatesMenu, EditMenu, GlobalMenu ]


menuEncoder : MenuSettings -> String
menuEncoder menu =
    case menu of
        LabelsMenu ->
            "labels"

        RolesMenu ->
            "roles"

        TemplatesMenu ->
            "templates"

        GlobalMenu ->
            "global"

        EditMenu ->
            --redirect
            ""


menuDecoder : String -> MenuSettings
menuDecoder menu =
    case menu of
        "labels" ->
            LabelsMenu

        "roles" ->
            RolesMenu

        "templates" ->
            TemplatesMenu

        "global" ->
            GlobalMenu

        _ ->
            LabelsMenu


menuToString : MenuSettings -> String
menuToString menu =
    case menu of
        LabelsMenu ->
            T.labels

        RolesMenu ->
            T.templateRoles

        TemplatesMenu ->
            T.tensionTemplates

        GlobalMenu ->
            T.organisation

        EditMenu ->
            T.editThisCircle ++ " 🡕"


menuToIcon : MenuSettings -> String
menuToIcon menu =
    case menu of
        LabelsMenu ->
            "icon-tag"

        RolesMenu ->
            "icon-user"

        TemplatesMenu ->
            "icon-exchange"

        GlobalMenu ->
            "icon-shield"

        EditMenu ->
            "icon-edit"


resetForm : Model -> Model
resetForm model =
    { model
        | artefact_form = initArtefactNodeForm (LoggedIn model.artefact_form.uctx) model.node_focus.nameid ColorPicker.initColor
        , hasUnsavedData = False
        , label_result = NotAsked
        , label_result_del = NotAsked
        , role_result = NotAsked
        , role_result_del = NotAsked
        , template_form = initTensionTemplateForm (LoggedIn model.template_form.uctx) model.node_focus.nameid
        , template_result = NotAsked
        , template_result_del = NotAsked
    }



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

        apis =
            session.apis

        menu =
            Dict.get "m" session.common.query |> withDefault [] |> List.head |> withDefault "" |> menuDecoder

        action =
            Dict.get "a" session.common.query |> withDefault [] |> List.head |> withDefault ""

        -- Focus
        newFocus =
            flags
                |> nameidFromFlags
                |> focusFromNameid

        -- What has changed
        fs =
            focusState SettingsBaseUri session.referer global.url session.common.node_focus newFocus

        model =
            { node_focus = newFocus
            , path_data =
                session.common.path_data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , menuFocus = menu
            , menuList = menuList
            , colorPicker = ColorPicker.init
            , artefact_form = initArtefactNodeForm session.common.user newFocus.nameid ColorPicker.initColor
            , hasUnsavedData = False

            -- Labels
            , labels = Loading
            , labels_top = RemoteData.Loading
            , labels_sub = RemoteData.Loading
            , label_add = ternary (action == "new" && menu == LabelsMenu) True False
            , label_edit = Nothing
            , label_result = NotAsked
            , label_result_del = NotAsked
            , label_anim_enter = Nothing

            -- Roles
            , nodeDoc = NodeDoc.init session.common.lexicon "" Nothing NodeDoc.NoView session.common.user
            , showMandate = ""
            , roles = Loading
            , roles_top = RemoteData.Loading
            , roles_sub = RemoteData.Loading
            , role_add = ternary (action == "new" && menu == RolesMenu) True False
            , role_edit = Nothing
            , role_result = NotAsked
            , role_result_del = NotAsked
            , role_anim_enter = Nothing

            -- Templates
            , template_form = initTensionTemplateForm session.common.user newFocus.nameid
            , templates = NotAsked
            , labelsPanel = LabelSearchPanel.init "" SelectLabel session.common.user
            , assigneesPanel = UserSearchPanel.init "" SelectUser session.common.user
            , templates_top = RemoteData.NotAsked
            , templates_sub = RemoteData.NotAsked
            , template_add = action == "new" && menu == TemplatesMenu
            , template_edit = Nothing
            , template_result = NotAsked
            , template_result_del = NotAsked
            , template_anim_enter = Nothing

            -- Orga
            , orga_rights = Loading
            , switch_result = RemoteData.NotAsked
            , switch_index = -1
            , lexicon = session.common.lexicon
            , lexicon_input = Dict.get "Tension" session.common.lexicon |> withDefault ""
            , mandate_input = Dict.get "Mandate" session.common.lexicon |> withDefault ""
            , lexicon_result = RemoteData.NotAsked

            -- Common
            , refresh_trial = 0
            , url = global.url
            , empty = {}
            , commonOp = CommonMsg NoMsg LogErr
            , helperBar = HelperBar.init SettingsBaseUri global.url.query newFocus session.common
            , help = Help.init session.common
            , tensionForm = NTF.init session.common
            , modal_confirm = ModalConfirm.init NoMsg
            , joinOrga = JoinOrga.init newFocus.nameid session.common
            , authModal = AuthModal.init (Dict.get "puid" session.common.query |> Maybe.map List.head |> withDefault Nothing) session.common
            , orgaMenu = OrgaMenu.init newFocus session.data.orga_menu session.data.orgs_data session.common
            , treeMenu = TreeMenu.init SettingsBaseUri global.url.query newFocus session.data.tree_menu session.data.tree_data session.common
            , actionPanel = ActionPanel.init session.common
            }

        cmds =
            [ ternary fs.focusChange (queryLocalGraph apis newFocus.nameid True (GotPath True)) Cmd.none
            , sendSleep PassedSlowLoadTreshold 500
            , Cmd.map OrgaMenuMsg (send OrgaMenu.OnLoad)
            , Cmd.map TreeMenuMsg (send TreeMenu.OnLoad)
            ]
                ++ (case menu of
                        LabelsMenu ->
                            [ getLabels apis newFocus.nameid GotLabels
                            , fetchLabelsTop apis newFocus.nameid False GotLabelsTop
                            , fetchLabelsSub apis newFocus.nameid False GotLabelsSub
                            ]

                        RolesMenu ->
                            [ getRoles apis newFocus.nameid GotRoles
                            , fetchRolesTop apis newFocus.nameid False GotRolesTop
                            , fetchRolesSub apis newFocus.nameid False GotRolesSub
                            ]

                        TemplatesMenu ->
                            [ getTensionTemplates apis newFocus.nameid GotTemplates
                            , fetchTensionTemplatesTop apis newFocus.nameid False GotTemplatesTop
                            , fetchTensionTemplatesSub apis newFocus.nameid False GotTemplatesSub
                            ]

                        GlobalMenu ->
                            [ getCircleRights apis (nid2rootid newFocus.nameid) GotRootRights ]

                        EditMenu ->
                            []
                   )
    in
    ( model
    , Cmd.batch cmds
    , if fs.refresh then
        send (UpdateSessionFocus (Just newFocus))

      else
        Cmd.none
    )



--
-- Msg
--


type Msg
    = -- Loading
      PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
    | GotPath Bool (GqlData LocalGraph)
      -- Page
    | ChangeMenuFocus MenuSettings
    | ChangeArtefactPost String String
    | SafeEdit Msg
    | SafeSend Msg
      -- Labels
    | GotLabels (GqlData (List LabelFull))
    | GotLabelsTop (RestData (List Label))
    | GotLabelsSub (RestData (List Label))
    | AddLabel
    | EditLabel LabelFull
    | CancelLabel
    | SubmitAddLabel Time.Posix
    | SubmitEditLabel Time.Posix
    | SubmitDeleteLabel String Time.Posix
    | GotLabel (GqlData LabelFull)
    | GotLabelDel (GqlData LabelFull)
      -- Roles
    | GotRoles (GqlData (List RoleExtFull))
    | GotRolesTop (RestData (List RoleExt))
    | GotRolesSub (RestData (List RoleExt))
    | AddRole
    | EditRole RoleExtFull
    | CancelRole
    | SubmitAddRole Time.Posix
    | SubmitEditRole Time.Posix
    | SubmitDeleteRole String Time.Posix
    | GotRole (GqlData RoleExtFull)
    | GotRoleDel (GqlData RoleExtFull)
    | ToggleMandate String
    | AddDomains
    | AddPolicies
    | AddResponsabilities
    | UpdateNodePost String String
    | ChangeMandateViewMode String InputViewMode
    | OnMandateRichText String String
    | OnToggleMandateMdHelp String
      -- Templates
    | GotTemplates (GqlData (List TensionTemplateFull))
    | AddTemplate
    | EditTemplate TensionTemplateFull
    | CancelTemplate
    | ChangeTemplatePost String String
    | ChangeTemplateDescription String
    | ChangeTemplateType TensionType.TensionType
    | ChangeTemplateRecursive Bool
    | ChangeTemplateViewMode InputViewMode
    | OnTemplateRichText String String
    | OnToggleTemplateMdHelp String
    | SubmitAddTemplate Time.Posix
    | SubmitEditTemplate Time.Posix
    | SubmitDeleteTemplate String Time.Posix
    | GotTemplate (GqlData TensionTemplateFull)
    | GotTemplateDel (GqlData TensionTemplateFull)
    | GotTemplatesTop (RestData (List TensionTemplateLite))
    | GotTemplatesSub (RestData (List TensionTemplateLite))
    | LabelSearchPanelMsg LabelSearchPanel.Msg
    | UserSearchPanelMsg UserSearchPanel.Msg
      -- Orga
    | GotRootRights (GqlData NodeRights)
    | SwitchUserCanJoin Int Bool
    | SwitchGuestCanCreateTension Int Bool
    | SwitchIsTemplateTensionOnly Int Bool
    | SwitchIsPinnedTensionfetchRecursively Int Bool
    | GotUserCanJoin (RestData Bool)
    | GotGuestCanCreateTension (RestData Bool)
    | GotIsTemplateTensionOnly (RestData Bool)
    | GotIsPinnedTensionfetchRecursively (RestData Bool)
    | OnLexiconInput String
    | OnMandateInput String
    | SubmitLexicon
    | GotLexicon (RestData Bool)
      -- Color Picker
    | OpenColor
    | CloseColor
    | SelectColor String
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

        ChangeMenuFocus menu ->
            case menu of
                EditMenu ->
                    case model.path_data of
                        Success lg ->
                            ( model, Cmd.none, send (NavigateRaw (toHref (Route.Tension_Dynamic_Dynamic_Action { param1 = nid2rootid lg.focus.nameid, param2 = getSourceTid lg.focus }))) )

                        _ ->
                            ( model, Cmd.none, Cmd.none )

                _ ->
                    let
                        query =
                            queryBuilder
                                [ ( "m", menuEncoder menu ) ]
                    in
                    ( model, Cmd.none, Nav.pushUrl global.key (toLink SettingsBaseUri model.node_focus.nameid [] ++ "?" ++ query) )

        ChangeArtefactPost field value ->
            let
                f =
                    model.artefact_form

                newForm =
                    { f | post = Dict.insert field value f.post }
            in
            ( { model | artefact_form = newForm, hasUnsavedData = True }, Cmd.none, Cmd.none )

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

        --
        -- Labels
        --
        GotLabels result ->
            let
                newModel =
                    { model | labels = result }
            in
            ( newModel, Cmd.none, Cmd.none )

        GotLabelsTop result ->
            let
                newModel =
                    { model | labels_top = result }
            in
            ( newModel, Cmd.none, Cmd.none )

        GotLabelsSub result ->
            let
                newModel =
                    { model | labels_sub = result }
            in
            ( newModel, Cmd.none, Cmd.none )

        AddLabel ->
            if model.label_add then
                ( model, Cmd.none, Cmd.none )

            else
                -- Toggle Add Label Box
                ( { model
                    | label_add = ternary model.label_add False True
                    , label_edit = Nothing
                    , label_anim_enter = Nothing
                    , colorPicker = ColorPicker.setColor Nothing model.colorPicker
                  }
                , Ports.bulma_driver "labelsTable"
                , Cmd.none
                )

        EditLabel label ->
            let
                f =
                    model.artefact_form

                newForm =
                    { f
                        | id = label.id
                        , post =
                            Dict.fromList
                                ([ ( "name", label.name ) ]
                                    ++ (label.color |> Maybe.map (\x -> [ ( "color", x ) ]) |> withDefault [])
                                    ++ (label.description |> Maybe.map (\x -> [ ( "description", x ) ]) |> withDefault [])
                                    ++ [ ( "old_name", label.name ) ]
                                )
                    }
            in
            ( { model
                | label_add = False
                , label_edit = Just label
                , artefact_form = newForm
                , colorPicker = ColorPicker.setColor (Dict.get "color" newForm.post) model.colorPicker
              }
            , Ports.bulma_driver "labelsTable"
            , Cmd.none
            )

        CancelLabel ->
            ( { model
                | label_add = False
                , label_edit = Nothing
                , label_result = NotAsked
                , label_result_del = NotAsked
                , label_anim_enter = Nothing
              }
                |> resetForm
            , Cmd.none
            , Cmd.none
            )

        SubmitAddLabel _ ->
            ( { model | label_result = LoadingSlowly }, addOneLabel apis model.artefact_form GotLabel, Cmd.none )

        SubmitEditLabel _ ->
            ( { model | label_result = LoadingSlowly }, updateOneLabel apis model.artefact_form GotLabel, Cmd.none )

        SubmitDeleteLabel id _ ->
            let
                f =
                    model.artefact_form

                newForm =
                    { f | id = id }
            in
            ( { model | label_result_del = LoadingSlowly, artefact_form = newForm }, removeOneLabel apis newForm GotLabelDel, Cmd.none )

        GotLabel result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | label_result = NotAsked }, Ports.raiseAuthModal model.artefact_form.uctx, Cmd.none )

                RefreshToken i ->
                    if model.label_add then
                        ( { model | refresh_trial = i }, sendSleep (Submit SubmitAddLabel) 500, send UpdateUserToken )

                    else
                        -- assume edit
                        ( { model | refresh_trial = i }, sendSleep (Submit SubmitEditLabel) 500, send UpdateUserToken )

                OkAuth label ->
                    let
                        d =
                            withDefaultData [] model.labels

                        new =
                            if model.label_add then
                                [ label ] ++ d

                            else
                                -- assume edit
                                LE.setIf (\x -> x.id == label.id) label d
                    in
                    ( { model
                        | label_result = result
                        , labels = Success new
                        , label_add = False
                        , label_edit = Nothing
                        , label_anim_enter = Just label.id
                      }
                        |> resetForm
                    , Cmd.none
                    , Cmd.none
                    )

                DuplicateErr ->
                    let
                        label_name =
                            Dict.get "name" model.artefact_form.post |> withDefault "" |> String.toLower

                        here name =
                            (withDefaultData [] model.labels |> List.filter (\x -> x.name == name) |> List.length)
                                > 0

                        form =
                            model.artefact_form
                    in
                    if model.label_add && not (here label_name) then
                        -- set the labels in the node labels list
                        ( { model | label_result = LoadingSlowly, artefact_form = { form | id = "" } }, send (Submit SubmitEditLabel), Cmd.none )

                    else
                        -- throw error if the labels is in the list of labels
                        ( { model | label_result = result }, Cmd.none, Cmd.none )

                NoAuth ->
                    ( { model | label_result = Failure [ T.noAuthErr ] }, Cmd.none, Cmd.none )

                _ ->
                    ( { model | label_result = result }, Cmd.none, Cmd.none )

        GotLabelDel result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | label_result_del = NotAsked }, Ports.raiseAuthModal model.artefact_form.uctx, Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep (Submit <| SubmitDeleteLabel model.artefact_form.id) 500, send UpdateUserToken )

                OkAuth _ ->
                    let
                        d =
                            withDefaultData [] model.labels

                        new =
                            List.filter (\x -> x.id /= model.artefact_form.id) d
                    in
                    ( { model | label_result_del = NotAsked, labels = Success new, label_add = False, label_edit = Nothing } |> resetForm
                    , Cmd.none
                    , Cmd.none
                    )

                _ ->
                    ( { model | label_result_del = result }, Cmd.none, Cmd.none )

        --
        -- Roles
        --
        GotRoles result ->
            let
                newModel =
                    { model | roles = result }
            in
            ( newModel, Cmd.none, Cmd.none )

        GotRolesTop result ->
            let
                newModel =
                    { model | roles_top = result }
            in
            ( newModel, Cmd.none, Cmd.none )

        GotRolesSub result ->
            let
                newModel =
                    { model | roles_sub = result }
            in
            ( newModel, Cmd.none, Cmd.none )

        AddRole ->
            if model.role_add then
                ( model, Cmd.none, Cmd.none )

            else
                -- Toggle Add Role Box
                ( { model
                    | role_add = ternary model.role_add False True
                    , role_edit = Nothing
                    , role_anim_enter = Nothing
                    , colorPicker = ColorPicker.setColor Nothing model.colorPicker
                  }
                , Ports.bulma_driver "rolesTable"
                , Cmd.none
                )

        EditRole role ->
            let
                f =
                    model.artefact_form

                newForm =
                    { f
                        | id = role.id
                        , post =
                            Dict.fromList
                                ([ ( "name", role.name ) ]
                                    ++ (role.color |> Maybe.map (\x -> [ ( "color", x ) ]) |> withDefault [])
                                    ++ (role.about |> Maybe.map (\x -> [ ( "about", x ) ]) |> withDefault [])
                                    ++ [ ( "old_name", role.name ) ]
                                )
                        , mandate = withDefault initMandate role.mandate
                        , role_type = role.role_type
                    }
            in
            ( { model
                | role_add = False
                , role_edit = Just role
                , artefact_form = newForm
                , colorPicker = ColorPicker.setColor (Dict.get "color" newForm.post) model.colorPicker
              }
            , Ports.bulma_driver "rolesTable"
            , Cmd.none
            )

        CancelRole ->
            ( { model
                | role_add = False
                , role_edit = Nothing
                , role_result = NotAsked
                , role_result_del = NotAsked
                , role_anim_enter = Nothing
                , nodeDoc = NodeDoc.init global.session.common.lexicon "" Nothing NodeDoc.NoView global.session.common.user
              }
                |> resetForm
            , Cmd.none
            , Cmd.none
            )

        SubmitAddRole _ ->
            ( { model | role_result = LoadingSlowly }, addOneRole apis model.artefact_form GotRole, Cmd.none )

        SubmitEditRole _ ->
            ( { model | role_result = LoadingSlowly }, updateOneRole apis model.artefact_form GotRole, Cmd.none )

        SubmitDeleteRole id _ ->
            let
                f =
                    model.artefact_form

                newForm =
                    { f | id = id }
            in
            ( { model | role_result_del = LoadingSlowly, artefact_form = newForm }, removeOneRole apis newForm GotRoleDel, Cmd.none )

        ToggleMandate rid ->
            if rid == "" || rid /= model.showMandate then
                ( { model | showMandate = rid }, Cmd.none, Cmd.none )

            else
                ( { model | showMandate = "" }, Cmd.none, Cmd.none )

        GotRole result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | role_result = NotAsked }, Ports.raiseAuthModal model.artefact_form.uctx, Cmd.none )

                RefreshToken i ->
                    if model.role_add then
                        ( { model | refresh_trial = i }, sendSleep (Submit SubmitAddRole) 500, send UpdateUserToken )

                    else
                        -- assume edit
                        ( { model | refresh_trial = i }, sendSleep (Submit SubmitEditRole) 500, send UpdateUserToken )

                OkAuth role ->
                    let
                        d =
                            withDefaultData [] model.roles

                        new =
                            if model.role_add then
                                [ role ] ++ d

                            else
                                -- assume edit
                                LE.setIf (\x -> x.id == role.id) role d
                    in
                    ( { model
                        | role_result = result
                        , roles = Success new
                        , role_add = False
                        , role_edit = Nothing
                        , role_anim_enter = Just role.id
                      }
                        |> resetForm
                    , Cmd.none
                    , Cmd.none
                    )

                NoAuth ->
                    ( { model | role_result = Failure [ T.noAuthErr ] }, Cmd.none, Cmd.none )

                DuplicateErr ->
                    let
                        role_name =
                            Dict.get "name" model.artefact_form.post |> withDefault "" |> String.toLower

                        here name =
                            (withDefaultData [] model.roles |> List.filter (\x -> x.name == name) |> List.length)
                                > 0

                        form =
                            model.artefact_form
                    in
                    if model.role_add && not (here role_name) then
                        -- set the roles in the node roles list
                        ( { model | role_result = LoadingSlowly, artefact_form = { form | id = "" } }, send (Submit SubmitEditRole), Cmd.none )

                    else
                        -- throw error if the roles is in the list of roles
                        ( { model | role_result = result }, Cmd.none, Cmd.none )

                _ ->
                    ( { model | role_result = result }, Cmd.none, Cmd.none )

        GotRoleDel result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | role_result_del = NotAsked }, Ports.raiseAuthModal model.artefact_form.uctx, Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep (Submit <| SubmitDeleteRole model.artefact_form.id) 500, send UpdateUserToken )

                OkAuth _ ->
                    let
                        d =
                            withDefaultData [] model.roles

                        new =
                            List.filter (\x -> x.id /= model.artefact_form.id) d
                    in
                    ( { model | role_result_del = NotAsked, roles = Success new, role_add = False, role_edit = Nothing } |> resetForm
                    , Cmd.none
                    , Cmd.none
                    )

                _ ->
                    ( { model | role_result_del = result }, Cmd.none, Cmd.none )

        AddResponsabilities ->
            ( { model | nodeDoc = NodeDoc.addResponsabilities model.nodeDoc }, Cmd.none, Cmd.none )

        AddDomains ->
            ( { model | nodeDoc = NodeDoc.addDomains model.nodeDoc }, Cmd.none, Cmd.none )

        AddPolicies ->
            ( { model | nodeDoc = NodeDoc.addPolicies model.nodeDoc }, Cmd.none, Cmd.none )

        UpdateNodePost field value ->
            let
                form =
                    model.artefact_form

                newNodeDoc =
                    NodeDoc.updatePost field value model.nodeDoc
            in
            ( { model
                | artefact_form = { form | mandate = NodeDoc.getMandate newNodeDoc, role_type = NodeDoc.getRoleType newNodeDoc |> withDefault form.role_type }
                , nodeDoc = newNodeDoc
                , hasUnsavedData = True
              }
            , Cmd.none
            , Cmd.none
            )

        ChangeMandateViewMode targetid viewMode ->
            let
                f =
                    model.artefact_form

                val =
                    case viewMode of
                        Write ->
                            "Write"

                        Preview ->
                            "Preview"
            in
            ( { model | artefact_form = { f | post = Dict.insert ("viewMode:" ++ targetid) val f.post } }, Cmd.none, Cmd.none )

        OnMandateRichText targetid command ->
            ( model, Ports.richText targetid command, Cmd.none )

        OnToggleMandateMdHelp targetid ->
            let
                f =
                    model.artefact_form

                field =
                    "isMdHelpOpen" ++ targetid

                v =
                    Dict.get field f.post |> withDefault "false"

                val =
                    ternary (v == "true") "false" "true"
            in
            ( { model | artefact_form = { f | post = Dict.insert field val f.post } }, Cmd.none, Cmd.none )

        -- Templates
        GotTemplates result ->
            ( { model | templates = result }, Cmd.none, Cmd.none )

        AddTemplate ->
            if model.template_add then
                ( model, Cmd.none, Cmd.none )

            else
                ( { model
                    | template_add = True
                    , template_edit = Nothing
                    , template_anim_enter = Nothing
                  }
                , Ports.bulma_driver "templatesTable"
                , Cmd.none
                )

        EditTemplate tpl ->
            let
                f =
                    model.template_form

                tplLabels =
                    withDefault [] tpl.labels

                tplAssignees =
                    withDefault [] tpl.assignees

                newForm =
                    { f
                        | id = tpl.id
                        , post =
                            Dict.fromList
                                [ ( "name", tpl.name )
                                , ( "title", tpl.title )
                                , ( "comment", tpl.comment )
                                , ( "old_name", tpl.name )
                                ]
                        , description = tpl.description
                        , type_ = tpl.type_
                        , is_recursive = tpl.is_recursive
                        , labels = tplLabels
                        , assignees = tplAssignees
                        , orig_labels = tplLabels
                        , orig_assignees = tplAssignees
                    }
            in
            ( { model
                | template_add = False
                , template_edit = Just tpl
                , template_form = newForm
              }
            , Ports.bulma_driver "templatesTable"
            , Cmd.none
            )

        CancelTemplate ->
            ( { model
                | template_add = False
                , template_edit = Nothing
                , template_result = NotAsked
                , template_result_del = NotAsked
                , template_anim_enter = Nothing
              }
                |> resetForm
            , Cmd.none
            , Cmd.none
            )

        ChangeTemplatePost field value ->
            let
                f =
                    model.template_form
            in
            ( { model | template_form = { f | post = Dict.insert field value f.post } }, Cmd.none, Cmd.none )

        ChangeTemplateDescription value ->
            let
                f =
                    model.template_form
            in
            ( { model | template_form = { f | description = ternary (value == "") Nothing (Just value) } }, Cmd.none, Cmd.none )

        ChangeTemplateType type_ ->
            let
                f =
                    model.template_form
            in
            ( { model | template_form = { f | type_ = type_ } }, Cmd.none, Cmd.none )

        ChangeTemplateRecursive val ->
            let
                f =
                    model.template_form
            in
            ( { model | template_form = { f | is_recursive = val } }, Cmd.none, Cmd.none )

        ChangeTemplateViewMode viewMode ->
            let
                f =
                    model.template_form
            in
            ( { model | template_form = { f | viewMode = viewMode } }, Cmd.none, Cmd.none )

        OnTemplateRichText targetid command ->
            ( model, Ports.richText targetid command, Cmd.none )

        OnToggleTemplateMdHelp targetid ->
            let
                f =
                    model.template_form

                field =
                    "isMdHelpOpen" ++ targetid

                v =
                    Dict.get field f.post |> withDefault "false"

                val =
                    ternary (v == "true") "false" "true"
            in
            ( { model | template_form = { f | post = Dict.insert field val f.post } }, Cmd.none, Cmd.none )

        SubmitAddTemplate _ ->
            ( { model | template_result = LoadingSlowly }, addOneTensionTemplate apis model.template_form GotTemplate, Cmd.none )

        SubmitEditTemplate _ ->
            ( { model | template_result = LoadingSlowly }, updateOneTensionTemplate apis model.template_form GotTemplate, Cmd.none )

        SubmitDeleteTemplate id_ _ ->
            let
                f =
                    model.template_form

                newForm =
                    { f | id = id_ }
            in
            ( { model | template_result_del = LoadingSlowly, template_form = newForm }, removeOneTensionTemplate apis newForm GotTemplateDel, Cmd.none )

        GotTemplate result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | template_result = NotAsked }, Ports.raiseAuthModal model.template_form.uctx, Cmd.none )

                RefreshToken i ->
                    if model.template_add then
                        ( { model | refresh_trial = i }, sendSleep (Submit SubmitAddTemplate) 500, send UpdateUserToken )

                    else
                        ( { model | refresh_trial = i }, sendSleep (Submit SubmitEditTemplate) 500, send UpdateUserToken )

                OkAuth tpl ->
                    let
                        d =
                            withDefaultData [] model.templates

                        new =
                            if model.template_add then
                                [ tpl ] ++ d

                            else
                                LE.setIf (\x -> x.id == tpl.id) tpl d
                    in
                    ( { model
                        | template_result = result
                        , templates = Success new
                        , template_add = False
                        , template_edit = Nothing
                        , template_anim_enter = Just tpl.id
                      }
                        |> resetForm
                    , Cmd.none
                    , send ResetSessionTemplates
                    )

                DuplicateErr ->
                    ( { model | template_result = Failure [ T.duplicateNameError ] }, Cmd.none, Cmd.none )

                _ ->
                    ( { model | template_result = result }, Cmd.none, Cmd.none )

        GotTemplateDel result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | template_result_del = NotAsked }, Ports.raiseAuthModal model.template_form.uctx, Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep (Submit <| SubmitDeleteTemplate model.template_form.id) 500, send UpdateUserToken )

                OkAuth _ ->
                    let
                        d =
                            withDefaultData [] model.templates

                        new =
                            List.filter (\x -> x.id /= model.template_form.id) d
                    in
                    ( { model | template_result_del = NotAsked, templates = Success new, template_add = False, template_edit = Nothing } |> resetForm
                    , Cmd.none
                    , send ResetSessionTemplates
                    )

                _ ->
                    ( { model | template_result_del = result }, Cmd.none, Cmd.none )

        GotTemplatesTop result ->
            ( { model | templates_top = result }, Cmd.none, Cmd.none )

        GotTemplatesSub result ->
            ( { model | templates_sub = result }, Cmd.none, Cmd.none )

        LabelSearchPanelMsg msg ->
            let
                ( panel, out ) =
                    LabelSearchPanel.update apis msg model.labelsPanel

                f =
                    model.template_form

                newForm =
                    Maybe.map
                        (\r ->
                            if Tuple.first r then
                                { f | labels = f.labels ++ [ Tuple.second r ] }

                            else
                                { f | labels = List.filter (\l -> l.name /= (Tuple.second r).name) f.labels }
                        )
                        out.result
                        |> withDefault f

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | labelsPanel = panel, template_form = newForm }
            , Cmd.batch (out.cmds |> List.map (\m -> Cmd.map LabelSearchPanelMsg m) |> List.append cmds)
            , Cmd.batch gcmds
            )

        UserSearchPanelMsg msg ->
            let
                ( panel, out ) =
                    UserSearchPanel.update apis msg model.assigneesPanel

                f =
                    model.template_form

                newForm =
                    Maybe.map
                        (\r ->
                            if Tuple.first r then
                                { f | assignees = f.assignees ++ [ Tuple.second r ] }

                            else
                                { f | assignees = List.filter (\u -> u.username /= (Tuple.second r).username) f.assignees }
                        )
                        out.result
                        |> withDefault f

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | assigneesPanel = panel, template_form = newForm }
            , Cmd.batch (out.cmds |> List.map (\m -> Cmd.map UserSearchPanelMsg m) |> List.append cmds)
            , Cmd.batch gcmds
            )

        -- Orga
        GotRootRights result ->
            ( { model | orga_rights = result }, Cmd.none, Cmd.none )

        SwitchUserCanJoin i confirmed ->
            let
                val =
                    withMaybeData model.orga_rights |> unwrap2 False .userCanJoin

                isPublic =
                    withMaybeData model.orga_rights |> Maybe.map .visibility |> Maybe.map (\x -> x == NodeVisibility.Public) |> withDefault False
            in
            if not val && not isPublic && not confirmed then
                -- show modal to confirm root circle is going to be public
                ( model
                , send <|
                    DoModalConfirmOpen (SwitchUserCanJoin i True)
                        { message = Just ( "Please confirm the change", "" )
                        , txts = [ ( "Enabling this setting will make the visibility of the root circle ", "" ), ( "Public", "is-strong" ), ( ".", "" ) ]
                        , confirmClass = "is-success"
                        , confirmLabel = T.confirm
                        }
                , Cmd.none
                )

            else
                ( { model | switch_result = RemoteData.Loading, switch_index = i }, setUserCanJoin apis (nid2rootid model.node_focus.nameid) (not val) GotUserCanJoin, Cmd.none )

        GotUserCanJoin result ->
            let
                data =
                    { model | switch_result = result }
            in
            case result of
                RemoteData.Success v ->
                    ( { data | switch_index = -1, orga_rights = withMapData (\x -> { x | userCanJoin = Just v }) model.orga_rights }
                    , Cmd.none
                    , Cmd.none
                    )

                _ ->
                    ( data, Cmd.none, Cmd.none )

        SwitchGuestCanCreateTension i _ ->
            let
                val =
                    withMaybeData model.orga_rights |> unwrap2 False .guestCanCreateTension
            in
            ( { model | switch_result = RemoteData.Loading, switch_index = i }, setGuestCanCreateTension apis (nid2rootid model.node_focus.nameid) (not val) GotGuestCanCreateTension, Cmd.none )

        GotGuestCanCreateTension result ->
            let
                data =
                    { model | switch_result = result }
            in
            case result of
                RemoteData.Success v ->
                    ( { data | switch_index = -1, orga_rights = withMapData (\x -> { x | guestCanCreateTension = Just v }) model.orga_rights }
                    , Cmd.none
                    , Cmd.none
                    )

                _ ->
                    ( data, Cmd.none, Cmd.none )

        SwitchIsTemplateTensionOnly i _ ->
            let
                val =
                    withMaybeData model.orga_rights |> unwrap2 False .isTemplateTensionOnly
            in
            ( { model | switch_result = RemoteData.Loading, switch_index = i }, setIsTemplateTensionOnly apis (nid2rootid model.node_focus.nameid) (not val) GotIsTemplateTensionOnly, Cmd.none )

        GotIsTemplateTensionOnly result ->
            let
                data =
                    { model | switch_result = result }
            in
            case result of
                RemoteData.Success v ->
                    ( { data | switch_index = -1, orga_rights = withMapData (\x -> { x | isTemplateTensionOnly = Just v }) model.orga_rights }
                    , Cmd.none
                    , Cmd.none
                    )

                _ ->
                    ( data, Cmd.none, Cmd.none )

        SwitchIsPinnedTensionfetchRecursively i _ ->
            let
                val =
                    withMaybeData model.orga_rights |> unwrap2 False .isPinnedTensionfetchRecursively
            in
            ( { model | switch_result = RemoteData.Loading, switch_index = i }, setIsPinnedTensionfetchRecursively apis (nid2rootid model.node_focus.nameid) (not val) GotIsPinnedTensionfetchRecursively, Cmd.none )

        GotIsPinnedTensionfetchRecursively result ->
            let
                data =
                    { model | switch_result = result }
            in
            case result of
                RemoteData.Success v ->
                    let
                        newPath =
                            withMapData
                                (\p ->
                                    { p
                                        | root = Maybe.map (\r -> { r | isPinnedTensionfetchRecursively = Just v }) p.root
                                    }
                                )
                                model.path_data
                    in
                    ( { data
                        | switch_index = -1
                        , orga_rights = withMapData (\x -> { x | isPinnedTensionfetchRecursively = Just v }) model.orga_rights
                        , path_data = newPath
                      }
                    , Cmd.none
                    , send (UpdateSessionPath (withMaybeData newPath))
                    )

                _ ->
                    ( data, Cmd.none, Cmd.none )

        OnLexiconInput val ->
            ( { model | lexicon_input = val }, Cmd.none, Cmd.none )

        OnMandateInput val ->
            ( { model | mandate_input = val }, Cmd.none, Cmd.none )

        SubmitLexicon ->
            let
                tensionVal =
                    String.trim model.lexicon_input

                mandateVal =
                    String.trim model.mandate_input

                dict =
                    (if tensionVal /= "" then
                        [ ( "Tension", upH tensionVal ), ( "tension", String.toLower tensionVal ) ]

                     else
                        []
                    )
                        ++ (if mandateVal /= "" then
                                [ ( "Mandate", upH mandateVal ), ( "mandate", String.toLower mandateVal ) ]

                            else
                                []
                           )
                        |> Dict.fromList

                jsonStr =
                    JE.encode 0 (JE.dict identity JE.string dict)
            in
            ( { model | lexicon_result = RemoteData.Loading }
            , setLexicon apis (nid2rootid model.node_focus.nameid) jsonStr GotLexicon
            , Cmd.none
            )

        GotLexicon result ->
            case result of
                RemoteData.Success _ ->
                    let
                        tensionVal =
                            String.trim model.lexicon_input

                        mandateVal =
                            String.trim model.mandate_input

                        newLexicon =
                            (if tensionVal /= "" then
                                [ ( "Tension", upH tensionVal ), ( "tension", String.toLower tensionVal ) ]

                             else
                                []
                            )
                                ++ (if mandateVal /= "" then
                                        [ ( "Mandate", upH mandateVal ), ( "mandate", String.toLower mandateVal ) ]

                                    else
                                        []
                                   )
                                |> Dict.fromList
                    in
                    ( { model | lexicon_result = result }
                    , Ports.saveLexicon newLexicon
                    , send (UpdateSessionLexicon newLexicon)
                    )

                _ ->
                    ( { model | lexicon_result = result }, Cmd.none, Cmd.none )

        -- Color Picker
        OpenColor ->
            ( { model | colorPicker = ColorPicker.open model.colorPicker }
            , if not model.colorPicker.isOpen then
                Cmd.batch [ Ports.outsideClickClose "cancelColorFromJs" "colorPicker" ]

              else
                Cmd.none
            , Cmd.none
            )

        CloseColor ->
            ( { model | colorPicker = ColorPicker.close model.colorPicker }, Cmd.none, Cmd.none )

        SelectColor color ->
            let
                newPicker =
                    model.colorPicker
                        |> ColorPicker.setColor (Just color)
                        |> ColorPicker.close

                form =
                    model.artefact_form

                newForm =
                    { form | post = Dict.insert "color" color form.post }
            in
            ( { model | colorPicker = newPicker, artefact_form = newForm }, Cmd.none, Ports.click "body" )

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
            ( model, Cmd.none, send (NavigateRaw (toLink SettingsBaseUri model.node_focus.rootnameid [] ++ query)) )

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


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ model =
    [ Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    , Ports.cancelColorFromJs (always CloseColor)
    ]
        ++ (HelperBar.subscriptions |> List.map (\s -> Sub.map HelperBarMsg s))
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        ++ (NTF.subscriptions model.tensionForm |> List.map (\s -> Sub.map NewTensionMsg s))
        ++ (JoinOrga.subscriptions model.joinOrga |> List.map (\s -> Sub.map JoinOrgaMsg s))
        ++ (AuthModal.subscriptions |> List.map (\s -> Sub.map AuthModalMsg s))
        ++ (OrgaMenu.subscriptions |> List.map (\s -> Sub.map OrgaMenuMsg s))
        ++ (TreeMenu.subscriptions |> List.map (\s -> Sub.map TreeMenuMsg s))
        ++ (ActionPanel.subscriptions model.actionPanel |> List.map (\s -> Sub.map ActionPanelMsg s))
        ++ (LabelSearchPanel.subscriptions model.labelsPanel |> List.map (\s -> Sub.map LabelSearchPanelMsg s))
        ++ (UserSearchPanel.subscriptions model.assigneesPanel |> List.map (\s -> Sub.map UserSearchPanelMsg s))
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

        org_id =
            String.join "/" <| LE.unique [ model.node_focus.rootnameid, model.node_focus.nameid |> String.split "#" |> LE.last |> withDefault "" ]
    in
    { title =
        case model.path_data of
            Success path ->
                unwrap org_id .name path.root ++ " · " ++ T.settings

            _ ->
                org_id ++ " · " ++ T.settings
    , body =
        [ div [ class "orgPane" ]
            [ HelperBar.view helperData model.helperBar |> Html.map HelperBarMsg
            , div [ id "mainPane" ] [ view_ model ]
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


view_ : Model -> Html Msg
view_ model =
    div [ class "columns is-centered" ]
        [ div [ class "column is-12 is-11-desktop is-9-fullhd" ]
            [ div [ class "columns" ]
                [ div [ class "column is-one-fifth" ] [ viewSettingsMenu model ]
                , div [ class "column" ] [ viewSettingsContent model ]
                ]
            ]
        ]


viewSettingsMenu : Model -> Html Msg
viewSettingsMenu model =
    nav [ id "menuSettings", class "menu" ]
        [ ul [ class "menu-list" ] <|
            (model.menuList
                |> List.concatMap
                    (\x ->
                        [ case x of
                            GlobalMenu ->
                                hr [ class "dropdown-divider" ] []

                            _ ->
                                text ""
                        , li []
                            [ a [ onClickPD (ChangeMenuFocus x), target "_blank", classList [ ( "is-active", x == model.menuFocus ) ] ]
                                [ A.icon1 (menuToIcon x) (menuToString x) ]
                            ]
                        ]
                    )
            )
        ]


viewSettingsContent : Model -> Html Msg
viewSettingsContent model =
    case model.menuFocus of
        LabelsMenu ->
            div []
                [ --@todo lazy loading...
                  viewLabels model
                , viewLabelsExt model.url T.labelsTop model.labels_top
                , viewLabelsExt model.url T.labelsSub model.labels_sub
                ]

        RolesMenu ->
            div []
                [ --@todo lazy loading...
                  viewRoles model
                , viewRolesExt model.commonOp model.url T.rolesTop model.roles_top
                , viewRolesExt model.commonOp model.url T.rolesSub model.roles_sub
                ]

        TemplatesMenu ->
            div []
                [ viewTemplates model
                , viewTensionTemplatesExt model.url T.inheritedTemplates model.templates_top
                , viewTensionTemplatesExt model.url T.subTemplates model.templates_sub
                ]

        GlobalMenu ->
            div []
                [ h2 [ class "subtitle is-size-3" ] [ text T.organisationSettings ]
                , viewOrgaSettings model.lexicon model.orga_rights model.switch_result model.switch_index
                , hr [] []
                , viewLexiconSettings model.lexicon_input model.mandate_input model.lexicon_result
                ]

        EditMenu ->
            -- redirection
            div [] [ text "" ]



{-
   LABEL VIEW
-}


viewLabelAddBox : Model -> Html Msg
viewLabelAddBox model =
    let
        isAdd =
            model.label_add

        form =
            model.artefact_form

        result =
            model.label_result

        name =
            Dict.get "name" form.post |> withDefault ""

        color =
            Dict.get "color" form.post

        description =
            Dict.get "description" form.post

        isLoading =
            result == LoadingSlowly

        isSendable =
            name /= ""

        txt =
            if isAdd then
                { submit = T.createLabel }

            else
                -- assume edit label
                { submit = T.updateLabel }

        doSubmit =
            if isAdd then
                ternary isSendable [ onClick (Submit SubmitAddLabel) ] []

            else
                -- assume edit label
                ternary isSendable [ onClick (Submit SubmitEditLabel) ] []

        doCancel =
            CancelLabel
    in
    div [ class "box" ]
        [ div [ class "field is-grouped is-grouped-multiline" ]
            [ p [ class "control" ]
                [ label [ class "label is-small" ] [ text (T.labelName ++ " *") ]
                , input
                    [ class "input autofocus"
                    , type_ "text"
                    , placeholder T.name
                    , value name
                    , onInput (ChangeArtefactPost "name")
                    , autofocus True
                    ]
                    []
                ]
            , p [ class "control" ]
                [ label [ class "label is-small" ] [ text T.color ]
                , ColorPicker.view { data = model.colorPicker, onOpen = OpenColor, onClose = CloseColor, onSelect = SelectColor }
                ]
            , p [ class "control is-expanded" ]
                [ label [ class "label is-small" ] [ text T.description ]
                , input
                    [ class "input"
                    , type_ "text"
                    , placeholder T.description
                    , value (withDefault "" description)
                    , onInput (ChangeArtefactPost "description")
                    ]
                    []
                ]
            , p [ class "control buttons", attribute "style" "margin-top: 1.5rem;" ]
                [ button
                    ([ class "button is-success is-small"
                     , classList [ ( "is-loading", isLoading ) ]
                     , disabled (not isSendable)
                     ]
                        ++ doSubmit
                    )
                    [ text txt.submit ]
                , button [ class "button is-small", onClick doCancel ] [ text T.cancel ]
                ]
            ]
        , div []
            [ span [ class "help-label" ] [ text T.preview, text ": " ]
            , viewLabel "" Nothing (Label "" (ternary (name == "") "label name" name) color [])
            ]
        , case result of
            Failure err ->
                viewGqlErrors err

            _ ->
                text ""
        ]


viewLabels : Model -> Html Msg
viewLabels model =
    let
        goToParent =
            showIf (model.node_focus.nameid /= model.node_focus.rootnameid)
                (viewGoRoot "" OnGoRoot)
    in
    div [ id "labelsTable" ]
        [ h2 [ class "subtitle is-size-3" ] [ text T.labels, goToParent ]
        , div [ class "level" ]
            [ div [ class "mr-4" ] [ showMsg "labels-help" "mb-4 is-info" "icon-info" (T.labelsInfoHeader model.lexicon) (T.labelsInfoDoc model.lexicon) ]
            , div [ class "level-right is-align-self-flex-start", classList [ ( "is-hidden", model.label_add ) ] ] [ button [ class "button is-success", onClick (SafeEdit AddLabel) ] [ textT T.newLabel ] ]
            ]
        , if model.label_add then
            viewLabelAddBox model

          else
            text ""
        , case model.labels of
            Success labels ->
                if List.length labels == 0 then
                    div [ class "" ] [ text T.noLabels, text "." ]

                else
                    div [ class "table-container" ]
                        [ table [ class "table is-fullwidth" ]
                            [ thead [ class "is-size-6" ]
                                [ tr []
                                    [ th [] [ text T.name ]
                                    , th [] [ text T.description ]
                                    , th [] []
                                    , th [] []
                                    ]
                                ]
                            , labels
                                |> List.concatMap
                                    (\d ->
                                        [ tr [ classList [ ( "settings-row-enter", model.label_anim_enter == Just d.id ) ] ] <|
                                            if model.label_edit == Just d then
                                                [ td [ colspan 4 ] [ viewLabelAddBox model ] ]

                                            else
                                                let
                                                    n_nodes =
                                                        withDefault 0 d.n_nodes
                                                in
                                                [ td [ onClick (SafeEdit <| EditLabel d) ] [ viewLabel "button-light" Nothing (Label d.id d.name d.color []) ]
                                                , td [ class "is-aligned-left" ] [ d.description |> withDefault "" |> text |> List.singleton |> span [] ]
                                                , td [ attribute "style" "min-width: 9.4rem;" ]
                                                    [ if n_nodes > 1 then
                                                        span [ class "is-italic is-size-7" ] [ A.icon1 "icon-alert-circle icon-sm" "Defined in ", n_nodes |> String.fromInt |> text, text " circles" ]

                                                      else
                                                        text ""
                                                    ]
                                                , td [ class "is-aligned-right is-size-7", attribute "style" "min-width: 6.4rem;" ]
                                                    [ span [ class "button-light", onClick (SafeEdit <| EditLabel d) ] [ text T.edit ]
                                                    , text " · "
                                                    , span
                                                        [ class "button-light"
                                                        , onClick <|
                                                            DoModalConfirmOpen (Submit <| SubmitDeleteLabel d.id)
                                                                { message = Just ( T.labelDeleteInfoHeader model.lexicon, "" )
                                                                , txts = [ ( T.confirmDeleteLabel, "" ), ( d.name, "is-strong" ), ( "?", "" ) ]
                                                                , confirmClass = "is-danger"
                                                                , confirmLabel = T.delete
                                                                }
                                                        ]
                                                        [ text T.remove ]
                                                    ]
                                                ]
                                        ]
                                            ++ (case model.label_result_del of
                                                    Failure err ->
                                                        [ ternary (model.artefact_form.id == d.id)
                                                            (td [] [ viewGqlErrors err ])
                                                            (text "")
                                                        ]

                                                    _ ->
                                                        []
                                               )
                                    )
                                |> tbody []
                            ]
                        ]

            Failure err ->
                viewGqlErrors err

            LoadingSlowly ->
                div [ class "spinner" ] []

            _ ->
                text ""
        ]


viewLabelsExt : Url -> String -> RestData (List Label) -> Html Msg
viewLabelsExt url txt_yes list_ext_d =
    case list_ext_d of
        RemoteData.Success data ->
            if List.length data == 0 then
                text ""

            else
                let
                    q =
                        url.query |> Maybe.map (\uq -> "?" ++ uq) |> Maybe.withDefault ""
                in
                div [ class "mt-6" ]
                    [ h2 [ class "subtitle is-size-6 has-text-weight-semibold" ] [ text (txt_yes ++ " ") ]
                    , data
                        |> List.map
                            (\d ->
                                let
                                    link_m =
                                        List.head d.nodes
                                            |> Maybe.map
                                                (\n ->
                                                    toLink SettingsBaseUri n.nameid [] ++ q
                                                )
                                in
                                viewLabel "ml-2" link_m d
                            )
                        |> span []
                    ]

        RemoteData.Failure err ->
            viewHttpErrors err

        RemoteData.Loading ->
            div [ class "spinner" ] []

        _ ->
            text ""



{-
   ROLE VIEW
-}


viewRoleAddBox : Model -> Html Msg
viewRoleAddBox model =
    let
        isAdd =
            model.role_add

        form =
            model.artefact_form

        result =
            model.role_result

        name =
            Dict.get "name" form.post |> withDefault ""

        color =
            Dict.get "color" form.post

        about =
            Dict.get "about" form.post

        role_type =
            form.role_type

        isLoading =
            result == LoadingSlowly

        isSendable =
            name /= ""

        txt =
            if isAdd then
                { submit = T.createRole }

            else
                -- assume edit role
                { submit = T.updateRole }

        doSubmit =
            if isAdd then
                ternary isSendable [ onClick (Submit SubmitAddRole) ] []

            else
                -- assume edit label
                ternary isSendable [ onClick (Submit SubmitEditRole) ] []

        doCancel =
            CancelRole
    in
    div [ class "box" ]
        [ div [ class "field is-grouped is-grouped-multiline" ]
            [ p [ class "control" ]
                [ label [ class "label is-small" ] [ text (T.roleName ++ " *") ]
                , input
                    [ class "input autofocus"
                    , type_ "text"
                    , placeholder T.name
                    , value name
                    , onInput (ChangeArtefactPost "name")
                    , autofocus True
                    ]
                    []
                ]
            , p [ class "control" ]
                [ label [ class "label is-small" ] [ text T.color ]
                , ColorPicker.view { data = model.colorPicker, onOpen = OpenColor, onClose = CloseColor, onSelect = SelectColor }
                ]
            , p [ class "control is-expanded" ]
                [ label [ class "label is-small" ] [ text T.about ]
                , input
                    [ class "input"
                    , type_ "text"
                    , placeholder T.about
                    , value (withDefault "" about)
                    , onInput (ChangeArtefactPost "about")
                    ]
                    []
                ]
            , p [ class "control" ]
                [ label [ class "label is-small" ] [ text T.authority, helperButton "ml-2 is-right" (T.authorityHelper model.lexicon) ]
                , viewSelectAuthority
                    { onChangePost = UpdateNodePost
                    , data = model.nodeDoc
                    }
                ]
            ]
        , div [ class "field mt-2 mb-3" ]
            [ span [ class "help-label" ] [ text T.preview, text ": " ]
            , viewRoleExt model.commonOp "is-small" Nothing { nameid = "", name = ternary (name == "") "role name" name, color = color, role_type = role_type }
            ]
        , viewMandateInput (initFormText model.lexicon (Just NodeType.Role))
            (Just form.mandate)
            { onChangePost = UpdateNodePost
            , onAddResponsabilities = AddResponsabilities
            , onAddDomains = AddDomains
            , onAddPolicies = AddPolicies
            , data = model.nodeDoc
            , mdOps =
                Just
                    { onChangeViewMode = ChangeMandateViewMode
                    , onRichText = OnMandateRichText
                    , onToggleMdHelp = OnToggleMandateMdHelp
                    , post = model.artefact_form.post
                    }
            }
        , div [ class "field is-grouped is-grouped-right mt-1" ]
            [ p [ class "control buttons" ]
                [ button
                    ([ class "button is-success is-small"
                     , classList [ ( "is-loading", isLoading ) ]
                     , disabled (not isSendable)
                     ]
                        ++ doSubmit
                    )
                    [ text txt.submit ]
                , button [ class "button is-small", onClick doCancel ] [ text T.cancel ]
                ]
            ]
        , case result of
            Failure err ->
                viewGqlErrors err

            _ ->
                text ""
        ]


viewRoles : Model -> Html Msg
viewRoles model =
    let
        goToParent =
            showIf (model.node_focus.nameid /= model.node_focus.rootnameid)
                (viewGoRoot "" OnGoRoot)
    in
    div [ id "rolesTable" ]
        [ h2 [ class "subtitle is-size-3" ] [ text T.templateRoles, goToParent ]
        , div [ class "level" ]
            [ div [ class "mr-4" ] [ showMsg "labels-help" "mb-4 is-info" "icon-info" T.rolesInfoHeader T.rolesInfoDoc ]
            , div [ class "level-right is-align-self-flex-start", classList [ ( "is-hidden", model.role_add ) ] ] [ button [ class "button is-success", onClick (SafeEdit AddRole) ] [ textT T.newRole ] ]
            ]
        , if model.role_add then
            viewRoleAddBox model

          else
            text ""
        , case model.roles of
            Success roles ->
                if List.length roles == 0 then
                    div [ class "" ] [ text T.noRoles, text "." ]

                else
                    div [ class "table-container" ]
                        [ table [ class "table is-fullwidth" ]
                            [ thead [ class "is-size-6" ]
                                [ tr []
                                    [ th [] [ text T.name ]
                                    , th [] [ text T.description ]
                                    , th [] []
                                    , th [] []
                                    , th [] []
                                    ]
                                ]
                            , roles
                                |> List.concatMap
                                    (\d ->
                                        [ tr [ classList [ ( "settings-row-enter", model.role_anim_enter == Just d.id ) ] ] <|
                                            if model.role_edit == Just d then
                                                [ td [ colspan 5 ] [ viewRoleAddBox model ] ]

                                            else
                                                let
                                                    n_nodes =
                                                        withDefault 0 d.n_nodes
                                                in
                                                [ td [ onClick (SafeEdit <| EditRole d) ] [ viewRoleExt model.commonOp "button-light is-small" Nothing d ]
                                                , td [ class "is-aligned-left" ] [ d.about |> withDefault "" |> text |> List.singleton |> span [] ]
                                                , td [ class "is-aligned-left" ] [ ternary (NodeDoc.hasMandate d.mandate) (span [ class "is-w", onClick (ToggleMandate d.id) ] [ A.icon0 "icon-book-open" ]) (text "") ]
                                                , td [ attribute "style" "min-width: 9.4rem;" ]
                                                    [ if n_nodes > 1 then
                                                        span [ class "is-italic is-size-7" ] [ A.icon1 "icon-alert-circle icon-sm" "Defined in ", n_nodes |> String.fromInt |> text, text " circles." ]

                                                      else
                                                        text ""
                                                    ]
                                                , td [ class "is-aligned-right is-size-7", attribute "style" "min-width: 6.4rem;" ]
                                                    [ span [ class "button-light", onClick (SafeEdit <| EditRole d) ] [ text T.edit ]
                                                    , text " · "
                                                    , span
                                                        [ class "button-light"
                                                        , onClick <|
                                                            DoModalConfirmOpen (Submit <| SubmitDeleteRole d.id)
                                                                { message = Just ( T.roleDeleteInfoHeader, "" )
                                                                , txts = [ ( T.confirmDeleteRole, "" ), ( d.name, "is-strong" ), ( "?", "" ) ]
                                                                , confirmClass = "is-danger"
                                                                , confirmLabel = T.delete
                                                                }
                                                        ]
                                                        [ text T.remove ]
                                                    ]
                                                ]
                                        ]
                                            ++ (if model.showMandate == d.id then
                                                    [ tr [] [ td [ class "px-5", colspan 5 ] [ viewMandateSection model.lexicon (Just d.role_type) d.mandate Nothing ] ] ]

                                                else
                                                    []
                                               )
                                            ++ (case model.role_result_del of
                                                    Failure err ->
                                                        [ ternary (model.artefact_form.id == d.id)
                                                            (td [] [ viewGqlErrors err ])
                                                            (text "")
                                                        ]

                                                    _ ->
                                                        []
                                               )
                                    )
                                |> tbody []
                            ]
                        ]

            Failure err ->
                viewGqlErrors err

            LoadingSlowly ->
                div [ class "spinner" ] []

            _ ->
                text ""
        ]


viewRolesExt : CommonMsg Msg -> Url -> String -> RestData (List RoleExt) -> Html Msg
viewRolesExt commonOp url txt_yes list_ext_d =
    case list_ext_d of
        RemoteData.Success data ->
            if List.length data == 0 then
                text ""

            else
                let
                    q =
                        url.query |> Maybe.map (\uq -> "?" ++ uq) |> Maybe.withDefault ""
                in
                div [ class "mt-6" ]
                    [ h2 [ class "subtitle is-size-6 has-text-weight-semibold" ] [ text (txt_yes ++ " ") ]
                    , data
                        |> List.map
                            (\d ->
                                let
                                    link_m =
                                        List.head d.nodes
                                            |> Maybe.map
                                                (\n ->
                                                    toLink SettingsBaseUri n.nameid [] ++ q
                                                )
                                in
                                viewRoleExt commonOp "ml-2 is-small" link_m d
                            )
                        |> span []
                    ]

        RemoteData.Failure err ->
            viewHttpErrors err

        RemoteData.Loading ->
            div [ class "spinner" ] []

        _ ->
            text ""



{-
   TEMPLATE VIEW
-}


viewTemplateAddBox : Model -> Html Msg
viewTemplateAddBox model =
    let
        isAdd =
            model.template_add

        form =
            model.template_form

        result =
            model.template_result

        tplName =
            Dict.get "name" form.post |> withDefault ""

        description =
            form.description |> withDefault ""

        title =
            Dict.get "title" form.post |> withDefault ""

        comment =
            Dict.get "comment" form.post |> withDefault ""

        isLoading =
            result == LoadingSlowly

        isSendable =
            tplName /= "" && (title /= "" || comment /= "")

        submitMsg =
            ternary isAdd (Submit SubmitAddTemplate) (Submit SubmitEditTemplate)
    in
    div [ class "box" ]
        [ div [ class "field" ]
            [ label [ class "label" ] [ text T.templateName, text "*" ]
            , div [ class "control" ]
                [ input [ class "input", type_ "text", placeholder T.templateName, value tplName, onInput (ChangeTemplatePost "name"), autofocus True ] []
                ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text T.templateDescription ]
            , div [ class "control" ]
                [ input [ class "input", type_ "text", placeholder T.templateDescriptionHelp, value description, onInput ChangeTemplateDescription ] []
                ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text T.templateTitle ]
            , div [ class "control" ]
                [ input [ class "input", type_ "text", placeholder T.templateTitle, value title, onInput (ChangeTemplatePost "title") ] []
                ]
            ]
        , div [ class "field md-editor" ]
            [ label [ class "label" ] [ text T.templateComment ]
            , viewCommentInputHeader
                { onChangeViewMode = ChangeTemplateViewMode
                , onRichText = OnTemplateRichText
                , onToggleMdHelp = OnToggleTemplateMdHelp
                }
                "templateCommentInput"
                form
            , div [ class "control" ]
                [ textarea
                    [ id "templateCommentInput"
                    , class "textarea"
                    , classList [ ( "is-invisible-force", form.viewMode == Preview ) ]
                    , placeholder T.templateComment
                    , value comment
                    , onInput (ChangeTemplatePost "comment")
                    , rows 3
                    ]
                    []
                , if form.viewMode == Preview then
                    div []
                        [ hr [] []
                        , div [ class "mt-2 mx-3" ]
                            [ renderMarkdown "is-human hidden-textarea" comment ]
                        ]

                  else
                    text ""
                ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text T.type_ ]
            , viewTensionTypePicker "template-type-menu" form.type_ ChangeTemplateType
            ]
        , div [ class "field" ]
            [ div [ class "control" ]
                [ label [ class "checkbox" ]
                    [ input [ type_ "checkbox", checked form.is_recursive, onClick (ChangeTemplateRecursive (not form.is_recursive)) ] []
                    , text (" " ++ T.isRecursive)
                    ]
                , p [ class "help" ] [ text T.isRecursiveHelp ]
                ]
            ]
        , let
            targets =
                getPath model.path_data |> List.map .nameid

            labelsOp =
                { selectedLabels = form.labels
                , targets = targets
                , isRight = False
                }

            assigneesOp =
                { selectedAssignees = form.assignees
                , targets = targets
                , isRight = False
                }

            hasLabels =
                not (List.isEmpty form.labels)

            hasAssignees =
                not (List.isEmpty form.assignees)
          in
          div [ class "field" ]
            [ label [ class "label" ] [ text T.labels, text " / ", text T.assignees ]
            , div [ class "control" ]
                [ div [ classList [ ( "is-flex is-align-items-center", not hasLabels && not hasAssignees ) ], class "mb-2" ]
                    [ LabelSearchPanel.viewNew labelsOp model.labelsPanel
                        |> Html.map LabelSearchPanelMsg
                    , UserSearchPanel.viewNew assigneesOp model.assigneesPanel
                        |> Html.map UserSearchPanelMsg
                    ]
                ]
            ]
        , div [ class "field is-grouped" ]
            [ div [ class "control" ]
                [ button
                    [ class "button is-success"
                    , classList [ ( "is-loading", isLoading ) ]
                    , disabled (not isSendable || isLoading)
                    , onClick submitMsg
                    ]
                    [ text (ternary isAdd T.newTensionTemplate T.save) ]
                ]
            , div [ class "control" ]
                [ button [ class "button", onClick CancelTemplate ] [ text T.cancel ] ]
            ]
        , case result of
            Failure err ->
                viewGqlErrors err

            _ ->
                text ""
        ]


viewTemplates : Model -> Html Msg
viewTemplates model =
    let
        goToParent =
            showIf (model.node_focus.nameid /= model.node_focus.rootnameid)
                (viewGoRoot "" OnGoRoot)
    in
    div [ id "templatesTable" ]
        [ h2 [ class "subtitle is-size-3" ] [ text T.tensionTemplates, goToParent ]
        , div [ class "level" ]
            [ div [ class "mr-4" ] [ showMsg "templates-help" "mb-4 is-info" "icon-info" T.tensionTemplatesInfoHeader T.tensionTemplatesInfoDoc ]
            , div [ class "level-right is-align-self-flex-start", classList [ ( "is-hidden", model.template_add ) ] ] [ button [ class "button is-success", onClick (SafeEdit AddTemplate) ] [ textT T.newTensionTemplate ] ]
            ]
        , if model.template_add then
            viewTemplateAddBox model

          else
            text ""
        , case model.templates of
            Success templates ->
                if List.isEmpty templates then
                    div [] [ text T.noTensionTemplates, text "." ]

                else
                    div [ class "table-container" ]
                        [ table [ class "table is-fullwidth" ]
                            ([ thead [ class "is-size-6" ]
                                [ tr []
                                    [ th [] [ text T.name ]
                                    , th [] [ text T.templateDescription ]
                                    , th [] [ text T.type_ ]
                                    , th [] [ text T.labels ]
                                    , th [] [ text T.assignees ]
                                    , th [] [ text T.isRecursive ]
                                    , th [] []
                                    ]
                                ]
                             ]
                                ++ (templates
                                        |> List.concatMap
                                            (\d ->
                                                [ tr [ classList [ ( "settings-row-enter", model.template_anim_enter == Just d.id ) ] ] <|
                                                    if model.template_edit == Just d then
                                                        [ td [ colspan 7 ] [ viewTemplateAddBox model ] ]

                                                    else
                                                        [ td [ onClick (SafeEdit <| EditTemplate d) ] [ span [ class "button-light" ] [ text d.name ] ]
                                                        , td [] [ text (withDefault "" d.description) ]
                                                        , td [] [ tensionIcon2 d.type_ ]
                                                        , td [] [ Bulk.View.viewLabels Nothing (withDefault [] d.labels) ]
                                                        , td [] [ viewUsers False (withDefault [] d.assignees) ]
                                                        , td []
                                                            [ if d.is_recursive then
                                                                A.icon "icon-check"

                                                              else
                                                                text ""
                                                            ]
                                                        , td [ class "is-aligned-right is-size-7", attribute "style" "min-width: 6.4rem;" ]
                                                            [ span [ class "button-light", onClick (SafeEdit <| EditTemplate d) ] [ text T.edit ]
                                                            , text " · "
                                                            , span
                                                                [ class "button-light"
                                                                , onClick <|
                                                                    DoModalConfirmOpen (Submit <| SubmitDeleteTemplate d.id)
                                                                        { message = Just ( T.templateDeleteInfoHeader, "" )
                                                                        , txts = [ ( T.confirmDeleteTemplate, "" ), ( d.name, "is-strong" ), ( "?", "" ) ]
                                                                        , confirmClass = "is-danger"
                                                                        , confirmLabel = T.delete
                                                                        }
                                                                ]
                                                                [ text T.remove ]
                                                            ]
                                                        ]
                                                ]
                                            )
                                   )
                            )
                        ]

            Loading ->
                div [ class "spinner" ] []

            LoadingSlowly ->
                div [ class "spinner" ] []

            Failure err ->
                viewGqlErrors err

            NotAsked ->
                text ""
        ]


viewTensionTemplatesExt : Url -> String -> RestData (List TensionTemplateLite) -> Html Msg
viewTensionTemplatesExt url txt_yes list_ext_d =
    case list_ext_d of
        RemoteData.Success data ->
            if List.isEmpty data then
                text ""

            else
                let
                    q =
                        url.query |> Maybe.map (\uq -> "?" ++ uq) |> Maybe.withDefault ""
                in
                div [ class "mt-6" ]
                    [ h2 [ class "subtitle is-size-6 has-text-weight-semibold" ] [ text (txt_yes ++ " ") ]
                    , data
                        |> List.map
                            (\d ->
                                let
                                    link_m =
                                        List.head d.nodes
                                            |> Maybe.map
                                                (\n ->
                                                    toLink SettingsBaseUri n.nameid [] ++ q
                                                )
                                in
                                span [ class "ml-2" ]
                                    [ case link_m of
                                        Just link ->
                                            a [ href link ] [ text d.name ]

                                        Nothing ->
                                            text d.name
                                    ]
                            )
                        |> span []
                    ]

        RemoteData.Failure err ->
            viewHttpErrors err

        RemoteData.Loading ->
            div [ class "spinner" ] []

        _ ->
            text ""


type alias SwitchRecord =
    { index : Int -- reference index
    , msg :
        Int
        -> Bool
        -> Msg -- Msg
    , title : String -- title text
    , help : String -- help text
    , val : NodeRights -> Maybe Bool
    }


viewOrgaSettings : Dict.Dict String String -> GqlData NodeRights -> RestData Bool -> Int -> Html Msg
viewOrgaSettings lexicon orga_rights switch_result switch_index =
    let
        switches =
            [ SwitchRecord 0 SwitchUserCanJoin T.orgaUserInvitation T.orgaUserInvitationHelp .userCanJoin
            , SwitchRecord 1 SwitchGuestCanCreateTension (T.guestCanCreateTension lexicon) T.guestCanCreateTensionHelp .guestCanCreateTension
            , SwitchRecord 2 SwitchIsTemplateTensionOnly T.isTemplateTensionOnly T.isTemplateTensionOnlyHelp .isTemplateTensionOnly
            , SwitchRecord 3 SwitchIsPinnedTensionfetchRecursively T.isPinnedTensionfetchRecursively T.isPinnedTensionfetchRecursivelyHelp .isPinnedTensionfetchRecursively
            ]
    in
    case orga_rights of
        Success or ->
            div [] <|
                List.map
                    (\x ->
                        let
                            ref_name =
                                "switch" ++ String.fromInt x.index
                        in
                        div [ class "media" ]
                            [ div [ class "field" ]
                                [ input [ onClick (x.msg x.index False), id ref_name, class "switch is-rounded is-success", type_ "checkbox", name ref_name, checked (x.val or == Just True) ] []
                                , label [ for ref_name ]
                                    [ text space_
                                    , text x.title

                                    -- Use loadingSlowly because here it causes eyes distraction !
                                    --, loadingSpin ((switch_result == RemoteData.Loading) && switch_index == x.index)
                                    ]
                                , case switch_result of
                                    RemoteData.Failure e ->
                                        if switch_index == x.index then
                                            viewHttpErrors e

                                        else
                                            text ""

                                    _ ->
                                        text ""
                                , div [ class "help-label" ] [ text x.help ]
                                ]
                            ]
                    )
                    switches

        Loading ->
            div [ class "spinner" ] []

        LoadingSlowly ->
            div [ class "spinner" ] []

        Failure err ->
            viewGqlErrors err

        NotAsked ->
            text ""


viewLexiconSettings : String -> String -> RestData Bool -> Html Msg
viewLexiconSettings lexicon_input mandate_input lexicon_result =
    div []
        [ h3 [ class "subtitle is-size-4" ] [ text T.terminology, span [ class "help" ] [ text T.terminologyHelp ] ]
        , div [ class "field is-horizontal" ]
            [ div [ class "field-label is-inline-flex is-small", style "max-width" "5rem" ]
                [ label [ class "label" ] [ text T.tensionUp_ ] ]
            , div [ class "field-body " ]
                [ div [ class "field has-addons" ]
                    [ div [ class "control is-expanded is-form-narrow" ]
                        [ input
                            [ class "input is-small"
                            , type_ "text"
                            , placeholder T.tensionTerminologyPlaceholder
                            , value lexicon_input
                            , onInput OnLexiconInput
                            ]
                            []
                        ]
                    , div [ class "control" ]
                        [ button
                            [ class "button is-small is-success"
                            , classList [ ( "is-loading", lexicon_result == RemoteData.Loading ) ]
                            , onClick SubmitLexicon
                            ]
                            [ text T.save ]
                        ]
                    ]
                ]
            ]
        , div [ class "help-label mb-5" ] [ text T.tensionTerminologyHelp ]
        , div [ class "field is-horizontal" ]
            [ div [ class "field-label is-inline-flex is-small", style "max-width" "5rem" ]
                [ label [ class "label" ] [ text T.mandateUp_ ] ]
            , div [ class "field-body" ]
                [ div [ class "field has-addons" ]
                    [ div [ class "control is-expanded is-form-narrow" ]
                        [ input
                            [ class "input is-small"
                            , type_ "text"
                            , placeholder T.mandateTerminologyPlaceholder
                            , value mandate_input
                            , onInput OnMandateInput
                            ]
                            []
                        ]
                    , div [ class "control" ]
                        [ button
                            [ class "button is-small is-success"
                            , classList [ ( "is-loading", lexicon_result == RemoteData.Loading ) ]
                            , onClick SubmitLexicon
                            ]
                            [ text T.save ]
                        ]
                    ]
                ]
            ]
        , div [ class "help-label mb-5" ] [ text T.mandateTerminologyHelp ]
        , case lexicon_result of
            RemoteData.Success _ ->
                p [ class "help has-text-success" ]
                    [ A.icon1 "icon-check" "", text T.settingsSavedRefresh ]

            RemoteData.Failure err ->
                viewHttpErrors err

            _ ->
                text ""
        ]
