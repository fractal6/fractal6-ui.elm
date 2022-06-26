module Org.Settings exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Array
import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Browser.Navigation as Nav
import Codecs exposing (QuickDoc)
import Components.AuthModal as AuthModal
import Components.ColorPicker as ColorPicker exposing (ColorPicker)
import Components.HelperBar as HelperBar exposing (HelperBar)
import Components.JoinOrga as JoinOrga
import Components.Loading as Loading exposing (GqlData, ModalData, RequestResult(..), WebData, fromMaybeData, loadingSpin, viewAuthNeeded, viewGqlErrors, viewHttpErrors, withDefaultData, withMapData, withMaybeData)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.NodeDoc as NodeDoc exposing (NodeDoc, viewMandateInput, viewMandateSection, viewSelectAuthority)
import Components.OrgaMenu as OrgaMenu
import Components.TreeMenu as TreeMenu
import Dict exposing (Dict)
import Extra exposing (ternary)
import Extra.Events exposing (onClickPD, onEnter, onKeydown, onTab)
import Extra.Url exposing (queryBuilder, queryParser)
import Extra.Views exposing (showMsg)
import Form exposing (isPostSendable)
import Form.Help as Help
import Form.NewTension as NTF exposing (NewTensionInput(..), TensionTab(..))
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.NodeVisibility as NodeVisibility
import Fractal.Enum.RoleType as RoleType
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, span, table, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, autofocus, checked, class, classList, colspan, disabled, for, href, id, list, name, placeholder, rows, style, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (Flags_, FractalBaseRoute(..), NodeFocus, basePathChanged, focusFromNameid, focusState, nameidFromFlags, nid2rootid, uriFromNameid, uriFromUsername)
import ModelCommon.Requests exposing (fetchLabelsSub, fetchLabelsTop, fetchRolesSub, fetchRolesTop, login, setGuestCanCreateTension, setUserCanJoin)
import ModelCommon.View exposing (getNodeTextFromNodeType, roleColor, viewLabel, viewRoleExt)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.PatchNode
    exposing
        ( addOneLabel
        , addOneRole
        , removeOneLabel
        , removeOneRole
        , updateOneLabel
        , updateOneRole
        )
import Query.PatchTension exposing (actionRequest)
import Query.QueryNode exposing (fetchNode, getCircleRights, getLabels, getRoles, queryLocalGraph)
import RemoteData exposing (RemoteData)
import Session exposing (Apis, GlobalCmd(..))
import Task
import Text as T exposing (textH, textT, upH)
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
                    DoNavigate link ->
                        ( send (Navigate link), Cmd.none )

                    DoReplaceUrl url ->
                        ( Cmd.none, send (ReplaceUrl url) )

                    DoUpdateToken ->
                        ( Cmd.none, send UpdateUserToken )

                    DoUpdateUserSession uctx ->
                        ( Cmd.none, send (UpdateUserSession uctx) )

                    DoUpdateOrgs orgs ->
                        ( Cmd.none, send (UpdateSessionOrgs orgs) )

                    DoUpdateTree tree ->
                        ( Cmd.none, send (UpdateSessionTree tree) )

                    DoCreateTension nameid ->
                        ( Cmd.map NewTensionMsg <| send (NTF.OnOpen (FromNameid nameid)), Cmd.none )

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
    , labels_top : WebData (List Label)
    , labels_sub : WebData (List Label)
    , label_add : Bool
    , label_edit : Maybe LabelFull
    , label_result : GqlData LabelFull
    , label_result_del : GqlData LabelFull

    -- Roles
    , nodeDoc : NodeDoc
    , showMandate : String
    , roles : GqlData (List RoleExtFull)
    , roles_top : WebData (List RoleExt)
    , roles_sub : WebData (List RoleExt)
    , role_add : Bool
    , role_edit : Maybe RoleExtFull
    , role_result : GqlData RoleExtFull
    , role_result_del : GqlData RoleExtFull

    -- Orga
    , orga_rights : GqlData NodeRights
    , switch_result : WebData Bool
    , switch_index : Int

    -- Common
    , helperBar : HelperBar
    , modal_confirm : ModalConfirm Msg
    , refresh_trial : Int
    , url : Url
    , empty : {}

    -- Components
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
    | GlobalMenu
    | EditMenu


menuList : List MenuSettings
menuList =
    [ LabelsMenu, RolesMenu, GlobalMenu, EditMenu ]


menuEncoder : MenuSettings -> String
menuEncoder menu =
    case menu of
        LabelsMenu ->
            "labels"

        RolesMenu ->
            "roles"

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

        "global" ->
            GlobalMenu

        _ ->
            LabelsMenu


menuToString : MenuSettings -> String
menuToString menu =
    case menu of
        LabelsMenu ->
            upH T.labels

        RolesMenu ->
            upH T.roles

        GlobalMenu ->
            upH "Organisation"

        EditMenu ->
            upH "edit circle"


menuToIcon : MenuSettings -> String
menuToIcon menu =
    case menu of
        LabelsMenu ->
            "icon-tag"

        RolesMenu ->
            "icon-user"

        GlobalMenu ->
            "icon-shield"

        EditMenu ->
            "icon-edit"


resetForm : Model -> Model
resetForm model =
    { model
        | artefact_form = initArtefactNodeForm (LoggedIn model.artefact_form.uctx) model.node_focus.nameid
        , hasUnsavedData = False
        , label_result = NotAsked
        , label_result_del = NotAsked
        , role_result = NotAsked
        , role_result_del = NotAsked
    }



--
-- Msg
--


type Msg
    = PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
      -- Data Queries
    | GotPath Bool (GqlData LocalGraph)
      -- Page
    | ChangeMenuFocus MenuSettings
    | ChangeArtefactPost String String
    | SafeEdit Msg
    | SafeSend Msg
      -- Labels
    | GotLabels (GqlData (List LabelFull))
    | GotLabelsTop (WebData (List Label))
    | GotLabelsSub (WebData (List Label))
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
    | GotRolesTop (WebData (List RoleExt))
    | GotRolesSub (WebData (List RoleExt))
    | AddRole
    | EditRole RoleExtFull
    | CancelRole
    | SubmitAddRole Time.Posix
    | SubmitEditRole Time.Posix
    | SubmitDeleteRole String Time.Posix
    | GotRole (GqlData RoleExtFull)
    | GotRoleDel (GqlData RoleExtFull)
    | ToggleMandate String
    | ChangeRoleAuthority RoleType.RoleType
    | AddDomains
    | AddPolicies
    | AddResponsabilities
    | UpdateNodePost String String
      -- Orga
    | GotRootRights (GqlData NodeRights)
    | SwitchUserCanJoin Int Bool
    | SwitchGuestCanCreateTension Int Bool
    | GotUserCanJoin (WebData Bool)
    | GotGuestCanCreateTension (WebData Bool)
      -- Color Picker
    | OpenColor
    | CloseColor
    | SelectColor String
      -- Common
    | NoMsg
    | LogErr String
    | Navigate String
    | ExpandRoles
    | CollapseRoles
    | OnGoRoot
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Components
    | HelpMsg Help.Msg
    | NewTensionMsg NTF.Msg
    | JoinOrgaMsg JoinOrga.Msg
    | AuthModalMsg AuthModal.Msg
    | OrgaMenuMsg OrgaMenu.Msg
    | TreeMenuMsg TreeMenu.Msg



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

        -- Query parameters
        query =
            queryParser global.url

        menu =
            Dict.get "m" query |> withDefault [] |> List.head |> withDefault "" |> menuDecoder

        action =
            Dict.get "a" query |> withDefault [] |> List.head |> withDefault ""

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
            , menuFocus = menu
            , menuList = menuList
            , colorPicker = ColorPicker.init
            , artefact_form = initArtefactNodeForm global.session.user newFocus.nameid
            , hasUnsavedData = False

            -- Labels
            , labels = Loading
            , labels_top = RemoteData.Loading
            , labels_sub = RemoteData.Loading
            , label_add = ternary (action == "new" && menu == LabelsMenu) True False
            , label_edit = Nothing
            , label_result = NotAsked
            , label_result_del = NotAsked

            -- Roles
            , nodeDoc = NodeDoc.create "" global.session.user
            , showMandate = ""
            , roles = Loading
            , roles_top = RemoteData.Loading
            , roles_sub = RemoteData.Loading
            , role_add = ternary (action == "new" && menu == RolesMenu) True False
            , role_edit = Nothing
            , role_result = NotAsked
            , role_result_del = NotAsked

            -- Orga
            , orga_rights = NotAsked
            , switch_result = RemoteData.NotAsked
            , switch_index = -1

            -- Common
            , helperBar = HelperBar.create
            , refresh_trial = 0
            , url = global.url
            , empty = {}
            , help = Help.init global.session.user
            , tensionForm = NTF.init global.session.user
            , modal_confirm = ModalConfirm.init NoMsg
            , joinOrga = JoinOrga.init newFocus.nameid global.session.user
            , authModal = AuthModal.init global.session.user (Dict.get "puid" query |> Maybe.map List.head |> withDefault Nothing)
            , orgaMenu = OrgaMenu.init newFocus global.session.orga_menu global.session.orgs_data global.session.user
            , treeMenu = TreeMenu.init SettingsBaseUri global.url.query newFocus global.session.tree_menu global.session.tree_data global.session.user
            }

        cmds =
            [ ternary fs.focusChange (queryLocalGraph apis newFocus.nameid (GotPath True)) Cmd.none
            , sendSleep PassedSlowLoadTreshold 500
            , Cmd.map OrgaMenuMsg (send OrgaMenu.OnLoad)
            , Cmd.map TreeMenuMsg (send TreeMenu.OnLoad)
            ]
                ++ (case menu of
                        LabelsMenu ->
                            [ getLabels apis newFocus.nameid GotLabels
                            , fetchLabelsTop apis newFocus.nameid GotLabelsTop
                            , fetchLabelsSub apis newFocus.nameid GotLabelsSub
                            ]

                        RolesMenu ->
                            [ getRoles apis newFocus.nameid GotRoles
                            , fetchRolesTop apis newFocus.nameid GotRolesTop
                            , fetchRolesSub apis newFocus.nameid GotRolesSub
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
                                    List.head path.path |> Maybe.map (\p -> p.nameid) |> withDefault ""
                            in
                            ( { model | path_data = Success newPath }, queryLocalGraph apis nameid (GotPath False), Cmd.none )

                _ ->
                    ( { model | path_data = result }, Cmd.none, Cmd.none )

        ChangeMenuFocus menu ->
            case menu of
                EditMenu ->
                    case model.path_data of
                        Success lg ->
                            ( model, send (Navigate (toHref (Route.Tension_Dynamic_Dynamic_Action { param1 = nid2rootid lg.focus.nameid, param2 = getSourceTid lg.focus }) ++ "?v=edit")), Cmd.none )

                        _ ->
                            ( model, Cmd.none, Cmd.none )

                _ ->
                    let
                        query =
                            queryBuilder
                                [ ( "m", menuEncoder menu ) ]
                    in
                    ( model, Cmd.none, Nav.pushUrl global.key (uriFromNameid SettingsBaseUri model.node_focus.nameid [] ++ "?" ++ query) )

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
                        , txts = [ ( upH T.confirmUnsafe, "" ) ]
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
                    | label_add = ternary (model.label_add == True) False True
                    , label_edit = Nothing
                    , colorPicker = ColorPicker.setColor Nothing model.colorPicker
                  }
                , Cmd.none
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
            , Cmd.none
            , Cmd.none
            )

        CancelLabel ->
            ( { model
                | label_add = False
                , label_edit = Nothing
                , label_result = NotAsked
                , label_result_del = NotAsked
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
                            withMaybeData model.labels |> withDefault []

                        new =
                            if model.label_add then
                                [ label ] ++ d

                            else
                                -- assume edit
                                List.map
                                    (\x ->
                                        if x.id == label.id then
                                            label

                                        else
                                            x
                                    )
                                    d
                    in
                    ( { model | label_result = result, labels = Success new, label_add = False, label_edit = Nothing } |> resetForm
                    , Cmd.none
                    , Cmd.none
                    )

                DuplicateErr ->
                    let
                        label_name =
                            Dict.get "name" model.artefact_form.post |> withDefault ""

                        here name =
                            (withMaybeData model.labels |> withDefault [] |> List.filter (\x -> x.name == name) |> List.length)
                                > 0

                        form =
                            model.artefact_form
                    in
                    if model.label_add && not (here label_name) then
                        -- set the labels in the node labels list
                        ( { model | label_result = LoadingSlowly, artefact_form = { form | id = "" } }, send (Submit SubmitEditLabel), Cmd.none )

                    else
                        -- trow error if the labels is in the list of labels
                        ( { model | label_result = result }, Cmd.none, Cmd.none )

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
                            withMaybeData model.labels |> withDefault []

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
                    | role_add = ternary (model.role_add == True) False True
                    , role_edit = Nothing
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
                                    ++ [ ( "role_type", RoleType.toString role.role_type ) ]
                                    ++ (role.about |> Maybe.map (\x -> [ ( "about", x ) ]) |> withDefault [])
                                    ++ [ ( "old_name", role.name ) ]
                                )
                        , mandate = withDefault initMandate role.mandate
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
                , nodeDoc = NodeDoc.create "" global.session.user
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
                            withMaybeData model.roles |> withDefault []

                        new =
                            if model.role_add then
                                [ role ] ++ d

                            else
                                -- assume edit
                                List.map
                                    (\x ->
                                        if x.id == role.id then
                                            role

                                        else
                                            x
                                    )
                                    d
                    in
                    ( { model | role_result = result, roles = Success new, role_add = False, role_edit = Nothing } |> resetForm
                    , Cmd.none
                    , Cmd.none
                    )

                DuplicateErr ->
                    let
                        role_name =
                            Dict.get "name" model.artefact_form.post |> withDefault ""

                        here name =
                            (withMaybeData model.roles |> withDefault [] |> List.filter (\x -> x.name == name) |> List.length)
                                > 0

                        form =
                            model.artefact_form
                    in
                    if model.role_add && not (here role_name) then
                        -- set the roles in the node roles list
                        ( { model | role_result = LoadingSlowly, artefact_form = { form | id = "" } }, send (Submit SubmitEditRole), Cmd.none )

                    else
                        -- trow error if the roles is in the list of roles
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
                            withMaybeData model.roles |> withDefault []

                        new =
                            List.filter (\x -> x.id /= model.artefact_form.id) d
                    in
                    ( { model | role_result_del = NotAsked, roles = Success new, role_add = False, role_edit = Nothing } |> resetForm
                    , Cmd.none
                    , Cmd.none
                    )

                _ ->
                    ( { model | role_result_del = result }, Cmd.none, Cmd.none )

        ChangeRoleAuthority role_type ->
            let
                form =
                    model.artefact_form

                newForm =
                    { form | post = Dict.insert "role_type" (RoleType.toString role_type) form.post }
            in
            ( { model | artefact_form = newForm }, Cmd.none, Cmd.none )

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
                | artefact_form = { form | mandate = NodeDoc.getMandate newNodeDoc }
                , nodeDoc = newNodeDoc
                , hasUnsavedData = True
              }
            , Cmd.none
            , Cmd.none
            )

        -- Orga
        GotRootRights result ->
            ( { model | orga_rights = result }, Cmd.none, Cmd.none )

        SwitchUserCanJoin i confirmed ->
            let
                val =
                    withMaybeData model.orga_rights |> Maybe.map .userCanJoin |> withDefault Nothing |> withDefault False

                isPublic =
                    withMaybeData model.orga_rights |> Maybe.map .visibility |> Maybe.map (\x -> x == NodeVisibility.Public) |> withDefault False
            in
            if val == False && not isPublic && not confirmed then
                -- show modal to confirm root circle is going to be public
                ( model
                , send <|
                    DoModalConfirmOpen (SwitchUserCanJoin i True)
                        { message = Just ( "Please confirm the change", "" )
                        , txts = [ ( "Enabling this setting will make the visibility of the root circle ", "" ), ( "Public", "is-strong" ), ( ".", "" ) ]
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
                    withMaybeData model.orga_rights |> Maybe.map .guestCanCreateTension |> withDefault Nothing |> withDefault False
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

        -- Color Picker
        OpenColor ->
            ( { model | colorPicker = ColorPicker.open model.colorPicker }
            , if model.colorPicker.isOpen == False then
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

        Navigate url ->
            ( model, Cmd.none, Nav.pushUrl global.key url )

        ExpandRoles ->
            ( { model | helperBar = HelperBar.expand model.helperBar }, Cmd.none, Cmd.none )

        CollapseRoles ->
            ( { model | helperBar = HelperBar.collapse model.helperBar }, Cmd.none, Cmd.none )

        OnGoRoot ->
            let
                query =
                    model.url.query |> Maybe.map (\uq -> "?" ++ uq) |> Maybe.withDefault ""
            in
            ( model, send (Navigate (uriFromNameid SettingsBaseUri model.node_focus.rootnameid [] ++ query)), Cmd.none )

        -- Confirm Modal
        DoModalConfirmOpen msg mess ->
            ( { model | modal_confirm = ModalConfirm.open msg mess model.modal_confirm }, Cmd.none, Cmd.none )

        DoModalConfirmClose _ ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, Cmd.none, Cmd.none )

        DoModalConfirmSend ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, send model.modal_confirm.msg, Cmd.none )

        -- Components
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
                                if Tuple.first o == True then
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


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ model =
    [ Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    , Ports.cancelColorFromJs (always CloseColor)
    ]
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        ++ (NTF.subscriptions model.tensionForm |> List.map (\s -> Sub.map NewTensionMsg s))
        ++ (JoinOrga.subscriptions model.joinOrga |> List.map (\s -> Sub.map JoinOrgaMsg s))
        ++ (AuthModal.subscriptions |> List.map (\s -> Sub.map AuthModalMsg s))
        ++ (OrgaMenu.subscriptions |> List.map (\s -> Sub.map OrgaMenuMsg s))
        ++ (TreeMenu.subscriptions |> List.map (\s -> Sub.map TreeMenuMsg s))
        |> Sub.batch



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    let
        helperData =
            { user = global.session.user
            , uriQuery = model.url.query
            , path_data = withMaybeData model.path_data
            , focus = model.node_focus
            , baseUri = SettingsBaseUri
            , data = model.helperBar
            , onExpand = ExpandRoles
            , onCollapse = CollapseRoles
            , onToggleTreeMenu = TreeMenuMsg TreeMenu.OnToggle
            }
    in
    { title = "Settings · " ++ (String.join "/" <| LE.unique [ model.node_focus.rootnameid, model.node_focus.nameid |> String.split "#" |> List.reverse |> List.head |> withDefault "" ])
    , body =
        [ HelperBar.view helperData
        , div [ id "mainPane" ] [ view_ model ]
        , Help.view model.empty model.help |> Html.map HelpMsg
        , NTF.view { tree_data = TreeMenu.getOrgaData_ model.treeMenu, path_data = model.path_data } model.tensionForm |> Html.map NewTensionMsg
        , JoinOrga.view model.empty model.joinOrga |> Html.map JoinOrgaMsg
        , AuthModal.view model.empty model.authModal |> Html.map AuthModalMsg
        , OrgaMenu.view model.empty model.orgaMenu |> Html.map OrgaMenuMsg
        , TreeMenu.view model.empty model.treeMenu |> Html.map TreeMenuMsg
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        ]
    }


view_ : Model -> Html Msg
view_ model =
    div [ class "columns is-centered" ]
        [ div [ class "column is-12 is-11-desktop is-9-fullhd mt-5" ]
            [ div [ class "section_" ]
                [ div [ class "columns" ]
                    [ div [ class "column is-one-fifth" ] [ viewSettingsMenu model ]
                    , div [ class "column" ] [ viewSettingsContent model ]
                    ]
                ]
            ]
        ]


viewSettingsMenu : Model -> Html Msg
viewSettingsMenu model =
    nav [ id "menuSettings", class "menu" ]
        [ ul [ class "menu-list" ] <|
            (model.menuList
                |> List.map
                    (\x ->
                        [ case x of
                            EditMenu ->
                                hr [ class "dropdown-divider has-background-border-light" ] []

                            _ ->
                                text ""
                        , li []
                            [ a [ onClickPD (ChangeMenuFocus x), target "_blank", classList [ ( "is-active", x == model.menuFocus ) ] ]
                                [ A.icon1 (menuToIcon x) (menuToString x) ]
                            ]
                        ]
                    )
                |> List.concat
            )
        ]


viewSettingsContent : Model -> Html Msg
viewSettingsContent model =
    case model.menuFocus of
        LabelsMenu ->
            div []
                [ --@todo lazy loading...
                  viewLabels model
                , viewLabelsExt T.labelsTop model.labels model.labels_top
                , viewLabelsExt T.labelsSub model.labels model.labels_sub
                ]

        RolesMenu ->
            div []
                [ --@todo lazy loading...
                  viewRoles model
                , viewRolesExt T.rolesTop model.roles model.roles_top
                , viewRolesExt T.rolesSub model.roles model.roles_sub
                ]

        GlobalMenu ->
            div []
                [ h2 [ class "subtitle is-size-3" ] [ text "Organisation settings" ]
                , viewOrgaSettings model.orga_rights model.switch_result model.switch_index
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
                [ label [ class "label is-small" ] [ text "Label name *" ]
                , input
                    [ class "input autofocus"
                    , type_ "text"
                    , placeholder "Label name"
                    , value name
                    , onInput (ChangeArtefactPost "name")
                    , autofocus True
                    ]
                    []
                ]
            , p [ class "control" ]
                [ label [ class "label is-small" ] [ textH T.color ]
                , ColorPicker.view { data = model.colorPicker, onOpen = OpenColor, onClose = CloseColor, onSelect = SelectColor }
                ]
            , p [ class "control is-expanded" ]
                [ label [ class "label is-small" ] [ textH T.description ]
                , input
                    [ class "input"
                    , type_ "text"
                    , placeholder "Description"
                    , value (withDefault "" description)
                    , onInput (ChangeArtefactPost "description")
                    ]
                    []
                ]
            , p [ class "control buttons", attribute "style" "margin-top: 1.5rem;" ]
                [ button [ class "button is-small", onClick doCancel ] [ textH T.cancel ]
                , button
                    ([ class "button is-success is-small"
                     , classList [ ( "is-loading", isLoading ) ]
                     , disabled (not isSendable)
                     ]
                        ++ doSubmit
                    )
                    [ textH txt.submit ]
                ]
            ]
        , div []
            [ span [ class "help-label" ] [ text "Preview: " ]
            , viewLabel "" (Label "" (ternary (name == "") "label name" name) color)
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
            if model.node_focus.nameid /= model.node_focus.rootnameid then
                span [ class "help-label is-grey-light button-light is-h has-text-weight-light", onClick OnGoRoot ] [ A.icon "arrow-up", text T.goRoot ]

            else
                text ""
    in
    div [ id "labelsTable" ]
        [ h2 [ class "subtitle is-size-3" ] [ textH T.labels, goToParent ]
        , div [ class "level" ]
            [ div [ class "mr-4" ] [ showMsg "labels-help" "mb-4" "icon-info" T.labelsInfoHeader T.labelsInfoDoc ]
            , div [ class "level-right" ] [ button [ class "button is-success", classList [ ( "is-active", model.label_add ) ], onClick (SafeEdit AddLabel) ] [ textT T.newLabel ] ]
            ]
        , if model.label_add then
            viewLabelAddBox model

          else
            text ""
        , case model.labels of
            Success labels ->
                if List.length labels == 0 then
                    div [ class "" ] [ textH T.noLabels, text "." ]

                else
                    div [ class "table-container" ]
                        [ table [ class "table is-fullwidth" ]
                            [ thead []
                                [ tr []
                                    [ th [] [ textH T.name ]
                                    , th [] [ textH T.description ]
                                    , th [] [ text "" ]
                                    , th [] []
                                    ]
                                ]
                            , labels
                                |> List.map
                                    (\d ->
                                        [ tr [] <|
                                            if model.label_edit == Just d then
                                                [ td [ colspan 4 ] [ viewLabelAddBox model ] ]

                                            else
                                                let
                                                    n_nodes =
                                                        withDefault 0 d.n_nodes
                                                in
                                                [ td [ onClick (SafeEdit <| EditLabel d) ] [ viewLabel "button-light" (Label d.id d.name d.color) ]
                                                , td [ class "is-aligned-left" ] [ d.description |> withDefault "" |> text |> List.singleton |> span [] ]
                                                , td [ attribute "style" "min-width: 9.4rem;" ]
                                                    [ if n_nodes > 1 then
                                                        span [ class "is-italic is-size-7" ] [ A.icon1 "icon-alert-circle icon-sm" "Defined in ", n_nodes |> String.fromInt |> text, text " circles" ]

                                                      else
                                                        text ""
                                                    ]
                                                , td [ class "is-aligned-right is-size-7", attribute "style" "min-width: 6.4rem;" ]
                                                    [ span [ class "button-light", onClick (SafeEdit <| EditLabel d) ] [ textH T.edit ]
                                                    , text " · "
                                                    , span
                                                        [ class "button-light"
                                                        , onClick <|
                                                            DoModalConfirmOpen (Submit <| SubmitDeleteLabel d.id)
                                                                { message = Just ( T.labelDeleteInfoHeader, "" )
                                                                , txts = [ ( upH T.confirmDeleteLabel, "" ), ( d.name, "is-strong" ), ( "?", "" ) ]
                                                                }
                                                        ]
                                                        [ textH T.remove ]
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
                                |> List.concat
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


viewLabelsExt : String -> GqlData (List LabelFull) -> WebData (List Label) -> Html Msg
viewLabelsExt txt_yes list_d list_ext_d =
    case list_ext_d of
        RemoteData.Success data ->
            if List.length data == 0 then
                text ""

            else
                let
                    circle_data =
                        withDefaultData [] list_d
                in
                div [ class "mt-6" ]
                    [ textH (txt_yes ++ " ")
                    , data
                        |> List.filter (\d -> not (List.member d.name (List.map (\x -> x.name) circle_data)))
                        |> List.map
                            (\d ->
                                viewLabel "ml-2" (Label d.id d.name d.color)
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
            Dict.get "role_type" form.post |> Maybe.map RoleType.fromString |> withDefault Nothing |> withDefault RoleType.Peer

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
                [ label [ class "label is-small" ] [ text "Role name *" ]
                , input
                    [ class "input autofocus"
                    , type_ "text"
                    , placeholder "Role name"
                    , value name
                    , onInput (ChangeArtefactPost "name")
                    , autofocus True
                    ]
                    []
                ]
            , p [ class "control" ]
                [ label [ class "label is-small" ] [ textH T.color ]
                , ColorPicker.view { data = model.colorPicker, onOpen = OpenColor, onClose = CloseColor, onSelect = SelectColor }
                ]
            , p [ class "control is-expanded" ]
                [ label [ class "label is-small" ] [ textH T.about ]
                , input
                    [ class "input"
                    , type_ "text"
                    , placeholder "About"
                    , value (withDefault "" about)
                    , onInput (ChangeArtefactPost "about")
                    ]
                    []
                ]
            , p [ class "control" ]
                [ label [ class "label is-small" ] [ textH T.authority ]
                , viewSelectAuthority "is-right"
                    { onSelect = ChangeRoleAuthority
                    , selection = role_type
                    }
                ]
            ]
        , div [ class "field mt-2 mb-3" ]
            [ span [ class "help-label" ] [ text "Preview: " ]
            , viewRoleExt "" (RoleExt "" (ternary (name == "") "role name" name) color role_type)
            ]
        , viewMandateInput (getNodeTextFromNodeType NodeType.Role)
            (Just form.mandate)
            { onChangePost = UpdateNodePost
            , onAddResponsabilities = AddResponsabilities
            , onAddDomains = AddDomains
            , onAddPolicies = AddPolicies
            , data = model.nodeDoc
            }
        , div [ class "field is-grouped is-grouped-right" ]
            [ p [ class "control buttons" ]
                [ button [ class "button is-small", onClick doCancel ] [ textH T.cancel ]
                , button
                    ([ class "button is-success is-small"
                     , classList [ ( "is-loading", isLoading ) ]
                     , disabled (not isSendable)
                     ]
                        ++ doSubmit
                    )
                    [ textH txt.submit ]
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
            if model.node_focus.nameid /= model.node_focus.rootnameid then
                span [ class "help-label is-grey-light button-light is-h has-text-weight-light", onClick OnGoRoot ] [ A.icon "arrow-up", text T.goRoot ]

            else
                text ""
    in
    div [ id "rolesTable" ]
        [ h2 [ class "subtitle is-size-3" ] [ textH T.roles, goToParent ]
        , div [ class "level" ]
            [ div [ class "mr-4" ] [ showMsg "labels-help" "mb-4" "icon-info" T.rolesInfoHeader T.rolesInfoDoc ]
            , div [ class "level-right" ] [ button [ class "button is-success level-right", classList [ ( "is-active", model.role_add ) ], onClick (SafeEdit AddRole) ] [ textT T.newRole ] ]
            ]
        , if model.role_add then
            viewRoleAddBox model

          else
            text ""
        , case model.roles of
            Success roles ->
                if List.length roles == 0 then
                    div [ class "" ] [ textH T.noRoles, text "." ]

                else
                    div [ class "table-container" ]
                        [ table [ class "table is-fullwidth" ]
                            [ thead []
                                [ tr []
                                    [ th [] [ textH T.name ]
                                    , th [] [ textH T.description ]
                                    , th [] [ text "" ]
                                    , th [] [ text "" ]
                                    , th [] []
                                    ]
                                ]
                            , roles
                                |> List.map
                                    (\d ->
                                        [ tr [] <|
                                            if model.role_edit == Just d then
                                                [ td [ colspan 5 ] [ viewRoleAddBox model ] ]

                                            else
                                                let
                                                    n_nodes =
                                                        withDefault 0 d.n_nodes
                                                in
                                                [ td [ onClick (SafeEdit <| EditRole d) ] [ viewRoleExt "button-light" (RoleExt d.id d.name d.color d.role_type) ]
                                                , td [ class "is-aligned-left" ] [ d.about |> withDefault "" |> text |> List.singleton |> span [] ]
                                                , td [ class "is-aligned-left" ] [ ternary (NodeDoc.hasMandate d.mandate) (span [ class "is-w", onClick (ToggleMandate d.id) ] [ A.icon0 "icon-book-open" ]) (text "") ]
                                                , td [ attribute "style" "min-width: 9.4rem;" ]
                                                    [ if n_nodes > 1 then
                                                        span [ class "is-italic is-size-7" ] [ A.icon1 "icon-alert-circle icon-sm" "Defined in ", n_nodes |> String.fromInt |> text, text " circles." ]

                                                      else
                                                        text ""
                                                    ]
                                                , td [ class "is-aligned-right is-size-7", attribute "style" "min-width: 6.4rem;" ]
                                                    [ span [ class "button-light", onClick (SafeEdit <| EditRole d) ] [ textH T.edit ]
                                                    , text " · "
                                                    , span
                                                        [ class "button-light"
                                                        , onClick <|
                                                            DoModalConfirmOpen (Submit <| SubmitDeleteRole d.id)
                                                                { message = Just ( T.roleDeleteInfoHeader, "" )
                                                                , txts = [ ( upH T.confirmDeleteRole, "" ), ( d.name, "is-strong" ), ( "?", "" ) ]
                                                                }
                                                        ]
                                                        [ textH T.remove ]
                                                    ]
                                                ]
                                        ]
                                            ++ (if model.showMandate == d.id then
                                                    [ tr [] [ td [ class "px-5", colspan 5 ] [ viewMandateSection (text "") d.mandate (Just d.role_type) ] ] ]

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
                                |> List.concat
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


viewRolesExt : String -> GqlData (List RoleExtFull) -> WebData (List RoleExt) -> Html Msg
viewRolesExt txt_yes list_d list_ext_d =
    case list_ext_d of
        RemoteData.Success data ->
            if List.length data == 0 then
                text ""

            else
                let
                    circle_data =
                        withDefaultData [] list_d
                in
                div [ class "mt-6" ]
                    [ textH (txt_yes ++ " ")
                    , data
                        |> List.filter (\d -> not (List.member d.name (List.map (\x -> x.name) circle_data)))
                        |> List.map
                            (\d ->
                                viewRoleExt "ml-2" (RoleExt d.id d.name d.color d.role_type)
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


viewOrgaSettings : GqlData NodeRights -> WebData Bool -> Int -> Html Msg
viewOrgaSettings orga_rights switch_result switch_index =
    let
        switches =
            [ SwitchRecord 0 SwitchUserCanJoin T.orgaUserInvitation T.orgaUserInvitationHelp .userCanJoin
            , SwitchRecord 1 SwitchGuestCanCreateTension T.guestCanCreateTension T.guestCanCreateTensionHelp .guestCanCreateTension
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
                                    [ text T.space_
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
                                , span [ class "help" ] [ text x.help ]
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
