module Org.Settings exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Array
import Auth exposing (AuthState(..), ErrState(..), doRefreshToken, parseErr, refreshAuthModal)
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav
import Codecs exposing (QuickDoc)
import Components.ColorPicker as ColorPicker exposing (ColorPicker)
import Components.HelperBar as HelperBar exposing (HelperBar)
import Components.Loading as Loading exposing (GqlData, ModalData, RequestResult(..), WebData, viewAuthNeeded, viewGqlErrors, viewHttpErrors, withDefaultData, withMaybeData)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm)
import Date exposing (formatTime)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Extra.Events exposing (onClickPD, onEnter, onKeydown, onTab)
import Extra.Url exposing (queryBuilder, queryParser)
import Form exposing (isPostSendable)
import Form.Help as Help
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, span, table, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, colspan, disabled, href, id, list, placeholder, rows, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Icon as I
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (Flags_, FractalBaseRoute(..), NodeFocus, basePathChanged, focusFromNameid, focusState, nameidFromFlags, uriFromNameid, uriFromUsername)
import ModelCommon.Requests exposing (fetchLabels, login)
import ModelCommon.View exposing (mediaTension, roleColor, viewLabel)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.AddTension exposing (addOneTension)
import Query.PatchNode exposing (addOneLabel, removeOneLabel, updateOneLabel)
import Query.PatchTension exposing (actionRequest)
import Query.QueryNode exposing (fetchNode, queryLabels, queryLocalGraph)
import RemoteData exposing (RemoteData)
import Task
import Text as T exposing (textH, textT, upH)
import Time


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

                    DoAuth uctx ->
                        ( send (DoOpenAuthModal uctx), Cmd.none )

                    DoUpdateToken ->
                        ( Cmd.none, send UpdateUserToken )

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
    , labels : GqlData (List LabelFull)
    , labels_sub : WebData (List LabelFull)
    , menuFocus : MenuSettings
    , menuList : List MenuSettings
    , label_add : Bool
    , label_edit : Maybe LabelFull
    , label_result : GqlData LabelFull
    , label_result_del : GqlData LabelFull
    , label_form : LabelNodeForm
    , colorPicker : ColorPicker

    -- Common
    , node_action : ActionState
    , isModalActive : Bool -- Only use by JoinOrga for now. (other actions rely on Bulma drivers)
    , modalAuth : ModalAuth
    , helperBar : HelperBar
    , refresh_trial : Int
    , help : Help.State
    , modal_confirm : ModalConfirm Msg
    }


type MenuSettings
    = LabelsMenu
    | SecurityMenu


menuList : List MenuSettings
menuList =
    [ LabelsMenu, SecurityMenu ]


menuEncoder : MenuSettings -> String
menuEncoder menu =
    case menu of
        LabelsMenu ->
            "labels"

        SecurityMenu ->
            "security"


menuDecoder : String -> MenuSettings
menuDecoder menu =
    case menu of
        "labels" ->
            LabelsMenu

        "security" ->
            SecurityMenu

        _ ->
            LabelsMenu


menuToString : MenuSettings -> String
menuToString menu =
    case menu of
        LabelsMenu ->
            upH T.labels

        SecurityMenu ->
            upH T.security


menuToIcon : MenuSettings -> String
menuToIcon menu =
    case menu of
        LabelsMenu ->
            "icon-tag"

        SecurityMenu ->
            "icon-shield"



--
-- Msg
--


type Msg
    = PassedSlowLoadTreshold -- timer
    | PushTension TensionForm (GqlData Tension -> Msg)
    | PushGuest ActionForm
    | Submit (Time.Posix -> Msg) -- Get Current Time
      -- Data Queries
    | GotPath (GqlData LocalGraph)
    | GotPath2 (GqlData LocalGraph)
      -- Page
    | GotLabels (GqlData (List LabelFull))
    | GotLabelsSub (WebData (List LabelFull))
    | ChangeMenuFocus MenuSettings
    | AddLabel
    | EditLabel LabelFull
    | CancelLabel
    | ChangeLabelPost String String
    | SubmitAddLabel Time.Posix
    | SubmitEditLabel Time.Posix
    | SubmitDeleteLabel String Time.Posix
    | GotLabel (GqlData LabelFull)
    | GotLabelDel (GqlData LabelFull)
      -- JoinOrga Action
    | DoJoinOrga String
    | DoJoinOrga2 (GqlData Node)
    | DoJoinOrga3 Node Time.Posix
    | JoinAck (GqlData ActionResult)
      -- Token refresh
    | DoOpenAuthModal UserCtx
    | DoCloseAuthModal String
    | ChangeAuthPost String String
    | SubmitUser UserAuthForm
    | GotSignin (WebData UserCtx)
    | SubmitKeyDown Int -- Detect Enter (for form sending)
      -- Color Picker
    | OpenColor
    | CloseColor
    | SelectLabelColor String
      -- Common
    | NoMsg
    | LogErr String
    | Navigate String
    | DoOpenModal
    | DoCloseModal ModalData
    | ExpandRoles
    | CollapseRoles
      -- Confirm Modal
    | DoModalConfirmOpen Msg (List ( String, String ))
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Help
    | HelpMsg Help.Msg



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
            Dict.get "m" query |> withDefault "" |> menuDecoder

        -- Focus
        newFocus =
            flags
                |> nameidFromFlags
                |> focusFromNameid

        -- What has changed
        fs =
            focusState SettingsBaseUri global.session.referer global.session.node_focus newFocus

        model =
            { node_focus = newFocus
            , path_data =
                global.session.path_data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , labels = Loading
            , labels_sub = RemoteData.Loading
            , menuFocus = menu
            , menuList = menuList
            , label_add = False
            , label_edit = Nothing
            , label_result = NotAsked
            , label_result_del = NotAsked
            , label_form = initLabelNodeForm global.session.user newFocus.nameid
            , colorPicker = ColorPicker.init

            -- Common
            , node_action = NoOp
            , isModalActive = False
            , modalAuth = Inactive
            , helperBar = HelperBar.create
            , refresh_trial = 0
            , help = Help.init global.session.user
            , modal_confirm = ModalConfirm.init NoMsg
            }

        cmds =
            [ ternary fs.focusChange (queryLocalGraph apis.gql newFocus.nameid GotPath) Cmd.none
            , sendSleep PassedSlowLoadTreshold 500
            ]
                ++ (case menu of
                        LabelsMenu ->
                            [ queryLabels apis.gql newFocus.nameid GotLabels
                            , fetchLabels apis.rest newFocus.nameid GotLabelsSub
                            ]

                        _ ->
                            []
                   )
    in
    ( model
    , Cmd.batch cmds
    , send (UpdateSessionFocus (Just newFocus))
    )


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global message model =
    let
        apis =
            global.session.apis
    in
    case message of
        PushTension form ack ->
            ( model, addOneTension apis.gql form ack, Cmd.none )

        PushGuest form ->
            ( model, actionRequest apis.gql form JoinAck, Cmd.none )

        PassedSlowLoadTreshold ->
            let
                labels =
                    ternary (model.labels == Loading) LoadingSlowly model.labels
            in
            ( { model | labels = labels }, Cmd.none, Cmd.none )

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        -- Data queries
        GotPath result ->
            let
                newModel =
                    { model | path_data = result }
            in
            case result of
                Success path ->
                    case path.root of
                        Just root ->
                            ( newModel, Cmd.none, send (UpdateSessionPath (Just path)) )

                        Nothing ->
                            let
                                nameid =
                                    List.head path.path |> Maybe.map (\p -> p.nameid) |> withDefault ""
                            in
                            ( newModel, queryLocalGraph apis.gql nameid GotPath2, Cmd.none )

                _ ->
                    ( newModel, Cmd.none, Cmd.none )

        GotPath2 result ->
            case model.path_data of
                Success prevPath ->
                    case result of
                        Success path ->
                            case path.root of
                                Just root ->
                                    let
                                        newPath =
                                            { prevPath | root = Just root, path = path.path ++ (List.tail prevPath.path |> withDefault []) }
                                    in
                                    ( { model | path_data = Success newPath }, Cmd.none, send (UpdateSessionPath (Just newPath)) )

                                Nothing ->
                                    let
                                        nameid =
                                            List.head path.path |> Maybe.map (\p -> p.nameid) |> withDefault ""

                                        newPath =
                                            { prevPath | path = path.path ++ (List.tail prevPath.path |> withDefault []) }
                                    in
                                    ( { model | path_data = Success newPath }, queryLocalGraph apis.gql nameid GotPath2, Cmd.none )

                        _ ->
                            ( model, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        GotLabels result ->
            let
                newModel =
                    { model | labels = result }
            in
            ( newModel, Cmd.none, Cmd.none )

        GotLabelsSub result ->
            let
                newModel =
                    { model | labels_sub = result }
            in
            ( newModel, Cmd.none, Cmd.none )

        ChangeMenuFocus menu ->
            let
                query =
                    queryBuilder
                        [ ( "m", menuEncoder menu ) ]
            in
            ( model, Cmd.none, Nav.pushUrl global.key (uriFromNameid SettingsBaseUri model.node_focus.nameid ++ "?" ++ query) )

        AddLabel ->
            ( { model | label_add = ternary (model.label_add == True) False True, label_edit = Nothing, colorPicker = ColorPicker.setColor Nothing model.colorPicker }
            , Cmd.none
            , Cmd.none
            )

        EditLabel label ->
            let
                f =
                    model.label_form

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
            ( { model | label_edit = Just label, label_form = newForm, label_add = False, colorPicker = ColorPicker.setColor (Dict.get "color" newForm.post) model.colorPicker }
            , Cmd.none
            , Cmd.none
            )

        CancelLabel ->
            let
                f =
                    model.label_form
            in
            ( { model | label_add = False, label_edit = Nothing, label_form = { f | post = Dict.empty }, label_result = NotAsked }, Cmd.none, Cmd.none )

        ChangeLabelPost field value ->
            let
                f =
                    model.label_form

                newForm =
                    { f | post = Dict.insert field value f.post }
            in
            ( { model | label_form = newForm }, Cmd.none, Cmd.none )

        SubmitAddLabel time ->
            ( { model | label_result = LoadingSlowly }, addOneLabel apis.gql model.label_form GotLabel, Cmd.none )

        SubmitEditLabel time ->
            ( { model | label_result = LoadingSlowly }, updateOneLabel apis.gql model.label_form GotLabel, Cmd.none )

        SubmitDeleteLabel id time ->
            let
                f =
                    model.label_form

                newForm =
                    { f | id = id }
            in
            ( { model | label_result_del = LoadingSlowly, label_form = newForm }, removeOneLabel apis.gql newForm GotLabelDel, Cmd.none )

        GotLabel result ->
            case doRefreshToken result model.refresh_trial of
                Authenticate ->
                    ( { model | label_result = NotAsked }, send (DoOpenAuthModal model.label_form.uctx), Cmd.none )

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
                    ( { model | label_result = result, labels = Success new, label_form = initLabelNodeForm global.session.user model.node_focus.nameid, label_add = False, label_edit = Nothing }
                    , Cmd.none
                    , Cmd.none
                    )

                NoAuth ->
                    case parseErr result of
                        DuplicateErr ->
                            ( { model | label_result = LoadingSlowly }, send (Submit SubmitEditLabel), Cmd.none )

                        _ ->
                            ( { model | label_result = result }, Cmd.none, Cmd.none )

        GotLabelDel result ->
            case doRefreshToken result model.refresh_trial of
                Authenticate ->
                    ( { model | label_result_del = NotAsked }, send (DoOpenAuthModal model.label_form.uctx), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep (Submit <| SubmitDeleteLabel model.label_form.id) 500, send UpdateUserToken )

                OkAuth label ->
                    let
                        d =
                            withMaybeData model.labels |> withDefault []

                        new =
                            List.filter (\x -> x.id /= model.label_form.id) d
                    in
                    ( { model | label_result_del = NotAsked, labels = Success new, label_form = initLabelNodeForm global.session.user model.node_focus.nameid, label_add = False, label_edit = Nothing }
                    , Cmd.none
                    , Cmd.none
                    )

                NoAuth ->
                    ( { model | label_result_del = result }, Cmd.none, Cmd.none )

        -- Join
        DoJoinOrga rootnameid ->
            case global.session.user of
                LoggedOut ->
                    ( { model | node_action = ActionAuthNeeded }
                    , send DoOpenModal
                    , Cmd.none
                    )

                LoggedIn uctx ->
                    ( { model | node_action = JoinOrga (JoinInit LoadingSlowly) }
                    , Cmd.batch [ fetchNode apis.gql rootnameid DoJoinOrga2, send DoOpenModal ]
                    , Cmd.none
                    )

        DoJoinOrga2 result ->
            case result of
                Success n ->
                    ( { model | node_action = JoinOrga (JoinInit LoadingSlowly) }
                    , send (Submit <| DoJoinOrga3 n)
                    , Cmd.none
                    )

                other ->
                    ( { model | node_action = JoinOrga (JoinInit result) }, Cmd.none, Cmd.none )

        DoJoinOrga3 node time ->
            let
                ( tid, bid ) =
                    node.source
                        |> Maybe.map (\b -> ( b.tension.id, b.id ))
                        |> withDefault ( "", "" )

                f =
                    initActionForm tid global.session.user

                form =
                    { f
                        | bid = "" -- do no set bid to pass the backend
                        , events_type = Just [ TensionEvent.UserJoin ]
                        , post =
                            Dict.fromList
                                [ ( "createdAt", fromTime time )
                                , ( "old", f.uctx.username )
                                , ( "new", node.nameid )
                                ]
                        , node = node
                    }
            in
            ( { model | node_action = JoinOrga (JoinValidation form LoadingSlowly) }
            , Cmd.batch [ send (PushGuest form), send DoOpenModal ]
            , Cmd.none
            )

        JoinAck result ->
            case model.node_action of
                JoinOrga (JoinValidation form _) ->
                    case doRefreshToken result model.refresh_trial of
                        Authenticate ->
                            ( model, send (DoOpenAuthModal form.uctx), Cmd.none )

                        RefreshToken i ->
                            ( { model | refresh_trial = i }, sendSleep (PushGuest form) 500, send UpdateUserToken )

                        OkAuth n ->
                            ( { model | node_action = JoinOrga (JoinValidation form result) }
                            , Cmd.none
                            , send UpdateUserToken
                            )

                        NoAuth ->
                            ( { model | node_action = JoinOrga (JoinValidation form result) }, Cmd.none, Cmd.none )

                default ->
                    ( model, Cmd.none, Cmd.none )

        -- Token Refresh
        DoOpenAuthModal uctx ->
            ( { model
                | modalAuth =
                    Active
                        { post = Dict.fromList [ ( "username", uctx.username ) ]
                        , result = RemoteData.NotAsked
                        }
              }
            , Cmd.none
            , Ports.open_auth_modal
            )

        DoCloseAuthModal link ->
            let
                cmd =
                    ternary (link /= "") (send (Navigate link)) Cmd.none
            in
            case model.node_action of
                JoinOrga _ ->
                    ( { model | modalAuth = Inactive }, Cmd.batch [ cmd, send (DoCloseModal { reset = True, link = "" }) ], Ports.close_auth_modal )

                _ ->
                    ( { model | modalAuth = Inactive }, cmd, Ports.close_auth_modal )

        ChangeAuthPost field value ->
            case model.modalAuth of
                Active form ->
                    let
                        newForm =
                            { form | post = Dict.insert field value form.post }
                    in
                    ( { model | modalAuth = Active newForm }, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        SubmitUser form ->
            ( model, login apis.auth form.post GotSignin, Cmd.none )

        GotSignin result ->
            case result of
                RemoteData.Success uctx ->
                    let
                        cmd =
                            case model.modalAuth of
                                Active f ->
                                    case Dict.get "msg" f.post of
                                        Just "GotOrga" ->
                                            sendSleep (Navigate (uriFromNameid OverviewBaseUri model.node_focus.rootnameid)) 500

                                        _ ->
                                            Cmd.none

                                _ ->
                                    Cmd.none
                    in
                    ( { model | modalAuth = Inactive }
                    , Cmd.batch [ send (DoCloseAuthModal ""), cmd ]
                    , send (UpdateUserSession uctx)
                    )

                other ->
                    case model.modalAuth of
                        Active form ->
                            ( { model | modalAuth = Active { form | result = result } }, Cmd.none, Cmd.none )

                        Inactive ->
                            ( model, Cmd.none, Cmd.none )

        SubmitKeyDown key ->
            case key of
                13 ->
                    let
                        form =
                            case model.modalAuth of
                                Active f ->
                                    f

                                Inactive ->
                                    UserAuthForm Dict.empty RemoteData.NotAsked
                    in
                    --ENTER
                    if isPostSendable [ "password" ] form.post then
                        ( model, send (SubmitUser form), Cmd.none )

                    else
                        ( model, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

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

        SelectLabelColor color ->
            let
                newPicker =
                    model.colorPicker
                        |> ColorPicker.setColor (Just color)
                        |> ColorPicker.close

                form =
                    model.label_form

                newForm =
                    { form | post = Dict.insert "color" color form.post }
            in
            ( { model | colorPicker = newPicker, label_form = newForm }, Cmd.none, Ports.click "body" )

        -- Common
        NoMsg ->
            ( model, Cmd.none, Cmd.none )

        LogErr err ->
            ( model, Ports.logErr err, Cmd.none )

        Navigate url ->
            ( model, Cmd.none, Nav.pushUrl global.key url )

        DoOpenModal ->
            ( { model | isModalActive = True }, Ports.open_modal "actionModal", Cmd.none )

        DoCloseModal data ->
            let
                gcmd =
                    if data.link /= "" then
                        send (Navigate data.link)

                    else
                        Cmd.none
            in
            ( { model | isModalActive = False }, gcmd, Ports.close_modal )

        ExpandRoles ->
            ( { model | helperBar = HelperBar.expand model.helperBar }, Cmd.none, Cmd.none )

        CollapseRoles ->
            ( { model | helperBar = HelperBar.collapse model.helperBar }, Cmd.none, Cmd.none )

        -- Confirm Modal
        DoModalConfirmOpen msg txts ->
            ( { model | modal_confirm = ModalConfirm.open msg txts model.modal_confirm }, Cmd.none, Cmd.none )

        DoModalConfirmClose _ ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, Cmd.none, Cmd.none )

        DoModalConfirmSend ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, send model.modal_confirm.msg, Cmd.none )

        -- Help
        HelpMsg msg ->
            let
                ( help, out ) =
                    Help.update apis msg model.help

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | help = help }, out.cmds |> List.map (\m -> Cmd.map HelpMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ _ =
    [ Ports.mcPD Ports.closeModalFromJs LogErr DoCloseModal
    , Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    , Ports.cancelColorFromJs (always CloseColor)
    ]
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        |> Sub.batch



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Settings · " ++ (String.join "/" <| LE.unique [ model.node_focus.rootnameid, model.node_focus.nameid |> String.split "#" |> List.reverse |> List.head |> withDefault "" ])
    , body =
        [ view_ global model
        , Help.view {} model.help |> Html.map HelpMsg
        , refreshAuthModal model.modalAuth { closeModal = DoCloseAuthModal, changePost = ChangeAuthPost, submit = SubmitUser, submitEnter = SubmitKeyDown }
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    let
        helperData =
            { onJoin = DoJoinOrga model.node_focus.rootnameid
            , onExpand = ExpandRoles
            , onCollapse = CollapseRoles
            , user = global.session.user
            , path_data = global.session.path_data
            , baseUri = SettingsBaseUri
            , data = model.helperBar
            }
    in
    div [ id "mainPane" ]
        [ HelperBar.view helperData
        , div [ class "columns is-centered" ]
            [ div [ class "column is-10-desktop is-10-widescreen is-9-fullhd" ]
                [ div [ class "section" ]
                    [ div [ class "columns" ]
                        [ div [ class "column is-2" ] [ viewSettingsMenu model ]
                        , div [ class "column is-10" ] [ viewSettingsContent model ]
                        ]
                    ]
                ]
            ]
        , setupActionModal model.isModalActive model.node_action
        ]


viewSettingsMenu : Model -> Html Msg
viewSettingsMenu model =
    nav [ id "menuSettings", class "menu" ]
        [ ul [ class "menu-list" ] <|
            (model.menuList
                |> List.map
                    (\x ->
                        li []
                            [ a [ onClickPD (ChangeMenuFocus x), target "_blank", classList [ ( "is-active", x == model.menuFocus ) ] ]
                                [ I.icon1 (menuToIcon x) (menuToString x) ]
                            ]
                    )
            )
        ]


viewSettingsContent : Model -> Html Msg
viewSettingsContent model =
    case model.menuFocus of
        LabelsMenu ->
            div []
                [ viewLabels model
                , viewLabelsSub model
                ]

        SecurityMenu ->
            div [] [ text "Work in progress" ]


viewLabels : Model -> Html Msg
viewLabels model =
    div [ id "labelsTable" ]
        [ h2 [ class "subtitle has-text-weight-semibold" ]
            [ textH T.labels
            , button [ class "button is-success is-pulled-right", onClick AddLabel ] [ textT T.newLabel ]
            , br [] []
            ]
        , case model.label_add of
            True ->
                viewLabelAddBox model

            False ->
                text ""
        , case model.labels of
            Success labels ->
                if List.length labels == 0 then
                    div [ class "" ] [ text "No label yet" ]

                else
                    table [ class "table is-fullwidth" ]
                        [ thead []
                            [ tr []
                                [ th [] [ textH T.name ]
                                , th [] [ textH T.description ]
                                , th [] [ text "" ]
                                , th [] []
                                ]
                            ]
                        , labels
                            |> List.indexedMap
                                (\i d ->
                                    [ tr [] <|
                                        if model.label_edit == Just d then
                                            [ td [ colspan 4 ] [ viewLabelAddBox model ] ]

                                        else
                                            let
                                                n_nodes =
                                                    d.n_nodes |> withDefault 0
                                            in
                                            [ td [] [ viewLabel "is-medium" (Label d.id d.name d.color) ]
                                            , td [ class "is-aligned-left" ] [ d.description |> withDefault "" |> text |> List.singleton |> span [] ]
                                            , td [ class "" ]
                                                [ if n_nodes > 1 then
                                                    span [ class "is-italic is-size-7" ] [ I.icon1 "icon-exclamation-circle" "Present in ", n_nodes |> String.fromInt |> text, text " circles." ]

                                                  else
                                                    text ""
                                                ]
                                            , td [ class "is-aligned-right is-size-7", attribute "style" "min-width: 6rem;" ]
                                                [ span [ class "button-light", onClick (EditLabel d) ] [ text "Edit" ]
                                                , text " · "
                                                , span
                                                    [ class "button-light"
                                                    , onClick <| DoModalConfirmOpen (Submit <| SubmitDeleteLabel d.id) [ ( T.confirmDeleteLabel, "" ), ( d.name, "is-strong" ), ( "?", "" ) ]
                                                    ]
                                                    [ text "Delete" ]
                                                ]
                                            ]
                                    ]
                                        ++ (case model.label_result_del of
                                                Failure err ->
                                                    [ td [] [ viewGqlErrors err ] ]

                                                _ ->
                                                    []
                                           )
                                )
                            |> List.concat
                            |> tbody []
                        ]

            Failure err ->
                viewGqlErrors err

            LoadingSlowly ->
                div [ class "spinner" ] []

            other ->
                text ""
        ]


viewLabelsSub : Model -> Html Msg
viewLabelsSub model =
    case model.labels_sub of
        RemoteData.Success labels ->
            if List.length labels == 0 then
                div [ class "mt-6" ] [ text "No sub-circle label yet" ]

            else
                div [ id "labelsTable" ]
                    [ h2 [ class "subtitle has-text-weight-semibold" ]
                        [ textT T.subLabels
                        ]
                    , table [ class "table is-fullwidth" ]
                        [ thead []
                            [ tr []
                                [ th [] [ textH T.name ]
                                , th [] [ textH T.description ]
                                , th [] [ text "" ]
                                , th [] []
                                ]
                            ]
                        , labels
                            |> List.indexedMap
                                (\i d ->
                                    [ tr [] <|
                                        if model.label_edit == Just d then
                                            [ td [ colspan 4 ] [ viewLabelAddBox model ] ]

                                        else
                                            let
                                                n_nodes =
                                                    d.n_nodes |> withDefault 0
                                            in
                                            [ td [] [ viewLabel "is-medium" (Label d.id d.name d.color) ]
                                            , td [ class "is-aligned-left" ] [ d.description |> withDefault "" |> text |> List.singleton |> span [] ]
                                            , td [ class "" ]
                                                [ if n_nodes > 1 then
                                                    span [ class "is-italic is-size-7" ] [ I.icon1 "icon-exclamation-circle" "Present in ", n_nodes |> String.fromInt |> text, text " circles." ]

                                                  else
                                                    text ""
                                                ]

                                            -- @debug: update and delete not implemented for sub circle
                                            , td [ class "is-aligned-right is-size-7", attribute "style" "min-width: 6rem;" ]
                                                [ span [ class "button-light" ] [ text "" ]
                                                , span [ class "button-light" ]
                                                    [ text "" ]
                                                ]
                                            ]
                                    ]
                                        ++ (case model.label_result_del of
                                                Failure err ->
                                                    [ td [] [ viewGqlErrors err ] ]

                                                _ ->
                                                    []
                                           )
                                )
                            |> List.concat
                            |> tbody []
                        ]
                    ]

        RemoteData.Failure err ->
            viewHttpErrors err

        RemoteData.Loading ->
            div [ class "spinner" ] []

        other ->
            text ""


viewLabelAddBox : Model -> Html Msg
viewLabelAddBox model =
    let
        form =
            model.label_form

        result =
            model.label_result

        name =
            Dict.get "name" form.post |> withDefault ""

        color =
            Dict.get "color" form.post

        description =
            Dict.get "description" form.post

        isLoading =
            model.label_result == LoadingSlowly

        isSendable =
            name /= ""

        txt =
            if model.label_add then
                { submit = T.createLabel }

            else
                -- assume edit label
                { submit = T.updateLabel }

        doSubmit =
            if model.label_add then
                ternary isSendable [ onClick (Submit <| SubmitAddLabel) ] []

            else
                -- assume edit label
                ternary isSendable [ onClick (Submit <| SubmitEditLabel) ] []

        doCancel =
            CancelLabel
    in
    div [ class "box has-background-light" ]
        [ div [ class "field is-grouped" ]
            [ p [ class "control" ]
                [ label [ class "label is-small" ] [ text "Label name *" ]
                , input
                    [ id "titleInput"
                    , class "input"
                    , type_ "text"
                    , placeholder "Label name"
                    , value name
                    , onInput (ChangeLabelPost "name")
                    ]
                    []
                ]
            , p [ class "control" ]
                [ label [ class "label is-small" ] [ text "Color" ]
                , ColorPicker.view { data = model.colorPicker, onOpen = OpenColor, onClose = CloseColor, onSelect = SelectLabelColor }
                ]
            , p [ class "control is-expanded" ]
                [ label [ class "label is-small" ] [ text "Description" ]
                , input
                    [ id "titleInput"
                    , class "input"
                    , type_ "text"
                    , placeholder "Description"
                    , value (withDefault "" description)
                    , onInput (ChangeLabelPost "description")
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
            [ span [ class "help-label", attribute "style" "display:initial !important;" ] [ text "Preview: " ]
            , viewLabel "" (Label "" (ternary (name == "") "label name" name) color)
            ]
        , case result of
            Failure err ->
                viewGqlErrors err

            _ ->
                text ""
        ]



-- Actions


setupActionModal : Bool -> ActionState -> Html Msg
setupActionModal isModalActive action =
    div
        [ id "actionModal"
        , class "modal modal-fx-fadeIn"
        , classList [ ( "is-active", isModalActive ) ]
        , attribute "data-modal-close" "closeModalFromJs"
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "actionModal"
            , onClick (DoCloseModal { reset = True, link = "" })
            ]
            []
        , div [ class "modal-content" ]
            [ case action of
                JoinOrga step ->
                    viewJoinOrgaStep step

                NoOp ->
                    text ""

                AskErr err ->
                    viewGqlErrors [ err ]

                ActionAuthNeeded ->
                    viewAuthNeeded DoCloseModal
            ]
        , button [ class "modal-close is-large", onClick (DoCloseModal { reset = True, link = "" }) ] []
        ]


viewJoinOrgaStep : JoinStep ActionForm -> Html Msg
viewJoinOrgaStep step =
    case step of
        JoinInit _ ->
            div [ class "box spinner" ] [ text "" ]

        JoinNotAuthorized errMsg ->
            viewGqlErrors errMsg

        JoinValidation form result ->
            case result of
                Success _ ->
                    div [ class "box is-light", onClick (DoCloseModal { reset = True, link = "" }) ]
                        [ I.icon1 "icon-check icon-2x has-text-success" " "
                        , textH T.welcomIn
                        , text " "
                        , span [ class "has-font-weight-semibold" ] [ text form.node.name ]
                        ]

                Failure err ->
                    viewGqlErrors err

                default ->
                    div [ class "box spinner" ] [ text "" ]
