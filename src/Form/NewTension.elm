module Form.NewTension exposing (..)

import Auth exposing (AuthState(..), doRefreshToken)
import Codecs exposing (LookupResult)
import Components.LabelSearchPanel as LabelSearchPanel
import Components.Loading as Loading exposing (ErrorData, GqlData, ModalData, RequestResult(..), viewAuthNeeded, viewGqlErrors, viewRoleNeeded, withDefaultData, withMaybeData)
import Components.Markdown exposing (renderMarkdown)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm)
import Components.NodeDoc as NodeDoc exposing (nodeAboutInputView, nodeLinksInputView, nodeMandateInputView)
import Dict
import Extra exposing (ternary)
import Extra.Events exposing (onClickPD, onClickPD2, onEnter, onKeydown, onTab)
import Form exposing (isPostEmpty, isPostSendable)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (toHref)
import Global exposing (Msg(..), send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, li, nav, option, p, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, placeholder, required, rows, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Icon as I
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (Apis, GlobalCmd(..), InputViewMode(..), TensionForm, UserState(..), getParentFragmentFromRole, initTensionForm)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), NodeFocus, getOrgaRoles, nodeFromFocus, nodeIdCodec)
import ModelCommon.View exposing (action2SourceStr, edgeArrow, getNodeTextFromNodeType, getTensionText, roleColor, tensionTypeColor)
import ModelSchema exposing (..)
import Ports
import Query.AddTension exposing (addOneTension)
import Text as T exposing (textH, textT, upH)
import Time


type State
    = State Model


type alias Model =
    { user : UserState
    , form : TensionForm
    , result : GqlData Tension
    , sources : List UserRole
    , step : TensionStep
    , isModalActive : Bool
    , activeTab : TensionTab
    , viewMode : InputViewMode
    , activeButton : Maybe Int
    , isLookupOpen : Bool
    , doAddLinks : Bool
    , doAddResponsabilities : Bool
    , doAddDomains : Bool
    , doAddPolicies : Bool
    , labelsPanel : LabelSearchPanel.State
    , lookup_users : List User

    -- Common
    , refresh_trial : Int
    , modal_confirm : ModalConfirm Msg
    }


type TensionTab
    = NewTensionTab
    | NewCircleTab


type TensionStep
    = TensionTypes
    | TensionSource
    | TensionFinal
    | TensionNotAuthorized ErrorData
    | AuthNeeded


init : UserState -> State
init user =
    initModel user |> State


initModel : UserState -> Model
initModel user =
    { user = user
    , form = initTensionForm user
    , result = NotAsked
    , sources = []
    , step = TensionFinal
    , isModalActive = False
    , activeTab = NewTensionTab
    , viewMode = Write
    , activeButton = Nothing
    , isLookupOpen = False
    , doAddLinks = False
    , doAddResponsabilities = False
    , doAddDomains = False
    , doAddPolicies = False
    , labelsPanel = LabelSearchPanel.init "" user
    , lookup_users = []

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }


initTensionTab : Model -> Model
initTensionTab model =
    let
        form =
            model.form

        newForm =
            { form
                | tension_type = TensionType.Operational
                , action = Nothing
                , blob_type = Nothing
                , node = initNodeFragment Nothing
                , users = []
            }
    in
    { model | activeTab = NewTensionTab, form = newForm }


initCircleTab : Model -> Model
initCircleTab model =
    let
        form =
            model.form

        node =
            initNodeFragment (Just NodeType.Role)

        newForm =
            { form
                | tension_type = TensionType.Governance
                , action = Just TensionAction.NewRole
                , blob_type = Just BlobType.OnNode
                , node = { node | charac = Just model.form.target.charac } -- inherit charac
                , users = [ { username = "", role_type = RoleType.Peer, pattern = "" } ]
            }
    in
    { model | activeTab = NewCircleTab, form = newForm }



-- Global methods


setTarget_ : Node -> Maybe NodeData -> State -> State
setTarget_ target node_data (State model) =
    State (setTarget target node_data model)


setTab_ : TensionTab -> State -> State
setTab_ tab (State model) =
    State { model | activeTab = tab }


getTargetid : State -> String
getTargetid (State model) =
    model.form.target.nameid



--- State Controls


open : Model -> Model
open data =
    { data | isModalActive = True }


switchTab : TensionTab -> Model -> Model
switchTab tab model =
    case tab of
        NewTensionTab ->
            initTensionTab model

        NewCircleTab ->
            initCircleTab model


close : Model -> Model
close data =
    { data | isModalActive = False }


setActiveButton : Bool -> Model -> Model
setActiveButton doClose data =
    if doClose then
        { data | activeButton = Just 0 }

    else
        { data | activeButton = Just 1 }


setViewMode : InputViewMode -> Model -> Model
setViewMode viewMode data =
    { data | viewMode = viewMode }


setStep : TensionStep -> Model -> Model
setStep step data =
    { data | step = step }


setForm : TensionForm -> Model -> Model
setForm form data =
    { data | form = form }


setResult : GqlData Tension -> Model -> Model
setResult result data =
    { data | result = result }


addLinks : Model -> Model
addLinks data =
    { data | doAddLinks = True }


addResponsabilities : Model -> Model
addResponsabilities data =
    { data | doAddResponsabilities = True }


addDomains : Model -> Model
addDomains data =
    { data | doAddDomains = True }


addPolicies : Model -> Model
addPolicies data =
    { data | doAddPolicies = True }


hasData : Model -> Bool
hasData data =
    isPostEmpty [ "title", "message" ] data.form.post == False



-- Update Form


setUctx : UserCtx -> Model -> Model
setUctx uctx data =
    let
        form =
            data.form

        newForm =
            { form | uctx = uctx }
    in
    { data | form = newForm }


setSources : List UserRole -> Model -> Model
setSources sources data =
    let
        form =
            data.form

        newForm =
            { form | source = List.head sources |> withDefault form.source }
    in
    { data | sources = sources, form = newForm }


setTensionType : TensionType.TensionType -> Model -> Model
setTensionType type_ data =
    let
        form =
            data.form

        newForm =
            { form | tension_type = type_ }
    in
    { data | form = newForm }


setSource : UserRole -> Model -> Model
setSource source data =
    let
        f =
            data.form

        newForm =
            { f | source = source }
    in
    { data | form = newForm }


setTarget : Node -> Maybe NodeData -> Model -> Model
setTarget target node_data data =
    let
        f =
            data.form

        newForm =
            { f | target = target, targetData = node_data |> withDefault initNodeData }
    in
    { data | form = newForm }


setTargetShort : NodeFocus -> Maybe NodeData -> Model -> Model
setTargetShort focus node_data data =
    let
        f =
            data.form

        newForm =
            { f | target = nodeFromFocus focus, targetData = node_data |> withDefault initNodeData }
    in
    { data | form = newForm }


setStatus : TensionStatus.TensionStatus -> Model -> Model
setStatus status data =
    let
        f =
            data.form

        newForm =
            { f | status = status }
    in
    { data | form = newForm }


setEvents : List TensionEvent.TensionEvent -> Model -> Model
setEvents events data =
    let
        f =
            data.form

        newForm =
            { f | events_type = Just events }
    in
    { data | form = newForm }


setLabels : List Label -> Model -> Model
setLabels labels data =
    let
        f =
            data.form

        newForm =
            { f | labels = labels }
    in
    { data | form = newForm }


addLabel : Label -> Model -> Model
addLabel label data =
    let
        f =
            data.form

        newForm =
            { f | labels = f.labels ++ [ label ] }
    in
    { data | form = newForm }


removeLabel : Label -> Model -> Model
removeLabel label data =
    let
        f =
            data.form

        newForm =
            { f | labels = LE.remove label f.labels }
    in
    { data | form = newForm }


post : String -> String -> Model -> Model
post field value data =
    let
        f =
            data.form

        newForm =
            { f | post = Dict.insert field value f.post }
    in
    { data | form = newForm }


postNode : String -> String -> Model -> Model
postNode field value data =
    { data | form = NodeDoc.updateNodeForm field value data.form }


resetPost : Model -> Model
resetPost data =
    let
        f =
            data.form

        newForm =
            { f | post = Dict.empty }
    in
    { data | form = newForm }


resetModel : Model -> Model
resetModel data =
    initModel data.user



-- User Lookup


updateUserPattern : Int -> String -> Model -> Model
updateUserPattern pos pattern data =
    let
        f =
            data.form

        newForm =
            { f | users = NodeDoc.updateUserPattern_ pos pattern f.users }
    in
    { data | form = newForm }


updateUserRole : Int -> String -> Model -> Model
updateUserRole pos role data =
    let
        f =
            data.form

        newForm =
            { f | users = NodeDoc.updateUserRole_ pos role f.users }
    in
    { data | form = newForm }


selectUser : Int -> String -> Model -> Model
selectUser pos username data =
    let
        f =
            data.form

        newForm =
            { f | users = NodeDoc.selectUser_ pos username f.users }
    in
    { data | form = newForm, isLookupOpen = False }


cancelUser : Int -> Model -> Model
cancelUser pos data =
    let
        f =
            data.form

        newForm =
            { f | users = NodeDoc.cancelUser_ pos f.users }
    in
    { data | form = newForm, isLookupOpen = False }


openLookup : Model -> Model
openLookup data =
    { data | isLookupOpen = True }


closeLookup : Model -> Model
closeLookup data =
    { data | isLookupOpen = False }


canExitSafe : Model -> Bool
canExitSafe data =
    hasData data && withMaybeData data.result == Nothing



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = -- Data control
      PushTension (GqlData Tension -> Msg)
    | OnSubmit (Time.Posix -> Msg)
      -- Modal control
    | OnOpen
    | OnResetModel
    | OnClose ModalData
    | OnCloseSafe String String
    | OnChangeInputViewMode InputViewMode
    | OnTensionStep TensionStep
    | OnSwitchTab TensionTab
      -- Doc change
    | OnChangeTensionType TensionType.TensionType
    | OnChangePost String String
    | OnAddLinks
    | OnAddDomains
    | OnAddPolicies
    | OnAddResponsabilities
    | OnSubmitTension Bool Time.Posix
    | OnTensionAck (GqlData Tension)
      -- User Quick Search
    | OnChangeUserPattern Int String
    | OnChangeUserRole Int String
    | OnChangeUserLookup (LookupResult User)
    | OnSelectUser Int String
    | OnCancelUser Int
    | OnShowLookupFs
    | OnCancelLookupFs
      -- Labels
    | LabelSearchPanelMsg LabelSearchPanel.Msg
      -- Confirm Modal
    | DoModalConfirmOpen Msg (List ( String, String ))
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Common
    | NoMsg
    | LogErr String


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe Bool
    }


noOut : Out
noOut =
    Out [] [] Nothing


out1 : List (Cmd Msg) -> Out
out1 cmds =
    Out cmds [] Nothing


out2 : List GlobalCmd -> Out
out2 cmds =
    Out [] cmds Nothing


update : Apis -> Msg -> State -> ( State, Out )
update apis message (State model) =
    update_ apis message model
        |> Tuple.mapFirst State


update_ : Apis -> Msg -> Model -> ( Model, Out )
update_ apis message model =
    case message of
        -- Data control
        PushTension ack ->
            ( model, out1 [ addOneTension apis.gql model.form ack ] )

        OnSubmit next ->
            ( model, out1 [ sendNow next ] )

        -- Modal control
        OnOpen ->
            case model.user of
                LoggedIn uctx ->
                    let
                        sources =
                            getOrgaRoles model.form.uctx.roles [ model.form.target.rootnameid ]
                    in
                    if sources == [] && model.refresh_trial == 0 then
                        ( { model | refresh_trial = 1 }, Out [ sendSleep OnOpen 500 ] [ DoUpdateToken ] Nothing )

                    else if sources == [] then
                        ( setStep (TensionNotAuthorized [ T.notOrgMember, T.joinForTension ]) model, noOut )

                    else
                        ( model |> setUctx uctx |> setSources sources |> open
                        , out1 [ Ports.open_modal "tensionModal" ]
                        )

                LoggedOut ->
                    ( setStep AuthNeeded model, noOut )

        OnClose data ->
            let
                cmds =
                    ternary data.reset [ sendSleep OnResetModel 333 ] []

                gcmds =
                    ternary (data.link /= "") [ DoNavigate data.link ] []
            in
            ( close model, Out ([ Ports.close_modal ] ++ cmds) gcmds Nothing )

        OnResetModel ->
            ( resetModel model, noOut )

        OnCloseSafe link onCloseTxt ->
            let
                doClose =
                    canExitSafe model
            in
            if doClose then
                ( model
                , out1 [ send (DoModalConfirmOpen (OnClose { reset = True, link = link }) [ ( upH T.confirmUnsaved, onCloseTxt ) ]) ]
                )

            else
                ( model, out1 [ send (OnClose { reset = True, link = link }) ] )

        OnChangeInputViewMode viewMode ->
            ( setViewMode viewMode model, noOut )

        OnTensionStep step ->
            ( setStep step model, out1 [ Ports.bulma_driver "tensionModal" ] )

        OnSwitchTab tab ->
            ( switchTab tab model, out1 [ Ports.bulma_driver "tensionModal" ] )

        -- Doc change
        OnChangeTensionType type_ ->
            ( setTensionType type_ model, noOut )

        OnChangePost field value ->
            case model.activeTab of
                NewTensionTab ->
                    ( post field value model, noOut )

                NewCircleTab ->
                    ( postNode field value model, noOut )

        OnAddLinks ->
            ( addLinks model, noOut )

        OnAddDomains ->
            ( addDomains model, noOut )

        OnAddPolicies ->
            ( addPolicies model, noOut )

        OnAddResponsabilities ->
            ( addResponsabilities model, noOut )

        OnSubmitTension doClose time ->
            let
                events =
                    case model.activeTab of
                        NewTensionTab ->
                            [ TensionEvent.Created ]

                        NewCircleTab ->
                            if doClose == True then
                                [ TensionEvent.Created, TensionEvent.BlobCreated, TensionEvent.BlobPushed ]

                            else
                                [ TensionEvent.Created, TensionEvent.BlobCreated ]
            in
            ( model
                |> post "createdAt" (fromTime time)
                |> setEvents events
                |> setStatus (ternary (doClose == True) TensionStatus.Closed TensionStatus.Open)
                |> setActiveButton doClose
                |> setResult LoadingSlowly
            , out1 [ send (PushTension OnTensionAck) ]
            )

        OnTensionAck result ->
            case doRefreshToken result model.refresh_trial of
                Authenticate ->
                    ( setResult NotAsked model
                    , out2 [ DoAuth model.form.uctx ]
                    )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, Out [ sendSleep (PushTension OnTensionAck) 500 ] [ DoUpdateToken ] Nothing )

                OkAuth tension ->
                    case model.activeTab of
                        NewTensionTab ->
                            ( setResult result model, out2 [ DoPushTension tension ] )

                        NewCircleTab ->
                            let
                                newNameid =
                                    model.form.node.nameid
                                        |> Maybe.map (\nid -> nodeIdCodec model.form.target.nameid nid (withDefault NodeType.Role model.form.node.type_))
                                        |> withDefault ""
                            in
                            ( setResult result model, out2 [ DoFetchNode newNameid ] )

                NoAuth ->
                    ( setResult result model, noOut )

        -- User Quick Search
        OnChangeUserPattern pos pattern ->
            ( updateUserPattern pos pattern model
            , out1 [ Ports.searchUser pattern ]
            )

        OnChangeUserRole pos role ->
            ( updateUserRole pos role model, noOut )

        OnChangeUserLookup users_ ->
            case users_ of
                Ok users ->
                    ( { model | lookup_users = users }, noOut )

                Err err ->
                    ( model, out1 [ Ports.logErr err ] )

        OnSelectUser pos username ->
            ( selectUser pos username model, noOut )

        OnCancelUser pos ->
            ( cancelUser pos model, noOut )

        OnShowLookupFs ->
            ( openLookup model
            , if model.isLookupOpen == False then
                out1 [ Ports.outsideClickClose "cancelLookupFsFromJs" "userSearchPanel" ]

              else
                noOut
            )

        OnCancelLookupFs ->
            ( closeLookup model, noOut )

        -- Labels
        LabelSearchPanelMsg msg ->
            let
                ( panel, out ) =
                    LabelSearchPanel.update apis msg model.labelsPanel

                newModel =
                    Maybe.map
                        (\r ->
                            if Tuple.first r == True then
                                addLabel (Tuple.second r) model

                            else
                                removeLabel (Tuple.second r) model
                        )
                        out.result
                        |> withDefault model
            in
            ( { newModel | labelsPanel = panel }, Out (out.cmds |> List.map (\m -> Cmd.map LabelSearchPanelMsg m)) out.gcmds Nothing )

        -- Confirm Modal
        DoModalConfirmOpen msg txts ->
            ( { model | modal_confirm = ModalConfirm.open msg txts model.modal_confirm }, noOut )

        DoModalConfirmClose _ ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, noOut )

        DoModalConfirmSend ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, out1 [ send model.modal_confirm.msg ] )

        -- Common
        NoMsg ->
            ( model, noOut )

        LogErr err ->
            ( model, out1 [ Ports.logErr err ] )


subscriptions =
    [ Ports.lookupUserFromJs OnChangeUserLookup
    , Ports.cancelLookupFsFromJs (always OnCancelLookupFs)
    , Ports.mcPD Ports.closeModalTensionFromJs LogErr OnClose
    , Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    ]
        ++ (LabelSearchPanel.subscriptions |> List.map (\s -> Sub.map LabelSearchPanelMsg s))



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    { users_data : GqlData UsersData
    , targets : List String
    }


view : Op -> State -> Html Msg
view op (State model) =
    div []
        [ viewModal op (State model)
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        ]


viewModal : Op -> State -> Html Msg
viewModal op_ (State model) =
    let
        op =
            { op_ | targets = op_.targets ++ [ model.form.target.nameid, model.form.source.nameid ] }
    in
    div
        [ id "tensionModal"
        , class "modal modal-fx-fadeIn"
        , classList [ ( "is-active", model.isModalActive ), ( "fixed-top", withMaybeData model.result == Nothing ) ]
        , attribute "data-modal-close" "closeModalTensionFromJs"
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "tensionModal"
            , onClick (OnCloseSafe "" "")
            ]
            []
        , div [ class "modal-content" ] [ viewStep op (State model) ]
        , button [ class "modal-close is-large", onClick (OnCloseSafe "" "") ] []
        ]


viewStep : Op -> State -> Html Msg
viewStep op (State model) =
    case model.step of
        TensionTypes ->
            div [ class "modal-card" ]
                [ div [ class "modal-card-head" ]
                    [ span [ class "has-text-weight-medium" ] [ text "Choose the type of tension to communicate:" ] ]
                , div [ class "modal-card-body" ]
                    [ div [ class "level buttonRadio" ] <|
                        List.map
                            (\tensionType ->
                                div [ class "level-item" ]
                                    [ div
                                        [ class <| "button " ++ tensionTypeColor "background" tensionType
                                        , onClick (OnTensionStep TensionSource)
                                        ]
                                        [ TensionType.toString tensionType |> text ]
                                    ]
                            )
                            TensionType.list
                    ]
                ]

        TensionSource ->
            viewSources (State model) TensionFinal

        TensionFinal ->
            case model.activeTab of
                NewTensionTab ->
                    viewTension op (State model)

                NewCircleTab ->
                    viewCircle op (State model)

        TensionNotAuthorized errMsg ->
            viewRoleNeeded errMsg

        AuthNeeded ->
            viewAuthNeeded OnClose


viewSources : State -> TensionStep -> Html Msg
viewSources (State model) nextStep =
    div [ class "modal-card" ]
        [ div [ class "modal-card-head" ]
            [ span [ class "has-text-weight-medium" ] [ "You have several roles in this organisation. Please select the role from which you want to " ++ action2SourceStr model.form.action |> text ] ]
        , div [ class "modal-card-body" ]
            [ div [ class "buttons buttonRadio", attribute "style" "margin-bottom: 2em; margin-right: 2em; margin-left: 2em;" ] <|
                List.map
                    (\r ->
                        button
                            [ class ("button buttonRole tooltip has-tooltip-bottom is-" ++ roleColor r.role_type)
                            , attribute "data-tooltip" ([ r.name, "of", getParentFragmentFromRole r ] |> String.join " ")
                            , onClick (OnTensionStep nextStep)
                            ]
                            [ text r.name ]
                    )
                    model.sources
            ]
        ]


viewTensionTabs : TensionTab -> Node -> Html Msg
viewTensionTabs tab targ =
    div [ id "tensionTabTop", class "tabs is-boxed has-text-weight-medium" ]
        [ ul []
            [ li [ classList [ ( "is-active", tab == NewTensionTab ) ] ] [ a [ onClickPD (OnSwitchTab NewTensionTab), target "blank_" ] [ I.icon1 "icon-exchange" "Tension" ] ]
            , if targ.type_ == NodeType.Circle then
                li [ classList [ ( "is-active", tab == NewCircleTab ) ] ] [ a [ onClickPD (OnSwitchTab NewCircleTab), target "blank_" ] [ I.icon1 "icon-target" "Role or Circle" ] ]

              else
                text ""
            ]
        ]



---
--- Final Views
---


viewTension : Op -> State -> Html Msg
viewTension op (State model) =
    let
        form =
            model.form

        title =
            Dict.get "title" form.post |> withDefault ""

        message =
            Dict.get "message" form.post |> withDefault ""

        txt =
            getTensionText

        isLoading =
            model.result == LoadingSlowly

        isSendable =
            isPostSendable [ "title" ] form.post

        submitTension =
            ternary isSendable [ onClick (OnSubmit <| OnSubmitTension False) ] []
    in
    case model.result of
        Success res ->
            let
                link =
                    Route.Tension_Dynamic_Dynamic { param1 = form.target.rootnameid, param2 = res.id } |> toHref
            in
            div [ class "box is-light" ]
                [ I.icon1 "icon-check icon-2x has-text-success" " "
                , textH txt.added
                , text " "
                , a
                    [ href link
                    , onClickPD (OnClose { reset = True, link = link })
                    , target "_blank"
                    ]
                    [ textH T.checkItOut ]
                ]

        other ->
            div [ class "panel modal-card" ]
                [ div [ class "panel-heading" ]
                    [ div [ class "level modal-card-title" ]
                        [ div [ class "level-left" ] <|
                            List.intersperse (text "\u{00A0}")
                                [ span [ class "is-size-6 has-text-weight-semibold has-text-grey" ]
                                    [ textT txt.title
                                    , span [ class "has-text-weight-medium" ] [ text " | " ]
                                    , span [ class "dropdown" ]
                                        [ span [ class "dropdown-trigger button-light" ]
                                            [ span [ attribute "aria-controls" "type-menu" ]
                                                [ span [ class <| "has-text-weight-medium " ++ tensionTypeColor "text" form.tension_type ]
                                                    [ text (TensionType.toString form.tension_type), span [ class "ml-2 arrow down" ] [] ]
                                                ]
                                            ]
                                        , div [ id "type-menu", class "dropdown-menu", attribute "role" "menu" ]
                                            [ div [ class "dropdown-content" ] <|
                                                List.map
                                                    (\t ->
                                                        div
                                                            [ class <| "dropdown-item button-light " ++ tensionTypeColor "text" t
                                                            , onClick (OnChangeTensionType t)
                                                            ]
                                                            [ TensionType.toString t |> text ]
                                                    )
                                                    TensionType.list
                                            ]
                                        ]
                                    ]
                                ]
                        , div [ class "level-right has-text-weight-medium" ] <| edgeArrow "button" (text form.source.name) (text form.target.name)
                        ]
                    ]
                , viewTensionTabs model.activeTab model.form.target
                , div [ class "modal-card-body" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ input
                                [ class "input autofocus followFocus"
                                , attribute "data-nextfocus" "textAreaModal"
                                , type_ "text"
                                , placeholder (upH T.title)
                                , required True
                                , value title
                                , onInput (OnChangePost "title")
                                ]
                                []
                            ]
                        , p [ class "help-label" ] [ textH txt.name_help ]
                        , br [] []
                        ]
                    , div [ class "message" ]
                        [ div [ class "message-header" ]
                            [ div [ class "tabs is-boxed is-small" ]
                                [ ul []
                                    [ li [ classList [ ( "is-active", model.viewMode == Write ) ] ]
                                        [ a [ onClickPD2 (OnChangeInputViewMode Write), target "_blank" ] [ text "Write" ] ]
                                    , li
                                        [ classList [ ( "is-active", model.viewMode == Preview ) ] ]
                                        [ a [ onClickPD2 (OnChangeInputViewMode Preview), target "_blank" ] [ text "Preview" ] ]
                                    ]
                                ]
                            ]
                        , div [ class "message-body" ]
                            [ div [ class "field" ]
                                [ div [ class "control" ]
                                    [ case model.viewMode of
                                        Write ->
                                            textarea
                                                [ id "textAreaModal"
                                                , class "textarea"
                                                , rows 5
                                                , placeholder (upH T.leaveCommentOpt)
                                                , value message
                                                , onInput (OnChangePost "message")
                                                ]
                                                []

                                        Preview ->
                                            div [] [ renderMarkdown "is-dark mt-4 mx-3" message, hr [] [] ]
                                    ]
                                , p [ class "help-label" ] [ textH txt.message_help ]
                                , br [] []
                                ]
                            ]
                        ]
                    , div [ class "field" ]
                        [ div [ class "control" ]
                            [ LabelSearchPanel.viewNew
                                { selectedLabels = form.labels
                                , targets = op.targets
                                , isAdmin = False
                                , exitSafe = canExitSafe model
                                }
                                model.labelsPanel
                                |> Html.map LabelSearchPanelMsg
                            ]
                        ]
                    ]
                , div [ class "modal-card-foot", attribute "style" "display: block;" ]
                    [ case other of
                        Failure err ->
                            viewGqlErrors err

                        _ ->
                            text ""
                    , div [ class "field is-grouped is-grouped-right" ]
                        [ div [ class "control" ]
                            [ div [ class "buttons" ]
                                [ button
                                    ([ class "button is-success"
                                     , classList [ ( "is-loading", isLoading ) ]
                                     , disabled (not isSendable)
                                     ]
                                        ++ submitTension
                                    )
                                    [ textH txt.submit ]
                                ]
                            ]
                        ]
                    ]
                ]


viewCircle : Op -> State -> Html Msg
viewCircle op (State model) =
    let
        form =
            model.form

        txt =
            getNodeTextFromNodeType (form.node.type_ |> withDefault NodeType.Role)

        isLoading =
            model.result == LoadingSlowly

        isSendable =
            form.node.name /= Nothing && (form.node.mandate |> Maybe.map (\x -> x.purpose)) /= Nothing

        submitTension =
            ternary isSendable [ onClickPD2 (OnSubmit <| OnSubmitTension False) ] []

        submitCloseTension =
            ternary isSendable [ onClickPD2 (OnSubmit <| OnSubmitTension True) ] []
    in
    case model.result of
        Success res ->
            let
                link =
                    Route.Tension_Dynamic_Dynamic_Action { param1 = form.target.rootnameid, param2 = res.id } |> toHref
            in
            div [ class "box is-light" ]
                [ I.icon1 "icon-check icon-2x has-text-success" " "
                , if model.activeButton == Just 0 then
                    textH txt.added

                  else
                    textH txt.tension_added
                , text " "
                , a
                    [ href link
                    , onClickPD (OnClose { reset = True, link = link })
                    , target "_blank"
                    ]
                    [ textH T.checkItOut ]
                ]

        other ->
            let
                message =
                    Dict.get "message" form.post |> withDefault ""

                nameid =
                    form.node.nameid |> withDefault ""

                --@Debug: NodeDoc has not its full State yet
                op_ =
                    { data = model
                    , lookup = model.lookup_users
                    , users_data = op.users_data
                    , targets = op.targets
                    , onChangePost = OnChangePost
                    , onAddLinks = OnAddLinks
                    , onAddDomains = OnAddDomains
                    , onAddPolicies = OnAddPolicies
                    , onAddResponsabilities = OnAddResponsabilities
                    , onChangeUserRole = OnChangeUserRole
                    , onSelectUser = OnSelectUser
                    , onCancelUser = OnCancelUser
                    , onShowLookupFs = OnShowLookupFs
                    , onChangeUserPattern = OnChangeUserPattern
                    }
            in
            div [ class "panel modal-card" ]
                [ div [ class "panel-heading" ]
                    [ div [ class "level modal-card-title" ]
                        [ div [ class "level-left" ] <|
                            List.intersperse (text "\u{00A0}")
                                [ span [ class "is-size-6 has-text-weight-semibold has-text-grey" ]
                                    [ textT txt.title
                                    , span [ class "has-text-weight-medium" ] [ text " | " ]
                                    , span
                                        [ class <| "has-text-weight-medium " ++ tensionTypeColor "text" form.tension_type ]
                                        [ text (TensionType.toString form.tension_type) ]
                                    ]
                                ]
                        , div [ class "level-right has-text-weight-medium" ] <| edgeArrow "button" (text form.source.name) (text form.target.name)
                        ]
                    ]
                , viewTensionTabs model.activeTab model.form.target
                , div [ class "modal-card-body" ]
                    [ nodeAboutInputView False OverviewBaseUri txt form.node op_
                    , div [ class "card cardForm" ]
                        [ div [ class "has-text-black is-aligned-center", attribute "style" "background-color: #e1e1e1;" ] [ textH T.mandate ]
                        , div [ class "card-content" ]
                            [ nodeMandateInputView txt form.node op_ ]
                        ]
                    , if model.doAddLinks || (form.users |> List.filter (\u -> u.username /= "")) /= [] then
                        div
                            [ class "card cardForm"
                            , attribute "style" "overflow: unset;"
                            ]
                            --[ div [ class "card-header" ] [ div [ class "card-header-title" ] [ textH T.firstLink ] ]
                            [ div [ class "has-text-black is-aligned-center", attribute "style" "background-color: #e1e1e1;" ] [ textH T.firstLink ]
                            , div [ class "card-content" ] [ nodeLinksInputView txt form model op_ ]
                            ]

                      else
                        div [ class "field" ]
                            [ div [ class "button is-info", onClick OnAddLinks ]
                                [ I.icon1 "icon-plus" "", text "Add first link" ]
                            ]
                    , br [] []
                    , div [ class "field" ]
                        [ div [ class "control" ]
                            [ textarea
                                [ class "textarea"
                                , rows 3
                                , placeholder (upH T.leaveCommentOpt)
                                , value message
                                , onInput <| OnChangePost "message"
                                ]
                                []
                            ]
                        , p [ class "help-label" ] [ textH txt.message_help ]
                        ]
                    , br [] []
                    ]
                , div [ class "modal-card-foot", attribute "style" "display: block;" ]
                    [ case other of
                        Failure err ->
                            viewGqlErrors err

                        _ ->
                            text ""
                    , div [ class "field is-grouped is-grouped-right" ]
                        [ div [ class "control" ]
                            [ div [ class "buttons" ]
                                [ button
                                    ([ class "button is-warning"
                                     , classList
                                        [ ( "is-loading", isLoading && model.activeButton == Just 1 ) ]
                                     , disabled (not isSendable || isLoading)
                                     ]
                                        ++ submitTension
                                    )
                                    [ textH txt.submit ]
                                , button
                                    ([ class "button is-success"
                                     , classList
                                        [ ( "is-loading", isLoading && model.activeButton == Just 0 ) ]
                                     , disabled (not isSendable || isLoading)
                                     ]
                                        ++ submitCloseTension
                                    )
                                    [ textH txt.close_submit ]
                                ]
                            ]
                        ]
                    ]
                ]
