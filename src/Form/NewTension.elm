module Form.NewTension exposing (..)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Codecs exposing (LookupResult)
import Components.LabelSearchPanel as LabelSearchPanel
import Components.Loading as Loading exposing (ErrorData, GqlData, ModalData, RequestResult(..), viewAuthNeeded, viewGqlErrors, viewRoleNeeded, withDefaultData, withMaybeData)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.NodeDoc as NodeDoc exposing (nodeAboutInputView, nodeMandateInputView)
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
import Html.Attributes exposing (attribute, autofocus, class, classList, contenteditable, disabled, href, id, list, placeholder, required, rows, style, tabindex, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelCommon exposing (Ev, InputViewMode(..), TensionForm, UserState(..), getChildren, getNode, getParentFragmentFromRole, getParents, getTargets, initTensionForm)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), getOrgaRoles, nid2rootid, nid2type, nodeIdCodec)
import ModelCommon.View exposing (FormText, action2SourceStr, getNodeTextFromNodeType, getTensionText, roleColor, tensionTypeColor)
import ModelSchema exposing (..)
import Ports
import Query.AddTension exposing (addOneTension)
import Session exposing (Apis, GlobalCmd(..), LabelSearchPanelOnClickAction(..))
import Text as T exposing (textH, textT, upH)
import Time


type State
    = State Model


type alias Model =
    { user : UserState
    , form : TensionForm
    , result : GqlData Tension
    , sources : List UserRole
    , targets : List PNode
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
    , path_data : Maybe LocalGraph

    -- Common
    , refresh_trial : Int
    , preventGlitch : Bool -- Debug flag to avoid have a glitch when clikin on a link in the modal that will reset it and make its border likeliy to change (from success to init)
    , modal_confirm : ModalConfirm Msg
    }


type TensionTab
    = NewTensionTab
    | NewRoleTab
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
    , targets = []
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
    , labelsPanel = LabelSearchPanel.init "" SelectLabel user
    , lookup_users = []
    , path_data = Nothing

    -- Common
    , refresh_trial = 0
    , preventGlitch = True
    , modal_confirm = ModalConfirm.init NoMsg
    }


initTensionTab : Model -> Model
initTensionTab model =
    let
        form =
            model.form

        newForm =
            { form
                | type_ = TensionType.Operational
                , action = Nothing
                , blob_type = Nothing
                , users = []
            }
    in
    { model | activeTab = NewTensionTab, form = newForm, result = NotAsked }


initCircleTab : NodeType.NodeType -> Model -> Model
initCircleTab type_ model =
    let
        form =
            model.form

        node =
            form.node

        newForm =
            { form
                | type_ = TensionType.Governance
                , blob_type = Just BlobType.OnNode
                , node = { node | type_ = Just type_ }
                , users = []
            }
                |> NodeDoc.updateNodeForm "name" (withDefault "" node.name)
    in
    case type_ of
        NodeType.Role ->
            { model | activeTab = NewRoleTab, form = { newForm | action = Just TensionAction.NewRole }, result = NotAsked }

        NodeType.Circle ->
            { model | activeTab = NewCircleTab, form = { newForm | action = Just TensionAction.NewCircle }, result = NotAsked }



-- Global methods
-- @debug; model.user not update after r UpdateUserSession !


setUser_ : UserState -> State -> State
setUser_ user (State model) =
    { model | user = user } |> State


setPath_ : LocalGraph -> State -> State
setPath_ p (State model) =
    let
        targets =
            getTargets (Success p) (Just [ RoleType.Guest, RoleType.Member ])

        newModel =
            setTarget (shrinkNode p.focus) model
    in
    { newModel | path_data = Just p, targets = targets } |> State



--setTargets_ : GqlData NodesDict -> State -> State
--setTargets_ odata (State model) =
--    let
--        targets =
--            case odata of
--                Success od ->
--                    getParents model.form.target.nameid odata
--                        ++ (getChildren model.form.target.nameid odata |> List.filter (\n -> n.role_type == Nothing))
--                        -- Circle
--                        ++ ([ model.form.target.nameid, model.form.source.nameid ] |> List.map (\nid -> getNode nid odata) |> List.filterMap identity)
--                        |> LE.uniqueBy (\n -> n.nameid)
--
--                _ ->
--                    []
--    in
--    State { model | targets = targets }


setTab_ : TensionTab -> State -> State
setTab_ tab (State model) =
    { model | activeTab = tab } |> State


fixGlitch_ : State -> State
fixGlitch_ (State model) =
    { model | preventGlitch = False } |> State



--- State Controls


open : Model -> Model
open data =
    { data | isModalActive = True }


switchTab : TensionTab -> Model -> Model
switchTab tab model =
    case tab of
        NewTensionTab ->
            initTensionTab model

        NewRoleTab ->
            initCircleTab NodeType.Role model

        NewCircleTab ->
            initCircleTab NodeType.Circle model


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
            { form | type_ = type_ }
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


setTarget : PNode -> Model -> Model
setTarget target data =
    let
        f =
            data.form

        newForm =
            { f | target = target }
    in
    { data | form = newForm }


setSourceShort : String -> Model -> Model
setSourceShort nameid data =
    let
        f =
            data.form

        newForm =
            -- onlmy nameid is used
            { f | source = { nameid = nameid, name = "", role_type = RoleType.Bot } }
    in
    { data | form = newForm }


setTargetShort : String -> Model -> Model
setTargetShort nameid data =
    let
        f =
            data.form

        newForm =
            -- onlmy nameid is used
            { f | target = { nameid = nameid, name = "" } }
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


setEvents : List Ev -> Model -> Model
setEvents events data =
    let
        f =
            data.form

        newForm =
            { f | events = events }
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
    let
        m =
            initModel data.user
    in
    { m | preventGlitch = False }



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



-- utils


canExitSafe : Model -> Bool
canExitSafe data =
    (hasData data && withMaybeData data.result == Nothing) == False


hasData : Model -> Bool
hasData data =
    isPostEmpty [ "title", "message" ] data.form.post == False



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
    | OnChangeTensionSource UserRole
    | OnChangeTensionTarget PNode
    | OnChangePost String String
    | OnAddLinks
    | OnAddDomains
    | OnAddPolicies
    | OnAddResponsabilities
    | OnSubmitTension Bool Time.Posix
    | OnTensionAck (GqlData Tension)
      -- User Quick Search
    | OnChangeUserPattern Int String
    | OnChangeUserLookup (LookupResult User)
    | OnSelectUser Int String
    | OnCancelUser Int
    | OnShowLookupFs
    | OnCancelLookupFs
      -- Labels
    | LabelSearchPanelMsg LabelSearchPanel.Msg
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
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


out0 : List (Cmd Msg) -> Out
out0 cmds =
    Out cmds [] Nothing


out1 : List GlobalCmd -> Out
out1 cmds =
    Out [] cmds Nothing


out2 : List (Cmd Msg) -> List GlobalCmd -> Out
out2 cmds gcmds =
    Out cmds gcmds Nothing


mapGlobalOutcmds : List GlobalCmd -> ( List (Cmd Msg), List (Cmd Global.Msg) )
mapGlobalOutcmds gcmds =
    gcmds
        |> List.map
            (\m ->
                case m of
                    DoModalAsk link reset ->
                        ( send (OnCloseSafe link reset), Cmd.none )

                    _ ->
                        ( Cmd.none, Cmd.none )
            )
        |> List.unzip


update : Apis -> Msg -> State -> ( State, Out )
update apis message (State model) =
    update_ apis message model
        |> Tuple.mapFirst State


update_ : Apis -> Msg -> Model -> ( Model, Out )
update_ apis message model =
    case message of
        -- Data control
        PushTension ack ->
            ( model, out0 [ addOneTension apis model.form ack ] )

        OnSubmit next ->
            ( model, out0 [ sendNow next ] )

        -- Modal control
        OnOpen ->
            case model.user of
                LoggedIn uctx ->
                    let
                        sources =
                            getOrgaRoles [ nid2rootid model.form.target.nameid ] model.form.uctx.roles
                    in
                    if sources == [] && model.refresh_trial == 0 then
                        ( { model | refresh_trial = 1 }, Out [ sendSleep OnOpen 500 ] [ DoUpdateToken ] Nothing )

                    else if sources == [] then
                        ( setStep (TensionNotAuthorized [ T.notOrgMember, T.joinForTension ]) model |> open
                        , out0 [ Ports.open_modal "tensionModal" ]
                        )

                    else
                        ( model |> setUctx uctx |> setSources sources |> open
                        , out0 [ Ports.open_modal "tensionModal" ]
                        )

                LoggedOut ->
                    ( setStep AuthNeeded model |> open, out0 [ Ports.open_modal "tensionModal" ] )

        OnClose data ->
            let
                cmds =
                    ternary data.reset [ sendSleep OnResetModel 333 ] []

                ( newModel, gcmds ) =
                    if data.link == "" then
                        ( model, [] )

                    else
                        ( { model | preventGlitch = True }, [ DoNavigate data.link ] )
            in
            ( close newModel, out2 ([ Ports.close_modal ] ++ cmds) gcmds )

        OnResetModel ->
            ( resetModel model, noOut )

        OnCloseSafe link onCloseTxt ->
            if canExitSafe model then
                ( model, out0 [ send (OnClose { reset = True, link = link }) ] )

            else
                ( model
                , out0 [ send (DoModalConfirmOpen (OnClose { reset = True, link = link }) { message = Nothing, txts = [ ( upH T.confirmUnsaved, onCloseTxt ) ] }) ]
                )

        OnChangeInputViewMode viewMode ->
            ( setViewMode viewMode model, noOut )

        OnTensionStep step ->
            ( setStep step model, out0 [ Ports.bulma_driver "tensionModal" ] )

        OnSwitchTab tab ->
            ( switchTab tab model, out0 [ Ports.bulma_driver "tensionModal" ] )

        -- Doc change
        OnChangeTensionType type_ ->
            ( setTensionType type_ model, noOut )

        OnChangeTensionSource source ->
            ( setSource source model, noOut )

        OnChangeTensionTarget target ->
            ( setTarget target model, noOut )

        OnChangePost field value ->
            case model.activeTab of
                NewTensionTab ->
                    ( post field value model, noOut )

                NewRoleTab ->
                    ( postNode field value model, noOut )

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
                newModel =
                    if model.activeTab == NewTensionTab then
                        let
                            form =
                                model.form
                        in
                        { model | form = { form | node = initNodeFragment Nothing } }

                    else
                        model

                events =
                    case model.activeTab of
                        NewTensionTab ->
                            [ Ev TensionEvent.Created "" "" ]

                        NewRoleTab ->
                            if doClose == True then
                                [ Ev TensionEvent.Created "" "", Ev TensionEvent.BlobCreated "" "", Ev TensionEvent.BlobPushed "" "" ]

                            else
                                [ Ev TensionEvent.Created "" "", Ev TensionEvent.BlobCreated "" "" ]

                        NewCircleTab ->
                            if doClose == True then
                                [ Ev TensionEvent.Created "" "", Ev TensionEvent.BlobCreated "" "", Ev TensionEvent.BlobPushed "" "" ]

                            else
                                [ Ev TensionEvent.Created "" "", Ev TensionEvent.BlobCreated "" "" ]
            in
            ( newModel
                |> post "createdAt" (fromTime time)
                |> setEvents events
                |> setStatus (ternary (doClose == True) TensionStatus.Closed TensionStatus.Open)
                |> setActiveButton doClose
                |> setResult LoadingSlowly
            , out0 [ send (PushTension OnTensionAck) ]
            )

        OnTensionAck result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( setResult NotAsked model
                    , out1 [ DoAuth model.form.uctx ]
                    )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, out2 [ sendSleep (PushTension OnTensionAck) 500 ] [ DoUpdateToken ] )

                OkAuth tension ->
                    case model.activeTab of
                        NewTensionTab ->
                            ( setResult result model, out1 [ DoPushTension tension ] )

                        NewRoleTab ->
                            let
                                newNameid =
                                    model.form.node.nameid
                                        |> Maybe.map (\nid -> nodeIdCodec model.form.target.nameid nid (withDefault NodeType.Role model.form.node.type_))
                                        |> withDefault ""
                            in
                            ( setResult result model, out1 [ DoPushTension tension, DoFetchNode newNameid ] )

                        NewCircleTab ->
                            let
                                newNameid =
                                    model.form.node.nameid
                                        |> Maybe.map (\nid -> nodeIdCodec model.form.target.nameid nid (withDefault NodeType.Circle model.form.node.type_))
                                        |> withDefault ""
                            in
                            ( setResult result model, out1 [ DoPushTension tension, DoFetchNode newNameid ] )

                DuplicateErr ->
                    ( setResult (Failure [ "Duplicate Error: this name is already taken." ]) model, noOut )

                _ ->
                    ( setResult result model, noOut )

        -- User Quick Search
        OnChangeUserPattern pos pattern ->
            ( updateUserPattern pos pattern model
            , out0 [ Ports.searchUser pattern ]
            )

        OnChangeUserLookup users_ ->
            case users_ of
                Ok users ->
                    ( { model | lookup_users = users }, noOut )

                Err err ->
                    ( model, out0 [ Ports.logErr err ] )

        OnSelectUser pos username ->
            ( selectUser pos username model, noOut )

        OnCancelUser pos ->
            ( cancelUser pos model, noOut )

        OnShowLookupFs ->
            ( openLookup model
            , if model.isLookupOpen == False then
                out0 [ Ports.outsideClickClose "cancelLookupFsFromJs" "usersSearchPanel" ]

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

                ( cmds, _ ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { newModel | labelsPanel = panel }
            , out2 (out.cmds |> List.map (\m -> Cmd.map LabelSearchPanelMsg m) |> List.append cmds) out.gcmds
            )

        -- Confirm Modal
        DoModalConfirmOpen msg mess ->
            ( { model | modal_confirm = ModalConfirm.open msg mess model.modal_confirm }, noOut )

        DoModalConfirmClose _ ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, noOut )

        DoModalConfirmSend ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, out0 [ send model.modal_confirm.msg ] )

        -- Common
        NoMsg ->
            ( model, noOut )

        LogErr err ->
            ( model, out0 [ Ports.logErr err ] )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    [ Ports.lookupUserFromJs OnChangeUserLookup
    , Ports.cancelLookupFsFromJs (always OnCancelLookupFs)
    , Ports.mcPD Ports.closeModalTensionFromJs LogErr OnClose
    , Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    ]
        ++ (LabelSearchPanel.subscriptions model.labelsPanel |> List.map (\s -> Sub.map LabelSearchPanelMsg s))



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    { users_data : GqlData UsersDict }


view : Op -> State -> Html Msg
view op (State model) =
    if model.preventGlitch then
        text ""

    else
        div []
            [ viewModal op (State model)
            , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
            ]


viewModal : Op -> State -> Html Msg
viewModal op (State model) =
    div
        [ id "tensionModal"
        , class "modal modal-fx-fadeIn"
        , classList [ ( "is-active", model.isModalActive ), ( "fixed-top", model.step == TensionFinal && withMaybeData model.result == Nothing ) ]
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
            -- See also Components.SelectType
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

                NewRoleTab ->
                    viewCircle op (State model)

                NewCircleTab ->
                    viewCircle op (State model)

        TensionNotAuthorized errMsg ->
            viewRoleNeeded errMsg

        AuthNeeded ->
            viewAuthNeeded OnClose


viewSources : State -> TensionStep -> Html Msg
viewSources (State model) nextStep =
    div [ class "modal-card submitFocus" ]
        [ div [ class "modal-card-head" ]
            [ span [ class "has-text-weight-medium" ] [ "You have several roles in this organisation. Please select the role from which you want to " ++ action2SourceStr model.form.action |> text ] ]
        , div [ class "modal-card-body" ]
            [ div [ class "buttons buttonRadio", attribute "style" "margin-bottom: 2em; margin-right: 2em; margin-left: 2em;" ] <|
                List.map
                    (\r ->
                        button
                            [ class ("button buttonRole tooltip has-tooltip-arrow defaultSubmit has-tooltip-bottom is-" ++ roleColor r.role_type)
                            , attribute "data-tooltip" ([ r.name, "of", getParentFragmentFromRole r ] |> String.join " ")
                            , onClick (OnTensionStep nextStep)
                            ]
                            [ text r.name ]
                    )
                    model.sources
            ]
        ]


viewTensionTabs : TensionTab -> PNode -> Html Msg
viewTensionTabs tab targ =
    let
        type_ =
            nid2type targ.nameid
    in
    div [ id "tensionTabTop", class "tabs bulma-issue-33 is-boxed" ]
        [ ul []
            [ li [ classList [ ( "is-active", tab == NewTensionTab ) ] ]
                [ a [ class "tootltip", attribute "data-tooltip" "Create a new tension.", onClickPD (OnSwitchTab NewTensionTab), target "blank_" ]
                    [ A.icon1 "icon-exchange" "Tension" ]
                ]
            , if type_ == NodeType.Circle then
                li [ classList [ ( "is-active", tab == NewRoleTab ) ] ]
                    [ a [ class "tootltip", attribute "data-tooltip" "Create or propose a new role.", onClickPD (OnSwitchTab NewRoleTab), target "blank_" ]
                        [ A.icon1 "icon-leaf" "Role" ]
                    ]

              else
                text ""
            , if type_ == NodeType.Circle then
                li [ classList [ ( "is-active", tab == NewCircleTab ) ] ]
                    [ a [ class "tootltip", attribute "data-tooltip" "Create or propose a new circle.", onClickPD (OnSwitchTab NewCircleTab), target "blank_" ]
                        [ A.icon1 "icon-git-branch" "Circle" ]
                    ]

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
            viewSuccess txt res model

        other ->
            div [ class "panel modal-card submitFocus" ]
                [ div [ class "panel-heading" ]
                    [ div [ class "level modal-card-title" ]
                        [ div [ class "level-left" ] <|
                            List.intersperse (text T.space_)
                                [ span [ class "is-size-6 has-text-weight-semibold has-text-grey" ]
                                    [ textT txt.title
                                    , span [ class "has-text-weight-medium" ] [ text " | " ]
                                    , span [ class "dropdown", style "vertical-align" "unset" ]
                                        [ span [ class "dropdown-trigger button-light" ]
                                            [ span [ attribute "aria-controls" "type-menu" ]
                                                [ span [ class <| "has-text-weight-medium " ++ tensionTypeColor "text" form.type_ ]
                                                    [ text (TensionType.toString form.type_), span [ class "ml-2 arrow down" ] [] ]
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
                        , viewRecipients model
                        ]
                    ]
                , viewTensionTabs model.activeTab model.form.target
                , div [ class "modal-card-body" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ input
                                [ class "input autofocus followFocus in-modal"
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
                            [ div [ class "tabs is-boxed is-small pl-1" ]
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
                                                , class "textarea in-modal"
                                                , rows 5
                                                , placeholder (upH T.leaveCommentOpt)
                                                , value message
                                                , onInput (OnChangePost "message")
                                                ]
                                                []

                                        Preview ->
                                            div [] [ renderMarkdown "is-light is-human mt-4 mx-3" message, hr [ class "has-background-grey-lighter" ] [] ]
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
                                , targets = model.targets
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
                    , div [ class "field" ]
                        [ div [ class "is-pulled-left" ]
                            [ button
                                [ class "button is-light"
                                , onClick (OnCloseSafe "" "")
                                ]
                                [ textH T.cancel ]
                            ]
                        , div [ class "is-pulled-right" ]
                            [ div [ class "buttons" ]
                                [ button
                                    ([ class "button is-success defaultSubmit"
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
            viewSuccess txt res model

        other ->
            let
                message =
                    Dict.get "message" form.post |> withDefault ""

                --@Debug: NodeDoc has not its full State yet
                op_ =
                    { data = model
                    , lookup = model.lookup_users
                    , users_data = op.users_data
                    , targets = model.targets |> List.map (\n -> n.nameid)
                    , onChangePost = OnChangePost
                    , onAddLinks = OnAddLinks
                    , onAddDomains = OnAddDomains
                    , onAddPolicies = OnAddPolicies
                    , onAddResponsabilities = OnAddResponsabilities
                    , onSelectUser = OnSelectUser
                    , onCancelUser = OnCancelUser
                    , onShowLookupFs = OnShowLookupFs
                    , onChangeUserPattern = OnChangeUserPattern
                    }
            in
            div [ class "panel modal-card submitFocus" ]
                [ div [ class "panel-heading" ]
                    [ div [ class "level modal-card-title" ]
                        [ div [ class "level-left" ] <|
                            List.intersperse (text T.space_)
                                [ span [ class "is-size-6 has-text-weight-semibold has-text-grey" ]
                                    [ textT txt.title
                                    , span [ class "has-text-weight-medium" ] [ text " | " ]
                                    , span
                                        [ class <| "has-text-weight-medium " ++ tensionTypeColor "text" form.type_ ]
                                        [ text (TensionType.toString form.type_) ]
                                    ]
                                ]
                        , viewRecipients model
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
                    , br [] []
                    , div [ class "field" ]
                        [ div [ class "control" ]
                            [ textarea
                                [ class "textarea in-modal"
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
                    , div [ class "field" ]
                        [ div [ class "is-pulled-left" ]
                            [ button
                                [ class "button is-light"
                                , onClick (OnCloseSafe "" "")
                                ]
                                [ textH T.cancel ]
                            ]
                        , div [ class "is-pulled-right" ]
                            [ div [ class "buttons" ]
                                [ button
                                    ([ class "button is-success defaultSubmit"
                                     , classList
                                        [ ( "is-loading", isLoading && model.activeButton == Just 0 ) ]
                                     , disabled (not isSendable || isLoading)
                                     ]
                                        ++ submitCloseTension
                                    )
                                    [ textH txt.close_submit ]
                                , button
                                    ([ class "button is-warning"
                                     , classList
                                        [ ( "is-loading", isLoading && model.activeButton == Just 1 ) ]
                                     , disabled (not isSendable || isLoading)
                                     ]
                                        ++ submitTension
                                    )
                                    [ textH txt.submit ]
                                ]
                            ]
                        ]
                    ]
                ]


viewRecipients : Model -> Html Msg
viewRecipients model =
    let
        form =
            model.form
    in
    div [ class "level-right" ]
        [ span [ class "has-text-grey-light is-size-6" ] [ textH (T.from ++ ": ") ]
        , span [ class "dropdown" ]
            [ span [ class "dropdown-trigger " ]
                [ span [ attribute "aria-controls" "source-menu" ]
                    [ span
                        [ class "button is-small is-light is-rounded", attribute "style" "border:1px solid black;" ]
                        [ text form.source.name, span [ class "ml-2 icon-chevron-down1" ] [] ]
                    ]
                ]
            , div [ id "source-menu", class "dropdown-menu", attribute "role" "menu" ]
                [ div [ class "dropdown-content" ] <|
                    List.map
                        (\t ->
                            div
                                [ class <| "dropdown-item has-text-weight-semibold button-light has-text-" ++ (roleColor t.role_type |> String.replace "primary" "info")
                                , onClick (OnChangeTensionSource t)
                                ]
                                [ A.icon1 "icon-user" t.name ]
                        )
                        (List.filter (\n -> n.nameid /= model.form.source.nameid) model.sources)
                ]
            ]
        , span [ class "right-arro mx-3" ] []
        , span [ class "has-text-grey-light is-size-6" ] [ textH (T.to ++ ": ") ]
        , span [ class "dropdown" ]
            [ span [ class "dropdown-trigger " ]
                [ span [ attribute "aria-controls" "target-menu" ]
                    [ span
                        [ class "button is-small is-light is-rounded", attribute "style" "border:1px solid black;" ]
                        [ text form.target.name, span [ class "ml-2 icon-chevron-down1" ] [] ]
                    ]
                ]
            , div [ id "target-menu", class "dropdown-menu is-right", attribute "role" "menu" ]
                [ div [ class "dropdown-content" ] <|
                    List.map
                        (\t ->
                            div
                                [ class <| "dropdown-item has-text-weight-semibold button-light"
                                , onClick (OnChangeTensionTarget t)
                                ]
                                [ A.icon1 (ternary (nid2type t.nameid == NodeType.Role) "icon-user" "icon-circle") t.name ]
                        )
                        (List.filter (\n -> n.nameid /= model.form.target.nameid) model.targets)
                ]
            ]
        ]


viewSuccess : FormText -> Tension -> Model -> Html Msg
viewSuccess txt res model =
    let
        link =
            Route.Tension_Dynamic_Dynamic { param1 = nid2rootid model.form.target.nameid, param2 = res.id } |> toHref
    in
    div [ class "box is-light", autofocus True, tabindex 0, onEnter (OnClose { reset = True, link = "" }) ]
        [ A.icon1 "icon-check icon-2x has-text-success" " "
        , textH txt.added
        , text " "
        , a
            [ href link
            , onClickPD (OnClose { reset = True, link = link })
            , target "_blank"
            ]
            [ textH T.checkItOut ]
        ]
