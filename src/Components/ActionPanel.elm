module Components.ActionPanel exposing (Msg(..), State, init, isOpen_, subscriptions, update, view)

import Auth exposing (ErrState(..), parseErr)
import Browser.Events as Events
import Components.Loading as Loading exposing (GqlData, ModalData, RequestResult(..), viewGqlErrors, withMaybeData)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.MoveTension as MoveTension
import Dict exposing (Dict)
import Dom
import Extra exposing (ternary)
import Extra.Events exposing (onClickPD)
import Form exposing (isPostEmpty, isPostSendable)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Icon as I
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (ActionForm, UserState(..), blobFromTensionHead, initActionForm)
import ModelCommon.Codecs exposing (ActionType(..), DocType(..), TensionCharac, nid2rootid)
import ModelSchema exposing (..)
import Ports
import Query.PatchTension exposing (actionRequest)
import Query.QueryTension exposing (getTensionHead)
import Session exposing (Apis, GlobalCmd(..))
import String.Format as Format
import Text as T exposing (textH, textT, upH)
import Time


type State
    = State Model


type alias Model =
    { user : UserState
    , action_result : GqlData ActionResult
    , isOpen : Bool
    , isModalActive : Bool
    , form : ActionForm
    , state : PanelState
    , step : ActionStep
    , domid : String -- allow multiple panel to coexists

    -- Common
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg

    -- Components
    , moveTension : MoveTension.State
    }


type PanelState
    = MoveAction
    | ArchiveAction
    | UnarchiveAction
    | LeaveAction
    | NoAction


type ActionStep
    = StepOne
    | StepAck


initModel : UserState -> Model
initModel user =
    { user = user
    , action_result = NotAsked
    , isOpen = False
    , isModalActive = False
    , form = initActionForm "" user
    , state = NoAction
    , step = StepOne
    , domid = ""

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    , moveTension = MoveTension.init user
    }


init : UserState -> State
init user =
    initModel user |> State


action2str : PanelState -> String
action2str action =
    case action of
        MoveAction ->
            upH T.move

        ArchiveAction ->
            upH T.archive

        UnarchiveAction ->
            upH T.unarchive

        LeaveAction ->
            upH T.leaveRole

        NoAction ->
            "no action"


action2header : PanelState -> String
action2header action =
    case action of
        MoveAction ->
            "Move {{type}}: "

        ArchiveAction ->
            "Archive {{type}}: "

        UnarchiveAction ->
            "Unarchive {{type}}: "

        LeaveAction ->
            "Leave {{type}}: "

        NoAction ->
            "no implemented"


action2post : PanelState -> String
action2post action =
    case action of
        MoveAction ->
            T.moved

        ArchiveAction ->
            T.documentArchived

        UnarchiveAction ->
            T.documentUnarchived

        LeaveAction ->
            T.roleLeft

        NoAction ->
            "error: No action requested"


action2color : PanelState -> String
action2color action =
    case action of
        MoveAction ->
            ""

        ArchiveAction ->
            "warning"

        UnarchiveAction ->
            "warning"

        LeaveAction ->
            "danger"

        NoAction ->
            ""



-- Global methods


isOpen_ : State -> Bool
isOpen_ (State model) =
    model.isOpen



--- State Controls


open : String -> String -> String -> Node -> Model -> Model
open domid tid bid node data =
    let
        f =
            data.form
    in
    { data | isOpen = True, domid = domid }
        |> setTid tid
        |> setBid bid
        |> setNode node


close : Model -> Model
close data =
    { data | isOpen = False }


setActionResult : GqlData ActionResult -> Model -> Model
setActionResult result data =
    let
        ( isModalActive, step ) =
            case result of
                Success _ ->
                    ( data.isModalActive, StepAck )

                Failure _ ->
                    ( data.isModalActive, data.step )

                _ ->
                    ( data.isModalActive, data.step )
    in
    { data | action_result = result, isModalActive = isModalActive, step = step }


activateModal : Model -> Model
activateModal data =
    { data | isModalActive = True }


deactivateModal : Model -> Model
deactivateModal data =
    { data | isModalActive = False }


terminate : Model -> Model
terminate data =
    let
        f =
            data.form
    in
    { data | isOpen = False, isModalActive = False, form = initActionForm f.tid (LoggedIn f.uctx), action_result = NotAsked }


setStep : ActionStep -> Model -> Model
setStep step data =
    { data | step = step }


setAction : PanelState -> Model -> Model
setAction action data =
    let
        events =
            case action of
                MoveAction ->
                    [ TensionEvent.Moved ]

                ArchiveAction ->
                    [ TensionEvent.BlobArchived ]

                UnarchiveAction ->
                    [ TensionEvent.BlobUnarchived ]

                LeaveAction ->
                    [ TensionEvent.UserLeft ]

                NoAction ->
                    []

        newData =
            case events of
                [ TensionEvent.UserLeft ] ->
                    data
                        |> updatePost "old"
                            (data.form.node.role_type
                                |> Maybe.map (\rt -> RoleType.toString rt)
                                |> withDefault ""
                            )
                        |> updatePost "new" data.form.node.nameid

                _ ->
                    data
    in
    { newData | state = action }
        |> setEvents events


updatePost : String -> String -> Model -> Model
updatePost field value data =
    let
        f =
            data.form
    in
    { data | form = { f | post = Dict.insert field value f.post } }


setTid : String -> Model -> Model
setTid tid data =
    let
        f =
            data.form
    in
    { data | form = { f | tid = tid } }


setBid : String -> Model -> Model
setBid bid data =
    let
        f =
            data.form
    in
    { data | form = { f | bid = bid } }


setNode : Node -> Model -> Model
setNode n data =
    let
        f =
            data.form
    in
    { data | form = { f | node = n } }


setEvents : List TensionEvent.TensionEvent -> Model -> Model
setEvents events data =
    let
        f =
            data.form
    in
    { data | form = { f | events_type = Just events } }



-- utils


canExitSafe : Model -> Bool
canExitSafe model =
    -- Condition to close safely (e.g. empty form data)
    (hasData model && withMaybeData model.action_result == Nothing) == False


hasData : Model -> Bool
hasData model =
    -- When you can commit (e.g. empty form data)
    isPostEmpty [ "message" ] model.form.post == False



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = -- Data
      OnOpen String String String Node
    | OnClose
    | PushAction ActionForm PanelState
    | OnSubmit (Time.Posix -> Msg)
    | OnOpenModal PanelState
    | OnCloseModal String
    | OnUpdatePost String String
    | OnActionSubmit Time.Posix
    | OnActionMove
      --
      --| ActionStep1 xxx
    | GotTensionToMove (GqlData TensionHead)
    | ArchiveDocAck (GqlData ActionResult)
    | LeaveRoleAck (GqlData ActionResult)
      -- move tension
    | DoMove TensionHead
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Common
    | NoMsg
    | LogErr String
    | Navigate String
      -- Components
    | MoveTensionMsg MoveTension.Msg


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe Bool -- define what data is to be returned
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


update : Apis -> Msg -> State -> ( State, Out )
update apis message (State model) =
    update_ apis message model
        |> Tuple.mapFirst State


update_ apis message model =
    case message of
        -- Data
        OnOpen domid tid bid node ->
            if model.isOpen == False then
                ( open domid tid bid node model, noOut )

            else
                ( model, out0 [ send OnClose ] )

        OnClose ->
            ( close model, noOut )

        PushAction form state ->
            let
                ackMsg =
                    case state of
                        MoveAction ->
                            \_ -> NoMsg

                        ArchiveAction ->
                            ArchiveDocAck

                        UnarchiveAction ->
                            ArchiveDocAck

                        LeaveAction ->
                            LeaveRoleAck

                        NoAction ->
                            \_ -> NoMsg
            in
            ( model, out0 [ actionRequest apis.gql form ackMsg ] )

        OnSubmit next ->
            ( model, out0 [ sendNow next ] )

        OnOpenModal action ->
            let
                newModel =
                    model
                        |> activateModal
                        |> setAction action
                        |> setStep StepOne
            in
            ( newModel, out0 [ Ports.open_modal "actionPanelModal" ] )

        OnCloseModal link ->
            let
                gcmds =
                    if link /= "" then
                        [ DoNavigate link ]

                    else if withMaybeData model.action_result /= Nothing then
                        case model.state of
                            MoveAction ->
                                []

                            ArchiveAction ->
                                [ DoDelNodes [ model.form.node.nameid ] ]

                            UnarchiveAction ->
                                []

                            LeaveAction ->
                                -- Ignore Guest deletion (either non visible or very small)
                                [ DoUpdateNode model.form.node.nameid (\n -> { n | first_link = Nothing }) ]

                            NoAction ->
                                []

                    else
                        []
            in
            ( terminate model, out2 [ Ports.close_modal ] gcmds )

        OnUpdatePost field value ->
            ( updatePost field value model, noOut )

        OnActionSubmit time ->
            let
                data =
                    model
                        |> updatePost "createdAt" (fromTime time)
                        |> setActionResult LoadingSlowly
            in
            ( data, out0 [ send (PushAction data.form data.state) ] )

        OnActionMove ->
            ( model, out0 [ getTensionHead apis.gql model.form.tid GotTensionToMove ] )

        GotTensionToMove result ->
            case result of
                Success th ->
                    ( model, out0 [ send (DoMove th), send OnClose ] )

                _ ->
                    ( model, noOut )

        DoMove t ->
            ( model, out0 [ Cmd.map MoveTensionMsg (send (MoveTension.OnOpen t.id t.receiver.nameid (blobFromTensionHead t))) ] )

        ArchiveDocAck result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( setActionResult NotAsked model
                    , out1 [ DoAuth model.form.uctx ]
                    )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, out2 [ sendSleep (PushAction model.form model.state) 500 ] [ DoUpdateToken ] )

                OkAuth _ ->
                    ( model |> close |> setActionResult result, Out [] [] (Just True) )

                _ ->
                    ( model |> close |> setActionResult result, noOut )

        LeaveRoleAck result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( setActionResult NotAsked model
                    , out1 [ DoAuth model.form.uctx ]
                    )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, out2 [ sendSleep (PushAction model.form model.state) 500 ] [ DoUpdateToken ] )

                OkAuth _ ->
                    ( model |> close |> setActionResult result, Out [] [] (Just True) )

                _ ->
                    ( model |> close |> setActionResult result, out0 [ Ports.click "body" ] )

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

        Navigate link ->
            ( model, out1 [ DoNavigate link ] )

        -- Components
        MoveTensionMsg msg ->
            let
                ( data, out ) =
                    MoveTension.update apis msg model.moveTension

                gcmds =
                    out.result
                        |> Maybe.map
                            (\x ->
                                if Tuple.first x == False then
                                    let
                                        ( nameid, parentid_new, nameid_new ) =
                                            Tuple.second x
                                    in
                                    [ DoMoveNode nameid parentid_new nameid_new ]

                                else
                                    []
                            )
                        |> withDefault []
            in
            ( { model | moveTension = data }, out2 (List.map (\m -> Cmd.map MoveTensionMsg m) out.cmds) (out.gcmds ++ gcmds) )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    [ Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    ]
        ++ (MoveTension.subscriptions |> List.map (\s -> Sub.map MoveTensionMsg s))
        ++ (if model.isOpen then
                [ Events.onMouseUp (Dom.outsideClickClose model.domid OnClose)
                , Events.onKeyUp (Dom.key "Escape" OnClose)
                ]

            else
                []
           )



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    { tc : Maybe TensionCharac
    , isAdmin : Bool
    , hasRole : Bool
    , isRight : Bool -- view option
    , domid : String
    , orga_data : GqlData NodesDict
    }


view : Op -> State -> Html Msg
view op (State model) =
    div []
        [ if model.isOpen && model.domid == op.domid then
            viewPanel op model

          else
            text ""
        , if model.isModalActive then
            viewModal op model

          else
            text ""
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        , MoveTension.view { orga_data = op.orga_data } model.moveTension |> Html.map MoveTensionMsg
        ]


viewPanel : Op -> Model -> Html Msg
viewPanel op model =
    div [ class "dropdown-content", classList [ ( "is-right", op.isRight ) ] ] <|
        (-- EDIT ACTION
         if model.form.node.role_type /= Just RoleType.Guest then
            [ div
                [ class "dropdown-item button-light"
                , onClick
                    (Navigate
                        ((Route.Tension_Dynamic_Dynamic_Action { param1 = nid2rootid model.form.node.nameid, param2 = model.form.tid } |> toHref)
                            ++ "?v=edit"
                        )
                    )
                ]
                [ I.icon1 "icon-edit-2" (upH T.edit) ]
            , hr [ class "dropdown-divider" ] []
            ]

         else
            []
        )
            -- MOVE ACTION
            ++ (if op.isAdmin then
                    [ div [ class "dropdown-item button-light", onClick OnActionMove ]
                        [ span [ class "right-arrow2 pl-0 pr-2" ] [], text (action2str MoveAction) ]
                    , hr [ class "dropdown-divider" ] []
                    ]

                else
                    []
               )
            -- ARCHIVE ACTION
            ++ (if op.isAdmin then
                    case Maybe.map (\c -> c.action_type) op.tc of
                        Just EDIT ->
                            [ div [ class "dropdown-item button-light is-warning", onClick (OnOpenModal ArchiveAction) ]
                                [ I.icon1 "icon-archive" (action2str ArchiveAction) ]
                            ]

                        Just ARCHIVE ->
                            [ div [ class "dropdown-item button-light", onClick (OnOpenModal UnarchiveAction) ]
                                [ I.icon1 "icon-archive" (action2str UnarchiveAction) ]
                            ]

                        _ ->
                            [ div [] [ text "not implemented" ] ]

                else
                    []
               )
            -- LEAVE ACTION
            ++ (if op.hasRole then
                    [ div [ class "dropdown-item button-light is-danger", onClick (OnOpenModal LeaveAction) ]
                        [ p []
                            [ I.icon1 "icon-log-out" (action2str LeaveAction) ]
                        ]
                    ]
                        |> List.append [ hr [ class "dropdown-divider" ] [] ]

                else
                    []
               )


viewModal : Op -> Model -> Html Msg
viewModal op model =
    div
        [ id "actionPanelModal"
        , class "modal modal-fx-fadeIn"
        , classList [ ( "is-active", model.isModalActive ) ]
        , attribute "data-modal-close" "closeActionPanelModalFromJs"
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "actionPanelModal"
            , onClick (OnCloseModal "")
            ]
            []
        , div [ class "modal-content" ] [ viewModalContent op model ]
        , button [ class "modal-close is-large", onClick (OnCloseModal "") ] []
        ]


viewModalContent : Op -> Model -> Html Msg
viewModalContent op model =
    case model.step of
        StepOne ->
            viewStep1 op model

        StepAck ->
            case model.action_result of
                Success _ ->
                    div
                        [ class "box is-light" ]
                        [ I.icon1 "icon-check icon-2x has-text-success" " "
                        , textH (action2post model.state)
                        ]

                Failure err ->
                    viewGqlErrors err

                _ ->
                    viewGqlErrors [ "not implemented." ]



--- Viewer


viewStep1 : Op -> Model -> Html Msg
viewStep1 op model =
    let
        header =
            action2header model.state

        color =
            action2color model.state

        form =
            model.form

        name =
            form.node.name

        type_ =
            form.node.type_
                |> NodeType.toString

        message =
            Dict.get "message" form.post |> withDefault ""

        isLoading =
            model.action_result == LoadingSlowly
    in
    div [ class "modal-card" ]
        [ div [ class ("modal-card-head has-background-" ++ color) ]
            [ div [ class "modal-card-title is-size-6 has-text-weight-semibold" ]
                [ header
                    |> Format.namedValue "type" type_
                    --|> Format.namedValue "name" name
                    |> text
                    |> List.singleton
                    |> span []
                , span [ class "has-text-primary" ] [ text name ]
                ]
            ]
        , div [ class "modal-card-body" ]
            [ div [ class "field" ]
                [ div [ class "control submitFocus" ]
                    [ textarea
                        [ class "textarea in-modal"
                        , rows 3
                        , placeholder (upH T.leaveCommentOpt)
                        , value message
                        , onInput <| OnUpdatePost "message"
                        ]
                        []
                    ]
                , p [ class "help-label" ] [ textH T.tensionMessageHelp ]
                ]
            ]
        , div [ class "modal-card-foot", attribute "style" "display: block;" ]
            [ case model.action_result of
                Failure err ->
                    div [ class "field" ] [ viewGqlErrors err ]

                _ ->
                    text ""
            , div [ class "field " ]
                [ div [ class "is-pulled-left" ]
                    [ button
                        [ class "button is-light"
                        , onClick (OnCloseModal "")
                        ]
                        [ textH T.cancel ]
                    ]
                , div [ class "is-pulled-right" ]
                    [ button
                        ([ class ("button defaultSubmit is-light is-" ++ color)
                         , classList [ ( "is-loading", isLoading ) ]
                         ]
                            ++ [ onClick (OnSubmit <| OnActionSubmit) ]
                        )
                        [ model.state |> action2str |> text ]
                    ]
                ]
            ]
        ]
