module Components.ActionPanel exposing (..)

import Components.Loading as Loading exposing (GqlData, RequestResult(..), loadingSpin, viewGqlErrors, withMapData, withMaybeData)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, selected, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Icon as I
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (ActionForm, UserState(..), initActionForm)
import ModelCommon.Codecs exposing (ActionType(..), DocType(..), NodeFocus, TensionCharac, nearestCircleid, nid2rootid, nid2type)
import ModelCommon.View exposing (viewUser)
import ModelSchema exposing (..)
import String.Format as Format
import Text as T exposing (textH, textT, upH)
import Time


type alias ActionPanel =
    { isEdit : Bool
    , isModalActive : Bool
    , domid : String -- allow multiple panel to coexists
    , form : ActionForm
    , state : ActionPanelState
    , step : ActionStep
    , action_result : GqlData ActionResult
    }


type ActionPanelState
    = MoveAction
    | ArchiveAction
    | UnarchiveAction
    | LeaveAction
    | NoAction


type ActionStep
    = StepOne
    | StepAck


action2str : ActionPanelState -> String
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


action2header : ActionPanelState -> String
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


action2post : ActionPanelState -> String
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


action2color : ActionPanelState -> String
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


init : String -> UserState -> ActionPanel
init tid user =
    { isEdit = False
    , isModalActive = False
    , form = initActionForm tid user
    , state = NoAction
    , step = StepOne
    , action_result = NotAsked
    , domid = ""
    }



-- State controls


open : String -> String -> ActionPanel -> ActionPanel
open domid bid data =
    let
        f =
            data.form
    in
    { data | isEdit = True, form = { f | bid = bid }, domid = domid }


close : ActionPanel -> ActionPanel
close data =
    { data | isEdit = False }


setActionResult : GqlData ActionResult -> ActionPanel -> ActionPanel
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


activateModal : ActionPanel -> ActionPanel
activateModal data =
    { data | isModalActive = True }


deactivateModal : ActionPanel -> ActionPanel
deactivateModal data =
    { data | isModalActive = False }


terminate : ActionPanel -> ActionPanel
terminate data =
    let
        f =
            data.form
    in
    { data | isEdit = False, isModalActive = False, form = initActionForm f.tid (LoggedIn f.uctx), action_result = NotAsked }


setStep : ActionStep -> ActionPanel -> ActionPanel
setStep step data =
    { data | step = step }


setAction : ActionPanelState -> ActionPanel -> ActionPanel
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



-- Accessors


isSuccess : ActionPanel -> Bool
isSuccess data =
    case data.action_result of
        Success _ ->
            True

        _ ->
            False



-- Update form


updatePost : String -> String -> ActionPanel -> ActionPanel
updatePost field value data =
    let
        f =
            data.form
    in
    { data | form = { f | post = Dict.insert field value f.post } }


setTid : String -> ActionPanel -> ActionPanel
setTid tid data =
    let
        f =
            data.form
    in
    { data | form = { f | tid = tid } }


setNode : Node -> ActionPanel -> ActionPanel
setNode n data =
    let
        f =
            data.form
    in
    { data | form = { f | node = n } }


setEvents : List TensionEvent.TensionEvent -> ActionPanel -> ActionPanel
setEvents events data =
    let
        f =
            data.form
    in
    { data | form = { f | events_type = Just events } }


type alias Op msg =
    { tc : Maybe TensionCharac
    , isAdmin : Bool
    , hasRole : Bool
    , isRight : Bool -- view option
    , domid : String
    , data : ActionPanel
    , onSubmit : (Time.Posix -> msg) -> msg
    , onOpenModal : ActionPanelState -> msg
    , onCloseModal : String -> msg
    , onNavigate : String -> msg
    , onActionSubmit : Time.Posix -> msg
    , onActionMove : Time.Posix -> msg
    , onUpdatePost : String -> String -> msg
    }


view : Op msg -> Html msg
view op =
    div []
        [ if op.data.isEdit && op.domid == op.data.domid then
            viewPanel op

          else
            text ""
        , if op.data.isModalActive then
            viewModal op

          else
            text ""
        ]


viewPanel : Op msg -> Html msg
viewPanel op =
    div [ class "dropdown-content", classList [ ( "is-right", op.isRight ) ] ] <|
        (-- EDIT ACTION
         if op.data.form.node.role_type /= Just RoleType.Guest then
            [ div
                [ class "dropdown-item button-light"
                , onClick
                    (op.onNavigate
                        ((Route.Tension_Dynamic_Dynamic_Action { param1 = nid2rootid op.data.form.node.nameid, param2 = op.data.form.tid } |> toHref)
                            ++ "?v=edit"
                        )
                    )
                ]
                [ I.icon1 "icon-pen" (upH T.edit) ]
            , hr [ class "dropdown-divider" ] []
            ]

         else
            []
        )
            -- MOVE ACTION
            ++ (if op.isAdmin then
                    [ div [ class "dropdown-item button-light", onClick (op.onSubmit <| op.onActionMove) ]
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
                            [ div [ class "dropdown-item button-light is-warning", onClick (op.onOpenModal ArchiveAction) ]
                                [ I.icon1 "icon-archive" (action2str ArchiveAction) ]
                            ]

                        Just ARCHIVE ->
                            [ div [ class "dropdown-item button-light", onClick (op.onOpenModal UnarchiveAction) ]
                                [ I.icon1 "icon-archive" (action2str UnarchiveAction) ]
                            ]

                        _ ->
                            [ div [] [ text "not implemented" ] ]

                else
                    []
               )
            -- LEAVE ACTION
            ++ (if op.hasRole then
                    [ div [ class "dropdown-item button-light is-danger", onClick (op.onOpenModal LeaveAction) ]
                        [ p []
                            [ I.icon1 "icon-log-out" (action2str LeaveAction) ]
                        ]
                    ]
                        |> List.append [ hr [ class "dropdown-divider" ] [] ]

                else
                    []
               )


viewModal : Op msg -> Html msg
viewModal op =
    div
        [ id "actionPanelModal"
        , class "modal modal-fx-fadeIn"
        , classList [ ( "is-active", op.data.isModalActive ) ]
        , attribute "data-modal-close" "closeActionPanelModalFromJs"
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "actionPanelModal"
            , onClick (op.onCloseModal "")
            ]
            []
        , div [ class "modal-content" ] [ viewModalContent op ]
        , button [ class "modal-close is-large", onClick (op.onCloseModal "") ] []
        ]


viewModalContent : Op msg -> Html msg
viewModalContent op =
    let
        form =
            op.data.form

        name =
            form.node.name

        type_ =
            form.node.type_

        isLoading =
            op.data.action_result == LoadingSlowly
    in
    case op.data.step of
        StepOne ->
            viewStep1 op.data.state op

        StepAck ->
            case op.data.action_result of
                Success t ->
                    div
                        [ class "box is-light" ]
                        [ I.icon1 "icon-check icon-2x has-text-success" " "
                        , textH (action2post op.data.state)
                        ]

                Failure err ->
                    viewGqlErrors err

                _ ->
                    viewGqlErrors [ "not implemented." ]



--- Viewer


viewStep1 : ActionPanelState -> Op msg -> Html msg
viewStep1 action op =
    let
        header =
            action2header action

        color =
            action2color action

        form =
            op.data.form

        name =
            form.node.name

        type_ =
            form.node.type_
                |> NodeType.toString

        message =
            Dict.get "message" form.post |> withDefault ""

        isLoading =
            op.data.action_result == LoadingSlowly
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
                        , onInput <| op.onUpdatePost "message"
                        ]
                        []
                    ]
                , p [ class "help-label" ] [ textH T.tensionMessageHelp ]
                ]
            ]
        , div [ class "modal-card-foot", attribute "style" "display: block;" ]
            [ case op.data.action_result of
                Failure err ->
                    div [ class "field" ] [ viewGqlErrors err ]

                _ ->
                    text ""
            , div [ class "field " ]
                [ div [ class "is-pulled-left" ]
                    [ button
                        [ class "button is-light"
                        , onClick (op.onCloseModal "")
                        ]
                        [ textH T.cancel ]
                    ]
                , div [ class "is-pulled-right" ]
                    [ button
                        ([ class ("button defaultSubmit is-light is-" ++ color)
                         , classList [ ( "is-loading", isLoading ) ]
                         ]
                            ++ [ onClick (op.onSubmit <| op.onActionSubmit) ]
                        )
                        [ action |> action2str |> text ]
                    ]
                ]
            ]
        ]



--- Utils


archiveActionToggle : Maybe TensionAction.TensionAction -> Maybe TensionAction.TensionAction
archiveActionToggle action_m =
    action_m
        |> Maybe.map
            (\action ->
                case action of
                    TensionAction.EditCircle ->
                        Just TensionAction.ArchivedCircle

                    TensionAction.ArchivedCircle ->
                        Just TensionAction.EditCircle

                    TensionAction.EditRole ->
                        Just TensionAction.ArchivedRole

                    TensionAction.ArchivedRole ->
                        Just TensionAction.EditRole

                    TensionAction.EditMd ->
                        Just TensionAction.ArchivedMd

                    TensionAction.ArchivedMd ->
                        Just TensionAction.EditMd

                    TensionAction.NewCircle ->
                        Nothing

                    TensionAction.NewRole ->
                        Nothing

                    TensionAction.NewMd ->
                        Nothing
            )
        |> withDefault Nothing
