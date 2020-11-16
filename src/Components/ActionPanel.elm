module Components.ActionPanel exposing (..)

import Components.Fa as Fa
import Components.Loading as Loading exposing (GqlData, RequestResult(..), loadingSpin, viewGqlErrors, withMapData, withMaybeData)
import Components.Text as T
import Dict exposing (Dict)
import Extra exposing (ternary)
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, selected, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (UserState(..))
import ModelCommon.Codecs exposing (ActionType(..), DocType(..), NodeFocus, TensionCharac, nearestCircleid)
import ModelCommon.View exposing (viewUser)
import ModelSchema exposing (..)
import Time


type alias ActionPanel =
    { isEdit : Bool
    , isModalActive : Bool
    , form : ActionForm
    , state : ActionPanelState
    , action_result : GqlData ActionResult
    }


type alias ActionForm =
    { uctx : UserCtx
    , tid : String
    , nid : String
    , bid : String
    , events_type : Maybe (List TensionEvent.TensionEvent)
    , post : Post
    }


type ActionPanelState
    = ArchiveAction
    | UnarchiveAction
    | LeaveAction
    | NoAction


initActionForm : UserState -> String -> ActionForm
initActionForm user tid =
    { uctx =
        case user of
            LoggedIn uctx ->
                uctx

            LoggedOut ->
                UserCtx "" Nothing (UserRights False False) []
    , tid = tid
    , nid = ""
    , bid = ""
    , events_type = Nothing
    , post = Dict.empty
    }


create : UserState -> String -> ActionPanel
create user tid =
    { isEdit = False
    , isModalActive = False
    , form = initActionForm user tid
    , state = NoAction
    , action_result = NotAsked
    }



-- State controls


edit : String -> ActionPanel -> ActionPanel
edit bid data =
    let
        f =
            data.form
    in
    { data | isEdit = True, form = { f | bid = bid } }


cancelEdit : ActionPanel -> ActionPanel
cancelEdit data =
    { data | isEdit = False }


setActionResult : GqlData ActionResult -> ActionPanel -> ActionPanel
setActionResult result data =
    let
        isModalActive =
            case result of
                Success _ ->
                    True

                Failure _ ->
                    True

                _ ->
                    False
    in
    { data | action_result = result, isModalActive = isModalActive }


disactiveModal : ActionPanel -> ActionPanel
disactiveModal data =
    { data | isModalActive = False }


closeModal : ActionPanel -> ActionPanel
closeModal data =
    let
        f =
            data.form
    in
    { data | isEdit = False, isModalActive = False, form = initActionForm (LoggedIn f.uctx) f.tid }



-- Update form


post : String -> String -> ActionPanel -> ActionPanel
post field value data =
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


setNid : String -> ActionPanel -> ActionPanel
setNid nid data =
    let
        f =
            data.form
    in
    { data | form = { f | nid = nid } }


setEvents : List TensionEvent.TensionEvent -> ActionPanel -> ActionPanel
setEvents events data =
    let
        f =
            data.form
    in
    { data | form = { f | events_type = Just events } }


setAction : ActionPanelState -> ActionPanel -> ActionPanel
setAction action data =
    { data | state = action }


type alias Op msg =
    { tc : Maybe TensionCharac
    , isAdmin : Bool
    , hasRole : Bool
    , isRight : Bool
    , node : NodeFocus
    , data : ActionPanel
    , onCloseModal : String -> msg
    , onSubmit : (Time.Posix -> msg) -> msg
    , onNavigate : String -> msg
    , onArchive : ActionPanelState -> Time.Posix -> msg
    , onLeave : ActionPanelState -> Time.Posix -> msg
    }


view : Op msg -> Html msg
view op =
    let
        actionType_m =
            Maybe.map (\c -> c.action_type) op.tc
    in
    div []
        [ if op.data.isEdit then
            div
                [ class "dropdown-content", classList [ ( "is-right", op.isRight ) ] ]
            <|
                [ div
                    [ class "dropdown-item button-light"
                    , onClick (op.onNavigate ((Route.Tension_Dynamic_Dynamic_Action { param1 = op.node.rootnameid, param2 = op.data.form.tid } |> toHref) ++ "?v=edit"))
                    ]
                    [ Fa.icon "fas fa-pen" T.edit ]
                , hr [ class "dropdown-divider" ] []
                ]
                    ++ [ case actionType_m of
                            Just EDIT ->
                                div [ class "dropdown-item button-light is-warning", onClick (op.onSubmit <| op.onArchive ArchiveAction) ]
                                    [ Fa.icon "fas fa-archive" T.archive, loadingSpin (op.data.action_result == LoadingSlowly && op.data.state == ArchiveAction) ]

                            Just ARCHIVE ->
                                div [ class "dropdown-item button-light", onClick (op.onSubmit <| op.onArchive UnarchiveAction) ]
                                    [ Fa.icon "fas fa-archive" T.unarchive, loadingSpin (op.data.action_result == LoadingSlowly && op.data.state == UnarchiveAction) ]

                            Just NEW ->
                                text ""

                            Nothing ->
                                div [] [ text "not implemented" ]
                       ]
                    ++ (if op.hasRole then
                            [ hr [ class "dropdown-divider" ] []
                            , div [ class "dropdown-item button-light is-danger", onClick (op.onSubmit <| op.onLeave LeaveAction) ]
                                [ p [] [ Fa.icon "fas fa-sign-out-alt" T.leaveRole, loadingSpin (op.data.action_result == LoadingSlowly && op.data.state == LeaveAction) ] ]
                            ]

                        else
                            []
                       )

          else
            text ""
        , if op.data.isModalActive then
            viewModal op

          else
            text ""
        ]


viewModal : Op msg -> Html msg
viewModal op =
    div
        [ id "actionPanelModal"
        , class "modal modal-fx-fadeIn elmModal"
        , classList [ ( "is-active", op.data.isModalActive ) ]
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "actionPanelModal"
            , onClick (op.onCloseModal "")
            ]
            []
        , div [ class "modal-content" ]
            [ case op.data.action_result of
                Success t ->
                    div
                        [ class "box is-light" ]
                        [ Fa.icon "fas fa-check fa-2x has-text-success" " "
                        , case op.data.state of
                            ArchiveAction ->
                                text "Document archived"

                            UnarchiveAction ->
                                text "Document unarchived"

                            LeaveAction ->
                                text "Role left"

                            NoAction ->
                                text "error: No action requested"
                        ]

                Failure err ->
                    viewGqlErrors err

                _ ->
                    viewGqlErrors [ "not implemented." ]
            ]
        , button [ class "modal-close is-large", onClick (op.onCloseModal "") ] []
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
