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


module Components.ConfirmOwner exposing (Msg(..), State, init, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Bulk exposing (UserState(..), uctxFromUser)
import Bulk.Codecs exposing (NodeFocus)
import Bulk.Error exposing (viewGqlErrors)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Dict exposing (Dict)
import Extra exposing (space_, ternary, textH, unwrap, unwrap2)
import Extra.Events exposing (onClickPD)
import Extra.Views exposing (showMsg)
import Form exposing (isPostEmpty)
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), RestData, isFailure, isSuccess, withMapDataRest, withMaybeData)
import Maybe exposing (withDefault)
import ModelSchema exposing (Post, UserCtx)
import Ports
import RemoteData
import Requests exposing (makeOwner)
import Session exposing (Apis, GlobalCmd(..))
import Text as T
import Time



-- ------------------------------
-- M O D E L
-- ------------------------------


type State
    = State Model


type alias Model =
    { user : UserState
    , isActive : Bool
    , isActive2 : Bool -- Let minimze VDOM load + prevent glitch while keeping css effects
    , focus : NodeFocus
    , target_username : String
    , owner_result : RestData Bool

    -- Common
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    }


initModel : UserState -> NodeFocus -> Model
initModel user focus =
    { user = user
    , isActive = False
    , isActive2 = False
    , focus = focus
    , target_username = ""
    , owner_result = RemoteData.NotAsked

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }


init : UserState -> NodeFocus -> State
init user focus =
    initModel user focus |> State



-- Global methods


isActive_ : State -> Bool
isActive_ (State model) =
    model.isActive



-- State Controls


resetModel : Model -> Model
resetModel model =
    initModel model.user model.focus



-- utils


canExitSafe : Model -> Bool
canExitSafe model =
    -- Condition to close safely (e.g. empty form data)
    True



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = SetIsActive2 Bool
    | OnOpen String
    | OnClose ModalData
    | OnCloseSafe String String
    | OnReset
    | OnSubmit Bool (Time.Posix -> Msg)
      -- Data
    | OnMakeOwner Time.Posix
    | GotMakeOwner (RestData Bool)
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
    , result : Maybe ( Bool, () ) -- define what data is to be returned
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
        SetIsActive2 v ->
            if v then
                ( { model | isActive = model.isActive2 }, out0 [ Ports.open_modal "ConfirmOwnerModal" ] )

            else
                ( { model | isActive2 = model.isActive }, noOut )

        OnOpen username ->
            ( { model | isActive2 = True, target_username = username }
            , out0 [ sendSleep (SetIsActive2 True) 10, Ports.open_modal "ConfirmOwnerModal" ]
            )

        OnClose data ->
            let
                ( newModel, gcmds ) =
                    if data.link == "" then
                        ( model, [] )

                    else
                        ( { model | isActive2 = True }, [ DoNavigate data.link ] )
            in
            ( { newModel | isActive = False }
            , out2
                [ Ports.close_modal
                , ternary data.reset (sendSleep OnReset 333) Cmd.none
                , sendSleep (SetIsActive2 False) 500
                ]
                gcmds
            )

        OnReset ->
            ( resetModel model, noOut )

        OnCloseSafe link onCloseTxt ->
            if canExitSafe model then
                ( model, out0 [ send (OnClose { reset = True, link = link }) ] )

            else
                ( model
                , out0
                    [ send
                        (DoModalConfirmOpen (OnClose { reset = True, link = link })
                            { message = Nothing, txts = [ ( T.confirmUnsaved, onCloseTxt ) ] }
                        )
                    ]
                )

        -- Data
        OnSubmit isSendable next ->
            if isSendable then
                ( model, out0 [ sendNow next ] )

            else
                ( model, noOut )

        OnMakeOwner time ->
            ( model, out0 [ makeOwner apis model.focus.nameid model.target_username GotMakeOwner ] )

        GotMakeOwner result ->
            let
                closeMsg =
                    send (OnClose { reset = True, link = "" })
            in
            case result of
                RemoteData.Success _ ->
                    ( { model | owner_result = result }
                    , out2 [ closeMsg ]
                        [ DoPushSystemNotif (withMapDataRest (\ok -> ternary ok T.ownerPromotion "not implemented") result)

                        -- do the Doload !
                        , DoUpdateNode model.focus.nameid identity
                        ]
                    )

                RemoteData.Failure err ->
                    ( { model | owner_result = result }
                    , out2 [ closeMsg ] [ DoPushSystemNotif (RemoteData.Failure err) ]
                    )

                _ ->
                    ( { model | owner_result = result }, noOut )

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
    [ Ports.mcPD Ports.closeModalFromJs LogErr OnClose
    , Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    ]
        ++ (if model.isActive then
                -- Extra module used when modal is active
                []

            else
                []
           )



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    {}


view : Op -> State -> Html Msg
view op (State model) =
    if model.isActive2 then
        div []
            [ viewModal op model
            , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
            ]

    else
        text ""


viewModal : Op -> Model -> Html Msg
viewModal op model =
    div
        [ id "ConfirmOwnerModal"
        , class "modal is-light modal-fx-fadeIn"
        , classList [ ( "is-active", model.isActive ) ]
        , attribute "data-modal-close" "closeModalFromJs"
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "ConfirmOwnerModal"
            , onClick (OnCloseSafe "" "")
            ]
            []
        , div [ class "modal-content" ]
            [ viewModalContent op model ]

        --, button [ class "modal-close is-large", onClick (OnCloseSafe "" "") ] []
        ]


viewModalContent : Op -> Model -> Html Msg
viewModalContent op model =
    let
        isSendable_ =
            model.target_username /= ""

        isLoading =
            Loading.isLoadingRest model.owner_result
    in
    div [ class "modal-card" ]
        [ div [ class "modal-card-head is-warning" ]
            [ div [ class "modal-card-title is-size-6 has-text-weight-semibold" ]
                [ text T.confirmAction ]
            ]
        , div [ class "modal-card-body" ]
            [ viewBody op model ]
        , div [ class "modal-card-foot", attribute "style" "display: block;" ]
            [ div [ class "field level is-mobile" ]
                [ div [ class "level-left" ]
                    [ button
                        [ class "button is-light"
                        , onClick (OnCloseSafe "" "")
                        ]
                        [ textH T.cancel ]
                    ]
                , div [ class "level-right" ]
                    [ button
                        [ class "button is-warning"
                        , classList [ ( "is-loading", isLoading ) ]
                        , disabled (not isSendable_)
                        , onClick (OnSubmit (isSendable_ && not isLoading) <| OnMakeOwner)
                        ]
                        [ textH T.confirmOwner ]
                    ]
                ]
            ]
        ]


viewBody : Op -> Model -> Html Msg
viewBody op model =
    div []
        [ showMsg "owner-0" "is-warning is-light" "icon-alert-triangle" T.newOwnerMessageWarning ""
        ]
