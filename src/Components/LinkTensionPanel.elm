{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2023 Fractale Co

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


port module Components.LinkTensionPanel exposing (Msg(..), State, hasTargets_, init, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Bulk exposing (UserState(..), uctxFromUser)
import Bulk.Error exposing (viewGqlErrors)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Dict exposing (Dict)
import Extra exposing (ternary, textH, upH)
import Form exposing (isPostEmpty)
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), isFailure, isSuccess, withMaybeData)
import Maybe exposing (withDefault)
import ModelSchema exposing (NodesDict, Post, TensionLight, UserCtx)
import Ports
import Requests exposing (TensionQuery, fetchTensionsLight, initTensionQuery)
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
    , isOpen : Bool
    , data_result : GqlData (List TensionLight)
    , selected : List TensionLight
    , select_all : Bool
    , form : TensionQuery -- user inputs

    -- Common
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    }


initModel : UserState -> Model
initModel user =
    { user = user
    , isOpen = False
    , data_result = NotAsked
    , selected = []
    , select_all = False
    , form = initTensionQuery

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }


init : UserState -> State
init user =
    initModel user |> State



-- Global methods


hasTargets_ : State -> Bool
hasTargets_ (State model) =
    List.length model.form.targetids > 0



-- State Controls


resetModel : Model -> Model
resetModel model =
    initModel model.user


setDataResult : GqlData (List TensionLight) -> Model -> Model
setDataResult result model =
    { model | data_result = result }



-- utils


canExitSafe : Model -> Bool
canExitSafe model =
    -- Condition to close safely (e.g. empty form data)
    not (hasData model && withMaybeData model.data_result == Nothing)


hasData : Model -> Bool
hasData model =
    -- When you can commit (e.g. empty form data)
    List.length model.selected /= 0



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = -- Data
      OnOpen
    | OnClose
    | SetTargets (List String)
    | OnSubmit (Time.Posix -> Msg)
    | OnQueryData
    | OnDataAck (GqlData (List TensionLight))
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
    , result : Maybe ( Bool, List String ) -- List of tid
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
        OnOpen ->
            ( { model | isOpen = True }
            , out0 [ Ports.outsideClickClose "closeLinkTensionPanelFromJs" "linkTensionPanel" ]
            )

        OnClose ->
            ( { model | isOpen = False }, noOut )

        SetTargets targetids ->
            let
                form =
                    model.form

                newForm =
                    { form | targetids = targetids }
            in
            ( { model | form = newForm }, out0 [ send OnQueryData ] )

        OnSubmit next ->
            ( model, out0 [ sendNow next ] )

        OnQueryData ->
            ( setDataResult LoadingSlowly model
            , out0 [ fetchTensionsLight apis model.form OnDataAck ]
            )

        OnDataAck result ->
            let
                data =
                    setDataResult result model
            in
            case parseErr result data.refresh_trial of
                Authenticate ->
                    ( setDataResult NotAsked model
                    , out0 [ Ports.raiseAuthModal (uctxFromUser model.user) ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep OnQueryData 500 ] [ DoUpdateToken ] )

                OkAuth d ->
                    ( data, Out [] [] (Just ( True, List.map .id d )) )

                _ ->
                    ( data, noOut )

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


subscriptions : List (Sub Msg)
subscriptions =
    [ Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    , closeLinkTensionPanelFromJs (always OnClose)
    ]


port closeLinkTensionPanelFromJs : (() -> msg) -> Sub msg



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    { tree_data : GqlData NodesDict
    }


view : Op -> State -> Html Msg
view op (State model) =
    div
        [ id "linkTensionPanel"
        , class "side-menu"
        , classList [("off", not model.isOpen)]
        ]
        [ case model.data_result of
            Success data ->
                viewData data op model

            Failure err ->
                viewGqlErrors err

            LoadingSlowly ->
                div [ class "spinner" ] []

            _ ->
                text ""
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        ]


viewData : List TensionLight -> Op -> Model -> Html Msg
viewData data op model =
    div [] <|
        List.map
            (\t ->
                div [] [ text t.title ]
            )
            data
