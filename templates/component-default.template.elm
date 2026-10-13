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

module ${module_name} exposing (Msg(..), State, init, subscriptions, update, view)

import Auth exposing (ErrState(..), parseErr)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.Out as O exposing (noOut, out0, out1, out2)
import Dict
import Form exposing (isPostEmpty)
import Fractale.Error exposing (viewGqlErrors)
import Fractale.User exposing (UserState(..), uctxFromUser)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Loading exposing (GqlData, ModalData, RequestResult(..), withMaybeData)
import ModelSchema exposing (Post, UserCtx)
import Ports
import Schema.Enum.TensionEvent as TensionEvent
import Session exposing (Apis, GlobalCmd(..), SessionCommon)
import Time
import Utils.Cmd exposing (send, sendNow, sendSleep)

-- ------------------------------
-- M O D E L
-- ------------------------------

type State
    = State Model

type alias Model =
    { data_result : GqlData MyData -- result of any query
    , form : MyForm -- user inputs
    -- Common
    , session : SessionCommon
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    }


initModel : SessionCommon -> Model
initModel session =
    { data_result = NotAsked
    , form = initForm session.user
    -- Common
    , session = session
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }

type alias MyData = String

type alias MyForm =
    { uctx : UserCtx
    , tid : String
    , target : String
    , events_type : Maybe (List TensionEvent.TensionEvent)
    , post : Post
    }

initForm : UserState -> MyForm
initForm user =
    { uctx = uctxFromUser user
    , tid = "" -- example
    , target = "" -- example
    , events_type = Nothing
    , post = Dict.empty
    }

init : SessionCommon -> State
init session =
    initModel session |> State


-- Global methods

--isOpen_ : State -> Bool
--isOpen_ (State model) =
--    model.isOpen

-- State Controls


resetModel : Model -> Model
resetModel model =
    initModel model.session

updatePost : String -> String -> Model -> Model
updatePost field value model =
    let
        form =
            model.form
    in
    { model | form = { form | post = Dict.insert field value form.post } }

setDataResult : GqlData MyData -> Model -> Model
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
    not (isPostEmpty [ "message" ] model.form.post)

-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    =
    -- Data
      OnLoad
    | OnReset
    | OnSubmit (Time.Posix -> Msg)
    | OnChangePost String String
    | DoDataQuery Time.Posix
    | OnQueryData
    | OnDataAck (GqlData MyData)
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Common
    | NoMsg
    | LogErr String


type alias Out =
    O.Out Msg ( Bool, MyData ) -- define what data is to be returned


update :Apis -> Msg -> State -> ( State, Out )
update apis message (State model) =
    update_ apis message model
        |> Tuple.mapFirst State


update_ apis message model =
    case message of
        -- Data
        OnLoad ->
            (model, out0 [send <| OnSubmit DoDataQuery ])

        OnReset ->
            ( resetModel model, noOut )

        OnSubmit next ->
            ( model , out0 [ sendNow next ])

        OnChangePost field value ->
            ( updatePost field value model, noOut )

        OnQueryData ->
            -- Adapt your query
            ( setDataResult LoadingSlowly model
            , out0 [getData apis model.form OnDataAck]
            )

        DoDataQuery _ ->
            (model , out0 [ send OnQueryData ])

        OnDataAck result ->
            let
                data =
                    setDataResult result model
            in
            case parseErr result data.refresh_trial of
                Authenticate ->
                    ( setDataResult NotAsked model
                    , out0 [ Ports.raiseAuthModal (uctxFromUser model.session.user) ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep OnQueryData 500 ] [ DoUpdateToken ] )

                OkAuth d ->
                    ( data, O.Out [] [] (Just ( True, d )) )

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



subscriptions : State -> List (Sub Msg)
subscriptions _ =
    [ Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    ]

-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    {}


view : Op -> State -> Html Msg
view op (State model) =
    div []
        [ case model.data_result of
            Success data ->
                viewData data op model
            Failure err ->
                viewGqlErrors err
            LoadingSlowly ->
                div [class "spinner"] []
            _ ->
                text ""
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        ]

viewData : MyData -> Op -> Model -> Html Msg
viewData data _ _ =
    div []
    [
        text data
    ]



-- Placeholder: replace with your query (src/Query/).


getData : Apis -> MyForm -> (GqlData MyData -> Msg) -> Cmd Msg
getData _ _ _ =
    Cmd.none
