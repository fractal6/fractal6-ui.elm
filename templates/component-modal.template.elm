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

module ${module_name} exposing (Msg(..), State, init, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Loading exposing (GqlData, ModalData, RequestResult(..), isFailure, isSuccess, withMaybeData)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Dict exposing (Dict)
import Extra exposing (space_, ternary, textH, unwrap, unwrap2)
import Extra.Events exposing (onClickPD)
import Form exposing (isPostEmpty)
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button,  div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span,  text, textarea,  ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import Bulk exposing (UserState(..), uctxFromUser)
import Bulk.Error exposing (viewGqlErrors)
import ModelSchema exposing (MyData, Post, UserCtx)
import Ports
import Query.AddData exposing (getData)
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
    , data_result : GqlData MyData -- result of any query
    , form : MyForm -- user inputs
    -- Common
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    }


initModel : UserState -> Model
initModel user =
    { user = user
    , isActive = False
    , isActive2 = False
    , data_result = NotAsked
    , form = initForm user
    -- Common
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

init : UserState -> State
init user =
    initModel user |> State


-- Global methods

isActive_ : State -> Bool
isActive_ (State model) =
    model.isActive

-- State Controls


resetModel : Model -> Model
resetModel model =
    initModel model.user

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


isSendable : Model -> Bool
isSendable model =
    -- when the form can be submited
    False



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = SetIsActive2 Bool
    | OnOpen
    | OnClose ModalData
    | OnCloseSafe String String
    | OnReset
    | OnSubmit (Time.Posix -> Msg)
    -- Data
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
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe ( Bool, MyData ) -- define what data is to be returned
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
                ( { model | isActive = model.isActive2 }, out0 [ Ports.open_modal "${module_basename}Modal" ] )

            else
                ( { model | isActive2 = model.isActive }, noOut )
        OnOpen ->
            ( {model | isActive2 = True}
            , out0 [sendSleep (SetIsActive2 True) 10, Ports.open_modal "${module_basename}Modal" ]
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
        OnChangePost field value ->
            ( updatePost field value model, noOut )

        OnSubmit next ->
            ( model , out0 [ sendNow next ])

        DoDataQuery time ->
            (model , out0 [ send OnQueryData ])

        OnQueryData ->
            -- Adapt your query
            ( setDataResult LoadingSlowly model
            , out0 [getData apis model.form OnDataAck]
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
                    ( data, Out [] [] (Just ( True, d )) )

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
        [ id "${module_basename}Modal"
        , class "modal is-light modal-fx-fadeIn"
        , classList [ ( "is-active", model.isActive ) ]
        , attribute "data-modal-close" "closeModalFromJs"
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "${module_basename}Modal"
            , onClick (OnCloseSafe "" "")
            ]
            []
        , div [ class "modal-content" ] [
            case model.data_result of
                Success data ->
                    let
                        link = data.id -- example @tofix
                    in
                    div [ class "notification is-success-light"]
                        [ button [ class "delete", onClick (OnCloseSafe "" "") ] []
                        , div [ class "is-flex is-align-items-center" ] [
                              A.icon1 "icon-check icon-2x has-text-success" " "
                            , text "data queried..."
                            , text space_
                            , a
                                [ href link
                                , onClickPD (OnClose { reset = True, link = link })
                                , target "_blank"
                                ]
                                [ textH T.checkItOut_masc ]
                            ]
                        ]
                _ ->
                    viewModalContent op model
                ]
        --, button [ class "modal-close is-large", onClick (OnCloseSafe "" "") ] []
        ]

viewModalContent : Op -> Model -> Html Msg
viewModalContent op model =
    let
        message =
            Dict.get "message" model.form.post

        isLoading =
            Loading.isLoading model.data_result
    in
    div [ class "modal-card" ]
        [ div [ class "modal-card-head" ]
            [ div [ class "modal-card-title is-wrapped is-size-6 has-text-weight-semibold" ]
                [ textH "EditMe" ]
            ]
        , div [ class "modal-card-body" ]
            [ div [ class "field" ]
                [ div [ class "label" ] [  text "EditMe"  ]
                , div [ class "control" ]
                    [ textarea
                        [ class "textarea"
                        , rows 5
                        , placeholder T.leaveCommentOpt
                        , value (withDefault "" message)
                        , onInput <| OnChangePost "message"
                        ]
                        []
                    , p [ class "help-label" ] [ textH T.tensionMessageHelp ]
                    ]
                ]
            ]
        , div [ class "modal-card-foot", attribute "style" "display: block;" ]
            [ case model.data_result of
                Failure err ->
                    div [ class "field" ] [ viewGqlErrors err ]

                _ ->
                    text ""
            , div [ class "field level is-mobile" ]
                [ div [ class "level-left" ]
                    [ button
                        [ class "button is-light"
                        , onClick (OnCloseSafe "" "")
                        ]
                        [ textH T.cancel ]
                    ]
                , div [ class "level-right" ]
                    [ button
                        ([ class "button is-light is-success"
                         , classList [ ( "is-loading", isLoading ) ]
                         , disabled (not (isSendable model) || isLoading)
                         ]
                            ++ [ onClick (OnSubmit <| DoDataQuery ) ]
                        )
                        [ textH T.submit ]
                    ]
                ]
            ]
        ]
