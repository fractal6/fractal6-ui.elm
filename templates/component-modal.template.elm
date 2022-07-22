module ${module_name} exposing (Msg(..), State, init, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Loading exposing (GqlData, ModalData, RequestResult(..), viewGqlErrors, withMaybeData, isFailure, isSuccess)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Dict exposing (Dict)
import Extra exposing (ternary, textH, upH)
import Extra.Events exposing (onClickPD)
import Form exposing (isPostEmpty)
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button,  div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span,  text, textarea,  ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (UserState(..), uctxFromUser)
import ModelSchema exposing (..)
import Ports
import Query.AddData exposing (getData)
import Session exposing (Apis, GlobalCmd(..))
import Text as T
import Time

type State
    = State Model

type alias Model =
    { user : UserState
    , isOpen : Bool
    , data_result : GqlData MyData -- result of any query
    , form : MyForm -- user inputs
    -- Common
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    }


initModel : UserState -> Model
initModel user =
    { user = user
    , isOpen = False
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

isOpen_ : State -> Bool
isOpen_ (State model) =
    model.isOpen

--- State Controls

open : Model -> Model
open model =
    { model | isOpen = True}

close : Model -> Model
close model =
    { model | isOpen = False }

reset : Model -> Model
reset model =
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
    (hasData model && withMaybeData model.data_result == Nothing) == False


hasData : Model -> Bool
hasData model =
    -- When you can commit (e.g. empty form data)
    (isPostEmpty [ "message" ] model.form.post) == False


isSendable : Model -> Bool
isSendable model =
    -- when the form can be submited
    False



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = OnOpen
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
        OnOpen ->
            ( open model
            , out0 [ Ports.open_modal "${module_basename}Modal" ]
            )

        OnClose data ->
            let
                cmds =
                    ternary data.reset [ sendSleep OnReset 333 ] []

                gcmds =
                    ternary (data.link /= "") [ DoNavigate data.link ] []
            in
            ( close model, out2 ([ Ports.close_modal ] ++ cmds) gcmds )

        OnReset ->
            ( reset model, noOut )

        OnCloseSafe link onCloseTxt ->
            if canExitSafe model then
                ( model, out0 [ send (OnClose { reset = True, link = link }) ] )

            else
                ( model
                , out0 [ send (DoModalConfirmOpen (OnClose { reset = True, link = link }) {message=Nothing , txts=[ ( upH T.confirmUnsaved, onCloseTxt ) ]}) ]
                )

        -- Data
        OnChangePost field value ->
            ( updatePost field value model, noOut )

        OnSubmit next ->
            ( model , out0 [ sendNow next ])

        DoDataQuery time ->
            -- setup your form (time)
            (model
            , out0 [ send OnQueryData ]
            )

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



subscriptions :  List (Sub Msg)
subscriptions =
    [ Ports.mcPD Ports.closeModalFromJs LogErr OnClose
    , Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    ]

-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    {}


view : Op -> State -> Html Msg
view op (State model) =
    div []
        [ viewModal op (State model)
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        ]


viewModal : Op -> State -> Html Msg
viewModal op (State model) =
    div
        [ id "${module_basename}Modal"
        , class "modal is-light modal-fx-fadeIn"
        , classList [ ( "is-active", model.isOpen ) ]
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
                        , A.icon1 "icon-check icon-2x has-text-success" " "
                        , text "data queried..."
                        , text " "
                        , a
                            [ href link
                            , onClickPD (OnClose { reset = True, link = link })
                            , target "_blank"
                            ]
                            [ textH T.checkItOut ]
                        ]
                _ ->
                    viewModalContent op (State model)
                ]
        --, button [ class "modal-close is-large", onClick (OnCloseSafe "" "") ] []
        ]

viewModalContent : Op -> State -> Html Msg
viewModalContent op (State model) =
    let

        isLoading =
            model.data_result == LoadingSlowly

    in
    div [ class "modal-card" ]
        [ div [ class "modal-card-head" ]
            [ div [ class "modal-card-title is-wrapped is-size-6 has-text-weight-semibold" ]
                [ textH "EditMe" ]
            ]
        , div [ class "modal-card-body" ]
            [ div [ class "field" ]
                [ div [ class "control" ]
                    [ span [] [ text "EditMe" ] ]
                ]
            , div [ class "field" ]
                [ div [ class "control" ]
                    [ textarea
                        [ class "textarea"
                        , rows 5
                        , placeholder (upH T.leaveCommentOpt)
                        , value message
                        , onInput <| OnChangePost "message"
                        ]
                        []
                    ]
                , p [ class "help-label" ] [ textH T.tensionMessageHelp ]
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
                         , disabled (not isSendable || isLoading)
                         ]
                            ++ [ onClick (OnSubmit <| DoDataQuery ) ]
                        )
                        [ textH T.submit ]
                    ]
                ]
            ]
        ]
