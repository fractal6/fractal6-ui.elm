module Components.SelectType exposing (Msg(..), State, init, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Dict exposing (Dict)
import Extra exposing (ternary, textH, upH)
import Extra.Events exposing (onClickPD)
import Extra.Views exposing (showMsg)
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionType as TensionType
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), viewGqlErrors, withMaybeData)
import Maybe exposing (withDefault)
import ModelCommon exposing (Ev, TensionForm, UserState(..), initTensionForm, uctxFromUser)
import ModelCommon.View exposing (tensionType2String, tensionTypeColor, tensionTypeIcon)
import ModelSchema exposing (..)
import Ports
import Query.PatchTension exposing (patchLiteral)
import Session exposing (Apis, GlobalCmd(..))
import Text as T
import Time


type State
    = State Model


type alias Model =
    { user : UserState
    , isOpen : Bool
    , type_orig : TensionType.TensionType
    , data_result : GqlData IdPayload -- result of any query
    , form : TensionForm -- user inputs

    -- Common
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    }


initModel : String -> UserState -> Model
initModel tid user =
    { user = user
    , isOpen = False
    , data_result = NotAsked
    , type_orig = TensionType.Operational
    , form = initTensionForm tid user

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }


init : String -> UserState -> State
init tid user =
    initModel tid user |> State



-- Global methods


isOpen_ : State -> Bool
isOpen_ (State model) =
    model.isOpen



--- State Controls


open : TensionType.TensionType -> Model -> Model
open type_ model =
    { model | isOpen = True, type_orig = type_ }


close : Model -> Model
close model =
    { model | isOpen = False }


reset : Model -> Model
reset model =
    initModel model.form.id model.user


updatePost : String -> String -> Model -> Model
updatePost field value model =
    let
        form =
            model.form
    in
    { model | form = { form | post = Dict.insert field value form.post } }


setDataResult : GqlData IdPayload -> Model -> Model
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
    model.form.type_ /= Nothing && model.form.type_ /= Just model.type_orig


isSendable : Model -> Bool
isSendable model =
    -- when the form can be submited
    hasData model



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = OnOpen TensionType.TensionType
    | OnClose ModalData
    | OnCloseSafe String String
    | OnReset
      -- Data
    | OnChangePost String String
    | OnSetType TensionType.TensionType
    | DoPatchData
    | OnSubmit (Time.Posix -> Msg)
    | OnPatchData Time.Posix
    | OnDataAck (GqlData IdPayload)
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
    , result : Maybe ( Bool, TensionType.TensionType ) -- define what data is to be returned
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
        OnOpen type_ ->
            ( open type_ model
            , out0 [ Ports.open_modal "SelectTypeModal" ]
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
                , out0 [ send (DoModalConfirmOpen (OnClose { reset = True, link = link }) { message = Nothing, txts = [ ( T.confirmUnsaved, onCloseTxt ) ] }) ]
                )

        -- Data
        OnChangePost field value ->
            ( updatePost field value model, noOut )

        OnSetType type_ ->
            let
                form =
                    model.form
            in
            ( { model | form = { form | type_ = Just type_ } }, noOut )

        OnPatchData time ->
            let
                form =
                    model.form

                newForm =
                    { form
                        | post =
                            model.form.post
                                |> Dict.insert "createdAt" (fromTime time)
                        , events =
                            [ Ev TensionEvent.TypeUpdated
                                (TensionType.toString model.type_orig)
                                (model.form.type_ |> withDefault TensionType.Operational |> TensionType.toString)
                            ]
                    }
            in
            ( { model | form = newForm }
            , out0 [ send DoPatchData ]
            )

        OnSubmit next ->
            ( model
            , out0 [ sendNow next ]
            )

        DoPatchData ->
            ( setDataResult LoadingSlowly model
            , out0 [ patchLiteral apis model.form OnDataAck ]
            )

        OnDataAck result ->
            let
                data =
                    setDataResult result model
            in
            case parseErr result data.refresh_trial of
                Authenticate ->
                    ( setDataResult NotAsked model
                    , out0 [ Ports.raiseAuthModal data.form.uctx ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep DoPatchData 500 ] [ DoUpdateToken ] )

                OkAuth _ ->
                    ( data
                    , Out [] [] (Just ( True, withDefault TensionType.Operational model.form.type_ ))
                    )

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
        [ id "SelectTypeModal"
        , class "modal is-light modal-fx-fadeIn"
        , classList [ ( "is-active", model.isOpen ) ]
        , attribute "data-modal-close" "closeModalFromJs"
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "SelectTypeModal"
            , onClick (OnCloseSafe "" "")
            ]
            []
        , div [ class "modal-content" ]
            [ case model.data_result of
                Success _ ->
                    div [ class "notification is-success-light" ]
                        [ button [ class "delete", onClick (OnCloseSafe "" "") ] []
                        , A.icon1 "icon-check icon-2x has-text-success" " "
                        , text T.tensionType_action_success
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
            [ div [ class "modal-card-title is-size-6 has-text-weight-semibold" ]
                [ text T.changeTensionType ]
            ]
        , div [ class "modal-card-body" ]
            [ showMsg "selectType-0" "is-info is-light" "icon-info" T.tensionTypeHeader T.tensionTypeDoc
            , div [ class "level buttonRadio" ] <|
                List.map
                    (\tensionType ->
                        let
                            isActive =
                                Just tensionType == model.form.type_
                        in
                        div [ class "level-item" ]
                            [ div
                                [ class <| "button has-text-light " ++ tensionTypeColor "background" tensionType
                                , classList [ ( "is-active", isActive ), ( "is-selected", isActive ) ]
                                , onClick (OnSetType tensionType)
                                ]
                                [ A.icon1 (tensionTypeIcon tensionType) (tensionType2String tensionType) ]
                            ]
                    )
                    TensionType.list
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
                        [ text T.cancel ]
                    ]
                , div [ class "level-right" ]
                    [ button
                        ([ class "button is-light is-success"
                         , classList [ ( "is-loading", isLoading ) ]
                         , disabled (not (isSendable model) || isLoading)
                         ]
                            ++ [ onClick (OnSubmit <| OnPatchData) ]
                        )
                        [ text T.updateType ]
                    ]
                ]
            ]
        ]
