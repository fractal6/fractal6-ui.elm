module Components.ConfirmContract exposing (Msg(..), State, init, subscriptions, update, view)

import Auth exposing (AuthState(..), doRefreshToken)
import Components.Loading as Loading exposing (GqlData, ModalData, RequestResult(..), viewGqlErrors, withMaybeData, withMaybeDataMap)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Extra.Events exposing (onClickPD)
import Extra.Views exposing (showMsg)
import Form exposing (isPostEmpty, isPostSendable)
import Fractal.Enum.ContractStatus as ContractStatus
import Fractal.Enum.ContractType as ContractType
import Fractal.Enum.TensionEvent as TensionEvent
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, form, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Icon as I
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (Apis, GlobalCmd(..), UserState(..))
import ModelCommon.Codecs exposing (nid2eor)
import ModelCommon.View exposing (viewTensionArrow)
import ModelSchema exposing (..)
import Ports
import Query.AddContract exposing (addOneContract)
import Text as T exposing (textH, textT, upH)
import Time


type State
    = State Model


type alias Model =
    { user : UserState
    , isOpen : Bool
    , data_result : GqlData Contract -- result of any query
    , contract : Maybe Contract
    , form : ContractForm -- user inputs

    -- Common
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    }


initModel : UserState -> Model
initModel user =
    { user = user
    , isOpen = False
    , data_result = NotAsked
    , contract = Nothing
    , form = initForm user

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }


type alias ContractForm =
    { uctx : UserCtx
    , tid : String
    , status : ContractStatus.ContractStatus
    , contract_type : ContractType.ContractType
    , event : EventFragment

    --, candidate:
    , participants : Maybe (List Vote)
    , post : Post
    }


initForm : UserState -> ContractForm
initForm user =
    { uctx =
        case user of
            LoggedIn uctx ->
                uctx

            LoggedOut ->
                UserCtx "" Nothing (UserRights False False) []
    , tid = "" -- example
    , status = ContractStatus.Open
    , contract_type = ContractType.AnyCoordoDual
    , event = initEventFragment
    , participants = Nothing
    , post = Dict.empty
    }


updateFormFromData : Contract -> ContractForm -> ContractForm
updateFormFromData c f =
    { f
        | tid = c.tension.id
        , status = c.status
        , contract_type = c.contract_type
        , event = c.event
        , participants = c.participants
    }


init : UserState -> State
init user =
    initModel user |> State



-- Global methods


isOpen_ : State -> Bool
isOpen_ (State model) =
    model.isOpen



--- State Controls


open : Post -> Maybe Contract -> Model -> Model
open post c_m model =
    let
        newForm =
            case c_m of
                Just c ->
                    updateFormFromData c model.form

                Nothing ->
                    model.form

        upf f =
            case Dict.get "message" post of
                Just m ->
                    { f | post = Dict.insert "message" m f.post }

                Nothing ->
                    f
    in
    { model | isOpen = True, contract = c_m, form = upf newForm }


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


setDataResult : GqlData Contract -> Model -> Model
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
    isPostEmpty [ "message" ] model.form.post == False



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = OnOpen Post (Maybe Contract)
    | OnClose ModalData
    | OnCloseSafe String String
    | OnReset
      -- Data
    | OnChangePost String String
    | DoAddContract
    | OnSubmit (Time.Posix -> Msg)
    | OnDataQuery Time.Posix
    | OnDataAck (GqlData Contract)
      -- Confirm Modal
    | DoModalConfirmOpen Msg (List ( String, String ))
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Common
    | NoMsg
    | LogErr String


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd

    --Bool : Does the parent modal should be closed
    --Contract : the result Contract
    , result : Maybe ( Bool, Contract )
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
        OnOpen post c_m ->
            ( open post c_m model
            , out0 [ Ports.open_modal "ConfirmContractModal" ]
            )

        OnClose data ->
            let
                cmds =
                    ternary data.reset [ sendSleep OnReset 333 ] []

                gcmds =
                    ternary (data.link /= "") [ DoNavigate data.link ] []

                res =
                    withMaybeDataMap (\r -> ( True, r )) model.data_result
            in
            ( close model, Out ([ Ports.close_modal ] ++ cmds) gcmds res )

        OnReset ->
            ( reset model, noOut )

        OnCloseSafe link onCloseTxt ->
            if canExitSafe model then
                ( model, out0 [ send (OnClose { reset = True, link = link }) ] )

            else
                ( model
                , out0 [ send (DoModalConfirmOpen (OnClose { reset = True, link = link }) [ ( upH T.confirmUnsaved, onCloseTxt ) ]) ]
                )

        -- Data
        OnChangePost field value ->
            ( updatePost field value model, noOut )

        DoAddContract ->
            -- Adapt your query
            ( model, out0 [ addOneContract apis.gql model.form OnDataAck ] )

        OnSubmit next ->
            ( model
            , out0 [ sendNow next ]
            )

        OnDataQuery time ->
            ( model |> updatePost "createdAt" (fromTime time)
            , out0 [ send DoAddContract ]
            )

        OnDataAck result ->
            let
                data =
                    setDataResult result model
            in
            case doRefreshToken result data.refresh_trial of
                Authenticate ->
                    ( setDataResult NotAsked model
                    , out1 [ DoAuth data.form.uctx ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep DoAddContract 500 ] [ DoUpdateToken ] )

                OkAuth d ->
                    ( data, Out [] [] (Just ( False, d )) )

                NoAuth ->
                    ( data, noOut )

        -- Confirm Modal
        DoModalConfirmOpen msg txts ->
            ( { model | modal_confirm = ModalConfirm.open msg txts model.modal_confirm }, noOut )

        DoModalConfirmClose _ ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, noOut )

        DoModalConfirmSend ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, out0 [ send model.modal_confirm.msg ] )

        -- Common
        NoMsg ->
            ( model, noOut )

        LogErr err ->
            ( model, out0 [ Ports.logErr err ] )


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
        [ id "ConfirmContractModal"
        , class "modal modal-fx-fadeIn"
        , classList [ ( "is-active", model.isOpen ) ]
        , attribute "data-modal-close" "closeModalFromJs"
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "ConfirmContractModal"
            , onClick (OnCloseSafe "" "")
            ]
            []
        , div [ class "modal-content" ]
            [ case model.data_result of
                Success data ->
                    let
                        link =
                            "todo"

                        --Route.Tension_Dynamic_Dynamic { param1 = nid2rootid model.form.target.nameid, param2 = res.id } |> toHref
                    in
                    div [ class "box is-light" ]
                        [ I.icon1 "icon-check icon-2x has-text-success" " "
                        , text "New contract created."
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
        , button [ class "modal-close is-large", onClick (OnCloseSafe "" "") ] []
        ]


viewModalContent : Op -> State -> Html Msg
viewModalContent op (State model) =
    let
        message =
            Dict.get "message" model.form.post |> withDefault ""

        isLoading =
            model.data_result == LoadingSlowly

        isSendable =
            True
    in
    div [ class "modal-card" ]
        [ div [ class "modal-card-head has-background-warning" ]
            [ div [ class "modal-card-title is-size-6 has-text-weight-semibold" ]
                [ textH T.newContract ]
            ]
        , div [ class "modal-card-body" ]
            [ showMsg "0" "is-info" "icon-info" T.contractInfoHeader T.contractInfo
            , showContractForm model.form
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
                , p [ class "help-label" ] [ textH T.tensionMessageHelp ]
                ]
            ]
        , div [ class "modal-card-foot", attribute "style" "display: block;" ]
            [ case model.data_result of
                Failure err ->
                    div [ class "field" ] [ viewGqlErrors err ]

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
                    [ button
                        ([ class "button is-light is-success"
                         , classList [ ( "is-loading", isLoading ) ]
                         , disabled (not isSendable || isLoading)
                         ]
                            ++ [ onClick (OnSubmit <| OnDataQuery) ]
                        )
                        [ textH T.createContract ]
                    ]
                ]
            ]
        ]


showContractForm : ContractForm -> Html Msg
showContractForm f =
    form [ class "box is-light form" ]
        [ div [ class "field is-horizontal" ]
            [ div [ class "field-label" ] [ label [ class "label" ] [ text "contract type" ] ]
            , div [ class "field-body" ]
                [ div [ class "field is-narro" ]
                    [ input [ class "input", value "Dual coordo", disabled True ] []
                    ]
                ]
            ]
        , div [ class "field is-horizontal" ]
            [ div [ class "field-label" ] [ label [ class "label" ] [ text "event" ] ]
            , div [ class "field-body" ] <|
                case f.event.event_type of
                    TensionEvent.Moved ->
                        let
                            emitter =
                                f.event.old |> withDefault "unknown" |> nid2eor

                            receiver =
                                f.event.new |> withDefault "unkown" |> nid2eor
                        in
                        [ div [ class "field is-narrow" ]
                            [ input [ class "input", value "move tension", disabled True ] []
                            ]
                        , viewTensionArrow "is-pulled-right" emitter receiver
                        ]

                    _ ->
                        [ text "not implemented" ]
            ]
        ]
