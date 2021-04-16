module Components.MoveTension exposing (Msg, State, init, subscriptions, update, view)

import Auth exposing (AuthState(..), doRefreshToken)
import Components.Loading as Loading exposing (GqlData, ModalData, RequestResult(..), viewGqlErrors)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Fractal.Enum.TensionEvent as TensionEvent
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Icon as I
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (Apis, GlobalCmd(..), UserState(..))
import ModelSchema exposing (..)
import Ports
import Query.PatchTension exposing (moveTension)
import Text as T exposing (textH, textT, upH)
import Time


type State
    = State Model


type alias Model =
    { user : UserState
    , isOpen : Bool
    , move_result : GqlData IdPayload
    , orga_data : GqlData NodesData
    , target : String -- keep origin target
    , form : MoveForm

    -- Common
    , refresh_trial : Int
    , modal_confirm : ModalConfirm Msg
    }


type alias MoveForm =
    { uctx : UserCtx
    , tid : String
    , target : String
    , events_type : Maybe (List TensionEvent.TensionEvent)
    , post : Post
    }


initForm : UserState -> MoveForm
initForm user =
    { uctx =
        case user of
            LoggedIn uctx ->
                uctx

            LoggedOut ->
                UserCtx "" Nothing (UserRights False False) []
    , tid = ""
    , target = ""
    , events_type = Nothing
    , post = Dict.empty
    }


init : UserState -> State
init user =
    initModel user |> State


initModel : UserState -> Model
initModel user =
    { user = user
    , isOpen = False
    , move_result = NotAsked
    , orga_data = Loading
    , form = initForm user
    , target = ""

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }



-- Global methods


isOpen_ : State -> Bool
isOpen_ (State model) =
    model.isOpen



--- State Controls


open : String -> String -> GqlData NodesData -> Model -> Model
open tid target odata model =
    let
        form =
            model.form
    in
    { model | isOpen = True, target = target, orga_data = odata, form = { form | tid = tid } }


close : Model -> Model
close model =
    { model | isOpen = False }


reset : Model -> Model
reset model =
    initModel model.user


setTarget : String -> Model -> Model
setTarget target model =
    let
        form =
            model.form
    in
    { model | form = { form | target = target } }


setMoveResult : GqlData IdPayload -> Model -> Model
setMoveResult result model =
    { model | move_result = result }



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = DoMoveTension
    | OnOpen String String (GqlData NodesData)
    | OnClose ModalData
    | OnCloseSafe String String
    | OnReset
      -- Data
    | OnChangeTarget String
    | OnSubmit (Time.Posix -> Msg)
    | OnMove String Time.Posix
    | OnMoveAck (GqlData IdPayload)
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
    , result : Maybe ( Bool, IdPayload ) -- define what data is to be returned
    }


noOut : Out
noOut =
    Out [] [] Nothing


out1 : List (Cmd Msg) -> Out
out1 cmds =
    Out cmds [] Nothing


out2 : List GlobalCmd -> Out
out2 cmds =
    Out [] cmds Nothing


update : Apis -> Msg -> State -> ( State, Out )
update apis message (State model) =
    update_ apis message model
        |> Tuple.mapFirst State


update_ apis message model =
    case message of
        DoMoveTension ->
            ( model, out1 [ moveTension apis.gql model.form OnMoveAck ] )

        OnOpen tid target odata ->
            ( open tid target odata model
            , out1 [ Ports.open_modal "MoveTensionModal" ]
            )

        OnClose data ->
            let
                cmds =
                    ternary data.reset [ sendSleep OnReset 333 ] []

                gcmds =
                    ternary (data.link /= "") [ DoNavigate data.link ] []
            in
            ( close model, Out ([ Ports.close_modal ] ++ cmds) gcmds Nothing )

        OnReset ->
            ( reset model, noOut )

        OnCloseSafe link onCloseTxt ->
            let
                -- Condition to close safely (e.g. empty form data)
                doClose =
                    True
            in
            if doClose then
                ( model
                , out1 [ send (DoModalConfirmOpen (OnClose { reset = True, link = link }) [ ( upH T.confirmUnsaved, onCloseTxt ) ]) ]
                )

            else
                ( model, out1 [ send (OnClose { reset = True, link = link }) ] )

        -- Data
        OnChangeTarget target ->
            ( setTarget target model, noOut )

        OnSubmit next ->
            ( model
            , out1 [ sendNow next ]
            )

        OnMove source time ->
            let
                form =
                    model.form

                newForm =
                    { form
                        | post =
                            Dict.insert "createdAt" (fromTime time) form.post
                                |> Dict.union
                                    (Dict.fromList
                                        [ ( "old", source )
                                        , ( "new", form.target )
                                        ]
                                    )
                    }
            in
            ( { model | form = newForm }
            , out1 [ send DoMoveTension ]
            )

        OnMoveAck result ->
            let
                data =
                    setMoveResult result model
            in
            case doRefreshToken result data.refresh_trial of
                Authenticate ->
                    ( setMoveResult NotAsked model
                    , out2 [ DoAuth data.form.uctx ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, Out [ sendSleep DoMoveTension 500 ] [ DoUpdateToken ] Nothing )

                OkAuth _ ->
                    ( data, Out [] [] Nothing )

                NoAuth ->
                    ( data, noOut )

        -- Confirm Modal
        DoModalConfirmOpen msg txts ->
            ( { model | modal_confirm = ModalConfirm.open msg txts model.modal_confirm }, noOut )

        DoModalConfirmClose _ ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, noOut )

        DoModalConfirmSend ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, out1 [ send model.modal_confirm.msg ] )

        -- Common
        NoMsg ->
            ( model, noOut )

        LogErr err ->
            ( model, out1 [ Ports.logErr err ] )


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
        [ id "MoveTensionModal"
        , class "modal modal-fx-fadeIn"
        , classList [ ( "is-active", model.isOpen ) ]
        , attribute "data-modal-close" "closeModalFromJs"
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "MoveTensionModal"
            , onClick (OnCloseSafe "" "")
            ]
            []
        , div [ class "modal-content" ] [ viewModalContent op (State model) ]
        , button [ class "modal-close is-large", onClick (OnCloseSafe "" "") ] []
        ]


viewModalContent : Op -> State -> Html Msg
viewModalContent op (State model) =
    div []
        [ span [] [ text "New tension receiver: " ]
        , button [] [ text "select a destination" ]
        , case model.move_result of
            Success data ->
                div [ class "box is-light", onClick (OnClose { reset = True, link = "" }) ]
                    [ I.icon1 "icon-check icon-2x has-text-success" " "
                    , text "data: "
                    , text data.id
                    ]

            Failure err ->
                viewGqlErrors err

            NotAsked ->
                text ""

            _ ->
                div [ class "box spinner" ] []

        -- footer: submit button
        ]
