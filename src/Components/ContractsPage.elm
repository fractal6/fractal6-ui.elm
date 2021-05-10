module Components.ContractsPage exposing (Msg(..), State, init, subscriptions, update, view)

import Auth exposing (AuthState(..), doRefreshToken)
import Components.Loading as Loading exposing (GqlData, ModalData, RequestResult(..), viewGqlErrors, withMaybeData)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm)
import Date exposing (formatTime)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Extra.Events exposing (onClickPD)
import Form exposing (isPostEmpty, isPostSendable)
import Fractal.Enum.ContractType as ContractType
import Fractal.Enum.TensionEvent as TensionEvent
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, table, tbody, td, text, textarea, tfoot, th, thead, tr, ul)
import Html.Attributes exposing (attribute, checked, class, classList, colspan, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Icon as I
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (Apis, GlobalCmd(..), UserState(..))
import ModelCommon.View exposing (viewUsernameLink)
import ModelSchema exposing (..)
import Ports
import Query.QueryContract exposing (getContracts)
import Text as T exposing (textH, textT, upH)
import Time


type State
    = State Model


type alias Model =
    { user : UserState
    , data_result : GqlData MyData -- result of any query
    , onClick_result : GqlData IdPayload
    , form : MyForm -- user inputs

    -- Common
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    }


initModel : UserState -> Model
initModel user =
    { user = user
    , data_result = NotAsked
    , onClick_result = NotAsked
    , form = initForm user

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }


type alias MyData =
    List Contract


type alias MyForm =
    { uctx : UserCtx
    , tid : String
    , cid : Maybe String
    , page : Int
    , page_len : Int
    , events_type : Maybe (List TensionEvent.TensionEvent)
    , post : Post
    }


initForm : UserState -> MyForm
initForm user =
    { uctx =
        case user of
            LoggedIn uctx ->
                uctx

            LoggedOut ->
                UserCtx "" Nothing (UserRights False False) []
    , tid = "" -- example
    , cid = Nothing
    , page = 0
    , page_len = 10
    , events_type = Nothing
    , post = Dict.empty
    }


init : UserState -> State
init user =
    initModel user |> State



-- Global methods
--isOpen_ : State -> Bool
--isOpen_ (State model) =
--    model.isOpen
--- State Controls


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
    isPostEmpty [ "message" ] model.form.post == False



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = -- Data
      OnLoad String (Maybe String)
    | OnChangePost String String
    | DoQueryData
    | OnSubmit (Time.Posix -> Msg)
    | OnDataQuery Time.Posix
    | OnDataAck (GqlData MyData)
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
        -- Data
        OnLoad tid cid ->
            let
                form =
                    model.form
            in
            ( { model | form = { form | tid = tid, cid = cid } }, out0 [ send (OnSubmit <| OnDataQuery) ] )

        OnChangePost field value ->
            ( updatePost field value model, noOut )

        DoQueryData ->
            -- Adapt your query
            ( model, out0 [ getContracts apis.gql model.form OnDataAck ] )

        OnSubmit next ->
            ( model
            , out0 [ sendNow next ]
            )

        OnDataQuery time ->
            ( model
            , out0 [ send DoQueryData ]
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
                    ( { data | refresh_trial = i }, out2 [ sendSleep DoQueryData 500 ] [ DoUpdateToken ] )

                OkAuth d ->
                    ( data, Out [] [] (Just ( True, d )) )

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
    []



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
                div [ class "spinner" ] []

            _ ->
                text ""
        ]


viewData : MyData -> Op -> Model -> Html Msg
viewData data op model =
    let
        headers =
            [ "Event", "Validation", "Author", "Opened", "" ]
    in
    table [ class "table is-fullwidth tensionContracts" ]
        [ thead [ class "is-size-7" ]
            [ tr [] (headers |> List.map (\x -> th [ class "has-text-weight-light" ] [ textH x ]))
            ]
        , data
            |> List.map (\d -> viewRow d model)
            |> List.concat
            |> tbody []
        ]


viewRow : Contract -> Model -> List (Html Msg)
viewRow d model =
    [ tr [ class "mediaBox" ]
        -- onClick (d.id)
        [ td [] [ a [] [ viewContractEvent d ] ]
        , td [] [ viewContractType d ]
        , td [ class "has-links-light" ] [ viewUsernameLink d.createdBy.username ]
        , td [] [ text (formatTime d.createdAt) ]

        -- participant
        -- n comments icons
        , td [ class "is-aligned-right is-size-7", attribute "style" "min-width: 6rem;" ]
            [ span
                [ class "button-light"

                --, onClick <| DoModalConfirmOpen (Submit <| SubmitDeleteLabel d.id) [ ( T.confirmDeleteLabel, "" ), ( d.name, "is-strong" ), ( "?", "" ) ]
                ]
                [ text "Cancel" ]
            ]
        ]
    ]
        ++ (case model.onClick_result of
                Failure err ->
                    [ td [] [ viewGqlErrors err ] ]

                _ ->
                    []
           )


viewContractEvent : Contract -> Html Msg
viewContractEvent d =
    span []
        [ case d.event.event_type of
            TensionEvent.Moved ->
                textH "tension movement"

            _ ->
                text ""
        ]


viewContractType : Contract -> Html Msg
viewContractType d =
    span []
        [ case d.contract_type of
            ContractType.AnyCoordoDual ->
                text "Coordo validation"

            _ ->
                text ""
        ]
