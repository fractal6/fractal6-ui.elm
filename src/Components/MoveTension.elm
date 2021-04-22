module Components.MoveTension exposing (Msg(..), State, init, subscriptions, update, view)

import Auth exposing (AuthState(..), doRefreshToken)
import Components.Loading as Loading exposing (GqlData, ModalData, RequestResult(..), viewGqlErrors, withMaybeData)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Form exposing (isPostEmpty, isPostSendable)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
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
import ModelCommon.Codecs exposing (nid2type, nodeIdCodec)
import ModelCommon.View exposing (roleColor)
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
    , target : String -- keep origin target
    , blob : Maybe Blob
    , form : MoveForm

    -- Common
    , refresh_trial : Int
    , modal_confirm : ModalConfirm Msg
    }


type alias MoveForm =
    { uctx : UserCtx
    , tid : String
    , target : Node
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
    , target = initNode
    , events_type = Just [ TensionEvent.Moved ]
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
    , form = initForm user
    , target = ""
    , blob = Nothing

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }



-- Global methods


isOpen_ : State -> Bool
isOpen_ (State model) =
    model.isOpen



--- State Controls


open : String -> String -> Maybe Blob -> Model -> Model
open tid target blob_m model =
    let
        form =
            model.form
    in
    { model | isOpen = True, target = target, blob = blob_m, form = { form | tid = tid } }


close : Model -> Model
close model =
    { model | isOpen = False }


reset : Model -> Model
reset model =
    initModel model.user


setTarget : Node -> Model -> Model
setTarget node model =
    let
        form =
            model.form
    in
    { model | form = { form | target = node } }


updatePost : String -> String -> Model -> Model
updatePost field value model =
    let
        form =
            model.form
    in
    { model | form = { form | post = Dict.insert field value form.post } }


setMoveResult : GqlData IdPayload -> Model -> Model
setMoveResult result model =
    { model | move_result = result }



-- utils


canExitSafe : Model -> Bool
canExitSafe model =
    (hasData model && withMaybeData model.move_result == Nothing) == False


hasData : Model -> Bool
hasData model =
    (isPostEmpty [ "message" ] model.form.post && model.form.target.nameid == "") == False



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = DoMoveTension
    | OnOpen String String (Maybe Blob)
    | OnClose ModalData
    | OnCloseSafe String String
    | OnReset
      -- Data
    | OnChangePost String String
    | OnChangeTarget Node
    | OnSubmit (Time.Posix -> Msg)
    | OnMove Time.Posix
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
        OnOpen tid target blob_m ->
            ( open tid target blob_m model
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
            if canExitSafe model then
                ( model, out1 [ send (OnClose { reset = True, link = link }) ] )

            else
                ( model
                , out1 [ send (DoModalConfirmOpen (OnClose { reset = True, link = link }) [ ( upH T.confirmUnsaved, onCloseTxt ) ]) ]
                )

        -- Data
        OnChangePost field value ->
            ( updatePost field value model, noOut )

        OnChangeTarget node ->
            ( setTarget node model, noOut )

        DoMoveTension ->
            ( model, out1 [ moveTension apis.gql model.form OnMoveAck ] )

        OnSubmit next ->
            ( model
            , out1 [ sendNow next ]
            )

        OnMove time ->
            let
                form =
                    model.form

                newForm =
                    { form
                        | post =
                            Dict.insert "createdAt" (fromTime time) form.post
                                |> Dict.union
                                    (Dict.fromList
                                        [ ( "old", model.target )
                                        , ( "new", form.target.nameid )
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
    { orga_data : GqlData NodesData }


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
        , div [ class "modal-content" ]
            [ case model.move_result of
                Success _ ->
                    div [ class "box is-light" ]
                        [ I.icon1 "icon-check icon-2x has-text-success" " "
                        , textH T.tensionMoved
                        ]

                _ ->
                    viewModalContent op (State model)
            ]
        , button [ class "modal-close is-large", onClick (OnCloseSafe "" "") ] []
        ]


viewModalContent : Op -> State -> Html Msg
viewModalContent op (State model) =
    let
        color =
            "warning"

        message =
            Dict.get "message" model.form.post |> withDefault ""

        isLoading =
            model.move_result == LoadingSlowly

        isSendable =
            model.form.target.nameid /= ""

        target =
            if List.member model.form.target.nameid [ "", model.target ] then
                "select a destination"

            else
                model.form.target.name
    in
    div [ class "modal-card" ]
        [ div [ class ("modal-card-head has-background-" ++ color) ]
            [ div [ class "modal-card-title is-size-6 has-text-weight-semibold" ]
                [ text "Move tension" ]
            ]
        , div [ class "modal-card-body" ]
            [ div [ class "field" ]
                [ div [ class "control" ]
                    [ span [] [ text "New receiver: " ]
                    , span [ class "dropdown" ]
                        [ span [ class "dropdown-trigger button-light", attribute "style" "border:1px solid white;" ]
                            [ span [ attribute "aria-controls" "target-menu" ]
                                [ span
                                    [ class "button is-small is-light is-inverted is-static" ]
                                    [ text target, span [ class "ml-2 icon-chevron-down" ] [] ]
                                ]
                            ]
                        , div [ id "target-menu", class "dropdown-menu", attribute "role" "menu" ]
                            [ div [ class "dropdown-content" ] <|
                                case op.orga_data of
                                    Success data ->
                                        let
                                            self_ =
                                                model.blob
                                                    |> Maybe.map
                                                        (\b ->
                                                            b.node
                                                                |> Maybe.map
                                                                    (\n ->
                                                                        nodeIdCodec model.target (withDefault "" n.nameid) (withDefault NodeType.Circle n.type_)
                                                                    )
                                                                |> withDefault ""
                                                        )
                                                    |> withDefault ""

                                            targets =
                                                Dict.values data
                                                    |> List.filter
                                                        (\n ->
                                                            n.nameid /= model.form.target.nameid && n.nameid /= model.target && n.role_type /= Just RoleType.Member && n.nameid /= self_
                                                        )
                                        in
                                        viewNodesSelector targets

                                    _ ->
                                        [ div [ class "spinner" ] [] ]
                            ]
                        ]
                    ]
                ]
            , div [ class "field" ]
                [ div [ class "control" ]
                    [ textarea
                        [ class "textarea in-modal"
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
            [ case model.move_result of
                Failure err ->
                    div [ class "field" ] [ viewGqlErrors err ]

                _ ->
                    text ""
            , div [ class "field is-grouped is-grouped-right" ]
                [ div [ class "control" ]
                    [ button
                        ([ class "button" ]
                            ++ [ onClick (OnClose { reset = True, link = "" }) ]
                        )
                        [ textH T.cancel ]
                    ]
                , div [ class "control" ]
                    [ button
                        ([ class ("button is-light is-" ++ color)
                         , classList [ ( "is-loading", isLoading ) ]
                         , disabled (not isSendable || isLoading)
                         ]
                            ++ [ onClick (OnSubmit <| OnMove) ]
                        )
                        [ text "Move tension" ]
                    ]
                ]
            ]
        ]


viewNodesSelector : List Node -> List (Html Msg)
viewNodesSelector targets =
    List.map
        (\n ->
            let
                color =
                    case n.role_type of
                        Just r ->
                            roleColor r |> String.replace "primary" "info"

                        Nothing ->
                            "light"
            in
            div
                [ class <| ("dropdown-item has-text-weight-semibold button-light has-text-" ++ color)
                , onClick (OnChangeTarget n)
                ]
                [ I.icon1 (ternary (nid2type n.nameid == NodeType.Role) "icon-user" "icon-circle") n.name ]
        )
        targets
