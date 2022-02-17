module Components.JoinOrga exposing (Msg(..), State, init, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Components.Loading as Loading exposing (ErrorData, GqlData, ModalData, RequestResult(..), viewAuthNeeded, viewGqlErrors, withMaybeData)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Extra.Events exposing (onClickPD)
import Form exposing (isPostEmpty)
import Fractal.Enum.TensionEvent as TensionEvent
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (ActionForm, Ev, UserState(..), form2cid, initActionForm, makeCandidateContractForm, uctxFromUser)
import ModelCommon.Codecs exposing (isMember, isPending, memberIdCodec, nid2rootid)
import ModelSchema exposing (..)
import Ports
import Query.AddContract exposing (addOneContract)
import Query.QueryContract exposing (getContractId)
import Query.QueryNode exposing (fetchNode)
import Session exposing (Apis, GlobalCmd(..))
import Text as T exposing (textH, textT, upH)
import Time


type State
    = State Model


type alias Model =
    { user : UserState
    , isOpen : Bool
    , form : ActionForm
    , step : JoinStep
    , nameid : String
    , join_result : GqlData IdPayload
    , isPending : Bool

    -- Common
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    }


type JoinStep
    = StepOne (GqlData Node)
    | StepAck IdPayload
    | JoinNotAuthorized ErrorData
    | AuthNeeded


initModel : String -> UserState -> Model
initModel nameid user =
    { user = user
    , isOpen = False
    , form = initActionForm "" user -- set later
    , step = StepOne Loading
    , join_result = NotAsked
    , nameid = nameid
    , isPending = False

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }


init : String -> UserState -> State
init nameid user =
    initModel nameid user |> State



-- Global methods


isOpen_ : State -> Bool
isOpen_ (State model) =
    model.isOpen



--- State Controls


open : Model -> Model
open model =
    { model | isOpen = True }


close : Model -> Model
close model =
    { model | isOpen = False }


reset : Model -> Model
reset model =
    initModel model.nameid model.user


updatePost : String -> String -> Model -> Model
updatePost field value model =
    let
        form =
            model.form
    in
    { model | form = { form | post = Dict.insert field value form.post } }


setJoinResult : GqlData IdPayload -> Model -> Model
setJoinResult result model =
    { model | join_result = result }


makeForm : UserState -> Node -> Time.Posix -> ActionForm
makeForm user node time =
    let
        ( tid, bid ) =
            node.source
                |> Maybe.map (\b -> ( b.tension.id, b.id ))
                |> withDefault ( "", "" )

        f =
            initActionForm tid user
    in
    { f
        | events = [ Ev TensionEvent.UserJoined "" f.uctx.username ]
        , post = Dict.fromList [ ( "createdAt", fromTime time ) ]
        , users = [ { username = f.uctx.username, name = Nothing, email = "", pattern = "" } ]
        , node = node
    }



-- utils


canExitSafe : Model -> Bool
canExitSafe model =
    -- Condition to close safely (e.g. empty form data)
    (hasData model && withMaybeData model.join_result == Nothing) == False


hasData : Model -> Bool
hasData model =
    -- When you can commit (e.g. empty form data)
    isPostEmpty [ "message" ] model.form.post == False


isSendable : Model -> Bool
isSendable model =
    -- when the form can be submited
    False



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = OnOpen String
    | OnClose ModalData
    | OnCloseSafe String String
    | OnReset
    | PushGuest ActionForm
    | OnRedirectPending String
    | OnNodePending (GqlData Node)
    | OnContractIdAck (GqlData IdPayload)
      -- Data
    | OnChangePost String String
    | OnSubmit (Time.Posix -> Msg)
      -- JoinOrga Action
    | OnJoin (GqlData Node)
    | OnJoin2 Node Time.Posix
    | OnJoinAck (GqlData IdPayload)
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
    , result : Maybe ( Bool, String ) -- define what data is to be returned
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
        OnOpen rootnameid ->
            case model.user of
                LoggedOut ->
                    ( { model | step = AuthNeeded } |> open
                    , out0 [ Ports.open_modal "JoinOrgaModal" ]
                    )

                LoggedIn uctx ->
                    if not (isMember uctx rootnameid || isPending uctx rootnameid) then
                        ( model |> open
                        , out0 [ Ports.open_modal "JoinOrgaModal", fetchNode apis rootnameid OnJoin ]
                        )

                    else
                        ( model, noOut )

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
                , out0 [ send (DoModalConfirmOpen (OnClose { reset = True, link = link }) { message = Nothing, txts = [ ( upH T.confirmUnsaved, onCloseTxt ) ] }) ]
                )

        OnRedirectPending rootnameid ->
            if not (isMember model.form.uctx rootnameid) then
                ( model, out0 [ fetchNode apis rootnameid OnNodePending ] )

            else
                ( model, noOut )

        OnNodePending result ->
            case result of
                Success node ->
                    let
                        -- Time is ignored here, we just want the contractid
                        form =
                            makeForm model.user node (Time.millisToPosix 0)
                    in
                    ( { model | form = form }, out0 [ getContractId apis (form2cid form) OnContractIdAck ] )

                _ ->
                    ( model, noOut )

        OnContractIdAck result ->
            case result of
                Success data ->
                    let
                        link =
                            Route.Tension_Dynamic_Dynamic_Contract_Dynamic { param1 = nid2rootid model.nameid, param2 = model.form.tid, param3 = data.id } |> toHref
                    in
                    ( model, out1 [ DoNavigate link, DoUpdateToken ] )

                Failure _ ->
                    ( model, out1 [ DoUpdateToken ] )

                _ ->
                    ( model, noOut )

        --Query
        PushGuest form ->
            --( model, actionRequest apis form JoinAck, Cmd.none )
            let
                contractForm =
                    makeCandidateContractForm form
            in
            ( setJoinResult LoadingSlowly model, out0 [ addOneContract apis contractForm OnJoinAck ] )

        -- Data
        OnChangePost field value ->
            ( updatePost field value model, noOut )

        OnSubmit next ->
            ( model, out0 [ sendNow next ] )

        OnJoin result ->
            ( { model | step = StepOne result }
            , case result of
                Success n ->
                    out0 [ send (OnSubmit <| OnJoin2 n) ]

                _ ->
                    noOut
            )

        OnJoin2 node time ->
            ( { model | form = makeForm model.user node time, join_result = NotAsked }, noOut )

        OnJoinAck result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | join_result = NotAsked }, out1 [ DoAuth model.form.uctx ] )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, out2 [ sendSleep (PushGuest model.form) 500 ] [ DoUpdateToken ] )

                OkAuth res ->
                    ( { model | join_result = result, step = StepAck res }
                    , out1 [ DoFetchNode (memberIdCodec model.form.node.nameid model.form.uctx.username) ]
                    )

                DuplicateErr ->
                    ( { model | join_result = result, isPending = True }, noOut )

                _ ->
                    ( { model | join_result = result }, noOut )

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
    [ Ports.triggerJoinFromJs (always (OnOpen (nid2rootid model.nameid)))
    , Ports.triggerJoinPendingFromJs (always (OnRedirectPending (nid2rootid model.nameid)))
    , Ports.mcPD Ports.closeModalFromJs LogErr OnClose
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
        [ id "JoinOrgaModal"
        , class "modal is-light modal-fx-fadeIn"
        , classList [ ( "is-active", model.isOpen ) ]
        , attribute "data-modal-close" "closeModalFromJs"
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "JoinOrgaModal"
            , onClick (OnCloseSafe "" "")
            ]
            []
        , div [ class "modal-content" ]
            [ -- class modal-card ?
              viewJoinStep op (State model)
            ]
        , button [ class "modal-close is-large", onClick (OnCloseSafe "" "") ] []
        ]


viewJoinStep : Op -> State -> Html Msg
viewJoinStep op (State model) =
    case model.step of
        StepOne n ->
            div [ class "modal-card-body" ]
                [ div [ class "field pb-2" ] [ text "What is your motivation to join this organisation ?" ]
                , div [ class "field" ]
                    [ div [ class "control submitFocus" ]
                        [ textarea
                            [ class "textarea"
                            , rows 3
                            , placeholder (upH T.leaveCommentOpt)
                            , value (Dict.get "message" model.form.post |> withDefault "")
                            , onInput <| OnChangePost "message"
                            ]
                            []
                        ]
                    ]
                , case n of
                    Failure err ->
                        viewGqlErrors err

                    _ ->
                        text ""
                , case model.join_result of
                    Failure err ->
                        if model.isPending then
                            -- @TODO: get the contract id when catching the error
                            --let
                            --    link =
                            --        Route.Tension_Dynamic_Dynamic_Contract_Dynamic { param1 = nid2rootid model.nameid, param2 = model.form.tid, param3 = data.id } |> toHref
                            --in
                            --div [ class "box is-light", onClick (OnClose { reset = True, link = "" }) ]
                            --    [ text "Request already sent. "
                            --    , a
                            --        [ href link
                            --        , onClickPD (OnCloseModal { reset = True, link = link })
                            --        , target "_blank"
                            --        ]
                            --        [ textH T.checkItOut ]
                            --    ]
                            div [ class "box is-light is-warning" ] [ text "You already have a pending invitation, please check it out." ]

                        else
                            viewGqlErrors err

                    _ ->
                        text ""
                , div [ class "field is-grouped is-grouped-right" ]
                    [ div [ class "control" ]
                        [ button
                            [ class "button is-primary"
                            , classList [ ( "is-loading", model.join_result == LoadingSlowly ) ]
                            , onClick (PushGuest model.form)
                            ]
                            [ text "Join" ]
                        ]
                    ]
                ]

        StepAck data ->
            let
                link =
                    Route.Tension_Dynamic_Dynamic_Contract_Dynamic { param1 = nid2rootid model.nameid, param2 = model.form.tid, param3 = data.id } |> toHref
            in
            div [ class "box is-light", onClick (OnClose { reset = True, link = "" }) ]
                [ A.icon1 "icon-check icon-2x has-text-success" " "
                , textH "Your request has been sent. "
                , a
                    [ href link
                    , onClickPD (OnClose { reset = True, link = link })
                    , target "_blank"
                    ]
                    [ textH T.checkItOut ]
                ]

        JoinNotAuthorized errMsg ->
            viewGqlErrors errMsg

        AuthNeeded ->
            viewAuthNeeded OnClose
