module Components.MoveTension exposing (Msg(..), State, init, subscriptions, update, view, viewNodeSelect)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Components.ConfirmContract as ConfirmContract
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Form exposing (isPostEmpty)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionEvent as TensionEvent
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), isSuccess, viewGqlErrors, withMaybeData, withMaybeDataMap)
import Maybe exposing (withDefault)
import ModelCommon exposing (Ev, UserState(..), sortNode)
import ModelCommon.Codecs exposing (DocType(..), nid2type, nodeIdCodec)
import ModelCommon.View exposing (action2icon, roleColor)
import ModelSchema exposing (..)
import Ports
import Query.PatchTension exposing (moveTension)
import Session exposing (Apis, GlobalCmd(..))
import Text as T exposing (textH, textT, upH)
import Time


type State
    = State Model


type alias Model =
    { user : UserState
    , isActive : Bool
    , isActive2 : Bool
    , move_result : GqlData TensionId
    , target : String -- keep origin target (receiverid)
    , form : MoveForm

    -- Blob
    , blob : Maybe Blob
    , encoded_nid : String
    , decoded_type_m : Maybe NodeType.NodeType

    -- Common
    , refresh_trial : Int
    , modal_confirm : ModalConfirm Msg
    , confirmContract : ConfirmContract.State
    }


type alias MoveForm =
    { uctx : UserCtx
    , tid : String
    , target : Node
    , events : List Ev
    , post : Post
    }


initForm : UserState -> MoveForm
initForm user =
    { uctx =
        case user of
            LoggedIn uctx ->
                uctx

            LoggedOut ->
                initUserctx
    , tid = ""
    , target = initNode
    , events = []
    , post = Dict.empty
    }


init : UserState -> State
init user =
    initModel user |> State


initModel : UserState -> Model
initModel user =
    { user = user
    , isActive = False
    , isActive2 = False
    , move_result = NotAsked
    , form = initForm user
    , target = ""
    , blob = Nothing
    , encoded_nid = ""
    , decoded_type_m = Nothing

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    , confirmContract = ConfirmContract.init user
    }



-- Global methods


isOpen_ : State -> Bool
isOpen_ (State model) =
    model.isActive



--- State Controls


open : String -> String -> Maybe Blob -> Model -> Model
open tid target blob_m model =
    let
        form =
            model.form

        ( encoded_nid, decoded_type_m ) =
            blob_m
                |> Maybe.map
                    (\b ->
                        b.node
                            |> Maybe.map
                                (\n -> ( withDefault "" n.nameid, n.type_ ))
                            |> withDefault ( "", Nothing )
                    )
                |> withDefault ( "", Nothing )
    in
    { model | isActive2 = True, target = target, form = { form | tid = tid }, blob = blob_m, encoded_nid = encoded_nid, decoded_type_m = decoded_type_m }


close : Model -> Model
close model =
    { model | isActive = False }


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


setMoveResult : GqlData TensionId -> Model -> Model
setMoveResult result model =
    { model | move_result = result }



-- utils


canExitSafe : Model -> Bool
canExitSafe model =
    (hasData model && withMaybeData model.move_result == Nothing) == False


hasData : Model -> Bool
hasData model =
    (isPostEmpty [ "message" ] model.form.post && model.form.target.nameid == "") == False


buildOutResult : Model -> ( String, String, String )
buildOutResult model =
    if model.encoded_nid /= "" then
        -- Blob here
        let
            decoded_nid =
                nodeIdCodec model.target model.encoded_nid (withDefault NodeType.Circle model.decoded_type_m)

            decoded_nid_new =
                nodeIdCodec model.form.target.nameid model.encoded_nid (withDefault NodeType.Circle model.decoded_type_m)
        in
        ( decoded_nid, model.form.target.nameid, decoded_nid_new )

    else
        -- simple tension here
        ( model.target, model.form.target.nameid, "" )



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = DoMoveTension
    | SetIsActive2 Bool
    | OnOpen String String (Maybe Blob)
    | OnClose ModalData
    | OnCloseSafe String String
    | OnReset
      -- Data
    | OnChangePost String String
    | OnChangeTarget Node
    | OnSubmit (Time.Posix -> Msg)
    | OnMove Time.Posix
    | OnMoveAck (GqlData TensionId)
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Common
    | NoMsg
    | LogErr String
    | ConfirmContractMsg ConfirmContract.Msg


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd

    --Bool: return True if modal is to be closed
    --Tuple: if it has a blob, and returns (old_nameid, new_receiverid,  new_nameid).
    --        else (simple tension) just (old_receiverid, new_receiverid, "")
    , result : Maybe ( Bool, Maybe ( String, String, String ) )
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
    update_ apis message model |> Tuple.mapFirst State


update_ apis message model =
    case message of
        SetIsActive2 v ->
            -- Prevent elm from computing the VDOM
            if v then
                ( { model | isActive = model.isActive2 }, out0 [ Ports.open_modal "MoveTensionModal" ] )

            else
                ( { model | isActive2 = model.isActive }, noOut )

        OnOpen tid target blob_m ->
            ( open tid target blob_m model
            , out0 [ sendSleep (SetIsActive2 True) 10, Ports.requireTreeData ]
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
            , Out
                [ Ports.close_modal
                , ternary data.reset (sendSleep OnReset 333) Cmd.none
                , sendSleep (SetIsActive2 False) 500
                ]
                gcmds
                (if isSuccess model.move_result then
                    Just ( True, Just (buildOutResult model) )

                 else
                    Just ( True, Nothing )
                )
            )

        OnReset ->
            ( reset model, noOut )

        OnCloseSafe link onCloseTxt ->
            if canExitSafe model then
                ( model, out0 [ send (OnClose { reset = True, link = link }) ] )

            else
                ( model
                , out0 [ send (DoModalConfirmOpen (OnClose { reset = True, link = link }) { message = Nothing, txts = [ ( upH T.confirmUnsaved, onCloseTxt ) ] }) ]
                )

        -- Data
        OnChangePost field value ->
            ( updatePost field value model, noOut )

        OnChangeTarget node ->
            ( setTarget node model, noOut )

        DoMoveTension ->
            ( setMoveResult LoadingSlowly model, out0 [ moveTension apis model.form OnMoveAck ] )

        OnSubmit next ->
            ( model
            , out0 [ sendNow next ]
            )

        OnMove time ->
            let
                form =
                    model.form

                newForm =
                    { form
                        | post = Dict.insert "createdAt" (fromTime time) form.post
                        , events = [ Ev TensionEvent.Moved model.target form.target.nameid ]
                    }
            in
            ( { model | form = newForm }
            , out0 [ send DoMoveTension ]
            )

        OnMoveAck result ->
            let
                contract_m =
                    withMaybeDataMap
                        (\x ->
                            x.contracts
                                |> Maybe.map (\y -> y |> List.filter (\c -> c.id == "") |> List.head)
                                |> withDefault Nothing
                        )
                        result
                        |> withDefault Nothing

                ( data, cmd ) =
                    case contract_m of
                        Just c ->
                            ( setMoveResult NotAsked model, Cmd.map ConfirmContractMsg (send (ConfirmContract.OnOpen model.target model.form.post (Just c))) )

                        Nothing ->
                            ( setMoveResult result model, Cmd.none )
            in
            case parseErr result data.refresh_trial of
                Authenticate ->
                    ( setMoveResult NotAsked model
                    , out0 [ Ports.raiseAuthModal data.form.uctx ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep DoMoveTension 500 ] [ DoUpdateToken ] )

                OkAuth _ ->
                    if cmd == Cmd.none then
                        ( data, Out [] [] (Just ( False, Just <| buildOutResult model )) )

                    else
                        -- Contract here
                        ( data, out0 [ cmd ] )

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

        ConfirmContractMsg msg ->
            let
                ( data, out ) =
                    ConfirmContract.update apis msg model.confirmContract

                cmds =
                    out.result
                        |> Maybe.map
                            (\( terminate, _ ) ->
                                if terminate == True then
                                    [ send (OnClose { reset = True, link = "" }) ]

                                else
                                    []
                            )
                        |> withDefault []
            in
            ( { model | confirmContract = data }, out2 (out.cmds |> List.map (\m -> Cmd.map ConfirmContractMsg m) |> List.append cmds) out.gcmds )


subscriptions =
    [ Ports.mcPD Ports.closeModalFromJs LogErr OnClose
    , Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    ]
        ++ (ConfirmContract.subscriptions |> List.map (\s -> Sub.map ConfirmContractMsg s))



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    { tree_data : GqlData NodesDict }


view : Op -> State -> Html Msg
view op (State model) =
    if model.isActive2 then
        div []
            [ viewModal op model
            , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
            , ConfirmContract.view {} model.confirmContract |> Html.map ConfirmContractMsg
            ]

    else
        text ""


viewModal : Op -> Model -> Html Msg
viewModal op model =
    div
        [ id "MoveTensionModal"
        , class "modal is-light modal-fx-fadeIn"
        , classList [ ( "is-active", model.isActive ) ]
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
                    div [ class "notification is-success-light" ]
                        [ button [ class "delete", onClick (OnCloseSafe "" "") ] []
                        , A.icon1 "icon-check icon-2x has-text-success" " "
                        , textH T.tensionMoved
                        ]

                _ ->
                    viewModalContent op model
            ]

        --, button [ class "modal-close is-large", onClick (OnCloseSafe "" "") ] []
        ]


viewModalContent : Op -> Model -> Html Msg
viewModalContent op model =
    let
        color =
            "warning"

        message =
            Dict.get "message" model.form.post |> withDefault ""

        isLoading =
            model.move_result == LoadingSlowly

        isSendable =
            model.form.target.nameid /= ""
    in
    div [ class "modal-card" ]
        [ div [ class ("modal-card-head has-background-" ++ color) ]
            [ div [ class "modal-card-title is-wrapped is-size-6 has-text-grey-dark has-text-weight-semibold" ]
                [ case model.blob of
                    Nothing ->
                        textH T.moveTension

                    Just blob ->
                        if blob.node /= Nothing then
                            blob.node
                                |> Maybe.map
                                    (\n ->
                                        span [] [ textH (T.moveNode ++ ": "), span [ class "has-text-primary" ] [ text (withDefault "" n.name) ] ]
                                    )
                                |> withDefault (text "error: node is empty")

                        else
                            text T.notImplemented

                --, button [ class "delete is-pulled-right", onClick (OnCloseSafe "" "") ] []
                ]
            ]
        , div [ class "modal-card-body" ]
            [ div [ class "field" ]
                [ div [ class "control" ]
                    [ span [] [ textH (T.newReceiver ++ ": ") ]
                    , span [ class "dropdown" ]
                        [ span [ class "dropdown-trigger" ]
                            [ span [ attribute "aria-controls" "target-menu" ]
                                [ if List.member model.form.target.nameid [ "", model.target ] then
                                    span
                                        [ class "button is-small is-light is-inverted" ]
                                        [ text "select a destination", span [ class "ml-2 icon-chevron-down1" ] [] ]

                                  else
                                    span
                                        [ class "button is-small is-rounded has-border" ]
                                        [ text model.form.target.name, span [ class "ml-2 icon-chevron-down1" ] [] ]
                                ]
                            ]
                        , div [ id "target-menu", class "dropdown-menu", attribute "role" "menu" ]
                            [ div [ class "dropdown-content has-border" ] <|
                                case op.tree_data of
                                    Success data ->
                                        let
                                            decoded_nid =
                                                nodeIdCodec model.target model.encoded_nid (withDefault NodeType.Circle model.decoded_type_m)
                                        in
                                        List.map
                                            (\n -> Lazy.lazy2 viewNodeSelect n OnChangeTarget)
                                            (Dict.values data
                                                |> List.filter
                                                    (\n ->
                                                        not (List.member n.nameid [ model.form.target.nameid, model.target, decoded_nid ])
                                                     --&& (decoded_nid /= (Maybe.map .nameid n.parent |> withDefault ""))
                                                    )
                                                |> List.sortWith sortNode
                                            )

                                    _ ->
                                        [ div [ class "spinner" ] [] ]
                            ]
                        ]
                    ]
                ]
            , div [ class "field" ]
                [ div [ class "control submitFocus" ]
                    [ textarea
                        [ class "textarea"
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
            [ case model.move_result of
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
                        ([ class ("button defaultSubmit is-light is-" ++ color)
                         , classList [ ( "is-loading", isLoading ) ]
                         , disabled (not isSendable || isLoading)
                         ]
                            ++ [ onClick (OnSubmit <| OnMove) ]
                        )
                        [ textH T.move ]
                    ]
                ]
            ]
        ]


viewNodeSelect : Node -> (Node -> msg) -> Html msg
viewNodeSelect n onChangeTarget =
    div
        [ class <| "dropdown-item has-text-weight-semibold button-light"
        , onClick (onChangeTarget n)
        ]
        [ case nid2type n.nameid of
            NodeType.Circle ->
                A.icon1 (action2icon { doc_type = NODE NodeType.Circle }) n.name

            NodeType.Role ->
                span []
                    [ A.icon1 (action2icon { doc_type = NODE NodeType.Role }) n.name
                    , case n.first_link of
                        Just f ->
                            span [ class "is-username is-size-7" ] [ text (" @" ++ f.username) ]

                        Nothing ->
                            text ""
                    ]
        ]
