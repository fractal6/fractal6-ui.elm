module Components.MoveTension exposing (Msg(..), State, init, subscriptions, update, view)

import Auth exposing (ErrState(..), parseErr)
import Components.ConfirmContract as ConfirmContract
import Components.Loading as Loading exposing (GqlData, ModalData, RequestResult(..), viewGqlErrors, withMaybeData, withMaybeDataMap)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
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
                initUserctx
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
    , encoded_nid = ""
    , decoded_type_m = Nothing

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    , confirmContract = ConfirmContract.init "" user
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
    { model | isOpen = True, target = target, form = { form | tid = tid }, blob = blob_m, encoded_nid = encoded_nid, decoded_type_m = decoded_type_m }


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

    --Bool: return True data result has just been received
    --      return False if data has been received (when closing modal typically)
    -- Tuple: if it has a blob, and returns (old_nameid, new_receiverid,  new_nameid).
    --        else (simple tension) just (old_receiverid, new_receiverid, "")
    , result : Maybe ( Bool, ( String, String, String ) )
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
        OnOpen tid target blob_m ->
            ( open tid target blob_m model
            , out0 [ Ports.open_modal "MoveTensionModal" ]
            )

        OnClose data ->
            let
                cmds =
                    ternary data.reset [ sendSleep OnReset 333 ] []

                gcmds =
                    ternary (data.link /= "") [ DoNavigate data.link ] []
            in
            case model.move_result of
                Success _ ->
                    ( close model, Out ([ Ports.close_modal ] ++ cmds) gcmds (Just ( False, buildOutResult model )) )

                _ ->
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

        -- Data
        OnChangePost field value ->
            ( updatePost field value model, noOut )

        OnChangeTarget node ->
            ( setTarget node model, noOut )

        DoMoveTension ->
            ( setMoveResult LoadingSlowly model, out0 [ moveTension apis.gql model.form OnMoveAck ] )

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
                    , out1 [ DoAuth data.form.uctx ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep DoMoveTension 500 ] [ DoUpdateToken ] )

                OkAuth _ ->
                    if cmd == Cmd.none then
                        ( data, Out [] [] (Just ( True, buildOutResult model )) )

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
                            (\( terminate, contract ) ->
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
    { orga_data : GqlData NodesDict }


view : Op -> State -> Html Msg
view op (State model) =
    div []
        [ viewModal op (State model)
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        , ConfirmContract.view {} model.confirmContract |> Html.map ConfirmContractMsg
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
                            text "not implemented"
                ]
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
                                            roleTest r_ =
                                                case model.decoded_type_m of
                                                    Just _ ->
                                                        -- keep circle
                                                        r_ == Nothing

                                                    Nothing ->
                                                        -- filter special roles
                                                        List.member r_ [ Just RoleType.Member, Just RoleType.Guest ] == False

                                            decoded_nid =
                                                nodeIdCodec model.target model.encoded_nid (withDefault NodeType.Circle model.decoded_type_m)

                                            targets =
                                                Dict.values data
                                                    |> List.filter
                                                        (\n ->
                                                            (n.nameid /= model.form.target.nameid)
                                                                && (n.nameid /= model.target)
                                                                && roleTest n.role_type
                                                                && (n.nameid /= decoded_nid)
                                                                && (decoded_nid /= (Maybe.map (\p -> p.nameid) n.parent |> withDefault ""))
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
                        ([ class ("button is-light is-" ++ color)
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
