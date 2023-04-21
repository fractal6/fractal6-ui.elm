{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2023 Fractale Co

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


module Components.JoinOrga exposing (JoinStep(..), Msg(..), State, init, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Bulk exposing (ActionForm, Ev, UserState(..), form2cid, initActionForm, makeCandidateContractForm, uctxFromUser)
import Bulk.Codecs exposing (isMember, isPending, nid2rootid)
import Bulk.Error exposing (viewAuthNeeded, viewGqlErrors)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.UserInput as UserInput
import Dict
import Extra exposing (ternary)
import Extra.Events exposing (onClickPD)
import Form exposing (isPostEmpty)
import Fractal.Enum.TensionEvent as TensionEvent
import Generated.Route as Route exposing (toHref)
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, button, div, i, p, span, strong, text, textarea)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, name, placeholder, rows, selected, target, value)
import Html.Events exposing (onClick, onInput)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), isSuccess, withMaybeData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Ports
import Query.AddContract exposing (addOneContract)
import Query.QueryContract exposing (getContractId)
import Query.QueryNode exposing (fetchNode)
import Session exposing (Apis, GlobalCmd(..), Screen, isMobile)
import Text as T
import Time


type State
    = State Model


type alias Model =
    { user : UserState
    , isOpen : Bool
    , form : ActionForm
    , step : JoinStep
    , node_data : GqlData Node
    , nameid : String
    , join_result : GqlData IdPayload
    , isPending : Bool

    -- Common
    , screen : Screen
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg

    -- Components
    , userInput : UserInput.State
    }


type JoinStep
    = JoinOne
    | InviteOne
    | AuthNeeded


initModel : String -> UserState -> Screen -> Model
initModel nameid user screen =
    { user = user
    , isOpen = False
    , form = initActionForm "" user -- set later
    , step = JoinOne
    , node_data = Loading
    , join_result = NotAsked
    , nameid = nameid
    , isPending = False

    -- Common
    , screen = screen
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg

    -- Components
    , userInput = UserInput.init [ nameid ] True True user
    }


init : String -> UserState -> Screen -> State
init nameid user screen =
    initModel nameid user screen |> State



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
    initModel model.nameid model.user model.screen


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


makeJoinForm : UserState -> Node -> Time.Posix -> ActionForm -> ActionForm
makeJoinForm user node time f =
    let
        ( tid, bid ) =
            node.source
                |> Maybe.map (\b -> ( b.tension.id, b.id ))
                |> withDefault ( "", "" )
    in
    { f
        | tid = tid
        , uctx = uctxFromUser user
        , events = [ Ev TensionEvent.UserJoined "" f.uctx.username ]
        , post = Dict.insert "createdAt" (fromTime time) f.post
        , users = [ { username = f.uctx.username, name = Nothing, email = "", pattern = "" } ]
        , node = node
    }


makeInviteForm : UserState -> Node -> Time.Posix -> ActionForm -> ActionForm
makeInviteForm user node time f =
    let
        ( tid, bid ) =
            node.source
                |> Maybe.map (\b -> ( b.tension.id, b.id ))
                |> withDefault ( "", "" )
    in
    { f
        | tid = tid
        , uctx = uctxFromUser user
        , post = Dict.insert "createdAt" (fromTime time) f.post
        , node = node
    }



-- utils


canExitSafe : Model -> Bool
canExitSafe model =
    -- Condition to close safely (e.g. empty form data)
    (not (hasData model)
        && (case model.step of
                JoinOne ->
                    True

                InviteOne ->
                    List.isEmpty model.form.events

                _ ->
                    True
           )
    )
        || isSuccess model.join_result


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
    = OnOpen String JoinStep
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
    | OnGetNode (GqlData Node)
    | OnJoin2 Node Time.Posix
    | OnInvite2 Node Time.Posix
    | OnJoinAck (GqlData IdPayload)
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Common
    | NoMsg
    | LogErr String
    | UpdateUctx UserCtx
      -- Components
    | UserInputMsg UserInput.Msg


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
        OnOpen rootnameid method ->
            case model.user of
                LoggedOut ->
                    ( { model | step = AuthNeeded } |> open
                    , out0 [ Ports.open_modal "JoinOrgaModal" ]
                    )

                LoggedIn uctx ->
                    if method == JoinOne && not (isMember uctx rootnameid || isPending uctx rootnameid) then
                        -- Join
                        ( { model | step = method } |> open
                        , out0 [ Ports.open_modal "JoinOrgaModal", fetchNode apis rootnameid OnGetNode ]
                        )

                    else if method == InviteOne then
                        -- Invite
                        ( { model | step = method } |> open
                        , out0
                            [ Ports.open_modal "JoinOrgaModal"
                            , fetchNode apis rootnameid OnGetNode
                            , Cmd.map UserInputMsg (send UserInput.OnLoad)
                            ]
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
            ( close model, out2 (Ports.close_modal :: cmds) gcmds )

        OnReset ->
            ( reset model, noOut )

        OnCloseSafe link onCloseTxt ->
            if canExitSafe model then
                ( model, out0 [ send (OnClose { reset = True, link = link }) ] )

            else
                ( model
                , out0 [ send (DoModalConfirmOpen (OnClose { reset = True, link = link }) { message = Nothing, txts = [ ( T.confirmUnsaved, onCloseTxt ) ] }) ]
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
                            makeJoinForm model.user node (Time.millisToPosix 0) model.form
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
            if List.member model.step [ JoinOne, InviteOne ] then
                let
                    contractForms =
                        makeCandidateContractForm form
                in
                ( setJoinResult LoadingSlowly model, out0 (List.map (\c -> addOneContract apis c OnJoinAck) contractForms) )

            else
                -- not implemented
                ( model, noOut )

        -- Data
        OnChangePost field value ->
            ( updatePost field value model, noOut )

        OnSubmit next ->
            ( model, out0 [ sendNow next ] )

        OnGetNode result ->
            let
                newModel =
                    { model | node_data = result }
            in
            case model.step of
                JoinOne ->
                    ( newModel
                    , case result of
                        Success n ->
                            out0 [ send (OnSubmit <| OnJoin2 n) ]

                        _ ->
                            noOut
                    )

                InviteOne ->
                    ( newModel
                    , case result of
                        Success n ->
                            out0 [ send (OnSubmit <| OnInvite2 n) ]

                        _ ->
                            noOut
                    )

                _ ->
                    -- not implemented
                    ( model, noOut )

        OnJoin2 node time ->
            ( { model | form = makeJoinForm model.user node time model.form, join_result = NotAsked }, noOut )

        OnInvite2 node time ->
            ( { model | form = makeInviteForm model.user node time model.form, join_result = NotAsked }, noOut )

        OnJoinAck result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | join_result = NotAsked }, out0 [ Ports.raiseAuthModal model.form.uctx ] )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, out2 [ sendSleep (PushGuest model.form) 500 ] [ DoUpdateToken ] )

                OkAuth _ ->
                    ( { model | join_result = result }
                      --, out1 [ DoFetchNode (memberIdCodec model.form.node.nameid model.form.uctx.username) ]
                    , out1 [ DoUpdateToken ]
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

        UpdateUctx uctx ->
            ( { model | user = LoggedIn uctx }, noOut )

        -- Components
        UserInputMsg msg ->
            let
                ( data, out ) =
                    UserInput.update apis msg model.userInput

                form =
                    model.form

                ( users, events ) =
                    case out.result of
                        Just ( selected, us ) ->
                            if selected then
                                ( us
                                , List.map
                                    (\u ->
                                        if u.email /= "" then
                                            -- do not store publicly email
                                            Ev TensionEvent.UserJoined "" ((String.split "@" u.email |> List.head |> withDefault "") ++ "@...")

                                        else
                                            Ev TensionEvent.UserJoined "" u.username
                                    )
                                    us
                                )

                            else
                                -- Assume only one delete at a time
                                case us of
                                    [ u ] ->
                                        let
                                            i =
                                                if u.email /= "" then
                                                    LE.elemIndex u.email (List.map .email form.users) |> withDefault -1

                                                else
                                                    LE.elemIndex u.username (List.map .username form.users) |> withDefault -1
                                        in
                                        ( LE.removeAt i form.users, LE.removeAt i form.events )

                                    _ ->
                                        ( form.users, form.events )

                        Nothing ->
                            ( form.users, form.events )

                ( cmds, gcmds ) =
                    ( [], [] )
            in
            ( { model | userInput = data, form = { form | users = users, events = events } }, out2 (List.map (\m -> Cmd.map UserInputMsg m) out.cmds |> List.append cmds) (out.gcmds ++ gcmds) )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    [ Ports.triggerJoinFromJs (always (OnOpen (nid2rootid model.nameid) JoinOne))
    , Ports.triggerJoinPendingFromJs (always (OnRedirectPending (nid2rootid model.nameid)))
    , Ports.mcPD Ports.closeModalFromJs LogErr OnClose
    , Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    , Ports.uctxPD Ports.loadUserCtxFromJs LogErr UpdateUctx
    ]
        ++ (if model.isOpen then
                UserInput.subscriptions model.userInput |> List.map (\s -> Sub.map UserInputMsg s)

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
              case model.join_result of
                Success data ->
                    viewSuccess data op model

                _ ->
                    viewJoinStep op model
            ]

        --, button [ class "modal-close is-large", onClick (OnCloseSafe "" "") ] []
        ]


viewSuccess : IdPayload -> Op -> Model -> Html Msg
viewSuccess data op model =
    if List.member model.step [ JoinOne, InviteOne ] then
        let
            link =
                Route.Tension_Dynamic_Dynamic_Contract_Dynamic { param1 = nid2rootid model.nameid, param2 = model.form.tid, param3 = data.id } |> toHref
        in
        div [ class "notification is-success-light", onClick (OnClose { reset = True, link = "" }) ]
            [ button [ class "delete", onClick (OnCloseSafe "" "") ] []
            , A.icon1 "icon-check icon-2x has-text-success" " "
            , text (T.requestSent ++ " ")
            , a
                [ href link
                , onClickPD (OnClose { reset = True, link = link })
                , target "_blank"
                ]
                [ text T.checkItOut_fem ]
            ]

    else
        text "step success not implemented. Please report this as a bug."


viewJoinStep : Op -> Model -> Html Msg
viewJoinStep op model =
    case model.step of
        JoinOne ->
            div [ class "modal-card-body" ]
                [ div [ class "field pb-2" ] [ text T.explainJoin ]
                , viewComment False model
                , case model.node_data of
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
                            --        [ text T.checkItOut_fem ]
                            --    ]
                            div [ class "box is-light is-warning" ] [ text T.checkYourPendingInvitation ]

                        else
                            viewGqlErrors err

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
                            [ class "button is-primary"
                            , classList [ ( "is-loading", model.join_result == LoadingSlowly ) ]
                            , onClick (PushGuest model.form)
                            , disabled ((Dict.get "message" model.form.post |> withDefault "") == "")
                            ]
                            [ text T.join ]
                        ]
                    ]
                ]

        InviteOne ->
            let
                name =
                    model.node_data |> withMaybeData |> Maybe.map .name |> withDefault ""
            in
            div [ class "modal-card-body" ]
                [ UserInput.view { label_text = span [] [ text (T.inviteMembers ++ " " ++ T.in_ ++ " "), strong [] [ text name ], text ":" ] } model.userInput |> Html.map UserInputMsg
                , viewComment True model
                , case model.node_data of
                    Failure err ->
                        viewGqlErrors err

                    _ ->
                        text ""
                , case model.join_result of
                    Failure err ->
                        if model.isPending then
                            div [ class "box is-light is-warning" ] [ text T.checkPendingInvitation ]

                        else
                            viewGqlErrors err

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
                            [ class "button is-primary"
                            , classList [ ( "is-loading", model.join_result == LoadingSlowly ) ]
                            , onClick (PushGuest model.form)
                            , disabled (List.isEmpty model.form.events)
                            ]
                            [ text T.invite ]
                        ]
                    ]
                ]

        AuthNeeded ->
            viewAuthNeeded OnClose


viewComment : Bool -> Model -> Html Msg
viewComment isOpt model =
    let
        message =
            Dict.get "message" model.form.post |> withDefault ""

        line_len =
            List.length <| String.lines message

        ( max_len, min_len ) =
            if isMobile model.screen then
                ( 5, 2 )

            else
                ( 10, 3 )
    in
    div [ class "field" ]
        [ div [ class "control submitFocus" ]
            [ textarea
                [ class "textarea"
                , rows (min max_len (max line_len min_len))
                , placeholder <| ternary isOpt T.leaveCommentOpt T.text
                , value message
                , onInput <| OnChangePost "message"
                ]
                []
            ]
        , if List.member model.step [ InviteOne ] then
            p [ class "help-label" ] [ text T.invitationMessageHelp ]

          else
            text ""
        ]
