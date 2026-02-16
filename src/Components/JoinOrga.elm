{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2026 Fractale Co

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


module Components.JoinOrga exposing (JoinStep(..), Msg(..), State, init, setCurrentDraft, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Bulk exposing (ActionForm, Ev, UserState(..), form2cid, initActionForm, makeCandidateContractForm, uctxFromUser)
import Bulk.Codecs exposing (isMember, isPending, nid2rootid)
import Bulk.Error exposing (viewAuthNeeded, viewGqlErrors)
import Codecs exposing (CommentDraft, DraftUpdate(..))
import Components.Comments as Comments
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.UserInput as UserInput
import Dict
import Extra exposing (space_, ternary, textH, unwrap, unwrap2)
import Extra.Events exposing (onClickPD)
import Form exposing (isPostEmpty)
import Fractal.Enum.TensionEvent as TensionEvent
import Generated.Route as Route exposing (toHref)
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, button, div, i, p, span, strong, text)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, target)
import Html.Events exposing (onClick)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), isSuccess, withMaybeData, withMaybeMapData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Ports
import Query.AddContract exposing (addOneContract)
import Query.QueryContract exposing (getContractId)
import Query.QueryNode exposing (fetchNode)
import Session exposing (Apis, GlobalCmd(..), SessionCommon)
import Text as T
import Time


type State
    = State Model


type alias Model =
    { isActive : Bool
    , isActive2 : Bool -- Let minimze VDOM load + prevent glitch while keeping css effects
    , form : ActionForm
    , step : JoinStep
    , node_data : GqlData Node
    , nameid : String
    , join_result : GqlData IdPayload
    , isPending : Bool
    , currentDraft : Maybe CommentDraft

    -- Common
    , session : SessionCommon
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    , modal_confirm3_link : String
    , draftSaveTimer : Int

    -- Components
    , userInput : UserInput.State
    , comments : Comments.State
    }


type JoinStep
    = JoinOne
    | InviteOne
    | AuthNeeded


initModel : String -> SessionCommon -> Model
initModel nameid session =
    { isActive = False
    , isActive2 = False
    , form = initActionForm "" session.user -- set later
    , step = JoinOne
    , node_data = Loading
    , join_result = NotAsked
    , nameid = nameid
    , isPending = False
    , currentDraft = Nothing

    -- Common
    , session = session
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    , modal_confirm3_link = ""
    , draftSaveTimer = 0

    -- Components
    , userInput = UserInput.init [ nameid ] True True session
    , comments = Comments.init nameid "" session
    }


init : String -> SessionCommon -> State
init nameid session =
    initModel nameid session |> State



-- Global methods


isActive_ : State -> Bool
isActive_ (State model) =
    model.isActive



--- State Controls


setCurrentDraft : Maybe CommentDraft -> State -> State
setCurrentDraft draft (State model) =
    State { model | currentDraft = draft }


resetModel : Model -> Model
resetModel model =
    initModel model.nameid model.session


openModal : Model -> Model
openModal model =
    { model | isActive2 = True }


closeModal : Model -> Model
closeModal model =
    { model | isActive = False }


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
    = SetIsActive2 Bool
    | OnOpen String JoinStep
    | OnClose ModalData
    | OnCloseSafe String String
    | OnReset
    | PushGuest ActionForm
    | OnRedirectPending String
    | OnNodePending (GqlData Node)
    | OnContractIdAck (GqlData IdPayload)
      -- Data
    | OnSubmit (Time.Posix -> Msg)
      -- JoinOrga Action
    | OnGetNode (GqlData Node)
    | OnJoin2 Node Time.Posix
    | OnInvite2 Node Time.Posix
    | OnJoinAck (GqlData IdPayload)
      -- Draft
    | SaveDraftDelayed Int
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
    | DoModalConfirm3Open String
    | DoModalConfirm3Discard
    | DoModalConfirm3SaveDraft
    | DoModalConfirm3KeepEditing
      -- Common
    | NoMsg
    | LogErr String
    | UpdateUctx UserCtx
      -- Components
    | UserInputMsg UserInput.Msg
    | CommentsMsg Comments.Msg


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
        SetIsActive2 v ->
            if v then
                ( { model | isActive = model.isActive2 }, out0 [ Ports.open_modal "JoinOrgaModal" ] )

            else
                ( { model | isActive2 = model.isActive }, noOut )

        OnOpen rootnameid method ->
            case model.session.user of
                LoggedOut ->
                    ( { model | step = AuthNeeded } |> openModal
                    , out0 [ sendSleep (SetIsActive2 True) 10 ]
                    )

                LoggedIn uctx ->
                    let
                        isPndg =
                            isPending uctx rootnameid

                        form =
                            model.form

                        newForm =
                            case model.currentDraft of
                                Just draft ->
                                    { form | post = Dict.insert "message" draft.message form.post }

                                Nothing ->
                                    form

                        newComments =
                            Comments.initWithDraft rootnameid "" model.session model.currentDraft
                    in
                    if method == JoinOne && not (isMember uctx rootnameid || isPndg) then
                        -- Join
                        ( { model | step = method, isPending = isPndg, form = newForm, comments = newComments } |> openModal
                        , out0
                            [ fetchNode apis rootnameid OnGetNode
                            , sendSleep (SetIsActive2 True) 10
                            ]
                        )

                    else if method == InviteOne then
                        -- Invite
                        ( { model | step = method, isPending = isPndg, form = newForm, comments = newComments } |> openModal
                        , out0
                            [ fetchNode apis rootnameid OnGetNode
                            , Cmd.map UserInputMsg (send UserInput.OnLoad)
                            , sendSleep (SetIsActive2 True) 10
                            ]
                        )

                    else
                        ( { model | isPending = isPndg }, noOut )

        OnClose data ->
            let
                ( newModel, gcmds ) =
                    if data.link == "" then
                        ( model, [] )

                    else
                        ( { model | isActive2 = True }, [ DoNavigate data.link ] )
            in
            ( closeModal newModel
            , out2
                [ Ports.close_modal
                , ternary data.reset (sendSleep OnReset 333) Cmd.none
                , sendSleep (SetIsActive2 False) 500
                ]
                gcmds
            )

        OnReset ->
            ( resetModel model, noOut )

        OnCloseSafe link _ ->
            if canExitSafe model then
                ( model, out0 [ send (OnClose { reset = True, link = link }) ] )

            else
                ( model
                , out0 [ send (DoModalConfirm3Open link) ]
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
                            makeJoinForm model.session.user node (Time.millisToPosix 0) model.form
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
            ( { model | form = makeJoinForm model.session.user node time model.form, join_result = NotAsked }, noOut )

        OnInvite2 node time ->
            ( { model | form = makeInviteForm model.session.user node time model.form, join_result = NotAsked }, noOut )

        OnJoinAck result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | join_result = NotAsked }, out0 [ Ports.raiseAuthModal model.form.uctx ] )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, out2 [ sendSleep (PushGuest model.form) 500 ] [ DoUpdateToken ] )

                OkAuth data ->
                    let
                        link =
                            Route.Tension_Dynamic_Dynamic_Contract_Dynamic { param1 = nid2rootid model.nameid, param2 = model.form.tid, param3 = data.id } |> toHref
                    in
                    ( { model | join_result = result }
                    , out2
                        [ send (OnCloseSafe "" "") ]
                        [ --Contract based event (DoLoad for pendings nodes)...
                          DoUpdateNode model.form.node.nameid identity
                        , DoUpdateDraft ClearNewInvite
                        , DoPushSystemNotif
                            { cls = "is-success"
                            , content =
                                div [ class "is-flex is-align-items-center mr-5" ]
                                    [ A.icon1 "icon-check icon-2x has-text-success" ""
                                    , text T.requestSent
                                    , text space_
                                    , a [ href link ]
                                        [ text T.checkItOut_fem ]
                                    ]
                            }
                        ]
                    )

                DuplicateErr ->
                    ( { model | join_result = result, isPending = True }
                    , case model.step of
                        JoinOne ->
                            out1 [ DoUpdateToken ]

                        _ ->
                            noOut
                    )

                _ ->
                    ( { model | join_result = result }, noOut )

        -- Draft
        SaveDraftDelayed timer ->
            if timer == model.draftSaveTimer then
                let
                    msg_ =
                        Dict.get "message" model.form.post
                            |> Maybe.map String.trim
                            |> withDefault ""
                in
                if msg_ == "" then
                    ( model, out1 [ DoUpdateDraft ClearNewInvite ] )

                else
                    ( model, out1 [ DoUpdateDraft (SaveNewInvite { message = msg_, updatedAt = "" }) ] )

            else
                ( model, noOut )

        -- Confirm Modal
        DoModalConfirmOpen msg mess ->
            ( { model | modal_confirm = ModalConfirm.open msg mess model.modal_confirm }, noOut )

        DoModalConfirmClose _ ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, noOut )

        DoModalConfirmSend ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, out0 [ send model.modal_confirm.msg ] )

        DoModalConfirm3Open link ->
            ( { model
                | modal_confirm = ModalConfirm.open NoMsg { message = Nothing, txts = [ ( T.confirmUnsavedDraft, "" ) ], confirmClass = "is-success", confirmLabel = T.confirm } model.modal_confirm
                , modal_confirm3_link = link
              }
            , noOut
            )

        DoModalConfirm3Discard ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }
            , Out [ send (OnClose { reset = True, link = model.modal_confirm3_link }) ] [ DoUpdateDraft ClearNewInvite ] Nothing
            )

        DoModalConfirm3SaveDraft ->
            let
                draftMessage =
                    Dict.get "message" model.form.post |> withDefault ""

                draft =
                    CommentDraft draftMessage ""
            in
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }
            , Out [ send (OnClose { reset = True, link = model.modal_confirm3_link }) ] [ DoUpdateDraft (SaveNewInvite draft) ] Nothing
            )

        DoModalConfirm3KeepEditing ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, noOut )

        -- Common
        NoMsg ->
            ( model, noOut )

        LogErr err ->
            ( model, out0 [ Ports.logErr err ] )

        UpdateUctx uctx ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | user = LoggedIn uctx }, isPending = isPending uctx model.nameid }, noOut )

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

        CommentsMsg msg ->
            let
                ( newComments, out ) =
                    Comments.update apis msg model.comments

                -- Sync message from Comments to JoinOrga.form when it changes
                ( newForm, draftCmds, newTimer ) =
                    case out.result of
                        Just (Comments.PostChanged ( "message", v )) ->
                            let
                                form =
                                    model.form

                                updatedForm =
                                    { form | post = Dict.insert "message" v form.post }

                                timer =
                                    model.draftSaveTimer + 1
                            in
                            if String.trim v == "" then
                                ( updatedForm, [ send (SaveDraftDelayed timer) ], timer )

                            else
                                ( updatedForm, [ sendSleep (SaveDraftDelayed timer) 3500 ], timer )

                        _ ->
                            ( model.form, [], model.draftSaveTimer )
            in
            ( { model | comments = newComments, form = newForm, draftSaveTimer = newTimer }
            , out2 (out.cmds |> List.map (Cmd.map CommentsMsg) |> List.append draftCmds) out.gcmds
            )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    [ Ports.triggerJoinFromJs (always (OnOpen (nid2rootid model.nameid) JoinOne))
    , Ports.triggerJoinPendingFromJs (always (OnRedirectPending (nid2rootid model.nameid)))
    , Ports.mcPD Ports.closeModalFromJs LogErr OnClose
    , Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    , Ports.uctxPD Ports.loadUserCtxFromJs LogErr UpdateUctx
    ]
        ++ (if model.isActive then
                (UserInput.subscriptions model.userInput |> List.map (\s -> Sub.map UserInputMsg s))
                    ++ (Comments.subscriptions model.comments |> List.map (Sub.map CommentsMsg))

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
    if model.isActive2 then
        div []
            [ viewModal op (State model)
            , ModalConfirm.view3
                { data = model.modal_confirm
                , onDiscard = DoModalConfirm3Discard
                , onSaveDraft = DoModalConfirm3SaveDraft
                , onKeepEditing = DoModalConfirm3KeepEditing
                }
            ]

    else
        text ""


viewModal : Op -> State -> Html Msg
viewModal op (State model) =
    div
        [ id "JoinOrgaModal"
        , class "modal modal-fx-fadeIn"
        , classList [ ( "is-active", model.isActive ) ]
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
                    -- @obsolete
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
            , text space_
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
            let
                commentOpts =
                    { hasTips = False
                    , isModal = True
                    , placeholderText = T.text
                    , messageHelper = ""
                    }
            in
            div [ class "modal-card-body" ]
                [ div [ class "field pb-2" ] [ text T.explainJoin ]
                , Comments.viewNewTensionCommentInput model.session commentOpts model.comments |> Html.map CommentsMsg
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
                            --div [ class "box", onClick (OnClose { reset = True, link = "" }) ]
                            --    [ text "Request already sent. "
                            --    , a
                            --        [ href link
                            --        , onClickPD (OnCloseModal { reset = True, link = link })
                            --        , target "_blank"
                            --        ]
                            --        [ text T.checkItOut_fem ]
                            --    ]
                            div [ class "box is-warning is-soft" ] [ text T.checkYourPendingInvitation ]

                        else
                            viewGqlErrors err

                    _ ->
                        text ""
                , div [ class "field level is-mobile" ]
                    [ div [ class "level-left" ]
                        [ button
                            [ class "button"
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
                    model.node_data |> withMaybeMapData .name |> withDefault ""

                commentOpts =
                    { hasTips = False
                    , isModal = True
                    , placeholderText = T.leaveCommentOpt
                    , messageHelper = T.invitationMessageHelp
                    }
            in
            div [ class "modal-card-body" ]
                [ UserInput.view { label_text = span [] [ text (T.inviteMembers ++ " " ++ T.in_ ++ " "), strong [] [ text name ], text ":" ] } model.userInput |> Html.map UserInputMsg
                , Comments.viewNewTensionCommentInput model.session commentOpts model.comments |> Html.map CommentsMsg
                , case model.node_data of
                    Failure err ->
                        viewGqlErrors err

                    _ ->
                        text ""
                , case model.join_result of
                    Failure err ->
                        if model.isPending then
                            div [ class "box is-warning is-soft" ] [ text T.checkPendingInvitation ]

                        else
                            viewGqlErrors err

                    _ ->
                        text ""
                , div [ class "field level is-mobile" ]
                    [ div [ class "level-left" ]
                        [ button
                            [ class "button"
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
