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


port module Components.CardPanel exposing (Msg(..), State, init, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Bulk exposing (CommentPatchForm, Ev, InputViewMode, TensionForm, UserState(..), eventFromForm, initCommentPatchForm, initTensionForm, pushCommentReaction, removeCommentReaction, uctxFromUser)
import Bulk.Bulma as B
import Bulk.Codecs exposing (DocType(..), getOrgaRoles)
import Bulk.Error exposing (viewGqlErrors, viewJoinForCommentNeeded)
import Bulk.View exposing (action2icon, tensionIcon3, viewTensionLight)
import Components.Comments as Comments
import Components.LabelSearchPanel as LabelSearchPanel
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.TreeMenu as TreeMenu exposing (viewSelectorTree)
import Components.UserInput as UserInput
import Components.UserSearchPanel as UserSearchPanel
import Dict exposing (Dict)
import Extra exposing (ternary, textH, unwrap, unwrap2, upH)
import Extra.Events exposing (onClickPD, onClickSP, onKeydown)
import Form exposing (isPostEmpty)
import Fractal.Enum.ProjectColumnType as ProjectColumnType
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, autofocus, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, style, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), isFailure, isSuccess, withDefaultData, withMapData, withMaybeData, withMaybeMapData)
import Maybe exposing (withDefault)
import ModelSchema exposing (CardKind(..), Comment, FocusNode, IdPayload, LocalGraph, Node, NodesDict, PatchTensionPayloadID, Post, ProjectCard, ProjectColumn, ProjectDraft, ReactionResponse, TensionLight, TensionPanel, UserCtx, emptyCard, initFocusNode, node2focus)
import Org.Tensions exposing (TypeFilter(..), defaultTypeFilter, typeDecoder, typeFilter2Text)
import Ports
import Query.PatchTension exposing (patchComment, patchLiteral, pushTensionPatch)
import Query.QueryTension exposing (getTensionPanel)
import Query.Reaction exposing (addReaction, deleteReaction)
import Requests exposing (TensionQuery, fetchTensionsLight, initTensionQuery)
import Session exposing (Apis, Conf, GlobalCmd(..), toReflink)
import Text as T
import Time



-- ------------------------------
-- M O D E L
-- ------------------------------


type State
    = State Model



-- @todo: set tensionid in TensionForm


type alias Model =
    { user : UserState
    , focus : String
    , isOpen : Bool
    , card : ProjectCard
    , tension_result : GqlData TensionPanel

    -- Form (Title, Status, Comment)
    , tension_form : TensionForm
    , tension_patch : GqlData PatchTensionPayloadID

    -- Title Result
    , isTitleEdit : Bool
    , title_result : GqlData IdPayload

    -- Components
    , assigneesPanel : UserSearchPanel.State
    , labelsPanel : LabelSearchPanel.State

    -- Common
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    }


initModel : String -> UserState -> Model
initModel nameid user =
    { user = user
    , focus = nameid
    , isOpen = False
    , card = emptyCard
    , tension_result = NotAsked

    -- Form (Title, Status, Comment)
    , tension_form = initTensionForm "" Nothing user

    -- Push Comment / Change status
    , tension_patch = NotAsked

    -- Title Result
    , isTitleEdit = False
    , title_result = NotAsked

    -- Components
    , assigneesPanel = UserSearchPanel.load Nothing user
    , labelsPanel = LabelSearchPanel.load Nothing user

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }


init : String -> UserState -> State
init nameid user =
    initModel nameid user |> State



-- Global methods
-- not yet
-- State Controls


resetModel : Model -> Model
resetModel model =
    initModel model.focus model.user



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = OnOpen ProjectCard
    | OnOutsideClickClose
    | OnClose
      --
      -- Tension action/patching (@todo componetize this, Org/Tension + Components/CardPanel)
      --
    | OnSubmit Bool (Time.Posix -> Msg)
    | OnQueryTension String
    | OnTensionAck (GqlData TensionPanel)
    | ExpandEvent Int
      -- Title
    | DoChangeTitle
    | CancelTitle
    | SubmitTitle Time.Posix
    | TitleAck (GqlData IdPayload)
      -- New Comment
    | ChangeComment String String
    | SubmitComment (Maybe TensionStatus.TensionStatus) Time.Posix
    | CommentAck (GqlData PatchTensionPayloadID)
      -- Edit comment
    | DoUpdateComment Comment
    | CancelCommentPatch
    | ChangeCommentPatch String String
    | SubmitCommentPatch Time.Posix
    | CommentPatchAck (GqlData Comment)
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Common
    | NoMsg
    | LogErr String
    | ChangeInputViewMode InputViewMode
    | ChangeUpdateViewMode InputViewMode
    | OnRichText String String
    | OnToggleMdHelp String
    | OnAddReaction String Int
    | OnAddReactionAck (GqlData ReactionResponse)
    | OnDeleteReaction String Int
    | OnDeleteReactionAck (GqlData ReactionResponse)
      -- Components
    | LabelSearchPanelMsg LabelSearchPanel.Msg
    | UserSearchPanelMsg UserSearchPanel.Msg
    | UserInputMsg UserInput.Msg


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe ( String, ProjectCard )
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
        OnOpen card ->
            ( { model | isOpen = True, card = card }
            , out0
                [ --Ports.bulma_driver "cardPanel"
                  case card.card of
                    CardTension a ->
                        send (OnQueryTension a.id)

                    CardDraft a ->
                        -- @todo
                        send NoMsg
                , sendSleep OnOutsideClickClose 500
                ]
            )

        OnOutsideClickClose ->
            ( model, out0 [ Ports.outsideClickClose "closeCardPanelFromJs" "cardPanel" ] )

        OnClose ->
            -- @warning: Ports.click to reset the outsideClickClose handler.
            --
            -- Tension action/patching (@todo componetize this, Org/Tension + Components/CardPanel)
            --
            ( resetModel model, out0 [ Ports.click "" ] )

        OnSubmit isLoading next ->
            if isLoading then
                ( model, noOut )

            else
                ( model, out0 [ sendNow next ] )

        OnQueryTension tid ->
            ( { model | tension_result = Loading }
            , out0 [ getTensionPanel apis (uctxFromUser model.user) tid OnTensionAck ]
            )

        OnTensionAck result ->
            -- @todo: get isTensionAdmin...
            ( { model | tension_result = result }, noOut )

        ExpandEvent i ->
            -- @fix/bulma: dropdown clidk handler lost during the operation
            ( { model | expandedEvents = model.expandedEvents ++ [ i ] }, noOut )

        DoChangeTitle ->
            -- @todo: set titleInput id
            ( { model | isTitleEdit = True }, out0 [ Ports.focusOn "titleInput" ] )

        CancelTitle ->
            ( { model | isTitleEdit = False, tension_form = initTensionForm "" Nothing model.user, title_result = NotAsked }, noOut )

        SubmitTitle time ->
            let
                form =
                    model.tension_form

                newForm =
                    { form
                        | post =
                            form.post
                                |> Dict.insert "createdAt" (fromTime time)
                        , events =
                            [ Ev TensionEvent.TitleUpdated
                                (model.tension_result |> withMaybeMapData .title |> withDefault "")
                                (Dict.get "title" form.post |> withDefault "")
                            ]
                    }
            in
            ( { model | tension_form = newForm, title_result = LoadingSlowly }, out0 [ patchLiteral apis model.tension_form TitleAck ] )

        TitleAck result ->
            case parseErr result 2 of
                OkAuth _ ->
                    let
                        tension_r =
                            case model.tension_result of
                                Success t ->
                                    Success { t | title = Dict.get "title" model.tension_form.post |> withDefault "" }

                                other ->
                                    other

                        resetForm =
                            initTensionForm "" Nothing model.user
                    in
                    ( { model | tension_result = tension_r, tension_form = resetForm, title_result = result, isTitleEdit = False }
                    , noOut
                    )

                _ ->
                    ( { model | title_result = result }, noOut )

        ChangeComment field value ->
            let
                form =
                    model.tension_form

                newForm =
                    if field == "message" && value == "" then
                        { form | post = Dict.remove field form.post }

                    else
                        { form | post = Dict.insert field value form.post }
            in
            ( { model | tension_form = newForm }, noOut )

        SubmitComment status_m time ->
            let
                form =
                    model.tension_form

                eventStatus =
                    case status_m of
                        Just TensionStatus.Open ->
                            [ Ev TensionEvent.Reopened "Closed" "Open" ]

                        Just TensionStatus.Closed ->
                            [ Ev TensionEvent.Closed "Open" "Closed" ]

                        Nothing ->
                            []

                newForm =
                    { form
                        | post = Dict.insert "createdAt" (fromTime time) form.post
                        , status = status_m
                        , events = eventStatus
                    }
            in
            ( { model | tension_form = newForm, tension_patch = LoadingSlowly }
            , out0 [ pushTensionPatch apis model.tension_form CommentAck ]
            )

        CommentAck result ->
            case parseErr result 2 of
                OkAuth tp ->
                    let
                        events =
                            model.tension_form.events

                        tension_r =
                            case model.tension_result of
                                Success t ->
                                    Success
                                        { t
                                            | status = withDefault t.status model.tension_form.status
                                            , history =
                                                withDefault [] t.history
                                                    ++ (events
                                                            |> List.map (\e -> eventFromForm e model.tension_form)
                                                       )
                                                    |> Just
                                            , comments =
                                                if (Dict.get "message" model.tension_form.post |> withDefault "") /= "" then
                                                    Just (withDefault [] t.comments ++ withDefault [] tp.comments)

                                                else
                                                    t.comments
                                        }

                                other ->
                                    other

                        resetForm =
                            initTensionForm "" Nothing model.user
                    in
                    ( { model
                        | tension_result = tension_r
                        , tension_form = resetForm
                        , tension_patch = result
                      }
                    , noOut
                    )

                _ ->
                    case result of
                        Failure _ ->
                            let
                                form =
                                    model.tension_form

                                resetForm =
                                    { form | status = Nothing }
                            in
                            ( { model | tension_patch = result, tension_form = resetForm }, noOut )

                        _ ->
                            ( { model | tension_patch = result }, noOut )

        DoUpdateComment c ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | id = c.id } }, out0 [ Ports.focusOn "updateCommentInput" ] )

        CancelCommentPatch ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | id = "", post = Dict.remove "message" form.post }, comment_result = NotAsked }, noOut )

        ChangeCommentPatch field value ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | post = Dict.insert field value form.post } }, noOut )

        SubmitCommentPatch time ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | post = Dict.insert "updatedAt" (fromTime time) form.post }, comment_result = LoadingSlowly }
            , out0 [ patchComment apis model.comment_form CommentPatchAck ]
            )

        CommentPatchAck result ->
            case parseErr result 2 of
                OkAuth comment ->
                    let
                        tension_r =
                            case model.tension_result of
                                Success t ->
                                    let
                                        comments =
                                            withDefault [] t.comments

                                        n =
                                            comments
                                                |> LE.findIndex (\c -> c.id == comment.id)
                                                |> withDefault 0
                                    in
                                    Success { t | comments = Just (LE.setAt n comment comments) }

                                other ->
                                    other

                        resetForm =
                            initCommentPatchForm model.user [ ( "reflink", "" ), ( "focusid", "" ) ]
                    in
                    ( { model | tension_result = tension_r, comment_form = resetForm, comment_result = result }, noOut )

                _ ->
                    ( { model | comment_result = result }, noOut )

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

        ChangeInputViewMode viewMode ->
            let
                form =
                    model.tension_form
            in
            ( { model | tension_form = { form | viewMode = viewMode } }, noOut )

        ChangeUpdateViewMode viewMode ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | viewMode = viewMode } }, noOut )

        OnRichText targetid command ->
            ( model, out0 [ Ports.richText targetid command ] )

        OnToggleMdHelp targetid ->
            case targetid of
                "commentInput" ->
                    let
                        form =
                            model.tension_form

                        field =
                            "isMdHelpOpen" ++ targetid

                        v =
                            Dict.get field form.post |> withDefault "false"

                        value =
                            ternary (v == "true") "false" "true"
                    in
                    ( { model | tension_form = { form | post = Dict.insert field value form.post } }, noOut )

                "updateCommentInput" ->
                    let
                        form =
                            model.comment_form

                        field =
                            "isMdHelpOpen" ++ targetid

                        v =
                            Dict.get field form.post |> withDefault "false"

                        value =
                            ternary (v == "true") "false" "true"
                    in
                    ( { model | comment_form = { form | post = Dict.insert field value form.post } }, noOut )

                _ ->
                    ( model, noOut )

        OnAddReaction cid type_ ->
            case model.user of
                LoggedIn uctx ->
                    ( model, out0 [ addReaction apis uctx.username cid type_ OnAddReactionAck ] )

                LoggedOut ->
                    ( model, out0 [ Ports.raiseAuthModal (uctxFromUser model.user) ] )

        OnAddReactionAck result ->
            let
                uctx =
                    uctxFromUser model.user
            in
            case parseErr result 2 of
                Authenticate ->
                    ( model, out0 [ Ports.raiseAuthModal uctx ] )

                OkAuth r ->
                    let
                        tension_result =
                            model.tension_result
                                |> withMapData (\tc -> { tc | comments = Maybe.map (pushCommentReaction uctx.username r) tc.comments })
                    in
                    ( { model | tension_result = tension_result }, noOut )

                _ ->
                    ( model, noOut )

        OnDeleteReaction cid type_ ->
            case model.user of
                LoggedIn uctx ->
                    ( model, out0 [ deleteReaction apis uctx.username cid type_ OnDeleteReactionAck ] )

                LoggedOut ->
                    ( model, out0 [ Ports.raiseAuthModal (uctxFromUser model.user) ] )

        OnDeleteReactionAck result ->
            let
                uctx =
                    uctxFromUser model.user
            in
            case parseErr result 2 of
                Authenticate ->
                    ( model, out0 [ Ports.raiseAuthModal uctx ] )

                OkAuth r ->
                    let
                        tension_result =
                            model.tension_result
                                |> withMapData (\tc -> { tc | comments = Maybe.map (removeCommentReaction uctx.username r) tc.comments })
                    in
                    ( { model | tension_result = tension_result }, noOut )

                _ ->
                    ( model, noOut )

        -- Components
        LabelSearchPanelMsg msg ->
            let
                ( panel, out ) =
                    LabelSearchPanel.update apis msg model.labelsPanel

                t =
                    Maybe.map
                        (\r ->
                            withMapData
                                (\x ->
                                    let
                                        labels =
                                            if Tuple.first r then
                                                withDefault [] x.labels ++ [ Tuple.second r ]

                                            else
                                                LE.remove (Tuple.second r) (withDefault [] x.labels)
                                    in
                                    { x | labels = Just labels }
                                )
                                model.tension_result
                        )
                        out.result
                        |> withDefault model.tension_result

                --isLabelOpen =
                --    LabelSearchPanel.isOpen_ panel
                --
                --( cmds, gcmds ) =
                --    mapGlobalOutcmds out.gcmds
            in
            ( { model | labelsPanel = panel, tension_result = t }
            , out2 (out.cmds |> List.map (\m -> Cmd.map LabelSearchPanelMsg m)) out.gcmds
            )

        UserSearchPanelMsg msg ->
            let
                ( panel, out ) =
                    UserSearchPanel.update apis msg model.assigneesPanel

                t =
                    Maybe.map
                        (\r ->
                            withMapData
                                (\x ->
                                    let
                                        assignees =
                                            if Tuple.first r then
                                                withDefault [] x.assignees ++ [ Tuple.second r ]

                                            else
                                                LE.remove (Tuple.second r) (withDefault [] x.assignees)
                                    in
                                    { x | assignees = Just assignees }
                                )
                                model.tension_result
                        )
                        out.result
                        |> withDefault model.tension_result

                --isAssigneeOpen =
                --    UserSearchPanel.isOpen_ panel
                --( cmds, gcmds ) =
                --    mapGlobalOutcmds out.gcmds
            in
            ( { model | assigneesPanel = panel, tension_result = t }
            , out2 (out.cmds |> List.map (\m -> Cmd.map UserSearchPanelMsg m)) out.gcmds
            )

        UserInputMsg msg ->
            let
                ( data, out ) =
                    UserInput.update apis msg model.userInput

                cmd =
                    case out.result of
                        Just ( selected, us ) ->
                            if selected then
                                case us of
                                    [ u ] ->
                                        Ports.pushInputSelection u.username

                                    _ ->
                                        Cmd.none

                            else
                                Cmd.none

                        Nothing ->
                            Cmd.none

                --( cmds, gcmds ) =
                --    mapGlobalOutcmds out.gcmds
            in
            ( { model | userInput = data }, out2 (cmd :: (out.cmds |> List.map (\m -> Cmd.map UserInputMsg m))) out.gcmds )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    if model.isOpen then
        [ Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
        , closeCardPanelFromJs (always OnClose)
        ]
            ++ (LabelSearchPanel.subscriptions model.labelsPanel |> List.map (\s -> Sub.map LabelSearchPanelMsg s))
            ++ (UserSearchPanel.subscriptions model.assigneesPanel |> List.map (\s -> Sub.map UserSearchPanelMsg s))

    else
        []


port closeCardPanelFromJs : (() -> msg) -> Sub msg



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    { conf : Conf

    --tree_data : GqlData NodesDict
    , path_data : GqlData LocalGraph
    }


view : Op -> State -> Html Msg
view op (State model) =
    div
        [ id "cardPanel"
        , class "side-menu"
        , classList [ ( "off", not model.isOpen ) ]
        ]
        [ case model.card.card of
            CardTension _ ->
                case model.tension_result of
                    Success tension ->
                        viewPanelTension op tension model

                    LoadingSlowly ->
                        div [ class "spinner" ] []

                    Failure err ->
                        viewGqlErrors err

                    _ ->
                        text ""

            CardDraft draft ->
                viewPanelDraft op draft model
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        ]



--
-- Tension views
--


viewPanelTension : Op -> TensionPanel -> Model -> Html Msg
viewPanelTension op tension model =
    div [ class "panel" ]
        [ div [ class "header-block" ]
            [ div [ class "panel-heading" ]
                [ text tension.title, button [ class "delete is-pulled-right", onClick OnClose ] [] ]
            ]
        , div [ class "main-block" ]
            [ viewTensionComments op tension model
            ]
        ]


viewTensionComments : Op -> TensionPanel -> Model -> Html Msg
viewTensionComments op t model =
    let
        userCanJoin =
            withMaybeData op.path_data
                |> Maybe.map
                    (\path ->
                        path.root |> Maybe.map (\r -> r.userCanJoin == Just True) |> withDefault False
                    )
                |> withDefault False

        userCanComment =
            -- Author or member can comment tension.
            -- is Author
            (uctx.username == t.createdBy.username)
                || -- is Member
                   (getOrgaRoles [ t.receiver.nameid ] uctx.roles /= [])

        userInput =
            case model.user of
                LoggedIn _ ->
                    if userCanComment then
                        Comments.viewTensionCommentInput t model.comments

                    else
                        viewJoinForCommentNeeded userCanJoin

                LoggedOut ->
                    if userCanJoin then
                        viewJoinForCommentNeeded userCanJoin

                    else
                        text ""

        comments =
            withDefault [] t.comments

        history =
            withDefault [] t.history
    in
    div [ class "comments" ]
        [ Comments.viewCommentsTension t.action history comments model.comments
        , hr [ class "has-background-border-light is-2" ] []
        , userInput
        ]



--
-- Draft views
--


viewPanelDraft : Op -> ProjectDraft -> Model -> Html Msg
viewPanelDraft op draft model =
    div [ class "panel" ]
        [ div [ class "header-block" ]
            [ div [ class "panel-heading" ] [ text draft.title, button [ class "delete is-pulled-right", onClick OnClose ] [] ]
            ]
        , div [ class "main-block" ]
            [ viewDraftComment draft
            ]
        ]


viewDraftComment : ProjectDraft -> Html Msg
viewDraftComment draft =
    div [ class "message" ]
        [ div [ class "message-header" ] [ text "" ]
        , div [ class "message-body" ] [ text "" ]
        ]
