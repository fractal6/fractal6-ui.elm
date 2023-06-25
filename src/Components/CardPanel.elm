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
import Auth exposing (ErrState(..), getTensionRights, parseErr)
import Bulk exposing (CommentPatchForm, Ev, InputViewMode, TensionForm, UserState(..), eventFromForm, initCommentPatchForm, initTensionForm, pushCommentReaction, removeCommentReaction, uctxFromUser)
import Bulk.Bulma as B
import Bulk.Codecs exposing (DocType(..), FractalBaseRoute(..), NodeFocus, getOrgaRoles, toLink)
import Bulk.Error exposing (viewGqlErrors, viewJoinForCommentNeeded, viewMaybeErrors)
import Bulk.View exposing (action2icon, statusColor, tensionIcon2, tensionIcon3, tensionStatus2str, viewCircleTarget, viewTensionDateAndUser, viewTensionLight)
import Components.Comments as Comments
import Components.LabelSearchPanel as LabelSearchPanel
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.TreeMenu as TreeMenu exposing (viewSelectorTree)
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
import Html.Attributes exposing (attribute, autofocus, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, spellcheck, style, target, type_, value)
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
import Scroll
import Session exposing (Apis, Conf, GlobalCmd(..), toReflink)
import Text as T
import Time



-- ------------------------------
-- M O D E L
-- ------------------------------


type State
    = State Model


type alias Model =
    { user : UserState
    , node_focus : NodeFocus
    , isOpen : Bool
    , card : ProjectCard
    , tension_result : GqlData TensionPanel
    , isTensionAdmin : Bool
    , path_data : GqlData LocalGraph -- tensionadmin

    -- Title
    , isTitleEdit : Bool
    , title_result : GqlData IdPayload
    , tension_form : TensionForm

    -- Components
    , assigneesPanel : UserSearchPanel.State
    , labelsPanel : LabelSearchPanel.State
    , comments : Comments.State

    -- Common
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    }


initModel : GqlData LocalGraph -> NodeFocus -> UserState -> Model
initModel path focus user =
    { user = user
    , node_focus = focus
    , isOpen = False
    , card = emptyCard
    , tension_result = NotAsked
    , isTensionAdmin = False
    , path_data = path

    -- Title Result
    , isTitleEdit = False
    , title_result = NotAsked
    , tension_form = initTensionForm "" Nothing user

    -- Components
    , assigneesPanel = UserSearchPanel.load Nothing user
    , labelsPanel = LabelSearchPanel.load Nothing user
    , comments = Comments.init focus.nameid "" user

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }


init : GqlData LocalGraph -> NodeFocus -> UserState -> State
init path focus user =
    initModel path focus user |> State



-- Global methods
-- not yet
-- State Controls


resetModel : Model -> Model
resetModel model =
    initModel model.path_data model.node_focus model.user



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = OnOpen ProjectCard
    | OnOutsideClickClose
    | OnClose
    | OnSetPath (GqlData LocalGraph)
      --
      -- Tension action/patching
      --
    | OnSubmit Bool (Time.Posix -> Msg)
    | OnQueryTension String
    | OnTensionAck (GqlData TensionPanel)
      -- Title
    | DoChangeTitle
    | OnChangeTitle String
    | OnCancelTitle
    | SubmitTitle Time.Posix
    | TitleAck (GqlData IdPayload)
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Common
    | NoMsg
    | LogErr String
    | ScrollToElement String
      -- Components
    | LabelSearchPanelMsg LabelSearchPanel.Msg
    | UserSearchPanelMsg UserSearchPanel.Msg
    | CommentsMsg Comments.Msg


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
            , out0 <|
                [ sendSleep OnOutsideClickClose 500 ]
                    ++ (case card.card of
                            CardTension a ->
                                [ send (OnQueryTension a.id)
                                , Cmd.map CommentsMsg (send <| Comments.SetTensionid a.id)
                                ]

                            CardDraft a ->
                                -- @todo
                                [ send NoMsg ]
                       )
            )

        OnOutsideClickClose ->
            ( model, out0 [ Ports.outsideClickClose "closeCardPanelFromJs" "cardPanel" ] )

        OnClose ->
            -- @warning: Ports.click to reset the outsideClickClose handler.
            ( resetModel model, out0 [ Ports.click "" ] )

        OnSetPath path_data ->
            ( { model | path_data = path_data }, noOut )

        OnSubmit isSendable next ->
            if isSendable then
                ( model, out0 [ sendNow next ] )

            else
                ( model, noOut )

        OnQueryTension tid ->
            let
                form =
                    model.tension_form
            in
            ( { model | tension_result = Loading, tension_form = { form | id = tid } }
            , out0 [ getTensionPanel apis (uctxFromUser model.user) tid OnTensionAck ]
            )

        OnTensionAck result ->
            case result of
                Success d ->
                    -- Memory Optimization: Do no store comments twice.
                    let
                        comments =
                            withDefault [] d.comments

                        history =
                            withDefault [] d.history

                        isAdmin =
                            getTensionRights (uctxFromUser model.user) result model.path_data
                    in
                    ( { model
                        | tension_result = Success { d | comments = Nothing, history = Nothing }
                        , isTensionAdmin = isAdmin
                      }
                    , out0
                        [ Cmd.map CommentsMsg (send <| Comments.SetComments comments)
                        , Cmd.map CommentsMsg (send <| Comments.SetHistory history)
                        , Ports.bulma_driver "cardPanel"
                        ]
                    )

                _ ->
                    ( { model | tension_result = result }, noOut )

        DoChangeTitle ->
            ( { model | isTitleEdit = True }, out0 [ Ports.focusOn "titleInput" ] )

        OnChangeTitle value ->
            let
                form =
                    model.tension_form
            in
            ( { model | tension_form = { form | post = Dict.insert "title" value form.post } }, noOut )

        OnCancelTitle ->
            ( { model | isTitleEdit = False, tension_form = initTensionForm model.tension_form.id Nothing model.user, title_result = NotAsked }, noOut )

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
                            initTensionForm model.tension_form.id Nothing model.user
                    in
                    ( { model | tension_result = tension_r, tension_form = resetForm, title_result = result, isTitleEdit = False }
                    , noOut
                    )

                _ ->
                    ( { model | title_result = result }, noOut )

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

        ScrollToElement did ->
            ( model, out0 [ Scroll.scrollToSubElement "main-block" did NoMsg ] )

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

        CommentsMsg msg ->
            let
                ( data, out ) =
                    Comments.update apis msg model.comments
            in
            ( { model | comments = data }
            , out2 (out.cmds |> List.map (\m -> Cmd.map CommentsMsg m)) out.gcmds
            )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    if model.isOpen then
        [ Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
        , closeCardPanelFromJs (always OnClose)
        ]
            ++ (LabelSearchPanel.subscriptions model.labelsPanel |> List.map (\s -> Sub.map LabelSearchPanelMsg s))
            ++ (UserSearchPanel.subscriptions model.assigneesPanel |> List.map (\s -> Sub.map UserSearchPanelMsg s))
            ++ (Comments.subscriptions model.comments |> List.map (\s -> Sub.map CommentsMsg s))

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
    div []
        [ div
            [ id "cardPanel"
            , class "side-menu is-large"
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
        , if model.isOpen then
            div [ class "side-menu-background", onClick OnClose ] []

          else
            text ""
        ]



--
-- Tension views
--


viewPanelTension : Op -> TensionPanel -> Model -> Html Msg
viewPanelTension op t model =
    div [ class "panel" ]
        [ div [ class "header-block" ]
            [ div [ class "panel-heading" ]
                [ if model.isTitleEdit then
                    let
                        newTitle =
                            Dict.get "title" model.tension_form.post |> withDefault t.title
                    in
                    viewTitleEdit newTitle t.title model.title_result

                  else
                    viewTitle t model
                , viewSubTitle op.conf t model
                ]
            ]
        , div [ id "main-block", class "main-block" ]
            [ viewTensionComments op t model
            ]
        ]


viewTitle : TensionPanel -> Model -> Html Msg
viewTitle t model =
    let
        uctx =
            uctxFromUser model.user

        isAuthor =
            t.createdBy.username == uctx.username
    in
    div []
        [ a
            [ class "title tensionTitle discrete-link is-human"
            , href (toLink TensionBaseUri t.receiver.nameid [ t.id ])
            ]
            [ text t.title ]
        , div [ class "is-pulled-right" ]
            [ if (model.isTensionAdmin || isAuthor) && t.action == Nothing then
                span
                    [ class "is-small button-light tooltip has-tooltip-arrow has-tooltip-left mr-4"
                    , attribute "data-tooltip" T.editTitle
                    , onClick DoChangeTitle
                    ]
                    [ A.icon "icon-edit-2" ]

              else
                text ""
            , button [ class "delete", onClick OnClose ] []
            ]
        ]


viewTitleEdit : String -> String -> GqlData IdPayload -> Html Msg
viewTitleEdit new old result =
    let
        isLoading =
            result == LoadingSlowly

        isSendable =
            new /= old
    in
    div []
        [ div [ class "field is-grouped" ]
            [ p [ class "control is-expanded" ]
                [ input
                    [ id "titleInput"
                    , class "input is-human"
                    , type_ "text"
                    , placeholder "Title*"
                    , spellcheck True
                    , value new
                    , onInput OnChangeTitle
                    ]
                    []
                ]
            , p [ class "control buttons" ]
                [ button
                    [ class "button is-success is-small"
                    , classList [ ( "is-loading", isLoading ) ]
                    , disabled (not isSendable)
                    , onClick (OnSubmit (isSendable && not isLoading) SubmitTitle)
                    ]
                    [ text T.update ]
                , button [ class "button is-small", onClick OnCancelTitle ] [ text T.cancel ]
                ]
            ]
        , viewMaybeErrors result
        ]


viewSubTitle : Conf -> TensionPanel -> Model -> Html Msg
viewSubTitle conf t model =
    div [ class "tensionSubtitle my-2" ]
        [ span
            [ class "tag is-rounded has-background-tag"

            --, classList [ ( "is-w", model.isTensionAdmin || isAuthor ) ]
            --, ternary (model.isTensionAdmin || isAuthor) (onClick <| SelectTypeMsg (SelectType.OnOpen t.type_)) (onClick NoMsg)
            ]
            [ tensionIcon2 t.type_ ]
        , if t.type_ /= TensionType.Governance || t.status == TensionStatus.Open then
            -- As Governance tension get automatically closed when there are created,
            -- there status is not relevant, I can cause confusion to user as the object exists.
            span [ class ("is-w tag is-rounded is-" ++ statusColor t.status), onClick (ScrollToElement "tensionCommentInput") ]
                [ t.status |> tensionStatus2str |> text ]

          else
            text ""
        , viewTensionDateAndUser conf "is-discrete" t.createdAt t.createdBy
        , viewCircleTarget { noMsg = NoMsg } "is-pulled-right" t.receiver
        ]


viewTensionComments : Op -> TensionPanel -> Model -> Html Msg
viewTensionComments op t model =
    let
        conf =
            op.conf

        mobileConf =
            { conf | screen = { w = 1, h = 1 } }

        userCanJoin =
            withMaybeData op.path_data
                |> Maybe.map
                    (\path ->
                        path.root |> Maybe.map (\r -> r.userCanJoin == Just True) |> withDefault False
                    )
                |> withDefault False

        userCanComment =
            case model.user of
                LoggedIn uctx ->
                    -- Author or member can comment tension.
                    -- is Author
                    (uctx.username == t.createdBy.username)
                        || -- is Member
                           (getOrgaRoles [ t.receiver.nameid ] uctx.roles /= [])

                LoggedOut ->
                    False

        userInput =
            case model.user of
                LoggedIn _ ->
                    if userCanComment then
                        Comments.viewTensionCommentInput mobileConf t model.comments |> Html.map CommentsMsg

                    else
                        viewJoinForCommentNeeded userCanJoin

                LoggedOut ->
                    if userCanJoin then
                        viewJoinForCommentNeeded userCanJoin

                    else
                        text ""
    in
    div [ class "comments" ]
        [ Comments.viewCommentsTension mobileConf t.action model.comments |> Html.map CommentsMsg
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
