{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2024 Fractale Co

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


port module Components.CardPanel exposing (CardPanelResult(..), Msg(..), State, init, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), getTensionRights, parseErr)
import Bulk exposing (CommentPatchForm, Ev, InputViewMode, TensionForm, UserState(..), eventFromForm, getPath, initCommentPatchForm, initTensionForm, pushCommentReaction, removeCommentReaction, uctxFromUser)
import Bulk.Bulma as B
import Bulk.Codecs exposing (DocType(..), FractalBaseRoute(..), NodeFocus, getOrgaRoles, toLink)
import Bulk.Error exposing (viewGqlErrors, viewJoinForCommentNeeded, viewMaybeErrors)
import Bulk.View exposing (action2icon, statusColor, tensionIcon2, tensionIcon3, tensionStatus2str, viewCircleTarget, viewLabels, viewTensionDateAndUser, viewTensionLight, viewUsers)
import Components.Comments as Comments exposing (OutType(..), viewCommentInputHeader)
import Components.LabelSearchPanel as LabelSearchPanel
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.UserSearchPanel as UserSearchPanel
import Dict exposing (Dict)
import Extra exposing (showIf, ternary, textH, unwrap, unwrap2, upH)
import Extra.Events exposing (onClickPD, onClickSP, onKeydown)
import Form exposing (isPostEmpty, isPostSendable)
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
import Loading exposing (GqlData, ModalData, RequestResult(..), isFailure, isSuccess, loadingSpin, withDefaultData, withMapData, withMaybeData, withMaybeMapData)
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelSchema exposing (CardKind(..), Comment, FocusNode, IdPayload, LocalGraph, Node, NodesDict, PatchTensionPayloadID, Post, ProjectCard, ProjectColumn, ProjectDraft, ReactionResponse, TensionLight, TensionPanel, UserCtx, emptyCard, initFocusNode, node2focus)
import Org.Tensions exposing (TypeFilter(..), defaultTypeFilter, typeDecoder, typeFilter2Text)
import Ports
import Query.PatchTension exposing (patchComment, patchLiteral, pushTensionPatch)
import Query.PatchUser exposing (toggleTensionSubscription)
import Query.QueryProject exposing (updateProjectDraft)
import Query.QueryTension exposing (getTensionPanel)
import Scroll
import Session exposing (Apis, CommonMsg, GlobalCmd(..), LabelSearchPanelOnClickAction(..), Session, UserSearchPanelOnClickAction(..), isMobile, toReflink)
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
    , path_data : GqlData LocalGraph -- tensionadmin
    , card : ProjectCard
    , isTensionAdmin : Bool
    , tension_result : GqlData TensionPanel
    , subscribe_result : GqlData Bool -- init a GotTensionHead

    -- Title / Draft Message
    , tension_form : TensionForm
    , isTitleEdit : Bool
    , title_result : GqlData IdPayload
    , isMessageEdit : Bool
    , message_result : GqlData IdPayload

    -- Components
    , assigneesPanel : UserSearchPanel.State
    , labelsPanel : LabelSearchPanel.State
    , comments : Comments.State

    -- Common
    , session : Session
    , mobileConf : Session
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    , commonOp : CommonMsg Msg
    }


initModel : Session -> GqlData LocalGraph -> NodeFocus -> UserState -> Model
initModel session path focus user =
    { user = user
    , node_focus = focus
    , isOpen = False
    , path_data = path
    , card = emptyCard
    , isTensionAdmin = False
    , tension_result = NotAsked
    , subscribe_result = NotAsked

    -- Title / Draft message
    , tension_form = initTensionForm "" Nothing user
    , isTitleEdit = False
    , title_result = NotAsked
    , isMessageEdit = False
    , message_result = NotAsked

    -- Components
    , assigneesPanel = UserSearchPanel.init "" AssignUser user
    , labelsPanel = LabelSearchPanel.init "" AssignLabel user
    , comments = Comments.init focus.nameid "" user

    -- Common
    , session = session
    , mobileConf = { session | screen = { w = 1, h = 1 } }
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    , commonOp = CommonMsg NoMsg LogErr
    }


init : Session -> GqlData LocalGraph -> NodeFocus -> UserState -> State
init session path focus user =
    initModel session path focus user |> State



-- Global methods
-- not yet
-- State Controls


resetModel : Model -> Model
resetModel model =
    initModel model.session model.path_data model.node_focus model.user



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
    | ToggleSubscription String
    | GotIsSubscribe (GqlData Bool)
      -- Title
    | DoChangeTitle
    | OnChangeTitle String
    | OnCancelTitle
    | SubmitTitle Time.Posix
    | TitleAck (GqlData IdPayload)
      -- Draft Message
    | DoChangeMessage
    | OnChangeMessage String
    | OnCancelMessage
    | SubmitMessage Time.Posix
    | MessageAck (GqlData IdPayload)
    | DoConvertDraft String ProjectDraft
    | DoRemoveDraft String
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Common
    | NoMsg
    | LogErr String
    | ScrollToElement String
    | ChangeInputViewMode InputViewMode
    | OnRichText String String
    | OnToggleMdHelp String
      -- Components
    | DoAssigneeEdit
    | UserSearchPanelMsg UserSearchPanel.Msg
    | DoLabelEdit
    | LabelSearchPanelMsg LabelSearchPanel.Msg
    | CommentsMsg Comments.Msg


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe CardPanelResult
    }


type CardPanelResult
    = UpdateCard ProjectCard
    | ConvertDraft String ProjectDraft
    | DeleteDraft String


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
            let
                form =
                    model.tension_form
            in
            case card.card of
                CardTension a ->
                    ( { model | isOpen = True, card = card, tension_form = { form | id = a.id } }
                    , out0
                        [ sendSleep OnOutsideClickClose 500
                        , send (OnQueryTension a.id)
                        , Cmd.map CommentsMsg (send <| Comments.SetTensionid a.id)
                        , Cmd.map UserSearchPanelMsg (send <| UserSearchPanel.SetTensionid a.id)
                        , Cmd.map LabelSearchPanelMsg (send <| LabelSearchPanel.SetTensionid a.id)
                        ]
                    )

                CardDraft a ->
                    ( { model | isOpen = True, card = card, tension_form = { form | id = a.id } }
                    , out0 [ sendSleep OnOutsideClickClose 500 ]
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
            ( { model | tension_result = Loading }
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
                        , Cmd.map CommentsMsg (send <| Comments.SetHistory history Nothing)
                        , Ports.bulma_driver "cardPanel"
                        ]
                    )

                _ ->
                    ( { model | tension_result = result }, noOut )

        ToggleSubscription username ->
            case model.tension_result of
                Success th ->
                    ( { model | subscribe_result = LoadingSlowly }
                    , out0 [ toggleTensionSubscription apis username model.tension_form.id (not th.isSubscribed) GotIsSubscribe ]
                    )

                _ ->
                    ( model, noOut )

        GotIsSubscribe result ->
            case result of
                Success d ->
                    let
                        th =
                            withMapData (\x -> { x | isSubscribed = d }) model.tension_result
                    in
                    ( { model | subscribe_result = result, tension_result = th }, noOut )

                _ ->
                    ( { model | subscribe_result = result }, noOut )

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
                        | post = form.post |> Dict.insert "createdAt" (fromTime time)
                        , events =
                            [ Ev TensionEvent.TitleUpdated
                                (model.tension_result |> withMaybeMapData .title |> withDefault "")
                                (Dict.get "title" form.post |> withDefault "")
                            ]
                    }
            in
            case model.card.card of
                CardTension _ ->
                    ( { model | tension_form = newForm, title_result = LoadingSlowly }, out0 [ patchLiteral apis model.tension_form TitleAck ] )

                CardDraft _ ->
                    ( { model | tension_form = newForm, title_result = LoadingSlowly }, out0 [ updateProjectDraft apis model.tension_form TitleAck ] )

        TitleAck result ->
            case parseErr result 2 of
                OkAuth _ ->
                    let
                        newTitle =
                            Dict.get "title" model.tension_form.post |> withDefault ""

                        tension_r =
                            case model.card.card of
                                CardTension _ ->
                                    case model.tension_result of
                                        Success t ->
                                            Success { t | title = newTitle }

                                        other ->
                                            other

                                CardDraft d ->
                                    model.tension_result

                        card =
                            model.card

                        card_r =
                            case model.card.card of
                                CardTension t ->
                                    CardTension { t | title = newTitle }

                                CardDraft d ->
                                    CardDraft { d | title = newTitle }

                        newCard =
                            { card | card = card_r }

                        resetForm =
                            initTensionForm model.tension_form.id Nothing model.user
                    in
                    ( { model
                        | card = newCard
                        , tension_result = tension_r
                        , tension_form = resetForm
                        , title_result = result
                        , isTitleEdit = False
                      }
                    , Out [] [] (Just (UpdateCard newCard))
                    )

                _ ->
                    ( { model | title_result = result }, noOut )

        -- Draft Message
        DoChangeMessage ->
            ( { model | isMessageEdit = True }, out0 [ Ports.focusOn "draftInput", Ports.bulma_driver "cardPanel" ] )

        OnChangeMessage value ->
            let
                form =
                    model.tension_form
            in
            ( { model | tension_form = { form | post = Dict.insert "message" value form.post } }, noOut )

        OnCancelMessage ->
            ( { model | isMessageEdit = False, tension_form = initTensionForm model.tension_form.id Nothing model.user, message_result = NotAsked }, noOut )

        SubmitMessage time ->
            let
                form =
                    model.tension_form

                newForm =
                    { form | post = form.post |> Dict.insert "createdAt" (fromTime time) }
            in
            ( { model | tension_form = newForm, message_result = LoadingSlowly }, out0 [ updateProjectDraft apis model.tension_form MessageAck ] )

        MessageAck result ->
            case parseErr result 2 of
                OkAuth _ ->
                    let
                        newMessage =
                            Dict.get "message" model.tension_form.post

                        card =
                            model.card

                        card_r =
                            case model.card.card of
                                CardDraft d ->
                                    CardDraft { d | message = newMessage }

                                _ ->
                                    model.card.card

                        newCard =
                            { card | card = card_r }

                        resetForm =
                            initTensionForm model.tension_form.id Nothing model.user
                    in
                    ( { model
                        | card = newCard
                        , tension_form = resetForm
                        , message_result = result
                        , isMessageEdit = False
                      }
                    , Out [] [] (Just (UpdateCard newCard))
                    )

                _ ->
                    ( { model | message_result = result }, noOut )

        DoConvertDraft cardid d ->
            ( model, Out [ send OnClose ] [] (Just <| ConvertDraft cardid d) )

        DoRemoveDraft cardid ->
            ( model, Out [ send OnClose ] [] (Just <| DeleteDraft cardid) )

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

        ChangeInputViewMode viewMode ->
            let
                form =
                    model.tension_form
            in
            ( { model | tension_form = { form | viewMode = viewMode } }, noOut )

        OnRichText targetid command ->
            ( model, out0 [ Ports.richText targetid command ] )

        OnToggleMdHelp targetid ->
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

        -- Components
        LabelSearchPanelMsg msg ->
            let
                ( panel, out ) =
                    LabelSearchPanel.update apis msg model.labelsPanel

                ( tension_r, mout ) =
                    Maybe.map2
                        (\r x ->
                            let
                                card =
                                    model.card

                                labels =
                                    if Tuple.first r then
                                        withDefault [] x.labels ++ [ Tuple.second r ]

                                    else
                                        LE.remove (Tuple.second r) (withDefault [] x.labels)
                            in
                            ( Success { x | labels = Just labels }
                            , case card.card of
                                CardTension t ->
                                    Just <| UpdateCard { card | card = CardTension { t | labels = Just labels } }

                                CardDraft _ ->
                                    Nothing
                            )
                        )
                        out.result
                        (withMaybeData model.tension_result)
                        |> withDefault ( model.tension_result, Nothing )

                --isLabelOpen =
                --    LabelSearchPanel.isOpen_ panel
                --
                --( cmds, gcmds ) =
                --    mapGlobalOutcmds out.gcmds
            in
            ( { model | labelsPanel = panel, tension_result = tension_r }
            , Out (out.cmds |> List.map (\m -> Cmd.map LabelSearchPanelMsg m)) out.gcmds mout
            )

        DoAssigneeEdit ->
            let
                targets =
                    getPath model.path_data |> List.map .nameid
            in
            ( model, out0 [ Cmd.map UserSearchPanelMsg (send (UserSearchPanel.OnOpen targets)) ] )

        DoLabelEdit ->
            let
                targets =
                    getPath model.path_data |> List.map .nameid

                receiver_m =
                    withMaybeData model.tension_result |> Maybe.map .receiver
            in
            case receiver_m of
                Just receiver ->
                    case LE.elemIndex receiver.nameid targets of
                        Just i ->
                            -- receiver is in the current focus/path
                            ( model, out0 [ Cmd.map LabelSearchPanelMsg (send (LabelSearchPanel.OnOpen (List.take (i + 1) targets) Nothing)) ] )

                        Nothing ->
                            -- receiver does not match the current focus/path
                            ( model, out0 [ Cmd.map LabelSearchPanelMsg (send (LabelSearchPanel.OnOpen [ receiver.nameid ] (Just False))) ] )

                Nothing ->
                    ( model, out0 [ Cmd.map LabelSearchPanelMsg (send (LabelSearchPanel.OnOpen targets Nothing)) ] )

        UserSearchPanelMsg msg ->
            let
                ( panel, out ) =
                    UserSearchPanel.update apis msg model.assigneesPanel

                ( tension_r, mout ) =
                    Maybe.map2
                        (\r x ->
                            let
                                card =
                                    model.card

                                assignees =
                                    if Tuple.first r then
                                        withDefault [] x.assignees ++ [ Tuple.second r ]

                                    else
                                        LE.remove (Tuple.second r) (withDefault [] x.assignees)
                            in
                            ( Success { x | assignees = Just assignees }
                            , case card.card of
                                CardTension t ->
                                    Just <| UpdateCard { card | card = CardTension { t | assignees = Just assignees } }

                                CardDraft _ ->
                                    Nothing
                            )
                        )
                        out.result
                        (withMaybeData model.tension_result)
                        |> withDefault ( model.tension_result, Nothing )

                --isAssigneeOpen =
                --    UserSearchPanel.isOpen_ panel
                --( cmds, gcmds ) =
                --    mapGlobalOutcmds out.gcmds
            in
            ( { model | assigneesPanel = panel, tension_result = tension_r }
            , Out (out.cmds |> List.map (\m -> Cmd.map UserSearchPanelMsg m)) out.gcmds mout
            )

        CommentsMsg msg ->
            let
                ( data, out ) =
                    Comments.update apis msg model.comments

                tension_result =
                    case out.result of
                        Just (TensionCommentAdded status) ->
                            withMapData (\t -> { t | status = withDefault t.status status }) model.tension_result

                        _ ->
                            model.tension_result
            in
            ( { model | comments = data, tension_result = tension_result }
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


view : GqlData NodesDict -> GqlData LocalGraph -> State -> Html Msg
view tree_data path_data (State model) =
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
                            viewPanelTension path_data tension model

                        LoadingSlowly ->
                            div [ class "spinner" ] []

                        Failure err ->
                            viewGqlErrors err

                        _ ->
                            text ""

                CardDraft draft ->
                    viewPanelDraft draft model
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


viewPanelTension : GqlData LocalGraph -> TensionPanel -> Model -> Html Msg
viewPanelTension path_data t model =
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
                , viewSubTitle model.session t model
                , hr [ class "my-0" ] []
                ]
            ]
        , div [ id "main-block", class "main-block" ]
            [ div [ class "columns m-0" ]
                [ div [ class "column is-9" ] [ viewTensionComments path_data t model ]
                , div [ class "column pl-1" ] [ viewTensionSidePane t model ]
                ]
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
            , target "_blank"
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


viewSubTitle : Session -> TensionPanel -> Model -> Html Msg
viewSubTitle session t model =
    div [ class "tensionSubtitle level mt-4" ]
        [ div [ class "level-left" ] <|
            List.map (div [ class "level-item" ] << List.singleton) <|
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
                , viewTensionDateAndUser session "is-discrete" t.createdAt t.createdBy
                ]
        , div [ class "level-right" ] <|
            List.map (div [ class "level-item" ] << List.singleton) <|
                [ viewCircleTarget model.commonOp "" t.receiver ]
        ]


viewTensionComments : GqlData LocalGraph -> TensionPanel -> Model -> Html Msg
viewTensionComments path_data t model =
    let
        userCanJoin =
            withMaybeData path_data
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
                        Comments.viewTensionCommentInput model.mobileConf t model.comments |> Html.map CommentsMsg

                    else
                        viewJoinForCommentNeeded userCanJoin

                LoggedOut ->
                    if userCanJoin then
                        viewJoinForCommentNeeded userCanJoin

                    else
                        text ""
    in
    div [ class "comments" ]
        [ Comments.viewCommentsTension model.mobileConf t.action model.comments |> Html.map CommentsMsg
        , hr [ class "has-background-border-light is-2" ] []
        , userInput
        ]


viewTensionSidePane : TensionPanel -> Model -> Html Msg
viewTensionSidePane t model =
    let
        uctx =
            uctxFromUser model.user

        isAuthor =
            t.createdBy.username == uctx.username

        isAdmin =
            model.isTensionAdmin

        hasAssigneeRight =
            isAdmin

        hasLabelRight =
            isAdmin || isAuthor

        --
        assignees =
            t.assignees |> withDefault []

        labels =
            t.labels |> withDefault []
    in
    div [ class "tensionSidePane" ]
        [ -- Assignees/User select
          div [ class "level" ]
            [ div [ class "level-left" ] [ text T.assignees ]
            , div
                [ class "level-item"
                , classList [ ( "is-w", hasAssigneeRight ) ]
                , ternary hasAssigneeRight (onClick DoAssigneeEdit) (onClick NoMsg)
                ]
                [ if List.length assignees > 0 then
                    viewUsers False assignees

                  else
                    div [ class "help", classList [ ( "is-w", hasAssigneeRight ) ] ] [ text T.addAssignees ]
                , div [ class "mt-4" ]
                    [ UserSearchPanel.view
                        { selectedAssignees = assignees
                        , targets = model.path_data |> withMaybeMapData (\x -> List.map .nameid x.path) |> withDefault []
                        , isRight = True
                        }
                        model.assigneesPanel
                        |> Html.map UserSearchPanelMsg
                    ]
                ]
            ]

        -- Label select
        , div
            [ class "level"
            ]
            [ div [ class "level-left" ] [ text T.labels ]
            , div
                [ class "level-item"
                , classList [ ( "is-w", hasLabelRight ) ]
                , ternary hasLabelRight (onClick DoLabelEdit) (onClick NoMsg)
                ]
                [ if List.length labels > 0 then
                    div [ class "tnesion-labelsList" ] [ viewLabels Nothing labels ]

                  else
                    div [ class "help", classList [ ( "is-w", hasLabelRight ) ] ] [ text T.addLabels ]
                , div [ class "mt-4" ]
                    [ LabelSearchPanel.view
                        { selectedLabels = labels
                        , targets = model.path_data |> withMaybeMapData (.focus >> .nameid >> List.singleton) |> withDefault []
                        , isRight = True
                        }
                        model.labelsPanel
                        |> Html.map LabelSearchPanelMsg
                    ]
                ]
            ]

        -- Subscriptions
        , hr [ class "has-background-border-light my-5" ] []
        , case model.user of
            LoggedIn _ ->
                let
                    ( iconElt, subscribe_txt ) =
                        case model.tension_result |> withMaybeData |> Maybe.map .isSubscribed of
                            Just True ->
                                ( A.icon1 "icon-bell-off icon-1x" T.unsubscribe, T.tensionSubscribeText )

                            Just False ->
                                ( A.icon1 "icon-bell icon-1x" T.subscribe, T.tensionUnsubscribeText )

                            Nothing ->
                                ( text "", "" )
                in
                div [ class "pb-0 my-5" ]
                    [ p
                        [ class "button is-fullwidth has-background-evidence is-small"
                        , style "border-radius" "5px"
                        , onClick (ToggleSubscription uctx.username)
                        ]
                        [ iconElt, loadingSpin (model.subscribe_result == LoadingSlowly) ]
                    , p [ class "help" ] [ text subscribe_txt ]
                    , case model.subscribe_result of
                        Failure err ->
                            viewGqlErrors err

                        _ ->
                            text ""
                    ]

            LoggedOut ->
                text ""

        -- Extras
        , hr [ class "has-background-border-light my-5" ] []
        , a
            [ class "is-smaller2 has-text-weight-semibold button-light discrete-link mb-4"
            , href (toLink TensionBaseUri t.receiver.nameid [ t.id ])
            , target "_blank"
            ]
            [ A.icon1 "icon-external-link" T.openNewTab ]
        ]



--
-- Draft views
--


viewPanelDraft : ProjectDraft -> Model -> Html Msg
viewPanelDraft draft model =
    let
        uctx =
            uctxFromUser model.user

        isAuthor =
            draft.createdBy.username == uctx.username

        -- @todo: get is draft admin
        -- @todo: edit Draft (case on SubmitTitle + edit draft comment
    in
    div [ class "panel" ]
        [ div [ class "header-block" ]
            [ div [ class "panel-heading" ]
                [ if model.isTitleEdit then
                    let
                        newTitle =
                            Dict.get "title" model.tension_form.post |> withDefault draft.title
                    in
                    viewTitleEdit newTitle draft.title model.title_result

                  else
                    div []
                        [ text draft.title
                        , div [ class "is-pulled-right" ]
                            [ if model.isTensionAdmin || isAuthor then
                                span
                                    [ class "is-small button-light tooltip has-tooltip-arrow has-tooltip-left mr-4"
                                    , attribute "data-tooltip" T.editTitle
                                    , onClick DoChangeTitle
                                    ]
                                    [ A.icon "icon-edit-2" ]

                              else
                                text ""
                            , button [ class "delete ", onClick OnClose ] []
                            ]
                        , div [ class "tensionSubtitle mt-3 level" ]
                            [ div [ class "level-left" ]
                                [ span [ class "level-item tag is-rounded has-background-tag" ]
                                    [ div [ class "help my-2" ] [ A.icon1 "icon-circle-draft" "Draft" ] ]
                                , div [ class "level-item" ] [ viewTensionDateAndUser model.session "is-discrete" draft.createdAt draft.createdBy ]
                                ]
                            ]
                        ]
                ]
            , hr [ class "my-0" ] []
            ]
        , div [ class "main-block" ]
            [ div [ class "columns m-0" ]
                [ div [ class "column is-9" ] [ viewDraftComment model.session model.isTensionAdmin model.isMessageEdit model.message_result model.tension_form draft ]
                , div [ class "column pl-1" ] [ viewDraftSidePane draft model ]
                ]
            ]
        ]


viewDraftComment : Session -> Bool -> Bool -> GqlData IdPayload -> TensionForm -> ProjectDraft -> Html Msg
viewDraftComment session isAdmin isEdit result form draft =
    let
        isAuthor =
            draft.createdBy.username == form.uctx.username

        message =
            withDefault "" draft.message
    in
    if isEdit then
        let
            new =
                Dict.get "message" form.post |> withDefault message
        in
        viewMessageEdit session new message form result

    else
        div [ class "message" ]
            [ div [ class "message-body" ]
                [ div []
                    [ showIf (isAdmin || isAuthor) <|
                        div [ class "level mb-0", style "margin-top" "-10px" ]
                            [ div [ class "level-left" ] []
                            , div [ class "level-right help button-light", onClick DoChangeMessage ] [ text T.edit ]
                            ]
                    , case message of
                        "" ->
                            div [ class "help is-italic" ] [ text T.noMessageProvided ]

                        msg ->
                            renderMarkdown "is-human" msg
                    ]
                ]
            ]


viewMessageEdit : Session -> String -> String -> TensionForm -> GqlData IdPayload -> Html Msg
viewMessageEdit session new old form result =
    let
        isLoading =
            result == LoadingSlowly

        isSendable =
            new /= old

        line_len =
            List.length <| String.lines new

        ( max_len, min_len ) =
            if isMobile session.screen then
                ( 6, 4 )

            else
                ( 15, 6 )

        opHeader =
            { onChangeViewMode = ChangeInputViewMode
            , onRichText = OnRichText
            , onToggleMdHelp = OnToggleMdHelp
            }
    in
    div [ class "submitFocus" ]
        [ div [ class "message" ]
            [ div [ class "message-header pb-0" ] [ viewCommentInputHeader opHeader "draftInput" form ]
            , div [ class "message-body" ]
                [ textarea
                    [ id "draftInput"
                    , class "textarea"
                    , rows (min max_len (max line_len min_len))
                    , placeholder T.leaveCommentOpt
                    , value new
                    , onInput <| OnChangeMessage
                    ]
                    []
                ]
            ]
        , case result of
            Failure err ->
                viewGqlErrors err

            _ ->
                text ""
        , div [ class "field is-grouped is-grouped-right" ]
            [ div [ class "control" ]
                [ div [ class "buttons" ]
                    [ button
                        [ class "button"
                        , onClick OnCancelMessage
                        ]
                        [ text T.cancel ]
                    , button
                        [ class "button is-success defaultSubmit"
                        , classList [ ( "is-loading", isLoading ) ]
                        , disabled (not isSendable)
                        , onClick (OnSubmit (isSendable && not isLoading) SubmitMessage)
                        ]
                        [ text T.update ]
                    ]
                ]
            ]
        ]


viewDraftSidePane : ProjectDraft -> Model -> Html Msg
viewDraftSidePane d model =
    let
        card =
            model.card

        uctx =
            uctxFromUser model.user

        isAuthor =
            d.createdBy.username == uctx.username

        isAdmin =
            model.isTensionAdmin
    in
    div [ class "tensionSidePane" ]
        ([ -- Extras
           hr [ class "is-transparent has-background-border-light my-5" ] []
         ]
            ++ (if isAdmin || isAuthor then
                    [ div
                        [ class "is-smaller2 has-text-weight-semibold button-light mb-4"
                        , onClick (DoConvertDraft card.id d)
                        ]
                        [ A.icon1 "icon-exchange" T.convertDraft ]
                    , div
                        [ class "is-smaller2 has-text-weight-semibold button-light mb-4"
                        , onClick (DoRemoveDraft card.id)
                        ]
                        [ A.icon1 "icon-trash" T.deleteDraft ]
                    ]

                else
                    []
               )
        )
