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


module Components.Comments exposing
    ( Msg(..)
    , OutType(..)
    , State
    , getCurrentMessage
    , init
    , initWithDraft
    , subscriptions
    , update
    , viewCommentInputHeader
    , viewCommentsContract
    , viewCommentsTension
    , viewContractCommentInput
    , viewNewTensionCommentInput
    , viewTensionCommentInput
    )

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Browser.Events as Events
import Bulk exposing (CommentPatchForm, Ev, InputViewMode(..), TensionForm, UserState(..), eventFromForm, initCommentPatchForm, initTensionForm, pushCommentReaction, removeCommentReaction, uctxFromUser)
import Bulk.Error exposing (viewGqlErrors)
import Bulk.Event exposing (viewEvent)
import Bulk.View exposing (statusColorReverse, viewTensionDateAndUserC, viewUpdated, viewUser0, viewUser2)
import Codecs exposing (CommentDraft, DraftUpdate(..))
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.EmojiPicker as EmojiPicker
import Components.UserInput as UserInput
import Dict
import Dom
import Extra exposing (showIf, ternary)
import Extra.Events exposing (onClickSafe)
import Form exposing (isPostSendable)
import Fractal.Enum.Lang as Lang
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, hr, li, p, span, strong, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, placeholder, rows, style, target, title, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import Json.Decode as JD
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), withMapData, withMaybeMapData)
import Markdown exposing (renderMarkdown, setMdCheckbox)
import Maybe exposing (withDefault)
import ModelSchema exposing (Comment, Event, IdPayload, PatchTensionPayloadID, Post, ReactionResponse, TensionHead, UserCtx)
import Ports
import Query.PatchContract exposing (pushContractComment)
import Query.PatchTension exposing (deleteComment, patchComment, pushTensionPatch)
import Query.Reaction exposing (addReaction, deleteReaction)
import Session exposing (Apis, GlobalCmd(..), SessionCommon, isMobile, toReflink)
import String.Format as Format
import Task
import Text as T
import Time



-- ------------------------------
-- M O D E L
-- ------------------------------


type State
    = State Model


type alias TensionCommon a =
    { a | status : TensionStatus.TensionStatus }


type alias EventTracker =
    { type_ : Maybe TensionEvent.TensionEvent
    , createdAt : String
    , i : Int
    , n : Int
    }


type alias Model =
    { focusid : String
    , comments : List Comment
    , history : List Event
    , expandedEvents : List Int
    , highlightedCommentId : String

    -- Push comment (Tension)
    , tension_form : TensionForm
    , contract_form : CommentPatchForm
    , tension_patch : GqlData PatchTensionPayloadID

    -- Edit Comment (Tension & Contract)
    , comment_form : CommentPatchForm
    , comment_result : GqlData Comment

    -- Delete Comment (cid, result)
    , comment_delete_result : ( String, GqlData IdPayload )

    -- Components
    , userInput : UserInput.State
    , emojiPicker : EmojiPicker.State
    , modal_confirm : ModalConfirm Msg

    -- Fade-out animation for deleted comments
    , fadingOut : List String

    -- Backup for checkbox operations (stores comment id and message being edited)
    , post_backup : Maybe { id : String, message : String }

    -- Common
    , session : SessionCommon
    , refresh_trial : Int -- use to refresh user token
    }


initModel : String -> String -> SessionCommon -> Model
initModel nameid tensionid session =
    { focusid = nameid
    , comments = []
    , history = []
    , expandedEvents = []
    , highlightedCommentId = ""
    , tension_form = initTensionForm session.lexicon tensionid Nothing session.user
    , tension_patch = NotAsked
    , contract_form = initCommentPatchForm session.user []
    , comment_form = initCommentPatchForm session.user [ ( "focusid", nameid ) ]
    , comment_result = NotAsked
    , comment_delete_result = ( "", NotAsked )

    -- Components
    , userInput = UserInput.init [ nameid ] False False session
    , emojiPicker = EmojiPicker.init
    , modal_confirm = ModalConfirm.init NoMsg

    -- Fade-out animation for deleted comments
    , fadingOut = []

    -- Backup for checkbox operations
    , post_backup = Nothing

    -- Common
    , session = session
    , refresh_trial = 0
    }


init : String -> String -> SessionCommon -> State
init nameid tensionid session =
    initModel nameid tensionid session |> State


initWithDraft : String -> String -> SessionCommon -> Maybe CommentDraft -> State
initWithDraft nameid tensionid session maybeDraft =
    let
        model =
            initModel nameid tensionid session

        tension_form =
            case maybeDraft of
                Just draft ->
                    let
                        f =
                            model.tension_form
                    in
                    { f | post = Dict.insert "message" draft.message f.post }

                Nothing ->
                    model.tension_form
    in
    State { model | tension_form = tension_form }



-- Global methods


getCurrentMessage : State -> Maybe String
getCurrentMessage (State model) =
    Dict.get "message" model.tension_form.post |> Maybe.map String.trim



-- State Controls


resetModel : Model -> Model
resetModel model =
    initModel model.tension_form.id model.focusid model.session


type Msg
    = ExpandEvent Int
    | SetTensionid String
    | SetContractid String
    | SetComments (List Comment)
    | SetHistory (List Event) (Maybe String)
    | PushEvents (List Event)
    | OnHighlight String
    | OnSetTarget (List String)
      -- Change Post
    | OnChangeComment String String
    | OnChangeContractComment String String
    | OnChangePatchComment String String
      -- Push Comment
    | SubmitTensionComment (Maybe TensionStatus.TensionStatus) Time.Posix
    | TensionCommentAck (GqlData PatchTensionPayloadID)
    | SubmitContractComment Time.Posix
    | ContractCommentAck (GqlData Comment)
      -- Edit comment
    | OnUpdateComment Comment
    | OnCancelComment String
    | SubmitCommentPatch Time.Posix
    | CommentPatchAck (GqlData Comment)
      -- Delete comment
    | OnDeleteComment String
    | SubmitDeleteComment String Time.Posix
    | DeleteCommentAck String (GqlData IdPayload)
      -- Clipboard
    | OnCopyLink String
    | OnClearCopyLink
      -- Reaction
    | OnAddReaction String Int
    | OnAddReactionAck (GqlData ReactionResponse)
    | OnDeleteReaction String Int
    | OnDeleteReactionAck (GqlData ReactionResponse)
      -- Markdown
    | OnCheckbox Checkbox
      -- Common
    | OnSubmit Bool (Time.Posix -> Msg)
    | NoMsg
    | LogErr String
    | ChangeInputViewMode InputViewMode
    | ChangeContractInputViewMode InputViewMode
    | ChangeUpdateViewMode InputViewMode
    | OnRichText String String
    | OnToggleMdHelp String
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Components
    | UserInputMsg UserInput.Msg
    | EmojiPickerMsg EmojiPicker.Msg


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe OutType
    }


type OutType
    = TensionCommentAdded (Maybe TensionStatus.TensionStatus)
    | PostChanged ( String, String )


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
        OnSubmit isSendable next ->
            if isSendable then
                ( model, out0 [ sendNow next ] )

            else
                ( model, noOut )

        ExpandEvent i ->
            -- @fix/bulma: dropdown clidk handler lost during the operation
            ( { model | expandedEvents = model.expandedEvents ++ [ i ] }, out0 [ Ports.bulma_driver "" ] )

        SetTensionid tensionid ->
            let
                form =
                    model.tension_form

                tension_form =
                    { form | id = tensionid }
            in
            ( { model | tension_form = tension_form }, noOut )

        SetContractid contractid ->
            let
                form =
                    model.contract_form

                contract_form =
                    { form | post = form.post |> Dict.insert "contractid" contractid }
            in
            ( { model | contract_form = contract_form }, noOut )

        SetComments comments ->
            ( { model | comments = comments }, out0 [ Ports.bulma_driver "" ] )

        SetHistory history eltid ->
            ( { model | history = history, highlightedCommentId = withDefault "" eltid }, noOut )

        PushEvents events ->
            -- @todo: update tension_head history here (need to create a SessionCommon.Cmd to handle this.
            ( { model | history = model.history ++ events }, noOut )

        OnHighlight id ->
            ( { model | highlightedCommentId = id }, noOut )

        OnSetTarget targets ->
            ( model
            , out0 [ Cmd.map UserInputMsg (send <| UserInput.ChangePath targets) ]
            )

        OnChangeComment field value ->
            let
                form =
                    model.tension_form

                tension_form =
                    if field == "message" && value == "" then
                        { form | post = Dict.remove field form.post }

                    else
                        { form | post = Dict.insert field value form.post }
            in
            ( { model | tension_form = tension_form }, Out [] [] (Just (PostChanged ( field, value ))) )

        OnChangeContractComment field value ->
            let
                form =
                    model.contract_form

                contract_form =
                    if field == "message" && value == "" then
                        { form | post = Dict.remove field form.post }

                    else
                        { form | post = Dict.insert field value form.post }
            in
            ( { model | contract_form = contract_form }, Out [] [] (Just (PostChanged ( field, value ))) )

        SubmitTensionComment status_m time ->
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

                tension_form =
                    { form
                        | post = Dict.insert "createdAt" (fromTime time) form.post
                        , status = status_m
                        , events = eventStatus
                    }
            in
            ( { model | tension_form = tension_form, tension_patch = LoadingSlowly }
            , out0 [ pushTensionPatch apis tension_form TensionCommentAck ]
            )

        TensionCommentAck result ->
            case parseErr result 2 of
                OkAuth tp ->
                    let
                        resetForm =
                            initTensionForm model.session.lexicon model.tension_form.id Nothing model.session.user
                    in
                    ( { model
                        | comments =
                            if (Dict.get "message" model.tension_form.post |> withDefault "") /= "" then
                                model.comments ++ withDefault [] tp.comments

                            else
                                model.comments
                        , history =
                            model.history
                                ++ (model.tension_form.events |> List.map (\e -> eventFromForm e model.tension_form))
                        , tension_form = resetForm
                        , tension_patch = result
                      }
                    , Out [ Ports.bulma_driver "" ] [ DoUpdateDraft (ClearComment model.tension_form.id) ] (Just (TensionCommentAdded model.tension_form.status))
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

        SubmitContractComment time ->
            let
                form =
                    model.contract_form

                contract_form =
                    { form | post = form.post |> Dict.insert "createdAt" (fromTime time) }
            in
            ( { model
                | contract_form = contract_form
                , comment_result = LoadingSlowly
              }
            , out0 [ pushContractComment apis contract_form ContractCommentAck ]
            )

        ContractCommentAck result ->
            case parseErr result 2 of
                OkAuth comment ->
                    let
                        resetForm =
                            initCommentPatchForm model.session.user []
                    in
                    ( { model
                        | comments =
                            if (Dict.get "message" model.contract_form.post |> withDefault "") /= "" then
                                model.comments ++ [ comment ]

                            else
                                model.comments
                        , contract_form = resetForm
                        , comment_result = result
                      }
                    , out0 [ Ports.bulma_driver "" ]
                    )

                _ ->
                    ( { model | comment_result = result }, noOut )

        OnUpdateComment c ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | id = c.id } }, out0 [ Ports.focusOn "updateCommentInput", Ports.bulma_driver c.createdAt ] )

        OnCancelComment createdAt ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | id = "", post = Dict.remove "message" form.post }, comment_result = NotAsked }, out0 [ Ports.bulma_driver createdAt ] )

        OnChangePatchComment field value ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | post = Dict.insert field value form.post } }, noOut )

        SubmitCommentPatch time ->
            let
                form =
                    model.comment_form

                comment_form =
                    { form | post = Dict.insert "updatedAt" (fromTime time) form.post }
            in
            ( { model | comment_form = comment_form, comment_result = LoadingSlowly }
            , out0 [ patchComment apis comment_form CommentPatchAck ]
            )

        CommentPatchAck result ->
            case parseErr result 2 of
                OkAuth comment ->
                    let
                        comments =
                            let
                                n =
                                    model.comments
                                        |> LE.findIndex (\c -> c.id == comment.id)
                                        |> withDefault -1
                            in
                            LE.setAt n comment model.comments

                        -- Restore backup if this was a stealth (checkbox) operation
                        resetForm =
                            case ( Dict.get "stealth" model.comment_form.post, model.post_backup ) of
                                ( Just "true", Just backup ) ->
                                    let
                                        f =
                                            initCommentPatchForm model.session.user [ ( "focusid", model.focusid ) ]
                                    in
                                    { f | id = backup.id, post = Dict.insert "message" backup.message f.post }

                                _ ->
                                    initCommentPatchForm model.session.user [ ( "focusid", model.focusid ) ]
                    in
                    ( { model | comments = comments, comment_form = resetForm, comment_result = result, post_backup = Nothing }
                    , out0 [ Ports.bulma_driver comment.createdAt ]
                    )

                _ ->
                    ( { model | comment_result = result, post_backup = Nothing }, noOut )

        -- Delete comment
        OnDeleteComment cid ->
            if List.head model.comments |> Maybe.map (\c -> c.id == cid) |> withDefault False then
                let
                    form =
                        model.comment_form
                in
                ( { model | comment_form = { form | id = cid, post = Dict.insert "message" "" form.post } }
                , out0 [ sendNow SubmitCommentPatch ]
                )

            else
                ( model, out0 [ sendNow (SubmitDeleteComment cid) ] )

        SubmitDeleteComment cid time ->
            let
                commentCreatedAt =
                    model.comments
                        |> List.filter (\c -> c.id == cid)
                        |> List.head
                        |> Maybe.map .createdAt
                        |> withDefault ""
            in
            ( { model | comment_delete_result = ( cid, LoadingSlowly ) }
            , out0 [ deleteComment apis model.tension_form.id cid commentCreatedAt (uctxFromUser model.session.user) time (DeleteCommentAck cid) ]
            )

        DeleteCommentAck cid result ->
            case parseErr result 2 of
                OkAuth _ ->
                    ( { model | fadingOut = cid :: model.fadingOut, comment_delete_result = ( cid, result ) }, noOut )

                Authenticate ->
                    ( model, out0 [ Ports.raiseAuthModal (uctxFromUser model.session.user) ] )

                _ ->
                    ( { model | comment_delete_result = ( cid, result ) }, noOut )

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

        ChangeContractInputViewMode viewMode ->
            let
                form =
                    model.contract_form
            in
            ( { model | contract_form = { form | viewMode = viewMode } }, noOut )

        ChangeUpdateViewMode viewMode ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | viewMode = viewMode } }, noOut )

        OnRichText targetid command ->
            ( model, out0 [ Ports.richText targetid command ] )

        OnToggleMdHelp targetid ->
            let
                toggleMdHelp form =
                    let
                        field =
                            "isMdHelpOpen" ++ targetid

                        v =
                            Dict.get field form.post |> withDefault "false"

                        value =
                            ternary (v == "true") "false" "true"
                    in
                    { form | post = Dict.insert field value form.post }
            in
            case targetid of
                "updateCommentInput" ->
                    ( { model | comment_form = toggleMdHelp model.comment_form }, noOut )

                "commentContractInput" ->
                    ( { model | contract_form = toggleMdHelp model.contract_form }, noOut )

                _ ->
                    -- Handles "commentInput", "textAreaModal", and any future tension_form targets
                    ( { model | tension_form = toggleMdHelp model.tension_form }, noOut )

        OnCopyLink cid ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | linkCopied = cid } }, out0 [ sendSleep OnClearCopyLink 2000 ] )

        OnClearCopyLink ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | linkCopied = "" } }, noOut )

        OnAddReaction cid type_ ->
            case model.session.user of
                LoggedIn uctx ->
                    ( model, out0 [ addReaction apis uctx.username cid type_ OnAddReactionAck ] )

                LoggedOut ->
                    ( model, out0 [ Ports.raiseAuthModal (uctxFromUser model.session.user) ] )

        OnAddReactionAck result ->
            let
                uctx =
                    uctxFromUser model.session.user
            in
            case parseErr result 2 of
                Authenticate ->
                    ( model, out0 [ Ports.raiseAuthModal uctx ] )

                OkAuth r ->
                    ( { model | comments = pushCommentReaction uctx.username r model.comments }, noOut )

                _ ->
                    ( model, noOut )

        OnDeleteReaction cid type_ ->
            case model.session.user of
                LoggedIn uctx ->
                    ( model, out0 [ deleteReaction apis uctx.username cid type_ OnDeleteReactionAck ] )

                LoggedOut ->
                    ( model, out0 [ Ports.raiseAuthModal (uctxFromUser model.session.user) ] )

        OnDeleteReactionAck result ->
            let
                uctx =
                    uctxFromUser model.session.user
            in
            case parseErr result 2 of
                Authenticate ->
                    ( model, out0 [ Ports.raiseAuthModal uctx ] )

                OkAuth r ->
                    ( { model | comments = removeCommentReaction uctx.username r model.comments }, noOut )

                _ ->
                    ( model, noOut )

        -- Markdown
        OnCheckbox checkbox ->
            case model.comments |> List.filter (\c -> c.id == checkbox.cid) |> List.head of
                Just c ->
                    let
                        -- Backup the current comment_form if user is editing a comment
                        backup =
                            if model.comment_form.id /= "" then
                                Dict.get "message" model.comment_form.post
                                    |> Maybe.map (\msg -> { id = model.comment_form.id, message = msg })

                            else
                                Nothing

                        -- Simulate comment updated
                        form =
                            model.comment_form

                        comment_form =
                            { form
                                | id = c.id
                                , post =
                                    form.post
                                        |> Dict.insert "message" (setMdCheckbox checkbox c.message)
                                        |> Dict.insert "stealth" "true"
                            }

                        -- send SubmitCommentPatch
                    in
                    ( { model | comment_form = comment_form, post_backup = backup }
                    , out0 [ send (OnSubmit True SubmitCommentPatch) ]
                    )

                Nothing ->
                    ( model, noOut )

        -- Confirm Modal
        DoModalConfirmOpen msg mess ->
            ( { model | modal_confirm = ModalConfirm.open msg mess model.modal_confirm }, noOut )

        DoModalConfirmClose _ ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, noOut )

        DoModalConfirmSend ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, out0 [ send model.modal_confirm.msg ] )

        -- Components
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

        EmojiPickerMsg msg ->
            let
                ( data, out ) =
                    EmojiPicker.update msg model.emojiPicker

                cmd =
                    case out.result of
                        Just emoji ->
                            Ports.pushEmojiSelection emoji

                        Nothing ->
                            Cmd.none
            in
            ( { model | emojiPicker = data }, out2 (cmd :: (out.cmds |> List.map (\m -> Cmd.map EmojiPickerMsg m))) out.gcmds )


type alias Checkbox =
    { isChecked : Bool
    , position : Int
    , cid : String
    }


checkboxDecoder : JD.Decoder Checkbox
checkboxDecoder =
    JD.map3 Checkbox
        (JD.field "isChecked" JD.bool)
        (JD.field "position" JD.int)
        (JD.field "cid" JD.string)


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    [ Ports.pd Ports.checkboxFromJs checkboxDecoder LogErr OnCheckbox ]
        ++ (if model.highlightedCommentId /= "" then
                [ Events.onMouseUp (JD.succeed (OnHighlight ""))
                , Events.onKeyUp (Dom.key "Escape" (OnHighlight ""))
                ]

            else
                []
           )
        ++ (UserInput.subscriptions model.userInput |> List.map (\s -> Sub.map UserInputMsg s))
        ++ (EmojiPicker.subscriptions model.emojiPicker |> List.map (\s -> Sub.map EmojiPickerMsg s))
        ++ [ Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose ]



-- ------------------------------
-- V I E W
-- ------------------------------


viewCommentsContract : SessionCommon -> State -> Html Msg
viewCommentsContract session (State model) =
    div []
        [ model.comments
            |> List.map
                (\c ->
                    viewComment session c model.comment_form model.comment_result model.comment_delete_result model.highlightedCommentId model.userInput model.emojiPicker (List.member c.id model.fadingOut)
                )
            |> div []
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        ]


viewCommentsTension : SessionCommon -> Maybe TensionAction.TensionAction -> State -> Html Msg
viewCommentsTension session action (State model) =
    div []
        [ viewComments_ session action model.history model.comments model.comment_form model.comment_result model.comment_delete_result model.expandedEvents model.highlightedCommentId model.userInput model.emojiPicker model.fadingOut
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        ]


viewComments_ :
    SessionCommon
    -> Maybe TensionAction.TensionAction
    -> List Event
    -> List Comment
    -> CommentPatchForm
    -> GqlData Comment
    -> ( String, GqlData IdPayload )
    -> List Int
    -> String
    -> UserInput.State
    -> EmojiPicker.State
    -> List String
    -> Html Msg
viewComments_ session action history comments comment_form comment_result comment_delete_result expandedEvents highlightedCommentId userInput emojiPicker fadingOut =
    let
        allEvts =
            -- When event and comment are created at the same time, show the comment first.
            List.indexedMap (\i c -> { type_ = Nothing, createdAt = c.createdAt, i = i, n = 0 }) comments
                ++ List.indexedMap
                    (\i e ->
                        { type_ = Just e.event_type
                        , createdAt =
                            if e.event_type == TensionEvent.CommentDeleted then
                                e.new |> withDefault e.createdAt

                            else
                                e.createdAt
                        , i = i
                        , n = 0
                        }
                    )
                    history
                |> List.sortBy .createdAt

        viewCommentOrEvent : EventTracker -> Html Msg
        viewCommentOrEvent e =
            case e.type_ of
                Just _ ->
                    case LE.getAt e.i history of
                        Just event ->
                            let
                                focusid =
                                    Dict.get "focusid" comment_form.post
                            in
                            Lazy.lazy4 viewEvent session focusid action event

                        Nothing ->
                            text ""

                Nothing ->
                    case LE.getAt e.i comments of
                        Just c ->
                            viewComment session c comment_form comment_result comment_delete_result highlightedCommentId userInput emojiPicker (List.member c.id fadingOut)

                        Nothing ->
                            text ""
    in
    allEvts
        -- Filter events if there a above a given number.
        -- If above, we keep track of the extra number of event
        -- until a non-event (i.e a comment) is met.
        |> LE.indexedFoldr
            (\i e d ->
                let
                    evts =
                        Tuple.first d

                    state =
                        Tuple.second d

                    isAbove =
                        (List.length evts > 6)
                            && (e.type_ /= Nothing)
                            && (evts
                                    |> List.take 6
                                    |> List.filter (\x -> x.type_ == Nothing)
                                    |> List.length
                               )
                            == 0

                    isClicked =
                        state.isClicked || List.member i expandedEvents
                in
                if e.type_ == Just TensionEvent.Created then
                    -- Ignore these type
                    ( evts, state )

                else if isAbove && state.nskip == 0 && not isClicked then
                    ( evts, { state | nskip = 1, i = i } )

                else if isAbove && state.nskip > 0 && not state.isClicked then
                    ( evts, { state | nskip = state.nskip + 1 } )

                else if state.nskip > 0 && e.type_ == Nothing && not state.isClicked then
                    let
                        btn =
                            { type_ = Nothing, n = state.nskip, createdAt = "", i = state.i }
                    in
                    ( [ e ] ++ [ btn ] ++ evts, { state | nskip = 0, isClicked = False } )

                else if e.type_ == Nothing then
                    ( [ e ] ++ evts, { state | nskip = 0, isClicked = False } )

                else
                    ( [ e ] ++ evts, { state | isClicked = isClicked } )
            )
            -- The tuple.first: filterered list of events
            -- The tuple.second: state of the fold loop. We stored the skips when a new comment is
            -- encoutered in order to insert a button later at the current position.
            ( [], { nskip = 0, isCollapsed = True, isClicked = False, i = 0 } )
        |> Tuple.first
        |> List.map
            (\x ->
                if x.n > 0 then
                    div
                        [ class "button is-small actionComment m-4"
                        , attribute "style" "left: 4%;"
                        , onClick (ExpandEvent x.i)
                        ]
                        [ text (T.showOlderEvents |> Format.value (String.fromInt x.n)) ]

                else
                    viewCommentOrEvent x
            )
        |> div []


viewComment : SessionCommon -> Comment -> CommentPatchForm -> GqlData Comment -> ( String, GqlData IdPayload ) -> String -> UserInput.State -> EmojiPicker.State -> Bool -> Html Msg
viewComment session c form result delete_result highlightedCommentId userInput emojiPicker isFadingOut =
    let
        isAuthor =
            c.createdBy.username == form.uctx.username

        reflink =
            toReflink session.url ++ "?goto=" ++ c.createdAt

        isFocused =
            c.createdAt == highlightedCommentId
    in
    div [ id c.createdAt, class "media section p-0", classList [ ( "comment-fadeout", isFadingOut ) ] ]
        [ div
            [ class "media-left is-hidden-mobile"
            , classList [ ( "is-hidden", isMobile session.screen ) ]
            ]
            [ viewUser2 c.createdBy.username ]
        , div
            [ class "media-content"
            , attribute "style" "width: 66.66667%;"
            ]
            [ if form.id == c.id && Dict.get "stealth" form.post /= Just "true" then
                viewUpdateInput session c form result userInput emojiPicker

              else
                div [ id c.id, class "message commentMessage", classList [ ( "is-focusing", isFocused ) ] ]
                    [ div [ class "message-header has-arrow-left pl-1-mobile", classList [ ( "is-author", isAuthor ) ] ]
                        [ span
                            [ --class "is-hidden-tablet"
                              classList [ ( "is-hidden", not (isMobile session.screen) ) ]
                            ]
                            [ viewUser0 c.createdBy.username ]
                        , viewTensionDateAndUserC session c.createdAt c.createdBy
                        , case c.updatedAt of
                            Just updatedAt ->
                                viewUpdated session updatedAt

                            Nothing ->
                                text ""
                        , div [ class "is-pulled-right" ]
                            [ div [ class "dropdown is-right mr-2" ]
                                [ div [ class "dropdown-trigger is-w" ]
                                    [ div
                                        [ class "ellipsis"
                                        , attribute "aria-controls" ("emoticon-" ++ c.id)
                                        , attribute "aria-haspopup" "true"
                                        ]
                                        [ A.icon "icon-smile icon-bg" ]
                                    ]
                                , div [ id ("emoticon-" ++ c.id), class "dropdown-menu emojis", attribute "role" "menu" ]
                                    [ Extra.emojis
                                        |> List.map (\( i, x, _ ) -> span [ onClick (OnAddReaction c.id i) ] [ text x ])
                                        |> div [ class "dropdown-content" ]
                                    ]
                                ]
                            , div [ class "dropdown is-right" ]
                                [ div [ class "dropdown-trigger is-w" ]
                                    [ div
                                        [ class "ellipsis"
                                        , attribute "aria-controls" ("edit-ellipsis-" ++ c.id)
                                        , attribute "aria-haspopup" "true"
                                        ]
                                        [ A.icon "icon-more-horizontal icon-lg" ]
                                    ]
                                , div [ id ("edit-ellipsis-" ++ c.id), class "dropdown-menu", attribute "role" "menu" ]
                                    [ div [ class "dropdown-content p-0" ] <|
                                        [ div [ class "dropdown-item", attribute "data-clipboard" reflink, onClick (OnCopyLink c.id) ] [ A.icon1 "icon-link" "Copy link" ] ]
                                            ++ (if isAuthor then
                                                    [ hr [ class "dropdown-divider" ] []
                                                    , div [ class "dropdown-item", onClick (OnUpdateComment c) ] [ A.icon1 "icon-edit-2" T.edit ]
                                                    , div
                                                        [ class "dropdown-item"
                                                        , onClick <|
                                                            DoModalConfirmOpen (OnDeleteComment c.id)
                                                                { message = Nothing
                                                                , txts = [ ( T.confirmDeleteComment, "" ) ]
                                                                , confirmClass = "is-danger"
                                                                , confirmLabel = T.delete
                                                                }
                                                        ]
                                                        [ A.icon1 "icon-trash" T.delete ]
                                                    ]

                                                else
                                                    []
                                               )
                                    ]
                                , if form.linkCopied == c.id then
                                    span [ class "copy-notif is-size-7 has-text-success" ] [ text "Copied!" ]

                                  else
                                    text ""
                                ]
                            ]
                        ]
                    , div [ class "message-body" ]
                        [ case c.message of
                            "" ->
                                div [ class "help is-italic" ] [ text T.noMessageProvided ]

                            message ->
                                renderMarkdown "is-human" message
                        , div [ class "emoji-reactions" ] <|
                            List.map
                                (\r ->
                                    case List.length r.users of
                                        0 ->
                                            text ""

                                        count ->
                                            let
                                                isSelected =
                                                    List.member form.uctx.username r.users

                                                elmId =
                                                    "emoji-" ++ c.id ++ String.fromInt r.type_
                                            in
                                            span
                                                [ class "tag mr-2 dropdown is-up"
                                                , classList [ ( "is-selected", isSelected ) ]
                                                ]
                                                [ div
                                                    [ class "dropdown-trigger"
                                                    , attribute "aria-controls" elmId
                                                    , attribute "aria-haspopup" "true"
                                                    , if isSelected then
                                                        onClick (OnDeleteReaction c.id r.type_)

                                                      else
                                                        onClick (OnAddReaction c.id r.type_)
                                                    ]
                                                    [ text (Extra.getEmoji r.type_), span [ class "px-1" ] [], text (String.fromInt count) ]
                                                , div [ id elmId, class "dropdown-menu", attribute "role" "menu" ]
                                                    [ div [ class "dropdown-content p-3" ]
                                                        [ span [ class "is-larger4 pr-2" ] [ text (Extra.getEmoji r.type_) ]
                                                        , case LE.unconsLast r.users of
                                                            Just ( u, [] ) ->
                                                                text (u ++ " " ++ T.reactedWith ++ " " ++ Extra.getEmojiName r.type_ ++ " emoji")

                                                            Just ( u, us ) ->
                                                                text (String.join ", " us ++ " " ++ T.and ++ " " ++ u ++ " " ++ T.reactedWith ++ " " ++ Extra.getEmojiName r.type_ ++ " emoji")

                                                            Nothing ->
                                                                text ""
                                                        ]
                                                    ]
                                                ]
                                )
                                c.reactions
                        ]
                    ]
            , showIf (c.id == Tuple.first delete_result) <|
                case Tuple.second delete_result of
                    Failure err ->
                        viewGqlErrors err

                    _ ->
                        text ""
            ]
        ]


viewDeleteCommentError : String -> ( String, GqlData IdPayload ) -> Html Msg
viewDeleteCommentError cid ( targetCid, result ) =
    if cid == targetCid then
        case result of
            Failure err ->
                viewGqlErrors err

            _ ->
                text ""

    else
        text ""


viewNewTensionCommentInput : SessionCommon -> CommentOpts -> State -> Html Msg
viewNewTensionCommentInput session opts (State model) =
    let
        opHeader =
            { onChangeViewMode = ChangeInputViewMode
            , onRichText = OnRichText
            , onToggleMdHelp = OnToggleMdHelp
            }
    in
    div [ class "message commentMessage" ]
        [ div [ class "message-header" ] [ viewCommentInputHeader opHeader "textAreaModal" model.tension_form ]
        , div [ class "message-body" ]
            [ div [ class "field" ]
                [ div [ class "control" ] [ viewCommentTextarea session "textAreaModal" opts model.tension_form model.userInput model.emojiPicker ]
                , showIf (opts.messageHelper /= "") <|
                    p [ class "help-label" ] [ text opts.messageHelper ]
                , showIf opts.hasTips <|
                    div
                        [ class "is-hidden-mobile is-pulled-right help"
                        , classList [ ( "is-hidden", isMobile session.screen ) ]
                        , style "font-size" "10px"
                        ]
                        [ text "Tips: <C+Enter> to submit" ]
                , br [ class "is-hidden-mobile", classList [ ( "is-hidden", isMobile session.screen ) ] ]
                    []
                ]
            ]
        ]


viewUpdateInput : SessionCommon -> Comment -> CommentPatchForm -> GqlData Comment -> UserInput.State -> EmojiPicker.State -> Html Msg
viewUpdateInput session comment form_ result userInput emojiPicker =
    let
        message =
            Dict.get "message" form_.post |> withDefault comment.message

        form =
            { form_ | post = Dict.insert "message" message form_.post }

        isSendable =
            message /= comment.message

        isLoading =
            Loading.isLoading result

        opHeader =
            { onChangeViewMode = ChangeUpdateViewMode
            , onRichText = OnRichText
            , onToggleMdHelp = OnToggleMdHelp
            }

        commentOpts =
            {}
    in
    div [ class "message commentMessage commentInput" ]
        [ div [ class "message-header has-arrow-left" ] [ viewCommentInputHeader opHeader "updateCommentInput" form ]
        , div [ class "message-body submitFocus" ]
            [ div [ class "field" ]
                [ div [ class "control" ]
                    [ viewCommentTextarea session "updateCommentInput" defaultCommentOpts form userInput emojiPicker ]
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
                            , onClick (OnCancelComment comment.createdAt)
                            ]
                            [ text T.cancel ]
                        , button
                            [ class "button is-success defaultSubmit"
                            , classList [ ( "is-loading", isLoading ) ]
                            , disabled (not isSendable)
                            , onClick (OnSubmit (isSendable && not isLoading) SubmitCommentPatch)
                            ]
                            [ text T.update ]
                        ]
                    ]
                ]
            ]
        ]


viewTensionCommentInput : SessionCommon -> TensionCommon a -> State -> Html Msg
viewTensionCommentInput session tension (State model) =
    let
        form =
            model.tension_form

        message =
            Dict.get "message" form.post |> withDefault ""

        isLoading =
            Loading.isLoading model.tension_patch

        isSendable =
            isPostSendable [ "message" ] form.post || (form.events |> List.filter (\x -> x.event_type == TensionEvent.Reopened || x.event_type == TensionEvent.Closed) |> List.length) > 0

        submit =
            onClick (OnSubmit (isSendable && not isLoading) <| SubmitTensionComment Nothing)

        ( submitCloseOpen, closeOpenTxt ) =
            case tension.status of
                TensionStatus.Open ->
                    ( onClick (OnSubmit (not isLoading) <| SubmitTensionComment (Just TensionStatus.Closed))
                    , ternary (message == "") T.close T.closeComment
                    )

                TensionStatus.Closed ->
                    ( onClick (OnSubmit (not isLoading) <| SubmitTensionComment (Just TensionStatus.Open))
                    , ternary (message == "") T.reopen T.reopenComment
                    )

        opHeader =
            { onChangeViewMode = ChangeInputViewMode
            , onRichText = OnRichText
            , onToggleMdHelp = OnToggleMdHelp
            }
    in
    div [ id "tensionCommentInput", class "media section p-0" ]
        [ div [ class "media-left is-hidden-mobile", classList [ ( "is-hidden", isMobile session.screen ) ] ]
            [ viewUser2 form.uctx.username ]
        , div [ class "media-content" ]
            [ div [ class "message commentMessage commentInput" ]
                [ div [ class "message-header has-arrow-left" ] [ viewCommentInputHeader opHeader "commentInput" form ]
                , div [ class "message-body submitFocus" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ viewCommentTextarea session "commentInput" defaultCommentOpts form model.userInput model.emojiPicker ]
                        ]
                    , case model.tension_patch of
                        Failure err ->
                            if isSendable then
                                viewGqlErrors err

                            else
                                text ""

                        _ ->
                            text ""
                    , div [ class "field is-grouped is-grouped-right" ]
                        [ div [ class "control", style "max-width" "100%" ]
                            [ div [ class "buttons" ]
                                [ button
                                    [ class "button"
                                    , classList [ ( "is-loading", isLoading && form.status /= Nothing ) ]
                                    , submitCloseOpen
                                    ]
                                    [ A.icon1 ("icon-alert-circle has-text-" ++ statusColorReverse tension.status) closeOpenTxt ]
                                , button
                                    [ class "button is-success defaultSubmit"
                                    , classList [ ( "is-loading", isLoading && form.status == Nothing ) ]
                                    , disabled (not isSendable)
                                    , submit
                                    ]
                                    [ text T.comment ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewContractCommentInput : SessionCommon -> State -> Html Msg
viewContractCommentInput session (State model) =
    let
        form =
            model.contract_form

        isLoading =
            Loading.isLoading model.comment_result

        isSendable =
            isPostSendable [ "message" ] form.post

        opHeader =
            { onChangeViewMode = ChangeContractInputViewMode
            , onRichText = OnRichText
            , onToggleMdHelp = OnToggleMdHelp
            }
    in
    div [ id "tensionCommentInput", class "media section p-0" ]
        [ div [ class "media-left is-hidden-mobile" ] [ viewUser2 form.uctx.username ]
        , div [ class "media-content" ]
            [ div [ class "message commentMessage commentInput" ]
                [ div [ class "message-header has-arrow-left" ] [ viewCommentInputHeader opHeader "commentContractInput" form ]
                , div [ class "message-body submitFocus" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ viewCommentTextarea session "commentContractInput" defaultCommentOpts form model.userInput model.emojiPicker ]
                        ]
                    , case model.comment_result of
                        Failure err ->
                            if isSendable then
                                viewGqlErrors err

                            else
                                text ""

                        _ ->
                            text ""
                    , div [ class "field is-grouped is-grouped-right" ]
                        [ div [ class "control" ]
                            [ div [ class "buttons" ]
                                [ button
                                    [ class "button defaultSubmit"
                                    , classList [ ( "is-loading", isLoading ) ]
                                    , disabled (not isSendable)
                                    , onClick (OnSubmit (isSendable && not isLoading) SubmitContractComment)
                                    ]
                                    [ text T.comment ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]



--
-- Shared View
--


type alias FormCommon a =
    { a
        | viewMode : InputViewMode
        , post : Post
    }


type alias OpCommentHeader msg =
    { onChangeViewMode : InputViewMode -> msg
    , onRichText : String -> String -> msg
    , onToggleMdHelp : String -> msg
    }


type alias CommentOpts =
    { isModal : Bool
    , hasTips : Bool
    , placeholderText : String
    , messageHelper : String
    }


defaultCommentOpts : CommentOpts
defaultCommentOpts =
    { isModal = False
    , placeholderText = T.leaveComment
    , messageHelper = ""
    , hasTips = False
    }


viewCommentInputHeader : OpCommentHeader msg -> String -> FormCommon a -> Html msg
viewCommentInputHeader op targetid form =
    let
        isMdHelpOpen =
            Dict.get ("isMdHelpOpen" ++ targetid) form.post == Just "true"
    in
    div [ class "level commentHeader" ]
        [ div [ class "level-left" ]
            [ div [ class "tabs is-boxed is-small" ]
                [ ul []
                    [ li [ classList [ ( "is-active", form.viewMode == Write ) ] ] [ a [ onClickSafe (op.onChangeViewMode Write), target "_blank" ] [ text T.write ] ]
                    , li [ classList [ ( "is-active", form.viewMode == Preview ) ] ] [ a [ onClickSafe (op.onChangeViewMode Preview), target "_blank" ] [ text T.preview ] ]
                    ]
                ]
            ]
        , div [ class "level-right is-hidden-mobile" ]
            [ div [ onClick (op.onRichText targetid "Heading"), title "Heading text" ] [ text "H" ]
            , div [ onClick (op.onRichText targetid "Bold"), class "", title "Bold text" ] [ strong [] [ text "B" ] ]
            , div [ onClick (op.onRichText targetid "Italic"), class "", title "Italic text" ] [ span [ class "is-italic" ] [ text "I" ] ]
            , div [ onClick (op.onRichText targetid "Strikethrough"), class "", title "Strikethrough" ] [ span [] [ text ("̶" ++ "S" ++ "̶") ] ]
            , div [ onClick (op.onRichText targetid "Quote"), class "mr-3", title "Quote" ] [ span [] [ A.icon "icon-quote-right icon-xs" ] ]
            , div [ onClick (op.onRichText targetid "Link"), class "", title "Link" ] [ span [] [ A.icon "icon-link icon-sm" ] ]
            , div [ onClick (op.onRichText targetid "List-ul"), class "", title "List" ] [ span [] [ A.icon "icon-list-ul icon-sm" ] ]
            , div [ onClick (op.onRichText targetid "List-ol"), class "", title "Ordered list" ] [ span [] [ A.icon "icon-list-ol icon-sm" ] ]
            , div [ onClick (op.onRichText targetid "List-check"), class "", title "Check list" ] [ span [] [ A.icon "icon-check-square icon-sm" ] ]
            , div [ onClick (op.onRichText targetid "Details"), class "mr-3", title "Collapsible section" ] [ A.icon "icon-chevron-right icon-sm" ]
            , div [ onClick (op.onRichText targetid "MentionUser"), class "", title "Mention an user" ] [ span [] [ A.icon "icon-at-sign icon-sm" ] ]
            , div [ onClick (op.onRichText targetid "MentionTension"), class "mr-3", title "Reference a tension" ] [ A.icon "icon-exchange icon-sm" ]
            , div
                [ onClick (op.onToggleMdHelp targetid)
                , class "is-right is-w"
                , classList [ ( "has-text-strong", isMdHelpOpen ) ]
                , title T.markdownSupport
                ]
                [ A.icon "icon-markdown" ]
            ]
        , if isMdHelpOpen then
            div [ id "mdLegend", class "box" ]
                [ button [ class "delete is-pulled-right", onClick (op.onToggleMdHelp targetid) ] []
                , renderMarkdown "" T.markdownHelp
                ]

          else
            text ""
        ]


viewCommentTextarea : SessionCommon -> String -> CommentOpts -> FormCommon a -> UserInput.State -> EmojiPicker.State -> Html Msg
viewCommentTextarea session targetid opts form userInput emojiPicker =
    let
        message =
            Dict.get "message" form.post |> withDefault ""

        line_len =
            List.length <| String.lines message

        -- Calculate max rows based on ~75% of screen height
        -- Assuming ~30px per line (font + padding)
        --session.screen.h*3//4 // 40
        ( max_len, min_len ) =
            if isMobile session.screen then
                if opts.isModal then
                    ( session.screen.h // 2 // 38, 2 )

                else
                    ( session.screen.h * 2 // 3 // 38, 4 )

            else if opts.isModal then
                ( session.screen.h * 2 // 3 // 38, 4 )

            else if targetid == "commentContractInput" then
                ( session.screen.h * 5 // 6 // 39, 4 )

            else
                ( session.screen.h * 5 // 6 // 39, 6 )

        onChangePost =
            if String.startsWith "update" targetid then
                OnChangePatchComment

            else if targetid == "commentContractInput" then
                OnChangeContractComment

            else
                OnChangeComment
    in
    div []
        [ textarea
            [ id targetid
            , class "textarea"
            , classList [ ( "is-invisible-force", form.viewMode == Preview ) ]
            , rows (min max_len (max line_len min_len))
            , placeholder opts.placeholderText
            , value message
            , onInput (onChangePost "message")

            --, contenteditable True
            ]
            []
        , if form.viewMode == Preview then
            div [ class "mt-2 mx-3" ]
                [ renderMarkdown "is-human hidden-textarea" message, hr [] [] ]

          else
            text ""
        , span [ id (targetid ++ "searchInput"), class "searchInput", attribute "aria-hidden" "true", attribute "style" "display:none;" ]
            [ UserInput.viewUserSeeker userInput |> Html.map UserInputMsg ]
        , span [ id (targetid ++ "emojiInput"), class "searchInput", attribute "aria-hidden" "true", attribute "style" "display:none;" ]
            [ EmojiPicker.viewEmojiSeeker emojiPicker |> Html.map EmojiPickerMsg ]
        ]
