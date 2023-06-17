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


module Components.Comments exposing (..)

import Assets as A
import Bulk exposing (CommentPatchForm, InputViewMode(..), TensionForm)
import Bulk.Codecs exposing (DocType(..), FractalBaseRoute(..), getTensionCharac, nid2rootid, tensionAction2NodeType, toLink, uriFromNameid)
import Bulk.Error exposing (viewGqlErrors)
import Bulk.View exposing (action2str, statusColor, tensionIcon2, tensionStatus2str, viewLabel, viewNodeRefShort, viewTensionDateAndUserC, viewUpdated, viewUser0, viewUser2, viewUsernameLink)
import Components.UserInput as UserInput
import Dict
import Extra exposing (decap, ternary, textD)
import Extra.Date exposing (formatDate)
import Extra.Events exposing (onClickSafe)
import Form exposing (isPostSendable)
import Fractal.Enum.Lang as Lang
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (toHref)
import Html exposing (Html, a, br, button, div, hr, i, li, p, span, strong, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, style, target, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy as Lazy
import List.Extra as LE
import Loading exposing (GqlData, RequestResult(..))
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelSchema exposing (Comment, Event, Label, PatchTensionPayloadID, TensionHead, UserCtx)
import Session exposing (Conf, isMobile)
import String.Extra as SE
import String.Format as Format
import Text as T
import Time


type alias TensionCommon a =
    { a | status : TensionStatus.TensionStatus }


type alias Op msg =
    { doChangeViewMode : InputViewMode -> msg
    , doExpandEvent : Int -> msg
    , doChangePost : String -> String -> msg
    , doRichText : String -> String -> msg
    , doToggleMdHelp : String -> msg

    -- Submit
    , doSubmit : Bool -> (Time.Posix -> msg) -> msg
    , doEditComment : Time.Posix -> msg
    , doSubmitComment : Maybe TensionStatus.TensionStatus -> Time.Posix -> msg

    -- Edit
    , doUpdate : Comment -> msg
    , doCancelComment : msg
    , doAddReaction : String -> Int -> msg
    , doDeleteReaction : String -> Int -> msg

    -- Components
    , userSearchInput : Maybe UserInput.State
    , userSearchInputMsg : Maybe (UserInput.Msg -> msg)
    , conf : Conf
    }


type alias OpNewComment msg =
    { doChangeViewMode : InputViewMode -> msg
    , doChangePost : String -> String -> msg
    , doRichText : String -> String -> msg
    , doToggleMdHelp : String -> msg
    , userSearchInput : Maybe UserInput.State
    , userSearchInputMsg : Maybe (UserInput.Msg -> msg)
    , conf : Conf
    }


type alias OpEditComment msg =
    { doUpdate : Comment -> msg
    , doCancelComment : msg
    , doChangeViewMode : InputViewMode -> msg
    , doChangePost : String -> String -> msg
    , doSubmit : Bool -> (Time.Posix -> msg) -> msg
    , doEditComment : Time.Posix -> msg
    , doRichText : String -> String -> msg
    , doToggleMdHelp : String -> msg
    , doAddReaction : String -> Int -> msg
    , doDeleteReaction : String -> Int -> msg
    , userSearchInput : Maybe UserInput.State
    , userSearchInputMsg : Maybe (UserInput.Msg -> msg)
    , conf : Conf
    }


type alias OpNewCommentTension msg =
    { doChangeViewMode : InputViewMode -> msg
    , doChangePost : String -> String -> msg
    , doSubmit : Bool -> (Time.Posix -> msg) -> msg
    , doSubmitComment : Maybe TensionStatus.TensionStatus -> Time.Posix -> msg
    , doRichText : String -> String -> msg
    , doToggleMdHelp : String -> msg
    , userSearchInput : Maybe UserInput.State
    , userSearchInputMsg : Maybe (UserInput.Msg -> msg)
    , conf : Conf
    }


type alias OpNewCommentContract msg =
    { doChangeViewMode : InputViewMode -> msg
    , doChangePost : String -> String -> msg
    , doSubmit : Bool -> (Time.Posix -> msg) -> msg
    , doSubmitComment : Time.Posix -> msg
    , doRichText : String -> String -> msg
    , doToggleMdHelp : String -> msg
    , userSearchInput : Maybe UserInput.State
    , userSearchInputMsg : Maybe (UserInput.Msg -> msg)
    , conf : Conf
    }


viewComments :
    Op msg
    -> Maybe TensionAction.TensionAction
    -> List Event
    -> List Comment
    -> CommentPatchForm
    -> GqlData Comment
    -> List Int
    -> Html msg
viewComments op action history comments comment_form comment_result expandedEvents =
    let
        allEvts =
            -- When event and comment are created at the same time, show the comment first.
            List.indexedMap (\i c -> { type_ = Nothing, createdAt = c.createdAt, i = i, n = 0 }) comments
                ++ List.indexedMap (\i e -> { type_ = Just e.event_type, createdAt = e.createdAt, i = i, n = 0 }) history
                |> List.sortBy .createdAt

        viewCommentOrEvent : { type_ : Maybe TensionEvent.TensionEvent, createdAt : String, i : Int, n : Int } -> Html msg
        viewCommentOrEvent e =
            case e.type_ of
                Just _ ->
                    case LE.getAt e.i history of
                        Just event ->
                            viewEvent op.conf (Dict.get "focusid" comment_form.post) action event

                        Nothing ->
                            text ""

                Nothing ->
                    case LE.getAt e.i comments of
                        Just c ->
                            viewComment op c comment_form comment_result

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
                        , attribute "style" "left:10%;"
                        , onClick (op.doExpandEvent x.i)
                        ]
                        [ text (T.showOlderEvents |> Format.value (String.fromInt x.n)) ]

                else
                    Lazy.lazy viewCommentOrEvent x
            )
        |> div []


viewCommentInput : Op msg -> TensionForm -> Html msg
viewCommentInput op form =
    div [ class "message" ]
        [ div [ class "message-header" ] [ viewCommentInputHeader "textAreaModal" "" op form ]
        , div [ class "message-body" ]
            [ div [ class "field" ]
                [ div [ class "control" ] [ viewCommentTextarea "textAreaModal" True T.leaveCommentOpt op form ]
                , p [ class "help-label" ] [ text form.txt.message_help ]
                , div [ class "is-hidden-mobile is-pulled-right help", style "font-size" "10px" ] [ text "Tips: <C+Enter> to submit" ]
                , br [ class "is-hidden-mobile" ] []
                ]
            ]
        ]


viewComment : Op msg -> Comment -> CommentPatchForm -> GqlData Comment -> Html msg
viewComment op c form result =
    let
        isAuthor =
            c.createdBy.username == form.uctx.username

        reflink =
            case Dict.get "reflink" form.post of
                Just ref ->
                    ref ++ "?goto=" ++ c.createdAt

                Nothing ->
                    "#"
    in
    div [ id c.createdAt, class "media section is-paddingless" ]
        [ div [ class "media-left is-hidden-mobile" ] [ viewUser2 c.createdBy.username ]
        , div
            [ class "media-content"
            , attribute "style" "width: 66.66667%;"
            ]
            [ if form.id == c.id then
                viewUpdateInput op c form result

              else
                div [ class "message" ]
                    [ div [ class "message-header has-arrow-left pl-1-mobile", classList [ ( "is-author", isAuthor ) ] ]
                        [ span [ class "is-hidden-tablet" ] [ viewUser0 c.createdBy.username ]
                        , viewTensionDateAndUserC op.conf c.createdAt c.createdBy
                        , case c.updatedAt of
                            Just updatedAt ->
                                viewUpdated op.conf updatedAt

                            Nothing ->
                                text ""
                        , div [ class "is-pulled-right" ]
                            [ div [ class "dropdown is-right mr-2" ]
                                [ div [ class "dropdown-trigger is-w is-h" ]
                                    [ div
                                        [ class "ellipsis"
                                        , attribute "aria-controls" ("emoticon-" ++ c.id)
                                        , attribute "aria-haspopup" "true"
                                        ]
                                        [ A.icon "icon-smile icon-bg" ]
                                    ]
                                , div [ id ("emoticon-" ++ c.id), class "dropdown-menu emojis", attribute "role" "menu" ]
                                    [ Extra.emojis
                                        |> List.map (\( i, x, _ ) -> span [ onClick (op.doAddReaction c.id i) ] [ text x ])
                                        |> div [ class "dropdown-content" ]
                                    ]
                                ]
                            , div [ class "dropdown is-right" ]
                                [ div [ class "dropdown-trigger is-w is-h" ]
                                    [ div
                                        [ class "ellipsis"
                                        , attribute "aria-controls" ("edit-ellipsis-" ++ c.id)
                                        , attribute "aria-haspopup" "true"
                                        ]
                                        [ A.icon "icon-more-horizontal icon-lg" ]
                                    ]
                                , div [ id ("edit-ellipsis-" ++ c.id), class "dropdown-menu", attribute "role" "menu" ]
                                    [ div [ class "dropdown-content p-0" ] <|
                                        [ div [ class "dropdown-item button-light", attribute "data-clipboard" reflink ] [ text "Copy link" ] ]
                                            ++ (if isAuthor then
                                                    [ hr [ class "dropdown-divider" ] []
                                                    , div [ class "dropdown-item button-light", onClick (op.doUpdate c) ] [ text T.edit ]
                                                    ]

                                                else
                                                    []
                                               )
                                    ]
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
                                                        onClick (op.doDeleteReaction c.id r.type_)

                                                      else
                                                        onClick (op.doAddReaction c.id r.type_)
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
            ]
        ]


viewUpdateInput : Op msg -> Comment -> CommentPatchForm -> GqlData Comment -> Html msg
viewUpdateInput op comment form_ result =
    let
        message =
            Dict.get "message" form_.post |> withDefault comment.message

        form =
            { form_ | post = Dict.insert "message" message form_.post }

        isSendable =
            message /= comment.message

        isLoading =
            Loading.isLoading result
    in
    div [ class "message commentInput" ]
        [ div [ class "message-header has-arrow-left" ] [ viewCommentInputHeader "updateCommentInput" "" op form ]
        , div [ class "message-body submitFocus" ]
            [ div [ class "field" ]
                [ div [ class "control" ]
                    [ viewCommentTextarea "updateCommentInput" False T.leaveComment op form ]
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
                            , onClick op.doCancelComment
                            ]
                            [ text T.cancel ]
                        , button
                            [ class "button is-success defaultSubmit"
                            , classList [ ( "is-loading", isLoading ) ]
                            , disabled (not isSendable)
                            , onClick (op.doSubmit isLoading op.doEditComment)
                            ]
                            [ text T.update ]
                        ]
                    ]
                ]
            ]
        ]


viewTensionCommentInput : Op msg -> TensionCommon a -> TensionForm -> GqlData PatchTensionPayloadID -> Html msg
viewTensionCommentInput op tension form result =
    let
        message =
            Dict.get "message" form.post |> withDefault ""

        isLoading =
            Loading.isLoading result

        isSendable =
            isPostSendable [ "message" ] form.post || (form.events |> List.filter (\x -> x.event_type == TensionEvent.Reopened || x.event_type == TensionEvent.Closed) |> List.length) > 0

        doSubmit =
            ternary isSendable [ onClick (op.doSubmit isLoading <| op.doSubmitComment Nothing) ] []

        submitCloseOpenTension =
            case tension.status of
                TensionStatus.Open ->
                    [ onClick (op.doSubmit isLoading <| op.doSubmitComment (Just TensionStatus.Closed)) ]

                TensionStatus.Closed ->
                    [ onClick (op.doSubmit isLoading <| op.doSubmitComment (Just TensionStatus.Open)) ]

        closeOpenText =
            case tension.status of
                TensionStatus.Open ->
                    ternary (message == "") T.close T.closeComment

                TensionStatus.Closed ->
                    ternary (message == "") T.reopen T.reopenComment
    in
    div [ id "tensionCommentInput", class "media section is-paddingless commentInput" ]
        [ div [ class "media-left is-hidden-mobile" ] [ viewUser2 form.uctx.username ]
        , div [ class "media-content" ]
            [ div [ class "message" ]
                [ div [ class "message-header has-arrow-left" ] [ viewCommentInputHeader "commentInput" "" op form ]
                , div [ class "message-body submitFocus" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ viewCommentTextarea "commentInput" False T.leaveComment op form ]
                        ]
                    , case result of
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
                                    ([ class "button"
                                     , classList [ ( "is-danger", tension.status == TensionStatus.Open ), ( "is-loading", isLoading && form.status /= Nothing ) ]
                                     ]
                                        ++ submitCloseOpenTension
                                    )
                                    [ text closeOpenText ]
                                , button
                                    ([ class "button is-success defaultSubmit"
                                     , classList [ ( "is-loading", isLoading && form.status == Nothing ) ]
                                     , disabled (not isSendable)
                                     ]
                                        ++ doSubmit
                                    )
                                    [ text T.comment ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewContractCommentInput : Op msg -> CommentPatchForm -> GqlData Comment -> Html msg
viewContractCommentInput op form result =
    let
        isLoading =
            Loading.isLoading result

        isSendable =
            isPostSendable [ "message" ] form.post

        doSubmit =
            ternary isSendable [ onClick (op.doSubmit isLoading (op.doSubmitComment Nothing)) ] []
    in
    div [ id "tensionCommentInput", class "media section is-paddingless commentInput" ]
        [ div [ class "media-left is-hidden-mobile" ] [ viewUser2 form.uctx.username ]
        , div [ class "media-content" ]
            [ div [ class "message" ]
                [ div [ class "message-header has-arrow-left" ] [ viewCommentInputHeader "commentContractInput" "" op form ]
                , div [ class "message-body submitFocus" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ viewCommentTextarea "commentContractInput" False T.leaveComment op form ]
                        ]
                    , case result of
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
                                    ([ class "button defaultSubmit"
                                     , classList [ ( "is-loading", isLoading ) ]
                                     , disabled (not isSendable)
                                     ]
                                        ++ doSubmit
                                    )
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


viewCommentInputHeader targetid cls_tabs op form =
    let
        isMdHelpOpen =
            Dict.get ("isMdHelpOpen" ++ targetid) form.post == Just "true"
    in
    div [ class "level commentHeader" ]
        [ div [ class "level-left" ]
            [ div [ class ("tabs is-boxed is-small " ++ cls_tabs) ]
                [ ul []
                    [ li [ classList [ ( "is-active", form.viewMode == Write ) ] ] [ a [ onClickSafe (op.doChangeViewMode Write), target "_blank" ] [ text T.write ] ]
                    , li [ classList [ ( "is-active", form.viewMode == Preview ) ] ] [ a [ onClickSafe (op.doChangeViewMode Preview), target "_blank" ] [ text T.preview ] ]
                    ]
                ]
            ]
        , div [ class "level-right is-hidden-mobile" ]
            [ div [ onClick (op.doRichText targetid "Heading"), class "tooltip has-tooltip-bottom", attribute "data-tooltip" "Heading text" ] [ text "H" ]
            , div [ onClick (op.doRichText targetid "Bold"), class "tooltip has-tooltip-bottom", attribute "data-tooltip" "Bold text" ] [ strong [] [ text "B" ] ]
            , div [ onClick (op.doRichText targetid "Italic"), class "tooltip has-tooltip-bottom", attribute "data-tooltip" "Italic text" ] [ span [ class "is-italic" ] [ text "I" ] ]
            , div [ onClick (op.doRichText targetid "Strikethrough"), class "tooltip has-tooltip-bottom", attribute "data-tooltip" "Strikethrough" ] [ span [] [ text ("̶" ++ "S" ++ "̶") ] ]
            , div [ onClick (op.doRichText targetid "Quote"), class "tooltip has-tooltip-bottom mr-3", attribute "data-tooltip" "Quote" ] [ span [] [ A.icon "icon-quote-right icon-xs" ] ]
            , div [ onClick (op.doRichText targetid "Link"), class "tooltip has-tooltip-bottom", attribute "data-tooltip" "Link" ] [ span [] [ A.icon "icon-link icon-sm" ] ]
            , div [ onClick (op.doRichText targetid "List-ul"), class "tooltip has-tooltip-bottom", attribute "data-tooltip" "List" ] [ span [] [ A.icon "icon-list-ul icon-sm" ] ]
            , div [ onClick (op.doRichText targetid "List-ol"), class "tooltip has-tooltip-bottom", attribute "data-tooltip" "Ordered list" ] [ span [] [ A.icon "icon-list-ol icon-sm" ] ]
            , div [ onClick (op.doRichText targetid "List-check"), class "tooltip has-tooltip-bottom mr-3", attribute "data-tooltip" "Check list" ] [ span [] [ A.icon "icon-check-square icon-sm" ] ]
            , div [ onClick (op.doRichText targetid "MentionUser"), class "tooltip has-tooltip-bottom", attribute "data-tooltip" "Mention an user" ] [ span [] [ A.icon "icon-at-sign icon-sm" ] ]
            , div [ onClick (op.doRichText targetid "MentionTension"), class "tooltip has-tooltip-bottom mr-3", attribute "data-tooltip" "Reference a tension" ] [ A.icon "icon-exchange icon-sm" ]
            , div
                [ onClick (op.doToggleMdHelp targetid)
                , class "tooltip has-tooltip-bottom is-right is-h is-w"
                , classList [ ( "is-highlight", isMdHelpOpen ) ]
                , attribute "data-tooltip" T.markdownSupport
                ]
                [ A.icon "icon-markdown" ]
            ]
        , if isMdHelpOpen then
            div [ id "mdLegend", class "box" ]
                [ button [ class "delete is-pulled-right", onClick (op.doToggleMdHelp targetid) ] []
                , renderMarkdown "" T.markdownHelp
                ]

          else
            text ""
        ]


viewCommentTextarea targetid isModal placeholder_txt op form =
    let
        message =
            Dict.get "message" form.post |> withDefault ""

        line_len =
            List.length <| String.lines message

        ( max_len, min_len ) =
            if isMobile op.conf.screen then
                if isModal then
                    ( 4, 2 )

                else
                    ( 6, 4 )

            else if isModal then
                ( 10, 4 )

            else if targetid == "commentContractInput" then
                ( 15, 4 )

            else
                ( 15, 6 )
    in
    div []
        [ textarea
            [ id targetid
            , class "textarea"
            , classList [ ( "is-invisible-force", form.viewMode == Preview ) ]
            , rows (min max_len (max line_len min_len))
            , placeholder placeholder_txt
            , value message
            , onInput (op.doChangePost "message")

            --, contenteditable True
            ]
            []
        , if form.viewMode == Preview then
            div [ class "mt-2 mx-3" ]
                [ renderMarkdown "is-human hidden-textarea" message, hr [ class "has-background-border-light" ] [] ]

          else
            text ""
        , span [ id (targetid ++ "searchInput"), class "searchInput", attribute "aria-hidden" "true", attribute "style" "display:none;" ]
            [ Maybe.map2
                (\userSearchInput userInputMsg ->
                    UserInput.viewUserSeeker userSearchInput |> Html.map userInputMsg
                )
                op.userSearchInput
                op.userSearchInputMsg
                |> withDefault (text "")
            ]
        ]



--
-- <View Event>
--
--


viewEvent : Conf -> Maybe String -> Maybe TensionAction.TensionAction -> Event -> Html msg
viewEvent conf focusid_m action event =
    let
        eventView =
            case event.event_type of
                TensionEvent.Reopened ->
                    viewEventStatus conf.lang conf.now event TensionStatus.Open

                TensionEvent.Closed ->
                    viewEventStatus conf.lang conf.now event TensionStatus.Closed

                TensionEvent.TitleUpdated ->
                    viewEventTitle conf.lang conf.now event

                TensionEvent.TypeUpdated ->
                    viewEventType conf.lang conf.now event

                TensionEvent.Visibility ->
                    viewEventVisibility conf.lang conf.now event

                TensionEvent.Authority ->
                    viewEventAuthority conf.lang conf.now event action

                TensionEvent.AssigneeAdded ->
                    viewEventAssignee conf.lang conf.now event True

                TensionEvent.AssigneeRemoved ->
                    viewEventAssignee conf.lang conf.now event False

                TensionEvent.LabelAdded ->
                    viewEventLabel focusid_m conf.lang conf.now event True

                TensionEvent.LabelRemoved ->
                    viewEventLabel focusid_m conf.lang conf.now event False

                TensionEvent.BlobPushed ->
                    viewEventPushed conf.lang conf.now event action

                TensionEvent.BlobArchived ->
                    viewEventArchived conf.lang conf.now event action True

                TensionEvent.BlobUnarchived ->
                    viewEventArchived conf.lang conf.now event action False

                TensionEvent.MemberLinked ->
                    viewEventMemberLinked conf.lang conf.now event action

                TensionEvent.MemberUnlinked ->
                    viewEventMemberUnlinked conf.lang conf.now event action

                TensionEvent.UserJoined ->
                    viewEventUserJoined conf.lang conf.now event action

                TensionEvent.UserLeft ->
                    viewEventUserLeft conf.lang conf.now event action

                TensionEvent.Moved ->
                    viewEventMoved conf.lang conf.now event

                TensionEvent.Mentioned ->
                    viewEventMentioned conf.lang conf.now event

                _ ->
                    []
    in
    if eventView == [] then
        text ""

    else
        div [ id event.createdAt, class "media is-paddingless actionComment" ] eventView


viewEventStatus : Lang.Lang -> Time.Posix -> Event -> TensionStatus.TensionStatus -> List (Html msg)
viewEventStatus lang now event status =
    let
        ( actionIcon, actionText ) =
            case status of
                TensionStatus.Open ->
                    ( "icon-alert-circle", T.reopened2 )

                TensionStatus.Closed ->
                    ( "icon-alert-circle", T.closed2 )
    in
    [ span [ class "media-left", style "margin-left" "-4px" ] [ A.icon (actionIcon ++ " icon-1half has-text-" ++ statusColor status) ]
    , span [ class "media-content", attribute "style" "padding-top: 4px;margin-left: -4px" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, strong [] [ text actionText ], text (formatDate lang now event.createdAt) ]
        ]
    ]


viewEventTitle : Lang.Lang -> Time.Posix -> Event -> List (Html msg)
viewEventTitle lang now event =
    let
        icon =
            A.icon "icon-edit-2"
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, text T.updated2, span [ class "is-strong" ] [ text T.theSubject ], text (formatDate lang now event.createdAt) ]
        , span [ class "ml-3" ]
            [ span [ class "is-strong is-crossed" ] [ event.old |> withDefault "" |> text ]
            , span [ class "arrow-right mx-1" ] []
            , span [ class "is-strong" ] [ event.new |> withDefault "" |> text ]
            ]
        ]
    ]


viewEventType : Lang.Lang -> Time.Posix -> Event -> List (Html msg)
viewEventType lang now event =
    let
        icon =
            A.icon "icon-edit-2"
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, text T.changed2, span [ class "is-strong" ] [ text T.theType_ ], text (formatDate lang now event.createdAt) ]
        , span [ class "ml-3" ]
            [ span [ class "is-strong" ] [ event.old |> withDefault "" |> TensionType.fromString |> withDefault TensionType.Operational |> tensionIcon2 ]
            , span [ class "arrow-right mx-1" ] []
            , span [ class "is-strong" ] [ event.new |> withDefault "" |> TensionType.fromString |> withDefault TensionType.Operational |> tensionIcon2 ]
            ]
        ]
    ]


viewEventVisibility : Lang.Lang -> Time.Posix -> Event -> List (Html msg)
viewEventVisibility lang now event =
    let
        icon =
            A.icon "icon-eye"
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, text T.changed2, span [ class "is-strong" ] [ text T.theVisibility ], text (formatDate lang now event.createdAt) ]
        , span [ class "ml-3" ]
            [ span [ class "is-strong" ] [ event.old |> withDefault "" |> text ]
            , span [ class "arrow-right mx-1" ] []
            , span [ class "is-strong" ] [ event.new |> withDefault "" |> text ]
            ]
        ]
    ]


viewEventAuthority : Lang.Lang -> Time.Posix -> Event -> Maybe TensionAction.TensionAction -> List (Html msg)
viewEventAuthority lang now event action =
    let
        ( icon, eventText ) =
            case tensionAction2NodeType action of
                Just NodeType.Circle ->
                    ( A.icon "icon-shield", T.theGovernance )

                Just NodeType.Role ->
                    ( A.icon "icon-key", T.theAuthority )

                _ ->
                    ( A.icon "icon-key", "unknown action" )
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, text T.changed2, span [ class "is-strong" ] [ text eventText ], text (formatDate lang now event.createdAt) ]
        , span [ class "ml-3" ]
            [ span [ class "is-strong" ] [ event.old |> withDefault "" |> text ]
            , span [ class "arrow-right mx-1" ] []
            , span [ class "is-strong" ] [ event.new |> withDefault "" |> text ]
            ]
        ]
    ]


viewEventAssignee : Lang.Lang -> Time.Posix -> Event -> Bool -> List (Html msg)
viewEventAssignee lang now event isNew =
    let
        icon =
            A.icon "icon-user"

        ( actionText, value ) =
            if isNew then
                ( T.assigned2, withDefault "" event.new )

            else
                ( T.unassigned2, withDefault "" event.old )
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <|
            List.intersperse (text " ")
                [ viewUsernameLink event.createdBy.username, strong [] [ text actionText ], viewUsernameLink value, text (formatDate lang now event.createdAt) ]
        ]
    ]


viewEventLabel : Maybe String -> Lang.Lang -> Time.Posix -> Event -> Bool -> List (Html msg)
viewEventLabel focusid_m lang now event isNew =
    let
        icon =
            A.icon "icon-tag"

        ( actionText, value ) =
            if isNew then
                ( T.addedTheLabel, withDefault "unknown" event.new )

            else
                ( T.removedTheLabel, withDefault "unknown" event.old )

        label =
            Label "" (SE.leftOfBack "§" value) (SE.rightOfBack "§" value |> Just) []

        link =
            Maybe.map
                (\nid ->
                    toLink TensionsBaseUri nid [] ++ ("?l=" ++ label.name)
                )
                focusid_m
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [ class "labelsList" ] <|
            List.intersperse (text " ")
                [ viewUsernameLink event.createdBy.username, strong [] [ text actionText ], viewLabel "" link label, text (formatDate lang now event.createdAt) ]
        ]
    ]


viewEventPushed : Lang.Lang -> Time.Posix -> Event -> Maybe TensionAction.TensionAction -> List (Html msg)
viewEventPushed lang now event action_m =
    let
        action =
            withDefault TensionAction.NewRole action_m
    in
    [ div [ class "media-left" ] [ A.icon "icon-share" ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, strong [] [ text T.published2 ], text T.this, textD (action2str action), text (formatDate lang now event.createdAt) ]
        ]
    ]


viewEventArchived : Lang.Lang -> Time.Posix -> Event -> Maybe TensionAction.TensionAction -> Bool -> List (Html msg)
viewEventArchived lang now event action_m isArchived =
    let
        action =
            withDefault TensionAction.NewRole action_m

        ( icon, txt ) =
            if isArchived then
                ( A.icon "icon-archive", T.archived2 )

            else
                ( i [ class "icon-archive icon-is-slashed" ] [], T.unarchived2 )
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, strong [] [ text txt ], text T.this, textD (action2str action), text (formatDate lang now event.createdAt) ]
        ]
    ]


viewEventMemberLinked : Lang.Lang -> Time.Posix -> Event -> Maybe TensionAction.TensionAction -> List (Html msg)
viewEventMemberLinked lang now event action_m =
    [ div [ class "media-left" ] [ A.icon "icon-user-check has-text-success" ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink (withDefault "" event.new), strong [] [ text T.linked2 ], text T.toThisRole, text (formatDate lang now event.createdAt) ]
        ]
    ]


viewEventMemberUnlinked : Lang.Lang -> Time.Posix -> Event -> Maybe TensionAction.TensionAction -> List (Html msg)
viewEventMemberUnlinked lang now event action_m =
    let
        action_txt =
            case (getTensionCharac (withDefault TensionAction.NewRole action_m)).doc_type of
                NODE NodeType.Circle ->
                    T.toThisOrganisation

                _ ->
                    T.toThisRole
    in
    [ div [ class "media-left" ] [ A.icon "icon-user has-text-danger" ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink (withDefault "" event.old), strong [] [ text T.unlinked2 ], text action_txt, text (formatDate lang now event.createdAt) ]
        ]
    ]


viewEventUserJoined : Lang.Lang -> Time.Posix -> Event -> Maybe TensionAction.TensionAction -> List (Html msg)
viewEventUserJoined lang now event action_m =
    let
        action_txt =
            T.theOrganisation
    in
    [ div [ class "media-left" ] [ A.icon "icon-log-in" ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink (withDefault "" event.new), strong [] [ text T.joined2 ], text action_txt, text (formatDate lang now event.createdAt) ]
        ]
    ]


viewEventUserLeft : Lang.Lang -> Time.Posix -> Event -> Maybe TensionAction.TensionAction -> List (Html msg)
viewEventUserLeft lang now event action_m =
    let
        action =
            withDefault TensionAction.NewRole action_m

        action_txt =
            case event.new of
                Just type_ ->
                    case RoleType.fromString type_ of
                        Just RoleType.Guest ->
                            T.theOrganisation

                        _ ->
                            T.this ++ " " ++ decap T.role

                Nothing ->
                    action2str action |> decap
    in
    [ div [ class "media-left" ] [ A.icon "icon-log-out" ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink (withDefault "" event.old), strong [] [ text T.left2 ], text action_txt, text (formatDate lang now event.createdAt) ]
        ]
    ]


viewEventMoved : Lang.Lang -> Time.Posix -> Event -> List (Html msg)
viewEventMoved lang now event =
    [ div [ class "media-left" ] [ span [ class "arrow-right2 pl-0 pr-0 mr-0" ] [] ]
    , div [ class "media-content" ]
        [ span [] <|
            List.intersperse (text " ")
                [ viewUsernameLink event.createdBy.username
                , strong [] [ text T.moved2 ]
                , text T.from
                , event.old |> Maybe.map (\nid -> viewNodeRefShort OverviewBaseUri nid) |> withDefault (text "unknown")
                , text T.to
                , event.new |> Maybe.map (\nid -> viewNodeRefShort OverviewBaseUri nid) |> withDefault (text "unknown")
                , text (formatDate lang now event.createdAt)
                ]
        ]
    ]


viewEventMentioned : Lang.Lang -> Time.Posix -> Event -> List (Html msg)
viewEventMentioned lang now event =
    case event.mentioned of
        Just { id, status, title, receiverid } ->
            let
                goto =
                    withDefault "" event.new
            in
            [ div [ class "media-left" ] [ A.icon "icon-message-square" ]
            , div [ class "media-content" ]
                [ span [] <|
                    List.intersperse (text " ")
                        [ viewUsernameLink event.createdBy.username
                        , strong [] [ text T.mentioned2 ]
                        , text (formatDate lang now event.createdAt)
                        , div []
                            [ a
                                [ class "is-strong is-size-6 discrete-link mr-4"
                                , href ((Route.Tension_Dynamic_Dynamic { param1 = nid2rootid receiverid, param2 = id } |> toHref) ++ "?goto=" ++ goto)
                                ]
                                [ span
                                    [ class "tooltip has-tooltip-arrow"
                                    , attribute "data-tooltip" (tensionStatus2str status)
                                    ]
                                    [ A.icon ("icon-alert-circle icon-sm marginTensionStatus has-text-" ++ statusColor status) ]
                                , text title
                                ]
                            , a
                                [ class "discrete-link is-discrete"
                                , href (uriFromNameid OverviewBaseUri receiverid [])
                                ]
                                [ receiverid |> String.replace "#" "/" |> text ]
                            ]
                        ]
                ]
            ]

        Nothing ->
            []



--
-- </ View Event>
--
