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
import Bulk.Error exposing (viewGqlErrors)
import Bulk.View exposing (viewTensionDateAndUserC, viewUpdated, viewUser0, viewUser2)
import Dict
import Extra exposing (ternary, textH, upH)
import Extra.Events exposing (onClickPD2)
import Form exposing (isPostSendable)
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, strong, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, contenteditable, disabled, href, id, placeholder, readonly, rows, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra as LE
import Loading exposing (GqlData, RequestResult(..))
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelSchema exposing (Comment, PatchTensionPayloadID, TensionHead, UserCtx)
import Session exposing (Conf, isMobile)
import Text as T
import Time


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
    , conf : Conf
    }


type alias OpNewComment msg =
    { doChangeViewMode : InputViewMode -> msg
    , doChangePost : String -> String -> msg
    , doSubmit : Bool -> (Time.Posix -> msg) -> msg
    , doSubmitComment : Maybe TensionStatus.TensionStatus -> Time.Posix -> msg
    , doRichText : String -> String -> msg
    , doToggleMdHelp : String -> msg
    , conf : Conf
    }


type alias OpNewCommentContract msg =
    { doChangeViewMode : InputViewMode -> msg
    , doChangePost : String -> String -> msg
    , doSubmit : Bool -> (Time.Posix -> msg) -> msg
    , doSubmitComment : Time.Posix -> msg
    , doRichText : String -> String -> msg
    , doToggleMdHelp : String -> msg
    , conf : Conf
    }


viewComment : OpEditComment msg -> Comment -> CommentPatchForm -> GqlData Comment -> Html msg
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
                viewUpdateInput op form.uctx c form result

              else
                div [ class "message" ]
                    [ div [ class "message-header has-arrow-left pl-1-mobile", classList [ ( "has-background-tag", isAuthor && False ) ] ]
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


viewUpdateInput : OpEditComment msg -> UserCtx -> Comment -> CommentPatchForm -> GqlData Comment -> Html msg
viewUpdateInput op uctx comment form result =
    let
        message =
            Dict.get "message" form.post |> withDefault comment.message

        isLoading =
            result == LoadingSlowly

        isSendable =
            message /= comment.message
    in
    div [ class "message commentInput" ]
        [ div [ class "message-header has-arrow-left" ] [ viewCommentInputHeader "updateCommentInput" "" op form ]
        , div [ class "message-body submitFocus" ]
            [ div [ class "field" ]
                [ div [ class "control" ]
                    [ viewCommentTextarea "updateCommentInput" False T.leaveComment op form message ]
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


viewCommentInput : OpNewComment msg -> UserCtx -> TensionHead -> TensionForm -> GqlData PatchTensionPayloadID -> Html msg
viewCommentInput op uctx tension form result =
    let
        message =
            Dict.get "message" form.post |> withDefault ""

        isLoading =
            result == LoadingSlowly

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
        [ div [ class "media-left is-hidden-mobile" ] [ viewUser2 uctx.username ]
        , div [ class "media-content" ]
            [ div [ class "message" ]
                [ div [ class "message-header has-arrow-left" ] [ viewCommentInputHeader "commentInput" "" op form ]
                , div [ class "message-body submitFocus" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ viewCommentTextarea "commentInput" False T.leaveComment op form message ]
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


viewContractCommentInput : OpNewCommentContract msg -> UserCtx -> CommentPatchForm -> GqlData Comment -> Html msg
viewContractCommentInput op uctx form result =
    let
        message =
            Dict.get "message" form.post |> withDefault ""

        isLoading =
            result == LoadingSlowly

        isSendable =
            isPostSendable [ "message" ] form.post

        doSubmit =
            ternary isSendable [ onClick (op.doSubmit isLoading op.doSubmitComment) ] []
    in
    div [ id "tensionCommentInput", class "media section is-paddingless commentInput" ]
        [ div [ class "media-left is-hidden-mobile" ] [ viewUser2 uctx.username ]
        , div [ class "media-content" ]
            [ div [ class "message" ]
                [ div [ class "message-header has-arrow-left" ] [ viewCommentInputHeader "commentContractInput" "" op form ]
                , div [ class "message-body submitFocus" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ viewCommentTextarea "commentContractInput" False T.leaveComment op form message ]
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
                    [ li [ classList [ ( "is-active", form.viewMode == Write ) ] ] [ a [ onClickPD2 (op.doChangeViewMode Write), target "_blank" ] [ text T.write ] ]
                    , li [ classList [ ( "is-active", form.viewMode == Preview ) ] ] [ a [ onClickPD2 (op.doChangeViewMode Preview), target "_blank" ] [ text T.preview ] ]
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


viewCommentTextarea targetid isModal placeholder_txt op form message =
    let
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
        ]
