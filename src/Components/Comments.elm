{-
  Fractale - Self-organisation for humans.
  Copyright (C) 2022 Fractale Co

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
import Dict
import Extra exposing (ternary, textH, upH)
import Extra.Events exposing (onClickPD2)
import Form exposing (isPostSendable)
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, strong, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, contenteditable, disabled, href, id, placeholder, readonly, rows, style, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Loading exposing (GqlData, ModalData, RequestResult(..), viewGqlErrors)
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelCommon exposing (CommentPatchForm, InputViewMode(..), TensionForm)
import ModelCommon.View exposing (viewTensionDateAndUserC, viewUpdated, viewUser0, viewUser2)
import ModelSchema exposing (Comment, PatchTensionPayloadID, TensionHead, UserCtx)
import Session exposing (Conf)
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
    , conf : Conf
    }


type alias OpNewComment msg =
    { doChangeViewMode : InputViewMode -> msg
    , doChangePost : String -> String -> msg
    , doSubmit : Bool -> (Time.Posix -> msg) -> msg
    , doSubmitComment : Maybe TensionStatus.TensionStatus -> Time.Posix -> msg
    , doRichText : String -> String -> msg
    , conf : Conf
    }


type alias OpNewCommentContract msg =
    { doChangeViewMode : InputViewMode -> msg
    , doChangePost : String -> String -> msg
    , doSubmit : Bool -> (Time.Posix -> msg) -> msg
    , doSubmitComment : Time.Posix -> msg
    , doRichText : String -> String -> msg
    , conf : Conf
    }


viewComment : OpEditComment msg -> Comment -> CommentPatchForm -> GqlData Comment -> Html msg
viewComment op c form result =
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
                    [ div [ class "message-header has-arrow-left pl-1-mobile" ]
                        [ span [ class "is-hidden-tablet" ] [ viewUser0 c.createdBy.username ]
                        , viewTensionDateAndUserC op.conf c.createdAt c.createdBy
                        , case c.updatedAt of
                            Just updatedAt ->
                                viewUpdated op.conf updatedAt

                            Nothing ->
                                text ""
                        , if c.createdBy.username == form.uctx.username then
                            div [ class "dropdown is-right is-pulled-right " ]
                                [ div [ class "dropdown-trigger" ]
                                    [ div
                                        [ class "ellipsis"
                                        , attribute "aria-controls" ("dropdown-menu_ellipsis" ++ c.id)
                                        , attribute "aria-haspopup" "true"
                                        ]
                                        [ A.icon "icon-ellipsis-v" ]
                                    ]
                                , div [ id ("dropdown-menu_ellipsis" ++ c.id), class "dropdown-menu", attribute "role" "menu" ]
                                    [ div [ class "dropdown-content p-0" ]
                                        [ div [ class "dropdown-item button-light" ] [ p [ onClick (op.doUpdate c) ] [ text T.edit ] ] ]
                                    ]
                                ]

                          else
                            text ""
                        ]
                    , div [ class "message-body" ]
                        [ case c.message of
                            "" ->
                                div [ class "help is-italic" ] [ text T.noMessageProvided ]

                            message ->
                                renderMarkdown "is-human" message
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
        [ div [ class "message-header has-arrow-left" ] [ viewCommentHeader "updateCommentInput" "" op form ]
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
                            [ class "button is-success defaultSubmit"
                            , classList [ ( "is-loading", isLoading ) ]
                            , disabled (not isSendable)
                            , onClick (op.doSubmit isLoading op.doEditComment)
                            ]
                            [ text T.update ]
                        , button
                            [ class "button"
                            , onClick op.doCancelComment
                            ]
                            [ text T.cancel ]
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
                [ div [ class "message-header has-arrow-left" ] [ viewCommentHeader "commentInput" "" op form ]
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
                                    ([ class "button is-success defaultSubmit"
                                     , classList [ ( "is-loading", isLoading && form.status == Nothing ) ]
                                     , disabled (not isSendable)
                                     ]
                                        ++ doSubmit
                                    )
                                    [ text T.comment ]
                                , button
                                    ([ class "button"
                                     , classList [ ( "is-danger", tension.status == TensionStatus.Open ), ( "is-loading", isLoading && form.status /= Nothing ) ]
                                     ]
                                        ++ submitCloseOpenTension
                                    )
                                    [ text closeOpenText ]
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
                [ div [ class "message-header has-arrow-left" ] [ viewCommentHeader "commentContractInput" "" op form ]
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


viewCommentHeader targetid cls_tabs op form =
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
            , div [ onClick (op.doRichText targetid "Quote"), class "tooltip has-tooltip-bottom", attribute "data-tooltip" "Quote" ] [ span [ style "font-size" "18px" ] [ text "”" ] ]
            , div [ onClick (op.doRichText targetid "Link"), class "tooltip has-tooltip-bottom", attribute "data-tooltip" "Link" ] [ span [] [ A.icon "icon-link icon-sm" ] ]
            , div [ onClick (op.doRichText targetid "MentionUser"), class "tooltip has-tooltip-bottom ml-2", attribute "data-tooltip" "Mention an user" ] [ text "@" ]
            , div [ onClick (op.doRichText targetid "MentionTension"), class "tooltip has-tooltip-bottom", attribute "data-tooltip" "Reference a tension" ] [ A.icon "icon-exchange icon-sm" ]
            , div [ class "tooltip has-tooltip-bottom is-right", attribute "data-tooltip" T.markdownSupport ] [ A.icon "icon-markdown" ]
            ]
        ]


viewCommentTextarea targetid isModal placeholder_txt op form message =
    let
        line_len =
            List.length <| String.lines message

        ( max_len, min_len ) =
            if op.conf.screen.w < 769 then
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
                [ renderMarkdown "is-human" message, hr [ class "has-background-border-light" ] [] ]

          else
            text ""
        ]
