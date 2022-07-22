module Components.Comments exposing (..)

import Assets as A
import Dict
import Extra exposing (ternary, textH, upH)
import Extra.Events exposing (onClickPD2)
import Form exposing (isPostSendable)
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, strong, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, readonly, rows, style, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Loading exposing (GqlData, ModalData, RequestResult(..), viewGqlErrors)
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelCommon exposing (CommentPatchForm, InputViewMode(..), TensionForm)
import ModelCommon.View exposing (viewTensionDateAndUserC, viewUpdated, viewUser0, viewUser2)
import ModelSchema exposing (Comment, PatchTensionPayloadID, TensionHead, UserCtx)
import Text as T
import Time


type alias OpEditComment msg =
    { doUpdate : Comment -> msg
    , doCancelComment : msg
    , doChangeViewMode : InputViewMode -> msg
    , doChangePost : String -> String -> msg
    , doSubmit : Bool -> (Time.Posix -> msg) -> msg
    , doEditComment : Time.Posix -> msg
    , now : Time.Posix
    }


type alias OpNewComment msg =
    { doChangeViewMode : InputViewMode -> msg
    , doChangePost : String -> String -> msg
    , doSubmit : Bool -> (Time.Posix -> msg) -> msg
    , doSubmitComment : Maybe TensionStatus.TensionStatus -> Time.Posix -> msg
    , rows : Int
    }


type alias OpNewCommentContract msg =
    { doChangeViewMode : InputViewMode -> msg
    , doChangePost : String -> String -> msg
    , doSubmit : Bool -> (Time.Posix -> msg) -> msg
    , doSubmitComment : Time.Posix -> msg
    , rows : Int
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
                        , viewTensionDateAndUserC op.now c.createdAt c.createdBy
                        , case c.updatedAt of
                            Just updatedAt ->
                                viewUpdated op.now updatedAt

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
                                        [ div [ class "dropdown-item button-light" ] [ p [ onClick (op.doUpdate c) ] [ textH T.edit ] ] ]
                                    ]
                                ]

                          else
                            text ""
                        ]
                    , div [ class "message-body" ]
                        [ case c.message of
                            "" ->
                                div [ class "help is-italic" ] [ text "No message provided." ]

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

        viewMode =
            form.viewMode

        isLoading =
            result == LoadingSlowly

        isSendable =
            message /= comment.message
    in
    div [ class "message commentInput" ]
        [ div [ class "message-header has-arrow-left" ]
            [ div [ class "tabs is-boxed is-small" ]
                [ ul []
                    [ li [ classList [ ( "is-active", viewMode == Write ) ] ] [ a [ onClickPD2 (op.doChangeViewMode Write), target "_blank" ] [ text "Write" ] ]
                    , li [ classList [ ( "is-active", viewMode == Preview ) ] ] [ a [ onClickPD2 (op.doChangeViewMode Preview), target "_blank" ] [ text "Preview" ] ]
                    ]
                ]
            ]
        , div [ class "message-body submitFocus" ]
            [ div [ class "field" ]
                [ div [ class "control" ]
                    [ case viewMode of
                        Write ->
                            let
                                line_len =
                                    List.length <| String.lines message
                            in
                            textarea
                                [ id "updateCommentInput"
                                , class "textarea"
                                , rows (min 15 (max line_len 7))
                                , placeholder (upH T.leaveComment)
                                , value message
                                , onInput (op.doChangePost "message")
                                ]
                                []

                        Preview ->
                            div [] [ renderMarkdown "is-human" message, hr [ class "has-background-border-light" ] [] ]
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
                            , onClick op.doCancelComment
                            ]
                            [ textH T.cancel ]
                        , button
                            [ class "button is-success defaultSubmit"
                            , classList [ ( "is-loading", isLoading ) ]
                            , disabled (not isSendable)
                            , onClick (op.doSubmit isLoading op.doEditComment)
                            ]
                            [ textH T.update ]
                        ]
                    ]
                ]
            ]
        ]


viewCommentInput : OpNewComment msg -> UserCtx -> TensionHead -> TensionForm -> GqlData PatchTensionPayloadID -> InputViewMode -> Html msg
viewCommentInput op uctx tension form result viewMode =
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
                    ternary (message == "") "Close tension" "Close and comment"

                TensionStatus.Closed ->
                    ternary (message == "") "Reopen tension" "Reopen and comment"
    in
    div [ id "tensionCommentInput", class "media section is-paddingless commentInput" ]
        [ div [ class "media-left is-hidden-mobile" ] [ viewUser2 uctx.username ]
        , div [ class "media-content" ]
            [ div [ class "message" ]
                [ div [ class "message-header has-arrow-left" ]
                    [ div [ class "tabs is-boxed is-small" ]
                        [ ul []
                            [ li [ classList [ ( "is-active", viewMode == Write ) ] ] [ a [ onClickPD2 (op.doChangeViewMode Write), target "_blank" ] [ text "Write" ] ]
                            , li [ classList [ ( "is-active", viewMode == Preview ) ] ] [ a [ onClickPD2 (op.doChangeViewMode Preview), target "_blank" ] [ text "Preview" ] ]
                            ]
                        ]
                    ]
                , div [ class "message-body submitFocus" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ case viewMode of
                                Write ->
                                    let
                                        line_len =
                                            List.length <| String.lines message
                                    in
                                    textarea
                                        [ id "commentInput"
                                        , class "textarea"
                                        , rows (min 15 (max line_len op.rows))
                                        , placeholder "Leave a comment"
                                        , value message
                                        , onInput (op.doChangePost "message")
                                        ]
                                        []

                                Preview ->
                                    div [ class "mt-4 mx-3" ] [ renderMarkdown "is-human" message, hr [ class "has-background-border-light" ] [] ]
                            ]
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
                                    [ text "Comment" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewContractCommentInput : OpNewCommentContract msg -> UserCtx -> CommentPatchForm -> GqlData Comment -> InputViewMode -> Html msg
viewContractCommentInput op uctx form result viewMode =
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
                [ div [ class "message-header has-arrow-left" ]
                    [ div [ class "tabs is-boxed is-small" ]
                        [ ul []
                            [ li [ classList [ ( "is-active", viewMode == Write ) ] ] [ a [ onClickPD2 (op.doChangeViewMode Write), target "_blank" ] [ text "Write" ] ]
                            , li [ classList [ ( "is-active", viewMode == Preview ) ] ] [ a [ onClickPD2 (op.doChangeViewMode Preview), target "_blank" ] [ text "Preview" ] ]
                            ]
                        ]
                    ]
                , div [ class "message-body submitFocus" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ case viewMode of
                                Write ->
                                    textarea
                                        [ id "commentInput"
                                        , class "textarea"
                                        , rows op.rows
                                        , placeholder "Leave a comment"
                                        , value message
                                        , onInput (op.doChangePost "message")
                                        ]
                                        []

                                Preview ->
                                    div [ class "mt-4 mx-3" ] [ renderMarkdown "is-human" message, hr [ class "has-background-border-light" ] [] ]
                            ]
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
                                    [ text "Comment" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
