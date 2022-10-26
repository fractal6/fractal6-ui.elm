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
    }


type alias OpNewCommentContract msg =
    { doChangeViewMode : InputViewMode -> msg
    , doChangePost : String -> String -> msg
    , doSubmit : Bool -> (Time.Posix -> msg) -> msg
    , doSubmitComment : Time.Posix -> msg
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
                        , viewTensionDateAndUserC form.uctx.lang op.now c.createdAt c.createdBy
                        , case c.updatedAt of
                            Just updatedAt ->
                                viewUpdated form.uctx.lang op.now updatedAt

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
        [ div [ class "message-header has-arrow-left" ] [ viewCommentHeader "" True op form ]
        , div [ class "message-body submitFocus" ]
            [ div [ class "field" ]
                [ div [ class "control" ]
                    [ case form.viewMode of
                        Write ->
                            let
                                line_len =
                                    List.length <| String.lines message
                            in
                            textarea
                                [ id "updateCommentInput"
                                , class "textarea"
                                , rows (min 15 (max line_len 6))
                                , placeholder T.leaveComment
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
                [ div [ class "message-header has-arrow-left" ] [ viewCommentHeader "" True op form ]
                , div [ class "message-body submitFocus" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ case form.viewMode of
                                Write ->
                                    let
                                        line_len =
                                            List.length <| String.lines message
                                    in
                                    textarea
                                        [ id "commentInput"
                                        , class "textarea"
                                        , rows (min 15 (max line_len 6))
                                        , placeholder T.leaveComment
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
                [ div [ class "message-header has-arrow-left" ] [ viewCommentHeader "" True op form ]
                , div [ class "message-body submitFocus" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ case form.viewMode of
                                Write ->
                                    let
                                        line_len =
                                            List.length <| String.lines message
                                    in
                                    textarea
                                        [ id "commentInput"
                                        , class "textarea"
                                        , rows (min 10 (max line_len 4))
                                        , placeholder T.leaveComment
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


viewCommentHeader cls_tabs withToolbar op form =
    div [ class "level commentHeader" ]
        [ div [ class "level-left" ]
            [ div [ class ("tabs is-boxed is-small " ++ cls_tabs) ]
                [ ul []
                    [ li [ classList [ ( "is-active", form.viewMode == Write ) ] ] [ a [ onClickPD2 (op.doChangeViewMode Write), target "_blank" ] [ text T.write ] ]
                    , li [ classList [ ( "is-active", form.viewMode == Preview ) ] ] [ a [ onClickPD2 (op.doChangeViewMode Preview), target "_blank" ] [ text T.preview ] ]
                    ]
                ]
            ]
        , if withToolbar then
            div [ class "level-right" ]
                [ --div [ class "tooltip has-tooltip-bottom px-3", attribute "data-tooltip" "Add heading text" ] [ text "H" ]
                  --, div [ class "tooltip has-tooltip-bottom px-3", attribute "data-tooltip" "Add bold text" ] [ strong [] [ text "B" ] ]
                  --, div [ class "tooltip has-tooltip-bottom px-3", attribute "data-tooltip" "Add italic text" ] [ span [ class "is-italic" ] [ text "I" ] ]
                  --, div [ class "tooltip has-tooltip-bottom px-3", attribute "data-tooltip" "Mention an user" ] [ text "@" ]
                  --, div [ class "tooltip has-tooltip-bottom px-3", attribute "data-tooltip" "Reference a tension" ] [ A.icon "icon-exchange icon-sm" ]
                  div [ class "tooltip has-tooltip-bottom pl-3 ml-2", attribute "data-tooltip" T.markdownSupport ] [ A.icon "icon-markdown" ]
                ]

          else
            text ""
        ]
