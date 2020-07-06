module Form.NewTension exposing (view)

import Components.Fa as Fa
import Components.Loading as Loading exposing (viewGqlErrors)
import Components.Markdown exposing (renderMarkdown)
import Components.Text as T
import Dict
import Extra exposing (ternary, withMaybeData)
import Extra.Events exposing (onClickPD2, onEnter, onKeydown, onTab)
import Form exposing (isPostSendable)
import Fractal.Enum.RoleType as RoleType
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, li, nav, option, p, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, placeholder, required, rows, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.View exposing (edgeArrow, getTensionText, tensionTypeSpan)
import ModelSchema exposing (GqlData, RequestResult(..), Tension, UserRole)


{-| --view : TensionForm -> GqlData data -> subData -> Html msg
What should be the signature ?!
-}
view form result sd =
    let
        txt =
            getTensionText

        isLoading =
            result == LoadingSlowly

        isSendable =
            isPostSendable [ "title" ] form.post

        submitTension =
            ternary isSendable [ onClick (sd.submitMsg <| sd.submitNextMsg form False) ] []

        message =
            Dict.get "message" form.post |> withDefault ""
    in
    case result of
        Success _ ->
            div [ class "box is-light modalClose", onClick (sd.closeModalMsg "") ]
                [ Fa.icon0 "fas fa-check fa-2x has-text-success" " ", text txt.added ]

        other ->
            div [ class "modal-card finalModal" ]
                [ div [ class "modal-card-head" ]
                    [ div [ class "level modal-card-title" ]
                        [ div [ class "level-left" ] <|
                            List.intersperse (text "\u{00A0}")
                                [ span [ class "is-size-6 has-text-weight-semibold has-text-grey" ] [ text (txt.title ++ " | "), tensionTypeSpan "has-text-weight-medium" "text" form.tension_type ] ]
                        , div [ class "level-right" ] <| edgeArrow "button" (text form.source.name) (text form.target.name)
                        ]
                    ]
                , div [ class "modal-card-body" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ input
                                [ class "input autofocus followFocus"
                                , attribute "data-nextfocus" "textAreaModal"
                                , type_ "text"
                                , placeholder "Title*"
                                , required True
                                , onInput (sd.changePostMsg "title")
                                ]
                                []
                            ]
                        , p [ class "help-label" ] [ text txt.name_help ]
                        , br [] []
                        ]
                    , div [ class "message" ]
                        [ div [ class "message-header" ]
                            [ div [ class "tabs is-boxed is-small" ]
                                [ ul []
                                    [ li [ classList [ ( "is-active", sd.viewMode == Write ) ] ] [ a [ onClickPD2 (sd.changeInputMsg Write), target "_blank" ] [ text "Write" ] ]
                                    , li [ classList [ ( "is-active", sd.viewMode == Preview ) ] ] [ a [ onClickPD2 (sd.changeInputMsg Preview), target "_blank" ] [ text "Preview" ] ]
                                    ]
                                ]
                            ]
                        , div [ class "message-body" ]
                            [ div [ class "field" ]
                                [ div [ class "control" ]
                                    [ case sd.viewMode of
                                        Write ->
                                            textarea
                                                [ id "textAreaModal"
                                                , class "textarea"
                                                , rows 10
                                                , placeholder "Leave a comment"
                                                , value message
                                                , onInput (sd.changePostMsg "message")
                                                ]
                                                []

                                        Preview ->
                                            div [] [ renderMarkdown message "is-dark", hr [] [] ]
                                    ]
                                , p [ class "help-label" ] [ text txt.message_help ]
                                , br [] []
                                ]
                            ]
                        ]
                    ]
                , div [ class "modal-card-foot", attribute "style" "display: block;" ]
                    [ case other of
                        Failure err ->
                            viewGqlErrors err

                        _ ->
                            div [] []
                    , div [ class "field is-grouped is-grouped-right" ]
                        [ div [ class "control" ]
                            [ div [ class "buttons" ]
                                [ button
                                    ([ class "button has-text-weight-semibold"
                                     , classList [ ( "is-success", isSendable ), ( "is-loading", isLoading ) ]
                                     , disabled (not isSendable)
                                     ]
                                        ++ submitTension
                                    )
                                    [ text txt.submit ]
                                ]
                            ]
                        ]
                    ]
                ]
