module Form.NewTension exposing (view)

import Components.Loading as Loading exposing (viewErrors)
import Dict
import Extra.Events exposing (onClickPD2, onEnter, onKeydown, onTab)
import Form exposing (isPostSendable)
import Fractal.Enum.RoleType as RoleType
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, li, nav, option, p, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, placeholder, rows, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.View exposing (edgeArrow, tensionTypeSpan)
import ModelSchema exposing (GqlData, RequestResult(..), Tension, UserRole)


{-| --view : TensionForm -> GqlData (Maybe Tension) -> (String -> String -> msg) -> ((msg -> TensionForm) -> Time.Posix -> msg) -> (TensionForm -> Time.Posix -> msg) -> Html msg
What should be the signature ?!
-}
view form result changePostMsg closeModalMsg submitMsg submitNextMsg =
    let
        isSendable =
            isPostSendable [ "title" ] form.post

        isLoading =
            result == LoadingSlowly
    in
    case result of
        Success _ ->
            div [ class "box has-background-success modalClose", onClick (closeModalMsg "") ] [ text "Tension added." ]

        other ->
            div [ class "modal-card finalModal" ]
                [ div [ class "modal-card-head" ]
                    [ div [ class "level modal-card-title" ]
                        [ div [ class "level-left" ] <|
                            List.intersperse (text "\u{00A0}")
                                [ span [ class "is-size-6 has-text-weight-semibold has-text-grey" ] [ text "New tension | ", tensionTypeSpan "has-text-weight-medium" "text" form.tension_type ] ]
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
                                , placeholder "Title"
                                , onInput (changePostMsg "title")
                                ]
                                []
                            ]
                        , p [ class "help-label" ] [ text "Title that sumarize your tension." ]
                        , br [] []
                        ]
                    , div [ class "field" ]
                        [ div [ class "control" ]
                            [ textarea
                                [ id "textAreaModal"
                                , class "textarea"
                                , rows 10
                                , placeholder "Leave a comment"
                                , onInput (changePostMsg "message")
                                ]
                                []
                            ]
                        , p [ class "help-label" ] [ text "Add a description to help others understand your issue." ]
                        ]
                    ]
                , div [ class "modal-card-foot", attribute "style" "display: block;" ]
                    [ case other of
                        Failure err ->
                            viewErrors err

                        _ ->
                            div [] []
                    , div [ class "field is-grouped is-grouped-right" ]
                        [ div [ class "control" ]
                            [ if isSendable then
                                div [ class "buttons" ]
                                    [ button
                                        [ class "button is-success has-text-weight-semibold"
                                        , classList [ ( "is-loading", isLoading ) ]
                                        , onClick (submitMsg <| submitNextMsg form)
                                        ]
                                        [ text "Submit new tension" ]
                                    ]

                              else
                                div [ class "buttons" ]
                                    [ button [ class "button has-text-weight-semibold", disabled True ]
                                        [ text "Submit new tension" ]
                                    ]
                            ]
                        ]
                    ]
                ]
