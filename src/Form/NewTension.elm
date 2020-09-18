module Form.NewTension exposing (NewTensionForm, NewTensionFormData, create, getInputData, reset, setActiveButton, setViewMode, view)

import Components.Fa as Fa
import Components.Loading as Loading exposing (viewGqlErrors)
import Components.Markdown exposing (renderMarkdown)
import Components.NodeDoc exposing (InputData)
import Components.Text as T
import Dict
import Extra exposing (ternary, withMaybeData)
import Extra.Events exposing (onClickPD, onClickPD2, onEnter, onKeydown, onTab)
import Form exposing (isPostSendable)
import Fractal.Enum.RoleType as RoleType
import Generated.Route as Route exposing (toHref)
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, li, nav, option, p, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, placeholder, required, rows, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.View exposing (edgeArrow, getTensionText, tensionTypeSpan)
import ModelSchema exposing (GqlData, RequestResult(..), Tension, UserRole)
import Time


type alias NewTensionForm =
    { activeButton : Maybe Int
    , viewMode : InputViewMode
    }


create : NewTensionForm
create =
    { activeButton = Nothing
    , viewMode = Write
    }


reset : NewTensionForm -> NewTensionForm
reset ntf =
    { ntf | activeButton = Nothing, viewMode = Write }


setActiveButton : Bool -> NewTensionForm -> NewTensionForm
setActiveButton doClose ntf =
    if doClose then
        { ntf | activeButton = Just 0 }

    else
        { ntf | activeButton = Just 1 }


setViewMode : InputViewMode -> NewTensionForm -> NewTensionForm
setViewMode viewMode ntf =
    { ntf | viewMode = viewMode }


type alias NewTensionFormData msg =
    { form : TensionForm
    , result : GqlData Tension
    , data : NewTensionForm
    , onChangeInputViewMode : InputViewMode -> msg
    , onChangeNode : String -> String -> msg
    , onCloseModal : String -> msg
    , onSubmitTension : TensionForm -> Bool -> Time.Posix -> msg
    , onSubmit : (Time.Posix -> msg) -> msg
    }


getInputData : NewTensionFormData msg -> InputData msg
getInputData ntd =
    { node = ntd.form.node
    , onChangeNode = ntd.onChangeNode
    }


view : NewTensionFormData msg -> Html msg
view ntd =
    let
        form =
            ntd.form

        txt =
            getTensionText

        isLoading =
            ntd.result == LoadingSlowly

        isSendable =
            isPostSendable [ "title" ] form.post

        submitTension =
            ternary isSendable [ onClick (ntd.onSubmit <| ntd.onSubmitTension form False) ] []

        message =
            Dict.get "message" form.post |> withDefault ""
    in
    case ntd.result of
        Success res ->
            let
                link =
                    Route.Tension_Dynamic_Dynamic { param1 = form.target.rootnameid, param2 = res.id } |> toHref
            in
            div [ class "box is-light" ]
                [ Fa.icon "fas fa-check fa-2x has-text-success" " "
                , text (txt.added ++ " ")
                , a
                    [ href link
                    , onClickPD (ntd.onCloseModal link)
                    , target "_blank"
                    ]
                    [ text T.checkItOut ]
                ]

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
                                , onInput (ntd.onChangeNode "title")
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
                                    [ li [ classList [ ( "is-active", ntd.data.viewMode == Write ) ] ] [ a [ onClickPD2 (ntd.onChangeInputViewMode Write), target "_blank" ] [ text "Write" ] ]
                                    , li [ classList [ ( "is-active", ntd.data.viewMode == Preview ) ] ] [ a [ onClickPD2 (ntd.onChangeInputViewMode Preview), target "_blank" ] [ text "Preview" ] ]
                                    ]
                                ]
                            ]
                        , div [ class "message-body" ]
                            [ div [ class "field" ]
                                [ div [ class "control" ]
                                    [ case ntd.data.viewMode of
                                        Write ->
                                            textarea
                                                [ id "textAreaModal"
                                                , class "textarea"
                                                , rows 10
                                                , placeholder "Leave a comment"
                                                , value message
                                                , onInput (ntd.onChangeNode "message")
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
