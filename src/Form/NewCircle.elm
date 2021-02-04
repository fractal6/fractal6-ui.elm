module Form.NewCircle exposing (view)

import Components.I as I
import Components.Loading as Loading exposing (RequestResult(..), viewGqlErrors, withMaybeData)
import Components.NodeDoc exposing (nodeAboutInputView, nodeLinksInputView, nodeMandateInputView)
import Dict
import Extra exposing (ternary)
import Extra.Events exposing (onClickPD, onClickPD2, onEnter, onKeydown, onTab)
import Form exposing (isPostSendable)
import Form.NewTension exposing (NewTensionForm, Op)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Generated.Route as Route exposing (toHref)
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, li, nav, option, p, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, placeholder, required, rows, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (FractalBaseRoute(..))
import ModelCommon.View exposing (edgeArrow, getNodeTextFromNodeType, tensionTypeSpan)
import Text as T



-- see Op in NewTension.elm


view : Op msg -> Html msg
view op =
    let
        data =
            op.data

        form =
            data.form

        txt =
            getNodeTextFromNodeType (form.node.type_ |> withDefault NodeType.Role)

        isLoading =
            data.result == LoadingSlowly

        isSendable =
            form.node.name /= Nothing && (form.node.mandate |> Maybe.map (\x -> x.purpose)) /= Nothing

        submitTension =
            ternary isSendable [ onClickPD2 (op.onSubmit <| op.onSubmitTension data False) ] []

        submitCloseTension =
            ternary isSendable [ onClickPD2 (op.onSubmit <| op.onSubmitTension data True) ] []
    in
    case data.result of
        Success res ->
            let
                link =
                    Route.Tension_Dynamic_Dynamic_Action { param1 = form.target.rootnameid, param2 = res.id } |> toHref
            in
            div [ class "box is-light" ]
                [ I.icon1 "icon-check icon-2x has-text-success" " "
                , if data.activeButton == Just 0 then
                    text (txt.added ++ " ")

                  else
                    text (txt.tension_added ++ " ")
                , a
                    [ href link
                    , onClickPD (op.onCloseModal link)
                    , target "_blank"
                    ]
                    [ text T.checkItOut ]
                ]

        other ->
            let
                title =
                    Dict.get "title" form.post |> withDefault ""

                nameid =
                    form.node.nameid |> withDefault ""
            in
            div [ class "modal-card finalModal" ]
                [ div [ class "modal-card-head" ]
                    [ div [ class "level modal-card-title" ]
                        [ div [ class "level-left" ] <|
                            List.intersperse (text "\u{00A0}")
                                [ span [ class "is-size-6 has-text-weight-semibold has-text-grey" ] [ text (txt.title ++ " |\u{00A0}"), tensionTypeSpan "has-text-weight-medium" "text" form.tension_type ] ]
                        , div [ class "level-right" ] <| edgeArrow "button" (text form.source.name) (text form.target.name)
                        ]
                    ]
                , div [ class "modal-card-body" ]
                    [ nodeAboutInputView False OverviewBaseUri txt form.node op
                    , div [ class "card cardForm" ]
                        [ div [ class "has-text-black is-aligned-center", attribute "style" "background-color: #e1e1e1;" ] [ text T.mandateH ]
                        , div [ class "card-content" ] [ nodeMandateInputView txt form.node op ]
                        ]
                    , if op.data.doAddLinks || (form.users |> List.filter (\u -> u.username /= "")) /= [] then
                        div
                            [ class "card cardForm"
                            , attribute "style" "overflow: unset;"
                            ]
                            --[ div [ class "card-header" ] [ div [ class "card-header-title" ] [ text T.firstLinkH ] ]
                            [ div [ class "has-text-black is-aligned-center", attribute "style" "background-color: #e1e1e1;" ] [ text T.firstLinkH ]
                            , div [ class "card-content" ] [ nodeLinksInputView txt form op.data op ]
                            ]

                      else
                        div [ class "field" ]
                            [ div [ class "button is-info", onClick op.onAddLinks ]
                                [ I.icon1 "icon-plus" "", text "Add first link" ]
                            ]
                    , br [] []
                    , div [ class "field" ]
                        [ div [ class "control" ]
                            [ textarea
                                [ class "textarea"
                                , rows 3
                                , placeholder T.leaveComment
                                , onInput <| op.onChangeNode "message"
                                ]
                                []
                            ]
                        , p [ class "help-label" ] [ text txt.message_help ]
                        ]
                    , br [] []
                    ]
                , div [ class "modal-card-foot", attribute "style" "display: block;" ]
                    [ case other of
                        Failure err ->
                            viewGqlErrors err

                        _ ->
                            text ""
                    , div [ class "field is-grouped is-grouped-right" ]
                        [ div [ class "control" ]
                            [ div [ class "buttons" ]
                                [ button
                                    ([ class "button is-warning"
                                     , classList
                                        [ ( "is-loading", isLoading && data.activeButton == Just 1 ) ]
                                     , disabled (not isSendable || isLoading)
                                     ]
                                        ++ submitTension
                                    )
                                    [ text txt.submit ]
                                , button
                                    ([ class "button is-success"
                                     , classList
                                        [ ( "is-loading", isLoading && data.activeButton == Just 0 ) ]
                                     , disabled (not isSendable || isLoading)
                                     ]
                                        ++ submitCloseTension
                                    )
                                    [ text txt.close_submit ]
                                ]
                            ]
                        ]
                    ]
                ]
