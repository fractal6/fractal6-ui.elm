module Form.NewCircle exposing (view)

import Components.Fa as Fa
import Components.Loading as Loading exposing (viewGqlErrors)
import Components.NodeDoc exposing (getFirstLinks, nodeAboutInputView, nodeLinksInputView, nodeMandateInputView)
import Components.Text as T
import Dict
import Extra exposing (ternary, withMaybeData)
import Extra.Events exposing (onClickPD, onClickPD2, onEnter, onKeydown, onTab)
import Form exposing (isPostSendable)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Generated.Route as Route exposing (toHref)
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, li, nav, option, p, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, placeholder, required, rows, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.View exposing (edgeArrow, getNodeTextFromNodeType, tensionTypeSpan)
import ModelSchema exposing (GqlData, Node, NodeFragment, RequestResult(..), UserRole, initNodeFragment)


{-| --view : TensionForm -> GqlData data -> subData -> Html msg
What should be the signature ?!
-}
view form result sd =
    let
        txt =
            getNodeTextFromNodeType (form.node.type_ |> withDefault NodeType.Role)

        isLoading =
            result == LoadingSlowly

        isSendable =
            form.node.name /= Nothing && (form.node.mandate |> Maybe.map (\x -> x.purpose)) /= Nothing

        submitTension =
            ternary isSendable [ onClickPD2 (sd.submitMsg <| sd.submitNextMsg form False) ] []

        submitCloseTension =
            ternary isSendable [ onClickPD2 (sd.submitMsg <| sd.submitNextMsg form True) ] []
    in
    case result of
        Success res ->
            let
                link =
                    Route.Tension_Dynamic_Dynamic_Action { param1 = form.target.rootnameid, param2 = res.id } |> toHref
            in
            div [ class "box is-light" ]
                [ Fa.icon "fas fa-check fa-2x has-text-success" " "
                , if sd.activeButton == Just 0 then
                    text (txt.added ++ " ")

                  else
                    text (txt.tension_added ++ " ")
                , a
                    [ href link
                    , onClickPD (sd.closeModalMsg link)
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
                    [ nodeAboutInputView form.node False sd.changePostMsg txt.name_help txt.about_help
                    , div [ class "box has-background-grey-lighter subForm" ]
                        [ div [ class "field is-horizontal" ]
                            [ div [ class "field-label is-small has-text-grey-darker" ] [ text "Tension title" ]
                            , div [ class "field-body control" ]
                                [ input
                                    [ class "input is-small"
                                    , type_ "text"
                                    , value title
                                    , onInput <| sd.changePostMsg "title"
                                    ]
                                    []
                                ]
                            ]
                        , div [ class "field is-horizontal" ]
                            [ div [ class "field-label is-small has-text-grey-darker" ] [ text "Identifier" ]
                            , div [ class "field-body control" ]
                                [ input
                                    [ class "input is-small"
                                    , type_ "text"
                                    , value nameid
                                    , onInput <| sd.changePostMsg "nameid"
                                    ]
                                    []
                                ]
                            ]
                        , p [ class "help-label is-pulled-left", attribute "style" "margin-top: 4px !important;" ] [ text T.autoFieldMessageHelp ]
                        ]
                    , br [] []
                    , div [ class "card cardForm" ]
                        [ div [ class "card-header" ] [ div [ class "card-header-title" ] [ text T.firstLinkH ] ]
                        , div [ class "card-content" ] [ nodeLinksInputView form.node sd.changePostMsg txt.firstLink_help ]
                        ]
                    , br [] []
                    , div [ class "card cardForm" ]
                        [ div [ class "card-header" ] [ div [ class "card-header-title" ] [ text T.mandateH ] ]
                        , div [ class "card-content" ] [ nodeMandateInputView form.node sd.changePostMsg txt ]
                        ]
                    , br [] []
                    , div [ class "field" ]
                        [ div [ class "control" ]
                            [ textarea
                                [ class "textarea"
                                , rows 5
                                , placeholder T.leaveComment
                                , onInput <| sd.changePostMsg "message"
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
                            div [] []
                    , div [ class "field is-grouped is-grouped-right" ]
                        [ div [ class "control" ]
                            [ div [ class "buttons" ]
                                [ button
                                    ([ class "button"
                                     , classList
                                        [ ( "is-warning", isSendable )
                                        , ( "is-loading", isLoading && sd.activeButton == Just 0 )
                                        ]
                                     , disabled (not isSendable || isLoading)
                                     ]
                                        ++ submitCloseTension
                                    )
                                    [ text txt.close_submit ]
                                , button
                                    ([ class "button has-text-weight-semibold"
                                     , classList
                                        [ ( "is-success", isSendable )
                                        , ( "is-loading", isLoading && sd.activeButton == Just 1 )
                                        ]
                                     , disabled (not isSendable || isLoading)
                                     ]
                                        ++ submitTension
                                    )
                                    [ text txt.submit ]
                                ]
                            ]
                        ]
                    ]
                ]
