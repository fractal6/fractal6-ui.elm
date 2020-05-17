module Components.HelperBar exposing (viewHelperBar)

import Array
import Components.Fa as Fa
import Global
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, type_)
import Html.Events exposing (custom, on, onClick, onInput, preventDefaultOn)
import Json.Decode as JD
import Maybe exposing (withDefault)
import ModelCommon exposing (NodePath, uriFromNameid)
import ModelCommon.Uri as Uri exposing (FractalBaseRoute(..))


viewHelperBar : Global.Model -> Maybe NodePath -> Html msg
viewHelperBar global maybeNodePath =
    let
        nodePath =
            maybeNodePath |> withDefault (Array.fromList [])
    in
    nav [ id "mainHeader", class "column is-full" ]
        [ div
            [ class "breadcrumb"
            , attribute "aria-label" "breadcrumbs"
            ]
            [ Fa.icon "fas fa-angle-right" ""
            , Array.indexedMap
                (\i p ->
                    if i < (Array.length nodePath - 1) then
                        li []
                            [ a
                                [ href (uriFromNameid OverviewBaseUri p.nameid) ]
                                --,onClickPD (NodeClicked p.nameid), attribute "target" "_self" ]
                                [ div [ classList [ ( "has-text-weight-bold", i == 0 ) ] ] [ text p.name ] ]
                            ]

                    else
                        li [ class "is-active has-text-weight-semibold" ]
                            [ a [ attribute "aria-current" "page", href "#" ]
                                [ div [ classList [ ( "has-text-weight-bold", i == 0 ) ] ] [ text p.name ] ]
                            ]
                )
                nodePath
                |> Array.toList
                |> ul [ attribute "style" "display: inline-flex;" ]
            ]
        , div [ class "tabs is-boxed" ]
            [ ul []
                [ li [ class "is-active" ]
                    [ a [] [ Fa.icon "fas fa-exchange-alt fa-sm" "Overview" ]
                    ]
                , li []
                    [ a [] [ Fa.icon "fas fa-exchange-alt" "Tensions" ]
                    ]
                , li []
                    [ a [] [ Fa.icon "fas fa-user" "Members" ]
                    ]
                ]
            ]
        ]
