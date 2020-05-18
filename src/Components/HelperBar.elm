module Components.HelperBar exposing (view, viewHelperBar)

import Array
import Components.Fa as Fa
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, type_)
import Html.Events exposing (custom, on, onClick, onInput, preventDefaultOn)
import Json.Decode as JD
import Maybe exposing (withDefault)
import ModelCommon exposing (NodePath, uriFromNameid)
import ModelCommon.Uri as Uri exposing (FractalBaseRoute(..))


view : FractalBaseRoute -> Maybe NodePath -> Html msg
view baseUri maybeNodePath =
    div [ id "mainHeader", class "columns is-centered" ]
        [ div [ class "column is-10" ]
            [ viewHelperBar baseUri maybeNodePath ]
        ]


viewHelperBar : FractalBaseRoute -> Maybe NodePath -> Html msg
viewHelperBar baseUri maybePath =
    nav [ id "mainHeader" ]
        [ viewPath baseUri maybePath
        , div [ class "tabs is-boxed" ]
            [ ul []
                [ li [ class "is-active" ]
                    [ a [] [ Fa.icon "fas fa-circle" "Overview" ] ]
                , li []
                    [ a [] [ Fa.icon "fas fa-exchange-alt" "Tensions" ] ]
                , li []
                    [ a [] [ Fa.icon "fas fa-user" "Members" ] ]
                ]
            ]
        ]


viewPath : FractalBaseRoute -> Maybe NodePath -> Html msg
viewPath baseUri maybePath =
    let
        path =
            maybePath |> withDefault (Array.fromList [])
    in
    div
        [ class "breadcrumb"
        , attribute "aria-label" "breadcrumbs"
        ]
        [ Fa.icon "fas fa-angle-right" ""
        , Array.indexedMap
            (\i p ->
                if i < (Array.length path - 1) then
                    li []
                        [ a
                            [ href (uriFromNameid baseUri p.nameid) ]
                            --,onClickPD (NodeClicked p.nameid), attribute "target" "_self" ]
                            [ div [ classList [ ( "has-text-weight-bold", i == 0 ) ] ] [ text p.name ] ]
                        ]

                else
                    li [ class "is-active has-text-weight-semibold" ]
                        [ a [ href (uriFromNameid baseUri p.nameid) ]
                            [ div [ classList [ ( "has-text-weight-bold", i == 0 ) ] ] [ text p.name ] ]
                        ]
            )
            path
            |> Array.toList
            |> ul [ attribute "style" "display: inline-flex;" ]
        ]
