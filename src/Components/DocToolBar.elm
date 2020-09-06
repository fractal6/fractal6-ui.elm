module Components.DocToolBar exposing (view)

import Components.Doc exposing (ActionView(..))
import Components.Fa as Fa
import Components.Text as T
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Maybe exposing (withDefault)
import ModelCommon.Uri exposing (FractalBaseRoute(..), NodeFocus, uriFromUsername)
import ModelSchema exposing (..)


view : NodeFocus -> String -> Maybe ActionView -> Html msg
view focus tid actionView =
    div [ class "field has-addons" ]
        [ p
            [ class "control tooltip"
            , attribute "data-tooltip" T.view
            ]
            [ a
                [ class "button is-small is-rounded"
                , classList [ ( "is-active", actionView == Just DocView ) ]
                , href
                    (Route.Tension_Dynamic_Dynamic_Action { param1 = focus.rootnameid, param2 = tid } |> toHref)
                ]
                [ Fa.icon0 "fas fa-eye" "" ]
            ]
        , p
            [ class "control tooltip"
            , attribute "data-tooltip" T.edit
            ]
            [ a
                [ class "button is-small is-rounded"
                , classList [ ( "is-active", actionView == Just DocEdit ) ]
                , href
                    ((Route.Tension_Dynamic_Dynamic_Action { param1 = focus.rootnameid, param2 = tid } |> toHref) ++ "?v=edit")
                ]
                [ Fa.icon0 "fas fa-pen" "" ]
            ]
        , p
            [ class "control tooltip"
            , attribute "data-tooltip" T.revisions
            ]
            [ a
                [ class "button is-small is-rounded"
                , classList [ ( "is-active", actionView == Just DocVersion ) ]
                , href
                    ((Route.Tension_Dynamic_Dynamic_Action { param1 = focus.rootnameid, param2 = tid } |> toHref) ++ "?v=history")
                ]
                [ Fa.icon0 "fas fa-history" "" ]
            ]
        ]
