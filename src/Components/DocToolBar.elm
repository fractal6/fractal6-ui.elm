module Components.DocToolBar exposing (view)

import Components.Doc exposing (ActionView(..))
import Components.I as I
import Extra exposing (ternary)
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Maybe exposing (withDefault)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), NodeFocus, uriFromUsername)
import ModelSchema exposing (..)
import Text as T exposing (textH, textT, upH)


type alias Op =
    { focus : NodeFocus
    , tid : String
    , actionView : Maybe ActionView
    }


view : Op -> Html msg
view op =
    let
        iconOpts =
            ternary (op.actionView == Nothing) "icon-xs" ""
    in
    div [ class "field has-addons docToolbar" ]
        [ p
            [ class "control tooltip"
            , attribute "data-tooltip" (upH T.view)
            ]
            [ a
                [ class "button is-small is-rounded"
                , classList [ ( "is-active", op.actionView == Just DocView ) ]
                , href
                    (Route.Tension_Dynamic_Dynamic_Action { param1 = op.focus.rootnameid, param2 = op.tid } |> toHref)
                ]
                [ I.icon ("icon-eye " ++ iconOpts) ]
            ]
        , p
            [ class "control tooltip"
            , attribute "data-tooltip" (upH T.edit)
            ]
            [ a
                [ class "button is-small is-rounded"
                , classList [ ( "is-active", op.actionView == Just DocEdit ) ]
                , href
                    ((Route.Tension_Dynamic_Dynamic_Action { param1 = op.focus.rootnameid, param2 = op.tid } |> toHref) ++ "?v=edit")
                ]
                [ I.icon ("icon-pen " ++ iconOpts) ]
            ]
        , p
            [ class "control tooltip"
            , attribute "data-tooltip" (upH T.revisions)
            ]
            [ a
                [ class "button is-small is-rounded"
                , classList [ ( "is-active", op.actionView == Just DocVersion ) ]
                , href
                    ((Route.Tension_Dynamic_Dynamic_Action { param1 = op.focus.rootnameid, param2 = op.tid } |> toHref) ++ "?v=history")
                ]
                [ I.icon ("icon-history " ++ iconOpts) ]
            ]
        ]
