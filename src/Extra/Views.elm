module Extra.Views exposing (..)

import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Icon as I
import Text as T exposing (textH, textT, upH)


showMsg : String -> String -> String -> String -> String -> Html msg
showMsg id_ color icon header message =
    let
        did =
            "acc" ++ id_
    in
    div [ class "accordion arrows-right" ]
        [ input [ id did, name "accordion", type_ "radio" ] []
        , section [ class ("acc message is-info is-small  " ++ color) ]
            [ label [ class "acc-title message-header", for did ] [ I.icon1 icon (upH header) ]
            , label [ class "acc-close", for "acc-close" ] []
            , div [ class "acc-content message-body" ] [ textH message ]
            ]
        , input [ id "acc-close", name "accordion", type_ "radio" ] []
        ]
