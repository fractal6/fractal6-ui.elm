module Extra.Views exposing (..)

import Assets as A
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Markdown exposing (renderMarkdown)
import Text as T exposing (textH, textT, upH)


showMsg : String -> String -> String -> String -> String -> Html msg
showMsg id_ cls icon header message =
    let
        did =
            "acc" ++ id_
    in
    if message == "" then
        div [ class ("notification p-4 m-0 mb-2 is-size-7 " ++ cls) ] [ A.icon1 icon (upH header) ]

    else
        div [ class "accordion arrows-right" ]
            [ input [ id did, name "accordion", type_ "radio" ] []
            , section [ class ("acc message is-small " ++ cls) ]
                [ label
                    [ class "acc-title message-header"
                    , attribute "title" T.clickMe
                    , for did
                    ]
                    [ A.icon1 icon (upH header) ]
                , label [ class "acc-close", for "acc-close" ] []
                , div [ class "acc-content " ]
                    [ renderMarkdown "message-body" message
                    ]
                ]
            , input [ id "acc-close", name "accordion", type_ "radio" ] []
            ]
