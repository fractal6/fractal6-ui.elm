{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2023 Fractale Co

   This file is part of Fractale.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
-}


module Extra.Views exposing (..)

import Assets as A
import Extra exposing (ternary, textH, upH)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Markdown exposing (renderMarkdown)
import Text as T


showMsg : String -> String -> String -> String -> String -> Html msg
showMsg id_ cls icon header message =
    if message == "" then
        div [ class ("notification p-4 m-0 mb-2 is-size-7 " ++ cls) ] [ renderMarkdown "" (" <i class=\"" ++ icon ++ "\"></i> <i class=\"is-space\"></i> " ++ header) ]

    else
        let
            did =
                "acc" ++ id_

            mkdCls =
                if String.contains "is-light" cls then
                    "is-light"

                else if String.contains "is-dark" cls then
                    "is-dark"

                else
                    ""
        in
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
                    [ renderMarkdown ("message-body " ++ mkdCls) message ]
                ]
            , input [ id "acc-close", name "accordion", type_ "radio" ] []
            ]
