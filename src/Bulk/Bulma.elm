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


module Bulk.Bulma exposing (dropdown)

import Assets as A
import Bulk exposing (UserState(..))
import Html exposing (Html, div, i, span, text)
import Html.Attributes exposing (attribute, autofocus, class, classList, id, style)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (withDefault)
import Text as T


dropdown : String -> String -> String -> Html msg -> msg -> Html msg -> Html msg
dropdown id_ dropdown_cls button_cls button_html msg content_html =
    span [ class ("dropdown " ++ dropdown_cls) ]
        [ span [ class "dropdown-trigger", onClick msg ]
            [ span [ attribute "aria-controls" id_ ]
                [ span
                    [ class ("button " ++ button_cls) ]
                    [ button_html, i [ class "ml-2 icon-chevron-down1", classList [ ( "icon-tiny", String.contains "is-small" button_cls ) ] ] [] ]
                ]
            ]
        , div [ id id_, class "dropdown-menu", attribute "role" "menu" ]
            [ -- The fixed position allow the dropdown to overflow the modal
              div [ class "dropdown-content has-border", style "position" "fixed" ]
                [ content_html ]
            ]
        ]
