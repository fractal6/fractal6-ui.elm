{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2024 Fractale Co

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


module Bulk.Bulma exposing (dropdown, dropdownLight)

import Assets as A
import Bulk exposing (UserState(..))
import Html exposing (Html, div, i, span, text)
import Html.Attributes exposing (attribute, autofocus, class, classList, id, style)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (withDefault)
import Text as T


type alias DropdownData msg =
    { dropdown_id : String
    , isOpen : Bool
    , dropdown_cls : String
    , button_cls : String -- ignore for button light
    , button_html : Html msg
    , menu_cls : String
    , content_cls : String
    , content_html : Html msg
    , msg : msg
    }


{-| Dropdown components - Snippets:

        , B.dropdown
            { dropdown_id = "target-menu"
            , isOpen = model.isTargetOpen
            , dropdown_cls = ""
            , button_cls = "is-small is-rounded has-border is-wrapped is-inline-block"
            , button_html = span [] [ text form.target.name, span [ class "ml-2 icon-chevron-down1" ] [] ]
            , msg = OnTargetClick
            , menu_cls = "is-right is-left-mobile"
            , content_cls = "has-border p-0"
            , content_html = viewSelectorTree (OnChangeTensionTarget tree_data) [ model.nodeDoc.form.target.nameid ] tree_data
            }

        B.dropdown "object-id" isOpen
           ("mr-2 " ++ ternary model.isOpenTargetFilter "is-active" "")
           "is-small"
           (A.icon1 "icon-edit" "click me")
           OnToggleTargetFilter
           ""
           (div []
                [ div [ class "dropdown-item" ] [ A.icon1 "icon-edit-2" T.edit ]
                , hr [ class "dropdown-divider" ] []
                , div [ class "dropdown-item" ] [ A.icon1 "icon-plus" T.addTensionColumn ]
                ])

-}
dropdown : DropdownData msg -> Html msg
dropdown op =
    span [ class ("dropdown elm " ++ op.dropdown_cls), classList [ ( "is-active", op.isOpen ) ] ]
        [ span [ class "dropdown-trigger", onClick op.msg ]
            [ span [ attribute "aria-controls" op.dropdown_id ]
                [ span [ class ("button " ++ op.button_cls) ]
                    [ op.button_html, i [ class "ml-2 icon-chevron-down1", classList [ ( "icon-tiny", String.contains "is-small" op.button_cls ) ] ] [] ]
                ]
            ]
        , div [ id op.dropdown_id, class ("dropdown-menu " ++ op.menu_cls), attribute "role" "menu" ]
            [ div [ class ("dropdown-content " ++ op.content_cls), style "max-height" "420px" ]
                [ if op.isOpen then
                    op.content_html

                  else
                    text ""
                ]
            ]
        ]


dropdownLight : DropdownData msg -> Html msg
dropdownLight op =
    span [ class ("dropdown elm " ++ op.dropdown_cls), classList [ ( "is-active", op.isOpen ) ] ]
        [ span [ class "dropdown-trigger", onClick op.msg ] [ span [ attribute "aria-controls" op.dropdown_id ] [ op.button_html ] ]
        , div [ id op.dropdown_id, class ("dropdown-menu " ++ op.menu_cls), attribute "role" "menu" ]
            [ div [ class ("dropdown-content " ++ op.content_cls), style "max-height" "420px" ]
                -- The fixed position allow the dropdown to overflow the modal
                --, style "position" "fixed" ]
                [ if op.isOpen then
                    op.content_html

                  else
                    text ""
                ]
            ]
        ]
