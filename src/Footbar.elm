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


module Footbar exposing (view)

import Assets as A
import Html exposing (Html, a, div, small, span, text)
import Html.Attributes exposing (attribute, class, href, id, target, title)
import Text as T


logo_footer : Html msg
logo_footer =
    a [ href "https://fractale.co", attribute "style" "position:relative;top:5px;" ] [ A.logo0 ]


view : Html msg
view =
    div [ id "footBar", class "footer" ]
        [ div [ class "level m-0" ]
            [ small [ class "level-item is-hidden-mobile" ] [ logo_footer, text "© 2023 The Fractale Team" ]
            , div [ class "level-item" ]
                [ div [ class "columns is-mobile is-multiline is-centered contacts" ]
                    [ span [ class "column is-narrow" ] [ a [ href "https://fractale.co/about" ] [ text "About" ] ]
                    , span [ class "column is-narrow" ] [ a [ href "https://doc.fractale.co", target "_blank", title "User documentation" ] [ text "Docs" ] ]
                    , span [ class "column is-narrow" ] [ a [ href "https://github.com/fractal6", target "_blank", title "Github" ] [ text "Code" ] ]
                    , span [ class "column is-narrow" ] [ a [ href "https://matrix.to/#/#fractal6:matrix.org", target "_blank", title "Chat on matrix" ] [ text "Community" ] ]
                    , span [ class "column is-narrow" ] [ a [ href "mailto:contact@fractale.co" ] [ text T.contactUs ] ]
                    ]
                ]
            , small [ class "level-item is-hidden-mobile is-invisible" ] [ text "© 2023 The Fractale Team" ]
            ]
        , div [ class "level m-0 is-hidden-tablet" ] [ small [ class "level-item" ] [ logo_footer, text "© 2023 The Fractale Team" ] ]
        ]
