{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2026 Fractale Co

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


module Utils.Color exposing (colorAttr, colorToTextColor)

import Html
import Html.Attributes exposing (attribute)


{-| Get Color style from string color
-}
colorAttr : String -> Html.Attribute msg
colorAttr color =
    let
        c =
            if String.startsWith "#" color then
                color

            else
                "var(--" ++ color ++ ")"
    in
    attribute "style"
        ("background-color:"
            ++ c
            ++ "; color:"
            ++ colorToTextColor color
            ++ ";"
        )


{-| Adjust the text color for dark background color
-}
colorToTextColor : String -> String
colorToTextColor color =
    if
        List.member (String.toUpper color)
            [ "#7FDBFF"
            , "#39CCCC"
            , "#01FF70"
            , "#FFDC00"
            , "#AAAAAA"
            , "#DDDDDD"
            ]
    then
        "#000"

    else if String.startsWith "white" color then
        "#000"

    else
        "#fff"
