{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2025 Fractale Co

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


module Assets.Logo exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Svg exposing (circle, ellipse, g, node, path, polygon, svg)
import Svg.Attributes exposing (class, cx, cy, d, height, id, points, r, rx, ry, transform, viewBox, width, x, y)


i18n : Html msg
i18n =
    svg
        [ Attr.attribute "xmlns" "http://www.w3.org/2000/svg"
        , Attr.style "fill" "var(--text)"
        , viewBox "0 0 24 24"
        , height "22"
        , width "22"
        ]
        [ Svg.path [ d "m12.87 15.07-2.54-2.51.03-.03A17.52 17.52 0 0 0 14.07 6H17V4h-7V2H8v2H1v2h11.17C11.5 7.92 10.44 9.75 9 11.35 8.07 10.32 7.3 9.19 6.69 8h-2c.73 1.63 1.73 3.17 2.98 4.56l-5.09 5.02L4 19l5-5 3.11 3.11.76-2.04M18.5 10h-2L12 22h2l1.12-3h4.75L21 22h2l-4.5-12m-2.62 7 1.62-4.33L19.12 17h-3.24Z" ] [] ]


focusCircle : Html msg
focusCircle =
    svg [ height "32px", width "32px", Attr.attribute "preserveAspectRatio" "xMidYMid meet", viewBox "0 0 102 102", Attr.attribute "xmlns" "http://www.w3.org/2000/svg", Attr.attribute "xmlns:xlink" "http://www.w3.org/1999/xlink" ]
        [ node "rect"
            [ height "100", width "100", x "0", y "0", Attr.attribute "style" "fill:none;stroke:none" ]
            []
        , node
            "circle"
            [ cx "51", cy "51", r "42", Attr.attribute "style" "fill:#e8e8e8;stroke:#4a79ac;stroke-width:8px" ]
            []
        ]


circles : Html msg
circles =
    svg [ height "32px", width "32px", Attr.attribute "preserveAspectRatio" "xMidYMid meet", viewBox "0 0 102 102", Attr.attribute "xmlns" "http://www.w3.org/2000/svg", Attr.attribute "xmlns:xlink" "http://www.w3.org/1999/xlink" ]
        [ node "rect"
            [ height "100", width "100", x "0", y "0", Attr.attribute "style" "fill:none;stroke:none" ]
            []
        , node
            "circle"
            [ cx "51", cy "51", r "50", Attr.attribute "style" "fill:#b8b8b8;stroke:black;stroke-width:1px" ]
            []
        , node "circle"
            [ cx "66", cy "51", r "30", Attr.attribute "style" "fill:#919191;stroke:black;stroke-width:1px" ]
            []
        , node "circle"
            [ cx "75", cy "51", r "15", Attr.attribute "style" "fill:#747474;stroke:black;stroke-width:1px" ]
            []
        ]


logo : Html msg
logo =
    svg
        [ id "logo"
        , class "logo-f6"
        , viewBox "0 0 100 100"
        , height "32"
        , width "32"
        ]
        [ ellipse
            [ cx "50"
            , cy "50"
            , rx "42"
            , ry "35"
            , Attr.attribute "style" "stroke:white;stroke-width:3"
            , Attr.attribute "fill" "rgba(124,240,10,0)"
            ]
            []
        , polygon
            [ points "10,95, 90,95, 50,30"
            , Attr.attribute "style" "stroke:white;stroke-width:3"
            , Attr.attribute "fill" "rgba(124,240,10,0)"
            ]
            []
        , polygon
            [ points "10,5, 90,5, 50,70"
            , Attr.attribute "style" "stroke:white;stroke-width:3"
            , Attr.attribute "fill" "rgba(124,240,10,0)"
            ]
            []
        ]


logo_fractal_fish : String -> String -> Html msg
logo_fractal_fish h w =
    --Lazy.lazy2 Logo.logo_fractal "25" "25"
    svg
        [ class "logo-f6"
        , viewBox "0 0 231.47 231.9"
        , height h
        , width w
        ]
        [ g
            []
            [ path
                [ d "M213.92,151.04c-16.21,15.89-38.46,24.42-60.7,26.36-20.15,2.36-41.46-3.92-56.95-16.94-.15-.12-.29-.24-.43-.36-1.56,23.25-9.57,45.09-23.05,63.48,13.34,5.36,27.91,8.31,43.17,8.31,60.68,0,110.47-46.62,115.52-105.99-4.23,9.36-10.21,18.01-17.55,25.13Z"
                ]
                []
            , path
                [ d "M66.03,71.03c1.31,1.48,2.57,2.99,3.8,4.53,3.33-9.58,8.93-18.55,17.09-26.21,12.33-11.62,28.14-18.32,44.87-19.37,49.41-3.93,86.14,52.85,52.38,92.65-4.14,5.02-10.07,9.01-16.05,11.2-17.93,7.14-40.98,3.52-52.3-12.97-4.87-5.87-7.7-14.24-6.57-21.9,1.41-10.85,9.51-22.42,20.82-24.17,7.49-1.33,16.1,2.48,19.29,9.53,3.34,6.66,1,14.95-5.09,18.81-1.29,1.01-3.19.74-4.1-.63-.81-1.21-.56-2.82.54-3.74,1.7-1.41,2.84-3.13,3.21-5.02.86-3.93-1.8-8.26-5.57-9.68-7.01-2.71-13.56,2.43-16.51,8.7-6.47,12.69,3.24,25.08,15.59,28.44,7.69,2.02,16.13.82,23.3-2.58,5.01-2.23,8.49-5.88,11.27-10.47,5.83-9.16,6.49-20.82,2.04-30.7-5.39-12.42-17.82-21.76-31.41-22.9-19.25-1.51-40.01,8.86-48.5,26.51-4.91,10.12-5.14,22.23-1.82,32.9,6.8,21.64,27.25,39.34,50.4,39.85,29.05.64,58.64-12.59,70.76-40.23,9.79-21.17,7.09-46.75-4.81-66.55-.88-1.47-1.82-2.91-2.8-4.32C184.59,16.66,152.22,0,115.95,0S48.1,16.25,26.83,41.78c14.87,6.89,28.19,16.76,39.2,29.25Z"
                ]
                []
            , path
                [ d "M48.77,86.24c-9.69-11-21.63-19.49-35.03-25.09C4.98,77.47,0,96.13,0,115.95c0,40.46,20.72,76.07,52.13,96.81,12.43-15.77,19.71-34.94,20.84-55.41,1.46-26.49-7.13-51.74-24.2-71.12Z"
                ]
                []
            ]
        ]


logo_fractal : String -> String -> Html msg
logo_fractal h w =
    svg
        [ viewBox "0 0 138.07764 139.45669"
        , height h
        , width w
        , class "logo-f6"
        ]
        [ g
            [ transform "translate(-45.284842,-45.848458)" ]
            [ path
                [ class "cls-1"
                , d "m 176.56,127.31 c -0.5,0.86 -1.13,1.91 -1.89,3.09 -15.16,24.45 -44.95,38.66 -73.68,27.72 C 80.8,150.8 66.96,132.44 66.42,110.9 c -0.23,-7.56 1.23,-15.3 4.89,-22.05 7.92,-14.58 24.36,-24.43 41.13,-23.21 16.67,1.32 31.5,13.24 31.71,30.74 0.25,12.84 -8.3,26.84 -21.92,28.24 -6.65,0.75 -13.45,-1.57 -17.92,-6.65 -3.3,-3.68 -4.88,-9.18 -3.28,-13.99 1.57,-5.86 7.73,-10.37 13.82,-8.93 0.84,0.22 1.64,0.58 2.34,1.05 2.19,1.44 4.36,5.38 1.09,6.57 -0.77,0.28 -1.74,-0.17 -2.46,-0.37 -0.77,-0.28 -1.22,-0.32 -1.9,-0.29 -2.16,-0.08 -4.49,1.82 -4.85,4.02 -1.04,3.78 2.45,7.22 6,8.31 3.46,1.15 7.51,0.57 10.54,-1.52 4.9,-3.43 7.26,-9.83 7.03,-15.77 -0.43,-8.88 -6.72,-14.89 -14.99,-17.09 -10.97,-3.01 -22.71,2.58 -29.38,11.3 -8.05,10.06 -7.05,25.1 -0.7,35.82 4.13,7.12 11.23,12.24 18.99,14.82 20.99,7.48 42.64,-4.62 52.27,-23.72 10.32,-19.78 6.43,-42.08 -9.6,-57.42 -5.64,-5.8 -12.69,-10.03 -20.46,-12.32 -16.09,-5.03 -33.9,-2.55 -48.67,5.81 -0.9,0.5 -1.8,1.01 -2.68,1.56 -8.86,5.73 -16.57,13.54 -22.31,23.24 -19.8,33.46 -8.73,76.63 24.72,96.43 33.45,19.8 76.62,8.73 96.42,-24.72 0.36,-0.61 0.71,-1.23 1.05,-1.85 3.87,-7.48 6.38,-15.06 6.03,-20.91 -0.29,-4.96 -4.71,-4.31 -6.8,-0.67 v 0 z"
                , id "path1"
                ]
                []
            ]
        ]
