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


module Assets.Shapes exposing (quarterDisc, quarterPath)

{-| Resolution-independent shapes, drawn in a unit `viewBox` so CSS sizes them.
Used as card surfaces inside the graphpack focused circle (`Org.Overview.viewCanvasCards`).
-}

import Html exposing (Html)
import Html.Attributes exposing (attribute)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, transform, viewBox)


{-| Quarter `i` (0..3, clockwise from top-left) of the circle inscribed in the 2x2 box of cells,
inset by `gap` on all sides and with `corner` fillets, both as fractions of the cell side.
-}
quarterDisc : Float -> Float -> Int -> Html msg
quarterDisc gap corner i =
    let
        angle =
            List.drop i [ 0, 90, 270, 180 ] |> List.head |> Maybe.withDefault 0
    in
    svg [ viewBox "0 0 1 1", attribute "aria-hidden" "true" ]
        [ path [ d (quarterPath gap corner), transform ("rotate(" ++ String.fromInt angle ++ " 0.5 0.5)") ] [] ]


{-| Top-left quarter: arc of radius `1 - gap` around (1,1), straight edges at `1 - gap/2`,
`corner` fillets tangent to both edges and to the arc.
-}
quarterPath : Float -> Float -> String
quarterPath gap corner =
    let
        e =
            1 - gap / 2

        rho =
            1 - gap

        -- Fillet center: `corner` from the vertical edge, `rho - corner` from the circle center.
        fx =
            e - corner

        fy =
            1 - sqrt ((rho - corner) ^ 2 - (1 - fx) ^ 2)

        -- Fillet/arc tangent point, on the ray from the circle center through the fillet center.
        k =
            rho / (rho - corner)

        ax =
            1 - (1 - fx) * k

        ay =
            1 - (1 - fy) * k

        pt x y =
            String.fromFloat x ++ " " ++ String.fromFloat y

        arc radius x y =
            "A " ++ pt radius radius ++ " 0 0 1 " ++ pt x y
    in
    String.join " "
        [ "M", pt e fy, "L", pt e (e - corner), arc corner (e - corner) e, "L", pt fy e, arc corner ay ax, arc rho ax ay, arc corner e fy, "Z" ]
