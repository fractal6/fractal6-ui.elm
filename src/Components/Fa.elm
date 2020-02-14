module Components.Fa exposing (icon, icon1, icon_, icon_1)

import Html exposing (Html, div, i, span, text)
import Html.Attributes exposing (attribute, class, style)


icon : String -> String -> Html msg
icon faClass words =
    div [] [ i [ class faClass ] [], text ("\u{00A0}" ++ words) ]


icon_ : String -> String -> Html msg
icon_ faClass words =
    span [ attribute "style" "padding-left: 2pt;" ] [ i [ class faClass ] [], text ("\u{00A0}" ++ words) ]


icon1 : String -> String -> Html msg
icon1 faClass words =
    span [] [ i [ class faClass ] [], text ("\u{00A0} " ++ words) ]


icon_1 : String -> String -> Html msg
icon_1 faClass words =
    span [ attribute "style" "padding-left: 2pt;" ] [ i [ class faClass ] [], text ("\u{00A0} " ++ words) ]
