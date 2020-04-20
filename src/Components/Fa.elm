module Components.Fa exposing (icon, icon0, icon_)

import Html exposing (Html, div, i, span, text)
import Html.Attributes exposing (attribute, class, style)


icon : String -> String -> Html msg
icon faClass words =
    span [] [ i [ class faClass ] [], text ("\u{00A0} " ++ words) ]


icon0 : String -> String -> Html msg
icon0 faClass words =
    span [] [ i [ class faClass ] [], text ("\u{00A0}" ++ words) ]


icon_ : String -> String -> Html msg
icon_ faClass words =
    span [ attribute "style" "padding-left: 2pt;" ] [ i [ class faClass ] [], text ("\u{00A0}" ++ words) ]
