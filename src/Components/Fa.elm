module Components.Fa exposing (fa, icon, icon0)

import Html exposing (Html, div, i, span, text)
import Html.Attributes exposing (attribute, class, style)


icon : String -> String -> Html msg
icon faClass words =
    span [] [ i [ class faClass ] [], text ("\u{00A0} " ++ words) ]


icon0 : String -> String -> Html msg
icon0 faClass words =
    span [] [ i [ class faClass ] [], text ("\u{00A0}" ++ words) ]


fa : String -> Html msg
fa faClass =
    i [ class faClass ] []
