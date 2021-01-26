module Components.I exposing (icon, icon1, icon0)

import Html exposing (Html, div, i, span, text)
import Html.Attributes exposing (attribute, class, style)

icon : String -> Html msg
icon cls =
    i [ class cls ] []

icon0 : String -> Html msg
icon0 cls =
    span [] [ icon cls, text "\u{00A0}" ]

icon1 : String -> String -> Html msg
icon1 cls words =
    span [] [ icon cls, text ("\u{00A0} " ++ words) ]




