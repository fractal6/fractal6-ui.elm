module Icon exposing (icon, icon0, icon1)

import Html exposing (Html, div, i, span, text)
import Html.Attributes exposing (attribute, class, style)
import Text as T


icon : String -> Html msg
icon cls =
    i [ class cls ] []


icon0 : String -> Html msg
icon0 cls =
    span [] [ icon cls, text T.space_ ]


icon1 : String -> String -> Html msg
icon1 cls words =
    span [] [ icon cls, text (T.space_ ++ T.space_ ++ words) ]
