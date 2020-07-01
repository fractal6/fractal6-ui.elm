module Components.NotFound exposing (viewNotFound)

import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (class, href)


viewNotFound : Html msg
viewNotFound =
    div [ class "section" ]
        [ h1 [ class "title" ] [ text "Sorry, Page Not found" ]
        ]
