module Components.Footbar exposing (view)

import Html exposing (Html, a, div, header, hr, i, nav, span, text)
import Html.Attributes as Attr exposing (attribute, class, href, id, style)


view : Html msg
view =
    div [ id "footBar", class "footer" ]
        [ div [ class "is-aligned-center" ]
            [ text "Contact us" ]
        ]
