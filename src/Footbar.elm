module Footbar exposing (view)

import Assets as A
import Html exposing (Html, a, div, header, hr, i, nav, small, span, text)
import Html.Attributes as Attr exposing (attribute, class, href, id, style, target, title)


view : Html msg
view =
    div [ id "footBar", class "footer" ]
        [ div [ class "level" ]
            [ div [ class "level-left" ] [ small [] [ text "Copyright © 2020 - 2022 The Fractale Team" ] ]
            , div [ class "level-item" ]
                [ div [ class "contacts" ]
                    [ small [ class "px-2" ] [ a [ href "https://doc.fractale.co", target "_blank" ] [ text "Documentation" ] ]
                    , small [ class "px-2" ] [ text "-" ]
                    , small [ class "px-2" ] [ a [ href "https://gitlab.com/fractal6", target "_blank", title "gitlab.com" ] [ A.icon "icon-gitlab" ] ]
                    , small [ class "px-2" ] [ text "-" ]
                    , small [ class "px-2" ] [ a [ href "https://matrix.to/#/#fractal6:matrix.org", target "_blank", title "Chat on matrix" ] [ A.icon "icon-message-circle" ] ]
                    , small [ class "px-2" ] [ text "-" ]
                    , small [ class "px-2" ] [ a [ href "mailto:contact@fractale.co" ] [ text "Contact us" ] ]
                    ]
                ]
            , div [ class "level-item" ] []
            ]
        ]
