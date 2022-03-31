module Footbar exposing (view)

import Assets as A
import Html exposing (Html, a, div, header, hr, i, nav, small, span, text)
import Html.Attributes as Attr exposing (attribute, class, href, id, style, target, title)


view : Html msg
view =
    div [ id "footBar", class "footer" ]
        [ div [ class "columns is-centered" ]
            [ div [ class "column is-4" ] [ small [] [ text "Copyright © 2020 - 2022 The Fractale Team" ] ]
            , div [ class "column is-4" ]
                [ div [ class "contacts" ]
                    [ span [] [ a [ href "mailto:contact@fractale.co" ] [ text "Contact us" ] ]
                    , span [] [ text "-" ]
                    , span [] [ a [ href "https://doc.fractale.co", target "_blank" ] [ text "Documentation" ] ]
                    , span [] [ text "-" ]
                    , span [] [ a [ href "https://gitlab.com/fractal6", target "_blank", title "gitlab.com" ] [ A.icon "icon-gitlab" ] ]
                    , span [] [ text "-" ]
                    , span [] [ a [ href "https://matrix.to/#/#fractal6:matrix.org", target "_blank", title "Chat on matrix" ] [ A.icon "icon-message-circle" ] ]

                    --, span [] [ a [ href "https://github.com/fractal6", target "_blank" ] [ A.icon "icon-github"  ] ]
                    ]
                ]
            , div [ class "column is-4" ] []
            ]
        ]
