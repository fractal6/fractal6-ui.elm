module Components.Footbar exposing (view)

import Components.Fa as Fa
import Html exposing (Html, a, div, header, hr, i, nav, span, text)
import Html.Attributes as Attr exposing (attribute, class, href, id, style, target)


view : Html msg
view =
    div [ id "footBar", class "footer" ]
        [ div [ class "columns is-centered" ]
            [ div [ class "column is-2" ]
                [ div [ class "contacts" ]
                    [ span [] [ text "Contact us" ]
                    , span [] [ text "-" ]
                    , span [] [ a [ href "https://gitlab.com/fractal6", target "_blank" ] [ Fa.icon0 "fab fa-gitlab" "" ] ]

                    --, span [] [ a [ href "https://github.com/fractal6", target "_blank" ] [ Fa.icon0 "fab fa-github" "" ] ]
                    ]
                ]
            ]
        ]
