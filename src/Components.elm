module Components exposing (layout)

import Browser exposing (Document)
import Components.Logo as Logo
import Generated.Route as Route exposing (Route)
import Html exposing (Html, a, div, header, hr, i, nav, span, text)
import Html.Attributes as Attr exposing (attribute, class, href, id, style)


layout : { page : Document msg } -> Document msg
layout { page } =
    { title = page.title
    , body =
        [ div [ id "app" ]
            [ viewHeader -- page.route / current route ?
            , div [ id "body", class "section is-paddingless" ] page.body
            ]
        ]
    }


viewHeader : Html msg
viewHeader =
    header [ id "navbarTop", class "has-navbar-fixed-top" ]
        [ nav
            [ class "navbar has-shadow is-fixed-top"
            , attribute "role" "navigation"
            , attribute "aria-label" "main navigation"
            ]
            [ div [ class "navbar-brand" ]
                [ a [ class "navbar-item", href "/" ]
                    --[ img [ alt "Fractal", attribute "height" "28", attribute "width" "112", src "https://bulma.io/images/bulma-logo.png" ] [] ]
                    [ Logo.logo_fractal

                    --, span [ class "is-size-5" ] [ text "Fractal6" ]
                    ]
                , div
                    [ class "navbar-burger burger"
                    , attribute "data-target" "navMenu"
                    , attribute "aria-expanded" "false"
                    , attribute "aria-label" "menu"
                    , attribute "role" "button"
                    ]
                    [ span [ attribute "aria-hidden" "true" ] []
                    , span [ attribute "aria-hidden" "true" ] []
                    , span [ attribute "aria-hidden" "true" ] []
                    ]
                ]
            , div [ id "navMenu", class "navbar-menu" ]
                [ div [ class "navbar-start" ]
                    [ viewLink "Help" Route.Top
                    , viewLink "Explore" Route.NotFound
                    ]
                , div [ class "navbar-end" ]
                    [ a [ class "navbar-item" ]
                        [ div [ class "button is-warning is-small" ]
                            [ div [ class "content" ]
                                [ text "Report an issue"
                                ]
                            ]
                        ]
                    , div [ class "navbar-item has-dropdown" ]
                        [ div [ class "navbar-link" ] [ text "Green" ]
                        , div [ class "navbar-dropdown is-right" ]
                            [ a [ class "navbar-item" ] [ text "Profile" ]
                            , a [ class "navbar-item" ] [ text "Settings" ]
                            , hr [ class "navbar-divider" ] []
                            , a [ id "themeButton_port", class "navbar-item", href "#" ] [ i [ class "fas fa-adjust fa-fw" ] [], text "\u{00A0} Toggle dark theme" ]
                            , hr [ class "navbar-divider" ] []
                            , a [ class "navbar-item" ] [ text "Sign Out" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewLink link route =
    a
        [ class "navbar-item"
        , href (Route.toHref route)
        ]
        [ text link ]
