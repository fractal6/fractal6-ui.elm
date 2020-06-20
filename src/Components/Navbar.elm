module Components.Navbar exposing (viewNavbar)

import Components.Logo as Logo
import Generated.Route as Route exposing (toHref)
import Html exposing (Html, a, div, header, hr, i, nav, span, text)
import Html.Attributes as Attr exposing (attribute, class, href, id, style)
import ModelCommon exposing (UserState(..))


viewNavbar : UserState -> Html msg
viewNavbar user =
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
                    [ viewLink "navbar-item" Route.Top "Help"
                    , viewLink "navbar-item" Route.NotFound "Explore"
                    ]
                , div [ class "navbar-end" ]
                    [ a [ class "navbar-item" ]
                        [ div [ class "button is-warning is-small" ]
                            [ div [ class "content" ]
                                [ text "Contact" ]
                            ]
                        ]
                    , userButton user
                    ]
                ]
            ]
        ]


userButton : UserState -> Html msg
userButton user =
    case user of
        LoggedIn uctx ->
            div [ class "navbar-item has-dropdown" ]
                [ div [ class "navbar-link" ] [ text uctx.username ]
                , div [ class "navbar-dropdown is-right" ]
                    [ viewLink "navbar-item" (Route.User_Dynamic { param1 = uctx.username }) "Profile"
                    , a [ class "navbar-item", href "#" ] [ text "Settings" ]

                    --, hr [ class "navbar-divider" ] []
                    --, a [ id "themeButton_port", class "navbar-item", href "#" ] [ i [ class "fas fa-adjust fa-fw" ] [], text "\u{00A0} Toggle dark theme" ]
                    , hr [ class "navbar-divider" ] []
                    , a [ class "navbar-item", href "/logout" ] [ text "Sign Out" ]
                    ]
                ]

        LoggedOut ->
            div [ class "navbar-item" ]
                [ viewLink "button is-small is-primary has-text-weight-bold" Route.Login "Login" ]


viewLink : String -> Route.Route -> String -> Html msg
viewLink classes route txt =
    a
        [ class classes
        , href (toHref route)
        ]
        [ text txt ]
