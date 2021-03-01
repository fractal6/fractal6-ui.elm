module Components.Navbar exposing (view)

import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, button, div, header, hr, i, nav, span, text)
import Html.Attributes as Attr exposing (attribute, class, href, id, style)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Icon as I
import Logo as Logo
import ModelCommon exposing (UserState(..))
import Text as T exposing (textH, textT, upH)


type alias Op =
    { user : UserState }


view : Op -> Html msg
view op =
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
                    , span [ class "has-text-warning", attribute "style" "padding-top: 10px; font-size: 0.7rem; margin-left: -2px;" ] [ text "Alpha" ]
                    ]
                , div
                    [ class "navbar-burger burger"
                    , attribute "data-target" "userMenu"
                    , attribute "aria-expanded" "false"
                    , attribute "aria-label" "menu"
                    , attribute "role" "button"
                    ]
                    [ span [ attribute "aria-hidden" "true" ] []
                    , span [ attribute "aria-hidden" "true" ] []
                    , span [ attribute "aria-hidden" "true" ] []
                    ]
                ]
            , div [ id "userMenu", class "navbar-menu" ]
                [ div [ class "navbar-start" ]
                    [ case op.user of
                        LoggedIn _ ->
                            a [ class "navbar-item", href (toHref Route.Top) ]
                                [ textH T.yourOrg ]

                        LoggedOut ->
                            text ""
                    , a
                        [ class "navbar-item", href (toHref Route.Explore) ]
                        [ textH T.explore ]
                    ]
                , div [ class "navbar-end" ]
                    [ helpButton op
                    , newButton op
                    , userButton op
                    ]
                ]
            ]
        ]


helpButton : Op -> Html msg
helpButton op =
    case op.user of
        LoggedIn uctx ->
            div
                [ class "navbar-item" ]
                [ div
                    [ class "navbar-link is-arrowless has-background-info button is-rounded is-small helpTrigger" ]
                    [ I.icon "icon-question" ]
                ]

        LoggedOut ->
            text ""


newButton : Op -> Html msg
newButton op =
    case op.user of
        LoggedIn uctx ->
            div
                [ class "navbar-item has-dropdown mx-2"
                , attribute "style" "align-items: center !important;"
                ]
                [ div
                    [ class "navbar-link has-background-primary button is-small"
                    , attribute "style" "padding-right: 1.65rem;"
                    ]
                    [ I.icon "icon-plus icon-bg" ]
                , div [ class "navbar-dropdown is-right" ]
                    [ a [ class "navbar-item", href (Route.toHref Route.New_Orga) ]
                        [ textH T.newOrganisation ]
                    ]
                ]

        LoggedOut ->
            text ""


userButton : Op -> Html msg
userButton op =
    case op.user of
        LoggedIn uctx ->
            div [ class "navbar-item has-dropdown" ]
                [ div
                    [ class "navbar-link"
                    , attribute "style" "padding-right: 1.85rem;"
                    ]
                    [ text uctx.username ]
                , div [ class "navbar-dropdown is-right" ]
                    [ a [ class "navbar-item", href (toHref <| Route.User_Dynamic { param1 = uctx.username }) ]
                        [ I.icon1 "icon-user" (upH T.profile) ]
                    , a [ class "navbar-item", href "#" ]
                        [ I.icon1 "icon-tool" (upH T.settings) ]

                    --, hr [ class "navbar-divider" ] []
                    --, a [ id "themeButton_port", class "navbar-item", href "#" ] [ i [ class "icon-adjust fa-fw" ] [], text "\u{00A0} Toggle dark theme" ]
                    , hr [ class "navbar-divider" ] []
                    , a [ class "navbar-item", href "/logout" ]
                        [ I.icon1 "icon-power" (upH T.signout) ]
                    ]
                ]

        LoggedOut ->
            div [ class "navbar-item" ]
                [ a [ class "button is-small is-primary has-text-weight-bold", href (toHref Route.Login) ]
                    [ textH T.signin ]
                ]
