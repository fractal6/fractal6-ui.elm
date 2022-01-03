module Components.Navbar exposing (view)

import Assets as A
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, button, div, header, hr, i, nav, span, text)
import Html.Attributes as Attr exposing (attribute, class, href, id, style)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import ModelCommon exposing (UserState(..))
import Text as T exposing (textH, textT, upH)


type alias Op =
    { user : UserState }


view : Op -> Html msg
view op =
    header [ id "navbarTop", class "has-navbar-fixed-top" ]
        [ nav
            [ class "navbar is-fixed-top"
            , attribute "role" "navigation"
            , attribute "aria-label" "main navigation"
            ]
            [ div [ class "navbar-brand" ]
                [ a [ class "navbar-item", href "/" ]
                    --[ img [ alt "Fractal", attribute "height" "28", attribute "width" "112", src "https://bulma.io/images/bulma-logo.png" ] [] ]
                    [ A.logo0 "white"
                    , span [ class "has-text-warning", attribute "style" "padding-top: 10px; font-size: 0.65rem; margin-left: -2px;" ] [ text "Alpha" ]
                    ]
                , div
                    [ class "burger navbar-burger"
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
                    [ notificationButton op
                    , helpButton op
                    , newButton op
                    , userButton op
                    ]
                ]
            ]
        ]


notificationButton : Op -> Html msg
notificationButton op =
    case op.user of
        LoggedIn _ ->
            a
                [ class "navbar-item", href (Route.toHref Route.Notifications) ]
                [ div
                    [ class "navbar-link is-arrowless is-rounded is-small notifTrigger" ]
                    [ A.icon "icon-bg icon-bell" ]
                ]

        LoggedOut ->
            text ""


helpButton : Op -> Html msg
helpButton op =
    case op.user of
        LoggedIn _ ->
            div
                [ class "navbar-item" ]
                [ div
                    [ class "navbar-link is-arrowless has-background-navbar button is-rounded is-small helpTrigger" ]
                    [ A.icon "icon-question" ]
                ]

        LoggedOut ->
            text ""


newButton : Op -> Html msg
newButton op =
    case op.user of
        LoggedIn _ ->
            div
                [ class "navbar-item has-dropdown" ]
                [ div
                    [ class "navbar-link is-small"
                    , attribute "style" "padding-right: 1.65rem;"
                    ]
                    [ A.icon "icon-plus icon-bg" ]
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
                        [ A.icon1 "icon-user" (upH T.profile) ]
                    , a [ class "navbar-item", href "#" ]
                        [ A.icon1 "icon-tool" (upH T.settings) ]

                    --, hr [ class "navbar-divider" ] []
                    --, a [ id "themeButton_port", class "navbar-item", href "#" ] [ i [ class "icon-adjust fa-fw" ] [], text (T.space_ ++ " Toggle dark theme") ]
                    , hr [ class "navbar-divider" ] []
                    , a [ class "navbar-item", href "/logout" ]
                        [ A.icon1 "icon-power" (upH T.signout) ]
                    ]
                ]

        LoggedOut ->
            div [ class "navbar-item" ]
                [ a [ class "button is-small is-primary has-text-weight-bold", href (toHref Route.Login) ]
                    [ textH T.signin ]
                ]
