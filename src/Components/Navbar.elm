module Components.Navbar exposing (view)

import Assets as A
import Generated.Route as Route exposing (Route(..), fromUrl, toHref)
import Html exposing (Html, a, button, div, header, hr, i, nav, span, text)
import Html.Attributes as Attr exposing (attribute, class, href, id, style, target, title)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Maybe exposing (withDefault)
import ModelCommon exposing (UserState(..))
import ModelCommon.Codecs exposing (FractalBaseRoute(..), urlToFractalRoute)
import Text as T exposing (textH, textT, upH)
import Url exposing (Url)


type alias Op msg =
    { user : UserState
    , url : Url
    , replaceUrl : String -> msg
    }


view : Op msg -> Html msg
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

                    --, span [ class "has-text-orange", attribute "style" "padding-top: 10px; font-size: 0.65rem; margin-left: -2px;" ] [ text "Alpha" ]
                    , span [ class "has-text-orange", attribute "style" "position:relative; top: -10px; font-size: 0.65rem;" ] [ text "Beta" ]
                    ]
                , A.burger "userMenu"
                ]
            , div [ id "userMenu", class "navbar-menu" ]
                [ div [ class "navbar-start" ] <|
                    (case op.user of
                        LoggedIn _ ->
                            let
                                hasLeftMenuBurger =
                                    urlToFractalRoute op.url
                                        |> Maybe.map
                                            (\url ->
                                                List.member url [ OverviewBaseUri, TensionsBaseUri, TensionBaseUri, MembersBaseUri, SettingsBaseUri ]
                                            )
                                        |> withDefault False
                            in
                            [ if hasLeftMenuBurger then
                                div [ class "navbar-item button-light pr-0 is-hidden-touch menuLeftTrigger" ] [ A.icon "icon-menu" ]

                              else
                                text ""
                            , a [ class "navbar-item", href (toHref Route.Top) ] [ textH T.yourOrg ]
                            ]

                        LoggedOut ->
                            []
                    )
                        ++ [ a [ class "navbar-item", href (toHref Route.Explore) ] [ textH T.explore ] ]
                , div [ class "navbar-end" ]
                    [ notificationButton op
                    , helpButton op
                    , newButton op
                    , userButton op
                    ]
                ]
            ]
        ]


notificationButton : Op msg -> Html msg
notificationButton op =
    case op.user of
        LoggedIn _ ->
            a
                [ class "navbar-item", href (Route.toHref Route.Notifications), title (upH T.notifications) ]
                [ div
                    [ class "navbar-link is-arrowless is-rounded is-small notifTrigger" ]
                    [ A.icon "icon-bg icon-bell" ]
                ]

        LoggedOut ->
            text ""


helpButton : Op msg -> Html msg
helpButton op =
    case op.user of
        LoggedIn _ ->
            div
                [ class "navbar-item" ]
                [ div
                    [ class "navbar-link is-arrowless has-background-navbar button is-rounded is-small helpTrigger"
                    , title "Help and feedback"
                    ]
                    [ A.icon "icon-question" ]
                ]

        LoggedOut ->
            text ""


newButton : Op msg -> Html msg
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


userButton : Op msg -> Html msg
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
                    [ a [ class "navbar-item", href (toHref <| Route.Dynamic { param1 = uctx.username }) ]
                        [ A.icon1 "icon-user" (upH T.profile) ]
                    , a [ class "navbar-item", href (toHref <| Route.Dynamic_Settings { param1 = uctx.username }) ]
                        [ A.icon1 "icon-tool" (upH T.settings) ]
                    , span [ id "themeTrigger", class "navbar-item is-w" ]
                        [ A.icon1 "icon-moon" "Toggle dark mode" ]
                    , hr [ class "navbar-divider" ] []

                    -- Prevout logout to be log in the browser history (@debug do not work)
                    , div [ class "navbar-item button-light", onClick (op.replaceUrl (toHref Route.Logout)) ]
                        [ A.icon1 "icon-power" (upH T.signout) ]
                    ]
                ]

        LoggedOut ->
            if fromUrl op.url == Just Signup then
                text ""

            else
                div [ class "navbar-item" ]
                    [ a [ class "button is-small is-success has-text-weight-bold", href (toHref Route.Signup) ]
                        [ textH T.join ]
                    ]
