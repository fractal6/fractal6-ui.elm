module Components.Navbar exposing (view)

import Assets as A
import Assets.Logo as Logo
import Extra exposing (ternary, textH, upH)
import Fractal.Enum.Lang as Lang
import Generated.Route as Route exposing (Route(..), fromUrl, toHref)
import Html exposing (Html, a, button, div, header, hr, i, nav, span, text)
import Html.Attributes as Attr exposing (attribute, class, classList, href, id, style, target, title)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Maybe exposing (withDefault)
import ModelCommon exposing (UserState(..))
import ModelCommon.Codecs exposing (FractalBaseRoute(..), isOrgUrl, toString)
import ModelCommon.View exposing (lang2str)
import Text as T
import Url exposing (Url)


view : UserState -> Url -> (String -> msg) -> Html msg
view user url replaceUrl =
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
                    (case user of
                        LoggedIn uctx ->
                            let
                                orgUrl =
                                    isOrgUrl url
                            in
                            [ if orgUrl then
                                div [ class "navbar-item button-light is-hidden-touch menuOrgaTrigger" ] [ A.icon "icon-menu" ]

                              else
                                text ""
                            , a
                                [ class "navbar-item"
                                , classList
                                    [ ( "is-active"
                                      , case fromUrl url of
                                            Just (Dynamic a) ->
                                                ternary (a.param1 == uctx.username) True False

                                            Just (User_Dynamic a) ->
                                                ternary (a.param1 == uctx.username) True False

                                            _ ->
                                                False
                                      )
                                    ]
                                , href (toHref Top)
                                ]
                                [ text T.yourOrg ]
                            ]

                        LoggedOut ->
                            []
                    )
                        ++ [ a
                                [ class "navbar-item"
                                , classList [ ( "is-active", fromUrl url == Just Explore ) ]
                                , href (toHref Explore)
                                ]
                                [ text T.explore ]
                           ]
                , div [ class "navbar-end" ] <|
                    [ notificationButton user url
                    , helpButton user
                    , newButton user
                    ]
                        ++ userButtons user url replaceUrl
                ]
            ]
        ]


notificationButton : UserState -> Url -> Html msg
notificationButton user url =
    case user of
        LoggedIn _ ->
            a
                [ class "navbar-item"
                , href (toHref Notifications)
                , title T.notifications
                , classList [ ( "is-active", fromUrl url == Just Notifications ) ]
                ]
                [ div
                    [ class "navbar-link is-arrowless notifTrigger"
                    , classList [ ( "is-active", fromUrl url == Just Notifications ) ]
                    ]
                    [ A.icon "icon-bg icon-bell" ]
                ]

        LoggedOut ->
            text ""


helpButton : UserState -> Html msg
helpButton user =
    case user of
        LoggedIn _ ->
            div
                [ class "navbar-item"
                , title "Help and feedback"
                ]
                [ div
                    [ class "navbar-link is-arrowless helpTrigger" ]
                    [ div [ class "button is-rounded is-small has-background-navbar", style "height" "inherit" ]
                        [ A.icon "icon-question" ]
                    ]
                ]

        LoggedOut ->
            text ""


newButton : UserState -> Html msg
newButton user =
    case user of
        LoggedIn _ ->
            div
                [ class "navbar-item has-dropdown is-hoverabl" ]
                [ div
                    [ class "navbar-link is-small"
                    , attribute "style" "padding-right: 1.65rem;"
                    ]
                    [ A.icon "icon-plus icon-bg" ]
                , div [ class "navbar-dropdown is-right" ]
                    [ a [ class "navbar-item", href (toHref New_Orga) ]
                        [ text T.newOrganisation ]
                    ]
                ]

        LoggedOut ->
            text ""


userButtons : UserState -> Url -> (String -> msg) -> List (Html msg)
userButtons user url replaceUrl =
    case user of
        LoggedIn uctx ->
            [ div [ class "navbar-item has-dropdown is-hoverabl" ]
                [ div
                    [ class "navbar-link"
                    , attribute "style" "padding-right: 1.85rem;"
                    ]
                    [ text uctx.username ]
                , div [ class "navbar-dropdown is-right" ]
                    [ a [ class "navbar-item", href (toString UsersBaseUri uctx.username []) ]
                        [ A.icon1 "icon-user" T.profile ]
                    , a [ class "navbar-item", href (toHref <| Dynamic_Settings { param1 = uctx.username }) ]
                        [ A.icon1 "icon-tool" T.settings ]
                    , span [ id "themeTrigger", class "navbar-item is-w" ]
                        [ A.icon1 "icon-moon" T.toggleLightMode ]
                    , hr [ class "navbar-divider" ] []

                    -- Prevout logout to be log in the browser history (@debug do not work)
                    , div [ class "navbar-item button-light", onClick (replaceUrl (toHref Logout)) ]
                        [ A.icon1 "icon-power" T.signout ]
                    ]
                ]
            ]

        LoggedOut ->
            [ div [ class "navbar-item has-dropdown is-hoverable" ]
                [ div
                    [ class "navbar-link"
                    , attribute "style" "padding-right: 1.85rem;"
                    ]
                    [ Logo.i18n ]
                , div [ class "navbar-dropdown is-right" ] <|
                    List.map
                        (\lang ->
                            span [ class "navbar-item button-light langTrigger", attribute "data-lang" (Lang.toString lang) ] [ text (lang2str lang) ]
                        )
                        Lang.list
                ]
            ]
                ++ (if List.member (fromUrl url) [ Just Login, Just Signup ] then
                        []

                    else
                        [ div [ class "navbar-item" ] [ a [ href (toHref Login) ] [ text T.signin ] ]
                        , div [ class "navbar-item notMe" ]
                            [ a [ class "button is-small is-success has-text-weight-bold", href (toHref Signup) ]
                                [ text T.join ]
                            ]
                        ]
                   )
