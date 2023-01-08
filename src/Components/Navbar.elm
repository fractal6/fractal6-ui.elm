{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2023 Fractale Co

   This file is part of Fractale.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
-}


module Components.Navbar exposing (view)

import Assets as A
import Assets.Logo as Logo
import Extra exposing (ternary, textH, upH)
import Fractal.Enum.Lang as Lang
import Generated.Route as Route exposing (Route(..), fromUrl, toHref)
import Html exposing (Html, a, button, div, header, hr, i, nav, span, strong, text)
import Html.Attributes as Attr exposing (attribute, class, classList, href, id, style, target, title)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Maybe exposing (withDefault)
import Bulk exposing (UserState(..))
import Bulk.Codecs exposing (FractalBaseRoute(..), isOrgUrl, toLink)
import Bulk.View exposing (lang2str)
import ModelSchema exposing (NotifCount)
import Text as T
import Url exposing (Url)


view : UserState -> NotifCount -> Url -> (String -> msg) -> Html msg
view user notif url replaceUrl =
    let
        orgUrl =
            isOrgUrl url
    in
    header [ id "navbarTop", class "has-navbar-fixed-top" ]
        [ nav
            [ class "navbar is-fixed-top"
            , attribute "role" "navigation"
            , attribute "aria-label" "main navigation"
            ]
            [ div [ class "navbar-brand" ]
                ([ a [ class "navbar-item", href "/" ]
                    --[ img [ alt "Fractal", attribute "height" "28", attribute "width" "112", src "https://bulma.io/images/bulma-logo.png" ] [] ]
                    [ A.logo0 "white"
                    , case user of
                        LoggedOut ->
                            strong [ class "is-recursiv", attribute "style" "position:relative;bottom:2px;" ] [ text "Fractale" ]

                        _ ->
                            text ""

                    --, span [ class "has-text-orange", attribute "style" "padding-top:10px;font-size:0.65rem;margin-left:-2px;" ] [ text "alpha" ]
                    , span [ class "has-text-orange", attribute "style" "position:relative;top:-10px;font-size:0.65rem;" ] [ text "beta" ]
                    ]
                 ]
                    ++ (if orgUrl then
                            case user of
                                LoggedIn _ ->
                                    [ div [ class "navbar-item button-light is-hidden-touch menuOrgaTrigger", title T.showOrgaMenu ] [ A.icon "icon-menu icon-bg" ]
                                    , div [ class "navbar-item button-light menuTreeTrigger", title T.showCircleMenu ] [ A.icon "icon-git-branch icon-bg" ]
                                    ]

                                LoggedOut ->
                                    [ div [ class "navbar-item button-light menuTreeTrigger", title T.showCircleMenu ] [ A.icon "icon-git-branch icon-bg" ] ]

                        else
                            []
                       )
                    ++ [ div [ class "navbar-touch-end" ] [ notificationButton "" user notif url ]
                       , A.burger "userMenu"
                       ]
                )
            , div [ id "userMenu", class "navbar-menu" ]
                [ div [ class "navbar-start" ] <|
                    (case user of
                        LoggedIn uctx ->
                            [ a
                                [ class "navbar-item is-size-7"
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
                                , href (toLink UsersBaseUri uctx.username [])
                                ]
                                [ text T.home ]
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
                    [ notificationButton "is-hidden-touch" user notif url
                    , helpButton user

                    --, newButton user
                    ]
                        ++ userButtons user url replaceUrl
                ]
            ]
        ]


notificationButton : String -> UserState -> NotifCount -> Url -> Html msg
notificationButton cls user notif url =
    case user of
        LoggedIn _ ->
            a
                [ class ("navbar-item " ++ cls)
                , href (toHref Notifications)
                , title T.notifications
                , classList [ ( "is-active", fromUrl url == Just Notifications ) ]
                ]
                [ div
                    [ class "navbar-link is-arrowless"
                    , classList [ ( "is-active", fromUrl url == Just Notifications ) ]
                    ]
                    [ A.icon "icon-bg icon-bell"
                    , if notif.unread_events > 0 then
                        span [ class "badge is-event-badge", style "margin-top" "-5px", title T.unreadNotif ] []

                      else
                        text ""
                    , if notif.pending_contracts > 0 then
                        span [ class "badge is-contract-badge is-top-left", attribute "style" "left:5px;margin-top:1px;", title T.pendingContract ] []

                      else
                        text ""
                    ]
                ]

        LoggedOut ->
            text ""


helpButton : UserState -> Html msg
helpButton user =
    case user of
        LoggedIn _ ->
            div
                [ class "navbar-item helpTrigger"
                , title "Help and feedback"
                ]
                [ div [ class "navbar-link is-arrowless is-hidden-touch" ]
                    [ div [ class "button is-rounded is-small has-background-navbar", style "height" "inherit" ] [ A.icon "icon-question" ] ]
                , div [ class "button-light is-hidden-tablet" ] [ A.icon1 "icon-question" "Help" ]
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
                    [ a [ class "navbar-item", href (toLink UsersBaseUri uctx.username []) ]
                        [ A.icon1 "icon-home" T.home ]
                    , a [ class "navbar-item", href (toHref <| Dynamic_Settings { param1 = uctx.username }) ]
                        [ A.icon1 "icon-tool" T.settings ]
                    , span [ id "themeTrigger", class "navbar-item is-w" ]
                        [ A.icon1 "icon-moon" T.toggleLightMode ]
                    , hr [ class "navbar-divider" ] []
                    , a [ class "navbar-item", href (toHref New_Orga) ]
                        [ A.icon1 "icon-plus" T.newOrganisation ]
                    , hr [ class "navbar-divider" ] []

                    -- Prevout logout to be log in the browser history (@debug do not work)
                    , div [ class "navbar-item button-light", onClick (replaceUrl (toHref Logout)) ]
                        [ A.icon1 "icon-power" T.signout ]
                    ]
                ]
            ]

        LoggedOut ->
            div [ class "navbar-item has-dropdown is-hoverable" ]
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
                :: (if List.member (fromUrl url) [ Just Login, Just Signup ] then
                        []

                    else
                        [ div [ class "navbar-item" ] [ a [ href (toHref Login) ] [ text T.signin ] ]
                        , div [ class "navbar-item notMe" ]
                            [ a [ class "button is-rounded is-small is-success has-text-weight-bold", href (toHref Signup) ]
                                [ text T.tryFree ]
                            ]
                        ]
                   )
