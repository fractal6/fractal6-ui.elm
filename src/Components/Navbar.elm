{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2026 Fractale Co

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


module Components.Navbar exposing (NavbarHandlers, view)

import Assets as A
import Assets.Logo as Logo
import Bulk exposing (UserState(..))
import Bulk.Codecs exposing (FractalBaseRoute(..), isOrgUrl, isTensionUrl, toLink)
import Bulk.Error exposing (viewGqlErrorsLight)
import Bulk.View exposing (lang2str, statusColor, tensionIcon)
import Extra exposing (showIf, ternary)
import Fractal.Enum.Lang as Lang
import Fractal.Enum.TensionStatus as TensionStatus
import Generated.Route as Route exposing (Route(..), fromUrl, toHref)
import Html exposing (Html, a, button, div, header, hr, nav, p, span, strong, text)
import Html.Attributes as Attr exposing (attribute, class, classList, href, id, style, target, title)
import Html.Events exposing (onClick)
import Maybe exposing (withDefault)
import ModelSchema exposing (NotifCount, OrgaInfo, TensionHead)
import Ports
import Session exposing (Apis, SessionCommon, Theme(..))
import Text as T
import Url exposing (Url)


{-| Handlers for navbar actions passed from parent.
-}
type alias NavbarHandlers msg =
    { onReplaceUrl : String -> msg
    , onCloseOutdated : msg
    , onScrollToTop : msg
    , onScrollToBottom : msg
    }


view : Apis -> SessionCommon -> NotifCount -> Maybe OrgaInfo -> Maybe TensionHead -> NavbarHandlers msg -> Html msg
view apis session notif orga_info tension_head handlers =
    let
        orgUrl =
            isOrgUrl session.url

        -- @debug: make it work even when orgaInfo is Nothing !
        hasVersionOutdated =
            (Maybe.map .client_version orga_info /= Just apis.client_version)
                && (orga_info /= Nothing)

        isLoggedOut =
            session.user == LoggedOut

        loggedClass =
            ternary isLoggedOut "is-logged-out" "is-logged-in"
    in
    header [ id "navbarTop", class ("has-navbar-fixed-top " ++ loggedClass) ]
        [ nav
            ([ class "navbar is-fixed-top"
             , classList [ ( "is-primary", isLoggedOut ) ]
             , attribute "role" "navigation"
             , attribute "aria-label" "main navigation"
             ]
                ++ ternary isLoggedOut [ attribute "data-theme" "light" ] []
            )
            [ div [ class "navbar-brand" ]
                ([ a [ class "navbar-item", href "/" ]
                    --[ img [ alt "Fractal", attribute "height" "28", attribute "width" "112", src "https://bulma.io/images/bulma-logo.png" ] [] ]
                    [ if isLoggedOut then
                        A.logo_inline

                      else
                        A.logo0
                    , showIf isLoggedOut <|
                        span [ class "logo-fractale-text" ]
                            [-- text "Fractale"
                             --, span [ class "has-text-warning", attribute "style" "padding-top:10px;font-size:0.65rem;margin-left:-2px;" ] [ text "alpha" ]
                             --, span [ class "has-text-warning", attribute "style" "position:relative;top:-10px;font-size:0.65rem;" ] [ text "beta" ]
                            ]
                    ]
                 ]
                    ++ (if orgUrl then
                            case session.user of
                                LoggedIn _ ->
                                    [ div [ class "navbar-item button-light is-hidden-touch menuOrgaTrigger", title T.showOrgaMenu ] [ A.icon "icon-git-commit icon-rotate-90 icon-bg" ]
                                    , div [ class "navbar-item button-light menuTreeTrigger", title T.showCircleMenu ] [ A.icon "icon-git-branch icon-bg" ]
                                    ]

                                LoggedOut ->
                                    [ div [ class "navbar-item button-light menuTreeTrigger", title T.showCircleMenu ] [ A.icon "icon-git-branch icon-bg" ] ]

                        else
                            []
                       )
                    ++ [ div [ class "navbar-touch-end" ] [ notificationButton "" session.user notif session.url ]
                       , A.burger "userMenu"
                       ]
                )
            , showIf hasVersionOutdated <|
                div [ class "f6-notification notification has-background-warning-soft" ]
                    [ button [ class "delete", onClick handlers.onCloseOutdated ] []
                    , a [ class "button-light is-light" ]
                        -- https://github.com/surprisetalk/elm-bulma/issues/17
                        [ p [ class "title is-6 mb-2" ] [ text "New Version Released 🎉" ]
                        , viewGqlErrorsLight [ "Refresh now for new features and improvements" ]
                        ]
                    ]
            , div [ id "userMenu", class "navbar-menu" ]
                [ div [ class "navbar-start" ] <|
                    (case session.user of
                        LoggedIn uctx ->
                            [ a
                                [ class "navbar-item is-size-7"
                                , classList
                                    [ ( "is-active"
                                      , case fromUrl session.url of
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
                                , classList [ ( "is-active", fromUrl session.url == Just Explore ) ]
                                , href (toHref Explore)
                                ]
                                [ text T.explore ]
                           ]
                        ++ (if isLoggedOut then
                                [ span [ class "vbar is-hidden-mobile", attribute "style" "margin-top: 21px !important; margin-left: 0; padding-left: 0; " ] []
                                , a
                                    [ class "navbar-item", target "_blank", href "https://doc.fractale.co" ]
                                    [ text "Docs" ]
                                , span [ class "vbar is-hidden-mobile", attribute "style" "margin-top: 21px !important; margin-left: 0; padding-left: 0; " ] []
                                , a
                                    [ class "navbar-item", href "https://github.com/fractal6/fractal6.go", target "_blank" ]
                                    [ text "Open Source" ]
                                ]

                            else
                                []
                           )
                , viewTensionTitle session tension_head handlers
                , div [ class "navbar-end" ] <|
                    [ notificationButton "is-hidden-touch" session.user notif session.url
                    , helpButton session.user
                    ]
                        ++ userButtons session handlers.onReplaceUrl
                ]
            ]
        ]


viewTensionTitle : SessionCommon -> Maybe TensionHead -> NavbarHandlers msg -> Html msg
viewTensionTitle session tension_head handlers =
    let
        isTensionPage =
            isTensionUrl session.url

        shouldShow =
            isTensionPage
                && session.scrollPosition
                /= Ports.ScrollTop
                && tension_head
                /= Nothing
    in
    case ( shouldShow, tension_head ) of
        ( True, Just th ) ->
            div [ class "navbar-tension-title is-hidden-mobile" ]
                [ span [ class "tension-type-status" ]
                    [ A.icon
                        ("icon-alert-circle icon-sm has-text-"
                            ++ statusColor th.status
                        )
                    , tensionIcon th.type_
                    ]
                , span
                    [ class "tension-title-text"
                    , title T.scrollToTop
                    , onClick handlers.onScrollToTop
                    ]
                    [ text th.title ]
                , button
                    [ class "button is-small ml-2"
                    , title T.scrollToBottom
                    , onClick handlers.onScrollToBottom
                    ]
                    [ A.icon "icon-chevron-down" ]
                ]

        _ ->
            text ""


notificationButton : String -> UserState -> NotifCount -> Url -> Html msg
notificationButton cls user notif url =
    case user of
        LoggedIn _ ->
            a
                [ class ("navbar-item px-3 " ++ cls)
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
                        span [ class "badge is-event-badge", title T.unreadNotif ] []

                      else
                        text ""
                    , if notif.pending_contracts > 0 then
                        span [ class "badge is-contract-badge is-top-left", title T.pendingContract ] []

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
                [ class "navbar-item has-dropdown px-2"
                , title "Help and feedback"
                ]
                [ div [ class "navbar-link is-arrowless is-hidden-touch" ]
                    [ div [ class "button is-rounded is-small has-background-navbar", style "height" "inherit" ] [ A.icon "icon-question" ] ]
                , div [ class "button-light is-hidden-tablet" ] [ A.icon1 "icon-question" "Help" ]
                , div [ class "navbar-dropdown has-border" ]
                    [ div [ class "navbar-item pb-3 helpTrigger", attribute "data-help" "QuickHelp" ]
                        [ text T.quickHelp ]
                    , hr [ class "navbar-divider" ] []
                    , div [ class "navbar-item py-3 helpTrigger", attribute "data-help" "AskQuestion" ]
                        [ text T.askQuestion ]
                    , hr [ class "navbar-divider" ] []
                    , div [ class "navbar-item pt-3 helpTrigger", attribute "data-help" "Feedback" ]
                        [ text T.giveFeedback ]
                    ]
                ]

        LoggedOut ->
            text ""


userButtons : SessionCommon -> (String -> msg) -> List (Html msg)
userButtons session replaceUrl =
    case session.user of
        LoggedIn uctx ->
            [ div [ class "navbar-item has-dropdown is-hoverabl" ]
                [ div
                    [ class "navbar-link"
                    , attribute "style" "padding-right: 1.85rem;"
                    ]
                    [ text uctx.username ]
                , div [ class "navbar-dropdown has-border is-right" ]
                    [ a [ class "navbar-item", href (toLink UsersBaseUri uctx.username []) ]
                        [ A.icon1 "icon-home" T.home ]
                    , a [ class "navbar-item", href (toHref <| Dynamic_Settings { param1 = uctx.username }) ]
                        [ A.icon1 "icon-tool" T.settings ]
                    , div [ id "themeTrigger", class "navbar-item pb-3" ]
                        [ case session.theme of
                            LightTheme ->
                                A.icon1 "icon-moon" T.toggleDarkMode

                            DarkTheme ->
                                A.icon1 "icon-sun" T.toggleLightMode
                        ]
                    , hr [ class "navbar-divider" ] []
                    , a [ class "navbar-item py-3", href (toHref New_Orga) ]
                        [ A.icon1 "icon-plus" T.newOrganisation ]
                    , hr [ class "navbar-divider" ] []

                    -- Prevout logout to be log in the browser history (@debug do not work)
                    , div [ class "navbar-item pt-3", onClick (replaceUrl (toHref Logout)) ]
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
                , div [ class "navbar-dropdown has-border is-right" ] <|
                    List.map
                        (\lang ->
                            span [ class "navbar-item button-light langTrigger", attribute "data-lang" (Lang.toString lang) ] [ text (lang2str lang) ]
                        )
                        Lang.list
                ]
                :: (if List.member (fromUrl session.url) [ Just Login, Just Signup ] then
                        []

                    else
                        [ div [ class "navbar-item" ]
                            [ a [ class "button is-rounded is-outlined has-background-primary", href (toHref Login) ] [ text T.signin ]
                            ]
                        , div [ class "navbar-item" ]
                            [ a [ class "button is-rounded is-signup", href (toHref Signup) ]
                                [ text T.tryFree ]
                            ]
                        ]
                   )
