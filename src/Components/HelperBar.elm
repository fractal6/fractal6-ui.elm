module Components.HelperBar exposing (HelperBar, collapse, create, expand, view)

import Array
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, type_)
import Html.Events exposing (onClick)
import Icon as I
import Json.Decode as JD
import Maybe exposing (withDefault)
import ModelCommon exposing (UserState(..), getParentFragmentFromRole)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), typeFromNameid, uriFromNameid)
import ModelCommon.View exposing (roleColor)
import ModelSchema exposing (LocalGraph, NodeCharac, UserRole)
import Ports
import Text as T exposing (textH, textT)


type HelperBar
    = Expanded
    | Collapsed


create : HelperBar
create =
    Collapsed


expand : HelperBar -> HelperBar
expand hb =
    Expanded


collapse : HelperBar -> HelperBar
collapse hb =
    Collapsed


numberRolesCollapsed : Int
numberRolesCollapsed =
    5


type alias Op msg =
    { baseUri : FractalBaseRoute
    , user : UserState
    , path_data : Maybe LocalGraph
    , data : HelperBar
    , onJoin : msg
    , onExpand : msg
    , onCollapse : msg
    }


view : Op msg -> Html msg
view hb =
    let
        ( focusid, rootnameid, charac ) =
            case hb.path_data of
                Just path ->
                    ( path.focus.nameid, path.root |> Maybe.map (\r -> r.nameid) |> withDefault "", Just path.focus.charac )

                Nothing ->
                    ( "", "", Nothing )
    in
    div [ id "helperBar", class "columns is-centered" ]
        [ nav [ class "column is-11-desktop is-11-widescreen is-10-fullhd" ]
            [ div [ class "navbar" ]
                [ div [ class "navbar-brand" ]
                    [ viewPath hb.baseUri hb.path_data ]
                , div
                    [ class "navbar-burger burger"
                    , attribute "data-target" "rolesMenu"
                    , attribute "aria-expanded" "false"
                    , attribute "aria-label" "menu"
                    , attribute "role" "button"
                    ]
                    [ span [ attribute "aria-hidden" "true" ] []
                    , span [ attribute "aria-hidden" "true" ] []
                    , span [ attribute "aria-hidden" "true" ] []
                    ]
                , div [ id "rolesMenu", class "navbar-menu" ]
                    [ case hb.user of
                        LoggedIn uctx ->
                            case hb.path_data of
                                Just path ->
                                    let
                                        roles =
                                            List.filter (\r -> r.rootnameid == rootnameid) uctx.roles
                                    in
                                    if List.length roles > 0 then
                                        memberButtons roles { hb | baseUri = OverviewBaseUri }

                                    else
                                        charac
                                            |> Maybe.map
                                                (\c ->
                                                    if c.userCanJoin then
                                                        joinButton hb.onJoin

                                                    else
                                                        text ""
                                                )
                                            |> withDefault (text "")

                                Nothing ->
                                    div [ class "navbar-end ph-button-1" ] []

                        LoggedOut ->
                            charac
                                |> Maybe.map
                                    (\c ->
                                        if c.userCanJoin then
                                            joinButton hb.onJoin

                                        else
                                            text ""
                                    )
                                |> withDefault (text "")
                    ]
                ]
            , div [ class "tabs is-boxed" ]
                [ ul []
                    ([ li [ classList [ ( "is-active", hb.baseUri == OverviewBaseUri ) ] ]
                        [ a [ href (uriFromNameid OverviewBaseUri focusid) ] [ I.icon1 "icon-sun" "Overview" ] ]
                     , li [ classList [ ( "is-active", hb.baseUri == TensionsBaseUri ) ] ]
                        [ a [ href (uriFromNameid TensionsBaseUri focusid) ] [ I.icon1 "icon-exchange" "Tensions" ] ]
                     , li [ classList [ ( "is-active", hb.baseUri == MembersBaseUri ) ] ]
                        [ a [ href (uriFromNameid MembersBaseUri focusid) ] [ I.icon1 "icon-user" "Members" ] ]
                     ]
                        ++ (Maybe.map
                                (\path ->
                                    if hb.user /= LoggedOut && path.focus.type_ == NodeType.Circle then
                                        [ li [ class "is-vbar-2" ] []
                                        , li [ classList [ ( "is-active", hb.baseUri == SettingsBaseUri ) ] ]
                                            [ a [ href (uriFromNameid SettingsBaseUri focusid) ] [ I.icon1 "icon-settings" "Settings" ] ]
                                        ]

                                    else
                                        []
                                )
                                hb.path_data
                                |> withDefault []
                           )
                    )
                ]
            ]
        ]


viewPath : FractalBaseRoute -> Maybe LocalGraph -> Html msg
viewPath baseUri maybePath =
    div
        [ class "breadcrumb"
        , attribute "aria-label" "breadcrumbs"
        ]
        [ I.icon0 "icon-layers icon-lg"
        , case maybePath of
            Just g ->
                g.path
                    |> List.indexedMap
                        (\i p ->
                            if i < (List.length g.path - 1) then
                                li []
                                    [ a [ href (uriFromNameid baseUri p.nameid) ]
                                        [ div [] [ text p.name ] ]
                                    ]

                            else
                                li [ class "is-acti has-text-weight-semibold" ]
                                    [ a [ href (uriFromNameid baseUri p.nameid) ]
                                        [ div [] [ text p.name ] ]
                                    , if g.focus.type_ == NodeType.Circle && List.length (List.filter (\c -> typeFromNameid c.nameid == NodeType.Circle) g.focus.children) > 0 then
                                        viewTree baseUri g

                                      else
                                        text ""
                                    ]
                        )
                    |> ul [ attribute "style" "display: inline-flex;" ]

            Nothing ->
                div [ class "ph-line is-1" ] []
        ]


viewTree : FractalBaseRoute -> LocalGraph -> Html msg
viewTree baseUri g =
    div [ class "dropdown" ]
        [ div [ class "dropdown-trigger px-2 button-light" ]
            [ div [ attribute "aria-controls" "tree-menu" ] [ I.icon "icon-chevron-down" ]
            ]
        , div [ id "tree-menu", class "dropdown-menu", attribute "role" "menu" ]
            [ div [ class "dropdown-content" ] <|
                (g.focus.children
                    |> List.filter (\c -> typeFromNameid c.nameid == NodeType.Circle)
                    |> List.map
                        (\c ->
                            a [ class "dropdown-item pl-2", href (uriFromNameid baseUri c.nameid) ] [ text c.name ]
                        )
                )
            ]
        ]


joinButton : msg -> Html msg
joinButton msg =
    div
        [ class "button is-small has-text-weight-semibold is-primary toolti has-tooltip-bottom navbar-end"
        , attribute "data-modal" "actionModal"

        --, attribute "data-tooltip" "Join this organisation."
        , onClick msg
        ]
        [ textH T.joinOrga ]


memberButtons : List UserRole -> Op msg -> Html msg
memberButtons roles_ hb =
    let
        roles =
            case hb.data of
                Expanded ->
                    roles_

                Collapsed ->
                    List.take numberRolesCollapsed roles_

        roleMoreLen =
            List.length roles_ - List.length roles

        lastButton =
            case hb.data of
                Expanded ->
                    div [ class "button is-small is-primary", onClick hb.onCollapse ] [ I.icon "icon-chevrons-left" ]

                Collapsed ->
                    if roleMoreLen > 0 then
                        div [ class "button has-font-weight-semibold is-small is-primary", onClick hb.onExpand ]
                            [ text ("+" ++ String.fromInt roleMoreLen)
                            , I.icon "icon-chevrons-right icon-padding-left"
                            ]

                    else
                        div [] []
    in
    roles
        |> List.indexedMap
            (\i r ->
                if (r.role_type /= RoleType.Member && r.role_type /= RoleType.Owner) || (r.role_type == RoleType.Owner && i == 0) then
                    [ a
                        [ class ("button buttonRole is-small toolti has-tooltip-bottom is-" ++ roleColor r.role_type)
                        , attribute "data-tooltip" (r.name ++ " of " ++ getParentFragmentFromRole r)
                        , href <| uriFromNameid hb.baseUri r.nameid
                        ]
                        [ text r.name ]

                    --++ [ span [ class "is-vbar-1" ] [] ]
                    ]

                else
                    [ text "" ]
            )
        |> List.concat
        |> List.reverse
        |> List.append [ lastButton ]
        |> List.reverse
        |> div [ class "buttons navbar-end" ]
