module Components.HelperBar exposing (HelperBar, collapse, create, expand, view)

import Array
import Components.Fa as Fa
import Fractal.Enum.RoleType as RoleType
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, type_)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Maybe exposing (withDefault)
import ModelCommon exposing (UserState(..), getParentFragmentFromRole)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), uriFromNameid)
import ModelCommon.View exposing (roleColor)
import ModelSchema exposing (LocalGraph, NodeCharac, UserRole)
import Ports
import Text as T


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


type alias HelperBarData msg =
    { onJoin : msg
    , onExpand : msg
    , onCollapse : msg
    , baseUri : FractalBaseRoute
    , user : UserState
    , path_data : Maybe LocalGraph
    , data : HelperBar
    }


view : HelperBarData msg -> Html msg
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
        [ nav [ class "column is-11-desktop is-11-widescreen is-9-fullhd" ]
            [ div [ class "navbar" ]
                [ div [ class "navbar-brand" ]
                    [ viewPath OverviewBaseUri hb.path_data ]
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
                    [ li [ classList [ ( "is-active", hb.baseUri == OverviewBaseUri ) ] ]
                        [ a [ href (uriFromNameid OverviewBaseUri focusid) ] [ Fa.icon "fas fa-circle" "Overview" ] ]
                    , li [ classList [ ( "is-active", hb.baseUri == TensionsBaseUri ) ] ]
                        [ a [ href (uriFromNameid TensionsBaseUri focusid) ] [ Fa.icon "fas fa-exchange-alt" "Tensions" ] ]
                    , li [ classList [ ( "is-active", hb.baseUri == MembersBaseUri ) ] ]
                        [ a [ href (uriFromNameid MembersBaseUri focusid) ] [ Fa.icon "fas fa-user" "Members" ] ]
                    ]
                ]
            ]
        ]


viewPath : FractalBaseRoute -> Maybe LocalGraph -> Html msg
viewPath baseUri maybePath =
    div
        [ class "breadcrumb"
        , attribute "aria-label" "breadcrumbs"
        ]
        [ Fa.icon "fas fa-angle-right" ""
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
                                    ]
                        )
                    |> ul [ attribute "style" "display: inline-flex;" ]

            Nothing ->
                div [ class "ph-line is-1" ] []
        ]


joinButton : msg -> Html msg
joinButton msg =
    div
        [ class "button is-small has-text-weight-semibold is-primary toolti has-tooltip-bottom navbar-end"
        , attribute "data-modal" "actionModal"

        --, attribute "data-tooltip" "Join this organisation."
        , onClick msg
        ]
        [ text T.joinOrga ]


memberButtons : List UserRole -> HelperBarData msg -> Html msg
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
                    div [ class "button is-small is-primary", onClick hb.onCollapse ] [ Fa.icon0 "fas fa-angle-left" "" ]

                Collapsed ->
                    if roleMoreLen > 0 then
                        div [ class "button has-font-weight-semibold is-small is-primary", onClick hb.onExpand ]
                            [ text ("+" ++ String.fromInt roleMoreLen)
                            , Fa.icon0 "fas fa-angle-right icon-padding-left" ""
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
