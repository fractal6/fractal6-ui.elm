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
import ModelSchema exposing (LocalGraph, UserRole)
import Ports


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
        rootnameid =
            case hb.path_data of
                Just path ->
                    path.root |> Maybe.map (\r -> r.nameid) |> withDefault ""

                Nothing ->
                    ""

        focusid =
            case hb.path_data of
                Just path ->
                    path.focus.nameid

                Nothing ->
                    ""
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
                                        joinButton hb.onJoin

                                Nothing ->
                                    div [ class "navbar-end ph-button-1" ] []

                        LoggedOut ->
                            joinButton hb.onJoin
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
                                li [ class "s-active has-text-weight-semibold" ]
                                    [ a [ href (uriFromNameid baseUri p.nameid) ]
                                        [ div [] [ text p.name ] ]
                                    ]
                        )
                    |> ul [ attribute "style" "display: inline-flex;" ]

            Nothing ->
                div [ class "ph-line-1" ] []
        ]


joinButton : msg -> Html msg
joinButton msg =
    div
        [ class "button is-small has-text-weight-semibold is-primary _modalTrigger_ toolti has-tooltip-bottom navbar-end"
        , attribute "data-modal" "actionModal" -- JS/Elm confcli, msg is not sent !

        --, attribute "data-tooltip" "Join this organisation."
        , onClick msg
        ]
        [ text "Join this organisation" ]


memberButtons : List UserRole -> HelperBarData msg -> Html msg
memberButtons roles_ hb =
    let
        roles =
            case hb.data of
                Expanded ->
                    roles_

                Collapsed ->
                    List.take 10 roles_

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
                if i == 0 then
                    let
                        href_ =
                            if r.role_type == RoleType.Member then
                                uriFromNameid MembersBaseUri r.rootnameid

                            else
                                uriFromNameid hb.baseUri r.nameid

                        vBar =
                            if List.length roles == 1 then
                                ""

                            else
                                "is-vbar-1"
                    in
                    [ div
                        -- @DEBUG: tooltip get stucked on click !
                        [ class ("button buttonRole is-hovered is-small toolti has-tooltip-bottom is-" ++ roleColor r.role_type)
                        , attribute "data-tooltip" (r.name ++ " of " ++ getParentFragmentFromRole r)
                        , href href_
                        ]
                        [ text r.name ]
                    ]
                        ++ [ span [ class vBar ] [] ]

                else
                    [ a
                        [ class ("button buttonRole is-small toolti has-tooltip-bottom is-" ++ roleColor r.role_type)
                        , attribute "data-tooltip" (r.name ++ " of " ++ getParentFragmentFromRole r)
                        , href <| uriFromNameid hb.baseUri r.nameid
                        ]
                        [ text r.name ]
                    ]
            )
        |> List.concat
        |> List.reverse
        |> List.append [ lastButton ]
        |> List.reverse
        |> div [ class "buttons navbar-end" ]
