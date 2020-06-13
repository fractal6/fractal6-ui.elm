module Components.HelperBar exposing (view)

import Array
import Components.Fa as Fa
import Fractal.Enum.RoleType as RoleType
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, type_)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Maybe exposing (withDefault)
import ModelCommon exposing (UserState(..), getParentFragmentFromRole)
import ModelCommon.Uri as Uri exposing (FractalBaseRoute(..), NodePath, uriFromNameid)
import ModelCommon.View exposing (roleColor)
import ModelSchema exposing (UserRole)
import Ports


view : FractalBaseRoute -> UserState -> Maybe NodePath -> msg -> Html msg
view baseUri user maybePath joinMsg =
    let
        path =
            maybePath |> withDefault (Array.fromList [])

        rootnameid =
            Array.get 0 path |> Maybe.map (\p -> p.nameid) |> withDefault ""

        focusid =
            Array.get (Array.length path - 1) path |> Maybe.map (\p -> p.nameid) |> withDefault ""
    in
    div [ id "helperBar", class "columns is-centered" ]
        [ nav [ class "column is-10-desktop is-10-widescreen is-9-fullhd" ]
            [ div [ class "columns" ]
                [ div [ class "column is-5" ]
                    [ viewPath baseUri path ]
                , div [ class "column is-6 is-offset-1" ]
                    [ case user of
                        LoggedIn uctx ->
                            case maybePath of
                                Just _ ->
                                    let
                                        roles =
                                            List.filter (\r -> r.rootnameid == rootnameid) uctx.roles
                                    in
                                    if List.length roles > 0 then
                                        memberButton baseUri roles

                                    else
                                        joinButton joinMsg

                                Nothing ->
                                    -- Loading
                                    text ""

                        LoggedOut ->
                            joinButton joinMsg
                    ]
                ]
            , div [ class "tabs is-boxed" ]
                [ ul []
                    [ li [ classList [ ( "is-active", baseUri == OverviewBaseUri ) ] ]
                        [ a [ href (uriFromNameid OverviewBaseUri focusid) ] [ Fa.icon "fas fa-circle" "Overview" ] ]
                    , li [ classList [ ( "is-active", baseUri == TensionsBaseUri ) ] ]
                        [ a [ href (uriFromNameid TensionsBaseUri focusid) ] [ Fa.icon "fas fa-exchange-alt" "Tensions" ] ]
                    , li [ classList [ ( "is-active", baseUri == UsersBaseUri ) ] ]
                        [ a [ href (uriFromNameid UsersBaseUri focusid) ] [ Fa.icon "fas fa-user" "Members" ] ]
                    ]
                ]
            ]
        ]


viewPath : FractalBaseRoute -> NodePath -> Html msg
viewPath baseUri path =
    div
        [ class "breadcrumb"
        , attribute "aria-label" "breadcrumbs"
        ]
        [ Fa.icon "fas fa-angle-right" ""
        , Array.indexedMap
            (\i p ->
                if i < (Array.length path - 1) then
                    li []
                        [ a
                            [ href (uriFromNameid baseUri p.nameid) ]
                            [ div [ classList [ ( "has-text-weight-bold", i == 0 ) ] ] [ text p.name ] ]
                        ]

                else
                    li [ class "is-active has-text-weight-semibold" ]
                        [ a [ href (uriFromNameid baseUri p.nameid) ]
                            [ div [ classList [ ( "has-text-weight-bold", i == 0 ) ] ] [ text p.name ] ]
                        ]
            )
            path
            |> Array.toList
            |> ul [ attribute "style" "display: inline-flex;" ]
        ]


joinButton : msg -> Html msg
joinButton msg =
    div
        [ class "button is-small has-text-weight-semibold is-primary _modalTrigger_  tooltip has-tooltip-bottom is-pulled-right"
        , attribute "data-modal" "actionModal" -- JS/Elm confcli, msg is not sent !

        --, attribute "data-tooltip" "Join this organisation."
        , onClick msg
        ]
        [ text "Join this organisation" ]


memberButton : FractalBaseRoute -> List UserRole -> Html msg
memberButton baseUri roles =
    roles
        |> List.indexedMap
            (\i r ->
                if i == 0 then
                    let
                        href_ =
                            if r.role_type == RoleType.Member then
                                "#"

                            else
                                uriFromNameid baseUri r.nameid

                        vBar =
                            if List.length roles == 1 then
                                ""

                            else
                                "is-vbar"
                    in
                    [ a
                        [ class ("button buttonRole is-hovered is-small has-text-weight-semiboldtooltip has-tooltip-bottom is-" ++ roleColor r.role_type)
                        , attribute "data-tooltip" (r.name ++ " of " ++ getParentFragmentFromRole r)
                        , href href_
                        ]
                        [ text r.name ]
                    ]
                        ++ [ span [ class vBar ] []
                           ]

                else
                    [ a
                        [ class ("button buttonRole is-small has-text-weight-semiboldtooltip has-tooltip-bottom is-" ++ roleColor r.role_type)
                        , attribute "data-tooltip" (r.name ++ " of " ++ getParentFragmentFromRole r)
                        , href <| uriFromNameid baseUri r.nameid
                        ]
                        [ text r.name ]
                    ]
            )
        |> List.concat
        |> div [ class "buttons is-pulled-right" ]
