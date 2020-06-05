module Components.HelperBar exposing (view)

import Array
import Components.Fa as Fa
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, type_)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Maybe exposing (withDefault)
import ModelCommon exposing (UserState(..))
import ModelCommon.Uri as Uri exposing (FractalBaseRoute(..), NodePath, uriFromNameid)
import ModelSchema exposing (UserRole)


view : FractalBaseRoute -> UserState -> Maybe NodePath -> msg -> Html msg
view baseUri user maybePath joinMsg =
    let
        path =
            maybePath |> withDefault (Array.fromList [])

        rootnameid =
            Array.get 0 path |> Maybe.map (\p -> p.nameid) |> withDefault ""
    in
    div [ id "helperBar", class "columns is-centered" ]
        [ nav [ class "column is-10" ]
            [ div [ class "level" ]
                [ div [ class "level-left" ]
                    [ div [ class "level-item" ] [ viewPath baseUri path ]
                    ]
                , div [ class "level-right" ]
                    [ div [ class "level-item" ]
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
                ]
            , div [ class "tabs is-boxed" ]
                [ ul []
                    [ li [ classList [ ( "is-active", baseUri == OverviewBaseUri ) ] ]
                        [ a [] [ Fa.icon "fas fa-circle" "Overview" ] ]
                    , li [ classList [ ( "is-active", baseUri == TensionsBaseUri ) ] ]
                        [ a [] [ Fa.icon "fas fa-exchange-alt" "Tensions" ] ]
                    , li [ classList [ ( "is-active", baseUri == UsersBaseUri ) ] ]
                        [ a [] [ Fa.icon "fas fa-user" "Members" ] ]
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
        [ class "button is-small has-text-weight-semibold is-primary _modalTrigger_  tooltip has-tooltip-bottom"
        , attribute "data-modal" "actionModal" -- JS/Elm confcli, msg is not sent !
        , attribute "data-tooltip" "Join this organisation."
        , onClick msg
        ]
        [ text "Join" ]


memberButton : FractalBaseRoute -> List UserRole -> Html msg
memberButton baseUri roles =
    roles
        |> List.map
            (\r ->
                a
                    [ class "button is-hovered is-small has-text-weight-semibold is-primary"
                    , attribute "style" "margin-right: 5px;"
                    , href <| uriFromNameid baseUri r.nameid
                    ]
                    [ text r.name ]
            )
        |> div []
