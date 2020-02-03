module Layouts.Org exposing (view)

import Generated.Routes as Routes exposing (Route, routes)
import Global
import Html exposing (..)
import Html.Attributes exposing (..)
import Utils.Spa as Spa


view : Spa.LayoutContext msg -> Html msg
view { page, route, global } =
    let
        --Debug.log "trying" global
        focus =
            case global.user of
                Global.LoggedOut state ->
                    state.focus

                Global.LoggedIn state _ ->
                    state.focus
    in
    div
        [ class "columns" ]
        [ div [ id "leftPane", class "column is-2" ] [ viewLeftPane route focus ]
        , div [ class "column is-10" ] [ page ]
        ]


viewLeftPane : Route -> Global.UserFocus -> Html msg
viewLeftPane currentRoute focus =
    nav [ class "menu is-hidden-mobile" ]
        [ p [ class "menu-label" ]
            [ div [ class "hero is-small is-primary is-bold" ]
                [ div [ class "hero-body has-text-centered" ] [ text focus.orga_name ] ]
            ]
        , ul [ class "menu-list" ]
            [ li [ class "menu-label" ]
                [ div [ class "hero is-small is-info is-bold" ]
                    [ div [ class "hero-body" ] [ text focus.circle_name ] ]
                ]
            , li []
                [ ul [ class "menu-list" ]
                    [ li []
                        [ a [ class "is-active" ]
                            [ text "Mandate" ]
                        ]
                    , li []
                        [ a [] [ text "Tensions" ] ]
                    , li []
                        [ a [] [ text "Journal" ] ]
                    , li []
                        [ a [] [ text "Members" ] ]
                    ]
                ]
            ]
        ]
