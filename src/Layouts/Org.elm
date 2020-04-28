module Layouts.Org exposing (view)

import Generated.Routes as Routes exposing (Route, routes)
import Global
import Html exposing (..)
import Html.Attributes exposing (..)
import Utils.Spa as Spa


view : Spa.LayoutContext msg -> Html msg
view { page, route, global } =
    let
        focus =
            case global.user of
                Global.LoggedOut session ->
                    session.node_focus

                Global.LoggedIn session _ ->
                    session.node_focus
    in
    page
