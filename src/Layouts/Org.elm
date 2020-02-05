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
    page
