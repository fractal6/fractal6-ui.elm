port module Ports exposing
    ( bulma_driver
    , init_circlePacking
    , log
    , toggle_theme
    )

import Json.Encode as Json


port outgoing : { action : String, data : Json.Value } -> Cmd msg


log : String -> Cmd msg
log message =
    outgoing
        { action = "LOG"
        , data = Json.string message
        }


bulma_driver : String -> Cmd msg
bulma_driver eltId =
    outgoing
        { action = "BULMA"
        , data = Json.string eltId
        }


toggle_theme : Cmd msg
toggle_theme =
    outgoing
        { action = "TOGGLE_TH"
        , data = Json.string ""
        }


init_circlePacking : String -> Cmd msg
init_circlePacking data =
    outgoing
        { action = "INIT_CIRCLEPACKING"
        , data = Json.string data
        }
