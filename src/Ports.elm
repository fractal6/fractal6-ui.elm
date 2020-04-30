port module Ports exposing
    ( bulma_driver
    , init_graphPack
    , log
    , toggle_theme
    )

import Json.Encode as Json



-- Ingoing
--port getBackTimeFrom : (String -> msg) -> Sub msg
-- How to use that ? (dayjs tipically or other use of a general library (updated nested data?)
-- Outgoing


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


init_graphPack : String -> Cmd msg
init_graphPack data =
    outgoing
        { action = "INIT_GRAPHPACK"
        , data = Json.string data
        }
