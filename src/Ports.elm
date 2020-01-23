port module Ports exposing (bulma_driver, log)

import Json.Encode as Json


port outgoing : { action : String, data : Json.Value } -> Cmd msg


log : String -> Cmd msg
log message =
    outgoing
        { action = "LOG"
        , data = Json.string message
        }


bulma_driver : Cmd msg
bulma_driver =
    outgoing
        { action = "BULMA"
        , data = Json.string ""
        }
