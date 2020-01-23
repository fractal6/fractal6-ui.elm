port module Ports exposing (log)

import Json.Encode as Json


port outgoing : { action : String, data : Json.Value } -> Cmd msg


log : String -> Cmd msg
log message =
    outgoing
        { action = "LOG"
        , data = Json.string message
        }


alert : String -> Cmd msg
alert message =
    outgoing
        { action = "ALERT"
        , data = Json.string message
        }
