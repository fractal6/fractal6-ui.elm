module Date exposing (formatTime)

import Iso8601 exposing (toTime)
import Time exposing (..)



--import DateTime exposing (Calendar, DateTime, getDate, getTime)


formatTime : String -> String
formatTime timePosix =
    let
        t =
            case toTime timePosix of
                Ok v ->
                    v

                default ->
                    Time.millisToPosix 0

        --dt =  DateTime.fromPosix t
        --data = getDate dt
        --time = getTime dt
    in
    [ Time.toDay utc t |> String.fromInt
    , Time.toMonth utc t |> toShortMonth

    --, Time.toYear utc t |> String.fromInt
    ]
        |> String.join " "


toShortMonth : Month -> String
toShortMonth month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"
