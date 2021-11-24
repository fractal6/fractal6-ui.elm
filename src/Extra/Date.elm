module Extra.Date exposing (formatDate)

import Date
import Iso8601 exposing (toTime)
import Time exposing (Posix, utc)
import Time.Distance as Distance



{-
   Time Reference:

      fromPosix utc (millisToPosix 0)
          == fromCalendarDate 1970 Jan 1
-}


formatDate : Posix -> String -> String
formatDate now timePosix =
    let
        time =
            case toTime timePosix of
                Ok v ->
                    v

                _ ->
                    Time.millisToPosix 0

        date =
            Date.fromPosix utc time

        nowDate =
            Date.fromPosix utc now
    in
    if Date.diff Date.Days nowDate date < 31 then
        Distance.inWords time now

    else if Date.diff Date.Days nowDate date <= 365 then
        formatCurrentYear time

    else
        formatPriorYear time


{-| Format date as "the 18 Dec"
-}
formatCurrentYear : Posix -> String
formatCurrentYear date =
    [ "the"
    , Time.toDay utc date |> String.fromInt
    , Time.toMonth utc date |> toShortMonth
    ]
        |> String.join " "


{-| Format date as "the 18 Dec, 2042"
-}
formatPriorYear : Posix -> String
formatPriorYear date =
    [ "the"
    , Time.toDay utc date |> String.fromInt
    , (Time.toMonth utc date |> toShortMonth) ++ ","
    , Time.toYear utc date |> String.fromInt
    ]
        |> String.join " "


toShortMonth : Time.Month -> String
toShortMonth month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"
