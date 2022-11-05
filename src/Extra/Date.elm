{-
  Fractale - Self-organisation for humans.
  Copyright (C) 2022 Fractale Co

  This file is part of Fractale.

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
-}

module Extra.Date exposing (diffTime, formatDate)

import Date
import Fractal.Enum.Lang as Lang
import Iso8601 exposing (toTime)
import Text as T
import Time exposing (Posix, posixToMillis, utc)
import Time.Distance as Distance
import Time.Distance.I18n as I18n
import Time.Distance.Types exposing (Locale)


{-| Return the delta in milli seconds
-}
diffTime : Time.Posix -> Time.Posix -> Int
diffTime new old =
    posixToMillis new - posixToMillis old



{-
   Time Reference:

      fromPosix utc (millisToPosix 0)
          == fromCalendarDate 1970 Jan 1
-}


formatDate : Lang.Lang -> Posix -> String -> String
formatDate lang now timePosix =
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
    if Date.diff Date.Days nowDate date <= 31 then
        --Distance.inWords time now
        Distance.inWordsWithConfig { withAffix = True } (toTimeI18n lang) time now

    else if Date.diff Date.Days nowDate date <= 365 then
        formatCurrentYear lang time

    else
        formatPriorYear lang time


{-| Format date as "the 18 Dec"
-}
formatCurrentYear : Lang.Lang -> Posix -> String
formatCurrentYear lang date =
    [ T.the
    , Time.toDay utc date |> String.fromInt
    , Time.toMonth utc date |> toShortMonth lang
    ]
        |> String.join " "


{-| Format date as "the 18 Dec, 2042"
-}
formatPriorYear : Lang.Lang -> Posix -> String
formatPriorYear lang date =
    [ T.the
    , Time.toDay utc date |> String.fromInt
    , (Time.toMonth utc date |> toShortMonth lang) ++ ","
    , Time.toYear utc date |> String.fromInt
    ]
        |> String.join " "


toTimeI18n : Lang.Lang -> Locale
toTimeI18n lang =
    case lang of
        Lang.En ->
            I18n.en

        Lang.Fr ->
            I18n.fr


toShortMonth : Lang.Lang -> Time.Month -> String
toShortMonth lang month =
    case lang of
        Lang.En ->
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

        Lang.Fr ->
            case month of
                Time.Jan ->
                    "Janv"

                Time.Feb ->
                    "Févr"

                Time.Mar ->
                    "Mars"

                Time.Apr ->
                    "Avr"

                Time.May ->
                    "Mai"

                Time.Jun ->
                    "Juin"

                Time.Jul ->
                    "Juill"

                Time.Aug ->
                    "Août"

                Time.Sep ->
                    "Sept"

                Time.Oct ->
                    "Oct"

                Time.Nov ->
                    "Nov"

                Time.Dec ->
                    "Dec"
