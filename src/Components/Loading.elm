module Components.Loading exposing (formatTime, slowTreshold, spinner, viewErrors)

--import DateTime exposing (Calendar, DateTime, getDate, getTime)

import Components.Asset as Asset
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (alt, class, height, src, width)
import Iso8601 exposing (toTime)
import Process
import Task
import Time exposing (..)



-- Logics


slowTreshold : Task.Task x ()
slowTreshold =
    -- before passing to Slow Loading status
    Process.sleep 500



-- Viewer


spinner : Html msg
spinner =
    img
        [ src Asset.loading
        , width 64
        , height 64
        , alt "Loading..."
        ]
        []


viewErrors : String -> Html msg
viewErrors errMsg =
    div [ class "box has-background-danger" ] [ text errMsg ]



-- Utils


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
