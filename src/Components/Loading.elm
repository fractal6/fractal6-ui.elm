module Components.Loading exposing (slowTreshold, spinner, viewErrors)

--import DateTime exposing (Calendar, DateTime, getDate, getTime)

import Components.Asset as Asset
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (alt, class, height, src, width)
import Process
import Task



-- Logics


slowTreshold : Float -> Task.Task x ()
slowTreshold t =
    -- before passing to Slow Loading status
    Process.sleep t



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
