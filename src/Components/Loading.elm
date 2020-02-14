module Components.Loading exposing (Status(..), icon, showWhatsup, slowTreshold)

import Components.Asset as Asset
import Html exposing (Html, img, text)
import Html.Attributes exposing (alt, height, src, width)
import Process
import Task



-- Viewer


icon : Html msg
icon =
    img
        [ src Asset.loading
        , width 64
        , height 64
        , alt "Loading..."
        ]
        []


error : String -> Html msg
error str =
    text ("Error loading " ++ str ++ ".")



-- Logics


type Status a
    = Loading
    | LoadingSlowly
    | Failed
    | Loaded a


slowTreshold : Task.Task x ()
slowTreshold =
    Process.sleep 500


showWhatsup : Html msg -> Status a -> Html msg
showWhatsup htmlMsg status =
    case status of
        Loading ->
            text ""

        LoadingSlowly ->
            icon

        Failed ->
            error "data"

        Loaded _ ->
            htmlMsg
