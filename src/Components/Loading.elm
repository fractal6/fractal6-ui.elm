module Components.Loading exposing (Status(..), showMaybeError, slowTreshold)

import Components.Asset as Asset
import Html exposing (Html, img, text)
import Html.Attributes exposing (alt, height, src, width)
import Process
import Task



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


showError : String -> Html msg
showError err =
    text ("Error: " ++ err)



-- Logics


type Status errorMsg data
    = Loading
    | LoadingSlowly
    | Failed errorMsg
    | Loaded data


slowTreshold : Task.Task x ()
slowTreshold =
    Process.sleep 500


showMaybeError : Status String data -> Html msg
showMaybeError status =
    case status of
        Loaded _ ->
            text ""

        Loading ->
            text ""

        LoadingSlowly ->
            spinner

        Failed err ->
            showError err
