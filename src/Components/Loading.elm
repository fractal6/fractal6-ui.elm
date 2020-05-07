module Components.Loading exposing (HttpError, expectJson, slowTreshold, spinner, viewErrors, viewHttpErrors)

--import DateTime exposing (Calendar, DateTime, getDate, getTime)

import Components.Asset as Asset
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (alt, class, height, src, width)
import Http
import Json.Decode as JD
import Process
import Task


type HttpError body
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int body
    | BadBody String



--| BadBody Http.Metadata body String
-- Logics


slowTreshold : Float -> Task.Task x ()
slowTreshold t =
    -- before passing to Slow Loading status
    Process.sleep t


expectJson : (Result (HttpError String) a -> msg) -> JD.Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ metadata body ->
                    Err (BadStatus metadata.statusCode body)

                Http.GoodStatus_ metadata body ->
                    case JD.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (BadBody (JD.errorToString err))



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


viewHttpErrors : HttpError String -> String
viewHttpErrors httpError =
    case httpError of
        BadUrl message ->
            message

        Timeout ->
            "Server is taking too long to respond. Please try again later."

        NetworkError ->
            "Unable to reach server."

        BadStatus statusCode body ->
            --if statusCode ==
            "Request failed with status code: " ++ String.fromInt statusCode ++ "!!!" ++ body

        BadBody message ->
            message
