module Components.Loading exposing (HttpError, WebData, expectJson, spinner, toErrorData, viewAuthNeeded, viewErrors, viewHttpErrors)

--import DateTime exposing (Calendar, DateTime, getDate, getTime)

import Components.Asset as Asset
import Html exposing (Html, div, img, p, text)
import Html.Attributes exposing (alt, class, height, src, width)
import Http
import Json.Decode as JD
import ModelOrg exposing (ErrorData)
import RemoteData exposing (RemoteData)



--
-- Model
--


type alias WebData a =
    RemoteData (HttpError String) a


type alias ErrorAuth =
    { errors : List ErrorDebug }


type alias ErrorDebug =
    { message : String
    , location : String
    }


type HttpError body
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int body
      --| BadBody Http.Metadata body String
    | BadBody String



-- Logics


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


errorDecoder : JD.Decoder ErrorAuth
errorDecoder =
    JD.map ErrorAuth <|
        JD.field "errors" <|
            (JD.list <|
                JD.map2 ErrorDebug
                    (JD.field "message" JD.string)
                    (JD.field "location" JD.string)
            )


errorHttpToString : HttpError String -> String
errorHttpToString httpError =
    case httpError of
        BadUrl message ->
            message

        Timeout ->
            "Server is taking too long to respond. Please try again later."

        NetworkError ->
            "Unable to reach server."

        BadStatus statusCode body ->
            if statusCode == 401 then
                let
                    errMsg =
                        case JD.decodeString errorDecoder body of
                            Ok err ->
                                err.errors |> List.map (\e -> e.message) |> String.join "\n"

                            Err errJD ->
                                "unknown error;\n" ++ JD.errorToString errJD
                in
                "Unauthaurized: " ++ errMsg

            else
                "Request failed with status code: " ++ String.fromInt statusCode ++ "!!!" ++ body

        BadBody message ->
            message


toErrorData : HttpError String -> ErrorData
toErrorData httpErr =
    [ errorHttpToString httpErr ]



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


viewAuthNeeded : Html msg
viewAuthNeeded =
    div [ class "box has-background-info" ]
        [ p [] [ text "Please login or create an account to perform this action." ]
        ]


viewErrors : ErrorData -> Html msg
viewErrors errMsg =
    List.map (\e -> p [] [ text e ]) errMsg
        |> div [ class "box has-background-danger" ]


viewHttpErrors : HttpError String -> Html msg
viewHttpErrors httpErr =
    httpErr
        |> toErrorData
        |> viewErrors
