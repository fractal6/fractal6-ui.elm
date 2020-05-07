module Components.Loading exposing (HttpError, WebData, expectJson, spinner, viewErrors, viewHttpErrors)

--import DateTime exposing (Calendar, DateTime, getDate, getTime)

import Components.Asset as Asset
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (alt, class, height, src, width)
import Http
import Json.Decode as JD
import RemoteData exposing (RemoteData)



--
-- Model
--


type alias WebData a =
    RemoteData (HttpError String) a


type alias ErrorAuth =
    { user_ctx :
        ErrorMsg
    }


type alias ErrorMsg =
    { field : String
    , msg : String
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
        JD.field "user_ctx" <|
            JD.map2 ErrorMsg
                (JD.field "field" JD.string)
                (JD.field "msg" JD.string)


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
                                err.user_ctx.msg

                            Err errJD ->
                                "unknown error;\n" ++ JD.errorToString errJD
                in
                "Unauthaurized: " ++ errMsg

            else
                "Request failed with status code: " ++ String.fromInt statusCode ++ "!!!" ++ body

        BadBody message ->
            message



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


viewHttpErrors : HttpError String -> Html msg
viewHttpErrors httpError =
    errorHttpToString httpError |> viewErrors
