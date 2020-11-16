module Components.Loading exposing (..)

--import DateTime exposing (Calendar, DateTime, getDate, getTime)

import Components.Asset as Asset
import Extra exposing (toUp1)
import Extra.Events exposing (onClickPD)
import Generated.Route as Route exposing (Route)
import Graphql.Http as GqlHttp
import Html exposing (Html, a, button, div, img, p, span, text)
import Html.Attributes exposing (alt, class, height, href, src, width)
import Http
import Json.Decode as JD
import Maybe exposing (withDefault)
import RemoteData exposing (RemoteData)



--
-- Remote Data
--


type RequestResult errors data
    = Success data
    | Failure errors
    | Loading
    | LoadingSlowly
    | NotAsked


type alias GqlData a =
    RequestResult ErrorData a


type alias WebData a =
    RemoteData (HttpError String) a



--
-- Remote Errors
--


type alias ErrorData =
    List String


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

            else if body == "" then
                "Request failed with status code: " ++ String.fromInt statusCode

            else
                body

        BadBody message ->
            message


errorGraphQLHttpToString : GqlHttp.HttpError -> String
errorGraphQLHttpToString httpError =
    case httpError of
        GqlHttp.BadUrl message ->
            message

        GqlHttp.Timeout ->
            "Server is taking too long to respond. Please try again later."

        GqlHttp.NetworkError ->
            "Unable to reach server."

        GqlHttp.BadStatus metadata body ->
            if metadata.statusCode == 401 then
                let
                    errMsg =
                        case JD.decodeString errorDecoder body of
                            Ok err ->
                                err.errors |> List.map (\e -> e.message) |> String.join "\n"

                            Err errJD ->
                                "unknown error;\n" ++ JD.errorToString errJD
                in
                "Unauthaurized: " ++ errMsg

            else if body == "" then
                "Request failed with status code: " ++ String.fromInt metadata.statusCode

            else
                body

        GqlHttp.BadPayload body ->
            "Graphql Http JSON decoder unexpected error."


toErrorData : HttpError String -> ErrorData
toErrorData httpErr =
    [ errorHttpToString httpErr ]



-- Viewer


spinner : Html msg
spinner =
    img
        [ src Asset.loading
        , width 26
        , height 26
        , alt "Loading..."
        ]
        []


loadingSpin : Bool -> Html msg
loadingSpin isLoading =
    if isLoading then
        span [ class "spinner2 is-small" ] []

    else
        text ""


viewGqlErrors : ErrorData -> Html msg
viewGqlErrors errMsg =
    errMsg
        |> List.map
            (\e ->
                let
                    err =
                        case JD.decodeString errorDecoder e of
                            Ok err_ ->
                                err_.errors
                                    |> List.head
                                    |> Maybe.map (\x -> toUp1 x.message)
                                    |> withDefault e

                            Err err_ ->
                                e
                in
                p [] [ text err ]
            )
        |> div [ class "box has-background-danger" ]


viewHttpErrors : HttpError String -> Html msg
viewHttpErrors httpErr =
    httpErr
        |> toErrorData
        |> viewGqlErrors


viewAuthNeeded : (String -> msg) -> Html msg
viewAuthNeeded forward =
    div [ class "modal-card" ]
        [ div [ class "modal-card-head" ] [ div [ class "modal-card-title is-size-6" ] [ text "Authentication needed" ] ]
        , div [ class "modal-card-body" ]
            [ p []
                [ text "Please "
                , button
                    [ class "button is-small is-success", onClickPD (forward (Route.toHref Route.Login)) ]
                    [ text "Login" ]
                , text " or "
                , button
                    [ class "button is-small is-primary", onClickPD (forward (Route.toHref Route.Signup)) ]
                    [ text "Signup" ]
                , text " to perform this action."
                ]
            ]
        ]


viewRoleNeeded : ErrorData -> Html msg
viewRoleNeeded errMsg =
    div [ class "modal-card" ]
        [ div [ class "modal-card-head has-background-warning" ] [ div [ class "modal-card-title is-size-6" ] [ text "Authorization needed" ] ]
        , div [ class "modal-card-body" ] <|
            List.map
                (\e ->
                    p [] [ text e ]
                )
                errMsg
        ]



-- RequestResult / Data methods


withDefaultData : a -> RequestResult e a -> a
withDefaultData default result =
    case result of
        Success d ->
            d

        _ ->
            default


withMaybeData : RequestResult e a -> Maybe a
withMaybeData result =
    case result of
        Success d ->
            Just d

        _ ->
            Nothing


withMaybeDataMap : (a -> b) -> RequestResult e a -> Maybe b
withMaybeDataMap resMap result =
    case result of
        Success d ->
            Just (resMap d)

        _ ->
            Nothing


withMapData : (a -> b) -> RequestResult e a -> RequestResult e b
withMapData resMap result =
    case result of
        Success d ->
            Success (resMap d)

        Failure err ->
            Failure err

        Loading ->
            Loading

        LoadingSlowly ->
            LoadingSlowly

        NotAsked ->
            NotAsked
