{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2024 Fractale Co

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


module Loading exposing (..)

import Assets as A
import Extra exposing (upH)
import Graphql.Http as GqlHttp
import Html exposing (Html, a, div, img, span, text)
import Html.Attributes exposing (alt, attribute, class, height, src, width)
import Http
import Json.Decode as JD
import Maybe exposing (withDefault)
import RemoteData exposing (RemoteData)
import Text as T



--
-- ModalData
--


type alias ModalData =
    { reset : Bool, link : String }



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


type alias RestData a =
    RemoteData (HttpError String) a


fromResult : Result (HttpError String) a -> GqlData a
fromResult result =
    case result of
        Err e ->
            Failure [ errorHttpToString e ]

        Ok x ->
            Success x



--
-- Remote Errors
--


type alias ErrorData =
    List String


type alias ErrorAuth =
    { errors : List ErrorDebug }


type alias ErrorDebug =
    { message : String
    , location : Maybe String
    , path : Maybe String
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


errorsDecoder : JD.Decoder ErrorAuth
errorsDecoder =
    JD.map ErrorAuth <|
        JD.field "errors" <|
            JD.list errorDecoder


errorDecoder : JD.Decoder ErrorDebug
errorDecoder =
    JD.map3 ErrorDebug
        (JD.field "message" JD.string)
        (JD.maybe <| JD.field "location" JD.string)
        (JD.maybe <| JD.field "path" JD.string)


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
                        case JD.decodeString errorsDecoder body of
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
    let
        err =
            case httpError of
                GqlHttp.BadUrl message ->
                    BadUrl message

                GqlHttp.Timeout ->
                    Timeout

                GqlHttp.NetworkError ->
                    NetworkError

                GqlHttp.BadStatus metadata body ->
                    BadStatus metadata.statusCode body

                GqlHttp.BadPayload message ->
                    BadBody (JD.errorToString message)
    in
    errorHttpToString err


toErrorData : HttpError String -> ErrorData
toErrorData httpErr =
    [ errorHttpToString httpErr ]



-- Viewer


spinner : Html msg
spinner =
    img
        [ src A.loading
        , width 26
        , height 26
        , alt (upH T.loading ++ "..")
        ]
        []


loadingDiv : Html msg
loadingDiv =
    div [ class "spinner" ] []


loadingSpin : Bool -> Html msg
loadingSpin hasLoading =
    if hasLoading then
        span [ class "spinner2 is-small", attribute "style" "left: 13px;" ] []

    else
        text ""


loadingSpinRight : Bool -> Html msg
loadingSpinRight hasLoading =
    if hasLoading then
        span [ class "mx-5 is-pulled-right spinner2 is-small" ] []

    else
        text ""



-- RequestResult / Data methods (GQL API)


isSuccess : GqlData a -> Bool
isSuccess data =
    case data of
        Success _ ->
            True

        _ ->
            False


isFailure : GqlData a -> Bool
isFailure data =
    case data of
        Failure _ ->
            True

        _ ->
            False


isLoading : GqlData a -> Bool
isLoading data =
    case data of
        Loading ->
            True

        LoadingSlowly ->
            True

        _ ->
            False


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


withMaybeMapData : (a -> b) -> RequestResult e a -> Maybe b
withMaybeMapData resMap result =
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


fromMaybeData : Maybe a -> RequestResult e a -> RequestResult e a
fromMaybeData ma type_ =
    ma
        |> Maybe.map (\x -> Success x)
        |> withDefault type_


withMaybeSlowly : RequestResult e a -> RequestResult e a
withMaybeSlowly result =
    if result == Loading then
        LoadingSlowly

    else
        result



--
-- Idem but for RestData (Rest API) :S
--


isSuccessRest : RestData a -> Bool
isSuccessRest data =
    case data of
        RemoteData.Success _ ->
            True

        _ ->
            False


isFailureRest : RestData a -> Bool
isFailureRest data =
    case data of
        RemoteData.Failure _ ->
            True

        _ ->
            False


isLoadingRest : RestData a -> Bool
isLoadingRest data =
    case data of
        RemoteData.Loading ->
            True

        _ ->
            False


withDefaultDataRest : a -> RemoteData e a -> a
withDefaultDataRest default result =
    case result of
        RemoteData.Success d ->
            d

        _ ->
            default


withMaybeDataRest : RemoteData e a -> Maybe a
withMaybeDataRest result =
    case result of
        RemoteData.Success d ->
            Just d

        _ ->
            Nothing


withMapDataRest : (a -> b) -> RemoteData e a -> RemoteData e b
withMapDataRest resMap result =
    case result of
        RemoteData.Success d ->
            RemoteData.Success (resMap d)

        RemoteData.Failure err ->
            RemoteData.Failure err

        RemoteData.Loading ->
            RemoteData.Loading

        RemoteData.NotAsked ->
            RemoteData.NotAsked


fromMaybeDataRest : Maybe a -> RemoteData e a -> RemoteData e a
fromMaybeDataRest ma type_ =
    ma
        |> Maybe.map (\x -> RemoteData.Success x)
        |> withDefault type_


mapRest2Gql : (a -> b) -> RestData a -> GqlData b
mapRest2Gql fun input =
    case input of
        RemoteData.Success data ->
            Success <| fun data

        RemoteData.Loading ->
            Loading

        RemoteData.NotAsked ->
            NotAsked

        RemoteData.Failure err ->
            Failure (toErrorData err)


rest2Gql : RestData a -> GqlData a
rest2Gql data =
    mapRest2Gql identity data
