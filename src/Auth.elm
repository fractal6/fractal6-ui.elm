module Auth exposing (ErrState(..), parseErr, parseErr2)

import Assets as A
import Loading exposing (GqlData, RequestResult(..), WebData, errorsDecoder, toErrorData)
import Html exposing (Html, a, br, button, div, i, input, label, p, span, text)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, name, placeholder, required, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Maybe exposing (withDefault)
import RemoteData exposing (RemoteData)
import String exposing (contains, startsWith)
import String.Extra as SE
import Text as T exposing (textH, textT, upH)



--
-- Model
--


type ErrState a
    = Authenticate
    | RefreshToken Int
    | OkAuth a
    | NoErr
    | DuplicateErr
    | UnknownErr



--
-- Logics
--


{-| Convert an error message to an error type
-}
messageToErrState : String -> Int -> ErrState a
messageToErrState message_ trial =
    let
        message =
            String.toLower message_
    in
    if contains "token is expired" message || contains "no token found" message then
        Authenticate

    else if startsWith "duplicate error" message then
        DuplicateErr

    else if contains "already exists for field" message then
        DuplicateErr

    else if startsWith "access denied" message || contains "refresh token" message then
        if trial == 0 then
            RefreshToken (trial + 1)

        else
            UnknownErr

    else
        UnknownErr


{-| For GQL Response
-}
parseErr : GqlData a -> Int -> ErrState a
parseErr data trial =
    case data of
        Success d ->
            OkAuth d

        Failure err ->
            if List.length err == 1 then
                case List.head err of
                    Just err_ ->
                        let
                            gqlErr =
                                err_
                                    |> String.replace "\n" ""
                                    |> SE.rightOf "{"
                                    |> SE.insertAt "{" 0
                                    |> JD.decodeString errorsDecoder
                        in
                        case gqlErr of
                            Ok errGql ->
                                case List.head errGql.errors of
                                    Just e ->
                                        messageToErrState e.message trial

                                    Nothing ->
                                        UnknownErr

                            Err errJD ->
                                messageToErrState err_ trial

                    Nothing ->
                        UnknownErr

            else
                UnknownErr

        _ ->
            NoErr


{-| For HTTP response
-}
parseErr2 : WebData a -> Int -> ErrState a
parseErr2 data trial =
    case data of
        RemoteData.Success d ->
            OkAuth d

        RemoteData.Failure error ->
            let
                err =
                    toErrorData error
            in
            if List.length err == 1 then
                case List.head err of
                    Just err_ ->
                        let
                            gqlErr =
                                err_
                                    |> String.replace "\n" ""
                                    |> SE.rightOf "{"
                                    |> SE.insertAt "{" 0
                                    |> JD.decodeString errorsDecoder
                        in
                        case gqlErr of
                            Ok errGql ->
                                case List.head errGql.errors of
                                    Just e ->
                                        messageToErrState e.message trial

                                    Nothing ->
                                        UnknownErr

                            Err errJD ->
                                messageToErrState err_ trial

                    Nothing ->
                        UnknownErr

            else
                UnknownErr

        _ ->
            NoErr
