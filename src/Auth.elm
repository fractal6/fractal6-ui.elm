{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2023 Fractale Co

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


module Auth exposing (ErrState(..), parseErr, parseErr2)

import Json.Decode as JD
import Loading exposing (GqlData, RequestResult(..), RestData, errorsDecoder, toErrorData)
import RemoteData
import String exposing (contains, startsWith)
import String.Extra as SE
import Text as T



--
-- Model
--


type ErrState a
    = Authenticate
    | RefreshToken Int
    | OkAuth a
    | NoErr
    | DuplicateErr
    | NameTooLong
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

    else if startsWith "duplicate error" message || contains "already exists for field" message then
        DuplicateErr

    else if startsWith "name too long" message then
        NameTooLong

    else if startsWith "access denied" message || contains "refresh token" message || contains "authorization failed" message then
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

        _ ->
            NoErr


{-| For HTTP response
-}
parseErr2 : RestData a -> Int -> ErrState a
parseErr2 data trial =
    case data of
        RemoteData.Success d ->
            OkAuth d

        RemoteData.Failure error ->
            case List.head (toErrorData error) of
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

        _ ->
            NoErr
