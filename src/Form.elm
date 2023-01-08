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


module Form exposing (..)

import Dict
import Bulk exposing (UserForm)
import ModelSchema exposing (Post)


isUsersSendable : List UserForm -> Bool
isUsersSendable user_forms =
    user_forms |> List.any (\u -> u.username /= "" || u.email /= "")


isPostEmpty : List String -> Post -> Bool
isPostEmpty keys post =
    keys
        |> List.map
            (\k ->
                Dict.get k post
                    |> Maybe.withDefault ""
            )
        |> List.all (\x -> String.length x == 0)


isPostSendable : List String -> Post -> Bool
isPostSendable keys post =
    keys
        |> List.map
            (\k ->
                Dict.get k post
                    |> Maybe.withDefault ""
                    |> String.trim
            )
        |> List.all (\x -> String.length x > 0)


isPostSendableOr : List String -> Post -> Bool
isPostSendableOr keys post =
    keys
        |> List.map
            (\k -> Dict.get k post)
        |> List.any (\x -> x /= Nothing)


isLoginSendable : Post -> Bool
isLoginSendable post =
    isPostSendable [ "username", "password" ] post


isSignupSendable : Post -> Bool
isSignupSendable post =
    isPostSendable [ "username", "email", "password" ] post


isPasswordResetSendable : Post -> Bool
isPasswordResetSendable post =
    isPostSendable [ "email", "challenge" ] post


isPasswordReset2Sendable : Post -> Bool
isPasswordReset2Sendable post =
    isPostSendable [ "password", "password2" ] post
