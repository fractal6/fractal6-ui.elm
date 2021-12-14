module Form exposing (..)

import Dict
import ModelCommon exposing (UserForm)
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
            )
        |> List.all (\x -> String.length x > 0)


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
