module Form exposing (..)

import Dict
import ModelSchema exposing (Post)


{-| Test require fields
-}
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
