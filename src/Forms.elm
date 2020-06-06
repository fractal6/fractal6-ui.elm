module Forms exposing (isPostSendable)

import Dict
import ModelSchema exposing (Post)


{-| Test require fields
-}
isPostSendable : List String -> Post -> Bool
isPostSendable keys post =
    keys
        |> List.map
            (\k ->
                Dict.get k post
                    |> Maybe.withDefault ""
            )
        |> List.all (\x -> String.length x > 0)
