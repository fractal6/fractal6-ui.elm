module Dom exposing (..)

import Extra exposing (ternary)
import Json.Decode as JD



--import DOM


key k msg =
    JD.field "key" JD.string
        |> JD.andThen
            (\x ->
                ternary (x == k) (JD.succeed msg) (JD.fail "nothing")
            )


outsideClickClose : String -> msg -> JD.Decoder msg
outsideClickClose targetId msg =
    -- like: DOM.target (isOutside targetId)
    JD.field "target" (isOutside targetId)
        |> JD.andThen
            (\isOut ->
                if isOut then
                    -- Ensure right click
                    JD.oneOf
                        [ JD.field "button" JD.int
                            |> JD.andThen
                                (\button ->
                                    if button == 0 then
                                        JD.succeed msg

                                    else
                                        JD.fail "non left click"
                                )
                        ]

                else
                    JD.fail "outside click fail"
            )


isOutside : String -> JD.Decoder Bool
isOutside targetId =
    JD.oneOf
        [ JD.field "id" JD.string
            |> JD.andThen
                (\id ->
                    ternary (targetId == id)
                        -- found match by id
                        (JD.succeed False)
                        -- try next decoder
                        (JD.fail "continue")
                )

        --, DOM.hasClass targetId |> JD.andThen (\x -> ternary x (JD.succeed False) (JD.fail "continue"))
        , JD.lazy (\_ -> isOutside targetId |> JD.field "parentNode")

        -- fallback if all previous decoders failed
        , JD.succeed True
        ]
