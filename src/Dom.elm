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


module Dom exposing (..)

import Extra exposing (ternary)
import Json.Decode as JD


{-| Detect keyboard input
-}
key k msg =
    JD.field "key" JD.string
        |> JD.andThen
            (\x ->
                ternary (x == k) (JD.succeed msg) (JD.fail "nothing")
            )


{-| Detect click outside a given object ID
-}
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

        -- ignore if a modal is open (@DEBUG: do not work, never get inside)
        , JD.field "class" (JD.list JD.string)
            |> JD.andThen
                (\cls ->
                    ternary (List.member "has-modal-active" cls)
                        -- found match by id
                        (JD.succeed False)
                        -- try next decoder
                        (JD.fail "continue")
                )
        , JD.lazy (\_ -> isOutside targetId |> JD.field "parentNode")

        -- fallback if all previous decoders failed
        , JD.succeed True
        ]
