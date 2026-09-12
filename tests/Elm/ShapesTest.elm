module Elm.ShapesTest exposing (tests)

import Assets.Shapes exposing (quarterPath)
import Expect
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "Quarter disc path"
        [ test "the shape used by the canvas cards" <|
            \_ ->
                quarterPath 0.06 0.06
                    |> Expect.equal "M 0.97 0.1246143706914079 L 0.97 0.9099999999999999 A 0.06 0.06 0 0 1 0.9099999999999999 0.97 L 0.1246143706914079 0.97 A 0.06 0.06 0 0 1 0.06492898687491289 0.9038636363636363 A 0.94 0.94 0 0 1 0.9038636363636363 0.06492898687491289 A 0.06 0.06 0 0 1 0.97 0.1246143706914079 Z"
        ]
