module Extra exposing (ternary)

import Dict exposing (Dict)


ternary test positive negative =
    case test of
        True ->
            positive

        False ->
            negative
