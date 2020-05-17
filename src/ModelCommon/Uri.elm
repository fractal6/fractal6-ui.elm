module ModelCommon.Uri exposing (FractalBaseRoute(..), toString)


type FractalBaseRoute
    = OverviewBaseUri
    | TensionsBaseUri
    | UsersBaseUri


toString : FractalBaseRoute -> String
toString route =
    case route of
        OverviewBaseUri ->
            -- /org
            "o"

        TensionsBaseUri ->
            -- /tensions
            "t"

        UsersBaseUri ->
            -- /users
            "u"
