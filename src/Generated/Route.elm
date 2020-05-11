module Generated.Route exposing
    ( Route(..)
    , fromUrl
    , toHref
    )

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Top
    | Login
    | Logout
    | NotFound
    | Signup
    | Test_Testa
    | Org_Dynamic { param1 : String }
    | User_Dynamic { param1 : String }
    | Org_Dynamic_Dynamic { param1 : String, param2 : String }


fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse routes


routes : Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map Top Parser.top
        , Parser.map Login (Parser.s "login")
        , Parser.map Logout (Parser.s "logout")
        , Parser.map NotFound (Parser.s "not-found")
        , Parser.map Signup (Parser.s "signup")
        , Parser.map Test_Testa (Parser.s "test" </> Parser.s "testa")
        , (Parser.s "org" </> Parser.string)
          |> Parser.map (\param1 -> { param1 = param1 })
          |> Parser.map Org_Dynamic
        , (Parser.s "user" </> Parser.string)
          |> Parser.map (\param1 -> { param1 = param1 })
          |> Parser.map User_Dynamic
        , (Parser.s "org" </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 -> { param1 = param1, param2 = param2 })
          |> Parser.map Org_Dynamic_Dynamic
        ]


toHref : Route -> String
toHref route =
    let
        segments : List String
        segments =
            case route of
                Top ->
                    []
                
                Login ->
                    [ "login" ]
                
                Logout ->
                    [ "logout" ]
                
                NotFound ->
                    [ "not-found" ]
                
                Signup ->
                    [ "signup" ]
                
                Test_Testa ->
                    [ "test", "testa" ]
                
                Org_Dynamic { param1 } ->
                    [ "org", param1 ]
                
                User_Dynamic { param1 } ->
                    [ "user", param1 ]
                
                Org_Dynamic_Dynamic { param1, param2 } ->
                    [ "org", param1, param2 ]
    in
    segments
        |> String.join "/"
        |> String.append "/"