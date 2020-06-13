module Generated.Route exposing
    ( Route(..)
    , fromUrl
    , toHref
    )

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Top
    | Signup
    | NotFound
    | Login
    | Logout
    | O_Dynamic { param1 : String }
    | T_Dynamic { param1 : String }
    | User_Dynamic { param1 : String }
    | O_Dynamic_Dynamic { param1 : String, param2 : String }
    | T_Dynamic_Dynamic { param1 : String, param2 : String }
    | T_Dynamic_Dynamic_Dynamic { param1 : String, param2 : String, param3 : String }
    | O_Dynamic_Dynamic_Dynamic { param1 : String, param2 : String, param3 : String }


fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse routes


routes : Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map Top Parser.top
        , Parser.map Signup (Parser.s "signup")
        , Parser.map NotFound (Parser.s "not-found")
        , Parser.map Login (Parser.s "login")
        , Parser.map Logout (Parser.s "logout")
        , (Parser.s "o" </> Parser.string)
          |> Parser.map (\param1 -> { param1 = param1 })
          |> Parser.map O_Dynamic
        , (Parser.s "t" </> Parser.string)
          |> Parser.map (\param1 -> { param1 = param1 })
          |> Parser.map T_Dynamic
        , (Parser.s "user" </> Parser.string)
          |> Parser.map (\param1 -> { param1 = param1 })
          |> Parser.map User_Dynamic
        , (Parser.s "o" </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 -> { param1 = param1, param2 = param2 })
          |> Parser.map O_Dynamic_Dynamic
        , (Parser.s "t" </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 -> { param1 = param1, param2 = param2 })
          |> Parser.map T_Dynamic_Dynamic
        , (Parser.s "t" </> Parser.string </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 param3 -> { param1 = param1, param2 = param2, param3 = param3 })
          |> Parser.map T_Dynamic_Dynamic_Dynamic
        , (Parser.s "o" </> Parser.string </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 param3 -> { param1 = param1, param2 = param2, param3 = param3 })
          |> Parser.map O_Dynamic_Dynamic_Dynamic
        ]


toHref : Route -> String
toHref route =
    let
        segments : List String
        segments =
            case route of
                Top ->
                    []
                
                Signup ->
                    [ "signup" ]
                
                NotFound ->
                    [ "not-found" ]
                
                Login ->
                    [ "login" ]
                
                Logout ->
                    [ "logout" ]
                
                O_Dynamic { param1 } ->
                    [ "o", param1 ]
                
                T_Dynamic { param1 } ->
                    [ "t", param1 ]
                
                User_Dynamic { param1 } ->
                    [ "user", param1 ]
                
                O_Dynamic_Dynamic { param1, param2 } ->
                    [ "o", param1, param2 ]
                
                T_Dynamic_Dynamic { param1, param2 } ->
                    [ "t", param1, param2 ]
                
                T_Dynamic_Dynamic_Dynamic { param1, param2, param3 } ->
                    [ "t", param1, param2, param3 ]
                
                O_Dynamic_Dynamic_Dynamic { param1, param2, param3 } ->
                    [ "o", param1, param2, param3 ]
    in
    segments
        |> String.join "/"
        |> String.append "/"