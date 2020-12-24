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
    | Explore
    | Dynamic { param1 : String }
    | New_Orga
    | T_Dynamic { param1 : String }
    | M_Dynamic { param1 : String }
    | S_Dynamic { param1 : String }
    | User_Dynamic { param1 : String }
    | O_Dynamic { param1 : String }
    | T_Dynamic_Dynamic { param1 : String, param2 : String }
    | O_Dynamic_Dynamic { param1 : String, param2 : String }
    | Tension_Dynamic_Dynamic { param1 : String, param2 : String }
    | M_Dynamic_Dynamic { param1 : String, param2 : String }
    | S_Dynamic_Dynamic { param1 : String, param2 : String }
    | Tension_Dynamic_Dynamic_Action { param1 : String, param2 : String }
    | T_Dynamic_Dynamic_Dynamic { param1 : String, param2 : String, param3 : String }
    | O_Dynamic_Dynamic_Dynamic { param1 : String, param2 : String, param3 : String }
    | M_Dynamic_Dynamic_Dynamic { param1 : String, param2 : String, param3 : String }
    | S_Dynamic_Dynamic_Dynamic { param1 : String, param2 : String, param3 : String }


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
        , Parser.map Explore (Parser.s "explore")
        , (Parser.string)
          |> Parser.map (\param1 -> { param1 = param1 })
          |> Parser.map Dynamic
        , Parser.map New_Orga (Parser.s "new" </> Parser.s "orga")
        , (Parser.s "t" </> Parser.string)
          |> Parser.map (\param1 -> { param1 = param1 })
          |> Parser.map T_Dynamic
        , (Parser.s "m" </> Parser.string)
          |> Parser.map (\param1 -> { param1 = param1 })
          |> Parser.map M_Dynamic
        , (Parser.s "s" </> Parser.string)
          |> Parser.map (\param1 -> { param1 = param1 })
          |> Parser.map S_Dynamic
        , (Parser.s "user" </> Parser.string)
          |> Parser.map (\param1 -> { param1 = param1 })
          |> Parser.map User_Dynamic
        , (Parser.s "o" </> Parser.string)
          |> Parser.map (\param1 -> { param1 = param1 })
          |> Parser.map O_Dynamic
        , (Parser.s "t" </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 -> { param1 = param1, param2 = param2 })
          |> Parser.map T_Dynamic_Dynamic
        , (Parser.s "o" </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 -> { param1 = param1, param2 = param2 })
          |> Parser.map O_Dynamic_Dynamic
        , (Parser.s "tension" </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 -> { param1 = param1, param2 = param2 })
          |> Parser.map Tension_Dynamic_Dynamic
        , (Parser.s "m" </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 -> { param1 = param1, param2 = param2 })
          |> Parser.map M_Dynamic_Dynamic
        , (Parser.s "s" </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 -> { param1 = param1, param2 = param2 })
          |> Parser.map S_Dynamic_Dynamic
        , (Parser.s "tension" </> Parser.string </> Parser.string </> Parser.s "action")
          |> Parser.map (\param1 param2 -> { param1 = param1, param2 = param2 })
          |> Parser.map Tension_Dynamic_Dynamic_Action
        , (Parser.s "t" </> Parser.string </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 param3 -> { param1 = param1, param2 = param2, param3 = param3 })
          |> Parser.map T_Dynamic_Dynamic_Dynamic
        , (Parser.s "o" </> Parser.string </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 param3 -> { param1 = param1, param2 = param2, param3 = param3 })
          |> Parser.map O_Dynamic_Dynamic_Dynamic
        , (Parser.s "m" </> Parser.string </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 param3 -> { param1 = param1, param2 = param2, param3 = param3 })
          |> Parser.map M_Dynamic_Dynamic_Dynamic
        , (Parser.s "s" </> Parser.string </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 param3 -> { param1 = param1, param2 = param2, param3 = param3 })
          |> Parser.map S_Dynamic_Dynamic_Dynamic
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
                
                Explore ->
                    [ "explore" ]
                
                Dynamic { param1 } ->
                    [ param1 ]
                
                New_Orga ->
                    [ "new", "orga" ]
                
                T_Dynamic { param1 } ->
                    [ "t", param1 ]
                
                M_Dynamic { param1 } ->
                    [ "m", param1 ]
                
                S_Dynamic { param1 } ->
                    [ "s", param1 ]
                
                User_Dynamic { param1 } ->
                    [ "user", param1 ]
                
                O_Dynamic { param1 } ->
                    [ "o", param1 ]
                
                T_Dynamic_Dynamic { param1, param2 } ->
                    [ "t", param1, param2 ]
                
                O_Dynamic_Dynamic { param1, param2 } ->
                    [ "o", param1, param2 ]
                
                Tension_Dynamic_Dynamic { param1, param2 } ->
                    [ "tension", param1, param2 ]
                
                M_Dynamic_Dynamic { param1, param2 } ->
                    [ "m", param1, param2 ]
                
                S_Dynamic_Dynamic { param1, param2 } ->
                    [ "s", param1, param2 ]
                
                Tension_Dynamic_Dynamic_Action { param1, param2 } ->
                    [ "tension", param1, param2, "action" ]
                
                T_Dynamic_Dynamic_Dynamic { param1, param2, param3 } ->
                    [ "t", param1, param2, param3 ]
                
                O_Dynamic_Dynamic_Dynamic { param1, param2, param3 } ->
                    [ "o", param1, param2, param3 ]
                
                M_Dynamic_Dynamic_Dynamic { param1, param2, param3 } ->
                    [ "m", param1, param2, param3 ]
                
                S_Dynamic_Dynamic_Dynamic { param1, param2, param3 } ->
                    [ "s", param1, param2, param3 ]
    in
    segments
        |> String.join "/"
        |> String.append "/"