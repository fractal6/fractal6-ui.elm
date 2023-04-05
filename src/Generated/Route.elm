module Generated.Route exposing
    ( Route(..)
    , fromUrl
    , toHref
    )

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Top
    | About
    | Explore
    | Help
    | Login
    | Logout
    | NotFound
    | Notifications
    | PasswordReset
    | Signup
    | Verification
    | Dynamic { param1 : String }
    | Dynamic_Settings { param1 : String }
    | New_Orga
    | M_Dynamic { param1 : String }
    | O_Dynamic { param1 : String }
    | P_Dynamic { param1 : String }
    | S_Dynamic { param1 : String }
    | T_Dynamic { param1 : String }
    | User_Dynamic { param1 : String }
    | User_Dynamic_Settings { param1 : String }
    | M_Dynamic_Dynamic { param1 : String, param2 : String }
    | O_Dynamic_Dynamic { param1 : String, param2 : String }
    | P_Dynamic_Dynamic { param1 : String, param2 : String }
    | S_Dynamic_Dynamic { param1 : String, param2 : String }
    | T_Dynamic_Dynamic { param1 : String, param2 : String }
    | Tension_Dynamic_Dynamic { param1 : String, param2 : String }
    | Tension_Dynamic_Dynamic_Action { param1 : String, param2 : String }
    | Tension_Dynamic_Dynamic_Contract { param1 : String, param2 : String }
    | M_Dynamic_Dynamic_Dynamic { param1 : String, param2 : String, param3 : String }
    | O_Dynamic_Dynamic_Dynamic { param1 : String, param2 : String, param3 : String }
    | P_Dynamic_Dynamic_Dynamic { param1 : String, param2 : String, param3 : String }
    | S_Dynamic_Dynamic_Dynamic { param1 : String, param2 : String, param3 : String }
    | T_Dynamic_Dynamic_Dynamic { param1 : String, param2 : String, param3 : String }
    | Tension_Dynamic_Dynamic_Contract_Dynamic { param1 : String, param2 : String, param3 : String }


fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse routes


routes : Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map Top Parser.top
        , Parser.map About (Parser.s "about")
        , Parser.map Explore (Parser.s "explore")
        , Parser.map Help (Parser.s "help")
        , Parser.map Login (Parser.s "login")
        , Parser.map Logout (Parser.s "logout")
        , Parser.map NotFound (Parser.s "not-found")
        , Parser.map Notifications (Parser.s "notifications")
        , Parser.map PasswordReset (Parser.s "password-reset")
        , Parser.map Signup (Parser.s "signup")
        , Parser.map Verification (Parser.s "verification")
        , (Parser.string)
          |> Parser.map (\param1 -> { param1 = param1 })
          |> Parser.map Dynamic
        , (Parser.string </> Parser.s "settings")
          |> Parser.map (\param1 -> { param1 = param1 })
          |> Parser.map Dynamic_Settings
        , Parser.map New_Orga (Parser.s "new" </> Parser.s "orga")
        , (Parser.s "m" </> Parser.string)
          |> Parser.map (\param1 -> { param1 = param1 })
          |> Parser.map M_Dynamic
        , (Parser.s "o" </> Parser.string)
          |> Parser.map (\param1 -> { param1 = param1 })
          |> Parser.map O_Dynamic
        , (Parser.s "p" </> Parser.string)
          |> Parser.map (\param1 -> { param1 = param1 })
          |> Parser.map P_Dynamic
        , (Parser.s "s" </> Parser.string)
          |> Parser.map (\param1 -> { param1 = param1 })
          |> Parser.map S_Dynamic
        , (Parser.s "t" </> Parser.string)
          |> Parser.map (\param1 -> { param1 = param1 })
          |> Parser.map T_Dynamic
        , (Parser.s "user" </> Parser.string)
          |> Parser.map (\param1 -> { param1 = param1 })
          |> Parser.map User_Dynamic
        , (Parser.s "user" </> Parser.string </> Parser.s "settings")
          |> Parser.map (\param1 -> { param1 = param1 })
          |> Parser.map User_Dynamic_Settings
        , (Parser.s "m" </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 -> { param1 = param1, param2 = param2 })
          |> Parser.map M_Dynamic_Dynamic
        , (Parser.s "o" </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 -> { param1 = param1, param2 = param2 })
          |> Parser.map O_Dynamic_Dynamic
        , (Parser.s "p" </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 -> { param1 = param1, param2 = param2 })
          |> Parser.map P_Dynamic_Dynamic
        , (Parser.s "s" </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 -> { param1 = param1, param2 = param2 })
          |> Parser.map S_Dynamic_Dynamic
        , (Parser.s "t" </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 -> { param1 = param1, param2 = param2 })
          |> Parser.map T_Dynamic_Dynamic
        , (Parser.s "tension" </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 -> { param1 = param1, param2 = param2 })
          |> Parser.map Tension_Dynamic_Dynamic
        , (Parser.s "tension" </> Parser.string </> Parser.string </> Parser.s "action")
          |> Parser.map (\param1 param2 -> { param1 = param1, param2 = param2 })
          |> Parser.map Tension_Dynamic_Dynamic_Action
        , (Parser.s "tension" </> Parser.string </> Parser.string </> Parser.s "contract")
          |> Parser.map (\param1 param2 -> { param1 = param1, param2 = param2 })
          |> Parser.map Tension_Dynamic_Dynamic_Contract
        , (Parser.s "m" </> Parser.string </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 param3 -> { param1 = param1, param2 = param2, param3 = param3 })
          |> Parser.map M_Dynamic_Dynamic_Dynamic
        , (Parser.s "o" </> Parser.string </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 param3 -> { param1 = param1, param2 = param2, param3 = param3 })
          |> Parser.map O_Dynamic_Dynamic_Dynamic
        , (Parser.s "p" </> Parser.string </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 param3 -> { param1 = param1, param2 = param2, param3 = param3 })
          |> Parser.map P_Dynamic_Dynamic_Dynamic
        , (Parser.s "s" </> Parser.string </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 param3 -> { param1 = param1, param2 = param2, param3 = param3 })
          |> Parser.map S_Dynamic_Dynamic_Dynamic
        , (Parser.s "t" </> Parser.string </> Parser.string </> Parser.string)
          |> Parser.map (\param1 param2 param3 -> { param1 = param1, param2 = param2, param3 = param3 })
          |> Parser.map T_Dynamic_Dynamic_Dynamic
        , (Parser.s "tension" </> Parser.string </> Parser.string </> Parser.s "contract" </> Parser.string)
          |> Parser.map (\param1 param2 param3 -> { param1 = param1, param2 = param2, param3 = param3 })
          |> Parser.map Tension_Dynamic_Dynamic_Contract_Dynamic
        ]


toHref : Route -> String
toHref route =
    let
        segments : List String
        segments =
            case route of
                Top ->
                    []
                
                About ->
                    [ "about" ]
                
                Explore ->
                    [ "explore" ]
                
                Help ->
                    [ "help" ]
                
                Login ->
                    [ "login" ]
                
                Logout ->
                    [ "logout" ]
                
                NotFound ->
                    [ "not-found" ]
                
                Notifications ->
                    [ "notifications" ]
                
                PasswordReset ->
                    [ "password-reset" ]
                
                Signup ->
                    [ "signup" ]
                
                Verification ->
                    [ "verification" ]
                
                Dynamic { param1 } ->
                    [ param1 ]
                
                Dynamic_Settings { param1 } ->
                    [ param1, "settings" ]
                
                New_Orga ->
                    [ "new", "orga" ]
                
                M_Dynamic { param1 } ->
                    [ "m", param1 ]
                
                O_Dynamic { param1 } ->
                    [ "o", param1 ]
                
                P_Dynamic { param1 } ->
                    [ "p", param1 ]
                
                S_Dynamic { param1 } ->
                    [ "s", param1 ]
                
                T_Dynamic { param1 } ->
                    [ "t", param1 ]
                
                User_Dynamic { param1 } ->
                    [ "user", param1 ]
                
                User_Dynamic_Settings { param1 } ->
                    [ "user", param1, "settings" ]
                
                M_Dynamic_Dynamic { param1, param2 } ->
                    [ "m", param1, param2 ]
                
                O_Dynamic_Dynamic { param1, param2 } ->
                    [ "o", param1, param2 ]
                
                P_Dynamic_Dynamic { param1, param2 } ->
                    [ "p", param1, param2 ]
                
                S_Dynamic_Dynamic { param1, param2 } ->
                    [ "s", param1, param2 ]
                
                T_Dynamic_Dynamic { param1, param2 } ->
                    [ "t", param1, param2 ]
                
                Tension_Dynamic_Dynamic { param1, param2 } ->
                    [ "tension", param1, param2 ]
                
                Tension_Dynamic_Dynamic_Action { param1, param2 } ->
                    [ "tension", param1, param2, "action" ]
                
                Tension_Dynamic_Dynamic_Contract { param1, param2 } ->
                    [ "tension", param1, param2, "contract" ]
                
                M_Dynamic_Dynamic_Dynamic { param1, param2, param3 } ->
                    [ "m", param1, param2, param3 ]
                
                O_Dynamic_Dynamic_Dynamic { param1, param2, param3 } ->
                    [ "o", param1, param2, param3 ]
                
                P_Dynamic_Dynamic_Dynamic { param1, param2, param3 } ->
                    [ "p", param1, param2, param3 ]
                
                S_Dynamic_Dynamic_Dynamic { param1, param2, param3 } ->
                    [ "s", param1, param2, param3 ]
                
                T_Dynamic_Dynamic_Dynamic { param1, param2, param3 } ->
                    [ "t", param1, param2, param3 ]
                
                Tension_Dynamic_Dynamic_Contract_Dynamic { param1, param2, param3 } ->
                    [ "tension", param1, param2, "contract", param3 ]
    in
    segments
        |> String.join "/"
        |> String.append "/"