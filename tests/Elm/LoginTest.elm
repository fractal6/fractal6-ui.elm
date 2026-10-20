module Elm.LoginTest exposing (tests)

import Dict
import Expect
import Generated.Route as Route exposing (toHref)
import Html.Attributes as Attr
import Pages.Login as Login exposing (Tab(..))
import RemoteData
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, tag, text)
import Text as T


tests : Test
tests =
    let
        card tab =
            Login.viewCard { form = { post = Dict.empty }, result = RemoteData.NotAsked, tab = tab }
                |> Query.fromHtml
    in
    describe "Sign-in/sign-up card"
        [ test "sign-in tab shows the login form and its shortcuts" <|
            \_ ->
                card SigninTab
                    |> Expect.all
                        [ Query.find [ tag "li", class "is-active" ] >> Query.has [ text T.signin_ ]
                        , Query.hasNot [ attribute (Attr.name "email") ]
                        , Query.has [ tag "a", attribute (Attr.href (toHref Route.PasswordReset)) ]
                        , Query.find [ tag "button" ] >> Query.has [ text T.signin ]
                        ]
        , test "sign-up tab shows the signup form" <|
            \_ ->
                card SignupTab
                    |> Expect.all
                        [ Query.find [ tag "li", class "is-active" ] >> Query.has [ text T.signup_ ]
                        , Query.has [ attribute (Attr.name "email") ]
                        , Query.hasNot [ tag "a", attribute (Attr.href (toHref Route.PasswordReset)) ]
                        , Query.find [ tag "button" ] >> Query.has [ text T.signup ]
                        ]
        ]
