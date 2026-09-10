module Elm.ActionCardTest exposing (tests)

import Components.ActionCard as ActionCard
import Components.AuthModal as AuthModal
import Elm.GraphTest exposing (session)
import Expect
import Fractale.Welcome as Welcome
import Generated.Route as Route exposing (toHref)
import Html.Attributes as Attr
import ModelSchema exposing (initUserctx)
import RemoteData
import Session exposing (Apis, GlobalCmd(..))
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, tag, text)


tests : Test
tests =
    let
        config =
            { icon = "icon-layout", title = "New project", description = "Organize work.", featured = True, action = ActionCard.Click "create" }

        apis =
            Apis "" "" "" "" "" "" "test"

        user =
            { initUserctx | username = "alice" }

        signup =
            AuthModal.init Nothing session
                |> AuthModal.update apis (AuthModal.DoOpenSignupModal "invitation")
                |> Tuple.first
                |> AuthModal.update apis (AuthModal.GotSignin (RemoteData.Success user))
                |> Tuple.first
    in
    describe "Action cards and signup welcome"
        [ test "buttons emit a message without submitting a form" <|
            \_ ->
                ActionCard.view [] config
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ tag "button", attribute (Attr.type_ "button"), class "is-featured" ]
                        , Event.simulate Event.click >> Event.expect "create"
                        ]
        , test "links preserve their URL and caller attributes" <|
            \_ ->
                ActionCard.view [ Attr.target "_blank" ] { config | action = ActionCard.Link "/p/org", featured = False }
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ tag "a", attribute (Attr.href "/p/org"), attribute (Attr.target "_blank") ]
                        , Query.hasNot [ class "is-featured" ]
                        ]
        , test "page and invite modal share the welcome and destinations" <|
            \_ ->
                Expect.all
                    (List.map
                        (\view _ ->
                            Query.fromHtml view
                                |> Expect.all
                                    [ Query.has [ tag "h1", text "@alice" ]
                                    , Query.findAll [ class "action-card" ] >> Query.count (Expect.equal 2)
                                    , Query.has [ tag "a", attribute (Attr.href (toHref Route.New_Orga)) ]
                                    , Query.has [ tag "a", attribute (Attr.href (toHref Route.Explore)) ]
                                    ]
                        )
                        [ Welcome.view { username = user.username, linkAttributes = always [] }
                        , AuthModal.view {} signup
                        ]
                    )
                    ()
        , test "modal cards close before navigating instead of following the anchor" <|
            \_ ->
                AuthModal.view {} signup
                    |> Query.fromHtml
                    |> Query.find [ class "action-card", attribute (Attr.href (toHref Route.New_Orga)) ]
                    |> Event.simulate Event.click
                    |> Expect.all
                        [ Event.expect (AuthModal.DoCloseAuthModal (toHref Route.New_Orga))
                        , Event.expectPreventDefault
                        , Event.expectStopPropagation
                        ]
        , test "navigation skips the old-page refresh while Got it retains it" <|
            \_ ->
                let
                    ( closed, navigating ) =
                        AuthModal.update apis (AuthModal.DoCloseAuthModal "/new/orga") signup

                    ( _, staying ) =
                        AuthModal.update apis (AuthModal.DoCloseAuthModal "") signup
                in
                Expect.all
                    [ \_ -> Expect.equal [ DoNavigate "/new/orga" ] navigating.gcmds
                    , \_ -> Expect.equal (Just ( False, user )) navigating.result
                    , \_ -> Expect.equal (Just ( True, user )) staying.result
                    , \_ -> Expect.equal [] staying.gcmds
                    , \_ -> AuthModal.view {} closed |> Query.fromHtml |> Query.hasNot [ class "is-active" ]
                    ]
                    ()
        ]
