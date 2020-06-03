module Components exposing (layout)

import Browser exposing (Document)
import Components.Navbar as Navbar
import Html exposing (Html, a, div, header, hr, i, nav, span, text)
import Html.Attributes as Attr exposing (attribute, class, href, id, style)
import ModelCommon exposing (Session)


layout : { page : Document msg, session : Session } -> Document msg
layout { page, session } =
    { title = page.title
    , body =
        [ div [ id "app" ]
            [ Navbar.viewNavbar session.user -- page.route / current route ?
            , div [ id "body", class "section is-paddingless" ] page.body
            ]
        ]
    }
