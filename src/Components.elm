module Components exposing (layout)

import Browser exposing (Document)
import Components.Navbar as Navbar
import Footbar
import Html exposing (Html, a, div, header, hr, i, nav, span, text)
import Html.Attributes as Attr exposing (attribute, class, href, id, style)
import Session exposing (Session)


layout : { page : Document msg, session : Session } -> Document msg
layout { page, session } =
    { title = page.title
    , body =
        [ div [ id "app" ]
            [ Navbar.view { user = session.user }
            , div [ id "body" ] <|
                --[ div [ class "notification is-info" ] [ div [ class "delete" ] [] , text session.referer.path ] ] ++
                page.body
            , Footbar.view
            ]
        ]
    }
