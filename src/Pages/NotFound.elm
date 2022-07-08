module Pages.NotFound exposing (Flags, Model, Msg, page)

import Assets as A
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Page exposing (Document, Page)


type alias Flags =
    ()


type alias Model =
    ()


type alias Msg =
    Never


page : Page Flags Model Msg
page =
    Page.static
        { view = view
        }


view : Document Msg
view =
    { title = "Page not found"
    , body = [ div [ class "section" ] [ A.viewNotFound ] ]
    }
