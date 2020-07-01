module Pages.NotFound exposing (Flags, Model, Msg, page)

import Components.NotFound exposing (viewNotFound)
import Html exposing (Html)
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
    , body = [ view_ ]
    }


view_ : Html Msg
view_ =
    viewNotFound
