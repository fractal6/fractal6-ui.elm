module Pages.NotFound exposing (Flags, Model, Msg, page)

import Html exposing (a, div, text)
import Html.Attributes exposing (class, href)
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
    { title = "NotFound"
    , body = [ view_ ]
    }


view_ : Html.Html Msg
view_ =
    div [ class "section" ]
        [ text "Page not found." ]
