module Pages.Top exposing (Model, Msg, page)

import Generated.Params as Params
import Html exposing (..)
import Html.Attributes exposing (..)
import Spa.Page
import Utils.Spa exposing (Page)


type alias Model =
    ()


type alias Msg =
    Never


page : Page Params.Top Model Msg model msg appMsg
page =
    Spa.Page.static
        { title = always "homepage"
        , view = always view
        }



-- VIEW


view : Html Msg
view =
    div []
        [ h1 [] [ text "Welcome" ]
        ]
