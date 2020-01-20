module Pages.SkuStatic exposing (Model, Msg, page)

import Generated.Params as Params
import Html exposing (..)
import Html.Attributes exposing (..)
import Spa.Page
import Utils.Spa exposing (Page)


type alias Model =
    ()


type alias Msg =
    Never


page : Page Params.SkuStatic Model Msg model msg appMsg
page =
    Spa.Page.static
        { title = always "SkuStatic"
        , view = always view
        }



-- VIEW


view : Html Msg
view =
    div
        [ class "columns" ]
        [ div [ class "column" ] [ text "1" ]
        , div [ class "column" ] [ text "2" ]
        , text "SkuStatic"
        ]
