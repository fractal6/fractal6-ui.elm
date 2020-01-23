module Pages.Human.Dynamic.Tensions exposing (Model, Msg, page)

import Spa.Page
import Html exposing (..)
import Generated.Human.Dynamic.Params as Params
import Utils.Spa exposing (Page)


page : Page Params.Tensions Model Msg model msg appMsg
page =
    Spa.Page.element
        { title = always "Human.Dynamic.Tensions"
        , init = always init
        , update = always update
        , subscriptions = always subscriptions
        , view = always view
        }



-- INIT


type alias Model =
    {}


init : Params.Tensions -> ( Model, Cmd Msg )
init _ =
    ( {}
    , Cmd.none
    )



-- UPDATE


type Msg
    = Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model
    , Cmd.none
    )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    text "Human.Dynamic.Tensions"