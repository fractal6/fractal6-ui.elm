module Pages.User.Dynamic.Tensions exposing (Model, Msg, page)

import Generated.User.Dynamic.Params as Params
import Html exposing (..)
import Spa.Page
import Utils.Spa exposing (Page)


page : Page Params.Tensions Model Msg model msg appMsg
page =
    Spa.Page.element
        { title = always "User.Dynamic.Tensions"
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
    text "User.Dynamic.Tensions"
