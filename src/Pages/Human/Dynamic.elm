module Pages.Human.Dynamic exposing (Model, Msg, page)

import Generated.Human.Params as Params
import Html exposing (..)
import Spa.Page
import Utils.Spa exposing (Page)


page : Page Params.Dynamic Model Msg model msg appMsg
page =
    Spa.Page.element
        { title = \{ model } -> String.join " | " [ model.asked_user ]
        , init = always init
        , update = always update
        , subscriptions = always subscriptions
        , view = always view
        }



-- INIT


type alias Model =
    { asked_user : String }


init : Params.Dynamic -> ( Model, Cmd Msg )
init params =
    ( { asked_user = params.param1 }
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
    p [] [ text "Dashboard for user ", b [] [ text model.asked_user ] ]
