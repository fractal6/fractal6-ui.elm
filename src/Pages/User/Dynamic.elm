module Pages.User.Dynamic exposing (Flags, Model, Msg, page)

import Global
import Html
import Page exposing (Document, Page)


type alias Flags =
    { param1 : String }


type alias Model =
    {}


type Msg
    = NoOp


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    ( {}, Cmd.none, Cmd.none )


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, Cmd.none )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.none


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "User.Dynamic"
    , body = [ Html.text "User.Dynamic" ]
    }