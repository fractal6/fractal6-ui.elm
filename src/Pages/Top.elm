module Pages.Top exposing (Flags, Model, Msg, page)

import Global exposing (Msg(..))
import Html exposing (Html, div, text)
import Page exposing (Document, Page)


type alias Flags =
    ()


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
    let
        model =
            {}
    in
    ( model, Cmd.none, Cmd.none )


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
    { title = "Top"
    , body = [ view_ global model ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    --[ div [ class "notification is-info" ] [ div [ class "delete" ] [] , text session.referer.path ] ] ++
    div [] [ text "Top" ]
