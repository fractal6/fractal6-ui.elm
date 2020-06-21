module Pages.Tension.Dynamic.Dynamic exposing (Flags, Model, Msg, page)

import Components.Org.Tension as Tension
import Global
import Html
import Page exposing (Document, Page)


type alias Flags =
    Tension.Flags


type alias Model =
    Tension.Model


type alias Msg =
    Tension.Msg


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = Tension.update
        , subscriptions = Tension.subscriptions
        , view = Tension.view
        }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    Tension.init global flags
