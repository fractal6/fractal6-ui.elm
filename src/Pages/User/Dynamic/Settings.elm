module Pages.User.Dynamic.Settings exposing (Flags, Model, Msg, page)

import Global
import Html
import Page exposing (Document, Page)
import User.Settings as Settings


type alias Flags =
    { param1 : String
    }


type alias Model =
    Settings.Model


type alias Msg =
    Settings.Msg


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = Settings.update
        , subscriptions = Settings.subscriptions
        , view = Settings.view
        }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    Settings.init global { param1 = flags.param1 }
