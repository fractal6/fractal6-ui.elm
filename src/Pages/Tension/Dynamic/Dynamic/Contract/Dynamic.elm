module Pages.Tension.Dynamic.Dynamic.Contract.Dynamic exposing (Flags, Model, Msg, page)

import Global
import Html
import Org.Tension as Tension exposing (TensionTab(..))
import Page exposing (Document, Page)


type alias Flags =
    { param1 : String
    , param2 : String
    , param3 : String
    }


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
    Tension.init global { param1 = flags.param1, param2 = flags.param2, param3 = Contracts, param4 = Just flags.param3 }
