module Pages.O.Dynamic exposing (Flags, Model, Msg, page)

import Org.Overview as O
import Global
import Html
import Page exposing (Document, Page)


type alias Flags =
    { param1 : String }


type alias Model =
    O.Model


type alias Msg =
    O.Msg


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = O.update
        , subscriptions = O.subscriptions
        , view = O.view
        }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global { param1 } =
    O.init global
        { param1 = param1
        , param2 = Nothing
        , param3 = Nothing
        }
