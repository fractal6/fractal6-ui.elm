module Pages.O.Dynamic exposing (Flags, Model, Msg, page)

import Components.Org.Overview as Org exposing (Model, Msg, init, subscriptions, update, view)
import Global
import Html
import Page exposing (Document, Page)


type alias Flags =
    { param1 : String }


type alias Model =
    Org.Model


type alias Msg =
    Org.Msg


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = Org.update
        , subscriptions = Org.subscriptions
        , view = Org.view
        }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global { param1 } =
    Org.init global
        { param1 = param1
        , param2 = Nothing
        , param3 = Nothing
        }
