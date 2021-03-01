module Pages.S.Dynamic.Dynamic.Dynamic exposing (Flags, Model, Msg, page)

import Org.Settings as S
import Global
import Html
import Page exposing (Document, Page)


type alias Flags =
    { param1 : String, param2 : String, param3 : String }


type alias Model =
    S.Model


type alias Msg =
    S.Msg


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = S.update
        , subscriptions = S.subscriptions
        , view = S.view
        }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global { param1, param2, param3 } =
    S.init global
        { param1 = param1
        , param2 = Just param2
        , param3 = Just param3
        }
