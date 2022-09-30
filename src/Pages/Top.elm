module Pages.Top exposing (Flags, Model, Msg, page)

import Global
import Html
import Page exposing (Document, Page)
import Pages.About as A


type alias Flags =
    ()


type alias Model =
    A.Model


type alias Msg =
    A.Msg


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = A.update
        , subscriptions = A.subscriptions
        , view = A.view
        }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    A.init global flags
