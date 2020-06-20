module Pages.Dynamic exposing (Flags, Model, Msg, page)

import Components.User.Profile as Profile exposing (Model, Msg, init, subscriptions, update, view)
import Global
import Html
import Page exposing (Document, Page)


type alias Flags =
    { param1 : String }


type alias Model =
    Profile.Model


type alias Msg =
    Profile.Msg


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = Profile.update
        , subscriptions = Profile.subscriptions
        , view = Profile.view
        }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    Profile.init global flags
