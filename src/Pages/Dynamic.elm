module Pages.Dynamic exposing (Flags, Model, Msg, page)

import User.Profile as Profile
import Global
import Html
import Page exposing (Document, Page)


type alias Flags =
    Profile.Flags


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
