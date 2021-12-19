module Pages.User.Dynamic exposing (Flags, Model, Msg, page)

import Global
import Html
import Page exposing (Document, Page)
import User.Profile as Profile


type alias Flags =
    Profile.Flags


type alias Model =
    Profile.Model


type alias Msg =
    Profile.Msg


page : Page Flags Model Msg
page =
    Page.component
        { init = Profile.init
        , update = Profile.update
        , subscriptions = Profile.subscriptions
        , view = Profile.view
        }
