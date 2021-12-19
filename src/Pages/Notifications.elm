module Pages.Notifications exposing (Flags, Model, Msg, page)

import Global
import Html
import Page exposing (Document, Page)
import User.Notifications as Notifications


type alias Flags =
    Notifications.Flags


type alias Model =
    Notifications.Model


type alias Msg =
    Notifications.Msg


page : Page Flags Model Msg
page =
    Page.component
        { init = Notifications.init
        , update = Notifications.update
        , subscriptions = Notifications.subscriptions
        , view = Notifications.view
        }
