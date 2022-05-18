module Pages.Logout exposing (Flags, Model, Msg, page)

import Generated.Route as Route exposing (Route)
import Global exposing (Msg(..), send, sendSleep)
import Html
import Http
import ModelCommon exposing (UserState(..))
import ModelCommon.Requests exposing (logout)
import Page exposing (Document, Page)


type alias Flags =
    ()


type alias Model =
    {}


type Msg
    = NoOp


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        gcmd =
            case global.session.user of
                LoggedOut ->
                    Global.navigate Route.Top

                LoggedIn _ ->
                    send LoggedOutUser
    in
    ( {}, logout global.session.apis NoOp, gcmd )


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, Cmd.none )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.none


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Logout"
    , body = [ Html.text "" ]
    }
