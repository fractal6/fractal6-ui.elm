module Global exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , navigate
    , subscriptions
    , update
    , view
    )

import Browser exposing (Document)
import Browser.Navigation as Nav
import Components
import Generated.Route as Route exposing (Route)
import ModelCommon exposing (..)
import Ports
import Task
import Url exposing (Url)



-- INIT


type alias Flags =
    ()



-- Model


type alias Model =
    { flags : Flags
    , url : Url
    , key : Nav.Key
    , session : Session
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        session =
            { user = LoggedOut
            , node_focus = Nothing
            , orga_data = Nothing
            , circle_tensions = Nothing
            , node_action = Nothing
            }

        --userInfo =
        --    { username = "abcdefghijklmnop"
        --    , display_name = "My name is DorVa"
        --    }
    in
    ( Model flags url key session
    , Cmd.batch
        [ Ports.log "Hello!"
        , Ports.bulma_driver ""
        , Ports.toggle_theme
        ]
    )



-- UPDATE


type Msg
    = Navigate Route
    | UpdateSessionFocus NodeFocusState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate route ->
            ( model
            , Nav.pushUrl model.key (Route.toHref route)
            )

        UpdateSessionFocus focus ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | node_focus = Just focus } }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view :
    { page : Document msg
    , global : Model
    , toMsg : Msg -> msg
    }
    -> Document msg
view { page, global, toMsg } =
    Components.layout
        { page = page
        }



-- COMMANDS


send : msg -> Cmd msg
send =
    Task.succeed >> Task.perform identity


navigate : Route -> Cmd Msg
navigate route =
    send (Navigate route)
