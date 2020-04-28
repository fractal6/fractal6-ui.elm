module Global exposing
    ( Flags
    , Model
    , Msg
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
import Ports
import Task
import Url exposing (Url)



-- INIT


type alias Flags =
    ()



-- Model


type alias UserInfo =
    { username : String
    , display_name : String
    }


type alias UserSession =
    { node_focus : String }


type User
    = LoggedOut UserSession
    | LoggedIn UserSession UserInfo



--type alias Model =
--    { user : User }


type alias Model =
    { flags : Flags
    , url : Url
    , key : Nav.Key
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        session =
            { node_focus = "aahahah"
            }

        userInfo =
            { username = "abcdefghijklmnop"
            , display_name = "My name is DorVa"
            }

        init_user =
            { user = LoggedIn session userInfo }
    in
    ( Model flags url key
    , Cmd.batch
        [ Ports.log "Hello!"
        , Ports.bulma_driver ""
        , Ports.toggle_theme
        ]
    )



-- UPDATE


type Msg
    = Navigate Route


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate route ->
            ( model
            , Nav.pushUrl model.key (Route.toHref route)
            )



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
