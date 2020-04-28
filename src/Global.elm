module Global exposing
    ( Flags
    , Model
    , Msg(..)
    , User(..)
    , init
    , subscriptions
    , update
    )

import Generated.Routes as Routes exposing (Route)
import Ports



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


type alias Model =
    { user : User }



-- Init


init : Commands msg -> Flags -> ( Model, Cmd Msg, Cmd msg )
init _ _ =
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
    ( init_user
    , Cmd.none
    , Cmd.batch
        [ Ports.log "Hello!"
        , Ports.bulma_driver ""
        , Ports.toggle_theme
        ]
    )


type Msg
    = Never


type alias Flags =
    ()


type alias Commands msg =
    { navigate : Route -> Cmd msg
    }


update : Commands msg -> Msg -> Model -> ( Model, Cmd Msg, Cmd msg )
update _ _ model =
    ( model
    , Cmd.none
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
