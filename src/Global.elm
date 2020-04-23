module Global exposing
    ( Flags
    , Model
    , Msg(..)
    , NID
    , User(..)
    , UserFocus
    , init
    , subscriptions
    , update
    )

import Generated.Routes as Routes exposing (Route)
import Ports



-- Model


type alias NID =
    String


type alias UserInfo =
    { username : String
    , display_name : String
    , nid : String
    }


type alias UserState =
    { focus : UserFocus }


type alias UserFocus =
    { circle_focus : NID
    , circle_name : String
    , orga_name : String
    }


type User
    = LoggedOut UserState
    | LoggedIn UserState UserInfo


type alias Model =
    { user : User }


type Msg
    = UpdateFocus String



--


type alias Flags =
    ()


type alias Commands msg =
    { navigate : Route -> Cmd msg
    }



-- Init


init : Commands msg -> Flags -> ( Model, Cmd Msg, Cmd msg )
init _ _ =
    let
        user_state_init =
            { focus =
                { circle_focus = "545687"
                , circle_name = "Sku Branck"
                , orga_name = "<> SkuSku"
                }
            }

        user_info_init =
            { username = "iam Sku"
            , display_name = "haaouaa"
            , nid = "xzdz54"
            }

        init_user =
            { user = LoggedIn user_state_init user_info_init }
    in
    ( init_user
    , Cmd.none
    , Cmd.batch
        [ Ports.log "Hello!"
        , Ports.bulma_driver ""
        , Ports.toggle_theme
        ]
    )


update : Commands msg -> Msg -> Model -> ( Model, Cmd Msg, Cmd msg )
update _ _ model =
    ( model
    , Cmd.none
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
