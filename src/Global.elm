port module Global exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , navigate
    , send
    , subscriptions
    , update
    , view
    )

import Browser exposing (Document)
import Browser.Navigation as Nav
import Components
import Generated.Route as Route exposing (Route)
import ModelCommon exposing (..)
import ModelOrg exposing (..)
import Ports
import Process
import Task
import Url exposing (Url)



-- INIT


type alias Flags =
    Maybe UserCtx



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
        userState =
            case flags of
                Just userCtx ->
                    LoggedIn userCtx

                Nothing ->
                    LoggedOut

        session =
            { user = userState
            , node_focus = Nothing
            , node_path = Nothing
            , orga_data = Nothing
            , circle_tensions = Nothing
            , node_action = Nothing
            }
    in
    ( Model flags url key session
    , Cmd.batch
        [ Ports.log "Hello!"
        , Ports.toggle_theme
        , Ports.bulma_driver ""
        ]
    )



-- UPDATE


type Msg
    = Navigate Route
    | UpdateSessionFocus NodeFocus
    | UpdateSessionOrga NodesData
    | UpdateUserSession UserCtx -- user is logged In !
    | LoggedOutUser
    | LoggedOutUserOk
    | RedirectOnLoggedIn -- user is logged In !


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate route ->
            ( model
            , Nav.pushUrl model.key (Route.toHref route)
            )

        RedirectOnLoggedIn ->
            let
                cmd =
                    case model.session.user of
                        LoggedIn uctx ->
                            navigate <| Route.User_Dynamic { param1 = uctx.username }

                        --Nav.replaceUrl global.key (String.join "/" [ "/user", uctx.username ])
                        LoggedOut ->
                            Task.perform (\_ -> RedirectOnLoggedIn) (Process.sleep 300)
            in
            ( model, cmd )

        LoggedOutUser ->
            case model.session.user of
                LoggedIn uctx ->
                    let
                        session =
                            model.session
                    in
                    ( { model | session = { session | user = LoggedOut } }, Ports.removeUserCtx uctx )

                LoggedOut ->
                    ( model, Cmd.none )

        LoggedOutUserOk ->
            ( model, navigate Route.Top )

        UpdateSessionFocus data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | node_focus = Just data } }
            , Cmd.none
            )

        UpdateSessionOrga data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | orga_data = Just data } }
            , Cmd.none
            )

        UpdateUserSession userCtx ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | user = LoggedIn userCtx } }
            , Ports.saveUserCtx userCtx
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ loggedOutOkFromJs (always LoggedOutUserOk)
        ]


port loggedOutOkFromJs : (() -> msg) -> Sub msg



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
        , session = global.session
        }



-- COMMANDS


send : msg -> Cmd msg
send =
    Task.succeed >> Task.perform identity


navigate : Route -> Cmd Msg
navigate route =
    send (Navigate route)
