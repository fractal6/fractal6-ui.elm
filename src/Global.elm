port module Global exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , navigate
    , send
    , sendNow
    , sendSleep
    , subscriptions
    , update
    , view
    )

import Browser exposing (Document)
import Browser.Navigation as Nav
import Codecs exposing (WindowPos)
import Components
import Components.Loading as Loading exposing (WebData, expectJson, toErrorData)
import Dict
import Generated.Route as Route exposing (Route)
import Http
import Json.Decode as JD
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (NodeFocus)
import ModelCommon.Requests exposing (tokenack)
import ModelSchema exposing (..)
import Ports
import Process
import RemoteData exposing (RemoteData)
import Task
import Time
import Url exposing (Url)



-- INIT


type alias Flags =
    SessionFlags



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
        ( session, cmds ) =
            fromLocalSession flags
    in
    ( Model flags url key session
    , Cmd.batch
        ([ Ports.log "Hello!"
         , Ports.toggle_theme
         , Ports.bulma_driver ""
         ]
            ++ cmds
        )
    )



-- UPDATE


type Msg
    = Navigate Route
    | UpdateReferer Url
    | UpdateUserSession UserCtx -- user is logged In !
    | UpdateUserTokenAck (WebData UserCtx)
    | UpdateUserToken
    | LoggedOutUser
    | LoggedOutUserOk
    | RedirectOnLoggedIn -- user is logged In !
    | UpdateSessionFocus (Maybe NodeFocus)
    | UpdateSessionPath (Maybe LocalGraph)
    | UpdateSessionOrga (Maybe NodesData)
    | UpdateSessionData (Maybe NodeData)
    | UpdateSessionTensions (Maybe TensionsData)
    | UpdateSessionTensionHead (Maybe TensionHead)
    | UpdateSessionAdmin (Maybe Bool)
    | UpdateSessionWindow (Maybe WindowPos)
    | UpdateSessionScreen Screen


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        apis =
            model.session.apis
    in
    case msg of
        Navigate route ->
            ( model, Nav.pushUrl model.key (Route.toHref route) )

        UpdateReferer url ->
            let
                session =
                    model.session

                referer =
                    case url.path of
                        "/logout" ->
                            session.referer

                        _ ->
                            Just url
            in
            ( { model | session = { session | referer = referer } }, Cmd.none )

        UpdateUserSession uctx ->
            let
                maybeNewMemberid =
                    case model.session.user of
                        LoggedIn uctxOld ->
                            if List.length uctxOld.roles == 1 && List.length uctx.roles > 1 then
                                List.head uctxOld.roles |> Maybe.map (\r -> r.nameid)

                            else
                                Nothing

                        LoggedOut ->
                            Nothing

                cmds =
                    case model.session.orga_data of
                        Just ndata ->
                            case maybeNewMemberid of
                                Just nid ->
                                    [ Ports.saveUserCtx uctx
                                    , Ports.removeRedrawGraphPack ndata nid
                                    ]

                                Nothing ->
                                    [ Ports.saveUserCtx uctx
                                    , Ports.redrawGraphPack ndata
                                    ]

                        Nothing ->
                            [ Ports.saveUserCtx uctx ]

                session =
                    model.session
            in
            ( { model | session = { session | user = LoggedIn uctx } }
            , Cmd.batch cmds
            )

        UpdateUserToken ->
            let
                session =
                    model.session
            in
            ( model
            , tokenack apis.auth UpdateUserTokenAck
            )

        UpdateUserTokenAck result ->
            let
                session =
                    model.session

                newModel =
                    { model | session = { session | token_data = result } }
            in
            case result of
                RemoteData.Success uctx ->
                    ( newModel
                    , send <| UpdateUserSession uctx
                    )

                default ->
                    ( newModel, Cmd.none )

        RedirectOnLoggedIn ->
            let
                cmd =
                    case model.session.user of
                        LoggedIn uctx ->
                            let
                                home =
                                    Route.User_Dynamic { param1 = uctx.username }
                            in
                            case model.session.referer of
                                Just referer ->
                                    referer
                                        |> Route.fromUrl
                                        |> Maybe.withDefault home
                                        |> navigate

                                Nothing ->
                                    navigate home

                        LoggedOut ->
                            sendSleep RedirectOnLoggedIn 300
            in
            ( model, cmd )

        LoggedOutUser ->
            case model.session.user of
                LoggedIn uctx ->
                    ( { model | session = resetSession model.flags }, Ports.removeUserCtx uctx )

                LoggedOut ->
                    ( model, Cmd.none )

        LoggedOutUserOk ->
            ( model, navigate Route.Top )

        --
        -- Update Session Data
        --
        UpdateSessionFocus data ->
            -- Reset Tension Head @here, to avois glitch or bad UX when navigating tensions.
            let
                session =
                    model.session
            in
            ( { model | session = { session | node_focus = data, tension_head = Nothing } }, Cmd.none )

        UpdateSessionPath data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | path_data = data } }, Cmd.none )

        UpdateSessionOrga data ->
            let
                session =
                    model.session

                -- Make a dict of user per circle
                users =
                    Maybe.map (\d -> orgaToUsersData d) data
            in
            ( { model | session = { session | orga_data = data, users_data = users } }, Cmd.none )

        UpdateSessionData data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | node_data = data } }, Cmd.none )

        UpdateSessionTensionHead data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | tension_head = data } }, Cmd.none )

        UpdateSessionTensions data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | tensions_data = data } }, Cmd.none )

        UpdateSessionAdmin data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | isAdmin = data } }, Cmd.none )

        UpdateSessionWindow data ->
            let
                session =
                    model.session

                -- @debug: Save window_pos in localStorage ?
            in
            ( { model | session = { session | window_pos = data } }, Cmd.none )

        UpdateSessionScreen data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | screen = data } }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ loggedOutOkFromJs (always LoggedOutUserOk)
        ]


port loggedOutOkFromJs : (() -> msg) -> Sub msg



-- VIEW


view : { page : Document msg, global : Model, toMsg : Msg -> msg } -> Document msg
view { page, global, toMsg } =
    Components.layout
        { page = page
        , session = global.session
        }



-- COMMANDS


send : msg -> Cmd msg
send =
    Task.succeed >> Task.perform identity


sendNow : (Time.Posix -> msg) -> Cmd msg
sendNow m =
    Task.perform m Time.now


sendSleep : msg -> Float -> Cmd msg
sendSleep msg time =
    Task.perform (\_ -> msg) (Process.sleep time)


navigate : Route -> Cmd Msg
navigate route =
    send (Navigate route)
