port module Global exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , navigate
    , send
    , sendSleep
    , subscriptions
    , update
    , view
    )

import Browser exposing (Document)
import Browser.Navigation as Nav
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
        ( userState, cmd ) =
            case flags.uctx of
                Just userCtxRaw ->
                    case JD.decodeValue userCtxDecoder userCtxRaw of
                        Ok uctx ->
                            ( LoggedIn uctx, Cmd.none )

                        Err err ->
                            ( LoggedOut, Ports.logErr (JD.errorToString err) )

                Nothing ->
                    ( LoggedOut, Cmd.none )

        session =
            initSession flags
    in
    ( Model flags url key { session | user = userState }
    , Cmd.batch
        [ Ports.log "Hello!"
        , Ports.toggle_theme
        , Ports.bulma_driver ""
        , cmd
        ]
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
    | UpdateSessionFocus2 (Maybe FocusNode)
    | UpdateSessionPath (Maybe LocalGraph)
    | UpdateSessionOrga (Maybe NodesData)
    | UpdateSessionData (Maybe NodeData)
    | UpdateSessionTensions (Maybe TensionsData)
    | UpdateSessionTensionHead (Maybe TensionHead)


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
                    let
                        session =
                            initSession model.flags
                    in
                    ( { model | session = { session | user = LoggedOut } }, Ports.removeUserCtx uctx )

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

        UpdateSessionFocus2 data ->
            -- Reset Tension Head @here, to avois glitch or bad UX when navigating tensions.
            let
                session =
                    model.session
            in
            ( { model | session = { session | focus = data } }, Cmd.none )

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


sendSleep : msg -> Float -> Cmd msg
sendSleep msg time =
    Task.perform (\_ -> msg) (Process.sleep time)


navigate : Route -> Cmd Msg
navigate route =
    send (Navigate route)
