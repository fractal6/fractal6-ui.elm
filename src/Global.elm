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
import Session
    exposing
        ( LabelSearchPanelModel
        , Screen
        , Session
        , SessionFlags
        , UserSearchPanelModel
        , fromLocalSession
        , resetSession
        )
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
    , now : Time.Posix
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( session, cmds ) =
            fromLocalSession flags
    in
    ( Model flags url key session (Time.millisToPosix 0)
    , Cmd.batch
        ([ Ports.log "Hello!"
         , Ports.toggle_theme
         , Ports.bulma_driver ""
         , now
         ]
            ++ cmds
        )
    )



-- UPDATE


type Msg
    = Navigate Route
    | ReplaceUrl String
    | SetTime Time.Posix
    | UpdateReferer Url
    | UpdateUserSession UserCtx -- user is logged In !
    | UpdateUserTokenAck (WebData UserCtx)
    | UpdateUserToken
    | LoggedOutUser
    | LoggedOutUserOk
    | RedirectOnLoggedIn -- user is logged In !
    | UpdateSessionFocus (Maybe NodeFocus)
    | UpdateSessionFocusOnly (Maybe NodeFocus)
    | UpdateSessionPath (Maybe LocalGraph)
    | UpdateSessionChildren (Maybe (List NodeId))
    | UpdateSessionOrga (Maybe NodesDict)
    | UpdateSessionData (Maybe NodeData)
    | UpdateSessionTensions (Maybe TensionsList)
    | UpdateSessionTensionsInt (Maybe TensionsList)
    | UpdateSessionTensionsExt (Maybe TensionsList)
    | UpdateSessionTensionsAll (Maybe TensionsList)
    | UpdateSessionTensionsCount (Maybe TensionsCount)
    | UpdateSessionTensionHead (Maybe TensionHead)
    | UpdateSessionAdmin (Maybe Bool)
    | UpdateSessionSubscribe (Maybe Bool)
    | UpdateSessionWindow (Maybe WindowPos)
    | UpdateSessionScreen Screen
    | UpdateSessionAuthorsPanel (Maybe UserSearchPanelModel)
    | UpdateSessionLabelsPanel (Maybe LabelSearchPanelModel)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        apis =
            model.session.apis
    in
    case msg of
        Navigate route ->
            ( model, Nav.pushUrl model.key (Route.toHref route) )

        ReplaceUrl url ->
            ( model, Nav.replaceUrl model.key url )

        SetTime time ->
            ( { model | now = time }, Cmd.none )

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
            ( model
            , tokenack apis UpdateUserTokenAck
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

                _ ->
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
                    ( { model | session = resetSession model.flags }, Ports.removeSession uctx )

                LoggedOut ->
                    ( model, Cmd.none )

        LoggedOutUserOk ->
            ( model, navigate Route.Top )

        --
        -- Update Session Data
        --
        UpdateSessionFocus data ->
            -- @here @warning: this Msg has side effect. It reset some session data, in order
            -- to avoid glitch or bad UX (seeing uncoherent tensions data) when navigating app.
            -- * reset Tensions and Tension page data.
            -- * reset Panel data.
            let
                session =
                    model.session
            in
            ( { model
                | session =
                    { session
                        | node_focus = data
                        , tension_head = Nothing
                        , tensions_data = Nothing
                        , tensions_int = Nothing
                        , tensions_ext = Nothing
                        , tensions_all = Nothing
                        , authorsPanel = Nothing
                        , labelsPanel = Nothing
                    }
              }
            , Cmd.none
            )

        UpdateSessionFocusOnly data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | node_focus = data } }, Cmd.none )

        UpdateSessionPath data ->
            -- Update also children. Children are used to account for the depth of tensions search.
            let
                session =
                    model.session
            in
            ( { model | session = { session | path_data = data, children = Nothing } }, Cmd.none )

        UpdateSessionChildren data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | children = data } }, Cmd.none )

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

        UpdateSessionTensionsInt data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | tensions_int = data } }, Cmd.none )

        UpdateSessionTensionsExt data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | tensions_ext = data } }, Cmd.none )

        UpdateSessionTensionsAll data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | tensions_all = data } }, Cmd.none )

        UpdateSessionTensionsCount data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | tensions_count = data } }, Cmd.none )

        UpdateSessionAdmin data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | isAdmin = data } }, Cmd.none )

        UpdateSessionSubscribe data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | isSubscribed = data } }, Cmd.none )

        UpdateSessionWindow data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | window_pos = data } }, Cmd.none )

        UpdateSessionScreen data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | screen = data } }, Cmd.none )

        UpdateSessionAuthorsPanel data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | authorsPanel = data } }, Cmd.none )

        UpdateSessionLabelsPanel data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | labelsPanel = data } }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
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


now : Cmd Msg
now =
    Task.perform SetTime Time.now


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
