module Global exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , navigate
    , now
    , send
    , sendNow
    , sendSleep
    , subscriptions
    , update
    , view
    )

import Auth exposing (parseErr2)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Codecs exposing (WindowPos)
import Components.Navbar as Navbar
import Dict
import Footbar
import Fractal.Enum.Lang as Lang
import Generated.Route as Route exposing (Route)
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (attribute, class, href, id, style)
import Html.Lazy as Lazy
import Http
import Json.Decode as JD
import Loading exposing (GqlData, RequestResult(..), WebData)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), NodeFocus, toString, uriFromNameid, urlToFractalRoute)
import Requests exposing (tokenack)
import ModelSchema exposing (..)
import Ports
import Process
import Query.QueryNotifications exposing (queryNotifCount)
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
         , Ports.bulma_driver ""
         , now
         , sendSleep RefreshNotifCount 1000
         ]
            ++ cmds
        )
    )



-- UPDATE


type Msg
    = Navigate Route
    | NavigateRaw String
    | ReplaceUrl String
    | SetTime Time.Posix
    | UpdateReferer Url
    | UpdateCanReferer (Maybe Url)
    | NavigateNode String
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
    | UpdateSessionTree (Maybe NodesDict)
    | UpdateSessionData (Maybe NodeData)
    | UpdateSessionOrgs (Maybe (List OrgaNode))
    | UpdateSessionTensions (Maybe (List Tension))
    | UpdateSessionTensionsInt (Maybe (List Tension))
    | UpdateSessionTensionsExt (Maybe (List Tension))
    | UpdateSessionTensionsAll (Maybe TensionsDict)
    | UpdateSessionTensionsCount (Maybe TensionsCount)
    | UpdateSessionTensionHead (Maybe TensionHead)
    | UpdateSessionAdmin (Maybe Bool)
    | UpdateSessionWindow (Maybe WindowPos)
    | UpdateSessionMenuOrga (Maybe Bool)
    | UpdateSessionMenuTree (Maybe Bool)
    | UpdateSessionScreen Screen
    | UpdateSessionLang String
    | UpdateSessionNotif NotifCount
    | RefreshNotifCount
    | AckNotifCount (GqlData NotifCount)
    | UpdateSessionAuthorsPanel (Maybe UserSearchPanelModel)
    | UpdateSessionLabelsPanel (Maybe LabelSearchPanelModel)
    | UpdateSessionNewOrgaData (Maybe OrgaForm)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        apis =
            model.session.apis
    in
    case msg of
        Navigate route ->
            ( model, Nav.pushUrl model.key (Route.toHref route) )

        NavigateRaw route ->
            ( model, Nav.pushUrl model.key route )

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

        UpdateCanReferer referer ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | can_referer = referer } }, Cmd.none )

        NavigateNode nameid ->
            case urlToFractalRoute model.url of
                Just OverviewBaseUri ->
                    ( model, Nav.replaceUrl model.key (uriFromNameid OverviewBaseUri nameid []) )

                _ ->
                    ( model, Cmd.none )

        UpdateUserSession uctx ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | user = LoggedIn uctx } }
              -- Update Components when Uctx change !
            , [ Ports.saveUserCtx uctx, sendSleep RefreshNotifCount 1000 ]
                ++ (case model.session.tree_data of
                        Just ndata ->
                            [ Ports.redrawGraphPack ndata ]

                        Nothing ->
                            []
                   )
                |> Cmd.batch
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
                    , sendSleep (UpdateUserSession uctx) 300
                    )

                _ ->
                    if parseErr2 result 1 == Auth.Authenticate then
                        case model.session.user of
                            LoggedIn uctx ->
                                ( newModel, Ports.raiseAuthModal uctx )

                            LoggedOut ->
                                ( newModel, Cmd.none )

                    else
                        ( newModel, Cmd.none )

        RedirectOnLoggedIn ->
            let
                cmd =
                    case model.session.user of
                        LoggedIn uctx ->
                            let
                                home =
                                    toString UsersBaseUri uctx.username []
                            in
                            case model.session.referer of
                                Just referer ->
                                    referer
                                        |> Url.toString
                                        |> NavigateRaw
                                        |> send

                                Nothing ->
                                    send (NavigateRaw home)

                        LoggedOut ->
                            sendSleep RedirectOnLoggedIn 333
            in
            ( model, cmd )

        LoggedOutUser ->
            case model.session.user of
                LoggedIn uctx ->
                    ( { model | session = resetSession model.session model.flags }, Ports.removeSession uctx )

                LoggedOut ->
                    ( model, Cmd.none )

        LoggedOutUserOk ->
            ( model, navigate Route.Top )

        --
        -- Update Session Data
        --
        UpdateSessionFocus data ->
            -- @here @warning: this Msg has side effect. It reset some session data, in order
            -- to avoid glitch or bad UX (seeing incoherent tensions data) when navigating app.
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
            -- Update also children. Children are used to manage tensions depth search option.
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

        UpdateSessionTree data ->
            let
                session =
                    model.session

                -- Eventually update path_data
                pdata =
                    Maybe.map
                        (\path_data ->
                            -- @optimize: only of node_focus.nameid == path_data.focus.nameid
                            case getNode path_data.focus.nameid (Maybe.map (\d -> Success d) data |> Maybe.withDefault NotAsked) of
                                Just n ->
                                    let
                                        f focus =
                                            { focus | visibility = n.visibility, mode = n.mode, name = n.name }
                                    in
                                    { path_data | focus = f path_data.focus }

                                Nothing ->
                                    path_data
                        )
                        model.session.path_data
            in
            ( { model | session = { session | tree_data = data, path_data = pdata } }, Cmd.none )

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

        UpdateSessionOrgs data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | orgs_data = data } }, Cmd.none )

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

        UpdateSessionWindow data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | window_pos = data } }, Cmd.none )

        UpdateSessionMenuOrga data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | orga_menu = data } }, Cmd.none )

        UpdateSessionMenuTree data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | tree_menu = data } }, Cmd.none )

        UpdateSessionScreen data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | screen = data } }, Cmd.none )

        UpdateSessionLang data ->
            let
                session =
                    model.session
            in
            case Lang.fromString data of
                Just lang ->
                    case session.user of
                        LoggedIn user ->
                            ( { model | session = { session | lang = lang, user = LoggedIn { user | lang = lang } } }, Cmd.none )

                        LoggedOut ->
                            ( { model | session = { session | lang = lang } }, Cmd.none )

                Nothing ->
                    ( model, Ports.logErr ("Error: Bad lang format: " ++ data) )

        UpdateSessionNotif data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | notif = data } }, Cmd.none )

        RefreshNotifCount ->
            case model.session.user of
                LoggedIn uctx ->
                    ( model, queryNotifCount apis { uctx = uctx } AckNotifCount )

                LoggedOut ->
                    ( model, Cmd.none )

        AckNotifCount result ->
            case result of
                Success data ->
                    ( model, send (UpdateSessionNotif data) )

                _ ->
                    ( model, Cmd.none )

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

        UpdateSessionNewOrgaData data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | newOrgaData = data } }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.loggedOutOkFromJs (always LoggedOutUserOk)
        , Ports.updateMenuOrgaFromJs UpdateSessionMenuOrga
        , Ports.updateMenuTreeFromJs UpdateSessionMenuTree
        , Ports.updateLangFromJs UpdateSessionLang
        , Ports.updateNotifFromJs (always RefreshNotifCount)
        ]



-- VIEW
--


view : { page : Document msg, global : Model, url : Url, msg1 : String -> msg } -> Document msg
view { page, global, url, msg1 } =
    layout
        { page = page
        , url = url
        , session = global.session
        , msg1 = msg1
        }


layout : { page : Document msg, url : Url, session : Session, msg1 : String -> msg } -> Document msg
layout { page, url, session, msg1 } =
    { title = page.title
    , body =
        [ div [ id "app" ]
            [ Lazy.lazy4 Navbar.view session.user session.notif url msg1
            , div [ id "body" ] page.body
            , Footbar.view
            ]
        ]
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
