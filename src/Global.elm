{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2024 Fractale Co

   This file is part of Fractale.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
-}


module Global exposing
    ( Flags
    , Model
    , Msg(..)
    , getConf
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

import Auth exposing (ErrState(..), parseErr, parseErr2)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Bulk exposing (OrgaForm, UserState(..), getNode, uctxFromUser)
import Bulk.Codecs exposing (FractalBaseRoute(..), NodeFocus, toLink, urlToFractalRoute)
import Bulk.Error exposing (viewGqlErrorsLight)
import Codecs exposing (RecentActivityTab, WindowPos)
import Components.Navbar as Navbar
import Dict
import Extra exposing (showIf, ternary, unwrap2)
import Extra.Url exposing (queryParser)
import Footbar
import Fractal.Enum.Lang as Lang
import Generated.Route as Route exposing (Route)
import Html exposing (Html, a, button, div, p, text)
import Html.Attributes exposing (class, classList, id)
import Html.Events exposing (onClick)
import Html.Lazy as Lazy
import List.Extra as LE
import Loading exposing (GqlData, RequestResult(..), RestData, errorHttpToString, isFailure, withMapData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Ports
import Process
import Query.PatchUser exposing (toggleOrgaWatch)
import Query.QueryNode exposing (getOrgaInfo)
import Query.QueryNotifications exposing (queryNotifCount)
import Query.QueryTension exposing (queryPinnedTensions)
import RemoteData
import Requests exposing (tokenack)
import Schemas.TreeMenu as TreeMenuSchema
import Session exposing (Conf, LabelSearchPanelModel, Screen, Session, SessionFlags, UserSearchPanelModel, ViewMode(..), fromLocalSession, resetSession)
import Task
import Time
import Url exposing (Url)



-- Model


type alias Flags =
    SessionFlags


type alias Model =
    { flags : Flags
    , url : Url
    , key : Nav.Key
    , session : Session
    , now : Time.Posix
    }


getConf : Model -> Conf
getConf global =
    let
        query =
            queryParser global.url

        viewMode =
            Dict.get "view" query
                |> withDefault []
                |> (\x ->
                        if List.any (\y -> List.member y x) [ "shared", "iframe" ] then
                            IFrameView

                        else
                            DesktopView
                   )
    in
    { screen = global.session.screen
    , now = global.now
    , lang = global.session.lang
    , theme = global.session.theme
    , url = global.url
    , user = global.session.user
    , viewMode = viewMode
    }



-- INIT


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( session, cmds ) =
            fromLocalSession flags
    in
    ( Model flags url key { session | system_notification = RemoteData.NotAsked } (Time.millisToPosix 0)
    , Cmd.batch
        ([ Ports.log "Hello!"
         , Ports.bulma_driver ""
         , now
         , send UpdateUserToken
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
      -- Update Session Data
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
    | UpdateSessionProject (Maybe ProjectData)
    | UpdateSessionAdmin (Maybe Bool)
    | UpdateSessionWindow (Maybe WindowPos)
    | UpdateSessionRecentActivityTab (Maybe RecentActivityTab)
    | UpdateSessionMenuOrga (Maybe Bool)
    | UpdateSessionMenuTree (Maybe TreeMenuSchema.PersistentModel)
    | UpdateSessionScreen Screen
    | UpdateSessionLang String
    | UpdateSessionNotif NotifCount
    | GotOrgaInfo (GqlData OrgaInfo)
    | RefreshNotifCount
    | AckNotifCount (GqlData NotifCount)
    | ToggleWatchOrga String
    | GotIsWatching (GqlData Bool)
    | RefreshPinTension String
    | AckPinTension (GqlData (Maybe (List PinTension)))
      -- Global utils msg
    | NavigateNode String
    | UpdateUserSession UserCtx -- user is logged In !
    | UpdateUserTokenAck (RestData UserCtx)
    | UpdateUserToken
    | LoggedOutUser
    | LoggedOutUserOk
    | RedirectOnLoggedIn -- user is logged In !
    | OnCloseOutdatedVersion
    | OnPushSystemNotif (RestData String)
    | OnClearSystemNotif
      -- utils
    | VOID
    | LogErr String
      -- Components data update
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

        --
        -- Update Session Data
        --
        UpdateSessionFocus data ->
            -- @warning: this Msg has side effect. It reset some session data, in order
            -- to avoid glitch or bad UX (seeing incoherent tensions data) when navigating app.
            -- * reset Tensions and Tension page data.
            -- * reset Panel data.
            let
                session =
                    model.session

                cmd =
                    case data of
                        Just n ->
                            -- If new orga context
                            if session.orgaInfo == Nothing || Just n.rootnameid /= Maybe.map .rootnameid model.session.node_focus then
                                getOrgaInfo apis (uctxFromUser model.session.user).username n.rootnameid GotOrgaInfo

                            else
                                Cmd.none

                        Nothing ->
                            Cmd.none
            in
            ( { model
                | session =
                    { session
                        | node_focus = data
                        , tension_head = Nothing
                        , project_data = Nothing
                        , tensions_data = Nothing
                        , tensions_int = Nothing
                        , tensions_ext = Nothing
                        , tensions_all = Nothing
                        , authorsPanel = Nothing
                        , labelsPanel = Nothing
                    }
              }
            , cmd
            )

        UpdateSessionFocusOnly data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | node_focus = data } }, Cmd.none )

        UpdateSessionPath data ->
            -- * Update also children. Children are used to manage tensions depth search option.
            -- * Fetch Pin tension if needed.
            let
                session =
                    model.session
            in
            case data of
                Just path ->
                    if path.focus.pinned == NotAsked || isFailure path.focus.pinned then
                        ( { model | session = { session | path_data = data, children = Nothing } }, Cmd.batch [ send (RefreshPinTension path.focus.nameid), Ports.propagatePath (List.map .nameid path.path) ] )

                    else
                        ( { model | session = { session | path_data = data, children = Nothing } }, Ports.propagatePath (List.map .nameid path.path) )

                Nothing ->
                    ( { model | session = { session | path_data = Nothing, children = Nothing } }, Cmd.none )

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
                            case getNode path_data.focus.nameid (Maybe.map (\d -> Success d) data |> withDefault NotAsked) of
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

                -- Eventually update orga_info
                orgaInfo =
                    Maybe.map2
                        (\oi nodes ->
                            { oi | n_tensions = Dict.foldl (\_ n count -> n.n_open_tensions + count) 0 nodes }
                        )
                        session.orgaInfo
                        data
            in
            ( { model | session = { session | tree_data = data, path_data = pdata, orgaInfo = orgaInfo } }, Cmd.none )

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

                -- Eventually Update pinned tensions
                pdata =
                    Maybe.map2
                        (\path th ->
                            let
                                focus =
                                    path.focus

                                newPinned =
                                    withMapData
                                        (Maybe.map
                                            (\pins ->
                                                case LE.findIndex (\p -> p.id == th.id) pins of
                                                    Just i ->
                                                        if th.isPinned then
                                                            LE.updateAt i
                                                                (\p ->
                                                                    { p
                                                                        | title = th.title
                                                                        , status = th.status
                                                                        , type_ = th.type_
                                                                    }
                                                                )
                                                                pins

                                                        else
                                                            LE.removeAt i pins

                                                    Nothing ->
                                                        if th.isPinned then
                                                            tensionHead2Pin th :: pins

                                                        else
                                                            pins
                                            )
                                        )
                                        focus.pinned
                            in
                            { path | focus = { focus | pinned = newPinned } }
                        )
                        session.path_data
                        data
                        |> (\x ->
                                case x of
                                    Just a ->
                                        Just a

                                    Nothing ->
                                        session.path_data
                           )

                -- history is deleted to save memory in page usong the Comments components
                tension_head =
                    case model.session.tension_head of
                        Just source ->
                            Maybe.map
                                (\th ->
                                    if th.history == Nothing then
                                        { th | history = source.history }

                                    else
                                        th
                                )
                                data

                        Nothing ->
                            data
            in
            ( { model | session = { session | tension_head = tension_head, path_data = pdata } }, Cmd.none )

        UpdateSessionProject data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | project_data = data } }, Cmd.none )

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
            ( { model | session = { session | window_pos = data } }, Ports.saveWindowpos data )

        UpdateSessionRecentActivityTab data ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | recent_activity_tab = data } }, Ports.saveRecentActivityTab data )

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
                    ( model, Cmd.batch [ send (UpdateSessionNotif data), Ports.updateNotif data ] )

                _ ->
                    ( model, Cmd.none )

        ToggleWatchOrga nameid ->
            case model.session.user of
                LoggedIn uctx ->
                    let
                        isWatching =
                            unwrap2 False .isWatching model.session.orgaInfo
                    in
                    ( model, toggleOrgaWatch apis uctx.username nameid (not isWatching) GotIsWatching )

                LoggedOut ->
                    ( model, Ports.raiseAuthNeeded )

        GotIsWatching result ->
            case parseErr result 2 of
                Authenticate ->
                    ( model, Ports.raiseAuthModal (uctxFromUser model.session.user) )

                OkAuth d ->
                    let
                        session =
                            model.session

                        orgaInfo =
                            Maybe.map
                                (\oi ->
                                    { oi | isWatching = Just d, n_watchers = ternary d (oi.n_watchers + 1) (max 0 (oi.n_watchers - 1)) }
                                )
                                session.orgaInfo
                    in
                    ( { model | session = { session | orgaInfo = orgaInfo } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotOrgaInfo result ->
            case result of
                Success data ->
                    let
                        session =
                            model.session

                        oi =
                            Maybe.map
                                (\nodes ->
                                    { data | n_tensions = Dict.foldl (\_ n count -> n.n_open_tensions + count) 0 nodes }
                                )
                                session.tree_data
                                |> withDefault data
                    in
                    ( { model | session = { session | orgaInfo = Just oi } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RefreshPinTension nameid ->
            ( model, queryPinnedTensions apis nameid AckPinTension )

        AckPinTension result ->
            let
                session =
                    model.session

                new_path =
                    Maybe.map
                        (\p ->
                            let
                                focus =
                                    p.focus
                            in
                            { p | focus = { focus | pinned = result } }
                        )
                        session.path_data
            in
            ( { model | session = { session | path_data = new_path } }, Ports.pathChanged )

        --Global Utils Msg
        NavigateNode nameid ->
            case urlToFractalRoute model.url of
                Just OverviewBaseUri ->
                    ( model, Nav.replaceUrl model.key (toLink OverviewBaseUri nameid []) )

                _ ->
                    ( model, Cmd.none )

        UpdateUserSession uctx ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | user = LoggedIn uctx } }
              -- Update Components when Uctx change !
            , [ Ports.saveUserCtx uctx
              , case session.node_focus of
                    Just n ->
                        getOrgaInfo apis uctx.username n.rootnameid GotOrgaInfo

                    Nothing ->
                        Cmd.none
              ]
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
                                    toLink UsersBaseUri uctx.username []
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

        OnCloseOutdatedVersion ->
            let
                session =
                    model.session

                orgaInfo =
                    Maybe.map
                        (\oi ->
                            { oi | client_version = session.apis.client_version }
                        )
                        session.orgaInfo
            in
            ( { model | session = { session | orgaInfo = orgaInfo } }, Cmd.none )

        OnPushSystemNotif result ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | system_notification = result } }, sendSleep OnClearSystemNotif 4000 )

        OnClearSystemNotif ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | system_notification = RemoteData.NotAsked } }, Cmd.none )

        -- Utils
        VOID ->
            ( model, Cmd.none )

        LogErr err ->
            ( model, Ports.logErr err )

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
        , Ports.pd Ports.updateMenuTreeFromJs TreeMenuSchema.decode LogErr UpdateSessionMenuTree
        , Ports.updateLangFromJs UpdateSessionLang
        , Ports.reloadNotifFromJs (always RefreshNotifCount)
        ]



-- VIEW
--


view : { page : Document msg, global : Model, url : Url, msg1 : String -> msg, msg2 : msg, onClearNotif : msg } -> Document msg
view { page, global, url, msg1, msg2, onClearNotif } =
    layout
        { page = page
        , url = url
        , session = global.session
        , msg1 = msg1
        , msg2 = msg2
        , onClearNotif = onClearNotif
        }


layout : { page : Document msg, url : Url, session : Session, msg1 : String -> msg, msg2 : msg, onClearNotif : msg } -> Document msg
layout { page, url, session, msg1, msg2, onClearNotif } =
    let
        ( notif_msg, notif_ok ) =
            case session.system_notification of
                RemoteData.Success msg ->
                    ( msg, Just True )

                RemoteData.Failure err ->
                    ( errorHttpToString err, Just False )

                _ ->
                    ( "", Nothing )
    in
    { title = page.title
    , body =
        [ div [ id "app" ]
            [ Lazy.lazy7 Navbar.view session.user session.notif session.orgaInfo session.apis url msg1 msg2
            , showIf (notif_ok /= Nothing) (viewNotif notif_msg (withDefault True notif_ok) onClearNotif)
            , div [ id "body" ] page.body
            , Footbar.view
            ]
        ]
    }


viewNotif : String -> Bool -> msg -> Html msg
viewNotif msg isOk closeMsg =
    let
        color =
            ternary isOk "is-success" "is-error"
    in
    div
        [ class "f6-notification notification is-light"
        , classList [ ( color, True ) ]
        ]
        [ button [ class "delete", onClick closeMsg ] []
        , div [ class "stealth-link" ]
            -- https://github.com/surprisetalk/elm-bulma/issues/17
            [ -- p [ class "title is-6 mb-2" ] [ text msg ]
              viewGqlErrorsLight [ msg ]
            ]
        ]



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
