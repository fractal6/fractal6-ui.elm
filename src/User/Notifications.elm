{-
  Fractale - Self-organisation for humans.
  Copyright (C) 2022 Fractale Co

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

module User.Notifications exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Browser.Navigation as Nav
import Codecs exposing (QuickDoc)
import Components.AuthModal as AuthModal
import Dict exposing (Dict)
import Extra exposing (mor, ternary, textH, upH, upT)
import Extra.Events exposing (onClickPD)
import Extra.Url exposing (queryBuilder, queryParser)
import Form exposing (isPostSendable)
import Form.Help as Help
import Fractal.Enum.ContractType as ContractType
import Fractal.Enum.Lang as Lang
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.TensionEvent as TensionEvent
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, small, span, strong, sup, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, target, title, type_)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading
    exposing
        ( GqlData
        , ModalData
        , RequestResult(..)
        , WebData
        , viewAuthNeeded
        , viewGqlErrors
        , viewHttpErrors
        , withMapData
        , withMaybeData
        , withMaybeSlowly
        )
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), focusFromNameid, nid2rootid)
import ModelCommon.Event
    exposing
        ( contractEventToText
        , contractToJonction
        , contractToLink
        , contractTypeToText
        , eventToIcon
        , eventToLink
        , eventTypeToText
        , viewContractMedia
        , viewEventMedia
        , viewNotifMedia
        )
import ModelCommon.View exposing (byAt, mediaTension, viewOrga)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.PatchUser exposing (markAllAsRead, markAsRead)
import Query.QueryNotifications exposing (queryNotifications)
import Query.QueryTension exposing (queryAssignedTensions)
import RemoteData exposing (RemoteData)
import Session exposing (Conf, GlobalCmd(..))
import Task
import Text as T
import Time
import Url exposing (Url)



---- PROGRAM ----


type alias Flags =
    ()


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


mapGlobalOutcmds : List GlobalCmd -> ( List (Cmd Msg), List (Cmd Global.Msg) )
mapGlobalOutcmds gcmds =
    gcmds
        |> List.map
            (\m ->
                case m of
                    DoNavigate link ->
                        ( send (Navigate link), Cmd.none )

                    DoReplaceUrl url ->
                        ( Cmd.none, send (ReplaceUrl url) )

                    DoUpdateToken ->
                        ( Cmd.none, send UpdateUserToken )

                    DoUpdateUserSession uctx ->
                        ( Cmd.none, send (UpdateUserSession uctx) )

                    _ ->
                        ( Cmd.none, Cmd.none )
            )
        |> List.unzip



---- MODEL----


type alias Model =
    { uctx : UserCtx
    , notifications_data : GqlData UserEvents
    , assigned_data : GqlData (Dict String (List Tension))
    , eid : String
    , menuFocus : MenuNotif
    , can_referer : Maybe Url

    -- Common
    , conf : Conf
    , help : Help.State
    , refresh_trial : Int
    , empty : {}
    , authModal : AuthModal.State
    }


type MenuNotif
    = NotificationsMenu
    | AssignedMenu


menuEncoder : MenuNotif -> String
menuEncoder menu =
    case menu of
        NotificationsMenu ->
            "last"

        AssignedMenu ->
            "assigned"


menuDecoder : String -> MenuNotif
menuDecoder menu =
    case menu of
        "assigned" ->
            AssignedMenu

        _ ->
            NotificationsMenu


menuList : List MenuNotif
menuList =
    [ NotificationsMenu, AssignedMenu ]


menuToString : MenuNotif -> ( String, String )
menuToString menu =
    case menu of
        NotificationsMenu ->
            ( T.notifications, T.notifications )

        AssignedMenu ->
            ( T.assigned, T.assignedTensions )


menuToIcon : MenuNotif -> String
menuToIcon menu =
    case menu of
        NotificationsMenu ->
            "icon-mail"

        AssignedMenu ->
            "icon-exchange"



---- MSG ----


type Msg
    = Submit (Time.Posix -> Msg) -- Get Current Time
    | LoadNotifications
    | LoadAssigned
    | GotNotifications (GqlData UserEvents)
    | GotAssigned (GqlData (Dict String (List Tension)))
    | MarkAsRead String
    | GotMarkAsRead (GqlData IdPayload)
    | MarkAllAsRead
    | GotMarkAllAsRead (GqlData IdPayload)
    | ChangeMenuFocus MenuNotif
      -- Common
    | NoMsg
    | PassedSlowLoadTreshold -- timer
    | LogErr String
    | Navigate String
    | DoOpenModal
    | DoCloseModal ModalData
    | GoBack
      -- Help
    | HelpMsg Help.Msg
    | AuthModalMsg AuthModal.Msg



-- INIT --


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        ( uctx, cmds, gcmds ) =
            case global.session.user of
                LoggedIn uctx_ ->
                    ( uctx_
                    , case menu of
                        NotificationsMenu ->
                            [ send LoadNotifications, sendSleep PassedSlowLoadTreshold 500 ]

                        AssignedMenu ->
                            [ send LoadAssigned, sendSleep PassedSlowLoadTreshold 500 ]
                    , [ send (UpdateSessionFocus Nothing) ]
                    )

                LoggedOut ->
                    ( initUserctx, [], [ Global.navigate <| Route.Login ] )

        -- Query parameters
        conf =
            { screen = global.session.screen, now = global.now, lang = global.session.lang }

        query =
            queryParser global.url

        menu =
            Dict.get "m" query |> withDefault [] |> List.head |> withDefault "" |> menuDecoder

        model =
            { uctx = uctx
            , notifications_data = Loading
            , assigned_data = Loading
            , eid = ""
            , menuFocus = menu
            , can_referer =
                Maybe.map
                    (\r ->
                        if
                            (String.dropLeft 1 r.path
                                |> String.split "/"
                                |> List.head
                                |> withDefault ""
                                |> String.append "/"
                            )
                                == toHref Route.Notifications
                        then
                            withDefault r global.session.can_referer

                        else
                            r
                    )
                    global.session.referer

            -- common
            , conf = conf
            , help = Help.init global.session.user conf
            , refresh_trial = 0
            , empty = {}
            , authModal = AuthModal.init global.session.user Nothing
            }
    in
    ( model
    , Cmd.batch cmds
    , Cmd.batch (gcmds ++ [ send (UpdateCanReferer model.can_referer) ])
    )



---- UPDATE ----


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global message model =
    let
        apis =
            global.session.apis
    in
    case message of
        PassedSlowLoadTreshold ->
            ( { model | notifications_data = withMaybeSlowly model.notifications_data, assigned_data = withMaybeSlowly model.assigned_data }, Cmd.none, Cmd.none )

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        LoadNotifications ->
            ( model, queryNotifications apis { first = 50, uctx = model.uctx } GotNotifications, Cmd.none )

        LoadAssigned ->
            ( model, queryAssignedTensions apis { first = 50, uctx = model.uctx } GotAssigned, Cmd.none )

        GotNotifications result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( model, Ports.raiseAuthModal model.uctx, Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep LoadNotifications 500, send UpdateUserToken )

                OkAuth data ->
                    let
                        ( i, j ) =
                            List.filter (\x -> not x.isRead) data
                                |> List.foldr
                                    (\a ( ne, nc ) ->
                                        case List.head a.event of
                                            Just (TensionEvent _) ->
                                                ( ne + 1, nc )

                                            Just (NotifEvent n) ->
                                                ( ne + 1, nc )

                                            Just (ContractEvent c) ->
                                                ( ne, nc + 1 )

                                            Nothing ->
                                                ( ne, nc )
                                    )
                                    ( 0, 0 )
                    in
                    ( { model | notifications_data = result }
                    , Cmd.none
                    , send (UpdateSessionNotif { unread_events = i, pending_contracts = j })
                    )

                _ ->
                    ( { model | notifications_data = result }, Cmd.none, Cmd.none )

        GotAssigned result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( model, Ports.raiseAuthModal model.uctx, Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep LoadAssigned 500, send UpdateUserToken )

                OkAuth _ ->
                    ( { model | assigned_data = result }, Cmd.none, Cmd.none )

                _ ->
                    ( { model | assigned_data = result }, Cmd.none, Cmd.none )

        MarkAsRead eid ->
            ( { model | eid = eid }, markAsRead apis eid True GotMarkAsRead, Cmd.none )

        GotMarkAsRead result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( model, Ports.raiseAuthModal model.uctx, Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep (MarkAsRead model.eid) 500, send UpdateUserToken )

                OkAuth _ ->
                    -- modified event inplace toSet isRead...
                    let
                        newData =
                            withMapData
                                (LE.updateIf (\a -> a.id == model.eid)
                                    (\a -> { a | isRead = True })
                                )
                                model.notifications_data
                    in
                    ( { model | notifications_data = newData }, Cmd.none, send RefreshNotifCount )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        MarkAllAsRead ->
            ( model, markAllAsRead apis model.uctx.username GotMarkAllAsRead, Cmd.none )

        ChangeMenuFocus menu ->
            let
                url =
                    toHref Route.Notifications

                query =
                    queryBuilder
                        [ ( "m", menuEncoder menu ) ]
            in
            ( model, Cmd.none, Nav.pushUrl global.key (url ++ "?" ++ query) )

        GotMarkAllAsRead result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( model, Ports.raiseAuthModal model.uctx, Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep MarkAllAsRead 500, send UpdateUserToken )

                OkAuth _ ->
                    -- modified event inplace toSet isRead...
                    let
                        newData =
                            withMapData
                                (List.map (\a -> ternary (editableEvent a.event) { a | isRead = True } a))
                                model.notifications_data
                    in
                    ( { model | notifications_data = newData }, Cmd.none, send RefreshNotifCount )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        -- Common
        NoMsg ->
            ( model, Cmd.none, Cmd.none )

        LogErr err ->
            ( model, Ports.logErr err, Cmd.none )

        Navigate url ->
            ( model, Cmd.none, Nav.pushUrl global.key url )

        DoOpenModal ->
            ( model, Ports.open_modal "actionModal", Cmd.none )

        DoCloseModal data ->
            let
                gcmd =
                    if data.link /= "" then
                        send (Navigate data.link)

                    else
                        Cmd.none
            in
            ( model, gcmd, Ports.close_modal )

        GoBack ->
            ( model, Cmd.none, send <| NavigateRaw <| withDefault "" <| Maybe.map .path <| model.can_referer )

        -- Help
        HelpMsg msg ->
            let
                ( help, out ) =
                    Help.update apis msg model.help

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | help = help }, out.cmds |> List.map (\m -> Cmd.map HelpMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        AuthModalMsg msg ->
            let
                ( data, out ) =
                    AuthModal.update apis msg model.authModal

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds

                -- reload silently the page if needed
                cmds_extra =
                    out.result
                        |> Maybe.map
                            (\o ->
                                if Tuple.first o == True then
                                    [ Nav.replaceUrl global.key (Url.toString global.url) ]

                                else
                                    []
                            )
                        |> withDefault []
            in
            ( { model | authModal = data }, out.cmds |> List.map (\m -> Cmd.map AuthModalMsg m) |> List.append (cmds ++ cmds_extra) |> Cmd.batch, Cmd.batch gcmds )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ _ =
    [ Ports.mcPD Ports.closeModalFromJs LogErr DoCloseModal
    ]
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        ++ (AuthModal.subscriptions |> List.map (\s -> Sub.map AuthModalMsg s))
        |> Sub.batch



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    { title = T.notifications
    , body =
        [ view_ global model
        , Help.view model.empty model.help |> Html.map HelpMsg
        , AuthModal.view model.empty model.authModal |> Html.map AuthModalMsg
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ id "notifications", class "top-section columns" ]
        [ div [ class "column is-2 is-3-fullhd" ] [ viewMenu model ]
        , div [ class "column is-8 is-6-fullhd pt-0" ]
            [ div []
                [ div
                    [ class "is-strong arrow-left is-w is-h bc is-pulled-left"
                    , attribute "style" "position:relative; top:-15px;"
                    , title T.goBack
                    , onClick GoBack
                    ]
                    []
                , case model.menuFocus of
                    NotificationsMenu ->
                        div [ class "is-2 has-text-centered is-pulled-right" ] [ div [ class "button is-small", onClick MarkAllAsRead ] [ text T.markAllAsRead ] ]

                    AssignedMenu ->
                        text ""
                ]
            , br [] []
            , h2 [ class "title" ] [ text (menuToString model.menuFocus |> Tuple.second) ]
            , case model.menuFocus of
                NotificationsMenu ->
                    case model.notifications_data of
                        Success notifications ->
                            if List.length notifications == 0 then
                                p [ class "content" ] [ text T.noNotificationsYet ]

                            else
                                viewNotifications model.conf notifications

                        LoadingSlowly ->
                            div [ class "spinner" ] []

                        Failure err ->
                            viewGqlErrors err

                        _ ->
                            text ""

                AssignedMenu ->
                    case model.assigned_data of
                        Success assigned ->
                            if Dict.size assigned == 0 then
                                p [ class "content" ] [ text T.noAssignedYet ]

                            else
                                viewAssigned model.conf assigned

                        LoadingSlowly ->
                            div [ class "spinner" ] []

                        Failure err ->
                            viewGqlErrors err

                        _ ->
                            text ""
            ]
        ]


viewMenu : Model -> Html Msg
viewMenu model =
    nav [ id "menuSettings", class "menu mt-desktop" ]
        [ ul [ class "menu-list" ] <|
            (menuList
                |> List.map
                    (\x ->
                        [ li []
                            [ a [ onClickPD (ChangeMenuFocus x), target "_blank", classList [ ( "is-active", x == model.menuFocus ) ] ]
                                [ A.icon1 (menuToIcon x) (menuToString x |> Tuple.first) ]
                            ]
                        ]
                    )
                |> List.concat
            )
        ]


viewNotifications : Conf -> UserEvents -> Html Msg
viewNotifications conf notifications =
    notifications
        |> List.map
            (\ue -> Lazy.lazy2 viewUserEvent conf ue)
        |> div [ class "box is-shrinked" ]


viewUserEvent : Conf -> UserEvent -> Html Msg
viewUserEvent conf ue =
    let
        firstEvent =
            List.head ue.event
    in
    case firstEvent of
        Just (TensionEvent e_) ->
            let
                -- LabelAdded is the first entry of the list
                -- when a label is added at the creation of a tension.
                -- I guess it's because the timestamp are equal which messed up the orders (@dgraph) ?
                e =
                    ue.event
                        |> List.map
                            (\uee ->
                                case uee of
                                    TensionEvent ee ->
                                        if ee.event_type == TensionEvent.Created then
                                            Just ee

                                        else
                                            Nothing

                                    _ ->
                                        Nothing
                            )
                        |> List.filterMap identity
                        |> List.head
                        |> withDefault e_

                node =
                    e.tension.receiver

                ev =
                    Dict.fromList
                        [ ( "id", ue.id )
                        , if e.event_type == TensionEvent.UserLeft && node.nameid == nid2rootid node.nameid && node.nameid == e.tension.emitterid then
                            -- Anchor tension !
                            ( "title", T.userLeft_orga_event )

                          else
                            ( "title", eventTypeToText e.event_type )
                        , ( "title_", e.tension.title )
                        , ( "target", node.name )
                        , ( "orga", nid2rootid node.nameid )
                        , ( "date", e.createdAt )
                        , ( "author", e.createdBy.username )
                        , ( "link", eventToLink ue e )
                        , ( "icon", eventToIcon e.event_type )
                        ]
            in
            viewNotif False ue node (viewEventMedia conf False ev)

        Just (ContractEvent c) ->
            let
                node =
                    c.tension.receiver

                ev =
                    Dict.fromList
                        [ ( "id", ue.id )
                        , ( "contract", contractTypeToText c.contract_type )
                        , ( "jonction", contractToJonction c.contract_type )
                        , ( "title", contractEventToText c.event.event_type )
                        , ( "target", node.name )
                        , ( "orga", nid2rootid node.nameid )
                        , ( "date", c.createdAt )
                        , ( "author", c.createdBy.username )
                        , ( "link", contractToLink ue c )
                        , ( "icon", eventToIcon c.event.event_type )
                        ]
            in
            viewNotif True ue node (viewContractMedia conf ev)

        Just (NotifEvent n) ->
            case n.tension of
                Just tension ->
                    let
                        node =
                            tension.receiver

                        link =
                            case n.contract of
                                Just contract ->
                                    -- Contract notification (e.g contract cancelled)
                                    (Route.Tension_Dynamic_Dynamic_Contract_Dynamic { param1 = nid2rootid tension.receiver.nameid, param2 = tension.id, param3 = contract.id } |> toHref)
                                        ++ "?eid="
                                        ++ ue.id

                                Nothing ->
                                    withDefault
                                        ((Route.Tension_Dynamic_Dynamic { param1 = nid2rootid tension.receiver.nameid, param2 = tension.id } |> toHref)
                                            ++ "?eid="
                                            ++ ue.id
                                        )
                                        n.link

                        ev =
                            Dict.fromList
                                [ ( "id", ue.id )
                                , ( "title", withDefault "no input message." n.message )
                                , ( "link", link )

                                --, ( "title_", tension.title )
                                , ( "target", node.name )
                                , ( "orga", nid2rootid node.nameid )
                                , ( "date", n.createdAt )
                                , ( "author", n.createdBy.username )
                                , ( "icon", "icon-info" )
                                ]
                    in
                    viewNotif False ue node (viewNotifMedia conf ev)

                Nothing ->
                    -- Something has beed removed, contract ?
                    text ""

        Nothing ->
            text ""


viewNotif : Bool -> UserEvent -> PNode -> Html Msg -> Html Msg
viewNotif isContract ue node content =
    let
        ( tooltip_cls, tooltip_txt ) =
            if isContract then
                ( "has-text-info", T.voteWaited )

            else
                ( "has-text-link is-w", T.markAsRead )
    in
    div [ class "media mediaBox is-hoverable" ]
        [ div [ class "media-left" ] [ viewOrga True node.nameid ]
        , div [ class "media-content" ] [ content ]
        , if not ue.isRead then
            div
                [ class "media-right tooltip"
                , attribute "data-tooltip" tooltip_txt
                , if isContract then
                    onClick NoMsg

                  else
                    onClick (MarkAsRead ue.id)
                ]
                [ div [ class ("Circle " ++ tooltip_cls) ] [] ]

          else
            text ""
        ]


editableEvent event =
    case List.head event of
        Just (ContractEvent _) ->
            False

        _ ->
            True


viewAssigned : Conf -> Dict String (List Tension) -> Html Msg
viewAssigned conf tensions_d =
    Dict.keys tensions_d
        |> List.map
            (\rootid ->
                let
                    tensions =
                        Dict.get rootid tensions_d |> withDefault []

                    orgaName =
                        upT rootid
                in
                div []
                    [ div [ class "mb-2" ] [ span [ class "subtitle" ] [ text orgaName ] ]
                    , tensions
                        |> List.map (\t -> mediaTension conf (focusFromNameid t.receiver.nameid) t True True "is-size-6 t-o" Navigate)
                        |> div [ id "tensionsTab", class "box is-shrinked mb-5" ]
                    ]
            )
        |> div []
