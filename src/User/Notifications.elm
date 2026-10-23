{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2026 Fractale Co

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


module User.Notifications exposing (Flags, Model, Msg, groupNotifs, init, page, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Browser.Navigation as Nav
import Components.AuthModal as AuthModal
import Dict exposing (Dict)
import Form.Help as Help
import Fractale.Codecs exposing (FractalBaseRoute(..), focusFromNameid, nid2rootid, toLink)
import Fractale.Error exposing (viewGqlErrors)
import Fractale.Event exposing (contractEventToText, contractToJonction, contractToLink, contractTypeToText, eventToIcon, eventToLink, eventTypeToText, viewContractMedia, viewEventMedia, viewNotifMedia)
import Fractale.User exposing (UserState(..), uctxFromUser)
import Fractale.View exposing (counter, mediaTension, viewGoBack, viewOrga)
import Generated.Route as Route exposing (toHref)
import Global exposing (Msg(..))
import Html exposing (Attribute, Html, a, br, div, h2, i, li, nav, p, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, id, target, title)
import Html.Events exposing (onClick)
import Html.Lazy as Lazy
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), withMapData, withMaybeSlowly)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.PatchUser exposing (markAllAsRead, markAsRead)
import Query.QueryNode exposing (queryOrgaNode)
import Query.QueryNotifications exposing (queryNotifications)
import Query.QueryTension exposing (queryAssignedTensions)
import Schema.Enum.RoleType as RoleType
import Schema.Enum.TensionEvent as TensionEvent
import Session exposing (CommonMsg, GlobalCmd(..), SessionCommon, toF6Referer)
import Set exposing (Set)
import Text as T
import Time
import Url exposing (Url)
import Utils.Bool exposing (ternary)
import Utils.Cmd exposing (send, sendNow, sendSleep)
import Utils.DomEvents exposing (onClickPD, onClickSP)
import Utils.String exposing (upT)
import Utils.Url exposing (queryBuilder, queryParser)



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
                        ( Cmd.none, send (NavigateRaw link) )

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
    , notif : NotifCount
    , notifications_data : GqlData (List (List UserEvent)) -- rows: adjacent comments/creation of a tension are merged
    , expanded : Set String -- merged rows (head event id) shown expanded
    , orga_avatars : Dict String String -- rootnameid -> avatar file id
    , assigned_data : GqlData (Dict String (List Tension))
    , eids : List String
    , goto_link : Maybe String -- navigate once eids are marked as read
    , menuFocus : MenuNotif

    -- Common
    , session : SessionCommon
    , help : Help.State
    , refresh_trial : Int
    , can_referer : Maybe Url
    , empty : {}
    , commonOp : CommonMsg Msg
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


menuToString : SessionCommon -> MenuNotif -> ( String, String )
menuToString session menu =
    case menu of
        NotificationsMenu ->
            ( T.inbox, T.notifications )

        AssignedMenu ->
            ( T.assigned, T.assignedTensions session.lexicon )


menuToIcon : MenuNotif -> String
menuToIcon menu =
    case menu of
        NotificationsMenu ->
            "icon-mail"

        AssignedMenu ->
            "icon-exchange"


menuToCount : MenuNotif -> NotifCount -> Html Msg
menuToCount menu notif =
    case menu of
        NotificationsMenu ->
            let
                c_event =
                    case notif.unread_events of
                        0 ->
                            text ""

                        i ->
                            counter i

                c_contract =
                    case notif.pending_contracts of
                        0 ->
                            text ""

                        i ->
                            counter i
            in
            span []
                [ span
                    [ title T.unreadNotif
                    ]
                    [ c_event ]
                , span
                    [ class "is-contract-badge-bg"
                    , title T.pendingContract
                    ]
                    [ c_contract ]
                ]

        AssignedMenu ->
            case notif.assigned_tensions of
                0 ->
                    text ""

                i ->
                    counter i



-- INIT --


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        ( uctx, cmds, gcmds ) =
            case global.session.common.user of
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

        menu =
            Dict.get "m" global.session.common.query |> withDefault [] |> List.head |> withDefault "" |> menuDecoder

        model =
            { uctx = uctx
            , notif = global.session.data.notif
            , notifications_data = Loading
            , expanded = Set.empty
            , orga_avatars = Dict.empty
            , assigned_data = Loading
            , eids = []
            , goto_link = Nothing
            , menuFocus = menu

            -- common
            , session = global.session.common
            , help = Help.init global.session.common
            , refresh_trial = 0
            , can_referer = toF6Referer (Route.fromUrl global.url |> withDefault Route.Top) global.session
            , empty = {}
            , commonOp = CommonMsg NoMsg LogErr
            , authModal = AuthModal.init Nothing global.session.common
            }
    in
    ( model
    , Cmd.batch cmds
    , Cmd.batch (gcmds ++ [ send (UpdateCanReferer model.can_referer) ])
    )



---- MSG ----


type Msg
    = Submit (Time.Posix -> Msg) -- Get Current Time
    | LoadNotifications
    | LoadAssigned
    | GotNotifications (GqlData UserEvents)
    | GotOrgaAvatars (GqlData (List OrgaNode))
    | GotAssigned (GqlData (Dict String (List Tension)))
    | MarkAsRead (List String) (Maybe String)
    | ToggleGroup String
    | GotMarkAsRead (GqlData IdPayload)
    | MarkAllAsRead
    | GotMarkAllAsRead (GqlData IdPayload)
    | ChangeMenuFocus MenuNotif
    | UpdateNotif NotifCount
      -- Common
    | NoMsg
    | PassedSlowLoadTreshold -- timer
    | LogErr String
    | DoOpenModal
    | DoCloseModal ModalData
    | GoBack
      -- Help
    | HelpMsg Help.Msg
    | AuthModalMsg AuthModal.Msg



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
            ( model, sendNow nextMsg, Cmd.none )

        LoadNotifications ->
            ( model
            , queryNotifications apis { first = 50, uctx = model.uctx } GotNotifications
            , send RefreshNotifCount
            )

        LoadAssigned ->
            ( model, queryAssignedTensions apis { first = 50, uctx = model.uctx } GotAssigned, Cmd.none )

        GotNotifications result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( model, Ports.raiseAuthModal model.uctx, Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep LoadNotifications 500, send UpdateUserToken )

                OkAuth data ->
                    --let
                    -- Exemple of computation (unread_events, unread_contracts with foldr.
                    --    ( i, j ) =
                    --        List.filter (\x -> not x.isRead) data
                    --            |> List.foldr
                    --                (\a ( ne, nc ) ->
                    --                    case List.head a.event of
                    --                        Just (TensionEvent _) ->
                    --                            ( ne + 1, nc )
                    --                        Just (NotifEvent n) ->
                    --                            ( ne + 1, nc )
                    --                        Just (ContractEvent c) ->
                    --                            ( ne, nc + 1 )
                    --                        Nothing ->
                    --                            ( ne, nc )
                    --                )
                    --                ( 0, 0 )
                    --in
                    ( { model | notifications_data = withMapData groupNotifs result }
                    , case data |> List.filterMap eventRootid |> LE.unique of
                        [] ->
                            Cmd.none

                        rootids ->
                            queryOrgaNode apis rootids GotOrgaAvatars
                    , Cmd.none
                    )

                _ ->
                    ( { model | notifications_data = withMapData groupNotifs result }, Cmd.none, Cmd.none )

        GotOrgaAvatars result ->
            -- No avatar on failure: the initials stay.
            ( { model | orga_avatars = Loading.withDefaultData [] result |> List.filterMap (\o -> Maybe.map (Tuple.pair o.nameid) o.avatar) |> Dict.fromList }
            , Cmd.none
            , Cmd.none
            )

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

        MarkAsRead eids link ->
            ( { model | eids = eids, goto_link = link }, markAsRead apis eids True GotMarkAsRead, Cmd.none )

        ToggleGroup gid ->
            ( { model | expanded = ternary (Set.member gid model.expanded) Set.remove Set.insert gid model.expanded }, Cmd.none, Cmd.none )

        GotMarkAsRead result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( model, Ports.raiseAuthModal model.uctx, Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep (MarkAsRead model.eids model.goto_link) 500, send UpdateUserToken )

                OkAuth _ ->
                    -- modified event inplace toSet isRead...
                    let
                        newData =
                            withMapData
                                (List.map (List.map (\a -> ternary (List.member a.id model.eids) { a | isRead = True } a)))
                                model.notifications_data
                    in
                    ( { model | notifications_data = newData }
                    , Cmd.none
                    , Cmd.batch [ send RefreshNotifCount, gotoLink model.goto_link ]
                    )

                _ ->
                    -- Do not trap the user on failure
                    ( model, Cmd.none, gotoLink model.goto_link )

        MarkAllAsRead ->
            ( model, markAllAsRead apis model.uctx.username GotMarkAllAsRead, Cmd.none )

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
                                (List.map (List.map (\a -> ternary (editableEvent a.event) { a | isRead = True } a)))
                                model.notifications_data
                    in
                    ( { model | notifications_data = newData }, Cmd.none, send RefreshNotifCount )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        ChangeMenuFocus menu ->
            let
                url =
                    toHref Route.Notifications

                query =
                    queryBuilder
                        [ ( "m", menuEncoder menu ) ]
            in
            ( model, Cmd.none, Nav.pushUrl global.key (url ++ "?" ++ query) )

        UpdateNotif notif ->
            ( { model | notif = notif }, Cmd.none, Cmd.none )

        -- Common
        NoMsg ->
            ( model, Cmd.none, Cmd.none )

        LogErr err ->
            ( model, Ports.logErr err, Cmd.none )

        DoOpenModal ->
            ( model, Ports.open_modal "actionModal", Cmd.none )

        DoCloseModal data ->
            let
                gcmd =
                    if data.link /= "" then
                        send (NavigateRaw data.link)

                    else
                        Cmd.none
            in
            ( model, Cmd.none, Cmd.batch [ gcmd, Ports.close_modal ] )

        GoBack ->
            ( model
            , Cmd.none
            , case model.can_referer of
                Just r ->
                    send <|
                        NavigateRaw <|
                            r.path
                                ++ (r.query |> Maybe.map (\uq -> "?" ++ uq) |> Maybe.withDefault "")

                Nothing ->
                    send (NavigateRaw (toLink UsersBaseUri (uctxFromUser model.session.user).username []))
            )

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
                                if Tuple.first o then
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
    , Ports.updateNotifFromJs UpdateNotif
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
        , Lazy.lazy2 Help.view model.empty model.help |> Html.map HelpMsg
        , Lazy.lazy2 AuthModal.view model.empty model.authModal |> Html.map AuthModalMsg
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ id "notifications", class "top-section columns" ]
        [ div [ class "column is-one-quarter-tablet is-one-fifth-desktop" ]
            [ viewGoBack GoBack
            , viewMenu model
            ]
        , div [ class "column is-three-quarters-widescreen pt-0" ]
            [ case model.menuFocus of
                NotificationsMenu ->
                    div [ class "is-2 has-text-centered is-pulled-right" ] [ div [ class "button is-small", onClick MarkAllAsRead ] [ text T.markAllAsRead ] ]

                AssignedMenu ->
                    text ""
            , h2 [ class "title" ] [ text (menuToString model.session model.menuFocus |> Tuple.second) ]
            , case model.menuFocus of
                NotificationsMenu ->
                    case model.notifications_data of
                        Success notifications ->
                            if List.length notifications == 0 then
                                p [ class "content" ] [ text T.noNotificationsYet ]

                            else
                                viewNotifications model.session model.orga_avatars model.expanded notifications

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
                                p [ class "content" ] [ text (T.noAssignedYet model.session.lexicon) ]

                            else
                                viewAssigned model.commonOp model.session assigned

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
    nav [ id "menuSettings", class "menu" ]
        [ ul [ class "menu-list" ] <|
            (menuList
                |> List.concatMap
                    (\x ->
                        [ li []
                            [ a
                                [ classList [ ( "is-active", x == model.menuFocus ) ]
                                , target "_blank"
                                , onClickPD
                                    (ChangeMenuFocus x)
                                ]
                                [ div [ class "is-inline-flex" ]
                                    [ A.icon1_wrap (menuToIcon x) (menuToString model.session x |> Tuple.first)
                                    , menuToCount x model.notif
                                    ]
                                ]
                            ]
                        ]
                    )
            )
        ]


viewNotifications : SessionCommon -> Dict String String -> Set String -> List (List UserEvent) -> Html Msg
viewNotifications session avatars expanded groups =
    -- Flat rows (no wrapper) to keep the .media + .media borders.
    groups
        |> List.concatMap
            (\group ->
                case group of
                    [ ue ] ->
                        [ Lazy.lazy4 viewUserEvent session avatars "" ue ]

                    ue :: _ ->
                        let
                            isExpanded =
                                Set.member ue.id expanded
                        in
                        Lazy.lazy4 viewCommentGroup session avatars isExpanded group
                            :: ternary isExpanded (List.map (Lazy.lazy4 viewUserEvent session avatars "is-child") group) []

                    [] ->
                        []
            )
        |> div [ class "box is-shrinked" ]


{-| Group the adjacent comment/creation notifications of a same tension (list ordered by date desc).
-}
groupNotifs : UserEvents -> List (List UserEvent)
groupNotifs =
    LE.groupWhile (\a b -> Maybe.map2 (\x y -> x.tension.id == y.tension.id) (mergeEvent a) (mergeEvent b) == Just True)
        >> List.map (\( x, xs ) -> x :: xs)


{-| Comment or creation event (not always the head, see viewUserEvent).
-}
mergeEvent : UserEvent -> Maybe EventNotif
mergeEvent ue =
    ue.event
        |> LE.findMap
            (\x ->
                case x of
                    TensionEvent e ->
                        ternary (List.member e.event_type [ TensionEvent.CommentPushed, TensionEvent.Created ]) (Just e) Nothing

                    _ ->
                        Nothing
            )


gotoLink : Maybe String -> Cmd Global.Msg
gotoLink link =
    Maybe.map (NavigateRaw >> send) link |> withDefault Cmd.none


{-| Merged row of the comments/creation of a tension: links to the unread range (or the whole group) and marks it as read.
-}
viewCommentGroup : SessionCommon -> Dict String String -> Bool -> List UserEvent -> Html Msg
viewCommentGroup session avatars isExpanded group =
    case ( group, List.head group |> Maybe.andThen mergeEvent ) of
        ( ue :: _, Just e ) ->
            let
                node =
                    e.tension.receiver

                unread =
                    List.filter (not << .isRead) group

                unreadIds =
                    List.map .id unread

                oldest =
                    LE.last (ternary (unread == []) group unread)
                        |> Maybe.andThen mergeEvent
                        |> Maybe.map .createdAt
                        |> withDefault e.createdAt

                link =
                    (Route.Tension_Dynamic_Dynamic { param1 = nid2rootid node.nameid, param2 = e.tension.id } |> toHref)
                        ++ "?goto="
                        ++ oldest
                        ++ "&until="
                        ++ e.createdAt

                ev =
                    Dict.fromList
                        [ ( "id", ue.id )
                        , ( "title", eventTypeToText session.lexicon e.event_type )
                        , ( "title_", e.tension.title )
                        , ( "target", node.name )
                        , ( "orga", nid2rootid node.nameid )
                        , ( "date", e.createdAt )
                        , ( "author", e.createdBy.username )
                        , ( "link", link )
                        , ( "icon", eventToIcon e.event_type )
                        ]

                linkAttrs =
                    ternary (unreadIds == []) [] [ onClickPD (MarkAsRead unreadIds (Just link)), target "_blank" ]

                nUnread =
                    List.length unreadIds

                nRead =
                    List.length group - nUnread

                -- The whole row toggles (see rowAttrs): the chevron is only an indicator.
                extra =
                    [ ternary (nUnread == 0) (text "") (span [ class "is-event-badge-bg", title T.unreadEvents ] [ counter nUnread ])
                    , ternary (nRead == 0) (text "") (span [ title T.readEvents ] [ counter nRead ])
                    , span [ class "button is-small has-text-text is-text ml-2" ] [ A.icon (ternary isExpanded "icon-chevron-up" "icon-chevron-down") ]
                    ]

                rowAttrs =
                    [ class "is-clickable", onClick (ToggleGroup ue.id) ]
            in
            viewNotif session avatars rowAttrs False unreadIds node extra (viewEventMedia session False linkAttrs ev)

        _ ->
            text ""


viewUserEvent : SessionCommon -> Dict String String -> String -> UserEvent -> Html Msg
viewUserEvent session avatars cls ue =
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
                        |> List.filterMap
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
                        |> List.head
                        |> withDefault e_

                node =
                    e.tension.receiver

                ev =
                    Dict.fromList
                        [ ( "id", ue.id )
                        , if e.event_type == TensionEvent.UserLeft && node.nameid == nid2rootid node.nameid && node.nameid == e.tension.emitterid then
                            -- Anchor tension !
                            case RoleType.fromString (withDefault "" e.new) of
                                Just RoleType.Owner ->
                                    ( "title", T.userLeft_owner_event )

                                Just RoleType.Guest ->
                                    ( "title", T.userLeft_orga_event )

                                _ ->
                                    ( "title", "Unknow UserLeft event, please report." )

                          else
                            ( "title", eventTypeToText session.lexicon e.event_type )
                        , ( "title_", e.tension.title )
                        , ( "target", node.name )
                        , ( "orga", nid2rootid node.nameid )
                        , ( "date", e.createdAt )
                        , ( "author", e.createdBy.username )
                        , ( "link", eventToLink ue e )
                        , ( "icon", eventToIcon e.event_type )
                        ]
            in
            viewNotif session avatars [ class cls ] False (ternary ue.isRead [] [ ue.id ]) node [] (viewEventMedia session False [] ev)

        Just (ContractEvent c) ->
            let
                node =
                    c.tension.receiver

                ev =
                    Dict.fromList
                        [ ( "id", ue.id )
                        , ( "contract", contractTypeToText c.contract_type )
                        , ( "jonction", contractToJonction c.contract_type )
                        , ( "title", contractEventToText session.lexicon Nothing c.event.event_type )
                        , ( "target", node.name )
                        , ( "orga", nid2rootid node.nameid )
                        , ( "date", c.createdAt )
                        , ( "author", c.createdBy.username )
                        , ( "link", contractToLink ue c )
                        , ( "icon", eventToIcon c.event.event_type )
                        ]
            in
            viewNotif session avatars [ class cls ] True (ternary ue.isRead [] [ ue.id ]) node [] (viewContractMedia session ev)

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
                    viewNotif session avatars [ class cls ] False (ternary ue.isRead [] [ ue.id ]) node [] (viewNotifMedia session ev)

                Nothing ->
                    -- Something has beed removed, contract ?
                    text ""

        Nothing ->
            text ""


{-| Root nameid of the event receiver.
-}
eventRootid : UserEvent -> Maybe String
eventRootid ue =
    case List.head ue.event of
        Just (TensionEvent e) ->
            Just (nid2rootid e.tension.receiver.nameid)

        Just (ContractEvent c) ->
            Just (nid2rootid c.tension.receiver.nameid)

        Just (NotifEvent n) ->
            Maybe.map (.receiver >> .nameid >> nid2rootid) n.tension

        Nothing ->
            Nothing


{-| Notification row; the read badge marks `unreadIds` as read, `extra` goes before it.
-}
viewNotif : SessionCommon -> Dict String String -> List (Attribute Msg) -> Bool -> List String -> PNode -> List (Html Msg) -> Html Msg -> Html Msg
viewNotif session avatars attrs isContract unreadIds node extra content =
    let
        ( tooltip_cls, tooltip_txt ) =
            if isContract then
                ( "is-contract-badge", T.voteWaited )

            else
                ( "is-event-badge is-w", T.markAsRead )
    in
    div (class "media mediaBox is-hoverable" :: attrs)
        [ div [ class "media-left is-hidden-mobile" ] [ viewOrga True session.file_server_url (Dict.get (nid2rootid node.nameid) avatars) node.nameid ]
        , div [ class "media-content" ] [ content ]
        , case extra of
            [] ->
                text ""

            _ ->
                div [ class "media-right is-inline-flex" ] extra
        , if unreadIds /= [] then
            div
                [ class "media-right"
                , title tooltip_txt
                , if isContract then
                    onClickSP NoMsg

                  else
                    onClickSP (MarkAsRead unreadIds Nothing)
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


viewAssigned : CommonMsg Msg -> SessionCommon -> Dict String (List Tension) -> Html Msg
viewAssigned commonOp session tensions_d =
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
                        |> List.map (\t -> mediaTension OverviewBaseUri commonOp session (nid2rootid t.receiver.nameid) t True True)
                        |> div [ id "tensionsTab", class "box is-shrinked mb-5" ]
                    ]
            )
        |> div []
