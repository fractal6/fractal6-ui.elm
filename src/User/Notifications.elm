module User.Notifications exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Browser.Navigation as Nav
import Codecs exposing (QuickDoc)
import Components.AuthModal as AuthModal
import Dict exposing (Dict)
import Extra exposing (ternary, textH, upH)
import Extra.Events exposing (onClickPD2)
import Form exposing (isPostSendable)
import Form.Help as Help
import Fractal.Enum.ContractType as ContractType
import Fractal.Enum.Lang as Lang
import Fractal.Enum.TensionEvent as TensionEvent
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, small, span, strong, sup, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, title, type_)
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
import ModelCommon.Codecs exposing (FractalBaseRoute(..), nid2rootid)
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
import ModelCommon.View exposing (byAt, viewOrga)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.PatchUser exposing (markAllAsRead, markAsRead)
import Query.QueryNotifications exposing (queryNotifications)
import RemoteData exposing (RemoteData)
import Session exposing (GlobalCmd(..))
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
    , eid : String

    -- Common
    , help : Help.State
    , refresh_trial : Int
    , now : Time.Posix
    , empty : {}
    , authModal : AuthModal.State
    }



---- MSG ----


type Msg
    = Submit (Time.Posix -> Msg) -- Get Current Time
    | LoadNotifications
    | GotNotifications (GqlData UserEvents)
    | MarkAsRead String
    | GotMarkAsRead (GqlData IdPayload)
    | MarkAllAsRead
    | GotMarkAllAsRead (GqlData IdPayload)
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
                    , [ send LoadNotifications, sendSleep PassedSlowLoadTreshold 500 ]
                    , [ send (UpdateSessionFocus Nothing) ]
                    )

                LoggedOut ->
                    ( initUserctx, [], [ Global.navigate <| Route.Login ] )

        model =
            { uctx = uctx
            , notifications_data = Loading
            , eid = ""

            -- common
            , help = Help.init global.session.user
            , refresh_trial = 0
            , now = global.now
            , empty = {}
            , authModal = AuthModal.init global.session.user Nothing
            }
    in
    ( model
    , Cmd.batch cmds
    , Cmd.batch gcmds
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
            ( { model | notifications_data = withMaybeSlowly model.notifications_data }, Cmd.none, Cmd.none )

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        LoadNotifications ->
            ( model, queryNotifications apis { first = 50, uctx = model.uctx } GotNotifications, Cmd.none )

        GotNotifications result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( model, Ports.raiseAuthModal model.uctx, Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep LoadNotifications 500, send UpdateUserToken )

                OkAuth th ->
                    ( { model | notifications_data = result }
                    , Cmd.none
                    , Cmd.none
                    )

                _ ->
                    ( { model | notifications_data = result }, Cmd.none, Cmd.none )

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
                    ( { model | notifications_data = newData }, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

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
                                (List.map (\a -> ternary (editableEvent a.event) { a | isRead = True } a))
                                model.notifications_data
                    in
                    ( { model | notifications_data = newData }, Cmd.none, Cmd.none )

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
            ( model, Cmd.none, send <| NavigateRaw <| withDefault "" <| Maybe.map .path <| global.session.referer )

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
    div [ id "notifications", class "top-section columns reverse-columns" ]
        [ div [ class "column is-2 is-3-fullhd" ] []
        , div [ class "column is-8 is-6-fullhd" ]
            [ div [ class "is-strong arrow-left is-w is-h bc is-pulled-left", title T.goBack, onClick GoBack ] []
            , br [] []
            , h2 [ class "title" ] [ text T.notifications ]
            , case model.notifications_data of
                Success notifications ->
                    if List.length notifications == 0 then
                        text T.noNotificationsYet

                    else
                        viewNotifications global.session.lang notifications model

                NotAsked ->
                    text ""

                Loading ->
                    text ""

                LoadingSlowly ->
                    div [ class "spinner" ] []

                Failure err ->
                    viewGqlErrors err
            ]
        , div [ class "column is-2 has-text-centered" ] [ div [ class "button is-small", onClick MarkAllAsRead ] [ text T.markAllAsRead ] ]
        ]


viewNotifications : Lang.Lang -> UserEvents -> Model -> Html Msg
viewNotifications lang notifications model =
    notifications
        |> List.map
            (\ue -> Lazy.lazy3 viewUserEvent lang model.now ue)
        |> div [ class "box is-shrinked" ]


viewUserEvent : Lang.Lang -> Time.Posix -> UserEvent -> Html Msg
viewUserEvent lang now ue =
    let
        firstEvent =
            List.head ue.event
    in
    case firstEvent of
        Just (TensionEvent e_) ->
            let
                -- LabelAdded is the first entry of the list
                -- when a label is added at the creation of a tension.
                -- I guess is because the timestamp are equal which messed up the orders (@dgraph) ???
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
            viewNotif ue node (viewEventMedia lang now False ev)

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
            div [ class "media mediaBox is-hoverable" ]
                [ div [ class "media-left" ] [ p [ class "image is-64x64" ] [ viewOrga True node.nameid ] ]
                , div [ class "media-content" ] [ viewContractMedia lang now ev ]
                , if not ue.isRead then
                    div
                        [ class "media-right tooltip"
                        , attribute "data-tooltip" T.voteWaited
                        ]
                        [ div [ class "Circle has-text-info" ] [] ]

                  else
                    text ""
                ]

        Just (NotifEvent n) ->
            case n.tension of
                Just tension ->
                    let
                        node =
                            tension.receiver

                        ev_ =
                            Dict.fromList
                                [ ( "id", ue.id )
                                , ( "title", withDefault "no input message." n.message )

                                --, ( "title_", tension.title )
                                , ( "target", node.name )
                                , ( "orga", nid2rootid node.nameid )
                                , ( "date", n.createdAt )
                                , ( "author", n.createdBy.username )
                                , ( "icon", "icon-info" )
                                ]
                    in
                    case n.contract of
                        Just contract ->
                            -- Contract notification (e.g contract cancelled)
                            let
                                link =
                                    (Route.Tension_Dynamic_Dynamic_Contract_Dynamic { param1 = nid2rootid tension.receiver.nameid, param2 = tension.id, param3 = contract.id } |> toHref)
                                        ++ "?eid="
                                        ++ ue.id

                                ev =
                                    Dict.insert "link" link ev_
                            in
                            viewNotif ue node (viewNotifMedia lang now ev)

                        Nothing ->
                            let
                                d =
                                    Debug.log "link" n.link

                                link =
                                    case n.link of
                                        Just l ->
                                            l

                                        Nothing ->
                                            (Route.Tension_Dynamic_Dynamic { param1 = nid2rootid tension.receiver.nameid, param2 = tension.id } |> toHref)
                                                ++ "?eid="
                                                ++ ue.id

                                ev =
                                    Dict.insert "link" link ev_
                            in
                            viewNotif ue node (viewNotifMedia lang now ev)

                Nothing ->
                    -- Something has beed removed, contract ?
                    text ""

        Nothing ->
            text ""


viewNotif : UserEvent -> PNode -> Html Msg -> Html Msg
viewNotif ue node content =
    div [ class "media mediaBox is-hoverable" ]
        [ div [ class "media-left" ] [ p [ class "image is-64x64" ] [ viewOrga True node.nameid ] ]
        , div [ class "media-content" ] [ content ]
        , if not ue.isRead then
            div
                [ class "media-right tooltip"
                , attribute "data-tooltip" T.markAsRead
                , onClick (MarkAsRead ue.id)
                ]
                [ div [ class "Circle has-text-link is-w" ] [] ]

          else
            text ""
        ]


editableEvent event =
    case List.head event of
        Just (ContractEvent _) ->
            False

        _ ->
            True
