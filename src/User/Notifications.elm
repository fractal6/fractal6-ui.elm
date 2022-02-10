module User.Notifications exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr, refreshAuthModal)
import Browser.Navigation as Nav
import Codecs exposing (QuickDoc)
import Components.HelperBar as HelperBar
import Components.Loading as Loading
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
import Dict exposing (Dict)
import Extra exposing (ternary)
import Extra.Events exposing (onClickPD2)
import Form exposing (isPostSendable)
import Form.Help as Help
import Fractal.Enum.ContractType as ContractType
import Fractal.Enum.TensionEvent as TensionEvent
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, small, span, strong, sup, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, type_)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), nid2rootid)
import ModelCommon.Event exposing (contractEventToText, contractToLink, contractTypeToText, eventToIcon, eventToLink, viewContractMedia, viewEventMedia)
import ModelCommon.Requests exposing (login)
import ModelCommon.View exposing (byAt, viewOrga)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.PatchUser exposing (markAllAsRead, markAsRead)
import Query.QueryNotifications exposing (queryNotifications)
import RemoteData exposing (RemoteData)
import Session exposing (GlobalCmd(..))
import String.Extra as SE
import Task
import Text as T exposing (textH, textT)
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

                    DoAuth uctx ->
                        ( send (DoOpenAuthModal uctx), Cmd.none )

                    DoUpdateToken ->
                        ( Cmd.none, send UpdateUserToken )

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
    , modalAuth : ModalAuth
    , help : Help.State
    , refresh_trial : Int
    , now : Time.Posix
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
      -- Token refresh
    | DoOpenAuthModal UserCtx
    | DoCloseAuthModal String
    | ChangeAuthPost String String
    | SubmitUser UserAuthForm
    | GotSignin (WebData UserCtx)
    | SubmitKeyDown Int -- Detect Enter (for form sending)
      -- Common
    | NoMsg
    | PassedSlowLoadTreshold -- timer
    | LogErr String
    | Navigate String
    | DoOpenModal
    | DoCloseModal ModalData
      -- Help
    | HelpMsg Help.Msg



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
            , modalAuth = Inactive
            , help = Help.init global.session.user
            , refresh_trial = 0
            , now = global.now
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
                    ( model, send (DoOpenAuthModal model.uctx), Cmd.none )

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
                    ( model, send (DoOpenAuthModal model.uctx), Cmd.none )

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
                    ( model, send (DoOpenAuthModal model.uctx), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep MarkAllAsRead 500, send UpdateUserToken )

                OkAuth _ ->
                    -- modified event inplace toSet isRead...
                    let
                        newData =
                            withMapData
                                (List.map (\a -> ternary (isTensionEvent a.event) { a | isRead = True } a))
                                model.notifications_data
                    in
                    ( { model | notifications_data = newData }, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        -- Token refresh
        DoOpenAuthModal uctx ->
            ( { model
                | modalAuth = Active { post = Dict.fromList [ ( "username", uctx.username ) ] } RemoteData.NotAsked
              }
            , Cmd.none
            , Ports.open_auth_modal
            )

        DoCloseAuthModal link ->
            let
                cmd =
                    ternary (link /= "") (send (Navigate link)) Cmd.none
            in
            ( { model | modalAuth = Inactive }, cmd, Ports.close_auth_modal )

        ChangeAuthPost field value ->
            case model.modalAuth of
                Active form r ->
                    let
                        newForm =
                            { form | post = Dict.insert field value form.post }
                    in
                    ( { model | modalAuth = Active newForm r }, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        SubmitUser form ->
            ( model, login apis form.post GotSignin, Cmd.none )

        GotSignin result ->
            case result of
                RemoteData.Success uctx ->
                    ( { model | modalAuth = Inactive }
                    , Cmd.batch [ send (DoCloseAuthModal "") ]
                    , send (UpdateUserSession uctx)
                    )

                _ ->
                    case model.modalAuth of
                        Active form _ ->
                            ( { model | modalAuth = Active form result }, Cmd.none, Cmd.none )

                        Inactive ->
                            ( model, Cmd.none, Cmd.none )

        SubmitKeyDown key ->
            case key of
                13 ->
                    let
                        form =
                            case model.modalAuth of
                                Active f _ ->
                                    f

                                Inactive ->
                                    UserAuthForm Dict.empty
                    in
                    --ENTER
                    if isPostSendable [ "password" ] form.post then
                        ( model, send (SubmitUser form), Cmd.none )

                    else
                        ( model, Cmd.none, Cmd.none )

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

        -- Help
        HelpMsg msg ->
            let
                ( help, out ) =
                    Help.update apis msg model.help

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | help = help }, out.cmds |> List.map (\m -> Cmd.map HelpMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ _ =
    [ Ports.mcPD Ports.closeModalFromJs LogErr DoCloseModal
    ]
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        |> Sub.batch



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Notifications"
    , body =
        [ view_ global model
        , Help.view {} model.help |> Html.map HelpMsg
        , case model.modalAuth of
            -- @debug: should not be necessary...
            Active _ _ ->
                refreshAuthModal model.modalAuth { closeModal = DoCloseAuthModal, changePost = ChangeAuthPost, submit = SubmitUser, submitEnter = SubmitKeyDown }

            Inactive ->
                text ""
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ id "notifications", class "section columns" ]
        [ div [ class "column is-6 is-offset-3 " ]
            [ h2 [ class "title" ] [ text "Notifications" ]
            , case model.notifications_data of
                Success notifications ->
                    if List.length notifications == 0 then
                        text "No notifications yet."

                    else
                        viewNotifications notifications model

                NotAsked ->
                    text ""

                Loading ->
                    text ""

                LoadingSlowly ->
                    div [ class "spinner" ] []

                Failure err ->
                    viewGqlErrors err
            ]
        , div [ class "column is-2 has-text-centered" ] [ div [ class "button", onClick MarkAllAsRead ] [ text "Mark all as read" ] ]
        ]


viewNotifications : UserEvents -> Model -> Html Msg
viewNotifications notifications model =
    notifications
        |> List.map
            (\ue -> Lazy.lazy2 viewUserEvent model.now ue)
        |> div []


viewUserEvent : Time.Posix -> UserEvent -> Html Msg
viewUserEvent now ue =
    let
        firstEvent =
            List.head ue.event

        ( isContract, ev, node ) =
            case firstEvent of
                Just (TensionEvent e) ->
                    let
                        link =
                            eventToLink ue e
                    in
                    ( False
                    , Dict.fromList
                        [ ( "id", ue.id )
                        , ( "title", e.event_type |> TensionEvent.toString |> SE.humanize )
                        , ( "title_", e.tension.title )
                        , ( "target", e.tension.receiver.name )
                        , ( "orga", nid2rootid e.tension.receiver.nameid )
                        , ( "date", e.createdAt )
                        , ( "author", e.createdBy.username )
                        , ( "link", link )
                        , ( "icon", eventToIcon e.event_type )
                        ]
                    , e.tension.receiver
                    )

                Just (ContractEvent c) ->
                    let
                        link =
                            contractToLink ue c
                    in
                    ( True
                    , Dict.fromList
                        [ ( "id", ue.id )
                        , ( "contract", c.contract_type |> contractTypeToText )
                        , ( "title", c.event.event_type |> contractEventToText )
                        , ( "target", c.tension.receiver.name )
                        , ( "orga", nid2rootid c.tension.receiver.nameid )
                        , ( "date", c.createdAt )
                        , ( "author", c.createdBy.username )
                        , ( "link", link )
                        , ( "icon", eventToIcon c.event.event_type )
                        ]
                    , c.tension.receiver
                    )

                Nothing ->
                    ( False, Dict.empty, PNode "" "" )
    in
    if isContract then
        div [ class "media mt-1" ]
            [ div [ class "media-left" ] [ p [ class "image is-64x64" ] [ viewOrga False node.nameid ] ]
            , div [ class "media-content" ]
                [ viewContractMedia now ev

                --, nav [class "level is-mobile"]
                ]
            , if not ue.isRead then
                div
                    [ class "media-right tooltip"
                    , attribute "data-tooltip" "A vote is waited from you."
                    ]
                    [ div [ class "Circle has-text-info" ] [] ]

              else
                text ""
            ]

    else if not isContract && not (Dict.isEmpty ev) then
        div [ class "media mt-1" ]
            [ div [ class "media-left" ] [ p [ class "image is-64x64" ] [ viewOrga True node.nameid ] ]
            , div [ class "media-content" ]
                [ viewEventMedia now False ev

                --, nav [class "level is-mobile"]
                ]
            , if not ue.isRead then
                div
                    [ class "media-right tooltip"
                    , attribute "data-tooltip" "Mark as read."
                    , onClick (MarkAsRead ue.id)
                    ]
                    [ div [ class "Circle has-text-link is-w" ] [] ]

              else
                text ""
            ]

    else
        text ""


isTensionEvent event =
    let
        ev =
            List.head event
    in
    case ev of
        Just (TensionEvent _) ->
            True

        Just (ContractEvent _) ->
            False

        Nothing ->
            False
