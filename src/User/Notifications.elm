module User.Notifications exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

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
        , withMaybeData
        , withMaybeSlowly
        )
import Dict exposing (Dict)
import Extra exposing (ternary)
import Form exposing (isPostSendable)
import Form.Help as Help
import Fractal.Enum.ContractType as ContractType
import Fractal.Enum.TensionEvent as TensionEvent
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, small, span, strong, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, type_)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Icon as I
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), nid2rootid)
import ModelCommon.Requests exposing (login)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.QueryNotifications exposing (queryNotifications)
import RemoteData exposing (RemoteData)
import Session exposing (GlobalCmd(..))
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

    -- Common
    , modalAuth : ModalAuth
    , help : Help.State
    , refresh_trial : Int
    }



---- MSG ----


type Msg
    = Submit (Time.Posix -> Msg) -- Get Current Time
    | LoadNotifications
    | GotNotifications (GqlData UserEvents)
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

            -- common
            , modalAuth = Inactive
            , help = Help.init global.session.user
            , refresh_trial = 0
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

        -- Token refresh
        DoOpenAuthModal uctx ->
            ( { model
                | modalAuth =
                    Active
                        { post = Dict.fromList [ ( "username", uctx.username ) ]
                        , result = RemoteData.NotAsked
                        }
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
                Active form ->
                    let
                        newForm =
                            { form | post = Dict.insert field value form.post }
                    in
                    ( { model | modalAuth = Active newForm }, Cmd.none, Cmd.none )

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
                        Active form ->
                            ( { model | modalAuth = Active { form | result = result } }, Cmd.none, Cmd.none )

                        Inactive ->
                            ( model, Cmd.none, Cmd.none )

        SubmitKeyDown key ->
            case key of
                13 ->
                    let
                        form =
                            case model.modalAuth of
                                Active f ->
                                    f

                                Inactive ->
                                    UserAuthForm Dict.empty RemoteData.NotAsked
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
            Active _ ->
                refreshAuthModal model.modalAuth { closeModal = DoCloseAuthModal, changePost = ChangeAuthPost, submit = SubmitUser, submitEnter = SubmitKeyDown }

            Inactive ->
                text ""
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ id "notifications", class "section columns is-centered" ]
        [ div [ class "column is-6" ]
            [ h2 [ class "title" ] [ text "Notifications" ]
            , case model.notifications_data of
                Success notifications ->
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
        ]


viewNotifications : UserEvents -> Model -> Html Msg
viewNotifications notifications model =
    notifications
        |> List.map
            (\e ->
                div [ class "media" ]
                    [ div [ class "media-left" ] [ I.icon "icon-edit" ]
                    , div [ class "media-content" ]
                        [ viewUserEvent e model

                        --, nav [class "level is-mobile"]
                        ]

                    --, div [class "media-right"] [ button [class "delete"][]]
                    ]
            )
        |> div []


viewUserEvent : UserEvent -> Model -> Html Msg
viewUserEvent ue model =
    let
        firstEvent =
            List.head ue.event

        ( title, address ) =
            case firstEvent of
                Just (TensionEvent e) ->
                    ( e.event_type |> TensionEvent.toString
                    , e.tension.receiver.name ++ "@" ++ nid2rootid e.tension.receiver.nameid
                    )

                Just (ContractEvent c) ->
                    ( c.contract_type |> ContractType.toString
                    , c.tension.receiver.name ++ "@" ++ nid2rootid c.tension.receiver.nameid
                    )

                Nothing ->
                    ( "never", "never" )
    in
    div [ class "content" ]
        [ p []
            [ strong [] [ text title ]
            , small [] [ text address ]
            , br [] []
            , text ""
            ]
        ]
