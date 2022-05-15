module User.Profile exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Browser.Navigation as Nav
import Codecs exposing (QuickDoc)
import Components.AuthModal as AuthModal
import Components.HelperBar as HelperBar
import Components.Loading as Loading exposing (GqlData, ModalData, RequestResult(..), WebData, viewAuthNeeded, viewGqlErrors, viewHttpErrors, withMaybeData)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Form exposing (isPostSendable)
import Form.Help as Help
import Fractal.Enum.RoleType as RoleType
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, h3, h4, h5, h6, hr, i, input, li, nav, p, span, strong, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, type_)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), NodeFocus, getRoles, getRootids, nid2rootid, uriFromNameid)
import ModelCommon.Requests exposing (getQuickDoc, login)
import ModelCommon.View exposing (getAvatar3, roleColor, viewOrgaMedia)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.QueryNode exposing (queryNodeExt)
import Query.QueryUser exposing (queryUctx)
import RemoteData exposing (RemoteData)
import Session exposing (GlobalCmd(..))
import Task
import Text as T exposing (textH, textT)
import Time
import Url exposing (Url)



---- PROGRAM ----


type alias Flags =
    { param1 : String }


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
    { username : String
    , user : GqlData UserCtx
    , uctx_my : Maybe UserCtx
    , orgas : GqlData (List NodeExt)

    -- Common
    , help : Help.State
    , refresh_trial : Int
    , authModal : AuthModal.State
    }



---- MSG ----


type Msg
    = PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
    | LoadNodes UserCtx
    | GotNodes (GqlData (List NodeExt))
    | GotUctx (GqlData UserCtx)
      -- Common
    | NoMsg
    | LogErr String
    | Navigate String
    | DoOpenModal
    | DoCloseModal ModalData
      -- Help
    | HelpMsg Help.Msg
    | AuthModalMsg AuthModal.Msg



-- INIT --


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        apis =
            global.session.apis

        username =
            flags.param1 |> Url.percentDecode |> withDefault ""

        uctx_my =
            case global.session.user of
                LoggedIn uctx ->
                    Just uctx

                LoggedOut ->
                    Nothing

        uctx_data =
            case uctx_my of
                Just uctx ->
                    if uctx.username == username then
                        Success uctx

                    else
                        Loading

                Nothing ->
                    Loading

        model =
            { username = username
            , user = uctx_data
            , uctx_my = uctx_my
            , orgas = Loading

            -- common
            , refresh_trial = 0
            , help = Help.init global.session.user
            , authModal = AuthModal.init global.session.user Nothing
            }

        cmds =
            [ case uctx_data of
                Success uctx ->
                    send (LoadNodes uctx)

                _ ->
                    queryUctx apis username GotUctx
            , sendSleep PassedSlowLoadTreshold 500
            ]
    in
    ( model
    , Cmd.batch cmds
    , send (UpdateSessionFocus Nothing)
    )



---- UPDATE ----


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global message model =
    let
        apis =
            global.session.apis
    in
    case message of
        LoadNodes uctx ->
            case getRootids uctx.roles of
                [] ->
                    ( model, Cmd.none, Cmd.none )

                rootids ->
                    ( model, queryNodeExt apis rootids GotNodes, Cmd.none )

        PassedSlowLoadTreshold ->
            let
                user =
                    ternary (model.user == Loading) LoadingSlowly model.user
            in
            ( { model | user = user }, Cmd.none, Cmd.none )

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        GotNodes result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( model, Ports.raiseAuthModal (withDefault initUserctx model.uctx_my), Cmd.none )

                RefreshToken i ->
                    let
                        uctx =
                            withMaybeData model.user |> withDefault initUserctx
                    in
                    ( { model | refresh_trial = i }, sendSleep (LoadNodes uctx) 500, send UpdateUserToken )

                OkAuth _ ->
                    ( { model | orgas = result }, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        GotUctx result ->
            case result of
                Success uctx ->
                    ( { model | user = result }, send (LoadNodes uctx), Cmd.none )

                _ ->
                    ( { model | user = result }, Cmd.none, Cmd.none )

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

        AuthModalMsg msg ->
            let
                ( data, out ) =
                    AuthModal.update apis msg model.authModal

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | authModal = data }, out.cmds |> List.map (\m -> Cmd.map AuthModalMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )


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
    { title = model.username
    , body =
        [ case model.user of
            Success user ->
                view_ global model user

            NotAsked ->
                text ""

            Loading ->
                text ""

            LoadingSlowly ->
                div [ class "spinner" ] []

            Failure err ->
                viewGqlErrors err
        , Help.view {} model.help |> Html.map HelpMsg
        , AuthModal.view {} model.authModal |> Html.map AuthModalMsg
        ]
    }


view_ : Global.Model -> Model -> UserCtx -> Html Msg
view_ _ model uctx =
    div [ id "profile", class "section" ]
        [ div [ class "columns" ]
            [ div [ class "column is-3" ]
                [ div [ class "columns is-centered" ]
                    [ viewProfileLeft model uctx ]
                ]
            , div [ class "column is-7 " ]
                [ viewProfileRight model uctx ]
            ]
        ]


viewProfileLeft : Model -> UserCtx -> Html Msg
viewProfileLeft _ uctx =
    div []
        [ div [ class "content" ] [ getAvatar3 uctx.username ]
        , div [ class "content" ]
            [ case uctx.name of
                Just name ->
                    div [ class "title is-4" ] [ text name ]

                Nothing ->
                    div [] []
            , div [ class "is-size-5" ] [ text ("@" ++ uctx.username) ]
            ]
        ]


viewProfileRight : Model -> UserCtx -> Html Msg
viewProfileRight model uctx =
    div []
        [ h1 [ class "subtitle" ] [ textH T.organisations ]
        , if List.length (getRoles uctx) == 0 then
            p [ class "section content" ] <|
                List.intersperse (text " ")
                    [ p [] [ text "Welcome," ]
                    , p [] <|
                        List.intersperse (text " ") <|
                            [ text "You can"
                            , a [ href (Route.toHref Route.Explore) ] [ text "Explore" ]
                            , text "public organisations"
                            , text ", or create your"
                            , a [ href (Route.toHref Route.New_Orga) ] [ textH "first organisation." ]
                            ]
                    ]

          else
            case model.orgas of
                Success orgas ->
                    viewUserOrgas uctx orgas

                Failure err ->
                    viewGqlErrors err

                _ ->
                    div []
                        [ div [ class "media box" ]
                            [ div [ class "media-content" ]
                                [ div [ class "columns" ]
                                    [ div [ class "column is-8" ]
                                        [ div [ class "ph-line is-0" ] []
                                        , div [ class "ph-line is-1" ] []
                                        ]
                                    ]
                                ]
                            ]
                        , div [ class "media box" ]
                            [ div [ class "media-content" ]
                                [ div [ class "columns" ]
                                    [ div [ class "column is-8" ]
                                        [ div [ class "ph-line is-0" ] []
                                        , div [ class "ph-line is-1" ] []
                                        ]
                                    ]
                                ]
                            ]
                        , div [ class "media box" ]
                            [ div [ class "media-content" ]
                                [ div [ class "columns" ]
                                    [ div [ class "column is-8" ]
                                        [ div [ class "ph-line is-0" ] []
                                        , div [ class "ph-line is-1" ] []
                                        ]
                                    ]
                                ]
                            ]
                        ]
        ]


viewUserOrgas : UserCtx -> List NodeExt -> Html Msg
viewUserOrgas uctx orgas =
    orgas
        |> List.map
            (\root -> viewOrgaMedia (LoggedIn uctx) root)
        |> div [ class "nodesList" ]
