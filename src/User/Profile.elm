module User.Profile exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Browser.Navigation as Nav
import Codecs exposing (QuickDoc)
import Components.AuthModal as AuthModal
import Dict exposing (Dict)
import Extra exposing (ternary, textH, upH)
import Form exposing (isPostSendable)
import Form.Help as Help
import Fractal.Enum.RoleType as RoleType
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, h3, h4, h5, h6, hr, i, input, li, nav, p, span, strong, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, type_)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), WebData, viewAuthNeeded, viewGqlErrors, viewHttpErrors, withMaybeData)
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), NodeFocus, getRoles, getRootids, nid2rootid, uriFromNameid)
import ModelCommon.View exposing (viewOrgaMedia, viewProfileC)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.QueryNode exposing (queryNodeExt)
import Query.QueryUser exposing (queryUserProfile)
import RemoteData exposing (RemoteData)
import Session exposing (GlobalCmd(..))
import Task
import Text as T
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
    , user : GqlData UserProfile
    , orgas : GqlData (List NodeExt)

    -- Common
    , help : Help.State
    , refresh_trial : Int
    , authModal : AuthModal.State
    , empty : {}
    }



---- MSG ----


type Msg
    = PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
    | LoadNodes (List UserRole)
    | GotNodes (GqlData (List NodeExt))
    | GotProfile (GqlData UserProfile)
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

        model =
            { username = username
            , user = Loading
            , orgas = Loading

            -- common
            , refresh_trial = 0
            , help = Help.init global.session.user global.session.screen
            , authModal = AuthModal.init global.session.user Nothing
            , empty = {}
            }

        cmds =
            [ queryUserProfile apis username GotProfile
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
        LoadNodes roles ->
            case getRootids roles of
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
                    ( model, Ports.raiseAuthModal (uctxFromUser global.session.user), Cmd.none )

                RefreshToken i ->
                    let
                        roles =
                            withMaybeData model.user |> Maybe.map .roles |> withDefault []
                    in
                    ( { model | refresh_trial = i }, sendSleep (LoadNodes roles) 500, send UpdateUserToken )

                OkAuth _ ->
                    ( { model | orgas = result }, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        GotProfile result ->
            case result of
                Success user ->
                    ( { model | user = result }, send (LoadNodes user.roles), Cmd.none )

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
    { title = model.username
    , body =
        [ case model.user of
            Success user ->
                view_ global.session.user user model

            NotAsked ->
                text ""

            Loading ->
                text ""

            LoadingSlowly ->
                div [ class "spinner" ] []

            Failure err ->
                viewGqlErrors err
        , Lazy.lazy2 Help.view model.empty model.help |> Html.map HelpMsg
        , Lazy.lazy2 AuthModal.view model.empty model.authModal |> Html.map AuthModalMsg
        ]
    }


view_ : UserState -> UserProfile -> Model -> Html Msg
view_ user_s user model =
    div [ id "profile", class "top-section" ]
        [ div [ class "columns" ]
            [ div [ class "column is-3" ]
                [ div [ class "columns is-centered m-0" ]
                    [ viewProfileC user ]
                ]
            , div [ class "column is-7 " ]
                [ viewProfileRight user_s user model ]
            ]
        ]


viewProfileRight : UserState -> UserProfile -> Model -> Html Msg
viewProfileRight user_s user model =
    div []
        [ h1 [ class "subtitle" ] [ text T.organisations ]
        , if List.length (getRoles user) == 0 then
            p [ class "section content" ]
                [ if (uctxFromUser user_s).username == model.username then
                    --List.intersperse (text " ")
                    --    [ p [] [ text "Welcome," ]
                    --    , p [] <|
                    --        List.intersperse (text " ") <|
                    --            [ text "You can"
                    --            , a [ href (toHref Route.Explore) ] [ text "Explore" ]
                    --            , text "public organisations"
                    --            , text ", or create your"
                    --            , a [ href (toHref Route.New_Orga) ] [ text "first organisation." ]
                    --            ]
                    --    ]
                    renderMarkdown "is-human is-size-bg" T.welcomeNoOrga

                  else
                    p [] [ text T.nothingToShow ]
                ]

          else
            case model.orgas of
                Success orgas ->
                    viewUserOrgas user orgas

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


viewUserOrgas : UserCommon a -> List NodeExt -> Html Msg
viewUserOrgas user orgas =
    orgas
        |> List.map
            (\root -> viewOrgaMedia (Just user) root)
        |> div [ class "nodesList" ]
