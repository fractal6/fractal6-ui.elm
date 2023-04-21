{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2023 Fractale Co

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


module User.Profile exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Auth exposing (ErrState(..), parseErr)
import Browser.Navigation as Nav
import Bulk exposing (..)
import Bulk.Codecs exposing (FractalBaseRoute(..), getRoles, getRootids)
import Bulk.Error exposing (viewGqlErrors)
import Bulk.View exposing (viewOrgaMedia, viewProfileC)
import Components.AuthModal as AuthModal
import Extra exposing (ternary)
import Form.Help as Help
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, div, h1, i, p, text)
import Html.Attributes exposing (class, id)
import Html.Lazy as Lazy
import Loading exposing (GqlData, ModalData, RequestResult(..), withMaybeData)
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.QueryNode exposing (queryNodeExt)
import Query.QueryUser exposing (queryUserProfile)
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
    { username : String
    , user : GqlData UserProfile
    , orgas : GqlData (List NodeExt)

    -- Common
    , help : Help.State
    , refresh_trial : Int
    , authModal : AuthModal.State
    , empty : {}
    }



-- INIT --


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        apis =
            global.session.apis

        conf =
            { screen = global.session.screen, now = global.now, lang = global.session.lang, url = global.url }

        username =
            flags.param1 |> Url.percentDecode |> withDefault ""

        model =
            { username = username
            , user = Loading
            , orgas = Loading

            -- common
            , refresh_trial = 0
            , help = Help.init global.session.user conf
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
    | DoOpenModal
    | DoCloseModal ModalData
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
                [ div [ class "columns is-centered m-0 pl-2" ]
                    [ viewProfileC user ]
                ]
            , div [ class "column is-7" ]
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
                                        , div [ class "ph-line is-1 mt-2" ] []
                                        ]
                                    ]
                                ]
                            ]
                        , div [ class "media box" ]
                            [ div [ class "media-content" ]
                                [ div [ class "columns" ]
                                    [ div [ class "column is-8" ]
                                        [ div [ class "ph-line is-0" ] []
                                        , div [ class "ph-line is-1 mt-2" ] []
                                        ]
                                    ]
                                ]
                            ]
                        , div [ class "media box" ]
                            [ div [ class "media-content" ]
                                [ div [ class "columns" ]
                                    [ div [ class "column is-8" ]
                                        [ div [ class "ph-line is-0" ] []
                                        , div [ class "ph-line is-1 mt-2" ] []
                                        ]
                                    ]
                                ]
                            ]
                        ]
        ]


viewUserOrgas : UserCommon a -> List NodeExt -> Html Msg
viewUserOrgas user orgas =
    orgas
        |> List.map (\root -> Lazy.lazy2 viewOrgaMedia (Just user) root)
        |> div [ class "nodesList" ]
