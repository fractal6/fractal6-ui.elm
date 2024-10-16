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


module Pages.Explore exposing (Flags, Model, Msg, page)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Browser.Navigation as Nav
import Bulk.Codecs exposing (FractalBaseRoute(..), toLink)
import Bulk.Error exposing (viewAuthNeeded, viewGqlErrors, viewHttpErrors)
import Bulk.View exposing (mediaOrga)
import Codecs exposing (QuickDoc)
import Components.AuthModal as AuthModal
import Dict exposing (Dict)
import Extra exposing (ternary, textH, upH)
import Form exposing (isPostSendable)
import Form.Help as Help
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, type_)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import Loading exposing (GqlData, ModalData, RequestResult(..), RestData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.QueryNode exposing (queryPublicOrga)
import RemoteData exposing (RemoteData)
import Session exposing (CommonMsg, GlobalCmd(..))
import Task
import Text as T
import Time
import Url



---- PROGRAM ----


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Flags =
    ()


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
    { orgas : GqlData (List NodeExt)

    -- Common
    , help : Help.State
    , refresh_trial : Int
    , empty : {}
    , authModal : AuthModal.State
    , commonOp : CommonMsg Msg
    }



-- INIT --


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        apis =
            global.session.apis

        model =
            { orgas = Loading

            -- common
            , help = Help.init global.session
            , refresh_trial = 0
            , empty = {}
            , authModal = AuthModal.init global.session.user Nothing
            , commonOp = CommonMsg NoMsg LogErr
            }

        cmds =
            [ send LoadNodeExts
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
    | LoadNodeExts
    | GotOrgas (GqlData (List NodeExt))
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
        LoadNodeExts ->
            ( model, queryPublicOrga apis GotOrgas, Cmd.none )

        PassedSlowLoadTreshold ->
            let
                orgas =
                    ternary (model.orgas == Loading) LoadingSlowly model.orgas
            in
            ( { model | orgas = orgas }, Cmd.none, Cmd.none )

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        -- Gql queries
        GotOrgas result ->
            ( { model | orgas = result }, Cmd.none, Cmd.none )

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
            ( model, Cmd.none, Cmd.batch [ Ports.close_modal, gcmd ] )

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
    { title = "Explore"
    , body =
        [ view_ global model
        , Lazy.lazy2 Help.view model.empty model.help |> Html.map HelpMsg
        , Lazy.lazy2 AuthModal.view model.empty model.authModal |> Html.map AuthModalMsg
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ id "explore", class "top-section" ]
        [ div [ class "columns is-centered" ]
            [ div [ class "column is-7" ]
                [ viewPublicOrgas model ]
            ]
        ]


viewPublicOrgas : Model -> Html Msg
viewPublicOrgas model =
    div []
        [ h1 [ class "subtitle" ] [ textH T.exploreOrganisations ]

        --, div  br [] []
        , div [ class "nodesList" ] <|
            case model.orgas of
                Loading ->
                    [ text "" ]

                NotAsked ->
                    [ text "" ]

                LoadingSlowly ->
                    [ div [ class "spinner" ] [] ]

                Failure err ->
                    [ viewGqlErrors err ]

                Success nodes ->
                    List.map (\n -> Lazy.lazy3 mediaOrga model.commonOp Nothing n) nodes
        ]
