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


module Pages.Verification exposing (Flags, Model, Msg, page)

import Assets as A exposing (almostThere)
import Browser.Navigation as Nav
import Components.AuthModal exposing (UserAuthForm)
import Dict exposing (Dict)
import Extra exposing (ternary, textH, upH)
import Extra.Events exposing (onKeydown)
import Extra.Url exposing (queryParser)
import Form exposing (isSignupSendable)
import Form.Help as Help
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, autofocus, class, classList, disabled, href, id, name, placeholder, required, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Loading exposing (WebData)
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Error exposing (viewHttpErrors)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Process
import RemoteData exposing (RemoteData)
import Requests exposing (signupValidate)
import Session exposing (GlobalCmd(..))
import String.Format as Format
import Task
import Text as T


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

                    _ ->
                        ( Cmd.none, Cmd.none )
            )
        |> List.unzip



--
-- Model
--


type alias Model =
    { form : UserAuthForm
    , result : WebData UserCtx
    , email_token : Maybe String
    , email : Maybe String

    -- common
    , help : Help.State
    }



--
-- Init
--


type alias Flags =
    ()


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        conf =
            { screen = global.session.screen, now = global.now, lang = global.session.lang, url = global.url }

        -- Query parameters
        query =
            queryParser global.url

        model =
            { form = { post = Dict.empty }
            , result = RemoteData.NotAsked
            , email_token = Dict.get "email_token" query |> Maybe.map List.head |> withDefault Nothing
            , email = Dict.get "email" query |> Maybe.map List.head |> withDefault Nothing
            , help = Help.init global.session.user conf
            }
    in
    ( model
    , case model.email_token of
        Just token ->
            send (SubmitVerification token)

        _ ->
            Cmd.none
    , Cmd.batch [ send (UpdateSessionFocus Nothing) ]
    )



--
-- Update
--


type Msg
    = SubmitVerification String
    | GotVerification (WebData UserCtx)
    | HelpMsg Help.Msg


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global message model =
    let
        apis =
            global.session.apis
    in
    case message of
        SubmitVerification token ->
            case global.session.user of
                LoggedOut ->
                    ( { model | result = RemoteData.Loading }
                    , signupValidate apis (Dict.fromList [ ( "email_token", token ) ]) GotVerification
                    , Cmd.none
                    )

                LoggedIn _ ->
                    ( model, Cmd.none, Cmd.none )

        GotVerification result ->
            ( { model | result = result }
            , Cmd.none
            , case result of
                RemoteData.Success uctx ->
                    Cmd.batch [ send (UpdateUserSession uctx), sendSleep (Navigate Route.Verification) 333 ]

                _ ->
                    Cmd.none
            )

        HelpMsg msg ->
            let
                ( data, out ) =
                    Help.update apis msg model.help

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | help = data }, out.cmds |> List.map (\m -> Cmd.map HelpMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        |> Sub.batch


view : Global.Model -> Model -> Document Msg
view global model =
    { title = T.signup
    , body =
        [ view_ global model
        , Help.view {} model.help |> Html.map HelpMsg
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ class "columns is-centered top-section" ]
        [ div [ class "column is-7 is-5-fullhd" ]
            [ viewVerification global model ]
        ]


viewVerification : Global.Model -> Model -> Html Msg
viewVerification global model =
    div []
        [ case model.result of
            RemoteData.Success uctx ->
                welcome uctx

            RemoteData.Loading ->
                div [ class "spinner" ] []

            RemoteData.Failure err ->
                viewHttpErrors err

            RemoteData.NotAsked ->
                case global.session.user of
                    LoggedOut ->
                        almostThere (withDefault "" model.email) T.toConfirmYourAccount (toHref Route.Signup)

                    LoggedIn uctx ->
                        welcome uctx
        ]


welcome : UserCtx -> Html Msg
welcome uctx =
    div []
        [ div [ class "mt-2 mb-6 is-size-bg" ]
            [ T.welcomeLetter
                |> Format.namedValue "username" uctx.username
                |> renderMarkdown "is-human"
            ]
        , div [ class "is-aligned-center" ]
            [ a [ class "button is-success is-light breakable", href (toHref Route.New_Orga) ] [ text T.gotItCreateOrga ]
            , br [] []
            , text T.or_
            , br [] []
            , a [ class "button is-success is-light breakable", href (toHref Route.Explore) ] [ text T.explorePublicOrga ]
            ]
        ]
