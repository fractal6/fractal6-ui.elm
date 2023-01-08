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


module Pages.Signup exposing (Flags, Model, Msg, page)

import Assets as A
import Browser.Navigation as Nav
import Components.AuthModal exposing (UserAuthForm)
import Dict exposing (Dict)
import Extra exposing (ternary, textH, upH)
import Extra.Events exposing (onKeydown)
import Form exposing (isSignupSendable)
import Fractal.Enum.Lang as Lang
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, autofocus, class, classList, disabled, href, id, name, placeholder, required, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as JE
import Loading exposing (WebData)
import Maybe exposing (withDefault)
import Bulk exposing (..)
import Bulk.Codecs exposing (FractalBaseRoute(..), toLink)
import Bulk.Error exposing (viewHttpErrors)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Process
import RemoteData exposing (RemoteData)
import Requests exposing (signup)
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



--
-- Model
--


type alias Model =
    { form : UserAuthForm
    , result : WebData Bool
    }



--
-- Init
--


type alias Flags =
    ()


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        gcmd =
            case global.session.user of
                LoggedIn uctx ->
                    send <| NavigateRaw <| toLink UsersBaseUri uctx.username []

                LoggedOut ->
                    Cmd.none

        model =
            { form = { post = Dict.fromList [ ( "lang", Lang.toString global.session.lang ) ] }
            , result = RemoteData.NotAsked
            }
    in
    ( model
    , Cmd.none
    , Cmd.batch [ gcmd, send (UpdateSessionFocus Nothing) ]
    )



--
-- Update
--


type Msg
    = SubmitUser UserAuthForm
    | ChangeUserPost String String
    | GotSignup (WebData Bool)
    | SubmitKeyDown Int


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global msg model =
    let
        apis =
            global.session.apis
    in
    case msg of
        ChangeUserPost field value ->
            let
                form =
                    model.form

                formUpdated =
                    { form | post = Dict.insert field value form.post }
            in
            ( { model | form = formUpdated }, Cmd.none, Cmd.none )

        SubmitUser form ->
            ( { model | result = RemoteData.Loading }
            , signup apis form.post GotSignup
            , Cmd.none
            )

        GotSignup result ->
            ( { model | result = result }
            , case result of
                RemoteData.Success ok ->
                    Nav.pushUrl global.key (toHref Route.Verification ++ "?email=" ++ (Dict.get "email" model.form.post |> withDefault ""))

                _ ->
                    Cmd.none
            , Cmd.none
            )

        SubmitKeyDown key ->
            case key of
                13 ->
                    --ENTER
                    if isSignupSendable model.form.post then
                        ( model, send (SubmitUser model.form), Cmd.none )

                    else
                        ( model, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.none


view : Global.Model -> Model -> Document Msg
view global model =
    { title = T.signup_
    , body = [ view_ global model ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ id "signupForm", class "columns is-centered top-section" ]
        [ div [ class "" ]
            [ viewSignup global model ]
        ]


viewSignup : Global.Model -> Model -> Html Msg
viewSignup global model =
    div [ class "card" ]
        [ div [ class "card-header" ]
            [ div [ class "card-header-title" ]
                [ text T.signup_ ]
            ]
        , div [ class "card-content" ]
            [ A.welcome
            , div [ class "subtitle is-size-6 is-strong" ] [ text T.createYourAccount, text ":" ]
            , div [ class "field is-horizntl" ]
                [ div [ class "field-lbl" ] [ label [ class "label" ] [ text T.email ] ]
                , div [ class "field-body" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ input
                                [ class "input autofocus followFocus"
                                , attribute "data-nextfocus" "usernameInput"
                                , type_ "text"
                                , placeholder "email"
                                , name "email"
                                , value (Dict.get "email" model.form.post |> withDefault "")
                                , attribute "autocomplete" "email"
                                , required True
                                , onInput (ChangeUserPost "email")
                                ]
                                []
                            ]
                        ]
                    ]
                ]
            , div [ class "field is-horizntl" ]
                [ div [ class "field-lbl" ] [ label [ class "label" ] [ text T.username ] ]
                , div [ class "field-body" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ input
                                [ id "usernameInput"
                                , class "input followFocus"
                                , attribute "data-nextfocus" "passwordInput"
                                , type_ "text"
                                , placeholder "username"
                                , name "username"
                                , value (Dict.get "username" model.form.post |> withDefault "")
                                , attribute "autocomplete" "username"
                                , required True
                                , onInput (ChangeUserPost "username")
                                ]
                                []
                            ]
                        ]
                    ]
                ]
            , div [ class "field is-horizntl" ]
                [ div [ class "field-lbl" ] [ label [ class "label" ] [ text T.password ] ]
                , div [ class "field-body" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ input
                                [ id "passwordInput"
                                , class "input"
                                , type_ "password"
                                , placeholder "password"
                                , name "password"
                                , value (Dict.get "password" model.form.post |> withDefault "")
                                , attribute "autocomplete" "password"
                                , required True
                                , onInput (ChangeUserPost "password")
                                , onKeydown SubmitKeyDown
                                ]
                                []
                            , p [ class "help" ] [ text T.passwordRequirements ]
                            ]
                        ]
                    ]
                ]
            , br [] []
            , div [ class "is-size-7 is-pulled-left" ]
                [ span [ class "mr-2" ] [ text T.alreadyAnAccount ]
                , a [ class "underlined-link", href (toHref Route.Login) ] [ textH T.signinNow ]
                ]
            , div [ class "field is-grouped is-grouped-right" ]
                [ div [ class "control" ]
                    [ if isSignupSendable model.form.post then
                        button
                            [ id "submitButton"
                            , class "button is-success"
                            , classList [ ( "is-loading", model.result == RemoteData.Loading ) ]
                            , onClick (SubmitUser model.form)
                            ]
                            [ text T.signup ]

                      else
                        button [ class "button", disabled True ] [ text T.signup ]
                    ]
                ]
            ]
        , div []
            [ case model.result of
                RemoteData.Failure err ->
                    viewHttpErrors err

                _ ->
                    text ""
            ]
        ]
