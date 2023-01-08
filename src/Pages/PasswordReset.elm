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


module Pages.PasswordReset exposing (Flags, Model, Msg, page)

import Assets as A exposing (almostThere)
import Browser.Navigation as Nav
import Components.AuthModal exposing (UserAuthForm)
import Dict exposing (Dict)
import Extra exposing (ternary, textH, upH)
import Extra.Events exposing (onKeydown)
import Extra.Url exposing (queryParser)
import Form exposing (isPasswordReset2Sendable, isPasswordResetSendable)
import Form.Help as Help
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, img, input, label, li, nav, p, small, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, name, placeholder, required, rows, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Image exposing (Image)
import Loading exposing (WebData, loadingSpin)
import Maybe exposing (withDefault)
import Bulk exposing (..)
import Bulk.Error exposing (viewHttpErrors)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import RemoteData exposing (RemoteData)
import Requests exposing (resetPassword, resetPassword2, resetPasswordChallenge, uuidCheck)
import Session exposing (GlobalCmd(..))
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
    , challenge_data : RemoteData Http.Error String
    , reset_result : WebData Bool
    , reset2_result : WebData UserCtx
    , token_reset : Maybe String
    , isValid : WebData Bool

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

        gcmd =
            Cmd.none

        model =
            { form = { post = Dict.empty }
            , result = RemoteData.NotAsked
            , challenge_data = RemoteData.Loading
            , reset_result = RemoteData.NotAsked
            , reset2_result = RemoteData.NotAsked
            , token_reset = Dict.get "x" query |> Maybe.map List.head |> withDefault Nothing
            , isValid = RemoteData.Loading
            , help = Help.init global.session.user conf
            }
    in
    case model.token_reset of
        Just token ->
            let
                form =
                    model.form

                newForm =
                    { form | post = Dict.insert "token" token form.post }
            in
            ( { model | form = newForm }, uuidCheck global.session.apis (Dict.fromList [ ( "token", token ) ]) GotUuidCheck, gcmd )

        Nothing ->
            ( model, send LoadCaptcha, gcmd )



--
-- Update
--


type Msg
    = LoadCaptcha
    | SubmitReset UserAuthForm
    | SubmitReset2 UserAuthForm
    | ChangeUserPost String String
    | GotReset (WebData Bool) -- use remotedata.
    | GotReset2 (WebData UserCtx) -- use remotedata.
      --| GotChallenge (RemoteData Http.Error File)
    | GotChallenge (RemoteData Http.Error (Maybe Image))
      --| FileLoaded String
    | SubmitKeyDown Int
    | GotUuidCheck (WebData Bool)
    | HelpMsg Help.Msg



-- For uploading file
--read : File -> Cmd Msg
--read file =
--    Task.perform FileLoaded (File.toUrl file)


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global message model =
    let
        apis =
            global.session.apis
    in
    case message of
        LoadCaptcha ->
            ( { model | challenge_data = RemoteData.Loading }, resetPasswordChallenge apis GotChallenge, Cmd.none )

        ChangeUserPost field value ->
            let
                form =
                    model.form

                formUpdated =
                    { form | post = Dict.insert field value form.post }
            in
            ( { model | form = formUpdated }, Cmd.none, Cmd.none )

        SubmitReset form ->
            ( { model | reset_result = RemoteData.Loading }
            , resetPassword apis form.post GotReset
            , Cmd.none
            )

        SubmitReset2 form ->
            ( { model | reset2_result = RemoteData.Loading }
            , resetPassword2 apis form.post GotReset2
            , Cmd.none
            )

        GotReset result ->
            ( { model | reset_result = result }
            , Cmd.none
            , Cmd.none
            )

        GotReset2 result ->
            ( { model | reset2_result = result }
            , Cmd.none
            , case result of
                RemoteData.Success uctx ->
                    send (UpdateUserSession uctx)

                _ ->
                    Cmd.none
            )

        GotChallenge result ->
            case result of
                RemoteData.Success (Just image) ->
                    ( { model | challenge_data = RemoteData.Success (Image.toPngUrl image) }, Cmd.none, Cmd.none )

                RemoteData.Success Nothing ->
                    ( { model | challenge_data = RemoteData.Failure Http.Timeout }, Cmd.none, Cmd.none )

                _ ->
                    ( { model | challenge_data = RemoteData.Loading }, Cmd.none, Cmd.none )

        --FileLoaded f ->
        --    ( { model | challenge_data = RemoteData.Success f }, Cmd.none, Cmd.none )
        SubmitKeyDown key ->
            case key of
                13 ->
                    --ENTER
                    if isPasswordResetSendable model.form.post then
                        ( model, send (SubmitReset model.form), Cmd.none )

                    else if isPasswordReset2Sendable model.form.post then
                        ( model, send (SubmitReset2 model.form), Cmd.none )

                    else
                        ( model, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        GotUuidCheck result ->
            ( { model | isValid = result }, Cmd.none, Cmd.none )

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
    { title = "Password reset"
    , body =
        [ view_ global model
        , Help.view {} model.help |> Html.map HelpMsg
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ class "columns is-centered top-section" ]
        [ div [ class "" ]
            [ case model.token_reset of
                Just t ->
                    case model.reset2_result of
                        RemoteData.Success _ ->
                            div []
                                [ div [ class "notification is-light is-success" ] [ text T.passwordUpdated ]
                                , a [ href "/" ] [ text T.goHome ]
                                ]

                        _ ->
                            case model.isValid of
                                RemoteData.Success True ->
                                    viewResetForm2 global model

                                RemoteData.Success False ->
                                    div [ class "notification is-light is-warning" ] [ text T.sessionExpired2 ]

                                RemoteData.Failure err ->
                                    viewHttpErrors err

                                RemoteData.Loading ->
                                    div [ class "spinner" ] []

                                RemoteData.NotAsked ->
                                    text ""

                Nothing ->
                    case model.reset_result of
                        RemoteData.Success True ->
                            almostThere (Dict.get "email" model.form.post |> withDefault "") T.toResetYourPassword (toHref Route.PasswordReset)

                        _ ->
                            viewResetForm global model
            ]
        ]


viewResetForm : Global.Model -> Model -> Html Msg
viewResetForm global model =
    div [ id "loginForm" ]
        [ div [ class "card" ]
            [ div [ class "card-header" ]
                [ div [ class "card-header-title" ]
                    [ text "Forgot your password?" ]
                ]
            , div [ class "card-content" ]
                [ div [ class "field is-horizntl" ]
                    [ div [ class "field-lbl" ] [ label [ class "label" ] [ text T.enterYourEmail ] ]
                    , div [ class "field-body" ]
                        [ div [ class "field" ]
                            [ div [ class "control" ]
                                [ input
                                    [ class "input autofocus"
                                    , attribute "data-nextfocus" "challengeInput"
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
                , div [ class "field is-horizntl mt-5" ]
                    [ div [ class "field-lbl" ] [ label [ class "label" ] [ text T.enterTheCode ] ]
                    , div [ class "field-body" ]
                        [ case model.challenge_data of
                            RemoteData.Success challenge ->
                                --div [ style "background-image" ("url('" ++ Image.toPngUrl challenge ++ "')") ] []
                                img [ src challenge, style "background" "white" ] []

                            RemoteData.Loading ->
                                --div [ class "spinner2" ] []
                                --loadingSpin True
                                img [ src "invis.gif", style "width" "150", style "height" "50", style "background" "white" ] []

                            RemoteData.Failure err ->
                                --viewHttpErrors err
                                div [ class "box has-background-danger is-size-6" ] [ text "Failed to fetch captcha" ]

                            RemoteData.NotAsked ->
                                text ""
                        , div [ class "button m-2", classList [ ( "is-loading", model.challenge_data == RemoteData.Loading ) ], onClick LoadCaptcha ] [ A.icon "icon-refresh-ccw" ]
                        , div [ class "field" ]
                            [ div [ class "control m-2" ]
                                [ input
                                    [ id "challengeInput"
                                    , class "input"
                                    , style "max-width" "200px"
                                    , type_ "challenge"
                                    , placeholder ""
                                    , name "challenge"
                                    , value (Dict.get "challenge" model.form.post |> withDefault "")
                                    , required True
                                    , onInput (ChangeUserPost "challenge")
                                    , onKeydown SubmitKeyDown
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                , br [] []
                , div [ class "field is-grouped is-grouped-right" ]
                    [ div [ class "control" ]
                        [ if isPasswordResetSendable model.form.post then
                            button
                                [ id "submitButton"
                                , class "button is-success"
                                , classList [ ( "is-loading", model.reset_result == RemoteData.Loading ) ]
                                , onClick (SubmitReset model.form)
                                ]
                                [ text "Reset password" ]

                          else
                            button [ class "button", disabled True ] [ text T.resetPassword ]
                        ]
                    ]
                ]
            ]
        , div []
            [ case model.result of
                RemoteData.Failure err ->
                    viewHttpErrors err

                _ ->
                    text ""
            , case model.reset_result of
                RemoteData.Success False ->
                    div [ class "notification is-light is-warning" ] [ text "Wrong code, please try again." ]

                RemoteData.Failure err ->
                    viewHttpErrors err

                _ ->
                    text ""
            ]
        ]


viewResetForm2 : Global.Model -> Model -> Html Msg
viewResetForm2 global model =
    div [ id "loginForm" ]
        [ div [ class "card" ]
            [ div [ class "card-header" ]
                [ div [ class "card-header-title" ]
                    [ text "Update your password" ]
                ]
            , div [ class "card-content" ]
                [ div [ class "field is-horizntl" ]
                    [ div [ class "field-lbl" ] [ label [ class "label" ] [ text T.newPassword ] ]
                    , div [ class "field-body" ]
                        [ div [ class "field" ]
                            [ div [ class "control" ]
                                [ input
                                    [ id "passwordInput"
                                    , class "input followFocus"
                                    , attribute "data-nextfocus" "passwordInput2"
                                    , type_ "password"
                                    , placeholder "password"
                                    , name "password"
                                    , value (Dict.get "password" model.form.post |> withDefault "")
                                    , attribute "autocomplete" "password"
                                    , required True
                                    , onInput (ChangeUserPost "password")
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                , div [ class "field is-horizntl" ]
                    [ div [ class "field-lbl" ] [ label [ class "label" ] [ text T.confirmYourPassword ] ]
                    , div [ class "field-body" ]
                        [ div [ class "field" ]
                            [ div [ class "control" ]
                                [ input
                                    [ id "passwordInput2"
                                    , class "input"
                                    , type_ "password"
                                    , placeholder "password"
                                    , name "password"
                                    , value (Dict.get "password2" model.form.post |> withDefault "")
                                    , attribute "autocomplete" "password"
                                    , required True
                                    , onInput (ChangeUserPost "password2")
                                    , onKeydown SubmitKeyDown
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                , br [] []
                , div [ class "field is-grouped is-grouped-right" ]
                    [ div [ class "control" ]
                        [ if isPasswordReset2Sendable model.form.post then
                            button
                                [ id "submitButton"
                                , class "button is-success"
                                , classList [ ( "is-loading", model.reset2_result == RemoteData.Loading ) ]
                                , onClick (SubmitReset2 model.form)
                                ]
                                [ text T.resetPassword ]

                          else
                            button [ class "button", disabled True ] [ text T.resetPassword ]
                        ]
                    ]
                ]
            ]
        , div []
            [ case model.result of
                RemoteData.Failure err ->
                    viewHttpErrors err

                _ ->
                    text ""
            , case model.reset2_result of
                RemoteData.Failure err ->
                    viewHttpErrors err

                _ ->
                    text ""
            ]
        ]
