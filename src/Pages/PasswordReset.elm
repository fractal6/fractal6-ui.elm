module Pages.PasswordReset exposing (Flags, Model, Msg, page)

import Browser.Navigation as Nav
import Components.Loading as Loading exposing (WebData, expectJson, loadingSpin, viewHttpErrors, withMapWebData)
import Dict exposing (Dict)
import Extra.Events exposing (onKeydown)
import Form exposing (isPasswordResetSendable)
import Generated.Route as Route exposing (Route)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, img, input, label, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, name, placeholder, required, rows, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Icon as I
import Image exposing (Image)
import Json.Decode as JD
import Json.Encode as JE
import Logo exposing (welcome)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Requests exposing (resetPassword, resetPasswordChallenge)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import RemoteData exposing (RemoteData)
import Task
import Text as T exposing (textH, textT)


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
    , challenge_data : RemoteData Http.Error String
    , reset_result : WebData Bool
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
                    Global.navigate <| Route.User_Dynamic { param1 = uctx.username }

                LoggedOut ->
                    Cmd.none

        model =
            { form =
                { post = Dict.empty, result = RemoteData.NotAsked }
            , challenge_data = RemoteData.Loading
            , reset_result = RemoteData.NotAsked
            }
    in
    ( model
    , send LoadCaptcha
    , gcmd
    )



--
-- Update
--


type Msg
    = LoadCaptcha
    | SubmitReset UserAuthForm
    | ChangeUserPost String String
    | GotReset (WebData Bool) -- use remotedata.
      --| GotChallenge (RemoteData Http.Error File)
    | GotChallenge (RemoteData Http.Error (Maybe Image))
      --| FileLoaded String
    | SubmitKeyDown Int



-- For uploading file
--read : File -> Cmd Msg
--read file =
--    Task.perform FileLoaded (File.toUrl file)


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global msg model =
    let
        apis =
            global.session.apis
    in
    case msg of
        LoadCaptcha ->
            ( { model | challenge_data = RemoteData.Loading }, resetPasswordChallenge apis.auth GotChallenge, Cmd.none )

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
            , resetPassword apis.auth form.post GotReset
            , Cmd.none
            )

        GotReset result ->
            ( { model | reset_result = result }, Cmd.none, Cmd.none )

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

                    else
                        ( model, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.none


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Login"
    , body = [ view_ global model ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ class "columns is-centered section" ]
        [ div [ class "column is-4" ]
            [ case model.reset_result of
                RemoteData.Success True ->
                    div [ class "notification is-light is-success" ] [ textH "An email has been sent to you with instructions." ]

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
                    [ div [ class "field-lbl" ] [ label [ class "label" ] [ text "Enter your email" ] ]
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
                    [ div [ class "field-lbl" ] [ label [ class "label" ] [ text "Enter the result of the following operation" ] ]
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
                        , div [ class "button m-2", classList [ ( "is-loading", model.challenge_data == RemoteData.Loading ) ], onClick LoadCaptcha ] [ I.icon "icon-refresh-ccw" ]
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
                            button [ class "button", disabled True ] [ textH "reset password" ]
                        ]
                    ]
                ]
            ]
        , div []
            [ case model.form.result of
                RemoteData.Failure err ->
                    viewHttpErrors err

                _ ->
                    text ""
            , case model.reset_result of
                RemoteData.Success False ->
                    div [ class "notification is-light is-warning" ] [ textH "wrong calcul, please try agin." ]

                RemoteData.Failure err ->
                    viewHttpErrors err

                _ ->
                    text ""
            ]
        ]
