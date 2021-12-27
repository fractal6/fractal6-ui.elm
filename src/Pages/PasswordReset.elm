module Pages.PasswordReset exposing (Flags, Model, Msg, page)

import Assets as A
import Browser.Navigation as Nav
import Components.Loading as Loading exposing (WebData, expectJson, loadingSpin, viewHttpErrors)
import Dict exposing (Dict)
import Extra.Events exposing (onKeydown)
import Extra.Url exposing (queryParser)
import Form exposing (isPasswordReset2Sendable, isPasswordResetSendable)
import Generated.Route as Route exposing (Route)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, img, input, label, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, name, placeholder, required, rows, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Image exposing (Image)
import Json.Decode as JD
import Json.Encode as JE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Requests exposing (resetPassword, resetPassword2, resetPasswordChallenge, uuidCheck)
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
    , token_reset : Maybe String
    , isValid : WebData Bool
    }



--
-- Init
--


type alias Flags =
    ()


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        -- Query parameters
        query =
            queryParser global.url

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
            , token_reset = Dict.get "x" query |> Maybe.map List.head |> withDefault Nothing
            , isValid = RemoteData.Loading
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
      --| GotChallenge (RemoteData Http.Error File)
    | GotChallenge (RemoteData Http.Error (Maybe Image))
      --| FileLoaded String
    | SubmitKeyDown Int
    | GotUuidCheck (WebData Bool)



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
            ( { model | reset_result = RemoteData.Loading }
            , resetPassword2 apis form.post GotReset
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

                    else if isPasswordReset2Sendable model.form.post then
                        ( model, send (SubmitReset2 model.form), Cmd.none )

                    else
                        ( model, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        GotUuidCheck result ->
            ( { model | isValid = result }, Cmd.none, Cmd.none )


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
            [ case model.token_reset of
                Just t ->
                    case model.reset_result of
                        RemoteData.Success True ->
                            div []
                                [ div [ class "notification is-light is-success" ] [ textH "Your password has been updated." ]
                                , a [ href "/" ] [ text "Go to home page." ]
                                ]

                        _ ->
                            case model.isValid of
                                RemoteData.Success True ->
                                    viewResetForm2 global model

                                RemoteData.Success False ->
                                    div [ class "notification is-light is-warning" ] [ textH "The session expired, please try again." ]

                                RemoteData.Failure err ->
                                    viewHttpErrors err

                                RemoteData.Loading ->
                                    div [ class "spinner" ] []

                                RemoteData.NotAsked ->
                                    text ""

                Nothing ->
                    case model.reset_result of
                        RemoteData.Success True ->
                            div [ class "notification is-light is-success" ] [ textH "An email has been sent to you with instructions.", br [] [], text "(check your spam)" ]

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
                    [ div [ class "field-lbl" ] [ label [ class "label" ] [ text "Enter the code below" ] ]
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
                    div [ class "notification is-light is-warning" ] [ textH "wrong code, please try again." ]

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
                    [ div [ class "field-lbl" ] [ label [ class "label" ] [ text "New Password" ] ]
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
                    [ div [ class "field-lbl" ] [ label [ class "label" ] [ text "Confirm your password" ] ]
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
                                , classList [ ( "is-loading", model.reset_result == RemoteData.Loading ) ]
                                , onClick (SubmitReset2 model.form)
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
                    div [ class "notification is-light is-warning" ] [ textH "Something gone wrong, please try again." ]

                RemoteData.Failure err ->
                    viewHttpErrors err

                _ ->
                    text ""
            ]
        ]
