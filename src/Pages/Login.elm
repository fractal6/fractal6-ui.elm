module Pages.Login exposing (Flags, Model, Msg, page)

import Assets as A
import Browser.Navigation as Nav
import Components.AuthModal exposing (UserAuthForm)
import Dict exposing (Dict)
import Extra exposing (ternary, textH, upH)
import Extra.Events exposing (onKeydown)
import Form exposing (isLoginSendable)
import Fractal.Enum.Lang as Lang
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, name, placeholder, required, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Loading exposing (WebData, expectJson, viewHttpErrors)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), toString)
import ModelCommon.Requests exposing (login)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import RemoteData exposing (RemoteData)
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
    , result : WebData UserCtx
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
                    send <| NavigateRaw <| toString UsersBaseUri uctx.username []

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
    | GotSignin (WebData UserCtx) -- use remotedata.
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
            , login apis form.post GotSignin
            , Cmd.none
            )

        GotSignin result ->
            let
                cmds =
                    case result of
                        RemoteData.Success uctx ->
                            [ send (UpdateUserSession uctx)
                            , sendSleep RedirectOnLoggedIn 333
                            ]

                        _ ->
                            []
            in
            ( { model | result = result }
            , Cmd.none
            , Cmd.batch cmds
            )

        SubmitKeyDown key ->
            case key of
                13 ->
                    --ENTER
                    if isLoginSendable model.form.post then
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
    { title = T.signin_
    , body = [ view_ model ]
    }


view_ : Model -> Html Msg
view_ model =
    div [ id "loginForm", class "columns is-centered top-section" ]
        [ div [ class "" ]
            [ viewLogin model ]
        ]


viewLogin : Model -> Html Msg
viewLogin model =
    div [ class "card" ]
        [ div [ class "card-header" ]
            [ div [ class "card-header-title" ]
                [ text T.signin_ ]
            ]
        , div [ class "card-content" ]
            [ A.welcome
            , div [ class "field is-horizntl" ]
                [ div [ class "field-lbl" ] [ label [ class "label" ] [ text T.username ] ]
                , div [ class "field-body" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ input
                                [ class "input autofocus followFocus"
                                , attribute "data-nextfocus" "passwordInput"
                                , type_ "text"
                                , placeholder "username or email"
                                , name "username"
                                , value (Dict.get "username" model.form.post |> withDefault "")
                                , attribute "autocomplete" "username"
                                , required True
                                , onInput (ChangeUserPost "username")
                                , onKeydown SubmitKeyDown
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
                            ]
                        ]
                    ]
                ]
            , br [] []
            , div [ class "is-size-7 is-pulled-left" ]
                [ span [ class "mr-2" ] [ text T.needAnAccount ]
                , a [ class "underlined-link", href (toHref Route.Signup) ] [ text T.signupNow ]
                , br [ class "mb-1" ] []
                , a [ class "underlined-link", href (toHref Route.PasswordReset) ] [ textH T.passwordForgotten ]
                ]
            , div [ class "field is-grouped is-grouped-right" ]
                [ div [ class "control" ]
                    [ if isLoginSendable model.form.post then
                        button
                            [ id "submitButton"
                            , class "button is-success"
                            , classList [ ( "is-loading", model.result == RemoteData.Loading ) ]
                            , onClick (SubmitUser model.form)
                            ]
                            [ text T.signin ]

                      else
                        button [ class "button", disabled True ] [ text T.signin ]
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
