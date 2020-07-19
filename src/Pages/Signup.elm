module Pages.Signup exposing (Flags, Model, Msg, page)

import Components.Loading as Loading exposing (WebData, expectJson, viewHttpErrors)
import Dict exposing (Dict)
import Form exposing (isPostSendable)
import Generated.Route as Route exposing (Route)
import Global exposing (Msg(..))
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, autofocus, class, classList, disabled, href, id, name, placeholder, required, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Requests exposing (signup)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Process
import RemoteData exposing (RemoteData)
import Task


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
    { form : UserForm
    }


type alias UserForm =
    { post : Dict String String
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
                    Global.navigate <| Route.User_Dynamic { param1 = uctx.username }

                LoggedOut ->
                    Cmd.none

        model =
            { form =
                { post = Dict.empty
                , result = RemoteData.NotAsked
                }
            }
    in
    ( model, Cmd.none, gcmd )



--
-- Update
--


type Msg
    = SubmitUser UserForm
    | ChangeUserPost String String
    | GotSignin (WebData UserCtx) -- use remotedata.


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
            ( model
            , signup apis.auth form.post GotSignin
            , Cmd.none
            )

        GotSignin result ->
            let
                cmds =
                    case result of
                        RemoteData.Success uctx ->
                            [ Global.send (UpdateUserSession uctx)
                            , Global.sendSleep RedirectOnLoggedIn 300
                            ]

                        default ->
                            []

                form =
                    model.form

                formUpdated =
                    { form | result = result }
            in
            ( { model | form = formUpdated }
            , Cmd.none
            , Cmd.batch cmds
            )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.none


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Signup"
    , body = [ view_ global model ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ class "columns is-centered section" ]
        [ div [ class "column is-3" ]
            [ viewLogin global model ]
        ]


viewLogin : Global.Model -> Model -> Html Msg
viewLogin global model =
    let
        isSendable =
            isPostSendable [ "username", "email", "password" ] model.form.post
    in
    div [ class "form" ]
        [ div [ class "card" ]
            [ div [ class "card-header" ]
                [ div [ class "card-header-title" ]
                    [ text "Signup" ]
                ]
            , div [ class "card-content" ]
                [ div [ class "field is-horizntl" ]
                    [ div [ class "field-lbl" ] [ label [ class "label" ] [ text "Username" ] ]
                    , div [ class "field-body" ]
                        [ div [ class "field" ]
                            [ div [ class "control" ]
                                [ input
                                    [ class "input autofocus followFocus"
                                    , attribute "data-nextfocus" "emailInput"
                                    , type_ "text"
                                    , placeholder "username"
                                    , name "username"
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
                    [ div [ class "field-lbl" ] [ label [ class "label" ] [ text "Email" ] ]
                    , div [ class "field-body" ]
                        [ div [ class "field" ]
                            [ div [ class "control" ]
                                [ input
                                    [ id "emailInput"
                                    , class "input followFocus"
                                    , attribute "data-nextfocus" "passwordInput"
                                    , type_ "text"
                                    , placeholder "email"
                                    , name "email"
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
                    [ div [ class "field-lbl" ] [ label [ class "label" ] [ text "Password" ] ]
                    , div [ class "field-body" ]
                        [ div [ class "field" ]
                            [ div [ class "control" ]
                                [ input
                                    [ id "passwordInput"
                                    , class "input followFocus"
                                    , attribute "data-nextfocus" "submitButton"
                                    , type_ "password"
                                    , placeholder "password"
                                    , name "password"
                                    , attribute "autocomplete" "password"
                                    , required True
                                    , onInput (ChangeUserPost "password")
                                    ]
                                    []
                                , p [ class "help" ] [ text "Password must be 8 characters or longer." ]
                                ]
                            ]
                        ]
                    ]
                , br [] []
                , div [ class "field is-grouped is-grouped-right" ]
                    [ div [ class "control" ]
                        [ if isSendable then
                            button
                                [ id "submitButton"
                                , class "button is-success has-text-weight-semibold"
                                , onClick (SubmitUser model.form)
                                ]
                                [ text "Sign up" ]

                          else
                            button [ class "button has-text-weight-semibold", disabled True ]
                                [ text "Sign up" ]
                        ]
                    ]
                ]
            ]
        , div []
            [ case model.form.result of
                RemoteData.Failure err ->
                    viewHttpErrors err

                default ->
                    text ""
            ]
        ]
