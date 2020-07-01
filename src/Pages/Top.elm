module Pages.Top exposing (Flags, Model, Msg, page)

import Browser.Navigation as Nav
import Components.Loading as Loading exposing (WebData, expectJson, viewHttpErrors)
import Dict exposing (Dict)
import Extra.Events exposing (onClickPD)
import Form exposing (isPostSendable)
import Generated.Route as Route exposing (Route)
import Global exposing (Msg(..))
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, name, placeholder, required, rows, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Requests exposing (login, signup)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
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
    , viewMode : ViewMode
    }


type ViewMode
    = Login
    | Signup


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
            , viewMode = Login
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
    | ChangeViewMode ViewMode


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global msg model =
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
            , case model.viewMode of
                Login ->
                    login form.post GotSignin

                Signup ->
                    signup form.post GotSignin
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

        ChangeViewMode viewMode ->
            ( { model | viewMode = viewMode }, Cmd.none, Ports.bulma_driver "" )


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
    viewHero model


viewHero : Model -> Html Msg
viewHero model =
    div [ id "welcome", class "hero is-dark is-bold" ]
        [ div [ class "hero-body" ]
            [ div [ class "columns is-centered section" ]
                [ div [ class "column is-3" ]
                    [ h1 [ class "title is-1" ]
                        [ text "Build unstoppable organisations" ]
                    , h2
                        [ class "subtitle" ]
                      <|
                        List.intersperse (text " ")
                            [ span [ class "has-text-weight-semibold" ] [ text "Grolab" ]
                            , text "is a collaborative platform inspired by"
                            , text "self-organized"
                            , text "systems and"
                            , text "open source culture. Join a network that enact collective intelligence and build transparent and resilient organisation."
                            ]
                    ]
                , div [ class "column is-3" ]
                    [ viewSignBox model ]
                ]
            ]
        ]


viewSignBox : Model -> Html Msg
viewSignBox model =
    div [ class "form" ]
        [ div [ class "card" ]
            [ div [ class "card-header" ]
                [ div [ class "card-header-title tabs is-fullwidth" ]
                    [ ul []
                        [ li [ classList [ ( "is-active", model.viewMode == Login ) ] ] [ a [ onClickPD (ChangeViewMode Login), target "_blank" ] [ text "Login" ] ]
                        , li [ classList [ ( "is-active", model.viewMode == Signup ) ] ] [ a [ onClickPD (ChangeViewMode Signup), target "_blank" ] [ text "Signup" ] ]
                        ]
                    ]
                ]
            , div [ class "card-content" ]
                [ case model.viewMode of
                    Login ->
                        viewLogin model

                    Signup ->
                        viewSignup model
                , div []
                    [ case model.form.result of
                        RemoteData.Failure err ->
                            viewHttpErrors err

                        default ->
                            text ""
                    ]
                ]
            ]
        ]


viewLogin : Model -> Html Msg
viewLogin model =
    let
        isSendable =
            isPostSendable [ "username", "password" ] model.form.post
    in
    div []
        [ div [ class "field is-horizntl" ]
            [ div [ class "field-lbl" ] [ label [ class "label" ] [ text "Username" ] ]
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
        , br [] []
        , div [ class "field is-grouped is-grouped-right" ]
            [ div [ class "control" ]
                [ if isSendable then
                    button
                        [ id "submitButton"
                        , class "button is-success has-text-weight-semibold"
                        , onClick (SubmitUser model.form)
                        ]
                        [ text "Sign in" ]

                  else
                    button [ class "button has-text-weight-semibold", disabled True ]
                        [ text "Sign in" ]
                ]
            ]
        ]


viewSignup : Model -> Html Msg
viewSignup model =
    let
        isSendable =
            isPostSendable [ "username", "email", "password" ] model.form.post
    in
    div [ class "" ]
        [ div [ class "field is-horizntl" ]
            [ div [ class "field-lbl" ] [ label [ class "label" ] [ text "Username" ] ]
            , div [ class "field-body" ]
                [ div [ class "field" ]
                    [ div [ class "control" ]
                        [ input
                            [ class "input autofocus followFocus"
                            , attribute "data-nextfocus" "emailInput2"
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
            [ div [ class "field-lbl" ] [ label [ class "label" ] [ text "Email" ] ]
            , div [ class "field-body" ]
                [ div [ class "field" ]
                    [ div [ class "control" ]
                        [ input
                            [ id "emailInput2"
                            , class "input followFocus"
                            , attribute "data-nextfocus" "passwordInput2"
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
            [ div [ class "field-lbl" ] [ label [ class "label" ] [ text "Password" ] ]
            , div [ class "field-body" ]
                [ div [ class "field" ]
                    [ div [ class "control" ]
                        [ input
                            [ id "passwordInput2"
                            , class "input followFocus"
                            , attribute "data-nextfocus" "submitButton2"
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
        , br [] []
        , div [ class "field is-grouped is-grouped-right" ]
            [ div [ class "control" ]
                [ if isSendable then
                    button
                        [ id "submitButton2"
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
