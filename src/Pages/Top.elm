module Pages.Top exposing (Flags, Model, Msg, page)

import Browser.Navigation as Nav
import Components.Loading as Loading exposing (WebData, expectJson, viewHttpErrors)
import Dict exposing (Dict)
import Extra.Events exposing (onClickPD, onKeydown)
import Form exposing (isLoginSendable, isSignupSendable)
import Generated.Route as Route exposing (Route)
import Global exposing (Msg(..), send, sendSleep)
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
import Text as T exposing (textH, textT, upH)



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



---- MODEL ----


type alias Model =
    { form : UserAuthForm
    , viewMode : ViewMode
    }


type ViewMode
    = Login
    | Signup



---- MSG ----


type Msg
    = SubmitUser UserAuthForm
    | ChangeUserPost String String
    | GotSignin (WebData UserCtx) -- use remotedata.
    | ChangeViewMode ViewMode
    | SubmitKeyDown Int



---- INIT ----


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        gcmd =
            case global.session.user of
                LoggedIn uctx ->
                    Nav.replaceUrl global.key <| Route.toHref <| Route.User_Dynamic { param1 = uctx.username }

                LoggedOut ->
                    Cmd.none

        model =
            { form = { post = Dict.empty, result = RemoteData.NotAsked }
            , viewMode = Login
            }
    in
    ( model, Cmd.none, gcmd )



---- UPDATE ----


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
            , case model.viewMode of
                Login ->
                    login apis.auth form.post GotSignin

                Signup ->
                    signup apis.auth form.post GotSignin
            , Cmd.none
            )

        GotSignin result ->
            let
                cmds =
                    case result of
                        RemoteData.Success uctx ->
                            [ send (UpdateUserSession uctx)
                            , sendSleep RedirectOnLoggedIn 300
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
            let
                form =
                    model.form
            in
            ( { model | viewMode = viewMode, form = { form | result = RemoteData.NotAsked } }, Cmd.none, Ports.bulma_driver "" )

        SubmitKeyDown key ->
            case key of
                13 ->
                    --ENTER
                    let
                        isSendable =
                            case model.viewMode of
                                Login ->
                                    isLoginSendable model.form.post

                                Signup ->
                                    isSignupSendable model.form.post
                    in
                    if isSendable then
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
                [ div [ class "column is-6-desktop is-4-widescreen is-3-fullhd" ]
                    [ h1 [ class "title is-1" ]
                        [ text "Build unstoppable organisations" ]
                    , h2
                        [ class "subtitle" ]
                      <|
                        List.intersperse (text " ")
                            [ span [ class "has-text-weight-semibold" ] [ text "Fractale" ]
                            , text "is a collaborative platform inspired by"
                            , text "self-organized"
                            , text "systems and"
                            , text "open source culture. Join a network that enact collective intelligence to build transparent and resilient organisation."
                            ]
                    ]
                , div [ class "column is-6-desktop is-4-widescreen is-3-fullhd" ]
                    [ viewSignBox model ]
                ]
            ]
        ]


viewSignBox : Model -> Html Msg
viewSignBox model =
    div [ class "card" ]
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


viewLogin : Model -> Html Msg
viewLogin model =
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
        , div [ attribute "style" "width: 225px;" ]
            [ a [ class "is-size-7 is-pulled-left mb-2", onClickPD (ChangeViewMode Signup), target "_blank" ]
                [ textH T.createAccount ]
            , a [ class "is-size-7 is-pulled-left", href (Route.toHref Route.Login) ]
                [ textH T.passwordForgotten ]
            ]
        , div [ class "field is-grouped is-grouped-right" ]
            [ div [ class "control" ]
                [ if isLoginSendable model.form.post then
                    button
                        [ id "submitButton"
                        , class "button is-success"
                        , onClick (SubmitUser model.form)
                        ]
                        [ text "Sign in" ]

                  else
                    button [ class "button", disabled True ]
                        [ text "Sign in" ]
                ]
            ]
        ]


viewSignup : Model -> Html Msg
viewSignup model =
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
                        , p [ class "help" ] [ text "Password must be 8 characters or longer." ]
                        ]
                    ]
                ]
            ]
        , br [] []
        , div [ class "field is-grouped is-grouped-right" ]
            [ div [ class "control" ]
                [ if isSignupSendable model.form.post then
                    button
                        [ id "submitButton2"
                        , class "button is-success"
                        , onClick (SubmitUser model.form)
                        ]
                        [ text "Sign up" ]

                  else
                    button [ class "button", disabled True ]
                        [ text "Sign up" ]
                ]
            ]
        ]
