{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2026 Fractale Co

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


module Pages.Login exposing (Flags, Model, Msg, Tab(..), initModel, page, update, viewCard)

import Assets as A
import Browser.Navigation as Nav
import Components.AuthModal exposing (UserAuthForm)
import Dict
import Form exposing (isLoginSendable, isSignupSendable)
import Fractale.Codecs exposing (FractalBaseRoute(..), toLink)
import Fractale.Error exposing (viewHttpErrors)
import Fractale.User exposing (UserState(..))
import Generated.Route as Route exposing (fromUrl, toHref)
import Global exposing (Msg(..))
import Html exposing (Html, a, button, div, input, label, li, p, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, name, placeholder, required, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Loading exposing (RestData)
import Maybe exposing (withDefault)
import ModelSchema exposing (UserCtx)
import Page exposing (Document, Page)
import Ports
import RemoteData
import Requests exposing (login, signup)
import Schema.Enum.Lang as Lang
import Text as T
import Utils.Bool exposing (ternary)
import Utils.Cmd exposing (send, sendSleep)
import Utils.DomEvents exposing (onClickPD, onKeydown)
import Utils.Html exposing (textH)


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
    , result : RestData UserCtx
    , tab : Tab
    }


type Tab
    = SigninTab
    | SignupTab



--
-- Init
--


type alias Flags =
    ()


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        gcmd =
            case global.session.common.user of
                LoggedIn uctx ->
                    send <| NavigateRaw <| toLink UsersBaseUri uctx.username []

                LoggedOut ->
                    Cmd.none
    in
    ( initModel global
    , Cmd.none
    , Cmd.batch [ gcmd, send (UpdateSessionFocus Nothing) ]
    )


{-| Opens on the sign-up tab on /signup (Pages.Signup aliases this page).
-}
initModel : Global.Model -> Model
initModel global =
    { form = { post = Dict.fromList [ ( "lang", Lang.toString global.session.common.lang ) ] }
    , result = RemoteData.NotAsked
    , tab = ternary (fromUrl global.url == Just Route.Signup) SignupTab SigninTab
    }



--
-- Update
--


type Msg
    = SubmitUser UserAuthForm
    | ChangeUserPost String String
    | GotSignin (RestData UserCtx)
    | GotSignup (RestData Bool)
    | ChangeTab Tab
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
            , case model.tab of
                SigninTab ->
                    login apis form.post GotSignin

                SignupTab ->
                    signup apis form.post GotSignup
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

        GotSignup result ->
            case result of
                RemoteData.Success _ ->
                    ( model
                    , Nav.pushUrl global.key (toHref Route.Verification ++ "?email=" ++ (Dict.get "email" model.form.post |> withDefault ""))
                    , Cmd.none
                    )

                RemoteData.Failure err ->
                    ( { model | result = RemoteData.Failure err }, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        ChangeTab tab ->
            -- bulma_driver binds the password toggle and focus helpers of the new inputs
            ( { model | tab = tab, result = RemoteData.NotAsked }, Ports.bulma_driver "", Cmd.none )

        SubmitKeyDown key ->
            -- 13: Enter
            if key == 13 && isSendable model then
                ( model, send (SubmitUser model.form), Cmd.none )

            else
                ( model, Cmd.none, Cmd.none )


isSendable : Model -> Bool
isSendable model =
    case model.tab of
        SigninTab ->
            isLoginSendable model.form.post

        SignupTab ->
            isSignupSendable model.form.post


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.none


view : Global.Model -> Model -> Document Msg
view global model =
    { title = ternary (model.tab == SigninTab) T.signin_ T.signup_
    , body = [ div [ id "loginForm", class "columns is-centered top-section" ] [ viewCard model ] ]
    }


{-| Also embedded in the home page (Pages.About).
-}
viewCard : Model -> Html Msg
viewCard model =
    div [ class "card has-background-evidence" ]
        [ div [ class "card-content" ]
            [ A.welcome
            , div [ class "tabs is-fullwidth" ]
                [ ul []
                    [ li [ classList [ ( "is-active", model.tab == SigninTab ) ] ] [ tabLink [] SigninTab [ text T.signin_ ] ]
                    , li [ classList [ ( "is-active", model.tab == SignupTab ) ] ] [ tabLink [] SignupTab [ text T.signup_ ] ]
                    ]
                ]
            , case model.tab of
                SigninTab ->
                    viewSignin model

                SignupTab ->
                    viewSignup model
            ]
        , case model.result of
            RemoteData.Failure err ->
                viewHttpErrors err

            _ ->
                text ""
        ]


{-| Switches the tab in place. The href keeps the link focusable and middle-clickable.
-}
tabLink : List (Html.Attribute Msg) -> Tab -> List (Html Msg) -> Html Msg
tabLink attrs tab =
    a (href (toHref (ternary (tab == SigninTab) Route.Login Route.Signup)) :: onClickPD (ChangeTab tab) :: target "_blank" :: attrs)


viewSignin : Model -> Html Msg
viewSignin model =
    div []
        [ viewField T.username
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
        , viewPassword model []
        , viewFooter model
            [ span [] [ text T.needAnAccount, tabLink [ class "ml-2 underlined-link" ] SignupTab [ text T.signupNow ] ]
            , a [ class "underlined-link", href (toHref Route.PasswordReset) ] [ textH T.passwordForgotten ]
            ]
        ]


viewSignup : Model -> Html Msg
viewSignup model =
    div []
        [ div [ class "subtitle is-size-6 is-strong" ] [ text T.createYourAccount, text ":" ]
        , viewField T.email
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
        , viewField T.username
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
        , viewPassword model [ p [ class "help" ] [ text T.passwordRequirements ] ]
        , viewFooter model
            [ span [] [ text T.alreadyAnAccount, tabLink [ class "ml-2 underlined-link" ] SigninTab [ textH T.signinNow ] ] ]
        ]


viewField : String -> List (Html Msg) -> Html Msg
viewField lbl content =
    div [ class "field is-horizntl" ]
        [ div [ class "field-lbl" ] [ label [ class "label" ] [ text lbl ] ]
        , div [ class "field-body" ] [ div [ class "field" ] content ]
        ]


viewPassword : Model -> List (Html Msg) -> Html Msg
viewPassword model help =
    viewField T.password
        (div [ class "control" ]
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
            , span [ class "passwordVisibilityTrigger icon-input-flex-right is-clickable" ]
                [ A.icon "icon-eye" ]
            ]
            :: help
        )


{-| Stacked shortcut links on the left, submit button on the right.
-}
viewFooter : Model -> List (Html Msg) -> Html Msg
viewFooter model links =
    let
        submitText =
            ternary (model.tab == SigninTab) T.signin T.signup
    in
    div [ class "level is-mobile mt-5" ]
        [ div [ class "level-left level-with-breakline is-size-7" ] links
        , div [ class "level-right field is-grouped is-grouped-right" ]
            [ div [ class "control" ]
                [ if isSendable model then
                    button
                        [ id "submitButton"
                        , class "button is-success"
                        , classList [ ( "is-loading", Loading.isLoadingRest model.result ) ]
                        , onClick (SubmitUser model.form)
                        ]
                        [ text submitText ]

                  else
                    button [ class "button", disabled True ] [ text submitText ]
                ]
            ]
        ]
