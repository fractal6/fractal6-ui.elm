module Pages.Top exposing (Flags, Model, Msg, page)

import Assets as A
import Browser.Navigation as Nav
import Components.AuthModal exposing (UserAuthForm)
import Dict exposing (Dict)
import Extra exposing (ternary, textH, upH)
import Extra.Events exposing (onClickPD, onKeydown)
import Form exposing (isLoginSendable, isSignupSendable)
import Fractal.Enum.Lang as Lang
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, dd, div, dl, dt, figcaption, figure, h1, h2, hr, i, iframe, img, input, label, li, nav, p, span, strong, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, height, href, id, name, placeholder, required, rows, src, style, target, title, type_, value, width)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Loading exposing (WebData, expectJson, viewHttpErrors)
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), toString)
import ModelCommon.Requests exposing (login, signup)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import RemoteData exposing (RemoteData)
import Task
import Text as T



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
    , result : WebData UserCtx
    , viewMode : ViewMode
    , lang : Lang.Lang
    }


type ViewMode
    = Login
    | Signup



---- MSG ----


type Msg
    = SubmitUser UserAuthForm
    | ChangeUserPost String String
    | GotSignin (WebData UserCtx)
    | GotSignup (WebData Bool)
    | ChangeViewMode ViewMode
    | SubmitEnter Int



---- INIT ----


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        gcmd =
            case global.session.user of
                LoggedIn uctx ->
                    Nav.replaceUrl global.key <| toString UsersBaseUri uctx.username []

                LoggedOut ->
                    Cmd.none

        model =
            { form = { post = Dict.empty }
            , result = RemoteData.NotAsked
            , viewMode = Login
            , lang = global.session.lang
            }
    in
    ( model
    , Cmd.none
    , Cmd.batch [ gcmd, send (UpdateSessionFocus Nothing) ]
    )



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
            ( { model | result = RemoteData.Loading }
            , case model.viewMode of
                Login ->
                    login apis form.post GotSignin

                Signup ->
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
            ( case result of
                RemoteData.Failure err ->
                    { model | result = RemoteData.Failure err }

                _ ->
                    model
            , case result of
                RemoteData.Success ok ->
                    Nav.pushUrl global.key (toHref Route.Verification ++ "?email=" ++ (Dict.get "email" model.form.post |> withDefault ""))

                _ ->
                    Cmd.none
            , Cmd.none
            )

        ChangeViewMode viewMode ->
            let
                form =
                    model.form
            in
            ( { model | viewMode = viewMode, result = RemoteData.NotAsked }, Cmd.none, Ports.bulma_driver "" )

        SubmitEnter key ->
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
    { title = T.welcome
    , body = [ view_ global model ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ id "welcome", class "top-section" ]
        [ div [ id "welcome-1" ] [ viewHero model ]
        , div [ id "welcome-2" ] [ viewFeatures model ]
        , div [ class "columns pt-5 pb-5" ]
            [ div [ class "hero column is-offset-2" ]
                [ div [ class "hero-body content" ]
                    [ div [ class "title is-2" ]
                        [ text (T.moreToCome ++ "...") ]
                    , renderMarkdown "pb-6 is-size-5" T.roadmapX

                    --ul [ class "pt-4 pb-6 is-size-5" ]
                    --    [ li [] [ text T.roadmap1 ]
                    --    , li [] [ text T.roadmap2 ]
                    --    , li [] [ text T.roadmap3 ]
                    --    , li [] [ text T.roadmap4 ]
                    --    , li [] [ text T.roadmap5 ]
                    --    , li [] [ text "GraphQL API" ]
                    --    ]
                    , div [ class "subtitle is-4" ]
                        [ text (T.keepInTouch1 ++ ",")
                        , br [ class "mb-1" ] []
                        , text (T.keepInTouch2 ++ ",")
                        , br [ class "mb-1" ] []
                        , a [ class "button is-primary is-size-5 is-text-aligned", href (toHref Route.Signup), style "border-radius" "8px" ] [ text T.joinUs ]
                        ]
                    ]
                ]
            ]
        ]


viewHero : Model -> Html Msg
viewHero model =
    div []
        [ div [ class "columns is-centered custom-b-1 m-0" ]
            [ div [ class "column is-7-desktop is-7-widescreen is-6-fullhd" ]
                [ h1 [ class "title top-title" ] [ renderMarkdown "is-human" T.heroTitle ]

                --[ text "Build unstoppable organizations" ]
                --[ text "Self-organization", br [] [], text "for humans" ]
                --[ text "Collective Intelligence", br [] [], text "at Work" ]
                , div [ class "columns is-vcentered" ]
                    [ div [ class "column is-6" ] [ h2 [ class "subtitle" ] [ renderMarkdown "is-recursive" T.heroSubtitle ] ]
                    , div [ class "column is-5" ]
                        [ a
                            [ class "button is-primary"
                            , href (toHref Route.Signup)
                            , style "border-radius" "8px"
                            ]
                            [ text T.tryNow ]
                        ]
                    ]
                ]
            , div [ class "column is-4-desktop is-4-widescreen is-3-fullhd is-hidden-mobile" ]
                [ viewSignBox model ]
            ]
        , br [] []
        , div [ class "columns is-centered pb-6 mb-5 m-0" ]
            [ div [ class "column is-11 is-9-fullhd" ]
                [ div [ class "columns is-5 is-variable content" ]
                    [ div [ class "column is-4" ] [ h1 [] [ logo, text T.about ], p [] [ renderMarkdown "is-human" T.aboutHero ] ]
                    , div [ class "column is-4" ] [ h1 [] [ logo, text T.forNpo ], p [] [ renderMarkdown "is-human" T.npoHero ] ]
                    , div [ class "column is-4" ] [ h1 [] [ logo, text T.forCorp ], p [] [ renderMarkdown "is-human" T.corpHero ] ]
                    ]
                ]
            ]
        ]


logo : Html Msg
logo =
    span [ attribute "style" "position:relative;top:10px;right:10px;" ] [ A.logo1 "white" ]


viewFeatures : Model -> Html Msg
viewFeatures model =
    div []
        [ div [ id "feature-1", class "hero is-fullheight is-primary-2 dub-top-section" ]
            [ div [ class "columns is-vcentered is-centered m-0" ]
                -- hero-body squeeze the columns !
                -- , style "align-items" "flex-start"
                [ div [ class "column is-4 content" ]
                    [ h1 [ class "title" ] [ text T.govTransp ]
                    , renderMarkdown "subtitle pt-5 pb-4 is-human" T.govTranspAbout
                    , dl [ class "is-icon-aligned" ]
                        [ dt [ class "mb-4" ] [ A.icon1 "icon-share icon-2x" T.govFeat1 ]
                        , dt [ class "mb-4" ] [ A.icon1 "icon-users icon-2x" T.govFeat2 ]
                        , dt [ class "mb-4" ] [ A.icon1 "icon-lock icon-2x" T.govFeat3 ]
                        , dt [ class "mb-4" ] [ A.icon1 "icon-eye icon-2x" T.govFeat4 ]
                        , dt [ class "mb-4" ] [ A.icon1 "icon-book-open icon-2x" T.govFeat5 ]
                        , dt [ class "mb-4" ] [ A.icon1 "icon-search icon-2x" T.govFeat6 ]
                        , dt [ class "mb-4" ] [ A.icon1 "icon-history icon-2x" T.govFeat7 ]
                        ]
                    ]
                , div [ class "column is-7 is-6-fullhd p-0 is-fullvh-desktop" ]
                    --[ figure [ class "image is-3by2" ]
                    [ figure [ class "image is-16by9 is-fullwidth shadow", title "Overview page of the Fractale Team." ]
                        [ img [ src "/data/screenshots/f6-overview.png" ] [] ]
                    , figcaption [] [ text "Overview of the organization that builds Fractale. ", a [ href "/o/f6" ] [ text "Explore it" ] ]
                    ]
                ]
            ]
        , div [ id "feature-2", class "hero is-fullheight is-info-2 dub-top-section" ]
            [ div [ class "columns is-vcentered is-centered m-0" ]
                --  pt-6 pb-6
                -- hero-body squeeze the columns !
                --, style "align-items" "flex-start"
                [ div [ class "column is-4 content" ]
                    [ h1 [ class "title" ] [ text T.asyncCom ]
                    , renderMarkdown "subtitle pt-5 pb-4 is-human" T.asyncComAbout
                    , dl [ class "is-icon-aligned" ]
                        [ dt [ class "mb-4" ] [ A.icon1 "icon-exchange icon-2x" T.asyncFeat1 ]
                        , dt [ class "mb-4" ] [ A.icon1 "icon-message-square icon-2x" T.asyncFeat2 ]
                        , dt [ class "mb-4" ] [ A.icon1 "icon-markdown icon-2x" T.asyncFeat3 ]
                        , dt [ class "mb-4" ] [ A.icon1 "icon-mail icon-2x" T.asyncFeat4 ]
                        , dt [ class "mb-4" ] [ A.icon1 "icon-tag icon-2x" T.asyncFeat5 ]
                        , dt [ class "mb-4" ] [ A.icon1 "icon-layout icon-2x" T.asyncFeat6 ]
                        , dt [ class "mb-4" ] [ A.icon1 "icon-search icon-2x" T.asyncFeat7 ]
                        ]
                    ]
                , div [ class "column is-7 is-6-fullhd p-0 is-fullvh-desktop" ]
                    [ figure [ class "image is-2by1 is-fullwidth shadow", title "Tensions page of the Fractale organization, arranged by circles." ]
                        [ img [ src "/data/screenshots/f6-tensions.png" ] [] ]
                    , figcaption [] [ text "Tensions page of the Fractale organization, arranged by circles. ", a [ href "/t/f6?v=circle" ] [ text "Explore it" ] ]
                    ]
                ]
            ]
        ]


viewSignBox : Model -> Html Msg
viewSignBox model =
    div [ class "card" ]
        [ div [ class "card-header" ]
            [ div [ class "card-header-title tabs is-fullwidth p-0" ]
                [ ul []
                    [ li [ classList [ ( "is-active", model.viewMode == Login ) ] ]
                        [ a [ onClickPD (ChangeViewMode Login), target "_blank" ] [ text T.signin_ ] ]
                    , li [ classList [ ( "is-active", model.viewMode == Signup ) ] ]
                        [ a [ onClickPD (ChangeViewMode Signup), target "_blank" ] [ text T.signup_ ] ]
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
                [ case model.result of
                    RemoteData.Failure err ->
                        viewHttpErrors err

                    _ ->
                        text ""
                ]
            ]
        ]


viewLogin : Model -> Html Msg
viewLogin model =
    div []
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
                            , onKeydown SubmitEnter
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
                            , onKeydown SubmitEnter
                            ]
                            []
                        ]
                    ]
                ]
            ]
        , br [] []
        , div [ class "is-size-7 is-pulled-left" ]
            [ span [ class "mr-2" ] [ text T.needAnAccount ]
            , a [ class "underlined-link", onClickPD (ChangeViewMode Signup), target "_blank" ] [ text T.signupNow ]
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


viewSignup : Model -> Html Msg
viewSignup model =
    div []
        [ A.welcome
        , div [ class "subtitle is-size-6 is-strong" ] [ text T.createYourAccount, text ":" ]
        , div [ class "field is-horizntl" ]
            [ div [ class "field-lbl" ] [ label [ class "label" ] [ text T.email ] ]
            , div [ class "field-body" ]
                [ div [ class "field" ]
                    [ div [ class "control" ]
                        [ input
                            [ class "input autofocus followFocus"
                            , attribute "data-nextfocus" "usernameInput2"
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
                            [ id "usernameInput2"
                            , class "input followFocus"
                            , attribute "data-nextfocus" "passwordInput2"
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
                            [ id "passwordInput2"
                            , class "input"
                            , type_ "password"
                            , placeholder "password"
                            , name "password"
                            , value (Dict.get "password" model.form.post |> withDefault "")
                            , attribute "autocomplete" "password"
                            , required True
                            , onInput (ChangeUserPost "password")
                            , onKeydown SubmitEnter
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
            , a [ class "underlined-link", onClickPD (ChangeViewMode Login), target "_blank" ] [ textH T.signinNow ]
            ]
        , div [ class "field is-grouped is-grouped-right" ]
            [ div [ class "control" ]
                [ if isSignupSendable model.form.post then
                    button
                        [ id "submitButton2"
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
