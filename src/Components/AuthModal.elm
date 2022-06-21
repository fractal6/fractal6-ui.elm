module Components.AuthModal exposing (Msg(..), State, UserAuthForm, init, signupModal, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Components.Loading as Loading exposing (GqlData, ModalData, RequestResult(..), WebData, isSuccess, viewGqlErrors, viewHttpErrors, withMaybeData)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Extra.Events exposing (onClickPD, onKeydown)
import Form exposing (isPostEmpty, isPostSendable)
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, strong, text, textarea, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelCommon exposing (UserState(..), uctxFromUser)
import ModelCommon.Requests exposing (login, signupValidate)
import ModelSchema exposing (..)
import Ports
import RemoteData exposing (RemoteData)
import Session exposing (Apis, GlobalCmd(..))
import String.Format as Format
import Text as T exposing (textH, textT, upH)
import Time


type State
    = State Model


type alias Model =
    { user : UserState
    , modalAuth : ModalAuth
    , refreshAfter : Bool
    , puid : Maybe String
    }


initModel : UserState -> Maybe String -> Model
initModel user puid =
    { user = user
    , modalAuth = Inactive
    , refreshAfter = False
    , puid = puid
    }


type alias UserAuthForm =
    { post : Dict String String
    }


type ModalAuth
    = Inactive
    | Active UserAuthForm (WebData UserCtx)


init : UserState -> Maybe String -> State
init user puid =
    initModel user puid |> State



-- Global methods
-- ...
--- State Controls
-- ...
-- utils
-- ...
-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = -- Token refresh
      OnStart
    | DoOpenAuthModal Bool UserCtx
    | DoOpenSignupModal String
    | DoCloseAuthModal String
    | ChangeAuthPost String String
    | SubmitUser UserAuthForm
    | GotSignin (WebData UserCtx)
    | SubmitKeyDown Int -- Detect Enter (for form sending)
      -- Common
    | NoMsg
    | LogErr String


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe ( Bool, UserCtx ) -- define what data is to be returned
    }


noOut : Out
noOut =
    Out [] [] Nothing


out0 : List (Cmd Msg) -> Out
out0 cmds =
    Out cmds [] Nothing


out1 : List GlobalCmd -> Out
out1 cmds =
    Out [] cmds Nothing


out2 : List (Cmd Msg) -> List GlobalCmd -> Out
out2 cmds gcmds =
    Out cmds gcmds Nothing


update : Apis -> Msg -> State -> ( State, Out )
update apis message (State model) =
    update_ apis message model
        |> Tuple.mapFirst State


update_ apis message model =
    case message of
        -- Token Refresh
        OnStart ->
            case model.puid of
                Just puid ->
                    case model.user of
                        LoggedIn _ ->
                            -- @future: manage multiple loggin:
                            -- * Check that the puid match the user.
                            ( model, noOut )

                        LoggedOut ->
                            ( model, out0 [ send (DoOpenSignupModal puid) ] )

                Nothing ->
                    ( model, noOut )

        DoOpenAuthModal refresh uctx ->
            case model.user of
                -- @DEBUG/codefactor: pass directly {user} to DoOpenAuthModal !
                LoggedIn _ ->
                    if model.modalAuth /= Inactive then
                        -- already open, pass
                        ( model, noOut )

                    else if uctx.username == "" then
                        ( model, out0 [ send (DoCloseAuthModal (toHref Route.Logout)) ] )

                    else
                        ( { model
                            | modalAuth = Active { post = Dict.fromList [ ( "username", uctx.username ) ] } RemoteData.NotAsked
                            , refreshAfter = refresh
                          }
                        , out0 [ Ports.open_auth_modal ]
                        )

                LoggedOut ->
                    ( model, noOut )

        DoOpenSignupModal puid ->
            ( { model
                | puid = Just puid
                , modalAuth = Active { post = Dict.fromList [ ( "puid", puid ) ] } RemoteData.NotAsked
                , refreshAfter = True
              }
            , out0 [ Ports.open_auth_modal ]
            )

        DoCloseAuthModal link ->
            let
                ret =
                    case model.modalAuth of
                        Active _ result ->
                            case result of
                                RemoteData.Success uctx ->
                                    Just ( model.refreshAfter, uctx )

                                _ ->
                                    Nothing

                        _ ->
                            Nothing
            in
            ( { model | modalAuth = Inactive }
            , Out [ Ports.close_auth_modal ] (ternary (link /= "") [ DoNavigate link ] []) ret
            )

        ChangeAuthPost field value ->
            case model.modalAuth of
                Active form r ->
                    let
                        newForm =
                            { form | post = Dict.insert field value form.post }
                    in
                    ( { model | modalAuth = Active newForm r }, noOut )

                Inactive ->
                    ( model, noOut )

        SubmitUser form ->
            case model.puid of
                Just puid ->
                    ( model, out0 [ signupValidate apis form.post GotSignin ] )

                Nothing ->
                    ( model, out0 [ login apis form.post GotSignin ] )

        GotSignin result ->
            case model.modalAuth of
                Active form _ ->
                    case result of
                        RemoteData.Success uctx ->
                            case model.puid of
                                Just _ ->
                                    ( { model | modalAuth = Active form result }
                                    , Out [] [ DoUpdateUserSession uctx ] (Just ( False, uctx ))
                                    )

                                Nothing ->
                                    ( { model | modalAuth = Inactive }
                                    , Out [ send (DoCloseAuthModal "") ] [ DoUpdateUserSession uctx ] (Just ( model.refreshAfter, uctx ))
                                    )

                        _ ->
                            ( { model | modalAuth = Active form result }, noOut )

                Inactive ->
                    ( model, noOut )

        SubmitKeyDown key ->
            case key of
                13 ->
                    let
                        form =
                            case model.modalAuth of
                                Active f _ ->
                                    f

                                Inactive ->
                                    UserAuthForm Dict.empty
                    in
                    --ENTER
                    if isPostSendable [ "password" ] form.post then
                        ( model, out0 [ send (SubmitUser form) ] )

                    else
                        ( model, noOut )

                _ ->
                    ( model, noOut )

        -- Common
        NoMsg ->
            ( model, noOut )

        LogErr err ->
            ( model, out0 [ Ports.logErr err ] )


subscriptions : List (Sub Msg)
subscriptions =
    [ Ports.uctxPD2 Ports.openAuthModalFromJs LogErr DoOpenAuthModal
    ]



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    {}


view : Op -> State -> Html Msg
view op (State model) =
    div []
        [ case model.puid of
            Just _ ->
                Lazy.lazy2 signupModal op model

            Nothing ->
                Lazy.lazy2 refreshModal op model
        ]


refreshModal : Op -> Model -> Html Msg
refreshModal op model =
    div
        [ id "authModal"
        , class "modal modal-pos-top modal-fx-fadeIn"
        , classList [ ( "is-active", model.modalAuth /= Inactive ) ]
        , attribute "data-modal-close" "closeAuthModalFromJs"
        ]
        [ div [ class "modal-background", onClick <| DoCloseAuthModal "" ] []
        , div [ class "modal-content" ]
            [ div [ class "has-text-centered" ] [ A.logo2 "#343c3d" ]
            , div [ class "box" ] <|
                case model.modalAuth of
                    Active form result ->
                        [ p [ class "field" ] [ text "Your session has expired. Please, confirm your password:" ]
                        , div [ class "field" ]
                            [ div [ class "field" ]
                                [ div [ class "control" ]
                                    [ input
                                        [ class "input is-disabled"
                                        , type_ "username"
                                        , name "username"
                                        , value (Dict.get "username" form.post |> withDefault "")
                                        , disabled True
                                        ]
                                        []
                                    ]
                                ]
                            , div [ class "field" ]
                                [ div [ class "control" ]
                                    [ input
                                        [ id "passwordInput"
                                        , class "input"
                                        , type_ "password"
                                        , placeholder "password"
                                        , name "password"
                                        , attribute "autocomplete" "password"
                                        , required True
                                        , onInput (ChangeAuthPost "password")
                                        , onKeydown SubmitKeyDown
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        , div [ attribute "style" "width: 225px;" ]
                            [ a [ class "is-size-7 is-pulled-left", href (toHref Route.PasswordReset) ]
                                [ textH T.passwordForgotten ]
                            ]
                        , div [ class "field is-grouped is-grouped-right" ]
                            [ div [ class "control" ]
                                [ if isPostSendable [ "password" ] form.post then
                                    button
                                        [ id "submitButton"
                                        , class "button is-success"
                                        , onClick (SubmitUser form)
                                        ]
                                        [ text "Refresh" ]

                                  else
                                    button [ class "button", disabled True ]
                                        [ text "Refresh" ]
                                ]
                            ]
                        , div []
                            [ case result of
                                RemoteData.Failure err ->
                                    viewHttpErrors err

                                _ ->
                                    text ""
                            ]
                        ]

                    Inactive ->
                        []
            ]
        , button [ class "modal-close is-large", onClick <| DoCloseAuthModal "" ] []
        ]


signupModal : Op -> Model -> Html Msg
signupModal op model =
    div
        [ id "authModal"
        , class "modal modal-pos-top modal-fx-fadeIn"
        , classList [ ( "is-active", model.modalAuth /= Inactive ) ]
        , attribute "data-modal-close" "closeAuthModalFromJs"
        ]
        [ div [ class "modal-background", onClick <| DoCloseAuthModal "" ] []
        , div [ class "modal-content" ]
            [ div [ class "has-text-centered" ] [ A.logo2 "#343c3d" ]
            , div [ class "box" ] <|
                case model.modalAuth of
                    Active form result ->
                        case result of
                            RemoteData.Success uctx ->
                                [ div [ class "px-3 mt-2 mb-2" ]
                                    [ T.welcomeLetter
                                        |> Format.namedValue "username" uctx.username
                                        |> renderMarkdown "is-human"
                                    ]
                                , div [ class "is-aligned-center" ]
                                    [ button [ class "button is-success is-light ", onClick <| DoCloseAuthModal "" ] [ text "Got it" ]
                                    ]
                                ]

                            _ ->
                                [ p [ class "field" ]
                                    [ text "You have been invited to join an organisation on "
                                    , strong [] [ text "Fractale" ]
                                    , text ". "
                                    , br [ class "mb-1" ] []
                                    , text "But first, you need to "
                                    , strong [] [ text "setup your account:" ]
                                    ]
                                , div [ class "field" ]
                                    [ div [ class "field" ]
                                        [ div [ class "label" ] [ text "Username" ]
                                        , div [ class "control" ]
                                            [ input
                                                [ class "input autofocus followFocus"
                                                , attribute "data-nextfocus" "passwordInput"
                                                , type_ "username"
                                                , placeholder "username"
                                                , name "username"
                                                , required True
                                                , onInput (ChangeAuthPost "username")
                                                , value (Dict.get "username" form.post |> withDefault "")
                                                ]
                                                []
                                            ]
                                        ]
                                    , div [ class "field" ]
                                        [ div [ class "label" ] [ text "Password" ]
                                        , div [ class "control" ]
                                            [ input
                                                [ id "passwordInput"
                                                , class "input"
                                                , type_ "password"
                                                , placeholder "password"
                                                , name "password"
                                                , required True
                                                , onInput (ChangeAuthPost "password")
                                                , onKeydown SubmitKeyDown
                                                ]
                                                []
                                            ]
                                        ]
                                    ]
                                , div [ class "field is-grouped is-grouped-right" ]
                                    [ div [ class "control" ]
                                        [ if isPostSendable [ "username", "password" ] form.post then
                                            button
                                                [ id "submitButton"
                                                , class "button is-success"
                                                , onClick (SubmitUser form)
                                                ]
                                                [ textH T.confirm ]

                                          else
                                            button [ class "button", disabled True ]
                                                [ textH T.confirm ]
                                        ]
                                    ]
                                , div []
                                    [ case result of
                                        RemoteData.Failure err ->
                                            viewHttpErrors err

                                        _ ->
                                            text ""
                                    ]
                                ]

                    Inactive ->
                        []
            ]
        , button [ class "modal-close is-large", onClick <| DoCloseAuthModal "" ] []
        ]
