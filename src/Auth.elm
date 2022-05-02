module Auth exposing (ErrState(..), parseErr, parseErr2, refreshAuthModal, signupModal)

import Assets as A
import Components.Loading as Loading exposing (GqlData, RequestResult(..), WebData, errorsDecoder, toErrorData, viewHttpErrors)
import Dict
import Extra.Events exposing (onKeydown)
import Form
import Generated.Route as Route exposing (Route)
import Html exposing (Html, a, br, button, div, i, input, label, p, span, text)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, name, placeholder, required, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelCommon exposing (ModalAuth(..))
import ModelSchema exposing (UserCtx)
import RemoteData exposing (RemoteData)
import String exposing (contains, startsWith)
import String.Extra as SE
import String.Format as Format
import Task
import Text as T exposing (textH, textT, upH)



--
-- Model
--


type ErrState a
    = Authenticate
    | RefreshToken Int
    | OkAuth a
    | NoErr
    | DuplicateErr
    | UnknownErr



--
-- Logics
--


{-| Convert an error message to an error type
-}
messageToErrState : String -> Int -> ErrState a
messageToErrState message_ trial =
    let
        message =
            String.toLower message_
    in
    if contains "token is expired" message || contains "no token found" message then
        Authenticate

    else if startsWith "duplicate error" message then
        DuplicateErr

    else if contains "already exists for field" message then
        DuplicateErr

    else if startsWith "access denied" message || contains "refresh token" message then
        if trial == 0 then
            RefreshToken (trial + 1)

        else
            UnknownErr

    else
        UnknownErr


{-| For GQL Response
-}
parseErr : GqlData a -> Int -> ErrState a
parseErr data trial =
    case data of
        Success d ->
            OkAuth d

        Failure err ->
            if List.length err == 1 then
                case List.head err of
                    Just err_ ->
                        let
                            gqlErr =
                                err_
                                    |> String.replace "\n" ""
                                    |> SE.rightOf "{"
                                    |> SE.insertAt "{" 0
                                    |> JD.decodeString errorsDecoder
                        in
                        case gqlErr of
                            Ok errGql ->
                                case List.head errGql.errors of
                                    Just e ->
                                        messageToErrState e.message trial

                                    Nothing ->
                                        UnknownErr

                            Err errJD ->
                                messageToErrState err_ trial

                    Nothing ->
                        UnknownErr

            else
                UnknownErr

        _ ->
            NoErr


{-| For HTTP response
-}
parseErr2 : WebData a -> Int -> ErrState a
parseErr2 data trial =
    case data of
        RemoteData.Success d ->
            OkAuth d

        RemoteData.Failure error ->
            let
                err =
                    toErrorData error
            in
            if List.length err == 1 then
                case List.head err of
                    Just err_ ->
                        let
                            gqlErr =
                                err_
                                    |> String.replace "\n" ""
                                    |> SE.rightOf "{"
                                    |> SE.insertAt "{" 0
                                    |> JD.decodeString errorsDecoder
                        in
                        case gqlErr of
                            Ok errGql ->
                                case List.head errGql.errors of
                                    Just e ->
                                        messageToErrState e.message trial

                                    Nothing ->
                                        UnknownErr

                            Err errJD ->
                                messageToErrState err_ trial

                    Nothing ->
                        UnknownErr

            else
                UnknownErr

        _ ->
            NoErr



--
-- View
--


refreshAuthModal modalAuth msgs =
    --let
    --    onCloseModal =
    --        if username == "" then
    --            msgs.closeModal (Route.toHref Route.Logout)
    --        else
    --            msgs.closeModal ""
    --in
    div
        [ id "authModal"
        , class "modal modal-pos-top modal-fx-fadeIn"
        , classList [ ( "is-active", modalAuth /= Inactive ) ]
        , attribute "data-modal-close" "closeAuthModalFromJs"
        ]
        [ div [ class "modal-background", onClick <| msgs.closeModal "" ] []
        , div [ class "modal-content" ]
            [ div [ class "has-text-centered" ] [ A.logo2 "#343c3d" ]
            , div [ class "box" ] <|
                case modalAuth of
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
                                        , onInput (msgs.changePost "password")
                                        , onKeydown msgs.submitEnter
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        , div [ attribute "style" "width: 225px;" ]
                            [ a [ class "is-size-7 is-pulled-left", href (Route.toHref Route.PasswordReset) ]
                                [ textH T.passwordForgotten ]
                            ]
                        , div [ class "field is-grouped is-grouped-right" ]
                            [ div [ class "control" ]
                                [ if Form.isPostSendable [ "password" ] form.post then
                                    button
                                        [ id "submitButton"
                                        , class "button is-success"
                                        , onClick (msgs.submit form)
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
        , button [ class "modal-close is-large", onClick <| msgs.closeModal "" ] []
        ]


signupModal modalAuth msgs =
    --let
    --    onCloseModal =
    --        if username == "" then
    --            msgs.closeModal (Route.toHref Route.Logout)
    --        else
    --            msgs.closeModal ""
    --in
    div
        [ id "authModal"
        , class "modal modal-pos-top modal-fx-fadeIn"
        , classList [ ( "is-active", modalAuth /= Inactive ) ]
        , attribute "data-modal-close" "closeAuthModalFromJs"
        ]
        [ div [ class "modal-background", onClick <| msgs.closeModal "" ] []
        , div [ class "modal-content" ]
            [ div [ class "has-text-centered" ] [ A.logo2 "#343c3d" ]
            , div [ class "box" ] <|
                case modalAuth of
                    Active form result ->
                        case result of
                            RemoteData.Success uctx ->
                                [ T.welcomeLetter
                                    |> Format.namedValue "username" uctx.username
                                    |> renderMarkdown "is-human px-3 mt-2"
                                , div [ class "is-aligned-center" ]
                                    [ button [ class "button is-success is-light ", onClick <| msgs.closeModal "" ] [ text "Got it" ]
                                    ]
                                ]

                            _ ->
                                [ p [ class "field" ] [ text "You have been invited to join an organisation on Fractale.", br [] [], text "Please, setup your account:" ]
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
                                                , onInput (msgs.changePost "username")
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
                                                , onInput (msgs.changePost "password")
                                                , onKeydown msgs.submitEnter
                                                ]
                                                []
                                            ]
                                        ]
                                    ]
                                , div [ class "field is-grouped is-grouped-right" ]
                                    [ div [ class "control" ]
                                        [ if Form.isPostSendable [ "username", "password" ] form.post then
                                            button
                                                [ id "submitButton"
                                                , class "button is-success"
                                                , onClick (msgs.submit form)
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
        , button [ class "modal-close is-large", onClick <| msgs.closeModal "" ] []
        ]
