module Auth exposing (AuthState(..), doRefreshToken, doRefreshToken2, refreshAuthModal)

import Components.Loading as Loading exposing (GqlData, RequestResult(..), WebData, errorDecoder, toErrorData, viewHttpErrors)
import Components.Markdown exposing (renderMarkdown)
import Extra.Events exposing (onKeydown)
import Form
import Html exposing (Html, a, br, button, div, i, input, label, p, span, text)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, name, placeholder, required, type_)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Maybe exposing (withDefault)
import ModelCommon exposing (ModalAuth(..))
import ModelSchema exposing (UserCtx)
import RemoteData exposing (RemoteData)
import String.Extra as SE
import Task


type AuthState a
    = Authenticate
    | RefreshToken Int
    | OkAuth a
    | NoAuth


{-|

    For GQL Response

-}
doRefreshToken : GqlData a -> Int -> AuthState a
doRefreshToken data trial =
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
                                    |> JD.decodeString errorDecoder
                        in
                        case gqlErr of
                            Ok errGql ->
                                case List.head errGql.errors of
                                    Just e ->
                                        if e.message == "token is expired" then
                                            Authenticate

                                        else if e.message == "Access denied" then
                                            if trial == 0 then
                                                RefreshToken (trial + 1)

                                            else
                                                NoAuth

                                        else
                                            NoAuth

                                    Nothing ->
                                        NoAuth

                            Err errJD ->
                                NoAuth

                    Nothing ->
                        NoAuth

            else
                NoAuth

        _ ->
            NoAuth


{-|

    For HTTP response

-}
doRefreshToken2 : WebData a -> Int -> AuthState a
doRefreshToken2 data trial =
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
                                    |> JD.decodeString errorDecoder
                        in
                        case gqlErr of
                            Ok errGql ->
                                case List.head errGql.errors of
                                    Just e ->
                                        if e.message == "token is expired" then
                                            Authenticate

                                        else if e.message == "Access denied" then
                                            if trial == 0 then
                                                RefreshToken (trial + 1)

                                            else
                                                NoAuth

                                        else
                                            NoAuth

                                    Nothing ->
                                        NoAuth

                            Err errJD ->
                                NoAuth

                    Nothing ->
                        NoAuth

            else
                NoAuth

        _ ->
            NoAuth



--
-- View
--


refreshAuthModal modalAuth msgs =
    let
        form_m =
            case modalAuth of
                Active f ->
                    Just f

                Inactive ->
                    Nothing
    in
    div
        [ id "refreshAuthModal"
        , class "modal modal2 modal-pos-top modal-fx-fadeIn elmModal"
        , classList [ ( "is-active", form_m /= Nothing ) ]
        ]
        [ div [ class "modal-background", onClick msgs.closeModal ] []
        , div [ class "modal-content", classList [] ]
            [ div [ class "box" ]
                [ p [] [ text "Your session expired. Please, confirm your password:" ]
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
                                    , attribute "autocomplete" "password"
                                    , required True
                                    , onInput (msgs.changePost "password")
                                    , onKeydown msgs.submitEnter
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                , div [ class "field is-grouped is-grouped-right" ]
                    [ div [ class "control" ]
                        [ case form_m of
                            Just form ->
                                if Form.isPostSendable [ "password" ] form.post then
                                    button
                                        [ id "submitButton"
                                        , class "button is-success has-text-weight-semibold"
                                        , onClick (msgs.submit form)
                                        ]
                                        [ text "Refresh" ]

                                else
                                    button [ class "button has-text-weight-semibold", disabled True ]
                                        [ text "Refresh" ]

                            Nothing ->
                                div [] []
                        ]
                    ]
                , div []
                    [ case form_m |> Maybe.map (\f -> f.result) |> withDefault RemoteData.NotAsked of
                        RemoteData.Failure err ->
                            viewHttpErrors err

                        default ->
                            text ""
                    ]
                ]
            ]
        , button [ class "modal-close is-large", onClick msgs.closeModal ] []
        ]
