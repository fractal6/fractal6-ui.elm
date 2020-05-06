module Pages.Signup exposing (Flags, Model, Msg, page)

import Dict exposing (Dict)
import Global
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, autofocus, class, classList, disabled, href, id, placeholder, rows, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelOrg exposing (..)
import Page exposing (Document, Page)
import RemoteData exposing (RemoteData)


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { form : UserForm
    }


type alias UserForm =
    { post : Dict String String
    , result : WebData UserCtx
    }


type alias WebData a =
    RemoteData Http.Error a


userDecoder : JD.Decoder UserCtx
userDecoder =
    JD.map3 UserCtx
        (JD.field "username" JD.string)
        (JD.maybe (JD.field "name" JD.string))
        (JD.field "roles" <|
            JD.list <|
                JD.map2 UserRole
                    (JD.field "nameid" JD.string)
                    (JD.field "role_type" JD.string)
        )



--
-- Init
--


type alias Flags =
    ()


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        model =
            { form =
                { post = Dict.empty
                , result = RemoteData.NotAsked
                }
            }
    in
    ( model, Cmd.none, Cmd.none )



--
-- Update
--


type Msg
    = SubmitUser UserForm
    | ChangeUserPost String String
    | GotSignin (WebData UserCtx) -- use remotedata.


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
            , Http.post
                { url = "http://localhost:8888/signup"
                , body = Http.jsonBody <| JE.dict identity JE.string form.post
                , expect = Http.expectJson (RemoteData.fromResult >> GotSignin) userDecoder
                }
            , Cmd.none
            )

        GotSignin res ->
            let
                form =
                    model.form

                formUpdated =
                    { form | result = res }
            in
            ( { model | form = formUpdated }, Cmd.none, Cmd.none )


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
        [ div [ class "column is-two-fifths" ]
            [ viewLogin global model ]
        ]


viewLogin : Global.Model -> Model -> Html Msg
viewLogin global model =
    let
        isSendable =
            isUserSendable model.form
    in
    div [ class "card" ]
        [ div [ class "card-header" ]
            [ div [ class "card-header-title" ]
                [ text "Sign up" ]
            ]
        , div [ class "card-content" ]
            [ div [ class "field is-horizontal" ]
                [ div [ class "field-label is-normal" ] [ label [ class "label" ] [ text "Username" ] ]
                , div [ class "field-body" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ input
                                [ class "input is-small autofocus followFocus"
                                , attribute "data-nextfocus" "emailInput"
                                , type_ "text"
                                , placeholder "username"
                                , onInput (ChangeUserPost "username")
                                ]
                                []
                            ]
                        ]
                    ]
                ]
            , div [ class "field is-horizontal" ]
                [ div [ class "field-label is-normal" ] [ label [ class "label" ] [ text "Email" ] ]
                , div [ class "field-body" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ input
                                [ id "emailInput"
                                , class "input is-small followFocus"
                                , attribute "data-nextfocus" "passwordInput"
                                , type_ "text"
                                , placeholder "email"
                                , onInput (ChangeUserPost "email")
                                ]
                                []
                            ]
                        ]
                    ]
                ]
            , div [ class "field is-horizontal" ]
                [ div [ class "field-label is-normal" ] [ label [ class "label" ] [ text "Password" ] ]
                , div [ class "field-body" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ input
                                [ id "passwordInput"
                                , class "input is-small followFocus"
                                , attribute "data-nextfocus" "submitButton"
                                , type_ "text"
                                , placeholder "password"
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
                            [ text "Sign up" ]

                      else
                        button [ class "button has-text-weight-semibold", disabled True ]
                            [ text "Sign up" ]
                    ]
                ]
            ]
        ]



--- Getters


isUserSendable : UserForm -> Bool
isUserSendable form =
    let
        requiredFields =
            [ Dict.get "username" form.post |> withDefault ""
            , Dict.get "email" form.post |> withDefault ""
            , Dict.get "password" form.post |> withDefault ""
            ]

        isSendable =
            List.all (\x -> String.length x > 1) requiredFields
    in
    isSendable
