module Pages.New.Orga exposing (Flags, Model, Msg, page)

import Browser.Navigation as Nav
import Components.Loading as Loading exposing (GqlData, RequestResult(..), WebData, viewAuthNeeded, viewGqlErrors, viewHttpErrors, viewWarnings)
import Components.NodeDoc exposing (updateNodeForm)
import Components.Text as T
import Dict exposing (Dict)
import Extra exposing (ternary, withDefaultData, withMapData, withMaybeData, withMaybeDataMap)
import Extra.Events exposing (onKeydown)
import Form exposing (isLoginSendable, isPostSendable)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (Route)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, name, placeholder, required, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Iso8601 exposing (fromTime)
import Json.Decode as JD
import Json.Encode as JE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Requests exposing (login)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.AddTension exposing (addOneOrga)
import RemoteData exposing (RemoteData)
import Task
import Time


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
    { form : TensionForm
    , result : GqlData RootNode

    -- common
    , isModalActive : Bool -- Only use by JoinOrga for now. (other actions rely on Bulma drivers)
    , modalAuth : ModalAuth
    }



--
-- Init
--


type alias Flags =
    ()


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        f_ =
            initTensionForm
                { rootnameid = "origin"
                , nameid = "origin"
                , isRoot = True
                , type_ = NodeType.Circle
                }

        f =
            { f_
                | status = TensionStatus.Closed
                , tension_type = TensionType.Governance
            }

        ( form, cmd ) =
            case global.session.user of
                LoggedOut ->
                    ( f, send (Navigate "/") )

                LoggedIn uctx ->
                    ( { f | uctx = uctx }, Cmd.none )

        model =
            { form = form
            , result = NotAsked

            --common
            , isModalActive = True
            , modalAuth = Inactive
            }
    in
    ( model, cmd, Cmd.none )



--
-- Update
--


type Msg
    = Submit (Time.Posix -> Msg) -- Get Current Time
    | ChangeNodePost String String -- {field value}
    | SubmitTension TensionForm Time.Posix -- Send form
    | OrgaAck (GqlData RootNode)
      -- Common
    | Navigate String
    | DoCloseModal String -- ports receive / Close modal
    | DoCloseAuthModal -- ports receive / Close modal
    | ChangeAuthPost String String
    | SubmitUser UserAuthForm
    | GotSignin (WebData UserCtx)
    | SubmitKeyDown Int -- Detect Enter (for form sending)


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global msg model =
    let
        apis =
            global.session.apis
    in
    case msg of
        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        SubmitTension form time ->
            let
                newForm =
                    { form
                        | post = Dict.insert "createdAt" (fromTime time) form.post
                        , events_type = Just [ TensionEvent.Created, TensionEvent.BlobCreated, TensionEvent.BlobPushed ]
                    }
            in
            ( { model | form = newForm, result = LoadingSlowly }
            , addOneOrga apis.gql form OrgaAck
            , Cmd.none
            )

        OrgaAck result ->
            ( { model | result = result }, Cmd.none, Cmd.none )

        ChangeNodePost field value ->
            let
                newForm =
                    updateNodeForm field value model.form
            in
            ( { model | form = newForm }, Cmd.none, Cmd.none )

        -- Common
        Navigate url ->
            ( model, Cmd.none, Nav.pushUrl global.key url )

        DoCloseModal link ->
            let
                gcmd =
                    if link /= "" then
                        send (Navigate link)

                    else
                        Cmd.none
            in
            ( { model | isModalActive = False }, gcmd, Ports.close_modal )

        DoCloseAuthModal ->
            ( { model | modalAuth = Inactive }, Cmd.none, Ports.close_auth_modal )

        ChangeAuthPost field value ->
            case model.modalAuth of
                Active form ->
                    let
                        newForm =
                            { form | post = Dict.insert field value form.post }
                    in
                    ( { model | modalAuth = Active newForm }, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        SubmitUser form ->
            ( model, login apis.auth form.post GotSignin, Cmd.none )

        GotSignin result ->
            case result of
                RemoteData.Success uctx ->
                    ( { model | modalAuth = Inactive }
                    , send DoCloseAuthModal
                    , send (UpdateUserSession uctx)
                    )

                other ->
                    case model.modalAuth of
                        Active form ->
                            let
                                newForm =
                                    { form | result = result }
                            in
                            ( { model | modalAuth = Active newForm }, Cmd.none, Cmd.none )

                        Inactive ->
                            ( model, Cmd.none, Cmd.none )

        SubmitKeyDown key ->
            case key of
                13 ->
                    let
                        form =
                            case model.modalAuth of
                                Active f ->
                                    f

                                Inactive ->
                                    UserAuthForm Dict.empty RemoteData.NotAsked
                    in
                    --ENTER
                    if isPostSendable [ "password" ] form.post then
                        ( model, send (SubmitUser form), Cmd.none )

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
    div [ class "columns is-centered section" ]
        [ div [ class "column is-3" ]
            [ div [ class "title" ] [ text "Create organisation" ]
            , viewOrgaForm global model
            ]
        ]


viewOrgaForm : Global.Model -> Model -> Html Msg
viewOrgaForm global model =
    let
        form =
            model.form

        node =
            form.node

        isLoading =
            model.result == LoadingSlowly

        isSendable =
            form.node.name /= Nothing && (form.node.mandate |> Maybe.map (\x -> x.purpose)) /= Nothing

        submitOrga =
            ternary isSendable [ onClick (Submit <| SubmitTension form) ] []

        purpose =
            node.mandate |> Maybe.map (\m -> m.purpose) |> withDefault ""
    in
    div [ id "newOrga", class "form" ]
        [ div [ class "field" ]
            [ div [ class "label" ] [ text T.nameH ]
            , div [ class "control" ]
                [ input
                    [ class "input autofocus followFocus"
                    , attribute "data-nextfocus" "aboutField"
                    , type_ "text"
                    , placeholder "Name*"
                    , value (node.name |> withDefault "")
                    , onInput <| ChangeNodePost "name"
                    , required True
                    ]
                    []
                ]
            , p [ class "help" ] [ text "Name of your organisation" ]
            ]
        , div [ class "field" ]
            [ div [ class "label" ] [ text T.aboutH ]
            , div [ class "control" ]
                [ input
                    [ id "aboutField"
                    , class "input followFocus"
                    , attribute "data-nextfocus" "textAreaModal"
                    , type_ "text"
                    , placeholder "About"
                    , value (node.about |> withDefault "")
                    , onInput <| ChangeNodePost "about"
                    ]
                    []
                ]
            , p [ class "help" ] [ text "Short description" ]
            ]
        , div [ class "field" ]
            [ div [ class "label" ] [ text T.purposeH ]
            , div [ class "control" ]
                [ textarea
                    [ id "textAreaModal"
                    , class "textarea"
                    , rows 5
                    , placeholder (T.purposeH ++ "*")
                    , value purpose
                    , onInput <| ChangeNodePost "purpose"
                    , required True
                    ]
                    []
                ]
            , p [ class "help" ] [ text "Purpose of this organisation" ]
            ]
        , div [ class "field is-grouped is-grouped-right" ]
            [ div [ class "control" ]
                [ div [ class "buttons" ]
                    [ button
                        ([ class "button has-text-weight-semibold"
                         , classList [ ( "is-success", isSendable ), ( "is-loading", isLoading ) ]
                         , disabled (not isSendable)
                         ]
                            ++ submitOrga
                        )
                        [ text "Create organisation" ]
                    ]
                ]
            ]
        , case model.result of
            Success n ->
                div [] [ text n.nameid ]

            Failure err ->
                viewGqlErrors err

            _ ->
                text ""
        ]
