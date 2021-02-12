module Pages.New.Orga exposing (Flags, Model, Msg, page)

import Auth exposing (AuthState(..), doRefreshToken2, refreshAuthModal)
import Browser.Navigation as Nav
import Components.Help as Help
import Components.Loading as Loading exposing (GqlData, RequestResult(..), WebData, viewHttpErrors, withDefaultData, withMapData, withMaybeData, withMaybeDataMap)
import Components.NodeDoc exposing (makeNewNodeId)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Extra.Events exposing (onKeydown)
import Form exposing (isLoginSendable, isPostSendable)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (Route)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, autocomplete, class, classList, disabled, href, id, name, placeholder, required, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Iso8601 exposing (fromTime)
import Json.Decode as JD
import Json.Encode as JE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), uriFromNameid)
import ModelCommon.Requests exposing (createOrga, login)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import RemoteData exposing (RemoteData)
import Task
import Text as T exposing (textH, textT, upH)
import Time


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


mapGlobalOutcmds : List GlobalCmd -> ( List (Cmd Msg), List (Cmd Global.Msg) )
mapGlobalOutcmds gcmds =
    gcmds
        |> List.map
            (\m ->
                case m of
                    DoNavigate link ->
                        ( send (Navigate link), Cmd.none )

                    DoAuth uctx ->
                        ( send (DoOpenAuthModal uctx), Cmd.none )

                    DoUpdateToken ->
                        ( Cmd.none, send UpdateUserToken )
            )
        |> List.unzip



--
-- Model
--


type alias Model =
    { form : OrgaForm
    , result : WebData NodeId

    -- common
    , isModalActive : Bool -- Only use by JoinOrga for now. (other actions rely on Bulma drivers)
    , modalAuth : ModalAuth
    , help : Help.State
    , refresh_trial : Int
    }


type alias OrgaForm =
    { uctx : UserCtx
    , post : Post
    }



--
-- Init
--


type alias Flags =
    ()


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    case global.session.user of
        LoggedOut ->
            let
                form =
                    { post = Dict.empty, uctx = initUserctx }
            in
            ( { form = form
              , result = RemoteData.NotAsked
              , isModalActive = False
              , modalAuth = Inactive
              , help = Help.init global.session.user
              , refresh_trial = 0
              }
            , send (Navigate "/")
            , Cmd.none
            )

        LoggedIn uctx ->
            let
                form =
                    { post = Dict.empty, uctx = uctx }
            in
            ( { form = form
              , result = RemoteData.NotAsked

              --common
              , isModalActive = False
              , modalAuth = Inactive
              , help = Help.init global.session.user
              , refresh_trial = 0
              }
            , Cmd.none
            , Cmd.none
            )



--
-- Update
--


type Msg
    = Submit (Time.Posix -> Msg) -- Get Current Time
    | PushOrga OrgaForm
    | ChangeNodePost String String -- {field value}
    | SubmitOrga OrgaForm Time.Posix -- Send form
    | OrgaAck (WebData NodeId)
      -- Token refresh
    | DoOpenAuthModal UserCtx -- ports receive / Open  modal
    | DoCloseAuthModal -- ports receive / Close modal
    | ChangeAuthPost String String
    | SubmitUser UserAuthForm
    | GotSignin (WebData UserCtx)
    | SubmitKeyDown Int -- Detect Enter (for form sending)
      -- Common
    | Navigate String
    | DoCloseModal String -- ports receive / Close modal
      -- Help
    | HelpMsg Help.Msg


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global message model =
    let
        apis =
            global.session.apis
    in
    case message of
        PushOrga form ->
            ( model, createOrga apis.auth form.post OrgaAck, Cmd.none )

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        SubmitOrga form time ->
            ( { model | form = model.form, result = RemoteData.Loading }
            , send (PushOrga form)
            , Cmd.none
            )

        OrgaAck result ->
            case doRefreshToken2 result model.refresh_trial of
                Authenticate ->
                    ( { model | result = RemoteData.NotAsked }, send (DoOpenAuthModal model.form.uctx), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep (PushOrga model.form) 500, send UpdateUserToken )

                OkAuth n ->
                    ( { model | result = result }
                    , send (Navigate (uriFromNameid OverviewBaseUri n.nameid))
                    , send UpdateUserToken
                    )

                NoAuth ->
                    ( { model | result = result }, Cmd.none, Cmd.none )

        ChangeNodePost field value ->
            let
                f =
                    model.form

                newForm =
                    case field of
                        "name" ->
                            { f
                                | post =
                                    f.post
                                        |> Dict.insert field value
                                        |> Dict.insert "nameid" (makeNewNodeId value)
                            }

                        _ ->
                            { f | post = Dict.insert field value f.post }
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

        DoOpenAuthModal uctx ->
            ( { model
                | modalAuth =
                    Active
                        { post = Dict.fromList [ ( "username", uctx.username ) ]
                        , result = RemoteData.NotAsked
                        }
              }
            , Cmd.none
            , Ports.open_auth_modal
            )

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

        -- Help
        HelpMsg msg ->
            let
                ( help, out ) =
                    Help.update apis msg model.help

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | help = help }, out.cmds |> List.map (\m -> Cmd.map HelpMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        |> Sub.batch


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Login"
    , body =
        [ view_ global model
        , Help.view {} model.help |> Html.map HelpMsg
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ id "createOrga", class "columns is-centered section" ]
        [ div [ class "column is-4-fullhd is-5-desktop" ]
            [ h1 [ class "title " ] [ text "Create your organisation" ]
            , viewOrgaForm global model
            ]
        , refreshAuthModal model.modalAuth { closeModal = DoCloseAuthModal, changePost = ChangeAuthPost, submit = SubmitUser, submitEnter = SubmitKeyDown }
        ]


viewOrgaForm : Global.Model -> Model -> Html Msg
viewOrgaForm global model =
    let
        post =
            model.form.post

        isLoading =
            model.result == RemoteData.Loading

        isSendable =
            isPostSendable [ "name", "purpose" ] post

        submitOrga =
            ternary isSendable [ onClick (Submit <| SubmitOrga model.form) ] []

        --
        name =
            Dict.get "name" post |> withDefault ""

        about =
            Dict.get "about" post |> withDefault ""

        purpose =
            Dict.get "purpose" post |> withDefault ""
    in
    div []
        [ div [ class "field" ]
            [ div [ class "label" ] [ textH T.name ]
            , div [ class "control" ]
                [ input
                    [ class "input autofocus followFocus"
                    , attribute "data-nextfocus" "aboutField"
                    , autocomplete False
                    , type_ "search"
                    , placeholder (upH T.name)
                    , value name
                    , onInput <| ChangeNodePost "name"
                    , required True
                    ]
                    []
                ]
            , p [ class "help" ] [ textH T.orgaNameHelp ]
            ]
        , div [ class "field" ]
            [ div [ class "label" ] [ textH T.about ]
            , div [ class "control" ]
                [ input
                    [ id "aboutField"
                    , class "input followFocus"
                    , attribute "data-nextfocus" "textAreaModal"
                    , autocomplete False
                    , type_ "search"
                    , placeholder (upH T.aboutOpt)
                    , value about
                    , onInput <| ChangeNodePost "about"
                    ]
                    []
                ]
            , p [ class "help" ] [ textH T.aboutHelp ]
            ]
        , div [ class "field" ]
            [ div [ class "label" ] [ textH T.purpose ]
            , div [ class "control" ]
                [ textarea
                    [ id "textAreaModal"
                    , class "textarea"
                    , rows 5
                    , placeholder (upH T.purpose)
                    , value purpose
                    , onInput <| ChangeNodePost "purpose"
                    , required True
                    ]
                    []
                ]
            , p [ class "help" ] [ textH T.purposeHelpOrga ]
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
                        [ textH T.create ]
                    ]
                ]
            ]
        , case model.result of
            RemoteData.Failure err ->
                viewHttpErrors err

            _ ->
                text ""
        ]
