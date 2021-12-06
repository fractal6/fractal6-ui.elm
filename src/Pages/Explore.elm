module Pages.Explore exposing (Flags, Model, Msg, page)

import Auth exposing (ErrState(..), parseErr, refreshAuthModal)
import Browser.Navigation as Nav
import Codecs exposing (QuickDoc)
import Components.HelperBar as HelperBar
import Components.Loading as Loading exposing (GqlData, ModalData, RequestResult(..), WebData, viewAuthNeeded, viewGqlErrors, viewHttpErrors)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Form exposing (isPostSendable)
import Form.Help as Help
import Fractal.Enum.NodeType as NodeType
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, type_)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Icon as I
import Iso8601 exposing (fromTime)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), uriFromNameid)
import ModelCommon.Requests exposing (getQuickDoc, login)
import ModelCommon.View exposing (viewOrgaMedia)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.QueryNode exposing (queryPublicOrga)
import RemoteData exposing (RemoteData)
import Session exposing (GlobalCmd(..))
import Task
import Text as T exposing (textH, textT)
import Time



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

                    _ ->
                        ( Cmd.none, Cmd.none )
            )
        |> List.unzip



---- MODEL----


type alias Model =
    { orgas : GqlData (List NodeExt)

    -- Common
    , modalAuth : ModalAuth
    , help : Help.State
    , refresh_trial : Int
    }



---- MSG ----


type Msg
    = PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
    | LoadNodeExts
    | GotOrgas (GqlData (List NodeExt))
      -- Token refresh
    | DoOpenAuthModal UserCtx
    | DoCloseAuthModal String
    | ChangeAuthPost String String
    | SubmitUser UserAuthForm
    | GotSignin (WebData UserCtx)
    | SubmitKeyDown Int -- Detect Enter (for form sending)
      -- Common
    | NoMsg
    | LogErr String
    | Navigate String
    | DoOpenModal
    | DoCloseModal ModalData
      -- Help
    | HelpMsg Help.Msg



-- INIT --


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        apis =
            global.session.apis

        model =
            { orgas = Loading

            -- common
            , modalAuth = Inactive
            , help = Help.init global.session.user
            , refresh_trial = 0
            }

        cmds =
            [ send LoadNodeExts
            , sendSleep PassedSlowLoadTreshold 500
            ]
    in
    ( model
    , Cmd.batch cmds
    , send (UpdateSessionFocus Nothing)
    )



---- UPDATE ----


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global message model =
    let
        apis =
            global.session.apis
    in
    case message of
        LoadNodeExts ->
            ( model, queryPublicOrga apis.gql GotOrgas, Cmd.none )

        PassedSlowLoadTreshold ->
            let
                orgas =
                    ternary (model.orgas == Loading) LoadingSlowly model.orgas
            in
            ( { model | orgas = orgas }, Cmd.none, Cmd.none )

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        -- Gql queries
        GotOrgas result ->
            ( { model | orgas = result }, Cmd.none, Cmd.none )

        -- refresh token
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

        DoCloseAuthModal link ->
            let
                cmd =
                    ternary (link /= "") (send (Navigate link)) Cmd.none
            in
            ( { model | modalAuth = Inactive }, cmd, Ports.close_auth_modal )

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
                    , Cmd.batch [ send (DoCloseAuthModal ""), send LoadNodeExts ]
                    , send (UpdateUserSession uctx)
                    )

                _ ->
                    case model.modalAuth of
                        Active form ->
                            ( { model | modalAuth = Active { form | result = result } }, Cmd.none, Cmd.none )

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

        -- Common
        NoMsg ->
            ( model, Cmd.none, Cmd.none )

        LogErr err ->
            ( model, Ports.logErr err, Cmd.none )

        Navigate url ->
            ( model, Cmd.none, Nav.pushUrl global.key url )

        DoOpenModal ->
            ( model, Ports.open_modal "actionModal", Cmd.none )

        DoCloseModal data ->
            let
                gcmd =
                    if data.link /= "" then
                        send (Navigate data.link)

                    else
                        Cmd.none
            in
            ( model, gcmd, Ports.close_modal )

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
subscriptions _ _ =
    [ Ports.mcPD Ports.closeModalFromJs LogErr DoCloseModal
    ]
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        |> Sub.batch



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Explore"
    , body =
        [ view_ global model
        , Help.view {} model.help |> Html.map HelpMsg
        , case model.modalAuth of
            -- @debug: should not be necessary...
            Active _ ->
                refreshAuthModal model.modalAuth { closeModal = DoCloseAuthModal, changePost = ChangeAuthPost, submit = SubmitUser, submitEnter = SubmitKeyDown }

            Inactive ->
                text ""
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ id "explore", class "section" ]
        [ div [ class "columns is-centered" ]
            [ div [ class "column is-7" ]
                [ viewPublicOrgas global.session.user model ]
            ]
        ]


viewPublicOrgas : UserState -> Model -> Html Msg
viewPublicOrgas user model =
    div []
        [ h1 [ class "subtitle" ] [ textH T.exploreOrganisations ]

        --, div  br [] []
        , div [ class "nodesList" ] <|
            case model.orgas of
                Loading ->
                    [ text "" ]

                NotAsked ->
                    [ text "" ]

                LoadingSlowly ->
                    [ div [ class "spinner" ] [] ]

                Failure err ->
                    [ viewGqlErrors err ]

                Success nodes ->
                    List.map (\n -> viewOrgaMedia user n) nodes
        ]
