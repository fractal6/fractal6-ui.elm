module Pages.Explore exposing (Flags, Model, Msg, page)

import Auth exposing (AuthState(..), doRefreshToken, refreshAuthModal)
import Browser.Navigation as Nav
import Codecs exposing (QuickDoc)
import Components.Fa as Fa
import Components.Help as Help exposing (FeedbackType, Help, HelpTab)
import Components.HelperBar as HelperBar
import Components.Loading as Loading exposing (GqlData, RequestResult(..), WebData, viewAuthNeeded, viewGqlErrors, viewHttpErrors)
import Date exposing (formatTime)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Form exposing (isPostSendable)
import Fractal.Enum.NodeType as NodeType
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, type_)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), uriFromNameid)
import ModelCommon.Requests exposing (getQuickDoc, login)
import ModelCommon.View exposing (getAvatar)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.AddTension exposing (addOneTension)
import Query.QueryNode exposing (NodeExt, queryNodeExt, queryPublicOrga)
import RemoteData exposing (RemoteData)
import Task
import Text as T
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



---- MODEL----


type alias Model =
    { orgas : GqlData (List Node)

    -- Common
    , modalAuth : ModalAuth
    , help : Help
    , refresh_trial : Int
    }


type alias Node =
    NodeExt



---- MSG ----


type Msg
    = PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
    | PushTension TensionForm (GqlData Tension -> Msg)
    | LoadNodes
    | GotOrga (GqlData (List Node))
      -- Token refresh
    | DoOpenAuthModal UserCtx -- ports receive / Open  modal
    | DoCloseAuthModal -- ports receive / Close modal
    | ChangeAuthPost String String
    | SubmitUser UserAuthForm
    | GotSignin (WebData UserCtx)
    | SubmitKeyDown Int -- Detect Enter (for form sending)
      -- Common
    | Navigate String
    | DoOpenModal -- ports receive / Open  modal
    | DoCloseModal String -- ports receive / Close modal
      -- Help
    | TriggerHelp String
    | GotQuickDoc (WebData QuickDoc)
    | ChangeHelpTab HelpTab
    | ChangePostAsk String String
    | ChangePostFeedback String String
    | ChangeFeedbackLabel FeedbackType
    | SubmitAsk Time.Posix
    | SubmitFeedback Time.Posix
    | AskAck (GqlData Tension)
    | AskFeedback (GqlData Tension)
    | DoCloseHelpModal String



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
            , help = Help.create global.session.user
            , refresh_trial = 0
            }

        cmds =
            [ send LoadNodes
            , sendSleep PassedSlowLoadTreshold 500
            ]
    in
    ( model
    , Cmd.batch cmds
    , Cmd.none
    )



---- UPDATE ----


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global msg model =
    let
        apis =
            global.session.apis
    in
    case msg of
        PushTension form ack ->
            ( model, addOneTension apis.gql form ack, Cmd.none )

        LoadNodes ->
            ( model, queryPublicOrga apis.gql GotOrga, Cmd.none )

        PassedSlowLoadTreshold ->
            let
                orgas =
                    ternary (model.orgas == Loading) LoadingSlowly model.orgas
            in
            ( { model | orgas = orgas }, Cmd.none, Cmd.none )

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        -- Gql queries
        GotOrga result ->
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
                    , Cmd.batch [ send DoCloseAuthModal, send LoadNodes ]
                    , send (UpdateUserSession uctx)
                    )

                other ->
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
        Navigate url ->
            ( model, Cmd.none, Nav.pushUrl global.key url )

        DoOpenModal ->
            ( model, Cmd.none, Ports.open_modal )

        DoCloseModal link ->
            let
                gcmd =
                    if link /= "" then
                        send (Navigate link)

                    else
                        Cmd.none
            in
            ( model, gcmd, Ports.close_modal )

        -- Help
        TriggerHelp _ ->
            ( { model | help = Help.open model.help }
            , Cmd.batch [ Ports.open_modal, getQuickDoc apis.data "en" GotQuickDoc ]
            , Cmd.none
            )

        GotQuickDoc result ->
            ( { model | help = Help.setDocResult result model.help }, Cmd.none, Cmd.none )

        ChangeHelpTab tab ->
            ( { model | help = Help.changeTab tab model.help }, Cmd.none, Cmd.none )

        ChangePostAsk field value ->
            ( { model | help = Help.postAsk field value model.help }, Cmd.none, Cmd.none )

        ChangePostFeedback field value ->
            ( { model | help = Help.postFeedback field value model.help }, Cmd.none, Cmd.none )

        ChangeFeedbackLabel type_ ->
            ( { model | help = Help.changeLabel type_ model.help }, Cmd.none, Cmd.none )

        SubmitAsk time ->
            let
                help =
                    model.help
                        |> Help.postAsk "createdAt" (fromTime time)
                        |> Help.setResultAsk LoadingSlowly
            in
            ( { model | help = help }
            , send (PushTension help.formAsk AskAck)
            , Cmd.none
            )

        SubmitFeedback time ->
            let
                help =
                    model.help
                        |> Help.postFeedback "createdAt" (fromTime time)
                        |> Help.setLabelsFeedback
                        |> Help.setResultFeedback LoadingSlowly
            in
            ( { model | help = help }
            , send (PushTension help.formFeedback AskFeedback)
            , Cmd.none
            )

        AskAck result ->
            let
                form =
                    model.help.formAsk
            in
            case doRefreshToken result model.refresh_trial of
                Authenticate ->
                    ( { model | help = Help.setResultAsk NotAsked model.help }, send (DoOpenAuthModal form.uctx), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep (PushTension form AskAck) 500, send UpdateUserToken )

                OkAuth tension ->
                    ( { model | help = Help.setResultAsk result model.help }, Cmd.none, Cmd.none )

                NoAuth ->
                    ( { model | help = Help.setResultAsk result model.help }, Cmd.none, Cmd.none )

        AskFeedback result ->
            let
                form =
                    model.help.formFeedback
            in
            case doRefreshToken result model.refresh_trial of
                Authenticate ->
                    ( { model | help = Help.setResultFeedback NotAsked model.help }, send (DoOpenAuthModal form.uctx), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep (PushTension form AskFeedback) 500, send UpdateUserToken )

                OkAuth tension ->
                    ( { model | help = Help.setResultFeedback result model.help }, Cmd.none, Cmd.none )

                NoAuth ->
                    ( { model | help = Help.setResultFeedback result model.help }, Cmd.none, Cmd.none )

        DoCloseHelpModal link ->
            let
                gcmd =
                    if link /= "" then
                        send (Navigate link)

                    else
                        Cmd.none
            in
            ( { model | help = Help.close model.help }, gcmd, Ports.close_modal )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.batch
        [ Ports.closeModalFromJs DoCloseModal
        , Ports.triggerHelpFromJs TriggerHelp
        ]



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Explore"
    , body =
        [ view_ global model
        , Help.view
            { data = model.help
            , onSubmit = Submit
            , onCloseModal = DoCloseHelpModal
            , onNavigate = Navigate
            , onChangeTab = ChangeHelpTab
            , onChangePostAsk = ChangePostAsk
            , onChangePostFeedback = ChangePostFeedback
            , onChangeLabel = ChangeFeedbackLabel
            , onSubmitAsk = SubmitAsk
            , onSubmitFeedback = SubmitFeedback
            }
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
                [ viewPublicOrgas model
                ]
            ]
        ]


viewPublicOrgas : Model -> Html Msg
viewPublicOrgas model =
    div [ class "" ] <|
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
                nodes
                    |> List.map (\n -> viewOrgaMedia n)
                    |> List.append [ div [ class "subtitle" ] [ text T.exploreOrganisations ], br [] [] ]


viewOrgaMedia : Node -> Html Msg
viewOrgaMedia node =
    let
        n_member =
            node.stats |> Maybe.map (\s -> s.n_member |> withDefault 0) |> withDefault 0 |> String.fromInt

        n_guest =
            node.stats |> Maybe.map (\s -> s.n_guest |> withDefault 0) |> withDefault 0 |> String.fromInt
    in
    div [ class "media box nodesList" ]
        [ div [ class "media-left" ]
            [ a
                [ class "image circleBase circle2"
                , href (uriFromNameid OverviewBaseUri node.nameid)
                ]
                [ getAvatar node.name ]
            ]
        , div [ class "media-content" ]
            [ div [ class "columns" ]
                [ div [ class "column is-8" ]
                    [ a [ href (uriFromNameid OverviewBaseUri node.nameid) ] [ text node.name ]
                    , case node.about of
                        Just ab ->
                            p [ class "is-italic pt-1" ] [ text ab ]

                        Nothing ->
                            text ""
                    ]
                , span [ class "column is-4" ]
                    [ div [ class "field is-grouped is-grouped-multiline is-pulled-right" ]
                        [ div [ class "control" ]
                            [ div [ class "tags has-addons" ]
                                [ span [ class "tag is-light" ] [ text "member" ]
                                , span [ class "tag is-white" ] [ text n_member ]
                                ]
                            ]
                        , div [ class "control" ]
                            [ div [ class "tags has-addons" ]
                                [ span [ class "tag is-light" ] [ text "guest" ]
                                , span [ class "tag is-white" ] [ text n_guest ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
