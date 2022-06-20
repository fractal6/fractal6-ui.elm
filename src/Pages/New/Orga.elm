module Pages.New.Orga exposing (Flags, Model, Msg, page)

import Auth exposing (ErrState(..), parseErr2)
import Browser.Navigation as Nav
import Components.AuthModal as AuthModal
import Components.Loading as Loading exposing (GqlData, HttpError(..), RequestResult(..), WebData, viewHttpErrors, withDefaultData, withMapData, withMaybeData, withMaybeDataMap)
import Components.NodeDoc exposing (viewUrlForm)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Extra.Events exposing (onKeydown)
import Form exposing (isLoginSendable, isPostSendable)
import Form.Help as Help
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
import ModelCommon.Codecs exposing (FractalBaseRoute(..), nameidEncoder, uriFromNameid)
import ModelCommon.Requests exposing (createOrga, login)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.QueryNode exposing (getNodeId)
import RemoteData exposing (RemoteData)
import Session exposing (GlobalCmd(..))
import Task
import Text as T exposing (textH, textT, upH)
import Time
import Url as Url


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

                    DoReplaceUrl url ->
                        ( Cmd.none, send (ReplaceUrl url) )

                    DoUpdateToken ->
                        ( Cmd.none, send UpdateUserToken )

                    DoUpdateUserSession uctx ->
                        ( Cmd.none, send (UpdateUserSession uctx) )

                    _ ->
                        ( Cmd.none, Cmd.none )
            )
        |> List.unzip



--
-- Model
--


type alias Model =
    { form : OrgaForm
    , result : WebData NodeId
    , isDuplicate : Bool
    , hasBeenDuplicated : Bool
    , isWriting : Maybe Bool
    , exist_result : GqlData IdPayload

    -- common
    , help : Help.State
    , refresh_trial : Int
    , authModal : AuthModal.State
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
              , isDuplicate = False
              , hasBeenDuplicated = False
              , isWriting = Nothing
              , exist_result = NotAsked
              , help = Help.init global.session.user
              , refresh_trial = 0
              , authModal = AuthModal.init global.session.user Nothing
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
              , isDuplicate = False
              , hasBeenDuplicated = False
              , isWriting = Nothing
              , exist_result = NotAsked

              --common
              , help = Help.init global.session.user
              , refresh_trial = 0
              , authModal = AuthModal.init global.session.user Nothing
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
    | CheckExist
    | CheckExistAck (GqlData { nameid : String })
    | SubmitOrga OrgaForm Time.Posix -- Send form
    | OrgaAck (WebData NodeId)
      -- Common
    | Navigate String
      -- Help
    | HelpMsg Help.Msg
    | AuthModalMsg AuthModal.Msg


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global message model =
    let
        apis =
            global.session.apis
    in
    case message of
        PushOrga form ->
            ( model, createOrga apis form.post OrgaAck, Cmd.none )

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        SubmitOrga form time ->
            ( { model | form = model.form, result = RemoteData.Loading }
            , send (PushOrga form)
            , Cmd.none
            )

        OrgaAck result ->
            case parseErr2 result model.refresh_trial of
                Authenticate ->
                    ( { model | result = RemoteData.NotAsked }, Ports.raiseAuthModal model.form.uctx, Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep (PushOrga model.form) 500, send UpdateUserToken )

                OkAuth n ->
                    ( { model | result = result, isDuplicate = False }
                    , sendSleep (Navigate (uriFromNameid OverviewBaseUri n.nameid)) 500
                    , Cmd.batch [ send UpdateUserToken, send (UpdateSessionOrgs Nothing) ]
                    )

                DuplicateErr ->
                    ( { model
                        | result = RemoteData.Failure (BadBody "Duplicate error: this name (URL) is already taken.")
                        , isDuplicate = True
                      }
                    , Cmd.none
                    , Cmd.none
                    )

                _ ->
                    ( { model | result = result, isDuplicate = False }, Cmd.none, Cmd.none )

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
                                        |> Dict.insert "nameid" (nameidEncoder value)
                            }

                        "nameid" ->
                            { f | post = Dict.insert field (nameidEncoder value) f.post }

                        _ ->
                            { f | post = Dict.insert field value f.post }

                ( isWriting, cmd ) =
                    if List.member field [ "name", "nameid" ] then
                        if model.isWriting == Nothing then
                            ( Just False, sendSleep CheckExist 1000 )

                        else
                            ( Just True, Cmd.none )

                    else
                        ( model.isWriting, Cmd.none )
            in
            ( { model | form = newForm, isWriting = isWriting }, cmd, Cmd.none )

        CheckExist ->
            case model.isWriting of
                Just False ->
                    case Dict.get "nameid" model.form.post of
                        Just nameid ->
                            ( { model | isWriting = Nothing }, getNodeId apis nameid CheckExistAck, Cmd.none )

                        Nothing ->
                            ( { model | isWriting = Nothing }, Cmd.none, Cmd.none )

                Just True ->
                    ( { model | isWriting = Just False }, sendSleep CheckExist 1000, Cmd.none )

                Nothing ->
                    ( model, Cmd.none, Cmd.none )

        CheckExistAck result ->
            case result of
                Success _ ->
                    ( { model | isDuplicate = True, hasBeenDuplicated = True }, Cmd.none, Cmd.none )

                _ ->
                    ( { model | isDuplicate = False }, Cmd.none, Cmd.none )

        -- Common
        Navigate url ->
            ( model, Cmd.none, Nav.pushUrl global.key url )

        -- Help
        HelpMsg msg ->
            let
                ( help, out ) =
                    Help.update apis msg model.help

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | help = help }, out.cmds |> List.map (\m -> Cmd.map HelpMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        AuthModalMsg msg ->
            let
                ( data, out ) =
                    AuthModal.update apis msg model.authModal

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds

                -- reload silently the page if needed
                cmds_extra =
                    out.result
                        |> Maybe.map
                            (\o ->
                                if Tuple.first o == True then
                                    [ Nav.replaceUrl global.key (Url.toString global.url) ]

                                else
                                    []
                            )
                        |> withDefault []
            in
            ( { model | authModal = data }, out.cmds |> List.map (\m -> Cmd.map AuthModalMsg m) |> List.append (cmds ++ cmds_extra) |> Cmd.batch, Cmd.batch gcmds )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        ++ (AuthModal.subscriptions |> List.map (\s -> Sub.map AuthModalMsg s))
        |> Sub.batch


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Login"
    , body =
        [ view_ global model
        , Help.view {} model.help |> Html.map HelpMsg
        , AuthModal.view {} model.authModal |> Html.map AuthModalMsg
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ id "createOrga", class "columns is-centered section" ]
        [ div [ class "column is-4-fullhd is-5-desktop" ]
            [ h1 [ class "title has-text-centered" ] [ text "Create your organisation" ]
            , viewOrgaForm global model
            ]
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
                    , type_ "text"
                    , placeholder (upH T.name)
                    , value name
                    , onInput <| ChangeNodePost "name"
                    , required True
                    ]
                    []
                , if model.hasBeenDuplicated then
                    viewUrlForm (Dict.get "nameid" post) (ChangeNodePost "nameid") model.isDuplicate

                  else
                    text ""
                ]
            , p [ class "help" ] [ textH T.orgaNameHelp ]
            , if model.isDuplicate then
                div [ class "has-text-danger" ] [ text "This name (URL) is already taken." ]

              else
                text ""
            ]
        , div [ class "field" ]
            [ div [ class "label" ] [ textH T.about ]
            , div [ class "control" ]
                [ input
                    [ id "aboutField"
                    , class "input followFocus"
                    , attribute "data-nextfocus" "textAreaModal"
                    , autocomplete False
                    , type_ "text"
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
