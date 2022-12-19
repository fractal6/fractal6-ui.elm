{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2022 Fractale Co

   This file is part of Fractale.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
-}


module Pages.New.Orga exposing (Flags, Model, Msg, page)

import Assets as A
import Auth exposing (ErrState(..), parseErr2)
import Browser.Navigation as Nav
import Components.AuthModal as AuthModal
import Components.NodeDoc exposing (viewUrlForm)
import Dict exposing (Dict)
import Extra exposing (ternary, textH, upH)
import Extra.Events exposing (onClickPD, onKeydown)
import Extra.Update as Update
import Extra.Url exposing (queryBuilder, queryParser)
import Form exposing (isLoginSendable, isPostSendable)
import Form.Help as Help
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.NodeVisibility as NodeVisibility
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, autocomplete, class, classList, disabled, href, id, name, placeholder, required, rows, target, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
import Http
import Iso8601 exposing (fromTime)
import Loading exposing (GqlData, HttpError(..), RequestResult(..), WebData, withDefaultData, withMapData, withMaybeData, withMaybeDataMap)
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), nameidEncoder, uriFromNameid)
import ModelCommon.Error exposing (viewHttpErrors)
import ModelCommon.View exposing (visibility2descr, visibility2icon)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.QueryNode exposing (getNodeId)
import RemoteData exposing (RemoteData)
import Requests exposing (createOrga)
import Session exposing (Conf, GlobalCmd(..))
import String.Format as Format
import Task exposing (Task)
import Text as T
import Time
import Url as Url exposing (Url)


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
                        ( Cmd.none, send (NavigateRaw link) )

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
    , step : OrgaStep
    , result : WebData NodeId
    , hasDuplicate : Bool
    , hasBeenDuplicate : Bool
    , isWriting : Maybe Bool
    , exist_result : GqlData IdPayload

    -- common
    , help : Help.State
    , refresh_trial : Int
    , authModal : AuthModal.State
    }


type OrgaStep
    = OrgaVisibilityStep
    | OrgaValidateStep


stepEncoder : OrgaStep -> String
stepEncoder menu =
    case menu of
        OrgaVisibilityStep ->
            "0"

        OrgaValidateStep ->
            "1"


stepDecoder : String -> OrgaStep
stepDecoder menu =
    case menu of
        "1" ->
            OrgaValidateStep

        _ ->
            OrgaVisibilityStep


orgaStepToString : OrgaForm -> OrgaStep -> String
orgaStepToString form step =
    case step of
        OrgaVisibilityStep ->
            T.visibility
                ++ (case Dict.get "visibility" form.post of
                        Just visibility ->
                            " (" ++ visibility ++ ")"

                        Nothing ->
                            ""
                   )

        OrgaValidateStep ->
            T.reviewAndValidate


initModel : UserState -> Conf -> Maybe OrgaForm -> Model
initModel user conf form_m =
    { form = withDefault { post = Dict.empty, uctx = uctxFromUser user } form_m
    , step = OrgaVisibilityStep
    , result = RemoteData.NotAsked
    , hasDuplicate = False
    , hasBeenDuplicate = False
    , isWriting = Nothing
    , exist_result = NotAsked
    , help = Help.init user conf
    , refresh_trial = 0
    , authModal = AuthModal.init user Nothing
    }



--
-- Init
--


type alias Flags =
    ()


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        conf =
            { screen = global.session.screen, now = global.now, lang = global.session.lang }

        query =
            queryParser global.url
    in
    case global.session.user of
        LoggedOut ->
            ( initModel global.session.user conf Nothing
            , Cmd.none
            , send (NavigateRaw "/")
            )

        LoggedIn uctx ->
            ( initModel global.session.user conf global.session.newOrgaData
                |> (\m ->
                        { m
                            | step =
                                Dict.get "step" query |> withDefault [] |> List.head |> withDefault "" |> stepDecoder
                        }
                   )
            , Cmd.none
            , Cmd.none
            )



--
-- Update
--


type Msg
    = Submit (Time.Posix -> Msg) -- Get Current Time
    | SaveData
    | PushOrga OrgaForm
    | OnChangeStep OrgaStep
    | OnSelectVisibility NodeVisibility.NodeVisibility
    | ChangeNodePost String String -- {field value}
    | CheckExist
    | CheckExistAck (GqlData NodeId)
    | SubmitOrga OrgaForm Time.Posix -- Send form
    | OrgaAck (WebData NodeId)
      -- Common
    | NoMsg
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
        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        SaveData ->
            ( model, Cmd.none, send (UpdateSessionNewOrgaData (Just model.form)) )

        PushOrga form ->
            ( model, createOrga apis form.post OrgaAck, Cmd.none )

        SubmitOrga form time ->
            ( { model | form = model.form, result = RemoteData.Loading, isWriting = Nothing }
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
                    ( { model | result = result }
                    , Cmd.none
                    , Cmd.batch
                        [ send UpdateUserToken
                        , send (UpdateSessionOrgs Nothing)
                        , send (UpdateSessionNewOrgaData Nothing)
                        , sendSleep (NavigateRaw (uriFromNameid OverviewBaseUri n.nameid [])) 500
                        ]
                    )

                DuplicateErr ->
                    ( { model
                        | result = RemoteData.Failure (BadBody T.duplicateNameError)
                        , hasDuplicate = True
                        , hasBeenDuplicate = True
                      }
                    , Cmd.none
                    , Cmd.none
                    )

                _ ->
                    ( { model | result = result }, Cmd.none, Cmd.none )

        OnChangeStep step ->
            let
                url =
                    toHref Route.New_Orga ++ "?" ++ queryBuilder [ ( "step", stepEncoder step ) ]
            in
            ( { model | step = step }
            , send SaveData
            , sendSleep (NavigateRaw url) 333
            )

        OnSelectVisibility visibility ->
            ( model
            , Cmd.batch
                [ send (ChangeNodePost "visibility" (NodeVisibility.toString visibility))
                , send (OnChangeStep OrgaValidateStep)
                ]
            , Cmd.none
            )

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
                    ( { model | hasDuplicate = True, hasBeenDuplicate = True }, Cmd.none, Cmd.none )

                _ ->
                    ( { model | hasDuplicate = False }, Cmd.none, Cmd.none )

        -- Common
        NoMsg ->
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
                                if Tuple.first o then
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
    { title = "Create your organisation"
    , body =
        [ view_ global model
        , Help.view {} model.help |> Html.map HelpMsg
        , AuthModal.view {} model.authModal |> Html.map AuthModalMsg
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ id "createOrga", class "columns is-centered top-section" ]
        [ div [ class "column is-4-fullhd is-5-desktop" ]
            [ h1 [ class "title has-text-centered" ] [ text T.createYourOrganisation ]
            , viewBreadcrumb model
            , case model.step of
                OrgaVisibilityStep ->
                    viewOrgaVisibility model

                OrgaValidateStep ->
                    viewOrgaValidate model
            ]
        ]


viewBreadcrumb : Model -> Html Msg
viewBreadcrumb model =
    let
        path =
            [ OrgaVisibilityStep, OrgaValidateStep ]
    in
    nav [ class "breadcrumb has-succeeds-separator is-small", attribute "aria-labels" "breadcrumbs" ]
        [ ul [] <|
            List.map
                (\x ->
                    li [ classList [ ( "is-active", x == model.step ) ] ] [ a [ onClickPD NoMsg, target "_blank" ] [ text (orgaStepToString model.form x) ] ]
                )
                path
        ]


viewOrgaVisibility : Model -> Html Msg
viewOrgaVisibility model =
    let
        form =
            model.form
    in
    div [ class "content" ]
        [ div [ class "subtitle" ] [ text T.organisationVisibility ]

        -- Show the choices as card.
        , NodeVisibility.list
            |> List.filter (\x -> x /= NodeVisibility.Secret)
            |> List.map
                (\x ->
                    let
                        isActive =
                            Just x == NodeVisibility.fromString (Dict.get "visibility" form.post |> withDefault "")
                    in
                    div
                        [ class "card has-border column is-paddingless m-3 is-h"
                        , classList [ ( "is-selected is-selectable", isActive ) ]

                        -- @debug: onCLick here do not work sometimes (for the 2nd element of the list ???
                        ]
                        [ div [ class "card-content p-4", onClick (OnSelectVisibility x) ]
                            [ h2 [ class "is-strong is-size-5 mb-5" ] [ A.icon1 (visibility2icon x ++ " icon-bg") (NodeVisibility.toString x) ]
                            , div [ class "content is-smaller2" ] [ text (visibility2descr x) ]
                            ]
                        ]
                )
            |> div [ class "columns" ]
        ]


viewOrgaValidate : Model -> Html Msg
viewOrgaValidate model =
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
            [ div [ class "label" ] [ text T.name ]
            , div [ class "control" ]
                [ input
                    [ class "input autofocus followFocus"
                    , attribute "data-nextfocus" "aboutField"
                    , autocomplete False
                    , type_ "text"
                    , placeholder T.name
                    , value name
                    , onInput <| ChangeNodePost "name"
                    , onBlur SaveData
                    , required True
                    ]
                    []
                , p [ class "help" ] [ text T.orgaNameHelp ]
                ]
            , if model.hasDuplicate || model.hasBeenDuplicate then
                div [ class "mt-3" ]
                    [ viewUrlForm (Dict.get "nameid" post) (ChangeNodePost "nameid") model.hasDuplicate ]

              else
                text ""
            , if model.hasDuplicate then
                let
                    nid =
                        Dict.get "nameid" post |> withDefault ""

                    username =
                        model.form.uctx.username
                in
                div [ class "f6-error message is-danger is-light is-small mt-1" ]
                    [ p [ class "message-body" ]
                        [ text T.duplicateNameError
                        , p [ class "is-hint mt-2" ] [ renderMarkdown "is-light" (T.duplicateOrgHint |> Format.value nid |> Format.value username) ]
                        ]
                    ]

              else
                text ""
            ]
        , div [ class "field" ]
            [ div [ class "label" ] [ text T.about ]
            , div [ class "control" ]
                [ input
                    [ id "aboutField"
                    , class "input followFocus"
                    , attribute "data-nextfocus" "textAreaModal"
                    , autocomplete False
                    , type_ "text"
                    , placeholder T.aboutOpt
                    , value about
                    , onInput <| ChangeNodePost "about"
                    , onBlur SaveData
                    ]
                    []
                ]
            , p [ class "help" ] [ text T.aboutHelp ]
            ]
        , div [ class "field" ]
            [ div [ class "label" ] [ text T.purpose ]
            , div [ class "control" ]
                [ textarea
                    [ id "textAreaModal"
                    , class "textarea"
                    , rows 5
                    , placeholder T.purpose
                    , value purpose
                    , onInput <| ChangeNodePost "purpose"
                    , onBlur SaveData
                    , required True
                    ]
                    []
                ]
            , p [ class "help" ] [ text T.purposeHelpOrga ]
            ]
        , div [ class "field pt-3 level is-mobile" ]
            [ div [ class "level-left" ]
                [ button [ class "button", onClick <| OnChangeStep OrgaVisibilityStep ]
                    [ A.icon0 "icon-chevron-left", text T.back ]
                ]
            , div [ class "level-right" ]
                [ div [ class "buttons" ]
                    [ button
                        ([ class "button has-text-weight-semibold"
                         , classList [ ( "is-success", isSendable ), ( "is-loading", isLoading ) ]
                         , disabled (not isSendable)
                         ]
                            ++ submitOrga
                        )
                        [ text T.create ]
                    ]
                ]
            ]
        , case model.result of
            RemoteData.Failure err ->
                viewHttpErrors err

            _ ->
                text ""
        ]
