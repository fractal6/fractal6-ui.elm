{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2026 Fractale Co

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


port module Components.ProjectSettingsPanel exposing (Msg(..), State, init, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Fractale.Form exposing (ProjectForm, initProjectForm)
import Fractale.User exposing (uctxFromUser)
import Fractale.Error exposing (viewGqlErrors)
import Fractale.View exposing (viewUserFull)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.UserInput as UserInput
import Dict
import Utils.Cmd exposing (send, sendNow, sendSleep)
import Html exposing (Html, button, div, hr, input, p, span, text, textarea)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, id, placeholder, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import Iso8601 exposing (fromTime)
import Loading exposing (GqlData, ModalData, RequestResult(..))
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelSchema exposing (ProjectData, ProjectFull, Username)
import Ports
import Query.PatchNode exposing (updateOneProject)
import Session exposing (Apis, GlobalCmd(..), SessionCommon)
import Text as T
import Time



-- ------------------------------
-- M O D E L
-- ------------------------------


type State
    = State Model


type alias Model =
    { isOpen : Bool
    , projectid : String
    , nameid : String
    , project : Maybe ProjectData

    -- Edit state
    , editField : Maybe EditField
    , titleDraft : String
    , descriptionDraft : String
    , form : ProjectForm
    , update_result : GqlData ProjectFull

    -- Common
    , session : SessionCommon
    , refresh_trial : Int
    , modal_confirm : ModalConfirm Msg

    -- Components
    , userInput : UserInput.State
    }


type EditField
    = EditTitle
    | EditDescription


initModel : String -> String -> SessionCommon -> Model
initModel projectid nameid session =
    { isOpen = False
    , projectid = projectid
    , nameid = nameid
    , project = Nothing

    -- Edit state
    , editField = Nothing
    , titleDraft = ""
    , descriptionDraft = ""
    , form = initProjectForm session.user nameid
    , update_result = NotAsked

    -- Common
    , session = session
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg

    -- Components
    , userInput = UserInput.init [ nameid ] True True session
    }


init : String -> String -> SessionCommon -> State
init projectid nameid session =
    initModel projectid nameid session |> State



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = -- Panel
      OnOpen ProjectData
    | OnOutsideClickClose
    | OnClose
      -- Edit
    | OnToggleEdit EditField
    | OnCancelEdit
    | OnChangeTitle String
    | OnChangeDescription String
    | Submit (Time.Posix -> Msg)
    | DoSubmitEdit Time.Posix
    | GotUpdateResult (GqlData ProjectFull)
      -- Collaborators
    | OnRemoveCollaborator String
    | DoRemoveCollaborator String Time.Posix
    | GotCollabResult (GqlData ProjectFull)
      -- Permissions
    | OnTogglePeerCanEdit
    | OnToggleGuestCanEdit
    | DoSubmitPermissions Time.Posix
    | GotPermResult (GqlData ProjectFull)
      -- Components
    | UserInputMsg UserInput.Msg
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Common
    | NoMsg
    | LogErr String


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe ProjectData
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


update_ : Apis -> Msg -> Model -> ( Model, Out )
update_ apis message model =
    case message of
        -- Panel
        OnOpen project ->
            ( { model
                | isOpen = True
                , project = Just project
                , titleDraft = project.name
                , descriptionDraft = project.description |> withDefault ""
                , editField = Nothing
                , update_result = NotAsked
                , form = initProjectForm model.session.user model.nameid |> (\f -> { f | id = project.id })
                , userInput = UserInput.init [ model.nameid ] True True model.session
              }
            , out0 [ sendSleep OnOutsideClickClose 500 ]
            )

        OnOutsideClickClose ->
            ( model, out0 [ Ports.outsideClickClose "closeProjectSettingsPanelFromJs" "projectSettingsPanel" True ] )

        OnClose ->
            ( { model | isOpen = False, editField = Nothing, update_result = NotAsked }, out0 [ Ports.click "" ] )

        -- Edit
        OnToggleEdit field ->
            case model.project of
                Just p ->
                    ( { model
                        | editField = Just field
                        , titleDraft = p.name
                        , descriptionDraft = p.description |> withDefault ""
                      }
                    , noOut
                    )

                Nothing ->
                    ( model, noOut )

        OnCancelEdit ->
            ( { model | editField = Nothing }, noOut )

        OnChangeTitle val ->
            ( { model | titleDraft = val }, noOut )

        OnChangeDescription val ->
            ( { model | descriptionDraft = val }, noOut )

        Submit nextMsg ->
            ( model, out0 [ sendNow nextMsg ] )

        DoSubmitEdit time ->
            case model.project of
                Just p ->
                    let
                        form =
                            model.form

                        newForm =
                            { form
                                | post =
                                    Dict.fromList
                                        ([ ( "updatedAt", fromTime time ) ]
                                            ++ (if model.titleDraft /= p.name then
                                                    [ ( "name", model.titleDraft ), ( "old_name", p.name ) ]

                                                else
                                                    []
                                               )
                                            ++ (if model.descriptionDraft /= (p.description |> withDefault "") then
                                                    [ ( "description", model.descriptionDraft ) ]

                                                else
                                                    []
                                               )
                                        )
                            }
                    in
                    ( { model | update_result = LoadingSlowly, form = newForm }
                    , out0 [ updateOneProject apis newForm GotUpdateResult ]
                    )

                Nothing ->
                    ( model, noOut )

        GotUpdateResult result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | update_result = NotAsked }
                    , out0 [ Ports.raiseAuthModal (uctxFromUser model.session.user) ]
                    )

                RefreshToken i ->
                    ( { model | refresh_trial = i }
                    , out2 [ sendSleep (Submit DoSubmitEdit) 500 ] [ DoUpdateToken ]
                    )

                OkAuth projectFull ->
                    let
                        updatedProject =
                            model.project
                                |> Maybe.map
                                    (\p ->
                                        { p
                                            | name = projectFull.name
                                            , description = projectFull.description
                                            , collaborators = projectFull.collaborators
                                            , peerCanEditProject = projectFull.peerCanEditProject
                                            , guestCanEditProject = projectFull.guestCanEditProject
                                        }
                                    )
                    in
                    ( { model
                        | update_result = result
                        , project = updatedProject
                        , editField = Nothing
                      }
                    , Out [] [] updatedProject
                    )

                _ ->
                    ( { model | update_result = result }, noOut )

        -- Collaborators
        OnRemoveCollaborator username ->
            ( model
            , out0 [ sendNow (DoRemoveCollaborator username) ]
            )

        DoRemoveCollaborator username time ->
            let
                form =
                    model.form

                newForm =
                    { form
                        | post = Dict.fromList [ ( "updatedAt", fromTime time ) ]
                        , collaborators_remove = [ username ]
                        , collaborators_add = []
                    }
            in
            ( { model | update_result = LoadingSlowly, form = newForm }
            , out0 [ updateOneProject apis newForm GotCollabResult ]
            )

        GotCollabResult result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | update_result = NotAsked }
                    , out0 [ Ports.raiseAuthModal (uctxFromUser model.session.user) ]
                    )

                RefreshToken i ->
                    let
                        retryUsername =
                            List.head model.form.collaborators_remove |> withDefault ""
                    in
                    ( { model | refresh_trial = i }
                    , out2 [ sendSleep (Submit (DoRemoveCollaborator retryUsername)) 500 ] [ DoUpdateToken ]
                    )

                OkAuth projectFull ->
                    let
                        updatedProject =
                            model.project
                                |> Maybe.map (\p -> { p | collaborators = projectFull.collaborators })
                    in
                    ( { model
                        | update_result = result
                        , project = updatedProject
                        , userInput = UserInput.init [ model.nameid ] True True model.session
                      }
                    , Out [] [] updatedProject
                    )

                _ ->
                    ( { model | update_result = result }, noOut )

        -- Permissions
        OnTogglePeerCanEdit ->
            let
                newProject =
                    model.project
                        |> Maybe.map (\p -> { p | peerCanEditProject = not p.peerCanEditProject })
            in
            ( { model | project = newProject }
            , out0 [ send (Submit DoSubmitPermissions) ]
            )

        OnToggleGuestCanEdit ->
            let
                newProject =
                    model.project
                        |> Maybe.map
                            (\p ->
                                if not p.guestCanEditProject then
                                    { p | guestCanEditProject = True, peerCanEditProject = True }

                                else
                                    { p | guestCanEditProject = False }
                            )
            in
            ( { model | project = newProject }
            , out0 [ send (Submit DoSubmitPermissions) ]
            )

        DoSubmitPermissions time ->
            case model.project of
                Just p ->
                    let
                        form =
                            model.form

                        newForm =
                            { form
                                | post = Dict.fromList [ ( "updatedAt", fromTime time ) ]
                                , peerCanEditProject = Just p.peerCanEditProject
                                , guestCanEditProject = Just p.guestCanEditProject
                            }
                    in
                    ( { model | update_result = LoadingSlowly, form = newForm }
                    , out0 [ updateOneProject apis newForm GotPermResult ]
                    )

                Nothing ->
                    ( model, noOut )

        GotPermResult result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | update_result = NotAsked }
                    , out0 [ Ports.raiseAuthModal (uctxFromUser model.session.user) ]
                    )

                RefreshToken i ->
                    ( { model | refresh_trial = i }
                    , out2 [ sendSleep (Submit DoSubmitPermissions) 500 ] [ DoUpdateToken ]
                    )

                OkAuth projectFull ->
                    let
                        updatedProject =
                            model.project
                                |> Maybe.map
                                    (\p ->
                                        { p
                                            | peerCanEditProject = projectFull.peerCanEditProject
                                            , guestCanEditProject = projectFull.guestCanEditProject
                                        }
                                    )
                    in
                    ( { model
                        | update_result = result
                        , project = updatedProject
                      }
                    , Out [] [] updatedProject
                    )

                _ ->
                    ( { model | update_result = result }, noOut )

        -- Components
        UserInputMsg msg ->
            let
                ( data, out ) =
                    UserInput.update apis msg model.userInput

                addCmd =
                    case out.result of
                        Just ( True, users ) ->
                            let
                                existingCollabs =
                                    model.project
                                        |> Maybe.map (.collaborators >> List.map .username)
                                        |> withDefault []
                            in
                            users
                                |> List.filter (\u -> not (List.member u.username existingCollabs))
                                |> List.map
                                    (\u ->
                                        let
                                            form =
                                                model.form

                                            newForm =
                                                { form
                                                    | collaborators_add = [ u.username ]
                                                    , collaborators_remove = []
                                                }
                                        in
                                        updateOneProject apis newForm GotCollabResult
                                    )

                        _ ->
                            []
            in
            ( { model
                | userInput = data
                , update_result =
                    if addCmd /= [] then
                        LoadingSlowly

                    else
                        model.update_result
              }
            , out2 (out.cmds |> List.map (\m -> Cmd.map UserInputMsg m) |> List.append addCmd) out.gcmds
            )

        -- Confirm Modal
        DoModalConfirmOpen msg mess ->
            ( { model | modal_confirm = ModalConfirm.open msg mess model.modal_confirm }, noOut )

        DoModalConfirmClose _ ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, noOut )

        DoModalConfirmSend ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, out0 [ send model.modal_confirm.msg ] )

        -- Common
        NoMsg ->
            ( model, noOut )

        LogErr err ->
            ( model, out0 [ Ports.logErr err ] )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    if model.isOpen then
        [ Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
        , closeProjectSettingsPanelFromJs (always OnClose)
        ]
            ++ (UserInput.subscriptions model.userInput |> List.map (\s -> Sub.map UserInputMsg s))

    else
        []



-- ------------------------------
-- V I E W
-- ------------------------------


view : State -> Html Msg
view (State model) =
    div
        [ id "projectSettingsPanel"
        , class "side-menu is-small"
        , classList [ ( "off", not model.isOpen ) ]
        ]
        [ viewPanel model
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        ]


viewPanel : Model -> Html Msg
viewPanel model =
    case model.project of
        Just project ->
            div [ class "panel" ]
                [ div [ class "header-block" ]
                    [ div [ class "panel-heading" ]
                        [ text T.projectSettings
                        , button [ class "delete is-pulled-right", onClick OnClose ] []
                        ]
                    ]
                , div [ class "main-block p-4" ]
                    [ viewTitleSection model project
                    , viewDescriptionSection model project
                    , viewCollaboratorsSection model project
                    , viewPermissionsSection project
                    ]
                , case model.update_result of
                    Failure err ->
                        div [ class "p-4" ] [ viewGqlErrors err ]

                    LoadingSlowly ->
                        div [ class "p-4" ] [ div [ class "spinner" ] [] ]

                    _ ->
                        text ""
                ]

        Nothing ->
            text ""


viewTitleSection : Model -> ProjectData -> Html Msg
viewTitleSection model project =
    div [ class "mb-4" ]
        [ case model.editField of
            Just EditTitle ->
                div [ class "field" ]
                    [ p [ class "help is-size-7 mb-1" ] [ text T.name ]
                    , div [ class "control" ]
                        [ input
                            [ class "input"
                            , type_ "text"
                            , value model.titleDraft
                            , onInput OnChangeTitle
                            , placeholder T.name
                            ]
                            []
                        ]
                    , div [ class "buttons mt-3" ]
                        [ button
                            [ class "button is-success is-small"
                            , disabled (model.titleDraft == "" || model.titleDraft == project.name)
                            , onClick (Submit DoSubmitEdit)
                            ]
                            [ text T.save ]
                        , button [ class "button is-small", onClick OnCancelEdit ] [ text T.cancel ]
                        ]
                    ]

            _ ->
                div [ class "is-flex is-align-items-center is-justify-content-space-between" ]
                    [ div []
                        [ p [ class "help is-size-7 mb-1" ] [ text T.name ]
                        , span [ class "is-size-5 has-text-weight-semibold" ] [ text project.name ]
                        ]
                    , span [ class "button-light", onClick (OnToggleEdit EditTitle) ]
                        [ A.icon "icon-edit-2" ]
                    ]
        ]


viewDescriptionSection : Model -> ProjectData -> Html Msg
viewDescriptionSection model project =
    div [ class "mb-5" ]
        [ case model.editField of
            Just EditDescription ->
                div [ class "field" ]
                    [ p [ class "help is-size-7 mb-1" ] [ text T.description ]
                    , div [ class "control" ]
                        [ textarea
                            [ class "textarea"
                            , rows 3
                            , value model.descriptionDraft
                            , onInput OnChangeDescription
                            , placeholder T.description
                            ]
                            []
                        ]
                    , div [ class "buttons mt-3" ]
                        [ button
                            [ class "button is-success is-small"
                            , disabled (withDefault "" project.description == model.descriptionDraft)
                            , onClick (Submit DoSubmitEdit)
                            ]
                            [ text T.save ]
                        , button [ class "button is-small", onClick OnCancelEdit ] [ text T.cancel ]
                        ]
                    ]

            _ ->
                div [ class "is-flex is-align-items-center is-justify-content-space-between" ]
                    [ div []
                        [ p [ class "help is-size-7 mb-1" ] [ text T.description ]
                        , case project.description of
                            Just desc ->
                                renderMarkdown "" "is-human" desc

                            Nothing ->
                                span [] [ text "—" ]
                        ]
                    , span [ class "button-light", onClick (OnToggleEdit EditDescription) ]
                        [ A.icon "icon-edit-2" ]
                    ]
        ]


viewCollaboratorsSection : Model -> ProjectData -> Html Msg
viewCollaboratorsSection model project =
    div [ class "mt-6", attribute "style" "border-top: 1px solid var(--border-color-light);" ]
        [ p [ class "help mt-6 is-size-7 mb-2" ] [ text T.collaborators ]
        , div []
            (if List.isEmpty project.collaborators then
                [ p [ class "is-italic" ] [ text "—" ] ]

             else
                List.map viewCollaboratorTag project.collaborators
            )
        , UserInput.view { label_text = text "", showEmail = False, placeholder_text = Just T.addACollaborator } model.userInput |> Html.map UserInputMsg
        ]


viewCollaboratorTag : Username -> Html Msg
viewCollaboratorTag user =
    div [ class "tagsinput tags has-addons m-0 mr-2 mb-2" ]
        [ span [ class "tag is-rounded" ]
            [ viewUserFull 0 False False { username = user.username, name = Nothing } ]
        , span
            [ class "tag is-delete is-rounded"
            , onClick (OnRemoveCollaborator user.username)
            ]
            []
        ]


viewPermissionsSection : ProjectData -> Html Msg
viewPermissionsSection project =
    div [ class "mt-6", attribute "style" "border-top: 1px solid var(--border-color-light);" ]
        [ p [ class "help mt-6 is-size-7 mb-2" ] [ text T.permissions ]
        , div [ class "field" ]
            [ Html.label [ class "checkbox" ]
                [ input
                    [ type_ "checkbox"
                    , checked project.peerCanEditProject
                    , onClick OnTogglePeerCanEdit
                    ]
                    []
                , span [ class "ml-2" ] [ text T.peerCanEditProject ]
                ]
            , p [ class "help" ] [ text T.peerCanEditProjectHelp ]
            ]
        , div [ class "field" ]
            [ Html.label [ class "checkbox" ]
                [ input
                    [ type_ "checkbox"
                    , checked project.guestCanEditProject
                    , onClick OnToggleGuestCanEdit
                    ]
                    []
                , span [ class "ml-2" ] [ text T.guestCanEditProject ]
                ]
            , p [ class "help" ] [ text T.guestCanEditProjectHelp ]
            ]
        ]



-- Ports


port closeProjectSettingsPanelFromJs : (() -> msg) -> Sub msg
