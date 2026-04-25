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


module Components.ProjectSearchPanel exposing (..)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Browser.Events as Events
import Bulk exposing (ProjectPanelForm, UserState(..), initProjectPanelForm)
import Bulk.Error exposing (viewGqlErrors)
import Dict
import Dom
import Extra exposing (ternary)
import Extra.Events exposing (onMousedownPD)
import Fractal.Enum.ProjectColumnType as ProjectColumnType
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, div, i, input, nav, p, span, text)
import Html.Attributes exposing (attribute, class, classList, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra as LE
import Loading exposing (GqlData, RequestResult(..), loadingSpin, withDefaultData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Ports
import Query.QueryNode exposing (getOpenProjectsForPanel)
import Query.QueryProject exposing (addProjectCard, removeProjectCards)
import Session exposing (Apis, GlobalCmd(..), ProjectSearchPanelOnClickAction(..))
import Text as T
import Time



{-
   A panel for selecting projects this tension belongs to.
-}


type alias Model =
    Session.ProjectSearchPanelModel


type alias OnClickAction =
    Session.ProjectSearchPanelOnClickAction


type State
    = State Model


id_target_name : String
id_target_name =
    "projectsPanelContent"


init : String -> OnClickAction -> UserState -> State
init tid action user =
    initModel tid action user |> State


initModel : String -> OnClickAction -> UserState -> Model
initModel tid action user =
    { isOpen = False
    , form = initProjectPanelForm tid user
    , click_result = NotAsked
    , action = action

    -- Lookup
    , lookup = []
    , pattern = ""
    , projects_data = NotAsked

    -- Common
    , refresh_trial = 0
    }


load : Maybe Model -> UserState -> State
load model user =
    case model of
        Just m ->
            State m

        Nothing ->
            init "" SelectProject user


getModel : State -> Model
getModel (State model) =
    model



-- Global methods


isOpen_ : State -> Bool
isOpen_ (State model) =
    model.isOpen



-- State control


open : List String -> Model -> Model
open targets data =
    let
        form =
            data.form
    in
    { data | isOpen = True, form = { form | targets = targets } }


close : Model -> Model
close data =
    { data | isOpen = False, click_result = NotAsked, pattern = "" }


click : ProjectWithColumns -> Bool -> Model -> Model
click project isNew data =
    let
        form =
            data.form
    in
    { data | form = { form | project = project, isNew = isNew } }


setClickResult : GqlData IdPayload -> Model -> Model
setClickResult result data =
    { data | click_result = result }


setPattern : String -> Model -> Model
setPattern pattern data =
    { data | pattern = pattern }



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = OnOpen (List String)
    | OnClose
    | OnClose_
    | OnChangePattern String
    | OnGotProjects (GqlData (List ProjectWithColumns))
    | OnProjectAdd ProjectWithColumns Time.Posix
    | OnProjectRemove String ProjectWithColumns Time.Posix
    | OnSubmit (Time.Posix -> Msg)
    | OnAddCardAck ProjectWithColumns ProjectColumnLite (GqlData (List ProjectCard))
    | OnRemoveCardAck String (GqlData (List String))
    | ResetClickResult
      -- Common
    | NoMsg
    | LogErr String


type ProjectActionResult
    = ProjectAdded TensionProject
    | ProjectRemoved String


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe ProjectActionResult
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
        OnOpen targets ->
            if not model.isOpen then
                let
                    hasChanged =
                        targets /= model.form.targets

                    ( newModel, cmd ) =
                        if hasChanged then
                            ( { model | projects_data = LoadingSlowly }
                            , [ getOpenProjectsForPanel apis targets Nothing OnGotProjects ]
                            )

                        else
                            ( model, [] )
                in
                ( open targets newModel
                , out0 <|
                    [ Ports.inheritWith "projectSearchPanel"
                    , Ports.focusOn "userInput"
                    ]
                        ++ cmd
                )

            else
                ( model, noOut )

        OnClose ->
            ( model, out0 [ sendSleep OnClose_ 50 ] )

        OnClose_ ->
            ( close model, noOut )

        OnGotProjects result ->
            ( { model | projects_data = result }, noOut )

        OnChangePattern pattern ->
            ( setPattern pattern model, noOut )

        OnProjectAdd project _ ->
            if model.click_result == LoadingSlowly then
                ( model, noOut )

            else
                let
                    newModel =
                        click project True model |> setClickResult LoadingSlowly

                    defaultCol =
                        case List.filter (\c -> c.col_type == ProjectColumnType.NoStatusColumn) project.columns |> List.head of
                            Just c ->
                                Just c

                            Nothing ->
                                List.sortBy .pos project.columns |> List.head
                in
                case defaultCol of
                    Just col ->
                        let
                            form =
                                { uctx = newModel.form.uctx
                                , tids = [ Just newModel.form.tid ]
                                , colid = col.id
                                , pos = 0
                                , post = Dict.empty
                                , title = ""
                                }
                        in
                        ( newModel
                        , out0 [ addProjectCard apis form (OnAddCardAck project col) ]
                        )

                    Nothing ->
                        ( setClickResult NotAsked newModel
                        , out0 [ Ports.logErr "Project has no columns" ]
                        )

        OnProjectRemove cardid project _ ->
            if model.click_result == LoadingSlowly then
                ( model, noOut )

            else
                let
                    newModel =
                        click project False model |> setClickResult LoadingSlowly
                in
                ( newModel
                , out0 [ removeProjectCards apis [ cardid ] (OnRemoveCardAck cardid) ]
                )

        OnAddCardAck project col result ->
            case result of
                Success (firstCard :: _) ->
                    let
                        tp =
                            { card = { id = firstCard.id, pos = firstCard.pos }
                            , column = col
                            , project = project
                            }
                    in
                    ( setClickResult NotAsked model
                    , Out [ sendSleep ResetClickResult 333 ] [] (Just (ProjectAdded tp))
                    )

                Success _ ->
                    ( setClickResult NotAsked model, noOut )

                Failure err ->
                    ( { model | click_result = Failure err }, noOut )

                _ ->
                    ( model, noOut )

        OnRemoveCardAck cardid result ->
            case result of
                Success _ ->
                    ( setClickResult NotAsked model
                    , Out [ sendSleep ResetClickResult 333 ] [] (Just (ProjectRemoved cardid))
                    )

                Failure err ->
                    ( { model | click_result = Failure err }, noOut )

                _ ->
                    ( model, noOut )

        OnSubmit next ->
            ( model, out0 [ sendNow next ] )

        ResetClickResult ->
            ( setClickResult NotAsked model, noOut )

        NoMsg ->
            ( model, noOut )

        LogErr err ->
            ( model, out0 [ Ports.logErr err ] )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    if model.isOpen then
        [ Events.onMouseUp (Dom.outsideClickClose id_target_name OnClose)
        , Events.onKeyUp (Dom.key "Escape" OnClose)
        ]

    else
        []



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    { selectedProjects : List TensionProject
    , targets : List String
    , isRight : Bool
    }


view : Op -> State -> Html Msg
view op (State model) =
    div [ id id_target_name ]
        [ if model.isOpen then
            view_ op model

          else
            text ""
        ]


view_ : Op -> Model -> Html Msg
view_ op model =
    nav [ id "projectSearchPanel", class "panel dropList", classList [ ( "is-right", op.isRight ) ] ]
        [ case model.projects_data of
            Success projects_d ->
                let
                    pattern_ =
                        String.toLower model.pattern

                    visible =
                        if pattern_ == "" then
                            projects_d

                        else
                            projects_d
                                |> List.filter (\p -> String.contains pattern_ (String.toLower p.name))
                in
                div []
                    [ div [ class "panel-block" ]
                        [ p [ class "control has-icons-left", classList [ ( "has-icons-right", model.pattern /= "" ) ] ]
                            [ input
                                [ id "userInput"
                                , class "input autofocus is-small"
                                , type_ "text"
                                , placeholder T.searchProjects
                                , value model.pattern
                                , onInput OnChangePattern
                                ]
                                []
                            , span [ class "icon is-left" ] [ i [ attribute "aria-hidden" "true", class "icon-search" ] [] ]
                            , if model.pattern /= "" then
                                span [ class "icon is-right is-clickable", onMousedownPD (OnChangePattern "") ] [ A.icon "icon-x" ]

                              else
                                text ""
                            ]
                        ]
                    , case model.click_result of
                        Failure err ->
                            viewGqlErrors err

                        _ ->
                            text ""
                    , viewProjectSelectors visible op model
                    ]

            Loading ->
                div [ class "spinner" ] []

            LoadingSlowly ->
                div [ class "spinner" ] []

            NotAsked ->
                text ""

            Failure err ->
                viewGqlErrors err
        ]


viewProjectSelectors : List ProjectWithColumns -> Op -> Model -> Html Msg
viewProjectSelectors projects op model =
    div [ class "selectors" ] <|
        if projects == [] then
            [ p [ class "panel-block" ] [ text T.noResultsFound ] ]

        else
            projects
                |> List.map
                    (\p ->
                        let
                            existing =
                                op.selectedProjects |> LE.find (\tp -> tp.project.id == p.id)

                            isActive =
                                existing /= Nothing

                            iconCls =
                                ternary isActive "icon-check-square" "icon-square"

                            isLoading =
                                model.click_result == LoadingSlowly && p.id == model.form.project.id

                            handler =
                                case existing of
                                    Just tp ->
                                        onClick (OnSubmit <| OnProjectRemove tp.card.id p)

                                    Nothing ->
                                        onClick (OnSubmit <| OnProjectAdd p)
                        in
                        Html.p
                            [ class "panel-block"
                            , classList [ ( "is-active", isActive ) ]
                            , handler
                            ]
                            [ span [ class "panel-icon" ] [ A.icon iconCls ]
                            , span [] [ text p.name ]
                            , loadingSpin isLoading
                            ]
                    )
