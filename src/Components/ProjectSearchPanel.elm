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
import Bulk.Codecs exposing (FractalBaseRoute(..), nid2rootid, toLink)
import Bulk.Error exposing (viewGqlErrors)
import Bulk.View exposing (viewCircleSimple)
import Dict
import Dom
import Extra exposing (send, sendNow, sendSleep, ternary)
import Extra.Events exposing (onClickSP, onMousedownPD)
import Fractal.Enum.ProjectColumnType as ProjectColumnType
import Html exposing (Html, a, div, i, input, nav, p, span, text)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra as LE
import Loading exposing (GqlData, RequestResult(..), loadingSpin, withDefaultData, withMapData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Ports
import Query.QueryNode exposing (getOpenProjectsForPanel)
import Query.QueryProject exposing (addProjectCard, moveProjectCard, removeProjectCards)
import Query.QueryTension exposing (getTensionProjects)
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
    , tension_projects = NotAsked
    , statusEditOpen = Nothing
    , status_result = NotAsked

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


getSelectedProjects : State -> List TensionProject
getSelectedProjects (State model) =
    withDefaultData [] model.tension_projects



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
      -- Tension cards
    | OnLoadCards
    | GotCards (GqlData (List TensionProject))
    | OnStatusEditOpen String
    | OnStatusEditClose
    | OnMoveCardToColumn String String
    | OnMoveCardAck (GqlData IdPayload)
      --
    | Navigate String
    | OnModalAsk String String
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
                        case model.action of
                            SelectProject ->
                                let
                                    tp =
                                        { card = { id = "", pos = 0 }
                                        , column = col
                                        , project = project
                                        }
                                in
                                ( setClickResult NotAsked newModel
                                , Out [ sendSleep ResetClickResult 333 ] [] (Just (ProjectAdded tp))
                                )

                            AssignProject ->
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
                case model.action of
                    SelectProject ->
                        ( setClickResult NotAsked newModel
                        , Out [ sendSleep ResetClickResult 333 ] [] (Just (ProjectRemoved project.id))
                        )

                    AssignProject ->
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

                        newModel =
                            { model | tension_projects = withMapData (\xs -> tp :: xs) model.tension_projects }
                    in
                    ( setClickResult NotAsked newModel, out0 [ sendSleep ResetClickResult 333 ] )

                Success _ ->
                    ( setClickResult NotAsked model, noOut )

                Failure err ->
                    ( { model | click_result = Failure err }, noOut )

                _ ->
                    ( model, noOut )

        OnRemoveCardAck cardid result ->
            case result of
                Success _ ->
                    let
                        newModel =
                            { model | tension_projects = withMapData (List.filter (\tp -> tp.card.id /= cardid)) model.tension_projects }
                    in
                    ( setClickResult NotAsked newModel, out0 [ sendSleep ResetClickResult 333 ] )

                Failure err ->
                    ( { model | click_result = Failure err }, noOut )

                _ ->
                    ( model, noOut )

        OnLoadCards ->
            case model.action of
                AssignProject ->
                    ( { model | tension_projects = LoadingSlowly }
                    , out0 [ getTensionProjects apis model.form.tid GotCards ]
                    )

                SelectProject ->
                    ( model, noOut )

        GotCards result ->
            ( { model | tension_projects = result }, noOut )

        OnStatusEditOpen cardid ->
            if model.statusEditOpen == Just cardid then
                ( model, noOut )

            else
                ( { model | statusEditOpen = Just cardid, status_result = NotAsked }, noOut )

        OnStatusEditClose ->
            ( { model | statusEditOpen = Nothing }, noOut )

        OnMoveCardToColumn cardid colid ->
            let
                tension_projects =
                    withMapData
                        (List.map
                            (\tp ->
                                if tp.card.id == cardid then
                                    let
                                        newCol =
                                            tp.project.columns
                                                |> List.filter (\c -> c.id == colid)
                                                |> List.head
                                                |> withDefault tp.column
                                    in
                                    { tp | column = newCol }

                                else
                                    tp
                            )
                        )
                        model.tension_projects
            in
            ( { model | tension_projects = tension_projects, statusEditOpen = Nothing, status_result = LoadingSlowly }
            , out0 [ moveProjectCard apis cardid 0 colid OnMoveCardAck ]
            )

        OnMoveCardAck result ->
            let
                refetch =
                    case result of
                        Failure _ ->
                            [ getTensionProjects apis model.form.tid GotCards ]

                        _ ->
                            []
            in
            ( { model | status_result = result }, out0 refetch )

        OnSubmit next ->
            ( model, out0 [ sendNow next ] )

        ResetClickResult ->
            ( setClickResult NotAsked model, noOut )

        Navigate link ->
            ( model, out1 [ DoNavigate link ] )

        OnModalAsk link onCloseTxt ->
            ( model, out1 [ DoModalAsk link onCloseTxt ] )

        NoMsg ->
            ( model, noOut )

        LogErr err ->
            ( model, out0 [ Ports.logErr err ] )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    let
        openSubs =
            if model.isOpen then
                [ Events.onMouseUp (Dom.outsideClickClose id_target_name OnClose)
                , Events.onKeyUp (Dom.key "Escape" OnClose)
                ]

            else
                []

        statusSubs =
            case model.statusEditOpen of
                Just cardid ->
                    [ Events.onMouseUp (Dom.outsideClickClose (statusDropdownId cardid) OnStatusEditClose)
                    , Events.onKeyUp (Dom.key "Escape" OnStatusEditClose)
                    ]

                Nothing ->
                    []
    in
    openSubs ++ statusSubs



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
            view_ False op model

          else
            text ""
        ]


viewNew : Op -> State -> Html Msg
viewNew op (State model) =
    span []
        [ span [ class "panel-selector-wrapper" ]
            [ div [ id id_target_name, class "is-reversed" ]
                [ if model.isOpen then
                    view_ True op model

                  else
                    text ""
                ]
            , div
                [ class "button is-small mr-2"
                , onClick (OnOpen op.targets)
                ]
                [ A.icon1 "icon-1x icon-layout" "", text T.projects ]
            ]
        , if List.length op.selectedProjects > 0 then
            span [ class "ml-2" ]
                (List.map
                    (\tp ->
                        span
                            [ class "tag has-border mr-2"
                            , attribute "style" "border-radius: 12px;"
                            ]
                            [ text tp.project.name ]
                    )
                    op.selectedProjects
                )

          else
            text ""
        ]


view_ : Bool -> Op -> Model -> Html Msg
view_ isEmbedded op model =
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
                div [] <|
                    ternary isEmbedded List.reverse identity <|
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
                        , viewProjectSelectors isEmbedded visible op model
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


viewProjectSelectors : Bool -> List ProjectWithColumns -> Op -> Model -> Html Msg
viewProjectSelectors isEmbedded projects op model =
    let
        editLink =
            toLink ProjectsBaseUri (op.targets |> List.head |> withDefault "") []

        viewEdit =
            p
                [ class "panel-block is-md is-w discrete-link"
                , if isEmbedded then
                    attribute "style" "border-bottom: 1px solid;"

                  else
                    attribute "style" "border-top: 1px solid;"
                , if isEmbedded then
                    onClick (OnModalAsk editLink "")

                  else
                    onClick (Navigate editLink)
                ]
                [ A.icon1 "icon-edit-2" T.addOrEditProjects ]
    in
    div []
        [ ternary isEmbedded viewEdit (text "")
        , div [ class "selectors" ] <|
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
                                , span [ class "is-pushed-right is-flex is-flex-wrap-wrap is-justify-content-flex-end" ]
                                    (List.map (\n -> viewCircleSimple n.nameid) p.nodes)
                                ]
                        )
        , ternary isEmbedded (text "") viewEdit
        ]



--
-- Tension projects card list
--


viewCards : Bool -> State -> Html Msg
viewCards canEdit (State model) =
    let
        tps =
            withDefaultData [] model.tension_projects

        moveError =
            case model.status_result of
                Failure errs ->
                    viewGqlErrors errs

                _ ->
                    text ""
    in
    if List.isEmpty tps then
        div []
            [ div [ class "help-label is-italic" ] [ text T.noProjectsYet ]
            , moveError
            ]

    else
        div []
            (List.map (viewCard canEdit model) tps ++ [ moveError ])


viewCard : Bool -> Model -> TensionProject -> Html Msg
viewCard canEdit model tp =
    let
        otherCols =
            tp.project.columns |> List.filter (\c -> c.id /= tp.column.id)

        canMove =
            canEdit && not (List.isEmpty otherCols)

        rootid =
            tp.project.nodes |> List.head |> Maybe.map (.nameid >> nid2rootid) |> withDefault ""

        isDropdownOpen =
            model.statusEditOpen == Just tp.card.id

        statusPill =
            viewProjectColumnTag tp.column.color
                tp.column.name
                (if canMove then
                    [ onClickSP <|
                        if isDropdownOpen then
                            OnStatusEditClose

                        else
                            OnStatusEditOpen tp.card.id
                    ]

                 else
                    []
                )
                (if canMove then
                    [ A.icon "icon-chevron-down ml-2" ]

                 else
                    []
                )

        dropdown =
            if isDropdownOpen then
                div [ class "tension-project-status-dropdown" ]
                    [ nav [ class "panel dropList" ]
                        (List.map
                            (\c ->
                                Html.p
                                    [ class "panel-block tension-project-column-item"
                                    , onClickSP (OnMoveCardToColumn tp.card.id c.id)
                                    ]
                                    [ viewProjectColumnTag c.color c.name [] [] ]
                            )
                            otherCols
                        )
                    ]

            else
                text ""
    in
    div [ class "tension-project-card", onClickSP NoMsg ]
        [ div [ class "tension-project-name" ]
            [ a [ href (toLink ProjectBaseUri rootid [ tp.project.id ]) ] [ text tp.project.name ] ]
        , div [ id (statusDropdownId tp.card.id), class "mt-1" ] [ statusPill, dropdown ]
        ]


statusDropdownId : String -> String
statusDropdownId cardid =
    "tension-project-status-dropdown-" ++ cardid


viewProjectColumnTag : Maybe String -> String -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewProjectColumnTag color name attrs extras =
    span (class "tag tension-project-status is-inline-flex is-align-items-center is-justify-content-center" :: attrs)
        ([ span [ class "mr-2 is-flex is-align-items-center", style "color" (withDefault "lightgrey" color) ] [ A.icon "icon-circle1" ]
         , text name
         ]
            ++ extras
        )
