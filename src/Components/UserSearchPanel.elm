{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2023 Fractale Co

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


module Components.UserSearchPanel exposing (..)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Browser.Events as Events
import Bulk exposing (AssigneeForm, Ev, UserState(..), initAssigneeForm)
import Bulk.Error exposing (viewGqlErrors)
import Bulk.View exposing (viewUserFull)
import Codecs exposing (LookupResult)
import Dict
import Dom
import Extra exposing (ternary)
import Fractal.Enum.TensionEvent as TensionEvent
import Global exposing (Msg(..), send, sendNow, sendSleep)
import Html exposing (Html, div, i, input, nav, p, span, text)
import Html.Attributes exposing (attribute, class, classList, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, RequestResult(..), loadingSpin, withDefaultData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Ports
import Query.PatchTension exposing (setAssignee)
import Query.QueryNode exposing (queryMembers)
import Session exposing (Apis, GlobalCmd(..), UserSearchPanelOnClickAction(..))
import Text as T
import Time



{-
   A panel for multi-user selection
-}


type alias Model =
    Session.UserSearchPanelModel


type alias OnClickAction =
    Session.UserSearchPanelOnClickAction


type State
    = State Model


id_target_name : String
id_target_name =
    "usersPanelContent"


init : String -> OnClickAction -> UserState -> State
init tid action user =
    initModel tid action user |> State


initModel : String -> OnClickAction -> UserState -> Model
initModel tid action user =
    { isOpen = False
    , form = initAssigneeForm tid user
    , click_result = NotAsked
    , action = action

    -- Lookup
    , lookup = []
    , pattern = ""
    , assignees_data = NotAsked

    -- Common
    , refresh_trial = 0
    }


load : Maybe Model -> UserState -> State
load model user =
    case model of
        Just m ->
            State { m | click_result = NotAsked }

        Nothing ->
            init "" SelectUser user


getModel : State -> Model
getModel (State model) =
    model



-- Global methods


isOpen_ : State -> Bool
isOpen_ (State model) =
    model.isOpen


isEmpty_ : State -> Bool
isEmpty_ (State model) =
    (withDefaultData [] model.assignees_data |> List.length) == 0



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


click : User -> Bool -> Model -> Model
click assignee isNew data =
    let
        form =
            data.form
    in
    { data | form = { form | assignee = assignee, isNew = isNew } }


setClickResult : GqlData IdPayload -> Model -> Model
setClickResult result data =
    { data | click_result = result }



-- Update Form


setEvents : List Ev -> Model -> Model
setEvents events data =
    let
        f =
            data.form
    in
    { data | form = { f | events = events } }


updatePost : String -> String -> Model -> Model
updatePost field value data =
    let
        f =
            data.form
    in
    { data | form = { f | post = Dict.insert field value f.post } }


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
    | ChangeAssigneeLookup (LookupResult User)
    | OnAssigneeClick User Bool Time.Posix
    | OnAssigneeAck (GqlData IdPayload)
    | OnSubmit (Time.Posix -> Msg)
    | OnGotAssignees (GqlData (List User))
    | SetAssignee AssigneeForm
    | ResetClickResult


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe ( Bool, User )
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
                    ( newModel, cmd ) =
                        ternary (targets /= model.form.targets)
                            ( { model | assignees_data = LoadingSlowly }, [ queryMembers apis targets OnGotAssignees ] )
                            ( model, [] )
                in
                ( open targets newModel
                , out0 <|
                    [ Ports.inheritWith "usersSearchPanel"
                    , Ports.focusOn "userInput"
                    ]
                        ++ cmd
                )

            else
                ( model, noOut )

        OnClose ->
            -- The delay is used to be able to toggle (close) the panel on clicking the button.
            ( model, out0 [ sendSleep OnClose_ 50 ] )

        OnClose_ ->
            ( close model, noOut )

        OnGotAssignees result ->
            ( { model | assignees_data = result }
            , case result of
                Success r ->
                    out0 [ Ports.initUserSearch r ]

                _ ->
                    noOut
            )

        OnChangePattern pattern ->
            ( setPattern pattern model
            , out0 [ Ports.searchUser pattern ]
            )

        ChangeAssigneeLookup data ->
            case data of
                Ok d ->
                    ( { model | lookup = d }, noOut )

                Err err ->
                    ( model, out0 [ Ports.logErr err ] )

        OnAssigneeClick assignee isNew time ->
            if model.click_result == LoadingSlowly then
                -- wait here !
                ( model, noOut )

            else
                let
                    newModel =
                        click assignee isNew model
                in
                case model.action of
                    AssignUser ->
                        let
                            data =
                                newModel
                                    |> updatePost "createdAt" (fromTime time)
                                    |> setEvents
                                        [ ternary
                                            isNew
                                            (Ev TensionEvent.AssigneeAdded "" assignee.username)
                                            (Ev TensionEvent.AssigneeRemoved assignee.username "")
                                        ]
                                    |> setClickResult LoadingSlowly
                        in
                        ( data
                        , out0 [ send (SetAssignee data.form) ]
                        )

                    SelectUser ->
                        let
                            data =
                                setClickResult LoadingSlowly newModel

                            users =
                                withDefaultData [] model.assignees_data
                                    |> (\x ->
                                            if isNew then
                                                x ++ [ assignee ]

                                            else
                                                LE.remove assignee x
                                       )
                        in
                        ( { data | assignees_data = Success users }
                        , Out [ sendSleep ResetClickResult 333 ] [] (Just ( data.form.isNew, data.form.assignee ))
                        )

        OnAssigneeAck result ->
            let
                data =
                    setClickResult result model
            in
            case parseErr result data.refresh_trial of
                Authenticate ->
                    ( setClickResult NotAsked model
                    , out0 [ Ports.raiseAuthModal data.form.uctx ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep (SetAssignee data.form) 500 ] [ DoUpdateToken ] )

                OkAuth _ ->
                    ( data, Out [] [] (Just ( data.form.isNew, data.form.assignee )) )

                _ ->
                    ( data, noOut )

        OnSubmit next ->
            ( model
            , out0 [ sendNow next ]
            )

        SetAssignee form ->
            ( model
            , out0 [ setAssignee apis form OnAssigneeAck ]
            )

        ResetClickResult ->
            ( setClickResult NotAsked model, noOut )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    if model.isOpen then
        [ Ports.lookupUserFromJs ChangeAssigneeLookup
        , Events.onMouseUp (Dom.outsideClickClose id_target_name OnClose)
        , Events.onKeyUp (Dom.key "Escape" OnClose)
        ]

    else
        []



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    { selectedAssignees : List User
    , targets : List String
    , isRight : Bool
    }


view : Op -> State -> Html Msg
view op (State model) =
    div [ id id_target_name ]
        [ if model.isOpen then
            let
                selectedAssignees =
                    List.concatMap
                        -- name is not passed from url
                        (\l ->
                            List.filter (\u -> l.username == u.username) (withDefaultData [] model.assignees_data)
                        )
                        op.selectedAssignees
            in
            view_ { op | selectedAssignees = selectedAssignees } model

          else
            text ""
        ]


view_ : Op -> Model -> Html Msg
view_ op model =
    nav [ id "usersSearchPanel", class "panel sidePanel", classList [ ( "is-right", op.isRight ) ] ]
        [ case model.assignees_data of
            Success assignees_d ->
                let
                    --user =
                    --    model.form.uctx |> List.singleton |> List.map (\u -> User u.username u.name)
                    users =
                        if model.pattern == "" then
                            List.sortBy .username op.selectedAssignees
                                -- First show looged user
                                ++ List.filter (\u -> model.form.uctx.username == u.username) assignees_d
                                -- sort by username
                                ++ List.sortBy .username (List.take 42 assignees_d)
                                -- uniq
                                |> LE.uniqueBy .username

                        else
                            LE.uniqueBy .username model.lookup
                in
                div []
                    [ div [ class "panel-block" ]
                        [ p [ class "control has-icons-left" ]
                            [ input
                                [ id "userInput"
                                , class "input autofocus is-small"
                                , type_ "text"
                                , placeholder T.searchUsers
                                , value model.pattern
                                , onInput OnChangePattern
                                ]
                                []
                            , span [ class "icon is-left" ] [ i [ attribute "aria-hidden" "true", class "icon-search" ] [] ]
                            ]
                        ]
                    , case model.click_result of
                        Failure err ->
                            viewGqlErrors err

                        _ ->
                            text ""
                    , viewAssigneeSelectors users op model
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


viewAssigneeSelectors : List User -> Op -> Model -> Html Msg
viewAssigneeSelectors users op model =
    div [ class "selectors" ] <|
        if users == [] then
            [ p [ class "panel-block" ] [ text T.noResultsFound ] ]

        else
            users
                |> List.map
                    (\u ->
                        let
                            isActive =
                                List.member u op.selectedAssignees

                            iconCls =
                                ternary isActive "icon-check-square" "icon-square"

                            isLoading =
                                model.click_result == LoadingSlowly && u.username == model.form.assignee.username
                        in
                        p
                            [ class "panel-block p-1"
                            , classList [ ( "is-active", isActive ) ]
                            , onClick (OnSubmit <| OnAssigneeClick u (not isActive))
                            ]
                            [ span [ class "panel-icon" ] [ A.icon iconCls ]
                            , viewUserFull 1 False False u
                            , loadingSpin isLoading
                            ]
                    )
