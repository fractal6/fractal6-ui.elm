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


module Components.UserSearchPanel exposing (..)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Browser.Events as Events
import Bulk exposing (AssigneeForm, Ev, UserState(..), initAssigneeForm)
import Bulk.Codecs exposing (FractalBaseRoute(..), toLink)
import Bulk.Error exposing (viewGqlErrors)
import Bulk.View exposing (getAvatar1, viewUserFull)
import Codecs exposing (userDecoder)
import Dict
import Dom
import Extra exposing (send, sendNow, sendSleep, ternary)
import Extra.Events exposing (onMousedownPD)
import Fractal.Enum.TensionEvent as TensionEvent
import Global exposing (Msg(..))
import Html exposing (Html, a, div, i, input, nav, p, span, text)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, title, type_, value)
import Html.Events exposing (onClick, onInput)
import Iso8601 exposing (fromTime)
import Json.Decode as JD
import List.Extra as LE
import Loading exposing (GqlData, RequestResult(..), loadingSpin, withDefaultData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Ports
import Query.PatchTension exposing (setAssignee)
import Query.QueryNode exposing (queryMembers)
import Query.QueryProject exposing (setProjectDraftAssignee)
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
    | SetTensionid String
    | SetAction OnClickAction
    | OnChangePattern String
    | ChangeAssigneeLookup (List User)
    | OnAssigneeClick User Bool Time.Posix
    | OnAssigneeAck (GqlData IdPayload)
    | OnSubmit (Time.Posix -> Msg)
    | OnGotAssignees (GqlData (List User))
    | SetAssignee AssigneeForm
    | ResetClickResult
      --
    | Navigate String
    | OnModalAsk String String
      -- Common
    | NoMsg
    | LogErr String


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

        SetTensionid tid ->
            let
                form =
                    model.form
            in
            ( { model | form = { form | tid = tid } }, noOut )

        SetAction action ->
            ( { model | action = action }, noOut )

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
            ( { model | lookup = data }, noOut )

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

                    AssignProjectDraftUser ->
                        let
                            data =
                                newModel |> setClickResult LoadingSlowly
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
            , case model.action of
                AssignProjectDraftUser ->
                    out0 [ setProjectDraftAssignee apis form OnAssigneeAck ]

                _ ->
                    out0 [ setAssignee apis form OnAssigneeAck ]
            )

        ResetClickResult ->
            ( setClickResult NotAsked model, noOut )

        Navigate link ->
            ( model, out1 [ DoNavigate link ] )

        OnModalAsk link onCloseTxt ->
            ( model, out1 [ DoModalAsk link onCloseTxt ] )

        -- Common
        NoMsg ->
            ( model, noOut )

        LogErr err ->
            ( model, out0 [ Ports.logErr err ] )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    if model.isOpen then
        [ Ports.pd Ports.lookupUserFromJs (JD.list userDecoder) LogErr ChangeAssigneeLookup
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
            view_ False { op | selectedAssignees = selectedAssignees } model

          else
            text ""
        ]


viewNew : Op -> State -> Html Msg
viewNew op (State model) =
    span []
        [ span [ class "panel-selector-wrapper" ]
            [ div [ id id_target_name, class "is-reversed" ]
                [ if model.isOpen then
                    let
                        selectedAssignees =
                            List.concatMap
                                (\l ->
                                    List.filter (\u -> l.username == u.username) (withDefaultData [] model.assignees_data)
                                )
                                op.selectedAssignees
                    in
                    view_ True { op | selectedAssignees = selectedAssignees } model

                  else
                    text ""
                ]
            , div
                [ class "button is-small mr-2"
                , onClick (OnOpen op.targets)
                ]
                [ A.icon1 "icon-1x icon-user" "", text T.assignees ]
            ]
        , if List.length op.selectedAssignees > 0 then
            viewUsers False op.selectedAssignees

          else
            text ""
        ]


view_ : Bool -> Op -> Model -> Html Msg
view_ isEmbedded op model =
    nav [ id "usersSearchPanel", class "panel dropList", classList [ ( "is-right", op.isRight ) ] ]
        [ case model.assignees_data of
            Success assignees_d ->
                let
                    --user =
                    --    model.form.uctx |> List.singleton |> List.map (\u -> User u.username u.name)
                    users =
                        if model.pattern == "" then
                            -- selection
                            op.selectedAssignees
                                -- First show self user
                                ++ List.filter (\u -> model.form.uctx.username == u.username) assignees_d
                                -- sort by username
                                ++ List.sortBy .username (List.take 42 assignees_d)
                                -- uniq
                                |> LE.uniqueBy .username

                        else
                            LE.uniqueBy .username model.lookup
                in
                div [] <|
                    ternary isEmbedded List.reverse identity <|
                        [ div [ class "panel-block" ]
                            [ p [ class "control has-icons-left", classList [ ( "has-icons-right", model.pattern /= "" ) ] ]
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
                        , viewAssigneeSelectors isEmbedded users op model
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


viewAssigneeSelectors : Bool -> List User -> Op -> Model -> Html Msg
viewAssigneeSelectors isEmbedded users op model =
    let
        editLink =
            toLink MembersBaseUri (op.targets |> List.head |> withDefault "") []

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
                [ A.icon1 "icon-user-plus" T.inviteNewMembers ]
    in
    div []
        [ ternary isEmbedded viewEdit (text "")
        , div [ class "selectors" ] <|
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
        , ternary isEmbedded (text "") viewEdit
        ]



--
-- User rendering
--


viewUsers : Bool -> List User -> Html msg
viewUsers isLinked users =
    span [ class "usersList" ] (List.map (\u -> viewUser isLinked u.username) users)


viewUser : Bool -> String -> Html msg
viewUser isLinked_ username =
    let
        isLinked =
            if String.contains "@" username then
                False

            else
                isLinked_
    in
    if isLinked then
        span [ class "mr-2", title username ]
            [ a [ href (toLink UsersBaseUri username []) ]
                [ getAvatar1 username ]
            ]

    else
        span [ class "mr-2", title username ] [ getAvatar1 username ]
