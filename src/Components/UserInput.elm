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


port module Components.UserInput exposing (Msg(..), State, init, subscriptions, update, view, viewUserSeeker)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Bulk exposing (UserForm, UserState(..), initUserForm, uctxFromUser)
import Bulk.Error exposing (viewGqlErrors)
import Bulk.View exposing (viewUserFull)
import Codecs exposing (LookupResult)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Dict exposing (Dict)
import Extra exposing (space_, ternary, textH, upH)
import Extra.Date exposing (diffTime)
import Extra.Events exposing (onClickPD)
import Form exposing (isPostEmpty, isUsersSendable)
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, style, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), isSuccess, loadingSpinRight, withMaybeData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Ports
import Query.QueryNode exposing (queryMembers)
import Query.QueryUser exposing (queryUser)
import Session exposing (Apis, GlobalCmd(..))
import Text as T
import Time


type State
    = State Model


type alias Model =
    { user : UserState
    , users_result : GqlData (List User)
    , form : List UserForm
    , pattern : String
    , lookup : List User
    , multiSelect : Bool
    , isInvite : Bool -- @TOIMPROVE: Either Invitation (user wide search), or mentions (member only search, email deactivated.)
    , lastPattern : String -- last pattern used to fetch data
    , lastTime : Time.Posix -- last time data were fetch
    , isOpen : Bool -- state of the selectors panel
    , targets : List String

    -- Common
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    }


initModel : Bool -> Bool -> UserState -> Model
initModel isInvite multiSelect user =
    { user = user
    , users_result = NotAsked
    , form = []
    , pattern = ""
    , lookup = []
    , multiSelect = multiSelect
    , isInvite = isInvite
    , lastPattern = ""
    , lastTime = Time.millisToPosix 0
    , isOpen = False
    , targets = []

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }


init : Bool -> Bool -> UserState -> State
init isInvite multiSelect user =
    initModel isInvite multiSelect user |> State



-- Global methods
--isOpen_ : State -> Bool
--isOpen_ (State model) =
--    model.isOpen
--- State Controls


reset : Model -> Model
reset model =
    initModel model.isInvite model.multiSelect model.user


open : Model -> Model
open model =
    { model | isOpen = True }


close : Model -> Model
close model =
    { model | isOpen = False, lookup = [], pattern = "" }


clickUser : User -> Model -> Model
clickUser user model =
    let
        form =
            initUserForm
    in
    { model
        | form = model.form ++ [ { form | username = user.username, name = user.name } ] |> LE.uniqueBy .username
        , pattern = ""
    }


clickEmail : String -> Model -> Model
clickEmail email model =
    let
        form =
            initUserForm
    in
    { model
        | form = model.form ++ [ { form | email = String.toLower email |> String.trim } ] |> LE.uniqueBy .email
        , pattern = ""
    }


unselect : Int -> Model -> Model
unselect i model =
    { model | form = LE.removeAt i model.form }


setPattern : String -> Model -> Model
setPattern value model =
    { model | pattern = value }


setDataResult : GqlData (List User) -> Model -> Model
setDataResult result model =
    { model | users_result = result }



-- utils
--
--
--
--
-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = -- Data
      OnLoad
    | OnOpenMembers
    | OnCloseMembers
    | OnReset
    | OnInput Bool String
    | OnClickUser User
    | OnClickEmail String
    | OnUnselect Int
    | DoQueryUser
    | OnUsersAck (GqlData (List User))
      -- Lookup
    | ChangeUserLookup (LookupResult User)
    | ChangePath (List String)
    | ChangePattern String
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

    -- Return True on user select and False on unselect
    , result : Maybe ( Bool, List UserForm ) -- define what data is to be returned
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


update_ apis message model =
    case message of
        -- Data
        OnLoad ->
            ( model, out0 [ Ports.focusOn "userInput" ] )

        OnOpenMembers ->
            if
                not (isSuccess model.users_result)
                    && not model.isInvite
                -- prevent multiple call to queryMembers user invitation input box
            then
                ( { model | isOpen = True, users_result = LoadingSlowly }, out0 [ queryMembers apis model.targets OnUsersAck ] )

            else if not model.isInvite && model.pattern == "" then
                ( { model | isOpen = True, lookup = withMaybeData model.users_result |> withDefault [] |> List.take 5 }, noOut )

            else
                ( model, noOut )

        OnCloseMembers ->
            ( close model, noOut )

        --Ports.inheritWith "usersSearchPanel"  @need it ?
        OnReset ->
            ( reset model, noOut )

        OnInput seemsEmail value ->
            if seemsEmail then
                ( setPattern value model, noOut )

            else
                ( setPattern value model, out0 [ send DoQueryUser ] )

        OnClickUser user ->
            let
                data =
                    clickUser user model
            in
            ( data, Out [] [] (Just ( True, data.form )) )

        OnClickEmail email ->
            let
                data =
                    clickEmail email model
            in
            ( data, Out [] [] (Just ( True, data.form )) )

        OnUnselect i ->
            ( unselect i model, Out [] [] (Just ( False, LE.getAt i model.form |> List.singleton |> List.filterMap identity )) )

        DoQueryUser ->
            ( setDataResult LoadingSlowly model
            , out0 [ queryUser apis (String.toLower model.pattern |> String.trim) OnUsersAck ]
            )

        OnUsersAck result ->
            let
                data =
                    setDataResult result model
            in
            case parseErr result data.refresh_trial of
                Authenticate ->
                    ( setDataResult NotAsked model
                    , out0 [ Ports.raiseAuthModal (uctxFromUser data.user) ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep DoQueryUser 500 ] [ DoUpdateToken ] )

                OkAuth users ->
                    ( data, out0 [ Ports.initUserSearchSeek users model.pattern ] )

                _ ->
                    ( data, noOut )

        ChangeUserLookup data ->
            case data of
                Ok d ->
                    if model.pattern == "" && not model.isInvite then
                        ( { model | lookup = withMaybeData model.users_result |> withDefault [] |> List.take 5 }, noOut )

                    else
                        ( { model | lookup = d }, noOut )

                Err err ->
                    ( model, out0 [ Ports.logErr err ] )

        ChangePath targets ->
            ( { model | targets = targets }, noOut )

        ChangePattern pattern ->
            ( setPattern pattern model, out0 [ Ports.searchUser pattern ] )

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
    [ Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    ]
        ++ (if (model.isOpen && not model.isInvite) || model.isInvite then
                -- Prevent for user mention search box to trigger msg when not open.
                -- For invite, the "open" status is handled in the parents components...
                [ Ports.lookupUserFromJs ChangeUserLookup ]

            else
                []
           )
        ++ (if not model.isInvite then
                [ Ports.propagatePathFromJs ChangePath
                , openMembersFromJs (always OnOpenMembers)
                , closeMembersFromJs (always OnCloseMembers)
                , changePatternFromJs ChangePattern
                ]

            else
                []
           )


port openMembersFromJs : (() -> msg) -> Sub msg


port closeMembersFromJs : (() -> msg) -> Sub msg


port changePatternFromJs : (String -> msg) -> Sub msg



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    { label_text : Html Msg }


view : Op -> State -> Html Msg
view op (State model) =
    div []
        [ viewInput op model
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        ]


viewUserSeeker : State -> Html Msg
viewUserSeeker (State model) =
    div [ class "panel sidePanel" ]
        [ div [ class "selectors" ] <|
            if model.lookup == [] then
                [ p [ class "panel-block help-label is-static", attribute "style" "cursor: default !important;" ] [ text T.userNotFound ] ]

            else
                model.lookup
                    |> List.map
                        (\u ->
                            p
                                [ class "panel-block pt-1 pb-1"
                                , onClick (OnClickUser u)
                                ]
                                [ viewUserFull 1 False False u ]
                        )
                    |> List.append [ loadingSpinRight (model.users_result == LoadingSlowly) ]
        ]


viewInput : Op -> Model -> Html Msg
viewInput op model =
    let
        hasSelected =
            List.length model.form > 0

        selectedUsers =
            List.indexedMap
                (\i x ->
                    div [ class "tagsinput tags has-addons m-0 mr-2", classList [ ( "singleSelection", not model.multiSelect ) ] ]
                        [ span [ class "tag is-rounded" ]
                            [ if x.email /= "" then
                                viewEmail x.email

                              else
                                viewUserFull 0 False False (User x.username x.name)
                            ]
                        , span [ class "tag is-delete is-rounded", onClick (OnUnselect i) ] []
                        ]
                )
                model.form

        seemsEmail =
            model.pattern
                |> String.split "@"
                |> (\l ->
                        case l of
                            [ a, b ] ->
                                a /= "" && b /= ""

                            _ ->
                                False
                   )
    in
    div []
        [ div [ class "field mb-5" ]
            [ label [ class "label" ] [ op.label_text ]
            , div [ class "control" ]
                (selectedUsers
                    ++ [ textarea
                            [ id "userInput"
                            , class "input is-rounded"

                            --, type_ "text"
                            , rows 1
                            , style "resize" "none"
                            , placeholder <| ternary hasSelected (ternary (not model.multiSelect && hasSelected) "" T.inviteSomeoneElse) T.usernameOrEmail
                            , value model.pattern
                            , ternary (not model.multiSelect && hasSelected)
                                (onClick NoMsg)
                                (onInput (OnInput seemsEmail))

                            --, disabled hasSelected
                            ]
                            []
                       ]
                )
            , if model.pattern == "" then
                text ""

              else if seemsEmail then
                viewEmailSelector op model

              else
                viewUserSelectors op model
            ]
        , case model.users_result of
            Failure err ->
                viewGqlErrors err

            _ ->
                text ""
        ]


viewUserSelectors : Op -> Model -> Html Msg
viewUserSelectors op model =
    div [ class "panel sidePanel" ]
        [ div [ class "selectors" ] <|
            if model.lookup == [] then
                [ p [ class "panel-block help-label is-static", attribute "style" "cursor: default !important;" ] [ text T.enterValidUserOrEmail ] ]

            else
                model.lookup
                    |> List.map
                        (\u ->
                            p
                                [ class "panel-block pt-1 pb-1"
                                , onClick (OnClickUser u)
                                ]
                                [ viewUserFull 1 False False u ]
                        )
                    |> List.append [ loadingSpinRight (model.users_result == LoadingSlowly) ]
        ]


viewEmailSelector : Op -> Model -> Html Msg
viewEmailSelector op model =
    div [ class "panel sidePanel" ]
        [ div [ class "selectors has-background-info-light" ]
            [ p
                [ class "panel-block"
                , onClick (OnClickEmail model.pattern)
                ]
                [ A.icon1 "icon-mail" T.invite
                , text space_
                , span [ class "is-italic" ] [ text "\"", text (String.trim model.pattern), text "\"" ]
                ]
            ]
        ]


viewEmail : String -> Html Msg
viewEmail email =
    span []
        [ span [ class "mr-2" ]
            [ A.icon "icon-mail"
            ]
        , span [ attribute "style" "position:relative;top:-2px;" ]
            [ span [ class "is-email" ] [ text email ] ]
        ]
