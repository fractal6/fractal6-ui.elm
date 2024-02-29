{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2024 Fractale Co

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


module Components.ProjectColumnModal exposing (ModalType(..), Msg(..), State, init, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Bulk exposing (UserState(..), uctxFromUser)
import Bulk.Error exposing (viewGqlErrors)
import Components.ColorPicker as ColorPicker exposing (ColorPicker)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Dict exposing (Dict)
import Extra exposing (ternary, textH, unwrap, unwrap2, upH)
import Extra.Events exposing (onClickPD)
import Form exposing (isPostEmpty)
import Fractal.Enum.ProjectColumnType as ProjectColumnType
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, autofocus, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), isFailure, isSuccess, withMapData, withMaybeData)
import Maybe exposing (withDefault)
import ModelSchema exposing (IdPayload, Post, ProjectColumn, ProjectColumnEdit, UserCtx)
import Ports
import Query.QueryProject exposing (addProjectColumn, getProjectColumn, updateProjectColumn)
import Session exposing (Apis, GlobalCmd(..))
import Text as T
import Time



-- ------------------------------
-- M O D E L
-- ------------------------------


type State
    = State Model


type alias Model =
    { user : UserState
    , isActive : Bool
    , isActive2 : Bool -- Let minimze VDOM load + prevent glitch while keeping css effects
    , data_result : GqlData String -- result of any query
    , form : ColumnForm -- user inputs
    , orig_form : ColumnForm -- original form for edits
    , modal_type : ModalType

    -- Common
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    , colorPicker : ColorPicker
    }


initModel : String -> UserState -> Model
initModel projectid user =
    { user = user
    , isActive = False
    , isActive2 = False
    , data_result = NotAsked
    , form = initForm projectid user
    , orig_form = initForm projectid user
    , modal_type = AddColumn

    -- Common
    , refresh_trial = 1
    , modal_confirm = ModalConfirm.init NoMsg
    , colorPicker = ColorPicker.init
    }


type ModalType
    = AddColumn
    | EditColumn
    | DeleteColumn -- @todo


toTitle : ModalType -> String
toTitle a =
    case a of
        AddColumn ->
            T.addColumn

        EditColumn ->
            T.editColumn

        DeleteColumn ->
            ""


toSubmit : ModalType -> String
toSubmit a =
    case a of
        AddColumn ->
            T.save

        EditColumn ->
            T.save

        DeleteColumn ->
            ""


type alias ColumnForm =
    { uctx : UserCtx
    , projectid : String
    , colid : String
    , col_type : Maybe ProjectColumnType.ProjectColumnType
    , pos : Maybe Int
    , post : Post
    }


initForm : String -> UserState -> ColumnForm
initForm projectid user =
    { uctx = uctxFromUser user
    , projectid = projectid
    , colid = ""
    , col_type = Nothing
    , pos = Nothing
    , post = Dict.empty
    }


init : String -> UserState -> State
init projectid user =
    initModel projectid user |> State



-- Global methods


isActive_ : State -> Bool
isActive_ (State model) =
    model.isActive



-- State Controls


resetModel : Model -> Model
resetModel model =
    initModel model.form.projectid model.user


updatePost : String -> String -> Model -> Model
updatePost field value model =
    let
        form =
            model.form
    in
    { model | form = { form | post = Dict.insert field value form.post } }


setDataResult : GqlData (ProjectColumnCommon a) -> Model -> Model
setDataResult result model =
    { model | data_result = withMapData .id result }


setColor : Maybe String -> Model -> Model
setColor color model =
    { model | colorPicker = ColorPicker.setColor color model.colorPicker }


type alias ProjectColumnCommon a =
    { a | id : String }



-- utils


canExitSafe : Model -> Bool
canExitSafe model =
    -- Condition to close safely (e.g. empty form data)
    not (hasData model && withMaybeData model.data_result == Nothing)


hasData : Model -> Bool
hasData model =
    -- When you can commit (e.g. empty form data)
    not (isPostEmpty [ "name", "about" ] model.form.post)


isSendable : Model -> Bool
isSendable model =
    -- when the form can be submited
    not (isPostEmpty [ "name" ] model.form.post)



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = SetIsActive2 Bool
    | OnOpenAdd Int
    | OnOpenEdit String
    | OnClose ModalData
    | OnCloseSafe String String
    | OnReset
    | OnSubmit (Time.Posix -> Msg)
      -- Data
    | OnChangePost String String
    | OnColAdd
    | OnColAddAck (GqlData ProjectColumn)
    | OnColQuery
    | OnColQueryAck (GqlData ProjectColumnEdit)
    | OnColEdit
    | OnColEditAck (GqlData IdPayload)
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Color Picker
    | OpenColor
    | CloseColor
    | SelectColor String
      -- Common
    | NoMsg
    | LogErr String


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe ( ModalType, ProjectColumn )
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
        SetIsActive2 v ->
            if v then
                ( { model | isActive = model.isActive2 }, out0 [ Ports.open_modal "ProjectColumnModalModal" ] )

            else
                ( { model | isActive2 = model.isActive }, noOut )

        OnOpenAdd pos ->
            let
                form =
                    model.form

                newForm =
                    { form | pos = Just pos, post = Dict.insert "color" ColorPicker.initColor form.post }
            in
            ( { model | isActive2 = True, modal_type = AddColumn, form = newForm }
            , out0 [ sendSleep (SetIsActive2 True) 10, Ports.open_modal "ProjectColumnModalModal" ]
            )

        OnOpenEdit colid ->
            let
                form =
                    model.form

                newForm =
                    { form | colid = colid }
            in
            ( { model | isActive2 = True, modal_type = EditColumn, form = newForm }
            , out0
                [ sendSleep (SetIsActive2 True) 10
                , Ports.open_modal "ProjectColumnModalModal"
                , send OnColQuery
                ]
            )

        OnClose data ->
            let
                ( newModel, gcmds ) =
                    if data.link == "" then
                        ( model, [] )

                    else
                        ( { model | isActive2 = True }, [ DoNavigate data.link ] )
            in
            ( { newModel | isActive = False }
            , out2
                [ Ports.close_modal
                , ternary data.reset (sendSleep OnReset 333) Cmd.none
                , sendSleep (SetIsActive2 False) 500
                ]
                gcmds
            )

        OnReset ->
            ( resetModel model, noOut )

        OnCloseSafe link onCloseTxt ->
            if canExitSafe model then
                ( model, out0 [ send (OnClose { reset = True, link = link }) ] )

            else
                ( model
                , out0
                    [ send
                        (DoModalConfirmOpen (OnClose { reset = True, link = link })
                            { message = Nothing, txts = [ ( T.confirmUnsaved, onCloseTxt ) ] }
                        )
                    ]
                )

        -- Data
        OnChangePost field value ->
            ( updatePost field value model, noOut )

        OnSubmit next ->
            ( model, out0 [ sendNow next ] )

        OnColQuery ->
            ( setDataResult LoadingSlowly model
            , out0 [ getProjectColumn apis model.form.colid OnColQueryAck ]
            )

        OnColQueryAck result ->
            let
                data =
                    setDataResult result model
            in
            case parseErr result data.refresh_trial of
                Authenticate ->
                    ( setDataResult NotAsked model
                    , out0 [ Ports.raiseAuthModal (uctxFromUser model.user) ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep OnColQuery 500 ] [ DoUpdateToken ] )

                OkAuth d ->
                    let
                        form =
                            model.form

                        orig_form =
                            { form
                                | pos = Just d.pos
                                , post =
                                    Dict.fromList
                                        [ ( "name", d.name )
                                        , ( "description", withDefault "" d.description )
                                        , ( "color", withDefault "" d.color )
                                        ]
                            }
                    in
                    ( { data | orig_form = orig_form, form = orig_form } |> setColor (Dict.get "color" orig_form.post)
                    , noOut
                    )

                _ ->
                    ( data, noOut )

        OnColAdd ->
            ( setDataResult LoadingSlowly model
            , out0 [ addProjectColumn apis model.form OnColAddAck ]
            )

        OnColAddAck result ->
            case result of
                Success d ->
                    ( setDataResult result model, Out [ send (OnClose { reset = True, link = "" }) ] [] (Just ( AddColumn, d )) )

                _ ->
                    ( setDataResult result model, noOut )

        OnColEdit ->
            ( setDataResult LoadingSlowly model
            , out0 [ updateProjectColumn apis model.form OnColEditAck ]
            )

        OnColEditAck result ->
            case result of
                Success d ->
                    let
                        col =
                            { id = model.form.colid
                            , name = getFromDict2 "name" model.form.post model.orig_form.post |> withDefault ""
                            , color = getFromDict2 "color" model.form.post model.orig_form.post
                            , pos = model.form.pos |> withDefault 0
                            , col_type = ProjectColumnType.NormalColumn

                            -- ignored
                            , cards = []
                            }
                    in
                    ( setDataResult result model, Out [ send (OnClose { reset = True, link = "" }) ] [] (Just ( EditColumn, col )) )

                _ ->
                    ( setDataResult result model, noOut )

        -- Confirm Modal
        DoModalConfirmOpen msg mess ->
            ( { model | modal_confirm = ModalConfirm.open msg mess model.modal_confirm }, noOut )

        DoModalConfirmClose _ ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, noOut )

        DoModalConfirmSend ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, out0 [ send model.modal_confirm.msg ] )

        -- Color Picker
        OpenColor ->
            ( { model | colorPicker = ColorPicker.open model.colorPicker }
            , if not model.colorPicker.isOpen then
                out0 [ Ports.outsideClickClose "cancelColorFromJs" "colorPicker" ]

              else
                noOut
            )

        CloseColor ->
            ( { model | colorPicker = ColorPicker.close model.colorPicker }, noOut )

        SelectColor color ->
            let
                newPicker =
                    model.colorPicker
                        |> ColorPicker.setColor (Just color)
                        |> ColorPicker.close

                form =
                    model.form

                newForm =
                    { form | post = Dict.insert "color" color form.post }
            in
            ( { model | colorPicker = newPicker, form = newForm }, out0 [ Ports.click "body" ] )

        -- Common
        NoMsg ->
            ( model, noOut )

        LogErr err ->
            ( model, out0 [ Ports.logErr err ] )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    [ Ports.cancelColorFromJs (always CloseColor)
    ]
        ++ (if model.isActive then
                -- Extra module used when modal is active
                [ Ports.mcPD Ports.closeModalFromJs LogErr OnClose
                , Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
                ]

            else
                []
           )



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    {}


view : Op -> State -> Html Msg
view op (State model) =
    if model.isActive2 then
        div []
            [ viewModal op model
            , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
            ]

    else
        text ""


viewModal : Op -> Model -> Html Msg
viewModal op model =
    div
        [ id "ProjectColumnModalModal"
        , class "modal is-light modal-fx-fadeIn"
        , classList [ ( "is-active", model.isActive ) ]
        , attribute "data-modal-close" "closeModalFromJs"
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "ProjectColumnModalModal"
            , onClick (OnCloseSafe "" "")
            ]
            []
        , div [ class "modal-content is-small" ]
            [ viewModalContent op model
            ]

        --, button [ class "modal-close is-large", onClick (OnCloseSafe "" "") ] []
        ]


viewModalContent : Op -> Model -> Html Msg
viewModalContent op model =
    let
        name =
            Dict.get "name" model.form.post

        description =
            Dict.get "description" model.form.post

        isLoading =
            Loading.isLoading model.data_result

        ( isSendable_, onSubmit ) =
            case model.modal_type of
                AddColumn ->
                    ( isSendable model, OnColAdd )

                EditColumn ->
                    ( model.form /= model.orig_form, OnColEdit )

                DeleteColumn ->
                    -- @TODO
                    ( True, NoMsg )
    in
    div [ class "modal-card submitFocus" ]
        [ div [ class "modal-card-head" ]
            [ div [ class "modal-card-title is-wrapped is-size-6 has-text-weight-semibold" ]
                [ textH (toTitle model.modal_type) ]
            ]
        , div [ class "modal-card-body" ]
            [ div [ class "field" ]
                [ div [ class "label" ] [ text T.name, span [ class "has-text-weight-normal" ] [ text " *" ] ]
                , div [ class "control" ]
                    [ input
                        [ class "input autofocus"
                        , type_ "text"
                        , placeholder T.name
                        , value (withDefault "" name)
                        , onInput <| OnChangePost "name"
                        ]
                        []
                    ]
                ]
            , p [ class "field is-horizontal mt-4" ]
                [ label [ class "field-label label" ] [ text T.color ]
                , div [ class "field-body control" ]
                    [ ColorPicker.view { data = model.colorPicker, onOpen = OpenColor, onClose = CloseColor, onSelect = SelectColor } ]
                ]
            , div [ class "field" ]
                [ div [ class "label" ] [ text T.description ]
                , div [ class "control" ]
                    [ textarea
                        [ class "textarea"
                        , rows 3
                        , placeholder T.leaveCommentOpt
                        , value (withDefault "" description)
                        , onInput <| OnChangePost "description"
                        , autofocus True
                        ]
                        []
                    , p [ class "help-label" ] [ textH T.columnAboutHelp ]
                    ]
                ]
            ]
        , div [ class "modal-card-foot", attribute "style" "display: block;" ]
            [ case model.data_result of
                Failure err ->
                    div [ class "field" ] [ viewGqlErrors err ]

                _ ->
                    text ""
            , div [ class "field level is-mobile" ]
                [ div [ class "level-left" ]
                    [ button
                        [ class "button is-light"
                        , onClick (OnCloseSafe "" "")
                        ]
                        [ textH T.cancel ]
                    ]
                , div [ class "level-right" ]
                    [ button
                        ([ class "button is-light is-success defaultSubmit"
                         , classList [ ( "is-loading", isLoading ) ]
                         , disabled (not isSendable_)
                         ]
                            ++ ternary (isSendable_ && not isLoading)
                                [ onClick onSubmit ]
                                []
                        )
                        [ textH (toSubmit model.modal_type) ]
                    ]
                ]
            ]
        ]



--
-- Utils
--


getFromDict2 : String -> Post -> Post -> Maybe String
getFromDict2 key dict1 dict2 =
    case Dict.get key dict1 of
        Just a ->
            Just a

        Nothing ->
            Dict.get key dict2
