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


port module Components.LinkTensionPanel exposing (Msg(..), State, hasTargets_, init, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Bulk exposing (UserState(..), uctxFromUser)
import Bulk.Bulma as B
import Bulk.Codecs exposing (DocType(..))
import Bulk.Error exposing (viewGqlErrors)
import Bulk.View exposing (action2icon)
import Components.LabelSearchPanel as LabelSearchPanel
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.TreeMenu as TreeMenu exposing (viewSelectorTree)
import Dict exposing (Dict)
import Extra exposing (ternary, textH, unwrap, upH)
import Extra.Events exposing (onClickPD, onClickSP, onKeydown)
import Form exposing (isPostEmpty)
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, autofocus, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, style, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), isFailure, isSuccess, withMaybeData)
import Maybe exposing (withDefault)
import ModelSchema exposing (FocusNode, LocalGraph, Node, NodesDict, Post, TensionLight, UserCtx, initFocusNode, node2focus)
import Ports
import Requests exposing (TensionQuery, fetchTensionsLight, initTensionQuery)
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
    , isOpen : Bool
    , target : FocusNode
    , data_result : GqlData (List TensionLight)
    , selected : List String
    , isSelectAll : Bool
    , form : TensionQuery -- user inputs

    -- Common
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    , labelSearchPanel : LabelSearchPanel.State
    }


initModel : UserState -> Model
initModel user =
    { user = user
    , isOpen = False
    , target = initFocusNode
    , data_result = NotAsked
    , selected = []
    , isSelectAll = False
    , form = initTensionQuery |> (\x -> { x | first = 100 })
    , labelSearchPanel = LabelSearchPanel.load Nothing user

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }


init : UserState -> State
init user =
    initModel user |> State



-- Global methods


hasTargets_ : State -> Bool
hasTargets_ (State model) =
    List.length model.form.targetids > 0



-- State Controls


resetModel : Model -> Model
resetModel model =
    initModel model.user


setDataResult : GqlData (List TensionLight) -> Model -> Model
setDataResult result model =
    { model | data_result = result }



-- utils


canExitSafe : Model -> Bool
canExitSafe model =
    -- Condition to close safely (e.g. empty form data)
    not (hasData model && withMaybeData model.data_result == Nothing)


hasData : Model -> Bool
hasData model =
    -- When you can commit (e.g. empty form data)
    List.length model.selected /= 0



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = -- Data
      OnOpen
    | OnOutsideClickClose
    | OnClose
    | OnChangeTarget (GqlData NodesDict) Node
    | SetTargets (GqlData LocalGraph) (List String)
    | OnSubmit (Time.Posix -> Msg)
    | OnQueryData
    | OnDataAck (GqlData (List TensionLight))
    | OnSearchInput String
    | OnSearchKeydown Int
    | OnSelect String
    | OnSelectAll
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Common
    | NoMsg
    | LogErr String
    | LabelSearchPanelMsg LabelSearchPanel.Msg


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe ( Bool, List String ) -- List of tid
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
        OnOpen ->
            ( { model | isOpen = True }
            , out0 [ Ports.bulma_driver "linkTensionPanel", sendSleep OnOutsideClickClose 500 ]
            )

        OnOutsideClickClose ->
            ( model, out0 [ Ports.outsideClickClose "closeLinkTensionPanelFromJs" "linkTensionPanel" ] )

        OnClose ->
            -- @warning: Ports.click to reset the outsideClickClose handler.
            ( resetModel model, out0 [ Ports.click "" ] )

        OnChangeTarget tree_data node ->
            let
                target =
                    node2focus node

                targetids =
                    TreeMenu.getList node.nameid tree_data

                form =
                    model.form

                newForm =
                    { form | targetids = targetids }
            in
            ( { model | target = target, form = newForm }, out0 [ send OnQueryData ] )

        SetTargets target_data targetids ->
            let
                target =
                    withMaybeData target_data |> unwrap initFocusNode .focus

                form =
                    model.form

                newForm =
                    { form | targetids = targetids }
            in
            ( { model | target = target, form = newForm }
            , out0 [ send OnQueryData ]
            )

        OnSubmit next ->
            ( model, out0 [ sendNow next ] )

        OnQueryData ->
            ( setDataResult LoadingSlowly model
            , out0 [ fetchTensionsLight apis model.form OnDataAck ]
            )

        OnDataAck result ->
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
                    ( { data | refresh_trial = i }, out2 [ sendSleep OnQueryData 500 ] [ DoUpdateToken ] )

                OkAuth d ->
                    ( data
                    , Out [] [] (Just ( True, List.map .id d ))
                    )

                _ ->
                    ( data, noOut )

        OnSearchInput val ->
            let
                form =
                    model.form
            in
            ( { model | form = { form | pattern = Just val } }, noOut )

        OnSearchKeydown key ->
            case key of
                13 ->
                    --ENTER
                    ( model, out0 [ send OnQueryData ] )

                27 ->
                    --ESC
                    ( model, out0 [ send (OnSearchInput "") ] )

                _ ->
                    ( model, noOut )

        OnSelect tid ->
            case LE.elemIndex tid model.selected of
                Just i ->
                    ( { model | selected = LE.removeAt i model.selected }, noOut )

                Nothing ->
                    ( { model | selected = tid :: model.selected }, noOut )

        OnSelectAll ->
            ( { model | isSelectAll = not model.isSelectAll }, noOut )

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

        -- Components
        LabelSearchPanelMsg msg ->
            let
                ( data, out ) =
                    LabelSearchPanel.update apis msg model.labelSearchPanel

                labels =
                    Maybe.map
                        (\r ->
                            if Tuple.first r then
                                model.form.labels ++ [ Tuple.second r ]

                            else
                                List.filter (\x -> x.name /= (Tuple.second r).name) model.form.labels
                        )
                        out.result
                        |> withDefault model.form.labels

                form =
                    model.form

                newForm =
                    { form | labels = labels }

                cmds =
                    if form.labels /= newForm.labels then
                        [ send OnQueryData ]

                    else
                        []

                gcmds =
                    []
            in
            ( { model | labelSearchPanel = data, form = newForm }, out2 (List.map (\m -> Cmd.map LabelSearchPanelMsg m) out.cmds |> List.append cmds) (out.gcmds ++ gcmds) )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    if model.isOpen then
        [ Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
        , closeLinkTensionPanelFromJs (always OnClose)
        ]
            ++ (LabelSearchPanel.subscriptions model.labelSearchPanel |> List.map (\s -> Sub.map LabelSearchPanelMsg s))

    else
        []


port closeLinkTensionPanelFromJs : (() -> msg) -> Sub msg



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    { tree_data : GqlData NodesDict
    , path_data : GqlData LocalGraph
    }


view : Op -> State -> Html Msg
view op (State model) =
    div
        [ id "linkTensionPanel"
        , class "side-menu"
        , classList [ ( "off", not model.isOpen ) ]
        ]
        [ viewPanel op model
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        ]


viewPanel : Op -> Model -> Html Msg
viewPanel op model =
    let
        labelFilter_html =
            span [ class "is-pushed-right" ]
                [ span
                    [ class "button is-small"
                    , onClick (LabelSearchPanelMsg (LabelSearchPanel.OnOpen [ model.target.nameid ] (Just True)))
                    ]
                    [ ternary (model.form.labels /= []) (span [ class "badge is-link2" ] []) (text "")
                    , text T.label
                    , A.icon "ml-2 icon-chevron-down1 icon-tiny"
                    ]
                , LabelSearchPanel.view { selectedLabels = model.form.labels, targets = [ model.target.nameid ], isRight = True } model.labelSearchPanel
                    |> Html.map LabelSearchPanelMsg
                ]
    in
    div [ class "panel", style "width" "100%" ] <|
        [ div [ class "panel-heading" ] [ text "Add tension to this project", button [ class "delete is-pulled-right", onClick OnClose ] [] ]
        , div [ class "panel-block no-border" ]
            [ B.dropdown "link-circle-source"
                "mr-2"
                "is-small"
                (A.icon1 (action2icon { doc_type = NODE model.target.type_ }) model.target.name)
                (viewSelectorTree (OnChangeTarget op.tree_data) [ model.target.nameid ] op.tree_data)
            , div [ class "control has-icons-left" ]
                [ input
                    [ class "input is-small"
                    , type_ "text"
                    , placeholder T.searchTensions
                    , autofocus False
                    , value (withDefault "" model.form.pattern)
                    , onInput OnSearchInput
                    , onKeydown OnSearchKeydown
                    ]
                    []
                , span [ class "icon is-left" ] [ A.icon "icon-search" ]
                ]
            ]
        ]
            ++ (case model.data_result of
                    Success data ->
                        (if List.length data /= 0 then
                            [ div [ class "panel-block is-top" ]
                                [ label [ class "is-h", onClickPD OnSelectAll ]
                                    [ input [ type_ "checkbox", checked model.isSelectAll ] []
                                    , text ((List.length data |> String.fromInt) ++ " most recent tensions")
                                    ]
                                , labelFilter_html
                                ]
                            ]

                         else
                            []
                        )
                            ++ List.map
                                (\t ->
                                    label [ class "panel-block", onClickPD (OnSelect t.id) ]
                                        [ input [ type_ "checkbox", checked (List.member t.id model.selected || model.isSelectAll) ] []
                                        , text t.title
                                        ]
                                )
                                data
                            ++ (if List.length data == 0 then
                                    [ p [ class "panel-block" ] [ text T.noResults, labelFilter_html ] ]

                                else
                                    let
                                        hasNoItem =
                                            List.length model.selected == 0
                                    in
                                    [ div [ class "panel-block mt-4 is-pulled-right" ]
                                        [ button
                                            [ class "button is-success s-outlined s-fullwidth"
                                            , disabled hasNoItem
                                            ]
                                            [ text "Add selected items" ]
                                        ]
                                    ]
                               )

                    Failure err ->
                        [ viewGqlErrors err ]

                    LoadingSlowly ->
                        [ div [ class "spinner" ] [] ]

                    _ ->
                        []
               )
