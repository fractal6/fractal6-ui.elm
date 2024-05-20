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


port module Components.LinkTensionPanel exposing (ColTarget, Msg(..), State, hasTargets_, init, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Bulk exposing (UserState(..), uctxFromUser)
import Bulk.Bulma as B
import Bulk.Codecs exposing (DocType(..))
import Bulk.Error exposing (viewGqlErrors)
import Bulk.View exposing (action2icon, tensionIcon3, viewTensionLight)
import Components.LabelSearchPanel as LabelSearchPanel
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.TreeMenu as TreeMenu exposing (viewSelectorTree)
import Dict exposing (Dict)
import Extra exposing (ternary, textH, unwrap, unwrap2, upH)
import Extra.Events exposing (onClickPD, onClickSP, onKeydown)
import Form exposing (isPostEmpty)
import Fractal.Enum.ProjectColumnType as ProjectColumnType
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, autofocus, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, style, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), isFailure, isSuccess, withDefaultData, withMapData, withMaybeData)
import Maybe exposing (withDefault)
import ModelSchema exposing (FocusNode, IdPayload, LocalGraph, Node, NodesDict, Post, ProjectCard, ProjectColumn, TensionLight, UserCtx, initFocusNode, node2focus)
import Org.Tensions exposing (TypeFilter(..), defaultTypeFilter, typeDecoder, typeFilter2Text)
import Ports
import Query.QueryProject exposing (addProjectCard, addProjectColumn, getNoStatusCol, updateProjectColumn)
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
    , projectid : String
    , noStatusCol : ColTarget
    , target : FocusNode
    , data_result : GqlData (List TensionLight)
    , add_result : GqlData Bool
    , selected : List String
    , form : TensionQuery -- user inputs
    , typeFilter : TypeFilter

    -- Common
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg

    -- Components
    , isOpenTargetFilter : Bool
    , isOpenTypeFilter : Bool
    , labelSearchPanel : LabelSearchPanel.State
    }


type alias ColTarget =
    { id : String
    , cards_len : Int
    }


initModel : String -> UserState -> Model
initModel projectid user =
    { user = user
    , isOpen = False
    , projectid = projectid
    , noStatusCol = { id = "", cards_len = 0 }
    , target = initFocusNode
    , data_result = NotAsked
    , add_result = NotAsked
    , selected = []
    , form = initTensionQuery |> (\x -> { x | first = 100, projectid = Just projectid, inProject = False })
    , typeFilter = AllTypes

    -- Components
    , isOpenTargetFilter = False
    , isOpenTypeFilter = False
    , labelSearchPanel = LabelSearchPanel.load Nothing user

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }


init : String -> UserState -> State
init projectid user =
    initModel projectid user |> State



-- Global methods


hasTargets_ : State -> Bool
hasTargets_ (State model) =
    List.length model.form.targetids > 0



-- State Controls


resetModel : Model -> Model
resetModel model =
    initModel model.projectid model.user


setDataResult : GqlData (List TensionLight) -> Model -> Model
setDataResult result model =
    { model | data_result = result }


resetQuery : Model -> Model
resetQuery model =
    { model | selected = [], isOpenTypeFilter = False, isOpenTargetFilter = False }



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
      OnOpen (Maybe ColTarget)
    | OnOutsideClickClose
    | OnClose
    | OnChangeTarget (GqlData NodesDict) Node
    | SetTargets (GqlData LocalGraph) (List String)
    | OnSubmit (Time.Posix -> Msg)
    | OnQueryData
    | OnDataAck (GqlData (List TensionLight))
    | OnNoStatusColAck (GqlData (Maybe ColTarget))
    | OnSearchInput String
    | OnSearchKeydown Int
    | OnChangeTypeFilter TypeFilter
    | OnSelect String
    | OnSelectAll
    | OnAddToProject
    | OnAddAck (GqlData (List ProjectCard))
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Common
    | NoMsg
    | LogErr String
      -- Components
    | OnToggleTargetFilter
    | OnToggleTypeFilter
    | LabelSearchPanelMsg LabelSearchPanel.Msg


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe ( String, List ProjectCard )
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
        OnOpen colTarget ->
            ( { model | isOpen = True }
            , out0
                [ --Ports.bulma_driver "linkTensionPanel"
                  sendSleep OnOutsideClickClose 500
                , case colTarget of
                    Just c ->
                        send (OnNoStatusColAck (Success colTarget))

                    Nothing ->
                        getNoStatusCol apis model.projectid OnNoStatusColAck
                ]
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
            ( resetQuery model |> setDataResult LoadingSlowly
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
                    ( data, noOut )

                _ ->
                    ( data, noOut )

        OnNoStatusColAck result ->
            -- @debug: Succedd of fail silently...
            case result of
                Success d ->
                    case d of
                        Just col ->
                            ( { model | noStatusCol = col }, noOut )

                        Nothing ->
                            let
                                form =
                                    { uctx = uctxFromUser model.user
                                    , projectid = model.projectid
                                    , colid = ""
                                    , col_type = Just ProjectColumnType.NoStatusColumn
                                    , pos = Just -1
                                    , post = Dict.fromList [ ( "name", "No Status" ) ]
                                    }
                            in
                            if model.noStatusCol.id == "_" then
                                ( model, noOut )

                            else
                                ( { model | noStatusCol = { id = "_", cards_len = 0 } }
                                , out0 [ addProjectColumn apis form (withMapData (\x -> Just { id = x.id, cards_len = 0 }) >> OnNoStatusColAck) ]
                                )

                _ ->
                    ( model, noOut )

        OnSearchInput val ->
            let
                form =
                    model.form
            in
            ( { model | form = { form | pattern = ternary (val == "") Nothing (Just val) } }, noOut )

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

        OnChangeTypeFilter value ->
            let
                form =
                    model.form

                newForm =
                    if List.member value [ OneType TensionType.Announcement, OneType TensionType.Governance, OneType TensionType.Help ] then
                        -- Only Open+Closed tensions
                        { form
                            | type_ = typeDecoder value
                            , status = Nothing
                        }

                    else
                        -- Only Open tensions
                        { form
                            | type_ = typeDecoder value
                            , status = Just TensionStatus.Open
                        }
            in
            ( { model | typeFilter = value, form = newForm }, out0 [ send OnQueryData ] )

        OnSelect tid ->
            case LE.elemIndex tid model.selected of
                Just i ->
                    ( { model | selected = LE.removeAt i model.selected }, noOut )

                Nothing ->
                    ( { model | selected = tid :: model.selected }, noOut )

        OnSelectAll ->
            if List.length model.selected == 0 then
                ( { model | selected = withMapData (List.map .id) model.data_result |> withDefaultData [] }, noOut )

            else
                ( { model | selected = [] }, noOut )

        OnAddToProject ->
            let
                form =
                    { uctx = uctxFromUser model.user
                    , title = ""
                    , colid = model.noStatusCol.id
                    , pos = model.noStatusCol.cards_len
                    , post = Dict.empty
                    , tids = List.map Just model.selected
                    }
            in
            ( { model | add_result = Loading }, out0 [ addProjectCard apis form OnAddAck ] )

        OnAddAck result ->
            case result of
                Success d ->
                    ( { model | add_result = Success True }
                    , Out [ send OnClose ] [] (Just ( model.noStatusCol.id, d ))
                    )

                _ ->
                    ( { model | add_result = withMapData (\_ -> True) result }, noOut )

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
        OnToggleTargetFilter ->
            ( { model | isOpenTargetFilter = not model.isOpenTargetFilter }, noOut )

        OnToggleTypeFilter ->
            ( { model | isOpenTypeFilter = not model.isOpenTypeFilter }, noOut )

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


view : GqlData NodesDict -> GqlData LocalGraph -> State -> Html Msg
view tree_data path_data (State model) =
    div
        [ id "linkTensionPanel"
        , class "side-menu is-medium"
        , classList [ ( "off", not model.isOpen ) ]
        ]
        [ viewPanel tree_data model
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        ]


viewPanel : GqlData NodesDict -> Model -> Html Msg
viewPanel tree_data model =
    let
        typeFilter_hthml =
            B.dropdown
                { dropdown_id = "type-filter-side"
                , isOpen = model.isOpenTypeFilter
                , dropdown_cls = "is-right"
                , button_cls = "is-small"
                , button_html = ternary (model.typeFilter /= defaultTypeFilter) (span [] [ span [ class "badge is-link-back" ] [], text T.type_ ]) (text T.type_)
                , msg = OnToggleTypeFilter
                , menu_cls = ""
                , content_cls = "p-0 has-border-light"
                , content_html =
                    div [] <|
                        [ div [ class "dropdown-item button-light", onClick <| OnChangeTypeFilter AllTypes ]
                            [ ternary (model.typeFilter == AllTypes) A.checked A.unchecked, text (typeFilter2Text AllTypes) ]
                        ]
                            ++ List.map
                                (\t ->
                                    div [ class "dropdown-item button-light", onClick <| OnChangeTypeFilter (OneType t) ]
                                        [ ternary (model.typeFilter == OneType t) A.checked A.unchecked, tensionIcon3 t ]
                                )
                                TensionType.list
                }

        labelFilter_html =
            span []
                [ span
                    [ class "button is-small"
                    , onClick (LabelSearchPanelMsg (LabelSearchPanel.OnOpen [ model.target.nameid ] (Just True)))
                    ]
                    [ ternary (model.form.labels /= []) (span [ class "badge is-link-back" ] []) (text "")
                    , text T.label
                    , A.icon "ml-2 icon-chevron-down1 icon-tiny"
                    ]
                , LabelSearchPanel.view { selectedLabels = model.form.labels, targets = [ model.target.nameid ], isRight = True } model.labelSearchPanel
                    |> Html.map LabelSearchPanelMsg
                ]

        filters_html =
            [ typeFilter_hthml, labelFilter_html ]

        hasItem =
            List.length model.selected > 0

        onSubmit =
            ternary hasItem
                OnAddToProject
                NoMsg
    in
    div [ class "panel" ] <|
        [ div [ class "header-block" ]
            [ div [ class "panel-heading" ] [ text T.addLinkedTensions, button [ class "delete is-pulled-right", onClick OnClose ] [] ]
            , div [ class "panel-block no-border" ]
                [ B.dropdown
                    { dropdown_id = "link-circle-source"
                    , isOpen = model.isOpenTargetFilter
                    , dropdown_cls = "mr-2"
                    , button_cls = "is-small"
                    , button_html = A.icon1 (action2icon { doc_type = NODE model.target.type_ }) model.target.name
                    , msg = OnToggleTargetFilter
                    , menu_cls = ""
                    , content_cls = "p-0 has-border-light"
                    , content_html = viewSelectorTree (OnChangeTarget tree_data) [ model.target.nameid ] tree_data
                    }
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
            , case model.data_result of
                Success data ->
                    if List.length data > 0 then
                        -- @warning: height of this element manually to be able to work on relative
                        -- height percentage for .parent-block (100% of the parent height).
                        div [ class "panel-block is-top is-size-7" ]
                            [ label [ class "is-h", onClickPD OnSelectAll ]
                                [ input [ type_ "checkbox", checked (List.length model.selected == List.length data) ] []
                                , text ((List.length data |> String.fromInt) ++ " most recent tensions")
                                ]
                            , span [ class "is-pushed-right" ] filters_html
                            ]

                    else
                        p [ class "panel-block" ] [ text T.noResults, span [ class "is-pushed-right" ] filters_html ]

                _ ->
                    text ""
            ]
        , div [ class "main-block" ] <|
            case model.data_result of
                Success data ->
                    List.map
                        (\t ->
                            Lazy.lazy2 viewLineSelect t model.selected
                        )
                        data

                Failure err ->
                    [ viewGqlErrors err ]

                LoadingSlowly ->
                    [ div [ class "spinner" ] [] ]

                _ ->
                    []
        , div [ class "panel-block footer-block" ]
            [ case model.add_result of
                Failure err ->
                    viewGqlErrors err

                _ ->
                    text ""
            , button
                [ class "button is-small is-success s-center is-pushed-right"
                , onClick onSubmit
                , classList [ ( "is-loading", Loading.isLoading model.add_result ) ]
                , disabled (not hasItem)
                ]
                [ text T.addSelectedItems ]
            ]
        ]


viewLineSelect : TensionLight -> List String -> Html Msg
viewLineSelect t selected =
    label [ class "panel-block tensionLight" ]
        [ input
            [ type_ "checkbox"
            , onClickPD (OnSelect t.id)
            , checked (List.member t.id selected)
            ]
            []
        , viewTensionLight t
        ]
