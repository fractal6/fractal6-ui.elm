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


module Components.Board exposing (Msg(..), State, board_result, init, nodeID, subscriptions, update, view)

import Assets as A
import Browser.Dom as Dom
import Browser.Events as Events
import Bulk exposing (UserState(..), uctxFromUser)
import Bulk.Bulma as B
import Bulk.Codecs exposing (ActionType(..), NodeFocus, getTensionCharac, nid2rootid)
import Bulk.View exposing (action2icon, action2str, statusColor, tensionIcon)
import Components.LabelSearchPanel exposing (viewLabels)
import Components.LinkTensionPanel as LinkTensionPanel exposing (ColTarget)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.ProjectColumnModal as ProjectColumnModal exposing (ModalType(..))
import Dict exposing (Dict)
import Dom
import Extra exposing (insertAt, send, sendSleep, ternary, unwrap)
import Extra.Events exposing (onClickPD, onDragEnd, onDragEnter, onDragLeave, onDragOverPD, onDragStart, onMousedownPD)
import Fractal.Enum.ProjectColumnType as ProjectColumnType
import Fractal.Enum.ProjectStatus as ProjectStatus
import Fractal.Enum.TensionStatus as TensionStatus
import Generated.Route as Route exposing (toHref)
import Html exposing (Html, a, br, div, hr, i, span, text, textarea)
import Html.Attributes exposing (attribute, autofocus, class, classList, href, id, readonly, rows, style, target, title, value)
import Html.Events exposing (onBlur, onClick, onInput, onMouseEnter, onMouseLeave)
import Html.Lazy as Lazy
import Json.Decode as JD
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), isLoading, withMapData, withMaybeData, withMaybeMapData)
import Maybe exposing (withDefault)
import ModelSchema exposing (CardKind(..), IdPayload, Label, Post, ProjectCard, ProjectColumn, ProjectColumnLite, ProjectData, ProjectDraft, Tension, TensionProject, User, UserCtx, Username)
import Ports
import Query.QueryProject exposing (addProjectCard, deleteProjectColumns, moveProjectCard, moveProjectColumn, removeProjectCards)
import Scroll exposing (scrollToSubBottom)
import Session exposing (Apis, GlobalCmd(..), SessionCommon)
import Task
import Text as T


type State
    = State Model


nodeID =
    "projectView"


type alias Model =
    { node_focus : NodeFocus
    , projectid : String
    , isProjectAdmin : Bool
    , project : ProjectData
    , hasTaskMove : Bool
    , hasNewCol : Bool
    , isAddingDraft : Maybe DraftForm
    , boardHeight : Maybe Float
    , movingCard : Maybe ProjectCard
    , movingCol : Maybe ProjectColumn
    , movingHoverCol : Maybe { pos : Int, colid : String, length : Int }
    , movingHoverT : Maybe { pos : Int, cardid : String, colid : String }
    , dragCount : Int
    , draging : Bool
    , board_result : GqlData String -- track board remote result silently
    , cardHover : String
    , cardEdit : String
    , activeCards : List String
    , colEdit : String
    , cardEditDropdown : Maybe ProjectCard
    , cardEditDropdownX : Float
    , cardEditDropdownY : Float

    -- Components
    , projectColumnModal : ProjectColumnModal.State

    -- Common
    , session : SessionCommon
    , refresh_trial : Int -- use to refresh user token
    , empty : {}
    , modal_confirm : ModalConfirm Msg
    }


type alias DraftForm =
    { uctx : UserCtx
    , title : String
    , colid : String
    , pos : Int
    , post : Post
    , tids : List (Maybe String)
    , blur_safe : Bool
    , pending : Bool
    }


initModel : String -> NodeFocus -> SessionCommon -> Model
initModel projectid focus session =
    { node_focus = focus
    , projectid = projectid
    , project = ProjectData "" "" Nothing ProjectStatus.Open [] [] [] False False
    , hasTaskMove = True
    , hasNewCol = True
    , isAddingDraft = Nothing
    , isProjectAdmin = False

    -- Board
    , boardHeight = Nothing
    , movingCard = Nothing
    , movingCol = Nothing
    , movingHoverCol = Nothing
    , movingHoverT = Nothing
    , dragCount = 0
    , draging = False

    --
    , board_result = NotAsked
    , cardHover = ""
    , cardEdit = ""
    , activeCards = []
    , colEdit = ""
    , cardEditDropdown = Nothing
    , cardEditDropdownX = 0
    , cardEditDropdownY = 0

    -- Components
    , projectColumnModal = ProjectColumnModal.init projectid session

    -- Common
    , session = session
    , refresh_trial = 0
    , empty = {}
    , modal_confirm = ModalConfirm.init NoMsg
    }


init : String -> NodeFocus -> SessionCommon -> State
init projectid focus session =
    initModel projectid focus session |> State



--
-- Getters
--


board_result : State -> GqlData String
board_result (State model) =
    model.board_result



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = OnLoad ProjectData
    | OnResize Int Int
    | FitBoard (Result Dom.Error Dom.Element)
    | ScrollToElement String
    | OnClearBoardResult
    | OnSetIsAdmin Bool
      -- Move Column
    | OnMoveColumn ProjectColumn
    | OnMoveColumnEnd
      -- Cards (move, edit, delete)
    | OnMove { pos : Int, colid : String, length : Int } ProjectCard
    | OnMoveEnd
    | OnCancelHov
    | OnMoveEnterCol { pos : Int, colid : String, length : Int } Bool
    | OnMoveLeaveCol
    | OnMoveLeaveCol_
    | OnMoveEnterT { pos : Int, cardid : String, colid : String }
    | OnColDrop String
    | GotCardMoved (GqlData IdPayload)
    | GotColMoved (GqlData IdPayload)
    | OnCardClick (Maybe ProjectCard)
    | OnCardHover String
    | OnCardHoverLeave
    | OnToggleCardEdit String
    | OnToggleColEdit String
    | OnRemoveCard String
    | OnRemoveCardAck (GqlData (List String))
    | OnRemoveColItems String
    | OnDeleteColumn String
    | OnDeleteColumnAck (GqlData (List String))
    | OnSetCardEditDropdown String (Result Dom.Error Dom.Element)
      --
    | OnAddCol
    | OnAddDraft String
    | OnAddTension String
    | OnDraftEdit String
    | OnDraftKeydown Int
    | OnDraftCancel
    | OnAddCardAck (GqlData (List ProjectCard))
    | OpenTensionPane (Maybe ColTarget)
    | OpenCardPane String
    | OnLinkTension ( String, List ProjectCard )
    | OnUpdateCard ProjectCard
    | OnConvertDraft String ProjectDraft
    | OnConvertDraftAck ProjectDraft Tension
      -- Components
    | ProjectColumnModalMsg ProjectColumnModal.Msg
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Common
    | NoMsg
    | LogErr String
    | OnUpdateModel (Model -> Model)


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe ( Bool, GqlData String ) -- define what data is to be returned
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
        OnLoad data ->
            ( { model | project = data }
            , out0
                [ Task.attempt FitBoard (Dom.getElement nodeID)
                , sendSleep (ScrollToElement nodeID) 333
                ]
            )

        OnResize w h ->
            let
                newScreen =
                    { w = w, h = h }
            in
            ( model, out2 [ Task.attempt FitBoard (Dom.getElement nodeID) ] [ DoUpdateScreen newScreen ] )

        FitBoard elt ->
            case elt of
                Ok e ->
                    let
                        h =
                            if e.viewport.height - e.element.y < 511 then
                                -- allow y-scroll here. Substract the header size.
                                e.viewport.height - 79

                            else
                                e.viewport.height - e.element.y
                    in
                    ( { model | boardHeight = Just h }, noOut )

                Err _ ->
                    ( model, noOut )

        ScrollToElement did ->
            ( model, out0 [ Scroll.scrollToElement did NoMsg ] )

        OnClearBoardResult ->
            ( { model | board_result = NotAsked }, noOut )

        OnSetIsAdmin isAdmin ->
            ( { model | isProjectAdmin = isAdmin }, noOut )

        -- Move Columns
        OnMoveColumn col ->
            if col.col_type == ProjectColumnType.NoStatusColumn then
                ( model, noOut )

            else
                ( { model | movingCol = Just col }, noOut )

        OnMoveColumnEnd ->
            let
                newModel =
                    { model | movingCol = Nothing, movingHoverCol = Nothing, movingHoverT = Nothing }
            in
            Maybe.map2
                (\col { pos, colid, length } ->
                    if col.id == colid then
                        ( newModel, noOut )

                    else if (getCol colid model.project |> Maybe.map .col_type) == Just ProjectColumnType.NoStatusColumn then
                        ( newModel, noOut )

                    else
                        -- Do not wait the query to success to move the column.
                        let
                            noStatusCols =
                                List.filter (\x -> x.col_type == ProjectColumnType.NoStatusColumn) model.project.columns

                            noStatusOffsetLocal =
                                noStatusCols
                                    |> List.filter (\x -> List.length x.cards == 0)
                                    |> List.length

                            noStatusOffsetRemote =
                                noStatusCols
                                    |> List.filter (\x -> List.length x.cards /= 0)
                                    |> List.length

                            pj =
                                model.project
                                    -- move a column to the given position
                                    |> (\d -> { d | columns = moveColAt col.id (pos + noStatusOffsetLocal) d.columns })
                        in
                        ( { newModel | project = pj, board_result = Loading }
                        , out0 [ moveProjectColumn apis col.id (pos - noStatusOffsetRemote) GotColMoved ]
                        )
                )
                model.movingCol
                model.movingHoverCol
                |> withDefault
                    ( { model | movingCol = Nothing }, noOut )

        -- Cards
        OnMove col card ->
            ( { model | draging = True, dragCount = 0, movingHoverCol = Just col, movingCard = Just card, isAddingDraft = Nothing }
            , noOut
            )

        OnMoveEnd ->
            let
                newModel =
                    { model | draging = False }
            in
            Maybe.map3
                (\card { pos, colid } c_hover ->
                    if card.id == c_hover.cardid then
                        ( newModel, out0 [ sendSleep OnCancelHov 300 ] )

                    else
                        -- Do not wait the query to success to move the column.
                        let
                            pos_fixed =
                                ternary (colid == card.colid && c_hover.pos > card.pos)
                                    (c_hover.pos - 1)
                                    c_hover.pos

                            pj =
                                model.project
                                    -- Remove the card from old pos
                                    |> (\d -> { d | columns = removeCard card d.columns })
                                    -- Add the card in new pos tension to list
                                    |> (\d -> { d | columns = pushCard { card | colid = colid, pos = pos_fixed } d.columns })
                        in
                        ( { newModel | project = pj, board_result = Loading }
                        , out0
                            [ moveProjectCard apis card.id pos_fixed colid GotCardMoved
                            , send OnCancelHov
                            ]
                        )
                )
                model.movingCard
                model.movingHoverCol
                model.movingHoverT
                |> withDefault
                    ( newModel, out0 [ sendSleep OnCancelHov 300 ] )

        OnCardClick c ->
            -- Highlight the border and show ellipsis on click
            -- or unselect.
            case c of
                Just card ->
                    ( { model | movingCard = c, activeCards = [ card.id ] }, noOut )

                Nothing ->
                    ( { model | movingCard = Nothing, cardEdit = "", activeCards = [] }, noOut )

        OnCancelHov ->
            ( { model | movingHoverCol = Nothing, movingHoverT = Nothing }, noOut )

        OnMoveEnterCol hover isLast ->
            -- @DEBUG: How to optimize / simplify that ?
            -- Does "dragCount" still usefull ??
            if Just hover == model.movingHoverCol && not isLast then
                ( { model | dragCount = 1 }, noOut )

            else if Just hover == model.movingHoverCol && isLast then
                let
                    c_h =
                        Maybe.map (\ch -> { ch | pos = hover.length }) model.movingHoverT
                in
                ( { model | movingHoverT = c_h }, noOut )

            else
                let
                    -- Add a virtual card hover in empty columns in order to be able to move card there.
                    ( last_cardid, n_cards ) =
                        LE.find (\x -> x.id == hover.colid) model.project.columns
                            |> unwrap ( "", -1 )
                                (\cols -> ( LE.last cols.cards |> unwrap "" .id, List.length cols.cards ))

                    mht_ =
                        if Maybe.map .colid model.movingHoverT /= Just hover.colid then
                            Nothing

                        else
                            model.movingHoverT

                    mht =
                        case mht_ of
                            Nothing ->
                                if n_cards == 0 then
                                    Just { pos = 0, cardid = "", colid = hover.colid }

                                else if n_cards > 0 then
                                    Just { pos = n_cards, cardid = last_cardid, colid = hover.colid }

                                else
                                    Nothing

                            Just _ ->
                                model.movingHoverT
                in
                ( { model | dragCount = 1, movingHoverCol = Just hover, movingHoverT = mht }, noOut )

        OnMoveLeaveCol ->
            ( { model | dragCount = model.dragCount - 1 }, out0 [ sendSleep OnMoveLeaveCol_ 15 ] )

        OnMoveLeaveCol_ ->
            if model.dragCount < 0 && model.draging then
                ( model, out0 [ send OnCancelHov ] )

            else
                ( model, noOut )

        OnMoveEnterT hover ->
            ( { model | movingHoverT = Just hover }, noOut )

        OnColDrop colid ->
            ( model, noOut )

        GotCardMoved result ->
            ( { model | board_result = withMapData .id result }, noOut )

        GotColMoved result ->
            ( { model | board_result = withMapData .id result }, noOut )

        OnAddCol ->
            let
                pos =
                    model.project.columns |> List.filter (\x -> x.pos >= 0) |> List.length
            in
            ( model, out0 [ Cmd.map ProjectColumnModalMsg (send (ProjectColumnModal.OnOpenAdd pos)) ] )

        OnAddDraft colid ->
            let
                title =
                    Maybe.map .title model.isAddingDraft |> withDefault ""

                pos =
                    LE.find (\b -> b.id == colid) model.project.columns |> unwrap [] .cards |> List.length

                uctx =
                    uctxFromUser model.session.user
            in
            ( { model | isAddingDraft = Just { uctx = uctx, tids = [ Nothing ], post = Dict.empty, title = title, colid = colid, pos = pos, blur_safe = True, pending = False } }
            , out0
                [ Ports.focusOn "draft-card-editable"
                , scrollToSubBottom colid NoMsg
                , sendSleep (OnUpdateModel (\m -> { m | isAddingDraft = Maybe.map (\draft -> { draft | blur_safe = False }) m.isAddingDraft })) 1000
                ]
            )

        OnAddTension colid ->
            let
                ( draftTitle, draftMessage ) =
                    Maybe.map .title model.isAddingDraft
                        |> withDefault ""
                        |> splitDraftBody

                pos =
                    LE.find (\b -> b.id == colid) model.project.columns |> unwrap [] .cards |> List.length

                draft =
                    { id = ""
                    , title = draftTitle
                    , message = ternary (draftMessage == "") Nothing (Just draftMessage)
                    , createdAt = ""
                    , createdBy = Username ""
                    , labels = Nothing
                    , assignees = Nothing
                    , cardid = ""
                    , colid = colid
                    , pos = pos
                    }

                conversion =
                    { draft = draft, project = toTensionProject colid pos model.project }
            in
            ( { model | isAddingDraft = Maybe.map (\f -> { f | pending = True, blur_safe = True }) model.isAddingDraft }
            , out2
                [ sendSleep (OnUpdateModel (\m -> { m | isAddingDraft = Nothing })) 800 ]
                [ DoCreateTension model.node_focus.nameid Nothing (Just conversion) ]
            )

        OnDraftEdit val ->
            ( { model | isAddingDraft = Maybe.map (\f -> { f | title = val }) model.isAddingDraft }, noOut )

        OnDraftKeydown key ->
            case key of
                13 ->
                    --ENTER
                    case model.isAddingDraft of
                        Just form ->
                            let
                                ( draftTitle, draftMessage ) =
                                    splitDraftBody form.title

                                submitForm =
                                    { form
                                        | title = draftTitle
                                        , post =
                                            if draftMessage == "" then
                                                form.post

                                            else
                                                Dict.insert "message" draftMessage form.post
                                    }
                            in
                            ternary (draftTitle /= "" && not (isLoading model.board_result))
                                ( { model | board_result = Loading }, out0 [ addProjectCard apis submitForm OnAddCardAck ] )
                                ( model, noOut )

                        Nothing ->
                            ( model, noOut )

                27 ->
                    --ESC
                    ( { model | isAddingDraft = Nothing }, noOut )

                _ ->
                    ( model, noOut )

        OnDraftCancel ->
            if Maybe.map .blur_safe model.isAddingDraft == Just True then
                ( model, noOut )

            else
                ( { model | isAddingDraft = Nothing }, noOut )

        OnAddCardAck result ->
            case result of
                Success cards ->
                    let
                        cmd =
                            case List.head cards of
                                Just c ->
                                    case c.card of
                                        CardTension _ ->
                                            OnCardClick (Just c)

                                        CardDraft _ ->
                                            OnAddDraft c.colid

                                Nothing ->
                                    NoMsg

                        isAD =
                            Maybe.map
                                (\draft ->
                                    { draft | blur_safe = True, title = "", post = Dict.empty }
                                )
                                model.isAddingDraft

                        d =
                            model.project

                        pj =
                            { d | columns = List.foldl (\c cols -> pushCard c cols) d.columns cards }
                    in
                    ( { model | project = pj, board_result = NotAsked, isAddingDraft = isAD }
                    , out0 [ send cmd ]
                    )

                Failure err ->
                    ( { model | board_result = Failure err }, noOut )

                _ ->
                    ( model, noOut )

        OnCardHover cardid ->
            ( { model | cardHover = cardid }, noOut )

        OnCardHoverLeave ->
            ( { model | cardHover = "" }, noOut )

        OnToggleCardEdit cardid ->
            let
                cmds =
                    [ Task.attempt (OnSetCardEditDropdown cardid) (Dom.getElement (cardid ++ "-ellipsis")) ]
            in
            ( { model | cardEdit = ternary (model.cardEdit == "") cardid "" }, out0 cmds )

        OnToggleColEdit colid ->
            ( { model | colEdit = ternary (model.colEdit == "") colid "" }, noOut )

        OnRemoveCard cardid ->
            ( { model | board_result = Loading }, out0 [ removeProjectCards apis [ cardid ] OnRemoveCardAck ] )

        OnRemoveCardAck result ->
            case result of
                Success cardids ->
                    let
                        pj =
                            List.foldl
                                (\cardid project ->
                                    case getCard cardid project of
                                        Just card ->
                                            project
                                                |> (\d -> { d | columns = removeCard card d.columns })

                                        Nothing ->
                                            project
                                )
                                model.project
                                cardids

                        activeCards =
                            List.filter (\x -> not (List.member x cardids)) model.activeCards
                    in
                    ( { model | board_result = NotAsked, project = pj, activeCards = activeCards }, noOut )

                Failure err ->
                    ( { model | board_result = Failure err }, noOut )

                _ ->
                    ( model, noOut )

        OnRemoveColItems colid ->
            let
                cards =
                    LE.find (\x -> x.id == colid) model.project.columns
                        |> Maybe.map (.cards >> List.map .id)
                        |> withDefault []
            in
            ( model, out0 [ removeProjectCards apis cards OnRemoveCardAck ] )

        OnDeleteColumn colid ->
            ( model, out0 [ deleteProjectColumns apis [ colid ] OnDeleteColumnAck ] )

        OnDeleteColumnAck result ->
            case result of
                Success uids ->
                    let
                        pj =
                            List.foldl
                                (\uid project ->
                                    case LE.findIndex (\b -> b.id == uid) project.columns of
                                        Just i ->
                                            { project | columns = LE.removeAt i project.columns }

                                        Nothing ->
                                            project
                                )
                                model.project
                                uids
                    in
                    ( { model | board_result = NotAsked, project = pj }, noOut )

                Failure err ->
                    ( { model | board_result = Failure err }, noOut )

                _ ->
                    ( model, noOut )

        OnSetCardEditDropdown cardid elt ->
            case elt of
                Ok e ->
                    let
                        card =
                            getCard cardid model.project
                    in
                    ( { model | cardEditDropdown = getCard cardid model.project, cardEditDropdownX = e.element.x, cardEditDropdownY = e.element.y }, noOut )

                Err _ ->
                    ( { model | cardEditDropdown = Nothing }, noOut )

        --
        OpenTensionPane colTarget ->
            ( model, out1 [ DoOpenLinkTensionPanel colTarget ] )

        OpenCardPane cardid ->
            case getCard cardid model.project of
                Just card ->
                    ( { model | activeCards = [ cardid ] }, out1 [ DoOpenCardPanel card ] )

                Nothing ->
                    ( model, noOut )

        OnLinkTension ( colid, cards ) ->
            let
                d =
                    model.project

                pj =
                    case LE.findIndex (\c -> c.id == colid) d.columns of
                        Just i ->
                            { d
                                | columns =
                                    LE.updateAt i
                                        (\c -> { c | cards = c.cards ++ cards })
                                        d.columns
                            }

                        Nothing ->
                            let
                                noStatusCol =
                                    { id = colid
                                    , name = "Triage"
                                    , color = Nothing
                                    , pos = 0
                                    , col_type = ProjectColumnType.NoStatusColumn
                                    , cards = cards
                                    }
                            in
                            { d | columns = noStatusCol :: d.columns }
            in
            ( { model | project = pj }, noOut )

        OnUpdateCard card ->
            let
                d =
                    model.project

                pj =
                    { d | columns = updateCard card d.columns }
            in
            ( { model | project = pj }, noOut )

        OnConvertDraft cardid draft ->
            case getCard cardid model.project of
                Just c ->
                    let
                        d =
                            { draft | cardid = c.id, colid = c.colid, pos = c.pos }

                        conversion =
                            { draft = d, project = toTensionProject c.colid c.pos model.project }
                    in
                    ( model, out1 [ DoCreateTension model.node_focus.nameid Nothing (Just conversion) ] )

                Nothing ->
                    ( { model | board_result = Failure [ "Draft not found" ] }, noOut )

        OnConvertDraftAck draft _ ->
            -- The new tension card is created by Form.NewTension via the
            -- selectedProjects mechanism. Here we only need to drop the
            -- former draft card if there was one.
            if draft.cardid == "" then
                ( model, noOut )

            else
                ( { model | board_result = Loading }
                , out0 [ removeProjectCards apis [ draft.cardid ] OnRemoveCardAck ]
                )

        -- Components
        ProjectColumnModalMsg msg ->
            let
                ( data, out ) =
                    ProjectColumnModal.update apis msg model.projectColumnModal

                pj =
                    case out.result of
                        Just ( a, b ) ->
                            let
                                d =
                                    model.project
                            in
                            case a of
                                AddColumn ->
                                    { d | columns = d.columns ++ [ b ] }

                                EditColumn ->
                                    { d
                                        | columns =
                                            LE.updateIf (\c -> c.id == b.id)
                                                (\c -> { c | name = b.name, color = b.color, pos = b.pos })
                                                d.columns
                                    }

                                _ ->
                                    d

                        Nothing ->
                            model.project
            in
            ( { model | projectColumnModal = data, project = pj }, Out (List.map (\m -> Cmd.map ProjectColumnModalMsg m) out.cmds) out.gcmds Nothing )

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

        OnUpdateModel f ->
            ( f model, noOut )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    [ Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    , Events.onResize (\w h -> OnResize w h)
    ]
        ++ (if model.colEdit /= "" then
                [ Events.onMouseUp (JD.succeed (OnToggleColEdit ""))
                , Events.onKeyUp (Dom.key "Escape" (OnToggleColEdit ""))
                ]

            else
                []
           )
        ++ (if model.cardEdit /= "" then
                [ Events.onMouseUp (JD.succeed (OnCardClick Nothing))
                , Events.onKeyUp (Dom.key "Escape" (OnCardClick Nothing))
                ]

            else if model.movingCard /= Nothing || model.activeCards /= [] then
                [ Events.onMouseUp (Dom.outsideClickClose "cardPanelContainer" (OnCardClick Nothing)) ]

            else
                []
           )
        ++ (ProjectColumnModal.subscriptions model.projectColumnModal |> List.map (\s -> Sub.map ProjectColumnModalMsg s))



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    { pattern : String
    , filterLabels : List Label
    , filterAssignees : List User
    }


matchCard : Op -> ProjectCard -> Bool
matchCard op card =
    let
        ( title, labels, assignees ) =
            case card.card of
                CardTension t ->
                    ( t.title, withDefault [] t.labels, withDefault [] t.assignees )

                CardDraft d ->
                    ( d.title, withDefault [] d.labels, withDefault [] d.assignees )

        matchPattern =
            op.pattern == "" || String.contains (String.toLower op.pattern) (String.toLower title)

        matchLabels =
            List.isEmpty op.filterLabels
                || List.any (\l -> List.any (\fl -> fl.name == l.name) op.filterLabels) labels

        matchAssignees =
            List.isEmpty op.filterAssignees
                || List.any (\u -> List.any (\fu -> fu.username == u.username) op.filterAssignees) assignees
    in
    matchPattern && matchLabels && matchAssignees


view : String -> List Label -> List User -> State -> Html Msg
view pattern filterLabels filterAssignees (State model) =
    let
        op =
            { pattern = pattern, filterLabels = filterLabels, filterAssignees = filterAssignees }
    in
    div []
        [ viewBoard op model
        , if model.cardEdit /= "" then
            viewCardDropdown model

          else
            text ""
        , ProjectColumnModal.view model.empty model.projectColumnModal |> Html.map ProjectColumnModalMsg
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        ]


{-| The given code defines a function `viewBoard` which takes four arguments:

1.  `op`: representing the operations (msgs) that can be performed on the given board.
2.  `header`: which is a function to display the header of each column in the board. The parameter of the header functioun are
    1.  An unique identifier of the column
    2.  A potential name for that column
    3.  The column id
    4.  The potential first card of the column
3.  `keys`: A list of ordered column's ids in the board.
4.  `data`: representing the tensions in each column of the board.

The function returns an HTML structure which displays the board with each column having its own header and list of tensions.

This function generates a drag-and-drop board with the ability to move card items around.

-}
viewBoard : Op -> Model -> Html Msg
viewBoard op model =
    let
        columns =
            model.project.columns |> List.filter (\x -> not (x.col_type == ProjectColumnType.NoStatusColumn && List.length x.cards == 0))
    in
    columns
        |> List.indexedMap
            (\i col ->
                let
                    colid =
                        col.id

                    filteredCards =
                        List.filter (matchCard op) col.cards

                    cards_len =
                        List.length filteredCards

                    c1 =
                        List.head filteredCards

                    isNoStatusCol =
                        col.col_type == ProjectColumnType.NoStatusColumn
                in
                [ div
                    ([ class "column is-3" ]
                        ++ ternary model.hasTaskMove
                            [ onDragEnter <| OnMoveEnterCol { pos = i, colid = colid, length = cards_len } False
                            , onDragLeave <| OnMoveLeaveCol

                            -- prevent the "card moving back to its original place" effect
                            , onDragOverPD NoMsg
                            ]
                            []
                    )
                    [ div
                        [ class "subtitle"
                        , attribute "draggable" (ternary isNoStatusCol "false" "true")
                        , onDragStart <| OnMoveColumn col
                        , onDragEnd <| OnMoveColumnEnd

                        -- @debug: allow move card in empty collumn
                        , onDragEnter <| OnMoveEnterT { pos = 0, cardid = unwrap "" .id c1, colid = colid }
                        ]
                        [ viewHeader model.session.lexicon model.isProjectAdmin (model.colEdit == colid) col ]
                    , filteredCards
                        --|> List.sortBy .createdAt
                        --|> (\l -> ternary (model.sortFilter == defaultSortFilter) l (List.reverse l))
                        |> List.indexedMap
                            (\j c ->
                                let
                                    -- we do not need, and don't calculate the relative position of card in the front.
                                    card =
                                        { c | pos = j }
                                in
                                [ -- Elm bug#1: if you remove this empty text
                                  --  It seems to be related/caused by the function composition that set an attribute
                                  --  in addProjectCardFunction response decoder
                                  text ""
                                , div
                                    ([ id card.id
                                     , class "box is-hoverable is-shrinked2 mb-2 mx-2 kb-card"
                                     ]
                                        ++ ternary model.hasTaskMove
                                            [ classList
                                                [ ( "is-dragging", model.draging && Maybe.map .id model.movingCard == Just card.id )
                                                , ( "is-focusing", Maybe.map .id model.movingCard == Just card.id || List.member card.id model.activeCards )
                                                ]
                                            , onClick (OnCardClick (Just card))
                                            , onMouseEnter (OnCardHover card.id)
                                            , onMouseLeave OnCardHoverLeave
                                            , attribute "draggable" "true"
                                            , onDragStart <| OnMove { pos = i, colid = colid, length = cards_len } card
                                            , onDragEnd <| OnMoveEnd
                                            , onDragEnter <| OnMoveEnterT { pos = j, cardid = card.id, colid = colid }
                                            ]
                                            []
                                    )
                                    (case card.card of
                                        CardTension t ->
                                            -- Does lazy will work with function in argment?
                                            [ Lazy.lazy5 viewMediaTension card.id (card.id == model.cardHover && model.isProjectAdmin) (card.id == model.cardEdit) model.node_focus t ]

                                        CardDraft d ->
                                            [ Lazy.lazy4 viewMediaDraft card.id (card.id == model.cardHover && model.isProjectAdmin) (card.id == model.cardEdit) d ]
                                    )
                                ]
                            )
                        |> List.concat
                        |> (\x ->
                                -- View Draft content editable
                                case model.isAddingDraft of
                                    Just form ->
                                        if form.colid == colid then
                                            x ++ [ viewDraftEditable model.session.lexicon form ]

                                        else
                                            x

                                    Nothing ->
                                        -- Add potential dragging div
                                        let
                                            xx =
                                                x
                                                    ++ [ text "" -- elm bug#1
                                                       , div
                                                            [ class "box"
                                                            , style "opacity" "0"
                                                            , onDragLeave <| OnMoveEnterCol { pos = i, colid = colid, length = cards_len } True
                                                            ]
                                                            []
                                                       ]
                                        in
                                        Maybe.map2
                                            (\c c_hov ->
                                                if
                                                    -- In this col
                                                    (c_hov.colid == colid)
                                                        -- Not just above the dragged element
                                                        && (c.id /= c_hov.cardid)
                                                        -- Not just below the dragged element
                                                        && (c.pos /= (c_hov.pos - 1) || colid /= c.colid)
                                                then
                                                    -- account for the extra text "" (see Elm bug#1) !
                                                    insertAt (c_hov.pos * 2) draggingDiv xx

                                                else
                                                    xx
                                            )
                                            model.movingCard
                                            model.movingHoverT
                                            |> withDefault xx
                           )
                        |> div [ id colid, class "content scrollbar-thin" ]
                    ]
                , div
                    [ class "divider is-vertical2 is-small is-hidden-mobile"
                    , classList [ ( "is-column-over", model.movingCol /= Nothing && Maybe.map .pos model.movingHoverCol == Just i && not isNoStatusCol ) ]
                    ]
                    []
                ]
            )
        |> List.concat
        |> (\x ->
                -- View New Col Button
                if model.hasNewCol && model.isProjectAdmin then
                    x ++ [ viewNewCol ]

                else
                    x
           )
        |> div
            [ id nodeID
            , class "columns is-fullwidth m-0 is-mobile kb-board board2"
            , attribute "style" <|
                case model.boardHeight of
                    Just h ->
                        "overflow-y: hidden; overflow-x: auto; height:" ++ String.fromFloat h ++ "px;"

                    Nothing ->
                        "overflow-y: hidden; overflow-x: auto;"
            ]


viewHeader : Dict.Dict String String -> Bool -> Bool -> ProjectColumn -> Html Msg
viewHeader lexicon isAdmin isEdited col =
    span []
        [ div [ class "level" ]
            [ div [ class "level-left ml-3" ]
                [ span [ class "mr-3", style "color" (withDefault "lightgrey" col.color) ] [ A.icon "icon-circle1 icon-lg" ], text col.name ]
            , span [ class "level-right" ]
                [ if isAdmin then
                    span
                        [ class "tag has-background-inherit is-rounded-light button-light has-border-small mx-1"
                        , onClick (OnAddDraft col.id)
                        ]
                        [ A.icon "icon-plus" ]

                  else
                    text ""
                , if col.col_type /= ProjectColumnType.NoStatusColumn && isAdmin then
                    B.dropdownLight
                        { dropdown_id = "col-ellipsis"
                        , isOpen = isEdited
                        , dropdown_cls = "mx-2 is-align-self-baseline is-right"
                        , button_cls = ""
                        , button_html = A.icon "button-light icon-more-horizontal icon-lg"
                        , msg = OnToggleColEdit (ternary isEdited "" col.id)
                        , menu_cls = ""
                        , content_cls = ""
                        , content_html =
                            div []
                                [ div
                                    [ class "dropdown-item button-light"
                                    , onClick (ProjectColumnModalMsg (ProjectColumnModal.OnOpenEdit col.id))
                                    ]
                                    [ A.icon1 "icon-edit-2" T.edit ]
                                , div
                                    [ class "dropdown-item button-light"
                                    , onClick (OpenTensionPane (Just { id = col.id, cards_len = List.length col.cards }))
                                    ]
                                    [ A.icon1 "icon-plus" (T.addTensionColumn lexicon) ]
                                , hr [ class "dropdown-divider my-4" ] []
                                , div [ class "dropdown-item button-light", onClick (OnDeleteColumn col.id) ]
                                    [ A.icon1 "icon-trash" T.deleteColumn ]
                                , div [ class "dropdown-item button-light is-danger", onClick (OnRemoveColItems col.id) ]
                                    [ A.icon1 "icon-trash" T.removeItemsColumn ]
                                ]
                        }

                  else
                    text ""
                ]
            ]
        ]


draggingDiv : Html Msg
draggingDiv =
    div
        [ class "box is-shrinked2 mb-2 mx-2 is-dragging is-growing has-border-link"

        --, style "height" "0rem"
        ]
        []


viewNewCol : Html Msg
viewNewCol =
    div [ class "column is-2 ml-2" ]
        [ div
            [ class "has-border is-dashed is-rounded-light is-aligned-center is-h is-w p-6 pl-5"
            , style "width" "100%"
            , onClick OnAddCol
            ]
            [ A.icon1 "icon-plus" "Add column" ]
        ]


viewMediaDraft : String -> Bool -> Bool -> ProjectDraft -> Html Msg
viewMediaDraft cardid isHovered isEdited d =
    let
        ellipsis =
            if isHovered || isEdited then
                span [ id (cardid ++ "-ellipsis"), class "px-2 has-text-text", onClick <| OnToggleCardEdit (ternary isEdited "" cardid) ]
                    [ A.icon "button-light icon-more-horizontal icon-bg" ]

            else
                text ""
    in
    div [ class "media mediaBox is-size-7" ]
        [ div [ class "media-content" ]
            [ div [ class "is-weak mb-2 is-flex is-justify-content-space-between" ]
                [ div [ class "is-inline-flex" ] [ A.icon1 "icon-circle-draft" "Draft", ellipsis ] ]
            , div []
                [ span [ class "link-like is-human mr-2", onClick (OpenCardPane cardid) ]
                    [ text d.title ]
                , case d.labels of
                    Just labels ->
                        viewLabels Nothing labels

                    Nothing ->
                        text ""
                ]
            ]
        , div [ class "media-right wrapped-container-33 is-flex is-flex-direction-column is-align-self-flex-end" ]
            [ if d.message /= Nothing then
                a
                    [ class "level-right is-pulled-right discrete-link"
                    , title "Has comment"
                    ]
                    [ A.icon "icon-message-square icon-sm" ]

              else
                text ""
            ]
        ]


viewDraftEditable : Dict String String -> DraftForm -> Html Msg
viewDraftEditable lexicon form =
    let
        rowsCount =
            String.split "\n" form.title |> List.length |> max 1
    in
    div [ class "is-relative mb-2 mx-2" ]
        [ textarea
            [ id "draft-card-editable"
            , class "box is-shrinked2 p-2 m-0"
            , autofocus True
            , readonly form.pending
            , value form.title
            , rows rowsCount
            , onInput OnDraftEdit
            , onBlur OnDraftCancel
            , Html.Events.preventDefaultOn "keydown" draftKeydownDecoder
            , style "width" "100%"
            , style "resize" "none"
            , style "font" "inherit"
            ]
            []
        , if form.pending then
            span
                [ class "spinner2 is-small"
                , style "position" "absolute"
                , style "top" "50%"
                , style "right" "20px"
                ]
                []

          else
            span
                [ class "button-light is-weak px-1"
                , style "position" "absolute"
                , style "top" "50%"
                , style "right" "4px"
                , style "transform" "translateY(-50%)"
                , title (T.addTensionColumn lexicon)
                , onMousedownPD (OnAddTension form.colid)
                ]
                [ A.icon "icon-exchange" ]
        ]


{-| Submit on Enter, cancel on Esc. Shift+Enter and other keys fall through to the textarea's native behavior.
-}
draftKeydownDecoder : JD.Decoder ( Msg, Bool )
draftKeydownDecoder =
    JD.field "keyCode" JD.int
        |> JD.andThen
            (\key ->
                case key of
                    13 ->
                        JD.field "shiftKey" JD.bool
                            |> JD.andThen
                                (\shift ->
                                    if shift then
                                        JD.fail "shift+enter -> default newline"

                                    else
                                        JD.succeed ( OnDraftKeydown 13, True )
                                )

                    27 ->
                        JD.succeed ( OnDraftKeydown 27, False )

                    _ ->
                        JD.fail "default"
            )


viewMediaTension : String -> Bool -> Bool -> NodeFocus -> Tension -> Html Msg
viewMediaTension cardid isHovered isEdited focus t =
    let
        n_comments =
            withDefault 0 t.n_comments

        status_html =
            case t.action of
                Just action ->
                    let
                        tc =
                            getTensionCharac action
                    in
                    div
                        [ title (action2str tc.action)
                        , style "left" "15px"
                        ]
                        [ A.icon0 (action2icon tc ++ " icon-sm") ]

                Nothing ->
                    case t.status of
                        TensionStatus.Closed ->
                            div
                                [ title T.closedTension
                                , style "left" "15px"
                                ]
                                [ A.icon ("icon-alert-circle icon-sm has-text-" ++ statusColor t.status) ]

                        _ ->
                            text ""

        ellipsis =
            if isHovered || isEdited then
                span [ id (cardid ++ "-ellipsis"), class "px-2 has-text-text", onClick <| OnToggleCardEdit (ternary isEdited "" cardid) ]
                    [ A.icon "button-light icon-more-horizontal icon-bg" ]

            else
                text ""
    in
    div
        [ class "media mediaBox is-size-7" ]
        [ div [ class "media-content" ]
            [ div [ class "is-weak mb-2 is-flex is-justify-content-space-between" ]
                [ div [] [ span [ class "mr-2" ] [ tensionIcon t.type_ ], text t.receiver.name, ellipsis ], status_html ]
            , div []
                [ span [ class "link-like is-human mr-2", onClick (OpenCardPane cardid) ]
                    [ text t.title ]
                , case t.labels of
                    Just labels ->
                        viewLabels (Just focus.nameid) labels

                    Nothing ->
                        text ""
                ]
            ]
        , div [ class "media-right wrapped-container-33 is-flex is-flex-direction-column is-align-self-flex-end" ]
            [ if n_comments > 1 then
                a
                    [ class "level-right is-pulled-right discrete-link"
                    , title (String.fromInt (n_comments - 1) ++ " comments")
                    ]
                    [ A.icon0 "icon-message-square icon-sm", text (String.fromInt (n_comments - 1)) ]

              else
                text ""
            ]
        ]


viewCardDropdown : Model -> Html Msg
viewCardDropdown model =
    case model.cardEditDropdown of
        Just card ->
            div
                [ class "dropdown-menu is-block"
                , attribute "role" "menu"
                , style "top" (String.fromFloat (model.cardEditDropdownY + 18) ++ "px")
                , style "left" (String.fromFloat model.cardEditDropdownX ++ "px")
                ]
                [ case card.card of
                    CardTension t ->
                        div [ class "dropdown-content p-0" ]
                            [ a
                                [ class "dropdown-item button-light discrete-link"
                                , href (Route.Tension_Dynamic_Dynamic { param1 = nid2rootid t.receiver.nameid, param2 = t.id } |> toHref)
                                , target "_blank"
                                ]
                                [ A.icon1 "icon-external-link" T.openNewTab ]
                            , hr [ class "dropdown-divider" ] []
                            , div [ class "dropdown-item button-light", onClick (OnRemoveCard card.id) ] [ A.icon1 "icon-x" T.removeFromProject ]
                            ]

                    CardDraft d ->
                        div [ class "dropdown-content p-0" ]
                            [ div [ class "dropdown-item button-light", onClick (OnConvertDraft card.id d) ] [ A.icon1 "icon-exchange" (T.convertDraft model.session.lexicon) ]
                            , hr [ class "dropdown-divider" ] []
                            , div [ class "dropdown-item button-light", onClick (OnRemoveCard card.id) ] [ A.icon1 "icon-trash" T.deleteDraft ]
                            ]
                ]

        Nothing ->
            text ""



--
-- Utils
--


{-| Split a draft body at the first newline: first line is the title, rest joined back is the message (trimmed).
-}
splitDraftBody : String -> ( String, String )
splitDraftBody body =
    case String.split "\n" body of
        first :: rest ->
            ( first, String.join "\n" rest |> String.trim )

        _ ->
            ( body, "" )


getCard : String -> ProjectData -> Maybe ProjectCard
getCard cardid data =
    LE.find (\a -> a.id == cardid) (data.columns |> List.map .cards |> List.concat)


{-| Build a TensionProject view of the board's project, anchored at a
specific column and card position. Used to pre-assign the source project
when converting a draft / creating a tension from a board column.
-}
toTensionProject : String -> Int -> ProjectData -> Maybe TensionProject
toTensionProject colid pos pj =
    let
        toLite : ProjectColumn -> ProjectColumnLite
        toLite c =
            { id = c.id, name = c.name, color = c.color, pos = c.pos, col_type = c.col_type }
    in
    pj.columns
        |> LE.find (\c -> c.id == colid)
        |> Maybe.map
            (\c ->
                { card = { id = "", pos = pos }
                , column = toLite c
                , project =
                    { id = pj.id
                    , name = pj.name
                    , columns = List.map toLite pj.columns
                    , nodes = List.map (\n -> { nameid = n.nameid }) pj.nodes
                    }
                }
            )


getCol : String -> ProjectData -> Maybe ProjectColumn
getCol colid data =
    LE.find (\a -> a.id == colid) data.columns


pushCard : ProjectCard -> List ProjectColumn -> List ProjectColumn
pushCard c columns =
    LE.updateIf
        (\a -> a.id == c.colid)
        (\a ->
            { a
                | cards = insertAt c.pos c a.cards

                -- Not needed, since we work in direct position on the front.
                --|> (\cards ->
                --        -- Increment the position of the elements to take into account the new insertion.
                --        let
                --            ( before, after ) =
                --                LE.splitAt (c.pos + 1) cards
                --        in
                --        before ++ List.map (\b -> { b | pos = b.pos + 1 }) after
                --   )
            }
        )
        columns


removeCard : ProjectCard -> List ProjectColumn -> List ProjectColumn
removeCard c columns =
    LE.updateIf
        (\a -> a.id == c.colid)
        (\a ->
            -- @warning; c.pos may not be consistent (see how pos position are managed front/back.)
            --{ a | cards = LE.removeAt c.pos a.cards }
            case LE.findIndex (\b -> b.id == c.id) a.cards of
                Just i ->
                    { a | cards = LE.removeAt i a.cards }

                Nothing ->
                    a
        )
        columns


updateCard : ProjectCard -> List ProjectColumn -> List ProjectColumn
updateCard c columns =
    LE.updateIf
        (\a -> a.id == c.colid)
        (\a ->
            { a | cards = LE.setIf (\b -> b.id == c.id) c a.cards }
        )
        columns


moveColAt : String -> Int -> List ProjectColumn -> List ProjectColumn
moveColAt colid pos columns =
    case LE.findIndex (\a -> a.id == colid) columns of
        Just i ->
            case LE.getAt i columns of
                Just a ->
                    columns
                        |> LE.removeAt i
                        |> insertAt pos a

                Nothing ->
                    columns

        Nothing ->
            columns
