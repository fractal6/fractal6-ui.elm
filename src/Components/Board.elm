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


port module Components.Board exposing (Msg(..), State, init, nodeID, subscriptions, update, view)

import Assets as A
import Browser.Dom as Dom
import Browser.Events as Events
import Bulk exposing (UserState(..), uctxFromUser)
import Bulk.Bulma as B
import Bulk.Codecs exposing (ActionType(..), NodeFocus, getTensionCharac, nid2rootid)
import Bulk.View exposing (action2icon, action2str, mediaTension, statusColor, tensionIcon, tensionStatus2str, tensionType2str, viewLabels)
import Components.LinkTensionPanel as LinkTensionPanel exposing (ColTarget)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.ProjectColumnModal as ProjectColumnModal exposing (ModalType(..))
import Dict exposing (Dict)
import Extra exposing (insertAt, ternary, unwrap)
import Extra.Events exposing (onClickPD, onDragEnd, onDragEnter, onDragLeave, onDragStart, onKeydown)
import Fractal.Enum.ProjectColumnType as ProjectColumnType
import Fractal.Enum.TensionStatus as TensionStatus
import Generated.Route as Route exposing (toHref)
import Global exposing (send, sendSleep)
import Html exposing (Html, a, br, div, hr, i, span, text)
import Html.Attributes exposing (attribute, autofocus, class, classList, contenteditable, href, id, style, target)
import Html.Events exposing (onBlur, onClick, onInput, onMouseEnter, onMouseLeave)
import Html.Lazy as Lazy
import Json.Decode as JD
import Json.Encode as JE
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), isLoading, withMapData, withMaybeData, withMaybeMapData)
import Maybe exposing (withDefault)
import ModelSchema exposing (CardKind(..), IdPayload, Post, ProjectCard, ProjectColumn, ProjectData, ProjectDraft, Tension, UserCtx)
import Ports
import Query.QueryProject exposing (addProjectCard, moveProjectCard, removeProjectCards)
import Scroll exposing (scrollToSubBottom)
import Session exposing (Apis, Conf, GlobalCmd(..))
import Task
import Text as T


type State
    = State Model


nodeID =
    "projectView"


type alias Model =
    { user : UserState
    , node_focus : NodeFocus
    , projectid : String
    , project : ProjectData
    , hasTaskMove : Bool
    , hasNewCol : Bool
    , isAddingDraft : Maybe DraftForm
    , boardHeight : Maybe Float
    , movingCard : Maybe ProjectCard
    , movingHoverCol : Maybe { pos : Int, colid : String, length : Int }
    , movingHoverT : Maybe { pos : Int, cardid : String, colid : String }
    , dragCount : Int
    , draging : Bool
    , projectColumnModal : ProjectColumnModal.State
    , board_result : GqlData String -- track board remote result silently
    , cardHover : String
    , cardEdit : String

    -- Common
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    }


type alias DraftForm =
    { uctx : UserCtx
    , title : String
    , colid : String
    , pos : Int
    , post : Post
    , tids : List (Maybe String)
    }


initModel : String -> NodeFocus -> UserState -> Model
initModel projectid focus user =
    { user = user
    , node_focus = focus
    , projectid = projectid
    , project = ProjectData "" "" []
    , hasTaskMove = True
    , hasNewCol = True
    , isAddingDraft = Nothing

    -- Board
    , boardHeight = Nothing
    , movingCard = Nothing
    , movingHoverCol = Nothing
    , movingHoverT = Nothing
    , dragCount = 0
    , draging = False

    --
    , projectColumnModal = ProjectColumnModal.init projectid user
    , board_result = NotAsked
    , cardHover = ""
    , cardEdit = ""

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }


init : String -> NodeFocus -> UserState -> State
init projectid focus user =
    initModel projectid focus user |> State



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = OnLoad ProjectData
    | OnResize Int Int
    | FitBoard (Result Dom.Error Dom.Element)
    | ScrollToElement String
    | OnMove { pos : Int, colid : String, length : Int } ProjectCard
    | OnCancelHov
    | OnEndMove
    | OnMoveEnterCol { pos : Int, colid : String, length : Int } Bool
    | OnMoveLeaveCol
    | OnMoveLeaveCol_
    | OnMoveEnterT { pos : Int, cardid : String, colid : String }
    | OnMoveDrop String
    | GotCardMoved (GqlData IdPayload)
    | OnCardClick (Maybe ProjectCard)
    | OnCardClick_ (Maybe ProjectCard)
    | OnCardHover String
    | OnCardHoverLeave
    | OnToggleCardEdit
    | OnToggleCardEdit_
    | OnRemoveCard
    | OnRemoveCardAck (GqlData (List String))
      --
    | OnAddCol
    | OnAddDraft String
    | OnDraftEdit String
    | OnDraftKeydown Int
    | OnDraftCancel
    | OnAddDraftAck (GqlData (List ProjectCard))
    | ProjectColumnModalMsg ProjectColumnModal.Msg
    | OpenTensionPane (Maybe ColTarget)
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

        OnMove col card ->
            ( { model | draging = True, dragCount = 0, movingHoverCol = Just col, movingCard = Just card }, noOut )

        OnEndMove ->
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
                            [ moveProjectCard apis card.id c_hover.pos colid GotCardMoved
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
                Just _ ->
                    ( model, out0 [ sendSleep (OnCardClick_ c) 50 ] )

                Nothing ->
                    ( { model | movingCard = Nothing, cardEdit = "" }, noOut )

        OnCardClick_ c ->
            -- Solves concurent message sent
            ( { model | movingCard = c }, noOut )

        OnCancelHov ->
            --let
            --    l1 =
            --        Debug.log "Cancel hov" ""
            --in
            ( { model | movingHoverCol = Nothing, movingHoverT = Nothing }, noOut )

        OnMoveEnterCol hover reset ->
            -- @DEBUG: How to optimize / simplify that ?
            -- Does "dragCount" still usefull ??
            let
                ( is_last, c_h ) =
                    Maybe.map2
                        (\ch h ->
                            ( ch.colid == h.colid && ch.pos == h.length - 1, Just { ch | pos = ch.pos + 1 } )
                        )
                        model.movingHoverT
                        model.movingHoverCol
                        |> withDefault ( False, model.movingHoverT )
            in
            if Just hover == model.movingHoverCol && not reset then
                -- ?
                ( { model | dragCount = 1 }, noOut )

            else if Just hover == model.movingHoverCol && reset && is_last then
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
            --let
            --    l1 =
            --        Debug.log "On move enter Card" hover.pos
            --in
            ( { model | movingHoverT = Just hover }, noOut )

        OnMoveDrop nameid ->
            -- @not implemented.
            ( { model | movingCard = Nothing, movingHoverCol = Nothing, movingHoverT = Nothing }, noOut )

        GotCardMoved result ->
            ( { model | board_result = withMapData .id result }, noOut )

        OnAddCol ->
            let
                pos =
                    List.length model.project.columns
            in
            ( model, out0 [ Cmd.map ProjectColumnModalMsg (send (ProjectColumnModal.OnOpenAdd pos)) ] )

        OnAddDraft colid ->
            let
                title =
                    Maybe.map .title model.isAddingDraft |> withDefault ""

                pos =
                    LE.find (\b -> b.id == colid) model.project.columns |> unwrap [] .cards |> List.length

                uctx =
                    uctxFromUser model.user
            in
            ( { model | isAddingDraft = Just { uctx = uctx, tids = [ Nothing ], post = Dict.empty, title = title, colid = colid, pos = pos } }
            , out0 [ Ports.focusOn "draft-card-editable", scrollToSubBottom colid NoMsg ]
            )

        OnDraftEdit val ->
            let
                form =
                    model.isAddingDraft

                title =
                    String.replace "<br>" "" val
                        |> String.replace "<div>" ""
                        |> String.replace "</div>" ""
            in
            ( { model | isAddingDraft = Maybe.map (\f -> { f | title = title }) form }, noOut )

        OnDraftKeydown key ->
            case key of
                13 ->
                    --ENTER
                    case model.isAddingDraft of
                        Just form ->
                            ternary (form.title /= "" && not (isLoading model.board_result))
                                ( { model | board_result = Loading }, out0 [ addProjectCard apis form OnAddDraftAck ] )
                                ( model, noOut )

                        Nothing ->
                            ( model, noOut )

                27 ->
                    --ESC
                    ( { model | isAddingDraft = Nothing }, noOut )

                _ ->
                    ( model, noOut )

        OnDraftCancel ->
            ( { model | isAddingDraft = Nothing }, noOut )

        OnAddDraftAck result ->
            case result of
                Success cards ->
                    let
                        d =
                            model.project

                        project =
                            { d | columns = List.foldl (\c cols -> pushCard c cols) d.columns cards }
                    in
                    ( { model | project = project, isAddingDraft = Nothing, board_result = NotAsked }
                    , out0 [ send (OnAddDraft (List.head cards |> unwrap "" .colid)) ]
                    )

                Failure err ->
                    ( { model | board_result = Failure err }, noOut )

                _ ->
                    ( model, noOut )

        OnCardHover cardid ->
            ( { model | cardHover = cardid }, noOut )

        OnCardHoverLeave ->
            ( { model | cardHover = "" }, noOut )

        OnToggleCardEdit ->
            ( model, out0 [ sendSleep OnToggleCardEdit_ 50 ] )

        OnToggleCardEdit_ ->
            -- Solve mesage concurrency
            ( { model | cardEdit = ternary (model.cardEdit == "") model.cardHover "" }, noOut )

        OnRemoveCard ->
            case model.cardEdit of
                "" ->
                    -- no card dropdown open
                    ( model, noOut )

                _ ->
                    let
                        cardid =
                            model.cardEdit
                    in
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
                    in
                    ( { model | board_result = NotAsked, project = pj }, noOut )

                Failure err ->
                    ( { model | board_result = Failure err }, noOut )

                _ ->
                    ( model, noOut )

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

        OpenTensionPane colTarget ->
            --@todo open panel with port
            ( model, noOut )

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
    , unselectCardFromJs (always (OnCardClick Nothing))
    , Events.onResize (\w h -> OnResize w h)
    ]
        ++ (ProjectColumnModal.subscriptions model.projectColumnModal |> List.map (\s -> Sub.map ProjectColumnModalMsg s))


port unselectCardFromJs : (() -> msg) -> Sub msg



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    {}


view : Op -> State -> Html Msg
view op (State model) =
    div []
        [ viewBoard op model
        , ProjectColumnModal.view {} model.projectColumnModal |> Html.map ProjectColumnModalMsg
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

                    j_last =
                        List.length col.cards - 1

                    c1 =
                        List.head col.cards
                in
                [ div
                    (class "column is-3"
                        :: ternary model.hasTaskMove
                            [ onDragEnter (OnMoveEnterCol { pos = i, colid = colid, length = j_last + 1 } False)
                            , onDragLeave OnMoveLeaveCol

                            -- @DEBUG doesn't work
                            --, onDrop (OnMoveDrop colid)
                            , attribute "ondragover" "return false"
                            ]
                            []
                    )
                    [ div
                        [ class "subtitle"
                        , onDragEnter (OnMoveEnterT { pos = 0, cardid = unwrap "" .id c1, colid = colid })
                        ]
                        [ viewHeader col c1 ]
                    , col.cards
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
                                  --  It seems to be related/caused by the function composition to set an attribute
                                  --  in addProjectCardFunction response decoder
                                  text ""
                                , div
                                    (class "box is-shrinked2 mb-2 mx-2 kb-card"
                                        :: ternary model.hasTaskMove
                                            [ classList
                                                [ ( "is-dragging", model.movingHoverT /= Nothing )
                                                , ( "is-dragged", Maybe.map .id model.movingCard == Just card.id )
                                                ]
                                            , attribute "draggable" "true"
                                            , attribute "ondragstart" "event.dataTransfer.setData(\"text/plain\", \"dummy\")"
                                            , onDragStart <| OnMove { pos = i, colid = colid, length = j_last + 1 } card
                                            , onDragEnd OnEndMove
                                            , onDragEnter (OnMoveEnterT { pos = j, cardid = card.id, colid = colid })
                                            , onClick (OnCardClick (Just card))
                                            , onMouseEnter (OnCardHover card.id)
                                            , onMouseLeave OnCardHoverLeave
                                            ]
                                            []
                                        ++ ternary (j_last == j && model.hasTaskMove)
                                            -- reset hoverT to draw below
                                            [ onDragLeave (OnMoveEnterCol { pos = i, colid = colid, length = j_last + 1 } True) ]
                                            []
                                    )
                                    (case card.card of
                                        CardTension t ->
                                            -- Does lazy will work with function in argment?
                                            [ Lazy.lazy4 viewMediaTension (card.id == model.cardHover) (card.id == model.cardEdit) model.node_focus t ]

                                        CardDraft d ->
                                            [ Lazy.lazy3 viewMediaDraft (card.id == model.cardHover) (card.id == model.cardEdit) d ]
                                    )
                                ]
                            )
                        |> List.concat
                        |> (\x ->
                                -- View Draft content editable
                                case model.isAddingDraft of
                                    Just form ->
                                        if form.colid == colid then
                                            x ++ [ viewDraftEditable form ]

                                        else
                                            x

                                    Nothing ->
                                        -- Add potential draggind div
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
                                                    insertAt (c_hov.pos * 2) draggingDiv x

                                                else
                                                    x
                                            )
                                            model.movingCard
                                            model.movingHoverT
                                            |> withDefault x
                           )
                        |> div [ id colid, class "content scrollbar-thin" ]
                    ]
                , div [ class "divider is-vertical2 is-small is-hidden-mobile" ] []
                ]
            )
        |> List.concat
        |> (\x ->
                -- View New Col Button
                if model.hasNewCol then
                    x ++ [ viewNewCol ]

                else
                    x
           )
        |> div
            [ id nodeID
            , class "columns is-fullwidth is-marginless is-mobile kb-board board2"
            , attribute "style" <|
                case model.boardHeight of
                    Just h ->
                        "overflow-y: hidden; overflow-x: auto; height:" ++ String.fromFloat h ++ "px;"

                    Nothing ->
                        "overflow-y: hidden; overflow-x: auto;"
            ]


viewHeader : ProjectColumn -> Maybe ProjectCard -> Html Msg
viewHeader col card =
    span []
        [ div [ class "level" ]
            [ div [ class "level-left ml-3" ] [ span [ class "mr-3", style "color" (withDefault "lightgrey" col.color) ] [ A.icon "icon-circle1 icon-lg" ], text col.name ]
            , span [ class "level-left" ]
                [ span
                    [ class "tag is-rounded-light button-light is-w has-border mx-1"
                    , onClick (OnAddDraft col.id)
                    ]
                    [ A.icon "icon-plus" ]
                , if col.col_type /= ProjectColumnType.NoStatusColumn then
                    div [ class "dropdown mx-2 is-align-self-baseline is-right" ]
                        [ div [ class "dropdown-trigger is-w is-h" ]
                            [ div
                                [ class "ellipsis"
                                , attribute "aria-controls" ("edit-ellipsis-" ++ col.id)
                                , attribute "aria-haspopup" "true"
                                ]
                                [ A.icon "icon-more-horizontal icon-lg" ]
                            ]
                        , div [ id ("edit-ellipsis-" ++ col.id), class "dropdown-menu", attribute "role" "menu" ]
                            [ div [ class "dropdown-content p-0" ] <|
                                [ div
                                    [ class "dropdown-item button-light"
                                    , onClick (ProjectColumnModalMsg (ProjectColumnModal.OnOpenEdit col.id))
                                    ]
                                    [ A.icon1 "icon-edit-2" T.edit ]
                                , hr [ class "dropdown-divider" ] []
                                , div
                                    [ class "dropdown-item button-light"
                                    , onClick (OpenTensionPane (Just { id = col.id, cards_len = List.length col.cards }))
                                    ]
                                    [ A.icon1 "icon-plus" T.addTensionColumn ]
                                , hr [ class "dropdown-divider" ] []
                                , div [ class "dropdown-item button-light" ]
                                    [ A.icon1 "icon-trash" T.delete ]
                                ]
                            ]
                        ]

                  else
                    text ""
                ]
            ]
        ]


draggingDiv : Html Msg
draggingDiv =
    div
        [ class "box is-shrinked2 mb-2 mx-2 is-dragging is-growing has-border-link"
        , style "opacity" "0.6"

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


viewMediaDraft : Bool -> Bool -> ProjectDraft -> Html Msg
viewMediaDraft isHovered isEdited d =
    let
        ellipsis =
            if isHovered || isEdited then
                B.dropdownLight
                    "card-ellipsis"
                    ("px-2 has-text-text " ++ ternary isEdited "is-active" "")
                    (A.icon "icon-more-horizontal is-h icon-bg")
                    OnToggleCardEdit
                    (div []
                        [ div [ class "dropdown-item button-light" ] [ A.icon1 "icon-exchange" "Convert draft to tension @todo" ]
                        , hr [ class "dropdown-divider" ] []
                        , div [ class "dropdown-item button-light", onClick OnRemoveCard ] [ A.icon1 "icon-trash" "Delete draft" ]
                        ]
                    )

            else
                text ""
    in
    div [ class "media mediaBox is-hoverable" ]
        [ div [ class "media-content is-smaller" ]
            [ div [ class "is-wrapped help is-icon-aligned mb-2" ] [ A.icon1 "icon-circle-draft" "Draft", ellipsis ]
            , div [] [ span [ class "link-like is-human" ] [ text d.title ] ]
            ]
        ]


viewDraftEditable : DraftForm -> Html Msg
viewDraftEditable form =
    div
        [ id "draft-card-editable"
        , class "box is-shrinked2 mb-2 mx-2 p-2"
        , contenteditable True
        , autofocus True
        , onKeydown OnDraftKeydown
        , onBlur OnDraftCancel

        -- OnInput does not work on contenteditable: https://github.com/elm/html/issues/24
        --, onInput op.onDraftEdit
        , Html.Events.on "input" (JD.map OnDraftEdit innerHtmlDecoder)
        , Html.Attributes.property "innerHTML" (JE.string form.title)
        ]
        []


innerHtmlDecoder =
    JD.at [ "target", "innerHTML" ] JD.string


viewMediaTension : Bool -> Bool -> NodeFocus -> Tension -> Html Msg
viewMediaTension isHovered isEdited focus t =
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
                    A.icon0 (action2icon tc ++ " icon-sm")

                Nothing ->
                    case t.status of
                        TensionStatus.Closed ->
                            A.icon ("icon-alert-circle icon-sm has-text-" ++ statusColor t.status)

                        _ ->
                            text ""

        ellipsis =
            if isHovered || isEdited then
                B.dropdownLight
                    "card-ellipsis"
                    ("px-2 has-text-text " ++ ternary isEdited "is-active" "")
                    (A.icon "icon-more-horizontal is-h icon-bg")
                    OnToggleCardEdit
                    (div []
                        [ div [ class "dropdown-item button-light" ]
                            [ a
                                [ class "stealth-link"
                                , href (Route.Tension_Dynamic_Dynamic { param1 = nid2rootid t.receiver.nameid, param2 = t.id } |> toHref)
                                , target "_blank"
                                ]
                                [ A.icon1 "" "", text " Open in a new tab", text " 🡕 " ]
                            ]
                        , hr [ class "dropdown-divider" ] []
                        , div [ class "dropdown-item button-light", onClick OnRemoveCard ] [ A.icon1 "icon-x" "Remove from project" ]
                        ]
                    )

            else
                text ""

        a1 =
            Debug.log "hey" "lazzzyy"
    in
    div
        [ class "media mediaBox is-hoverable is-size-7" ]
        [ div [ class "media-content is-smaller" ]
            [ div [ class "help mb-2 is-flex is-justify-content-space-between" ]
                [ div [ class "is-flex-inline" ] [ span [ class "mr-2" ] [ tensionIcon t.type_ ], text t.receiver.name, ellipsis ], div [] [ status_html ] ]
            , div []
                [ span [ class "link-like is-human mr-2" ] [ text t.title ]
                , case t.labels of
                    Just labels ->
                        viewLabels (Just focus.nameid) labels

                    Nothing ->
                        text ""
                ]
            ]
        ]



--
-- Utils
--


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
        (\a -> { a | cards = LE.removeAt c.pos a.cards })
        columns


getCard : String -> ProjectData -> Maybe ProjectCard
getCard cardid data =
    LE.find (\a -> a.id == cardid) (data.columns |> List.map .cards |> List.concat)
