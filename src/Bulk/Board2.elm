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


module Bulk.Board2 exposing (..)

import Assets as A
import Bulk exposing (UserState(..))
import Bulk.Bulma as B
import Bulk.Codecs exposing (ActionType(..), NodeFocus, getTensionCharac, nid2rootid)
import Bulk.View exposing (action2icon, action2str, mediaTension, statusColor, tensionIcon, tensionStatus2str, tensionType2str, viewLabels)
import Dict exposing (Dict)
import Extra exposing (insertAt, ternary, unwrap)
import Extra.Events exposing (onClickPD, onDragEnd, onDragEnter, onDragLeave, onDragStart, onKeydown)
import Fractal.Enum.TensionStatus as TensionStatus
import Generated.Route as Route exposing (toHref)
import Html exposing (Html, a, br, div, hr, i, span, text)
import Html.Attributes exposing (attribute, autofocus, class, classList, contenteditable, href, id, style, target)
import Html.Events exposing (onBlur, onClick, onInput, onMouseEnter, onMouseLeave)
import Html.Lazy as Lazy
import Json.Decode as JD
import Json.Encode as JE
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelSchema exposing (CardKind(..), Post, ProjectCard, ProjectColumn, ProjectDraft, Tension, UserCtx)
import Session exposing (Conf)
import Text as T


type alias DraftForm =
    { uctx : UserCtx
    , title : String
    , colid : String
    , pos : Int
    , post : Post
    , tids : List (Maybe String)
    }


type alias Op msg =
    { hasTaskMove : Bool
    , hasNewCol : Bool
    , isAddingDraft : Maybe DraftForm
    , conf : Conf
    , node_focus : NodeFocus
    , boardId : String
    , boardHeight : Maybe Float
    , movingCard : Maybe ProjectCard
    , movingHoverCol : Maybe { pos : Int, colid : String, length : Int }
    , movingHoverT : Maybe { pos : Int, cardid : String, colid : String }
    , cardHover : String
    , cardEdit : String

    -- Board Msg
    , onMove : { pos : Int, colid : String, length : Int } -> ProjectCard -> msg
    , onCancelHov : msg
    , onEndMove : msg
    , onMoveEnterCol : { pos : Int, colid : String, length : Int } -> Bool -> msg
    , onMoveLeaveCol : msg
    , onMoveEnterT : { pos : Int, cardid : String, colid : String } -> msg
    , onMoveDrop : String -> msg
    , onCardClick : Maybe ProjectCard -> msg
    , noMsg : msg
    , onAddCol : msg
    , onDraftEdit : String -> msg
    , onDraftKeydown : Int -> msg
    , onDraftCancel : msg
    , onCardHover : String -> msg
    , onCardHoverLeave : msg
    , onToggleCardEdit : msg
    , onRemoveCard : msg
    }


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
viewBoard : Op msg -> (ProjectColumn -> Maybe ProjectCard -> Html msg) -> List ProjectColumn -> Html msg
viewBoard op header columns =
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
                        :: ternary op.hasTaskMove
                            [ onDragEnter (op.onMoveEnterCol { pos = i, colid = colid, length = j_last + 1 } False)
                            , onDragLeave op.onMoveLeaveCol

                            -- @DEBUG doesn't work
                            --, onDrop (OnMoveDrop colid)
                            , attribute "ondragover" "return false"
                            ]
                            []
                    )
                    [ div
                        [ class "subtitle"
                        , onDragEnter (op.onMoveEnterT { pos = 0, cardid = unwrap "" .id c1, colid = colid })
                        ]
                        [ header col c1 ]
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
                                        :: ternary op.hasTaskMove
                                            [ classList
                                                [ ( "is-dragging", op.movingHoverT /= Nothing )
                                                , ( "is-dragged", Maybe.map .id op.movingCard == Just card.id )
                                                ]
                                            , attribute "draggable" "true"
                                            , attribute "ondragstart" "event.dataTransfer.setData(\"text/plain\", \"dummy\")"
                                            , onDragStart <| op.onMove { pos = i, colid = colid, length = j_last + 1 } card
                                            , onDragEnd op.onEndMove
                                            , onDragEnter (op.onMoveEnterT { pos = j, cardid = card.id, colid = colid })
                                            , onClick (op.onCardClick (Just card))
                                            , onMouseEnter (op.onCardHover card.id)
                                            , onMouseLeave op.onCardHoverLeave
                                            ]
                                            []
                                        ++ ternary (j_last == j && op.hasTaskMove)
                                            -- reset hoverT to draw below
                                            [ onDragLeave (op.onMoveEnterCol { pos = i, colid = colid, length = j_last + 1 } True) ]
                                            []
                                    )
                                    (case card.card of
                                        CardTension t ->
                                            -- Does lazy will work with function in argment?
                                            [ Lazy.lazy6 viewMediaTension (card.id == op.cardHover) (card.id == op.cardEdit) op.node_focus t op.onToggleCardEdit op.onRemoveCard ]

                                        CardDraft d ->
                                            [ Lazy.lazy2 viewMediaDraft (card.id == op.cardHover) d ]
                                    )
                                ]
                            )
                        |> List.concat
                        |> (\x ->
                                -- View Draft content editable
                                case op.isAddingDraft of
                                    Just form ->
                                        if form.colid == colid then
                                            x ++ [ viewDraftEditable op form ]

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
                                            op.movingCard
                                            op.movingHoverT
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
                if op.hasNewCol then
                    x ++ [ viewNewCol op ]

                else
                    x
           )
        |> div
            [ id op.boardId
            , class "columns is-fullwidth is-marginless is-mobile kb-board board2"
            , attribute "style" <|
                case op.boardHeight of
                    Just h ->
                        "overflow-y: hidden; overflow-x: auto; height:" ++ String.fromFloat h ++ "px;"

                    Nothing ->
                        "overflow-y: hidden; overflow-x: auto;"
            ]


draggingDiv : Html msg
draggingDiv =
    div
        [ class "box is-shrinked2 mb-2 mx-2 is-dragging is-growing has-border-link"
        , style "opacity" "0.6"

        --, style "height" "0rem"
        ]
        []


viewNewCol : Op msg -> Html msg
viewNewCol op =
    div [ class "column is-2 ml-2" ]
        [ div
            [ class "has-border is-dashed is-rounded-light is-aligned-center is-h is-w p-6 pl-5"
            , style "width" "100%"
            , onClick op.onAddCol
            ]
            [ A.icon1 "icon-plus" "Add column" ]
        ]


viewMediaDraft : Bool -> ProjectDraft -> Html msg
viewMediaDraft isHovered d =
    div [ class "media mediaBox is-hoverable" ]
        [ div [ class "media-content is-smaller" ]
            [ div [ class "is-wrapped help is-icon-aligned mb-2" ] [ A.icon1 "icon-circle-draft" "Draft" ]
            , div [] [ span [ class "link-like is-human" ] [ text d.title ] ]
            ]
        ]


viewDraftEditable : Op msg -> DraftForm -> Html msg
viewDraftEditable op form =
    div
        [ id "draft-card-editable"
        , class "box is-shrinked2 mb-2 mx-2 p-2"
        , contenteditable True
        , autofocus True
        , onKeydown op.onDraftKeydown
        , onBlur op.onDraftCancel

        -- OnInput does not work on contenteditable: https://github.com/elm/html/issues/24
        --, onInput op.onDraftEdit
        , Html.Events.on "input" (JD.map op.onDraftEdit innerHtmlDecoder)
        , Html.Attributes.property "innerHTML" (JE.string form.title)
        ]
        []


innerHtmlDecoder =
    JD.at [ "target", "innerHTML" ] JD.string


viewMediaTension : Bool -> Bool -> NodeFocus -> Tension -> msg -> msg -> Html msg
viewMediaTension isHovered isEdited focus t onToggleCardEdit onRemoveCard =
    let
        k =
            Debug.log "hey" ""

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
                --span [ class "px-2 has-text-text" ]
                --    [ A.icon "icon-more-horizontal is-h icon-bg" ]
                B.dropdownLight
                    "card-ellipsis"
                    ("px-2 has-text-text " ++ ternary isEdited "is-active" "")
                    (A.icon "icon-more-horizontal is-h icon-bg")
                    onToggleCardEdit
                    (div []
                        [ div [ class "dropdown-item button-light" ] [ a [ class "stealth-link", href (Route.Tension_Dynamic_Dynamic { param1 = nid2rootid t.receiver.nameid, param2 = t.id } |> toHref), target "_blank" ] [ text " 🡕 ", text "Open in a new tab" ] ]
                        , hr [ class "dropdown-divider" ] []
                        , div [ class "dropdown-item button-light", onClick onRemoveCard ] [ text "Remove from project" ]
                        ]
                    )

            else
                text ""
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
