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


module Bulk.Board exposing (..)

import Assets as A
import Bulk exposing (UserState(..))
import Bulk.Codecs exposing (NodeFocus)
import Bulk.View exposing (mediaTension)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Extra.Events exposing (onDragEnd, onDragEnter, onDragLeave, onDragStart)
import Html exposing (Html, div, i, span, text)
import Html.Attributes exposing (attribute, class, classList, id, style)
import Html.Events exposing (onClick)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelSchema exposing (Tension)
import Session exposing (Conf)
import Text as T


type alias Op msg =
    { hasTaskMove : Bool
    , hasNewCol : Bool
    , conf : Conf
    , node_focus : NodeFocus
    , boardId : String
    , boardHeight : Maybe Float
    , movingTension : Maybe Tension
    , movingHoverCol : Maybe { pos : Int, to_receiverid : String }
    , movingHoverT : Maybe { pos : Int, tid : String, to_receiverid : String }

    -- Board Msg
    , onColumnHover : Maybe String -> msg
    , onMove : { pos : Int, to_receiverid : String } -> Tension -> msg
    , onCancelHov : msg
    , onEndMove : msg
    , onMoveEnterCol : { pos : Int, to_receiverid : String } -> Bool -> msg
    , onMoveLeaveCol : msg
    , onMoveEnterT : { pos : Int, tid : String, to_receiverid : String } -> msg
    , onMoveDrop : String -> msg
    , noMsg : msg
    , onAddCol : msg
    }


{-| The given code defines a function `viewBoard` which takes four arguments:

1.  `op`: representing the operations (msgs) that can be performed on the given board.
2.  `header`: which is a function to display the header of each column in the board. The parameter of the header functioun are
    1.  An unique identifier of the column
    2.  A potential name for that column
    3.  The potential first tension of the column
3.  `keys_title`: A list of pairs representing the keys and names of each column in the board.
4.  `data`: representing the tensions in each column of the board.

The function returns an HTML structure which displays the board with each column having its own header and list of tensions.

This function generates a drag-and-drop board with the ability to move tension items around.

-}
viewBoard : Op msg -> (String -> String -> Maybe Tension -> Html msg) -> List ( String, String ) -> Dict String (List Tension) -> Html msg
viewBoard op header keys_title data =
    keys_title
        |> List.indexedMap
            (\i ( key, name ) ->
                let
                    tensions =
                        Dict.get key data |> withDefault []

                    j_last =
                        List.length tensions - 1

                    t_m =
                        List.head tensions
                in
                [ div
                    ([ class "column is-3" ]
                        ++ ternary op.hasTaskMove
                            [ onDragEnter (op.onMoveEnterCol { pos = i, to_receiverid = key } False)
                            , onDragLeave op.onMoveLeaveCol

                            -- @DEBUG doesn't work
                            --, onDrop (OnMoveDrop key)
                            , attribute "ondragover" "return false"

                            --, onMouseEnter (OnColumnHover (Just key)
                            ]
                            []
                    )
                    [ div [ class "subtitle is-aligned-center mb-0 pb-3" ] [ header key name t_m ]
                    , tensions
                        --|> List.sortBy .createdAt
                        --|> (\l -> ternary (model.sortFilter == defaultSortFilter) l (List.reverse l))
                        |> List.indexedMap
                            (\j t ->
                                let
                                    draggedTid =
                                        Maybe.map .id op.movingTension

                                    itemDragged =
                                        draggedTid == Just t.id

                                    upperTid =
                                        LE.getAt (j - 1) tensions |> Maybe.map .id

                                    isHoveredUp =
                                        -- exclude the dragged item
                                        not itemDragged
                                            -- hovered tension
                                            && (Maybe.map .tid op.movingHoverT == Just t.id)
                                            -- exclude if the dragged item is next
                                            && (draggedTid /= upperTid)

                                    hasLastColumn =
                                        -- exclude the dragged item
                                        not itemDragged
                                            -- nothing to drag
                                            && (op.movingHoverT == Nothing)
                                            -- last item
                                            && (j_last == j && Maybe.map .pos op.movingHoverCol == Just i)

                                    draggingDiv =
                                        div
                                            [ class "box is-shrinked2 mb-2 mx-2 is-dragging is-growing"
                                            , style "opacity" "0.6"

                                            --, style "height" "0rem"
                                            ]
                                            []
                                in
                                ternary isHoveredUp
                                    [ draggingDiv ]
                                    []
                                    ++ [ div
                                            ([ class "box is-shrinked2 mb-2 mx-2"
                                             ]
                                                ++ ternary op.hasTaskMove
                                                    [ classList [ ( "is-dragging", op.movingHoverT /= Nothing ) ]
                                                    , attribute "draggable" "true"
                                                    , attribute "ondragstart" "event.dataTransfer.setData(\"text/plain\", \"dummy\")"
                                                    , onDragStart <| op.onMove { pos = i, to_receiverid = t.receiver.nameid } t
                                                    , onDragEnd op.onEndMove
                                                    , onDragEnter (op.onMoveEnterT { pos = j, tid = t.id, to_receiverid = t.receiver.nameid })
                                                    ]
                                                    []
                                                ++ (if j_last == j && op.hasTaskMove then
                                                        -- reset hoverT to draw below
                                                        [ onDragLeave (op.onMoveEnterCol { pos = i, to_receiverid = t.receiver.nameid } True) ]

                                                    else
                                                        []
                                                   )
                                            )
                                            [ mediaTension { noMsg = op.noMsg } op.conf op.node_focus t True False "is-size-6" ]
                                       ]
                                    ++ ternary hasLastColumn
                                        [ draggingDiv ]
                                        []
                            )
                        |> List.concat
                        |> div [ class "content scrollbar-thin" ]
                    ]
                , div [ class "divider is-vertical2 is-small is-hidden-mobile" ] []
                ]
            )
        |> List.concat
        |> (\x ->
                x
                    ++ (if op.hasNewCol then
                            [ viewNewCol op ]

                        else
                            []
                       )
           )
        |> div
            [ id op.boardId
            , class "columns is-fullwidth is-marginless is-mobile kb-board"

            --, onMouseLeave (OnColumnHover Nothing)
            , attribute "style" <|
                case op.boardHeight of
                    Just h ->
                        "overflow-y: hidden; overflow-x: auto; height:" ++ String.fromFloat h ++ "px;"

                    Nothing ->
                        "overflow-y: hidden; overflow-x: auto;"
            ]


viewNewCol : Op msg -> Html msg
viewNewCol op =
    div [ class "column is-2" ]
        [ div
            [ class "has-border is-dashed is-rounded-light is-aligned-center is-h is-w p-6 pl-5"
            , style "width" "100%"
            , onClick op.onAddCol
            ]
            [ A.icon1 "icon-plus" "Add column" ]
        ]
