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
import Bulk exposing (UserState(..), getParentFragmentFromRole)
import Bulk.Codecs exposing (NodeFocus, uriFromNameid, uriFromUsername)
import Bulk.View exposing (mediaTension)
import Dict exposing (Dict)
import Extra exposing (colorAttr, ternary, upH)
import Extra.Date exposing (formatDate)
import Extra.Events exposing (onClickPD, onDragEnd, onDragEnter, onDragLeave, onDragStart, onDrop, onEnter, onKeydown, onTab)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.Lang as Lang
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.NodeVisibility as NodeVisibility
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (toHref)
import Html exposing (Html, a, br, button, div, hr, i, p, span, sub, text)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, style, title)
import Html.Events exposing (onClick)
import Html.Lazy as Lazy
import Identicon
import List.Extra as LE
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelSchema exposing (Tension)
import Session exposing (Conf)
import String.Extra as SE
import Text as T
import Time


type alias Op msg =
    { hasTaskMove : Bool
    , conf : Conf
    , node_focus : NodeFocus
    , boardId : String
    , boardHeight : Maybe Float
    , movingTension : Maybe Tension
    , movingHoverC : Maybe { pos : Int, to_receiverid : String }
    , movingHoverT : Maybe { pos : Int, tid : String, to_receiverid : String }

    -- Board Msg
    , onColumnHover : Maybe String -> msg
    , onMove : { pos : Int, to_receiverid : String } -> Tension -> msg
    , onCancelHov : msg
    , onEndMove : msg
    , onMoveEnterC : { pos : Int, to_receiverid : String } -> Bool -> msg
    , onMoveLeaveC : msg
    , onMoveEnterT : { pos : Int, tid : String, to_receiverid : String } -> msg
    , onMoveDrop : String -> msg
    , onNoTask : msg
    }


viewBoard : Op msg -> (String -> Maybe Tension -> Html msg) -> List String -> Dict String (List Tension) -> Html msg
viewBoard op header keys data =
    keys
        |> List.indexedMap
            (\i n ->
                let
                    tensions =
                        Dict.get n data |> withDefault []

                    j_last =
                        List.length tensions - 1

                    t_m =
                        List.head tensions
                in
                [ div
                    ([ class "column is-3" ]
                        ++ ternary op.hasTaskMove
                            [ onDragEnter (op.onMoveEnterC { pos = i, to_receiverid = n } False)
                            , onDragLeave op.onMoveLeaveC

                            -- @DEBUG doesn't work
                            --, onDrop (OnMoveDrop n)
                            , attribute "ondragover" "return false"

                            --, onMouseEnter (OnColumnHover (Just n)
                            ]
                            []
                    )
                    [ div [ class "subtitle is-aligned-center mb-0 pb-3" ] [ header n t_m ]
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
                                            && (j_last == j && Maybe.map .pos op.movingHoverC == Just i)

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
                                                        [ onDragLeave (op.onMoveEnterC { pos = i, to_receiverid = t.receiver.nameid } True) ]

                                                    else
                                                        []
                                                   )
                                            )
                                            [ mediaTension op.conf op.node_focus t True False "is-size-6" ]
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
                if List.length x == 0 then
                    [ div [ class "ml-6 p-6" ]
                        [ text T.noTensionsAssigneesYet
                        , ternary (op.node_focus.nameid /= op.node_focus.rootnameid)
                            (span [ class "help-label button-light is-h is-discrete", onClick op.onNoTask ] [ A.icon "arrow-up", text T.goRoot ])
                            (text "")
                        ]
                    ]

                else
                    x
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
