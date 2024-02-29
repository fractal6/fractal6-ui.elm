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


module Components.ModalConfirm exposing (..)

import Bulk exposing (UserState(..))
import Bulk.Codecs exposing (ActionType(..), DocType(..))
import Extra.Views exposing (showMsg)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (attribute, class, classList, id)
import Html.Events exposing (onClick)
import Loading exposing (ModalData, RequestResult(..))
import ModelSchema exposing (..)
import Text as T


type alias ModalConfirm msg =
    { isOpen : Bool
    , msg : msg
    , mess : TextMessage
    }


type alias TextMessage =
    { message : Maybe ( String, String )
    , txts : List ( String, String )
    }


init : msg -> ModalConfirm msg
init m =
    { isOpen = False, msg = m, mess = { message = Nothing, txts = [ ( "", "" ) ] } }


open : msg -> TextMessage -> ModalConfirm msg -> ModalConfirm msg
open m mess model =
    { model | isOpen = True, msg = m, mess = mess }


close : ModalConfirm msg -> ModalConfirm msg
close model =
    { model | isOpen = False }


type alias Op msg =
    { data : ModalConfirm msg
    , onClose : ModalData -> msg
    , onConfirm : msg
    }


view : Op msg -> Html msg
view op =
    div
        [ id "confirmModal"
        , class "modal is-light modal-fx-fadeIn"
        , classList [ ( "is-active", op.data.isOpen ) ]
        , attribute "data-modal-close" "closeModalConfirmFromJs"
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "confirmModal"
            , onClick (op.onClose { reset = True, link = "" })
            ]
            []
        , div [ class "modal-content" ]
            [ viewConfirm op ]
        , button [ class "modal-close is-large", onClick (op.onClose { reset = True, link = "" }) ] []
        ]


viewConfirm : Op msg -> Html msg
viewConfirm op =
    div [ class "modal-card" ]
        [ div [ class "modal-card-body" ]
            [ case op.data.mess.message of
                Just m ->
                    showMsg "0" "is-info is-light" "icon-info" (Tuple.first m) (Tuple.second m)

                Nothing ->
                    text ""
            , op.data.mess.txts |> List.map (\( x, y ) -> span [ class y ] [ text x ]) |> List.intersperse (text " ") |> span []
            , div [ class "field is-grouped is-grouped-right" ]
                [ div [ class "buttons" ]
                    [ button
                        [ class "button is-small is-success", onClick op.onConfirm ]
                        [ text T.confirm ]
                    , button
                        [ class "button is-small is-danger", onClick (op.onClose { reset = True, link = "" }) ]
                        [ text T.cancel ]
                    ]
                ]
            ]
        ]
