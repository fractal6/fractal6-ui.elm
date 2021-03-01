module Components.ModalConfirm exposing (..)

import Icon as I
import Components.Loading as Loading exposing (GqlData, ModalData, RequestResult(..), loadingSpin, viewGqlErrors, withMapData, withMaybeData)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, selected, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (ActionForm, UserState(..), initActionForm)
import ModelCommon.Codecs exposing (ActionType(..), DocType(..), NodeFocus, TensionCharac, nearestCircleid, nid2rootid, typeFromNameid)
import ModelCommon.View exposing (viewUser)
import ModelSchema exposing (..)
import String.Format as Format
import Text as T exposing (textH, textT)
import Time


type alias ModalConfirm msg =
    { isOpen : Bool
    , msg : msg
    , txts : TextMessage
    }


type alias TextMessage =
    List ( String, String )


init : msg -> ModalConfirm msg
init m =
    { isOpen = False, msg = m, txts = [ ( "", "" ) ] }


open : msg -> TextMessage -> ModalConfirm msg -> ModalConfirm msg
open m txt model =
    { model | isOpen = True, msg = m, txts = txt }


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
        , class "modal modal-fx-fadeIn"
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
            [ op.data.txts |> List.map (\( x, y ) -> span [ class y ] [ text x ]) |> List.intersperse (text " ") |> span []
            , div [ class "field is-grouped is-grouped-right" ]
                [ div [ class "buttons" ]
                    [ button
                        [ class "button is-small is-danger", onClick (op.onClose { reset = True, link = "" }) ]
                        [ text "Cancel" ]
                    , button
                        [ class "button is-small is-success", onClick op.onConfirm ]
                        [ text "Confirm" ]
                    ]
                ]
            ]
        ]
