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


module Components.ColorPicker exposing
    ( ColorPicker
    , ColumnRowOp
    , Op
    , close
    , columnNameInputId
    , init
    , initColor
    , open
    , setColor
    , view
    , viewColumnRow
    , viewPopup
    )

import Assets as A
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (attribute, class, disabled, id, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (withDefault)
import ModelSchema exposing (ColumnDraft)
import Text as T
import Utils.Bool exposing (ternary)
import Utils.DomEvents exposing (onClickSP)


type alias ColorPicker =
    { isOpen : Bool
    , color : String
    , colors : List String
    }


init : ColorPicker
init =
    { isOpen = False
    , color = initColor
    , colors =
        [ "#154b84"
        , "#0074D9"
        , "#7FDBFF"
        , "#39CCCC"
        , "#3D9970"
        , "#2ECC40"
        , "#01FF70"
        , "#FFDC00"
        , "#FF851B"
        , "#FF4136"
        , "#85144b"
        , "#F012BE"
        , "#B10DC9"
        , "#DDDDDD"
        , "#AAAAAA"
        , "#111111"
        ]
    }


initColor : String
initColor =
    "#154b84"



-- State control


open : ColorPicker -> ColorPicker
open data =
    { data | isOpen = True }


close : ColorPicker -> ColorPicker
close data =
    { data | isOpen = False }


setColor : Maybe String -> ColorPicker -> ColorPicker
setColor color_m data =
    case color_m of
        Just color ->
            { data | color = color }

        Nothing ->
            { data | color = initColor }


type alias Op msg =
    { data : ColorPicker
    , onOpen : msg
    , onClose : msg
    , onSelect : String -> msg
    }


view : Op msg -> Html msg
view op =
    span []
        [ button
            [ class "buttonColor"
            , attribute "style" ("background-color:" ++ op.data.color ++ ";")
            , onClickSP
                (if op.data.isOpen then
                    op.onClose

                 else
                    op.onOpen
                )
            ]
            []
        , viewPopup
            { id = "colorPicker"
            , isOpen = op.data.isOpen
            , colors = op.data.colors
            , onSelect = op.onSelect
            }
        ]


{-| Popup-only view for callers that own their swatch (e.g. per-row pickers).

The popup is mounted only when `isOpen` is true, so multiple instances on a page
are safe — at most one is in the DOM at a time. Caller is responsible for
matching `id` with `Ports.outsideClickClose`.

-}
viewPopup : { id : String, isOpen : Bool, colors : List String, onSelect : String -> msg } -> Html msg
viewPopup op =
    if op.isOpen then
        div [ id op.id ]
            [ div [ class "colorBoxes" ]
                [ span [ class "is-size-7" ]
                    [ text T.selectColor, text ":" ]
                , div []
                    (op.colors
                        |> List.map
                            (\c ->
                                button
                                    [ class "buttonColor"
                                    , onClick (op.onSelect c)
                                    , attribute "style" ("background-color:" ++ c ++ ";")
                                    ]
                                    []
                            )
                    )
                ]
            ]

    else
        text ""



-- Project column row (uses viewPopup as the per-row swatch picker).


{-| Build a stable DOM id for a column-name input. The `prefix` lets each caller
namespace its rows (e.g. "column-name" vs "ptemplate-column-name").
-}
columnNameInputId : String -> Int -> String
columnNameInputId prefix idx =
    prefix ++ "-" ++ String.fromInt idx


type alias ColumnRowOp msg =
    { col : ColumnDraft
    , idx : Int
    , nCols : Int
    , isPickerActive : Bool
    , colors : List String
    , inputIdPrefix : String
    , onOpenColor : msg
    , onCloseColor : msg
    , onSelectColor : String -> msg
    , onChangeName : String -> msg
    , onChangeDesc : String -> msg
    , onMove : Int -> msg
    , onRemove : msg
    }


viewColumnRow : ColumnRowOp msg -> Html msg
viewColumnRow op =
    let
        accent =
            withDefault "var(--bulma-border)" op.col.color

        isFirst =
            op.idx == 0

        isLast =
            op.idx == op.nCols - 1

        inputCls =
            "input is-small editable-soft-input"

        bareInputStyle =
            [ style "border" "none"
            , style "background" "transparent"
            , style "box-shadow" "none"
            ]
    in
    div
        [ class "p-3 mb-2 has-background-body"
        , style "border" "1px solid var(--bulma-border)"
        , style "border-left" ("4px solid " ++ accent)
        , style "border-radius" "var(--bulma-radius)"
        ]
        [ div [ class "is-flex is-align-items-center" ]
            [ span [ class "mr-2 is-flex-shrink-0" ]
                [ view
                    { data =
                        { isOpen = op.isPickerActive
                        , color = withDefault initColor op.col.color
                        , colors = op.colors
                        }
                    , onOpen = op.onOpenColor
                    , onClose = op.onCloseColor
                    , onSelect = op.onSelectColor
                    }
                ]
            , input
                ([ id (columnNameInputId op.inputIdPrefix op.idx)
                 , class inputCls
                 , type_ "text"
                 , value op.col.name
                 , placeholder T.name
                 , onInput op.onChangeName
                 , style "font-weight" "600"
                 , style "flex" "1"
                 ]
                    ++ bareInputStyle
                )
                []
            , div [ class "buttons has-addons mb-0 ml-2 is-flex-shrink-0" ]
                [ button
                    [ class "button is-small"
                    , disabled isFirst
                    , onClick (op.onMove -1)
                    , attribute "title" T.moveUp
                    ]
                    [ A.icon "icon-chevron-up" ]
                , button
                    [ class "button is-small"
                    , disabled isLast
                    , onClick (op.onMove 1)
                    , attribute "title" T.moveDown
                    ]
                    [ A.icon "icon-chevron-down" ]
                , button
                    [ class "button is-small has-text-danger"
                    , disabled (op.nCols <= 1)
                    , onClick op.onRemove
                    , attribute "title" T.removeColumn
                    ]
                    [ A.icon "icon-x" ]
                ]
            ]
        , input
            ([ class inputCls
             , type_ "text"
             , value op.col.description
             , placeholder T.descriptionOptional
             , onInput op.onChangeDesc
             , style "color" "var(--bulma-text-weak)"
             , style "font-size" "0.85rem"
             ]
                ++ bareInputStyle
            )
            []
        ]
