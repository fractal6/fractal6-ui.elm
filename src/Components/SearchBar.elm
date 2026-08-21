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
   along with Fractale.  If not, see <https://www.gnu.org/licenses/>.
-}


module Components.SearchBar exposing (Op, viewSearchBarCol, viewSearchBarLevel, viewSearchField)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Fractale.User exposing (UserState(..))
import Fractale.Error exposing (viewGqlErrors)
import Dict
import Utils.Bool exposing (ternary)
import Utils.DomEvents exposing (onMousedownPD)
import Html exposing (Html, button, div, form, i, input, span, text)
import Html.Attributes exposing (attribute, autofocus, class, disabled, id, name, placeholder, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput, onSubmit)
import Json.Decode as JD
import Loading exposing (GqlData, ModalData, RequestResult(..), withMaybeData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Ports
import Text as T
import Time


type alias Op msg =
    { id_name : String
    , column_class : String
    , field_class : String
    , placeholder_txt : String
    , onChangePattern : String -> msg
    , onSearchKeyDown : Int -> msg
    , onSubmitText : String -> msg
    }


{-| Bare search field (no layout wrapper). For embedding in custom layouts.
-}
viewSearchField : Op msg -> String -> String -> Html msg
viewSearchField op pattern_init pattern =
    let
        -- Per-bar token: tensions / projects / members / activities / orgs stay in separate native histories.
        historyName =
            "fractale-" ++ op.id_name
    in
    form [ class ("field has-addons searchBar " ++ op.field_class), onSubmit (op.onSearchKeyDown 13) ]
        [ div [ class "control is-expanded" ]
            [ input
                [ id op.id_name
                , class "is-rounded input is-small pr-6"
                , type_ "search"
                , name historyName
                , attribute "autocomplete" historyName
                , autofocus False
                , placeholder op.placeholder_txt
                , value pattern
                , onInput op.onChangePattern
                , on "keydown" (onKeydownExceptEnter op.onSearchKeyDown)
                ]
                []

            -- onMousedownPD protects from focus stealing on click.
            , span [ class "icon-input-flex-right", onMousedownPD (op.onSearchKeyDown 0) ]
                [ if pattern_init /= "" then
                    span [ class "delete is-hidden-mobile", onClick (op.onSubmitText "") ] []

                  else
                    text ""
                , span [ class "vbar" ] []
                , span [ class "button-light px-1", onClick (op.onSearchKeyDown 13) ]
                    [ A.icon "icon-search" ]
                ]
            ]
        ]


{-| Search bar with columns layout wrapper.
-}
viewSearchBarCol : Op msg -> String -> String -> Html msg
viewSearchBarCol op pattern_init pattern =
    div [ id op.id_name, class "searchBar" ]
        [ div [ class "columns mb-0" ]
            [ div [ class ("column " ++ op.column_class) ]
                [ viewSearchField op pattern_init pattern ]
            ]
        ]


{-| Search bar with level layout wrapper (search left, custom content right).
-}
viewSearchBarLevel : Op msg -> String -> String -> List (Html msg) -> Html msg
viewSearchBarLevel op pattern_init pattern rightContent =
    div [ class "level" ]
        [ div [ class "level-left is-flex-grow" ]
            [ viewSearchField op pattern_init pattern ]
        , div [ class "level-right" ]
            rightContent
        ]


-- Enter fires keydown 13 then form submit; both would call onSearchKeyDown 13, so skip 13 here.
onKeydownExceptEnter : (Int -> msg) -> JD.Decoder msg
onKeydownExceptEnter toMsg =
    keyCode
        |> JD.andThen
            (\k ->
                if k == 13 then
                    JD.fail "enter"

                else
                    JD.succeed (toMsg k)
            )
