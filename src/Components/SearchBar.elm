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


module Components.SearchBar exposing (viewSearchBar)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Bulk exposing (UserState(..))
import Bulk.Error exposing (viewGqlErrors)
import Dict
import Extra exposing (ternary)
import Extra.Events exposing (onKeydown)
import Html exposing (Html, button, div, i, input, span, text)
import Html.Attributes exposing (attribute, autocomplete, autofocus, class, disabled, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Loading exposing (GqlData, ModalData, RequestResult(..), withMaybeData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Ports
import Text as T
import Time


type alias Op msg =
    { id_name : String
    , placeholder_txt : String
    , onChangePattern : String -> msg
    , onSearchKeyDown : Int -> msg
    , onSubmitText : String -> msg
    }


viewSearchBar : Op msg -> String -> String -> Html msg
viewSearchBar op pattern_init pattern =
    div [ id op.id_name, class "searchBar" ]
        [ div [ class "columns mb-0" ]
            [ div [ class "column is-8" ]
                [ div [ class "field has-addons" ]
                    [ div [ class "control is-expanded" ]
                        [ input
                            [ class "is-rounded input is-small pr-6"
                            , type_ "search"
                            , autocomplete False
                            , autofocus False
                            , placeholder op.placeholder_txt
                            , value pattern
                            , onInput op.onChangePattern
                            , onKeydown op.onSearchKeyDown
                            ]
                            []
                        , span [ class "icon-input-flex-right" ]
                            [ if pattern_init /= "" then
                                span [ class "delete is-hidden-mobile", onClick (op.onSubmitText "") ] []

                              else
                                text ""
                            , span [ class "vbar has-border-color" ] []
                            , span [ class "button-light is-w px-1", onClick (op.onSearchKeyDown 13) ]
                                [ A.icon "icon-search" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
