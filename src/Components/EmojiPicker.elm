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


port module Components.EmojiPicker exposing (Msg(..), State, init, subscriptions, update, viewEmojiSeeker)

import Components.EmojiData exposing (Emoji, searchEmojis)
import Extra.Events exposing (onMousedownPD)
import Html exposing (Html, div, p, span, text)
import Html.Attributes exposing (attribute, class, title)
import Session exposing (GlobalCmd(..))


type State
    = State Model


type alias Model =
    { isOpen : Bool
    , pattern : String
    , results : List Emoji
    }


init : State
init =
    State
        { isOpen = False
        , pattern = ""
        , results = searchEmojis Nothing ""
        }


type Msg
    = OnOpen
    | OnClose
    | OnChangePattern String
    | OnClickEmoji String


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe String
    }


out0 : Out
out0 =
    { cmds = [], gcmds = [], result = Nothing }


update : Msg -> State -> ( State, Out )
update msg (State model) =
    case msg of
        OnOpen ->
            ( State { model | isOpen = True, pattern = "", results = searchEmojis Nothing "" }
            , out0
            )

        OnClose ->
            ( State { model | isOpen = False, pattern = "", results = searchEmojis Nothing "" }
            , out0
            )

        OnChangePattern pattern ->
            ( State { model | pattern = pattern, results = searchEmojis Nothing pattern }
            , out0
            )

        OnClickEmoji emoji ->
            ( State { model | isOpen = False, pattern = "", results = searchEmojis Nothing "" }
            , { out0 | result = Just emoji }
            )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    [ openEmojiPickerFromJs (\_ -> OnOpen) ]
        ++ (if model.isOpen then
                [ closeEmojiPickerFromJs (\_ -> OnClose)
                , changeEmojiPatternFromJs OnChangePattern
                ]

            else
                []
           )


port openEmojiPickerFromJs : (() -> msg) -> Sub msg


port closeEmojiPickerFromJs : (() -> msg) -> Sub msg


port changeEmojiPatternFromJs : (String -> msg) -> Sub msg



-- ------------------------------
-- V I E W
-- ------------------------------


viewEmojiSeeker : State -> Html Msg
viewEmojiSeeker (State model) =
    div [ class "panel dropList emojiPicker" ]
        [ div [ class "selectors" ]
            [ div [ class "emojiGrid" ] <|
                if model.results == [] then
                    [ p [ class "panel-block help-label is-static", attribute "style" "cursor: default !important;" ] [ text "No emoji found" ] ]

                else
                    model.results
                        |> List.map
                            (\e ->
                                span
                                    [ class "emojiItem"
                                    , title e.name
                                    , onMousedownPD (OnClickEmoji e.unicode)
                                    ]
                                    [ text e.unicode ]
                            )
            ]
        ]
