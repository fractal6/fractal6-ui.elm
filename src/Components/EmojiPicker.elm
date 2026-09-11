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


port module Components.EmojiPicker exposing (Msg(..), State, init, isOpen_, subscriptions, update, viewEmojiSeeker)

import Components.EmojiData exposing (Emoji, searchEmojis)
import Utils.DomEvents exposing (onMousedownPD)
import Html exposing (Html, div, p, span, text)
import Html.Attributes exposing (attribute, class, classList, title)
import List.Extra as LE
import Ports
import Session exposing (GlobalCmd(..))
import Text as T


type State
    = State Model


type alias Model =
    { isOpen : Bool
    , pattern : String
    , results : List Emoji
    , activePos : Int
    , isArrowMode : Bool
    }


{-| Emojis per row in the grid; mirrors the CSS layout (305px / 32px).
Used to translate ArrowUp/Down into a row jump. Update if `_emoji.scss`
changes the picker width or item size.
-}
rowSize : Int
rowSize =
    9


init : State
init =
    State (resetAll { isOpen = False, pattern = "", results = [], activePos = 0, isArrowMode = False })


isOpen_ : State -> Bool
isOpen_ (State model) =
    model.isOpen


resetNav : Model -> Model
resetNav m =
    { m | activePos = 0, isArrowMode = False }


resetAll : Model -> Model
resetAll m =
    resetNav { m | pattern = "", results = searchEmojis Nothing "" }


type Msg
    = OnOpen
    | OnClose
    | OnChangePattern String
    | OnClickEmoji String
    | OnArrowMove String
    | OnSelectActive
    | NoMsg


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
            ( State (resetAll { model | isOpen = True }), out0 )

        OnClose ->
            ( State (resetAll { model | isOpen = False }), out0 )

        OnChangePattern pattern ->
            ( State (resetNav { model | pattern = pattern, results = searchEmojis Nothing pattern })
            , out0
            )

        OnClickEmoji emoji ->
            ( State (resetAll { model | isOpen = False })
            , { out0 | result = Just emoji }
            )

        OnArrowMove dir ->
            if model.isOpen && not (List.isEmpty model.results) then
                if not model.isArrowMode then
                    ( State { model | activePos = 0, isArrowMode = True }, out0 )

                else
                    let
                        delta =
                            case dir of
                                "up" ->
                                    -rowSize

                                "down" ->
                                    rowSize

                                "left" ->
                                    -1

                                "right" ->
                                    1

                                _ ->
                                    0

                        newPos =
                            clamp 0 (List.length model.results - 1) (model.activePos + delta)
                    in
                    ( State { model | activePos = newPos }, out0 )

            else
                ( State model, out0 )

        OnSelectActive ->
            case LE.getAt model.activePos model.results of
                Just e ->
                    update (OnClickEmoji e.unicode) (State model)

                Nothing ->
                    ( State model, out0 )

        NoMsg ->
            ( State model, out0 )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    [ openEmojiPickerFromJs (\_ -> OnOpen) ]
        ++ (if model.isOpen then
                [ closeEmojiPickerFromJs (\_ -> OnClose)
                , changeEmojiPatternFromJs OnChangePattern
                , Ports.arrowFromJs OnArrowMove
                , Ports.selectActiveItemFromJs (\_ -> OnSelectActive)
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
                        |> List.indexedMap
                            (\i e ->
                                span
                                    [ class "emojiItem"
                                    , classList [ ( "is-active", model.isArrowMode && model.activePos == i ) ]
                                    , title e.name
                                    , onMousedownPD (OnClickEmoji e.unicode)
                                    ]
                                    [ text e.unicode ]
                            )
            ]
        , if model.results == [] then
            text ""

          else
            p [ class "help-label is-weak p-1", attribute "style" "cursor: default !important;", onMousedownPD NoMsg ] [ text T.typeToFilterEmoji ]
        ]
