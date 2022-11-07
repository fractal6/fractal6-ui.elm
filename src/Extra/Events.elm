{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2022 Fractale Co

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


module Extra.Events exposing (..)

import Html
import Html.Events
    exposing
        ( custom
        , keyCode
        , on
        , preventDefaultOn
        , stopPropagationOn
        )
import Json.Decode as JD



--
-- Utils
--
-- Need the target attr to be set to _self as a workaround: https://github.com/elm/browser/issues/74


onClickPD : msg -> Html.Attribute msg
onClickPD msg =
    preventDefaultOn "click" <| JD.succeed ( msg, True )


onClickPD2 : msg -> Html.Attribute msg
onClickPD2 message =
    custom "click" <|
        JD.map
            (\_ ->
                { message = message
                , stopPropagation = True
                , preventDefault = True
                }
            )
            (JD.succeed message)



--
-- Element positioning
--


onClickPos message =
    --onClickPos : msg -> Html.Attribute msg
    on "click" (JD.map message eventPos)


eventPos : JD.Decoder (Maybe ( Int, Int ))
eventPos =
    JD.maybe <|
        JD.map2 Tuple.pair
            --(JD.at [ "target", "offsetTop" ] JD.int)
            --(JD.at [ "target", "offsetLeft" ] JD.int)
            (JD.field "clientX" JD.int)
            (JD.field "clientY" JD.int)



--
-- Loading
--


onLoad : msg -> Html.Attribute msg
onLoad message =
    on "onrender" <| JD.succeed message



--
-- Hot-key event
--


onKeydown : (Int -> msg) -> Html.Attribute msg
onKeydown message =
    let
        decodeKey =
            JD.andThen JD.succeed keyCode
    in
    on "keydown" <| JD.map (\key -> message key) decodeKey


onKeyup : (Int -> msg) -> Html.Attribute msg
onKeyup message =
    let
        decodeKey =
            JD.andThen JD.succeed keyCode
    in
    on "keyup" <| JD.map (\key -> message key) decodeKey


onKeyCode : Int -> Bool -> Bool -> msg -> Html.Attribute msg
onKeyCode expectedCode a b message =
    --on "keyup"
    custom "keyup" <|
        JD.map
            (\_ ->
                { message = message
                , stopPropagation = a
                , preventDefault = b
                }
            )
            (JD.andThen
                (\code ->
                    if code == expectedCode then
                        JD.succeed message

                    else
                        JD.fail (String.fromInt code)
                )
                keyCode
            )


onEnter : msg -> Html.Attribute msg
onEnter message =
    onKeyCode 13 False False message


onTab : msg -> Html.Attribute msg
onTab message =
    onKeyCode 9 False False message



--
-- Dragging
--


onDragStart : msg -> Html.Attribute msg
onDragStart message =
    on "dragstart" (JD.succeed message)


onDragEnd : msg -> Html.Attribute msg
onDragEnd message =
    on "dragend" (JD.succeed message)


onDragEnter : msg -> Html.Attribute msg
onDragEnter message =
    on "dragenter" (JD.succeed message)


onDragLeave : msg -> Html.Attribute msg
onDragLeave message =
    on "dragleave" (JD.succeed message)


onDrop : msg -> Html.Attribute msg
onDrop message =
    custom "drop" <|
        JD.map
            (\_ ->
                { message = message
                , stopPropagation = True
                , preventDefault = True
                }
            )
            (JD.succeed message)
