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


module Utils.Emoji exposing (emojis, getEmoji, getEmojiName)

import List.Extra as LE


emojis : List ( Int, String, String )
emojis =
    [ ( 1, String.fromChar '👍', "thumbsup" )
    , ( 2, String.fromChar '👎', "thumbsdown" )
    , ( 3, String.fromChar '🙂', "happy" )
    , ( 4, String.fromChar '🎉', " hooray" )
    , ( 5, String.fromChar '😕', "confused" )
    , ( 6, "❤️", "heart" )
    , ( 7, String.fromChar '🙏', "clap" )

    --, String.fromChar '❓'
    , ( 8, String.fromChar '👀', "eyes" )

    --, String.fromChar '🚜'
    ]


getEmoji : Int -> String
getEmoji type_ =
    case LE.getAt (type_ - 1) emojis of
        Just ( _, b, _ ) ->
            b

        Nothing ->
            ""


getEmojiName : Int -> String
getEmojiName type_ =
    case LE.getAt (type_ - 1) emojis of
        Just ( _, _, c ) ->
            c

        Nothing ->
            ""
