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


module Utils.String exposing
    ( cleanDup
    , decap
    , regexContains
    , regexFromString
    , regexfirstMatchLength
    , space_
    , upA
    , upH
    , upT
    )

import Regex exposing (Regex)
import String
import String.Extra as SE


upH : String -> String
upH s =
    SE.toSentenceCase s


upT : String -> String
upT s =
    SE.toTitleCase s


upA : String -> String
upA t =
    String.toUpper t


decap : String -> String
decap t =
    SE.decapitalize t


space_ : String
space_ =
    "\u{00A0}"


regexFromString : String -> Regex
regexFromString =
    Regex.fromString >> Maybe.withDefault Regex.never


regexContains : String -> String -> Bool
regexContains pattern text =
    Regex.contains (regexFromString pattern) text


regexfirstMatchLength : String -> String -> Maybe Int
regexfirstMatchLength pattern text =
    case Regex.findAtMost 1 (regexFromString pattern) text of
        [] ->
            Nothing

        firstMatch :: _ ->
            Just (String.length firstMatch.match)


cleanDup : String -> String -> String
cleanDup c s =
    -- Remove any repetition of the character c in string s
    s |> Regex.replace (regexFromString (c ++ c ++ "+")) (always c)
