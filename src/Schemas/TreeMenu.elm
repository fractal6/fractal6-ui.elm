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


module Schemas.TreeMenu exposing (ExpandedLines, PersistentModel, decode, encode, toPersistant)

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Json.Encode.Extra as JEE
import Loading exposing (ModalData)
import Maybe exposing (withDefault)


type alias ExpandedLines =
    Dict String Bool


type alias PersistentModel =
    { isActive : Bool
    , hover : Maybe String
    , expanded_lines : ExpandedLines
    }


toPersistant : { a | isActive : Bool, hover : Maybe String, expanded_lines : ExpandedLines } -> PersistentModel
toPersistant model =
    { isActive = model.isActive
    , hover = model.hover
    , expanded_lines = model.expanded_lines
    }


encode : PersistentModel -> JE.Value
encode m =
    JE.object
        [ ( "isActive", JE.bool m.isActive )
        , ( "hover", JEE.maybe JE.string m.hover )
        , ( "expanded_lines", JE.dict identity JE.bool m.expanded_lines )
        ]


decode : JD.Decoder (Maybe PersistentModel)
decode =
    JD.maybe <|
        JD.map3 PersistentModel
            (JD.field "isActive" JD.bool)
            (JD.maybe <| JD.field "hover" JD.string)
            (JD.field "expanded_lines" (JD.dict JD.bool))
