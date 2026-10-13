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


module Components.Out exposing (Out, noOut, out0, out1, out2)

{-| Outcome of a component `update`: local `cmds`, `gcmds` for Global and an optional `result` for the parent.
Components alias it with their own types: `type alias Out = O.Out Msg MyResult`.
-}

import Session exposing (GlobalCmd)


type alias Out msg result =
    { cmds : List (Cmd msg)
    , gcmds : List GlobalCmd
    , result : Maybe result
    }


noOut : Out msg result
noOut =
    Out [] [] Nothing


out0 : List (Cmd msg) -> Out msg result
out0 cmds =
    Out cmds [] Nothing


out1 : List GlobalCmd -> Out msg result
out1 cmds =
    Out [] cmds Nothing


out2 : List (Cmd msg) -> List GlobalCmd -> Out msg result
out2 cmds gcmds =
    Out cmds gcmds Nothing
