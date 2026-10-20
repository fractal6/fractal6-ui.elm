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


module Pages.Signup exposing (Flags, Model, Msg, page)

import Page exposing (Page)
import Pages.Login as Login



-- /signup is the Login page opened on the sign-up tab.


type alias Flags =
    Login.Flags


type alias Model =
    Login.Model


type alias Msg =
    Login.Msg


page : Page Flags Model Msg
page =
    Login.page
