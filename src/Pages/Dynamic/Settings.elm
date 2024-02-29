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


module Pages.Dynamic.Settings exposing (Flags, Model, Msg, page)

import Global
import Html
import Page exposing (Document, Page)
import User.Settings as Settings


type alias Flags =
    { param1 : String
    }


type alias Model =
    Settings.Model


type alias Msg =
    Settings.Msg


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = Settings.update
        , subscriptions = Settings.subscriptions
        , view = Settings.view
        }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    Settings.init global { param1 = flags.param1 }
