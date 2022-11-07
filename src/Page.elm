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


module Page exposing
    ( Page, Document, Bundle
    , upgrade
    , static, sandbox, element, component
    )

{-|

@docs Page, Document, Bundle

@docs upgrade

@docs static, sandbox, element, component

-}

import Browser
import Global
import Spa


type alias Document msg =
    Browser.Document msg


type alias Page flags model msg =
    Spa.Page flags model msg Global.Model Global.Msg


type alias Bundle msg =
    Spa.Bundle msg


upgrade :
    (pageModel -> model)
    -> (pageMsg -> msg)
    -> Page pageFlags pageModel pageMsg
    ->
        { init : pageFlags -> Global.Model -> ( model, Cmd msg, Cmd Global.Msg )
        , update : pageMsg -> pageModel -> Global.Model -> ( model, Cmd msg, Cmd Global.Msg )
        , bundle : pageModel -> Global.Model -> Bundle msg
        }
upgrade =
    Spa.upgrade


static : { view : Document msg } -> Page flags () msg
static =
    Spa.static


sandbox :
    { init : model
    , update : msg -> model -> model
    , view : model -> Document msg
    }
    -> Page flags model msg
sandbox =
    Spa.sandbox


element :
    { init : flags -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Document msg
    }
    -> Page flags model msg
element =
    Spa.element


component :
    { init : Global.Model -> flags -> ( model, Cmd msg, Cmd Global.Msg )
    , update : Global.Model -> msg -> model -> ( model, Cmd msg, Cmd Global.Msg )
    , subscriptions : Global.Model -> model -> Sub msg
    , view : Global.Model -> model -> Document msg
    }
    -> Page flags model msg
component =
    Spa.component
