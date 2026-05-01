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


module Fractale.User exposing
    ( UserState(..)
    , freshSessionOnOrgaSwitch
    , maybeUctx
    , orgaToUsers
    , orgaToUsersData
    , uctxFromUser
    )

import Dict
import Fractale.Codecs exposing (nearestCircleid)
import List.Extra as LE
import ModelSchema exposing (..)
import Utils.List exposing (toMapOfList)


type UserState
    = LoggedOut
    | LoggedIn UserCtx


uctxFromUser : UserState -> UserCtx
uctxFromUser user =
    case user of
        LoggedIn uctx ->
            uctx

        LoggedOut ->
            initUserctx


maybeUctx : UserState -> Maybe UserCtx
maybeUctx user =
    case user of
        LoggedIn uctx ->
            Just uctx

        LoggedOut ->
            Nothing


freshSessionOnOrgaSwitch : { a | orgChange : Bool, isInit : Bool } -> { b | lexicon : Dict.Dict String String } -> { b | lexicon : Dict.Dict String String }
freshSessionOnOrgaSwitch fs common =
    -- On a real org switch (we knew the previous org and it differs), drop the
    -- lexicon so the page snapshots empty and views fall back to defaults until
    -- GotOrgaInfo lands the new org's lexicon.
    if fs.orgChange && not fs.isInit then
        { common | lexicon = Dict.empty }

    else
        common


{-| @obsolete
-}
orgaToUsersData : NodesDict -> UsersDict
orgaToUsersData nd =
    nd
        |> Dict.toList
        |> List.filterMap (\( k, n ) -> Maybe.map (\fs -> ( nearestCircleid k, { username = fs.username, name = fs.name } )) n.first_link)
        |> toMapOfList


{-| @obsolete
-}
orgaToUsers : NodesDict -> List User
orgaToUsers nd =
    nd
        |> Dict.toList
        |> List.filterMap (\( k, n ) -> n.first_link)
        |> LE.uniqueBy .username
