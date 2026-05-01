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


module Query.QueryUser exposing
    ( IsSubscribe
    , getIsSubscribe
    , isSubscribePayload
    , queryUser
    , queryUserFull
    , queryUserProfile
    , queryUserRoles
    , userFullPayload
    , userProfilePayload
    , usernameFilter
    , usernamesFilter
    )

import Schema.Enum.RoleType as RoleType
import Schema.InputObject as Input
import Schema.Object
import Schema.Object.Node
import Schema.Object.Tension
import Schema.Object.User
import Schema.Object.UserRights
import Schema.Query as Query
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Query.QueryNode exposing (nodeIdPayload, nodeOrgaFilter, searchUserFilter)
import RemoteData
import String.Extra as SE



--
-- Query User
--


usernameFilter : String -> Query.GetUserOptionalArguments -> Query.GetUserOptionalArguments
usernameFilter username a =
    { a | username = Present username }


queryUserProfile url username msg =
    makeGQLQuery url
        (Query.getUser
            (usernameFilter username)
            userProfilePayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


queryUserFull url username msg =
    makeGQLQuery url
        (Query.getUser
            (usernameFilter username)
            userFullPayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


userProfilePayload : SelectionSet UserProfile Schema.Object.User
userProfilePayload =
    SelectionSet.succeed UserProfile
        |> with Schema.Object.User.username
        |> with Schema.Object.User.name
        |> with Schema.Object.User.lang
        |> with
            (Schema.Object.User.rights identity <|
                SelectionSet.map3 UserRights
                    Schema.Object.UserRights.canLogin
                    Schema.Object.UserRights.canCreateRoot
                    Schema.Object.UserRights.type_
            )
        |> with
            (Schema.Object.User.roles identity
                (SelectionSet.map4 UserRole
                    Schema.Object.Node.name
                    Schema.Object.Node.nameid
                    (Schema.Object.Node.role_type |> SelectionSet.map (withDefault RoleType.Peer))
                    Schema.Object.Node.color
                )
                |> SelectionSet.map (withDefault [])
            )
        |> with Schema.Object.User.notifyByEmail
        |> with Schema.Object.User.bio
        |> with Schema.Object.User.location



-- @DEBUG:
-- 1. import withFragment does not work ?!?!
-- 2. cant managed to use **extensible record** with selectionset.


userFullPayload : SelectionSet UserFull Schema.Object.User
userFullPayload =
    SelectionSet.succeed UserFull
        |> with Schema.Object.User.username
        |> with Schema.Object.User.name
        |> with
            (Schema.Object.User.rights identity <|
                SelectionSet.map3 UserRights
                    Schema.Object.UserRights.canLogin
                    Schema.Object.UserRights.canCreateRoot
                    Schema.Object.UserRights.type_
            )
        |> with
            (Schema.Object.User.roles identity
                (SelectionSet.map4 UserRole
                    Schema.Object.Node.name
                    Schema.Object.Node.nameid
                    (Schema.Object.Node.role_type |> SelectionSet.map (withDefault RoleType.Peer))
                    Schema.Object.Node.color
                )
                |> SelectionSet.map (withDefault [])
            )
        |> with Schema.Object.User.notifyByEmail
        |> with Schema.Object.User.lang
        |> with Schema.Object.User.bio
        |> with Schema.Object.User.location
        |> with Schema.Object.User.email



--
-- Query Users
--


usersDecoder : Maybe (List (Maybe user)) -> Maybe (List user)
usersDecoder data =
    data
        |> Maybe.map
            (\d ->
                d
                    |> List.filterMap identity
                    |> Just
            )
        |> withDefault Nothing


queryUser url userfrag msg =
    makeGQLQuery url
        (Query.queryUser
            (userFilter userfrag)
            userPayload
        )
        (RemoteData.fromResult >> decodeResponse usersDecoder >> msg)


userFilter : String -> Query.QueryUserOptionalArguments -> Query.QueryUserOptionalArguments
userFilter userfrag a =
    let
        userreg =
            let
                fragments =
                    String.split " " userfrag
                        |> List.map (\frag -> "^" ++ frag)
                        |> String.join "|"
            in
            "/" ++ fragments ++ "/i"
    in
    { a
        | first = Present 30
        , filter =
            Input.buildUserFilter
                (\b ->
                    { b
                        | username = { regexp = Present userreg, eq = Absent, in_ = Absent } |> Present
                        , or =
                            Present
                                [ Input.buildUserFilter
                                    (\c ->
                                        { c | name = { regexp = Present userreg } |> Present }
                                    )
                                    |> Just
                                ]
                    }
                )
                |> Present
    }


userPayload : SelectionSet User Schema.Object.User
userPayload =
    SelectionSet.succeed User
        |> with Schema.Object.User.username
        |> with Schema.Object.User.name



--
-- Query Users Roles
--


usersRoleDecoder : Maybe (List (Maybe Member)) -> Maybe (List Member)
usersRoleDecoder data =
    data
        |> Maybe.map
            (\d ->
                d
                    |> List.filterMap identity
                    |> List.filter (\x -> x.roles /= [])
                    |> Just
            )
        |> withDefault Nothing


queryUserRoles url nameid usernames pattern msg =
    makeGQLQuery url
        (Query.queryUser
            (case pattern of
                Just _ ->
                    searchUserFilter pattern

                Nothing ->
                    usernamesFilter usernames
            )
            (userRolesPayload nameid)
        )
        (RemoteData.fromResult >> decodeResponse usersRoleDecoder >> msg)


usernamesFilter : List String -> Query.QueryUserOptionalArguments -> Query.QueryUserOptionalArguments
usernamesFilter usernames a =
    { a
        | filter =
            Input.buildUserFilter
                (\c ->
                    { c | username = Present { eq = Absent, regexp = Absent, in_ = List.map Just usernames |> Present } }
                )
                |> Present
    }


userRolesPayload : String -> SelectionSet Member Schema.Object.User
userRolesPayload nameid =
    SelectionSet.succeed Member
        |> with Schema.Object.User.username
        |> with Schema.Object.User.name
        -- Retired is alreaady ignored in queryMembersLocal
        |> with (Schema.Object.User.roles (nodeOrgaFilter nameid [ RoleType.Pending, RoleType.Guest, RoleType.Member ]) userRoleExtendedPayload |> withDefaultSelectionMap [])


userRoleExtendedPayload : SelectionSet UserRoleExtended Schema.Object.Node
userRoleExtendedPayload =
    SelectionSet.succeed UserRoleExtended
        |> with Schema.Object.Node.name
        |> with Schema.Object.Node.nameid
        |> with (Schema.Object.Node.role_type |> withDefaultSelectionMap RoleType.Pending)
        |> with Schema.Object.Node.color
        |> with (Schema.Object.Node.createdAt |> SelectionSet.map decodedTime)
        |> with (Schema.Object.Node.parent identity nodeIdPayload)



--
-- Check if user is subscribe to the given tension
--


isSubscribeDecoder : Maybe IsSubscribe -> Maybe Bool
isSubscribeDecoder data =
    data
        |> Maybe.map
            (\d ->
                d.subscriptions /= Nothing && d.subscriptions /= Just []
            )


getIsSubscribe url username tid msg =
    makeGQLQuery url
        (Query.getUser
            (usernameFilter username)
            (isSubscribePayload tid)
        )
        (RemoteData.fromResult >> decodeResponse isSubscribeDecoder >> msg)


type alias IsSubscribe =
    { subscriptions : Maybe (List IdPayload)

    -- @debug; needs of @isPrivate
    , username : String
    }


isSubscribePayload : String -> SelectionSet IsSubscribe Schema.Object.User
isSubscribePayload tid =
    SelectionSet.map2 IsSubscribe
        (Schema.Object.User.subscriptions (\a -> { a | filter = Present <| Input.buildTensionFilter (\x -> { x | id = Present [ encodeId tid ] }) })
            (SelectionSet.map IdPayload (Schema.Object.Tension.id |> SelectionSet.map decodedId))
        )
        Schema.Object.User.username
