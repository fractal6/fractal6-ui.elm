{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2023 Fractale Co

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


module Auth exposing
    ( ErrState(..)
    , getNodeRights
    , getProjectRights
    , getTensionRights
    , hasAdminRole
    , hasLazyAdminRole
    , parseErr
    , parseErr2
    )

import Bulk exposing (getChildren)
import Bulk.Codecs exposing (getCircleRoles, getCoordoRoles, getOrgaRoles, isOwner, nearestCircleid, nid2rootid)
import Dict
import Extra exposing (ternary, textH, upH)
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Json.Decode as JD
import Loading exposing (GqlData, RequestResult(..), RestData, errorsDecoder, toErrorData, withMaybeData)
import Maybe exposing (withDefault)
import ModelSchema exposing (LocalGraph, Node, NodesDict, ProjectAuth, TensionAuth, TensionHead, UserCtx, UserRole, initNode)
import RemoteData
import String exposing (contains, startsWith)
import String.Extra as SE
import Text as T



--
-- Model
--


type ErrState a
    = Authenticate
    | RefreshToken Int
    | OkAuth a
    | NoAuth
    | DuplicateErr
    | NameTooLong
    | UnknownErr
    | NoErr



--
-- Logics
--


{-| Convert an error message to an error type
-}
messageToErrState : String -> Int -> ErrState a
messageToErrState message_ trial =
    let
        message =
            String.toLower message_
    in
    if
        contains "token is expired" message
            || contains "no token found" message
    then
        Authenticate

    else if
        startsWith "duplicate error" message
            || contains "already exists for field" message
    then
        DuplicateErr

    else if startsWith "name too long" message then
        NameTooLong

    else if
        startsWith "access denied" message
            || contains "refresh token" message
            || contains "authorization failed" message
            || contains "mutation failed because authorization failed" message
    then
        if trial == 0 then
            RefreshToken (trial + 1)

        else
            NoAuth

    else
        UnknownErr


{-| For GQL Response
-}
parseErr : GqlData a -> Int -> ErrState a
parseErr data trial =
    case data of
        Success d ->
            OkAuth d

        Failure err ->
            case List.head err of
                Just err_ ->
                    let
                        gqlErr =
                            err_
                                |> String.replace "\n" ""
                                |> SE.rightOf "{"
                                |> SE.insertAt "{" 0
                                |> JD.decodeString errorsDecoder
                    in
                    case gqlErr of
                        Ok errGql ->
                            case List.head errGql.errors of
                                Just e ->
                                    messageToErrState e.message trial

                                Nothing ->
                                    UnknownErr

                        Err errJD ->
                            messageToErrState err_ trial

                Nothing ->
                    UnknownErr

        _ ->
            NoErr


{-| For HTTP response
-}
parseErr2 : RestData a -> Int -> ErrState a
parseErr2 data trial =
    case data of
        RemoteData.Success d ->
            OkAuth d

        RemoteData.Failure error ->
            case List.head (toErrorData error) of
                Just err_ ->
                    let
                        gqlErr =
                            err_
                                |> String.replace "\n" ""
                                |> SE.rightOf "{"
                                |> SE.insertAt "{" 0
                                |> JD.decodeString errorsDecoder
                    in
                    case gqlErr of
                        Ok errGql ->
                            case List.head errGql.errors of
                                Just e ->
                                    messageToErrState e.message trial

                                Nothing ->
                                    UnknownErr

                        Err errJD ->
                            messageToErrState err_ trial

                Nothing ->
                    UnknownErr

        _ ->
            NoErr



{-
   Auth
-}


{-| Try best to known if user has admin role somewhere in the organisation
with limited informations.
@debug: assignees, etc...
-}
hasLazyAdminRole : UserCtx -> Maybe NodeMode.NodeMode -> String -> Bool
hasLazyAdminRole uctx mode_m nameid =
    let
        filter_role =
            case mode_m of
                Just NodeMode.Agile ->
                    \r -> List.member r.role_type [ RoleType.Peer, RoleType.Coordinator, RoleType.Owner ]

                Just NodeMode.Coordinated ->
                    \r -> List.member r.role_type [ RoleType.Coordinator, RoleType.Owner ]

                Nothing ->
                    \r -> List.member r.role_type [ RoleType.Coordinator, RoleType.Owner ]
    in
    uctx.roles
        |> List.filter filter_role
        |> List.map (\r -> nid2rootid r.nameid)
        |> List.member (nid2rootid nameid)


{-| Like lazy but only look on the given circle
-}
hasAdminRole : UserCtx -> Maybe LocalGraph -> Bool
hasAdminRole uctx path_m =
    case path_m of
        Just p ->
            let
                orga_roles =
                    getOrgaRoles [ nid2rootid p.focus.nameid ] uctx.roles

                filter_role =
                    case p.focus.mode of
                        NodeMode.Agile ->
                            \r -> List.member r.role_type [ RoleType.Peer, RoleType.Coordinator ]

                        NodeMode.Coordinated ->
                            \r -> List.member r.role_type [ RoleType.Coordinator, RoleType.Owner ]
            in
            if List.member p.focus.nameid (List.map .nameid orga_roles) then
                -- User is admin of its own roles (ie to add thing **inside** projects or files etc)
                -- Not to modify it directly (as for getNodeRights...)
                True

            else if isOwner uctx p.focus.nameid then
                True

            else
                -- User is admin of a circle/role.
                orga_roles
                    |> List.filter filter_role
                    |> List.map (\r -> nearestCircleid r.nameid)
                    |> List.member p.focus.nameid

        Nothing ->
            False


{-| Returns Admin roles, which covers the

  - Admin User of the orga
  - User with coordo role below that node
  - User with the first coordo role on parent, if no coordo below.

-}
getNodeRights : UserCtx -> Node -> GqlData NodesDict -> List UserRole
getNodeRights uctx target_ odata =
    let
        -- The authority to edit a Node is determined by the tension receiver Node,
        -- which is the direct parent of the givent Node.
        target =
            withMaybeData odata
                |> Maybe.map
                    (\data ->
                        Dict.get (target_.parent |> Maybe.map .nameid |> withDefault target_.nameid) data
                    )
                |> withDefault Nothing
                |> withDefault initNode

        orgaRoles =
            getOrgaRoles [ target.nameid ] uctx.roles
    in
    if List.length orgaRoles == 0 then
        []

    else if isOwner uctx target.nameid then
        List.filter (\r -> r.role_type == RoleType.Owner) orgaRoles

    else
        let
            childrenRoles =
                getChildren target.nameid odata
                    |> List.filter (\n -> n.type_ == NodeType.Role)

            childrenCoordos =
                List.filter (\n -> n.role_type == Just RoleType.Coordinator) childrenRoles

            circleRoles =
                getCircleRoles [ target.nameid ] orgaRoles

            allCoordoRoles =
                getCoordoRoles orgaRoles

            coordoRoles =
                getCoordoRoles circleRoles
        in
        case target.mode of
            NodeMode.Agile ->
                case circleRoles of
                    [] ->
                        -- No member in this circle
                        orgaRoles

                    circleRoles_ ->
                        circleRoles_

            NodeMode.Coordinated ->
                case coordoRoles of
                    [] ->
                        -- No coordo in this circle
                        if List.length childrenCoordos == 0 && List.length allCoordoRoles > 0 then
                            allCoordoRoles

                        else
                            []

                    coordoRoles_ ->
                        coordoRoles_


{-| Return True if user has tension edition rights, which covers the:

  - Orga Admin of the tension receiver node.
  - Coordo of the tension receiver node.
  - Assignee of the tension.

-}
getTensionRights : UserCtx -> GqlData (TensionAuth a) -> GqlData LocalGraph -> Bool
getTensionRights uctx th_d path_d =
    case th_d of
        Success th ->
            case path_d of
                Success p ->
                    let
                        orgaRoles =
                            getOrgaRoles [ p.focus.nameid ] uctx.roles

                        childrenRoles =
                            p.focus.children |> List.filter (\n -> n.role_type /= Nothing)

                        childrenCoordos =
                            List.filter (\n -> n.role_type == Just RoleType.Coordinator) childrenRoles

                        circleRoles =
                            --getCircleRoles [ th.receiver.nameid, th.emitter.nameid ] orgaRoles
                            getCircleRoles [ th.receiver.nameid ] orgaRoles

                        coordoRoles =
                            getCoordoRoles circleRoles
                    in
                    if List.member uctx.username (withDefault [] th.assignees |> List.map .username) then
                        -- assignee
                        True
                        --else if uctx.username == th.createdBy.username then
                        --    -- Author
                        --    True

                    else if isOwner uctx p.focus.nameid then
                        -- is Owner
                        True

                    else
                        -- has role base autorization
                        case p.focus.mode of
                            NodeMode.Agile ->
                                -- Is a  Circle member
                                (List.length circleRoles > 0)
                                    || -- Or No member in this circle
                                       (List.length orgaRoles > 0)

                            NodeMode.Coordinated ->
                                -- Is a circle coordo
                                (List.length coordoRoles > 0)
                                    || -- Or No coordo in this circle
                                       (List.length childrenCoordos == 0 && List.length (getCoordoRoles orgaRoles) > 0)

                _ ->
                    False

        _ ->
            False


{-| Return True if user has project edition rights, which covers the:

  - Orga owner.
  - Coordo of the circles where the projects is linked.
  - @TODO the projects leaders (members)

-}
getProjectRights : UserCtx -> ProjectAuth a -> GqlData LocalGraph -> Bool
getProjectRights uctx pauth path_d =
    case path_d of
        Success p ->
            let
                orgaRoles =
                    getOrgaRoles [ p.focus.nameid ] uctx.roles

                childrenRoles =
                    p.focus.children |> List.filter (\n -> n.role_type /= Nothing)

                childrenCoordos =
                    List.filter (\n -> n.role_type == Just RoleType.Coordinator) childrenRoles

                circleRoles =
                    getCircleRoles (List.map .nameid pauth.nodes) orgaRoles

                coordoRoles =
                    getCoordoRoles circleRoles
            in
            if List.member uctx.username (pauth.leaders |> List.map .username) then
                -- Leaders
                True

            else if isOwner uctx p.focus.nameid then
                -- is Owner
                True

            else
                -- has role base autorization
                case p.focus.mode of
                    NodeMode.Agile ->
                        -- Is a  Circle member
                        (List.length circleRoles > 0)
                            || -- Or No member in this circle
                               (List.length orgaRoles > 0)

                    NodeMode.Coordinated ->
                        -- Is a circle coordo
                        (List.length coordoRoles > 0)
                            || -- Or No coordo in this circle
                               (List.length childrenCoordos == 0 && List.length (getCoordoRoles orgaRoles) > 0)

        _ ->
            False
