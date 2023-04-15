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


module Bulk.Codecs exposing (..)

import Array
import Extra exposing (cleanDup, ternary)
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.NodeVisibility as NodeVisibility
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Generated.Route as Route exposing (Route(..), fromUrl, toHref)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelSchema exposing (EmitterOrReceiver, LocalGraph, Node, NodeFragment, UserCommon, UserCtx, UserRole, UserRoleCommon)
import Url exposing (Url)



--
-- Routing types
--


type alias Flags_ =
    { param1 : String, param2 : Maybe String, param3 : Maybe String }


type FractalBaseRoute
    = OverviewBaseUri
    | TensionsBaseUri
    | ProjectsBaseUri
    | MembersBaseUri
    | SettingsBaseUri
    | UsersBaseUri
    | TensionBaseUri
    | MandateBaseUri
    | ContractsBaseUri
    | ProjectBaseUri


isTensionBaseUri : FractalBaseRoute -> Bool
isTensionBaseUri r =
    List.member r [ TensionBaseUri, MandateBaseUri, ContractsBaseUri ]


isProjectBaseUri : FractalBaseRoute -> Bool
isProjectBaseUri r =
    List.member r [ ProjectBaseUri ]



--
-- Focus
--


type alias NodeFocus =
    { rootnameid : String
    , nameid : String
    , type_ : NodeType.NodeType -- @obsololete: known from nameid
    }


type alias FocusState =
    { isInit : Bool
    , orgChange : Bool
    , focusChange : Bool
    , paramsChange : Bool
    , menuChange : Bool
    , refresh : Bool
    }


toLink : FractalBaseRoute -> String -> List String -> String
toLink route param params =
    case route of
        -- User  url
        UsersBaseUri ->
            if
                -- Those name are reserved in the backend though.
                List.member param
                    [ "explore"
                    , "login"
                    , "logout"
                    , "notifications"
                    , "signup"
                    , "verification"
                    , "new"
                    , "tension"
                    , "tensions"
                    , "user"
                    , "users"
                    , "org"
                    ]
            then
                "/user/" ++ param

            else
                "/" ++ param

        -- Org Url
        OverviewBaseUri ->
            [ "/o" ] ++ String.split "#" param |> String.join "/"

        TensionsBaseUri ->
            [ "/t" ] ++ String.split "#" param |> String.join "/"

        ProjectsBaseUri ->
            [ "/p" ] ++ String.split "#" param |> String.join "/"

        MembersBaseUri ->
            [ "/m" ] ++ String.split "#" param |> String.join "/"

        SettingsBaseUri ->
            [ "/s" ] ++ String.split "#" param |> String.join "/"

        TensionBaseUri ->
            toHref (Route.Tension_Dynamic_Dynamic { param1 = nid2rootid param, param2 = LE.getAt 0 params |> withDefault "" })

        MandateBaseUri ->
            toHref (Route.Tension_Dynamic_Dynamic_Action { param1 = nid2rootid param, param2 = LE.getAt 0 params |> withDefault "" })

        ContractsBaseUri ->
            toHref (Route.Tension_Dynamic_Dynamic_Contract { param1 = nid2rootid param, param2 = LE.getAt 0 params |> withDefault "" })

        ProjectBaseUri ->
            toHref (Route.Tension_Dynamic_Dynamic { param1 = nid2rootid param, param2 = LE.getAt 0 params |> withDefault "" })


{-| Just use to check if you are in organisation tab...
-}
urlToFractalRoute : Url -> Maybe FractalBaseRoute
urlToFractalRoute url =
    fromUrl url
        |> Maybe.map
            (\u ->
                case u of
                    O_Dynamic _ ->
                        Just OverviewBaseUri

                    O_Dynamic_Dynamic _ ->
                        Just OverviewBaseUri

                    O_Dynamic_Dynamic_Dynamic _ ->
                        Just OverviewBaseUri

                    T_Dynamic a ->
                        Just TensionsBaseUri

                    T_Dynamic_Dynamic a ->
                        Just TensionsBaseUri

                    T_Dynamic_Dynamic_Dynamic a ->
                        Just TensionsBaseUri

                    P_Dynamic a ->
                        Just ProjectsBaseUri

                    P_Dynamic_Dynamic a ->
                        Just ProjectsBaseUri

                    P_Dynamic_Dynamic_Dynamic a ->
                        Just ProjectsBaseUri

                    M_Dynamic _ ->
                        Just MembersBaseUri

                    M_Dynamic_Dynamic _ ->
                        Just MembersBaseUri

                    M_Dynamic_Dynamic_Dynamic _ ->
                        Just MembersBaseUri

                    S_Dynamic _ ->
                        Just SettingsBaseUri

                    S_Dynamic_Dynamic _ ->
                        Just SettingsBaseUri

                    S_Dynamic_Dynamic_Dynamic _ ->
                        Just SettingsBaseUri

                    Tension_Dynamic_Dynamic _ ->
                        Just TensionBaseUri

                    Tension_Dynamic_Dynamic_Action _ ->
                        Just MandateBaseUri

                    Tension_Dynamic_Dynamic_Contract _ ->
                        Just ContractsBaseUri

                    Tension_Dynamic_Dynamic_Contract_Dynamic _ ->
                        Just ContractsBaseUri

                    Dynamic _ ->
                        Just UsersBaseUri

                    Dynamic_Settings _ ->
                        Just UsersBaseUri

                    _ ->
                        Nothing
            )
        |> withDefault Nothing


isOrgUrl : Url -> Bool
isOrgUrl url =
    urlToFractalRoute url
        |> Maybe.map
            (\u ->
                List.member u [ OverviewBaseUri, TensionsBaseUri, ProjectsBaseUri, MembersBaseUri, SettingsBaseUri ]
                    || isTensionBaseUri u
                    || isProjectBaseUri u
            )
        |> withDefault False


focusState : FractalBaseRoute -> Maybe Url -> Url -> Maybe NodeFocus -> NodeFocus -> FocusState
focusState baseUri referer url maybeFocus newFocus =
    let
        oldFocus =
            maybeFocus |> withDefault newFocus

        isInit =
            maybeFocus == Nothing

        orgChange =
            newFocus.rootnameid /= oldFocus.rootnameid || isInit

        focusChange =
            newFocus.nameid /= oldFocus.nameid || isInit

        paramsChange =
            (referer |> Maybe.map (\r -> r.query /= url.query) |> withDefault False) || orgChange

        menuChange =
            basePathChanged baseUri referer || orgChange
    in
    { isInit = isInit
    , orgChange = orgChange
    , focusChange = focusChange
    , paramsChange = paramsChange
    , menuChange = menuChange
    , refresh = focusChange || menuChange
    }


basePathChanged : FractalBaseRoute -> Maybe Url -> Bool
basePathChanged loc url =
    let
        baseRef =
            toLink loc "" [] |> String.dropLeft 1 |> String.split "/" |> List.head |> withDefault ""

        base =
            url |> Maybe.map (\u -> u.path) |> withDefault "/" |> String.dropLeft 1 |> String.split "/" |> List.head |> withDefault ""
    in
    base /= baseRef



{-
   Uri Codec
-}


uriFromNameid : FractalBaseRoute -> String -> List String -> String
uriFromNameid loc nameid params =
    toLink loc nameid params


uriFromUsername : FractalBaseRoute -> String -> String
uriFromUsername loc username =
    toLink loc username []


nameidFromFlags : Flags_ -> String
nameidFromFlags flags =
    let
        rootnameid =
            flags.param1

        focusFragment =
            String.join "#"
                [ flags.param2 |> withDefault ""
                , flags.param3 |> withDefault ""
                ]
    in
    String.join "#" [ rootnameid, focusFragment ]
        |> Url.percentDecode
        |> withDefault ""



{-
   Node Codec
-}


focusFromPath : LocalGraph -> NodeFocus
focusFromPath path =
    { rootnameid = nid2rootid path.focus.nameid
    , nameid = path.focus.nameid
    , type_ = path.focus.type_
    }


focusFromNameid : String -> NodeFocus
focusFromNameid nameid_ =
    let
        path =
            String.split "#" nameid_ |> Array.fromList

        -- get key nav path node
        rootid =
            Array.get 0 path |> withDefault ""

        lastNode =
            Array.get 1 path |> withDefault ""

        role =
            Array.get 2 path |> withDefault ""

        nodeType =
            ternary (role == "") NodeType.Circle NodeType.Role

        isRoot =
            lastNode == "" && role == ""

        -- build the node name ID
        nameid =
            if isRoot then
                rootid

            else if nodeType == NodeType.Circle then
                String.join "#" [ rootid, lastNode ]

            else
                String.join "#" [ rootid, lastNode, role ]
    in
    NodeFocus rootid nameid nodeType


nid2type : String -> NodeType.NodeType
nid2type nameid =
    let
        path =
            String.split "#" nameid
    in
    if List.length path == 3 then
        NodeType.Role

    else
        NodeType.Circle


isRole : String -> Bool
isRole nid =
    nid2type nid == NodeType.Role


isCircle : String -> Bool
isCircle nid =
    nid2type nid == NodeType.Circle


isBaseMember : String -> Bool
isBaseMember nameid =
    case String.split "#" nameid of
        [ a, b, c ] ->
            if String.left 1 c == "@" then
                True

            else
                False

        _ ->
            False


userFromBaseMember : String -> Maybe String
userFromBaseMember nameid =
    case String.split "#" nameid of
        [ a, b, c ] ->
            String.dropLeft 1 c |> Just

        _ ->
            Nothing


nid2eor : String -> EmitterOrReceiver
nid2eor nid =
    let
        name =
            nid
                |> String.split "#"
                |> (\l ->
                        if List.length l == 1 then
                            Just [ nid ]

                        else
                            List.tail l
                   )
                |> withDefault [ nid ]
                |> String.join "/"
    in
    { nameid = nid
    , name = name
    , role_type = Nothing
    , color = Nothing
    }


eor2ur er =
    --eor2ur : NodeCommon a -> UserRole
    { name = er.name
    , nameid = er.nameid
    , role_type = withDefault RoleType.Member er.role_type
    , color = er.color
    }


ur2eor : UserRoleCommon a -> EmitterOrReceiver
ur2eor ur =
    { name = ur.name
    , nameid = ur.nameid
    , role_type = Just ur.role_type
    , color = ur.color
    }


memberIdCodec : String -> String -> String
memberIdCodec rootnameid username =
    -- Returns the namid of a new Role given an username and a rootnameid
    String.join "#" [ rootnameid, "", "@" ++ username ]


memberIdDecodec : String -> String
memberIdDecodec nameid =
    -- Returns the username of member given the Member node nameid
    String.split "@" nameid |> LE.last |> withDefault ""


nodeIdCodec : String -> String -> NodeType.NodeType -> String
nodeIdCodec parentid targetid type_ =
    -- Returns the nameid of a new Circle/Role given the parentid and the nameid fragment.
    let
        rootnameid =
            nid2rootid parentid
    in
    case type_ of
        NodeType.Circle ->
            if targetid == "" then
                -- root node has empty nameid in its blob source !
                rootnameid

            else
                String.join "#" [ rootnameid, targetid ]

        NodeType.Role ->
            if isBaseMember targetid then
                targetid

            else if rootnameid == parentid then
                String.join "#" [ rootnameid, "", targetid ]

            else
                String.join "#" [ parentid, targetid ]


nameidEncoder : String -> String
nameidEncoder nameid =
    nameid
        |> String.trim
        |> String.toLower
        |> String.trim
        |> String.map
            (\c ->
                if List.member c [ ' ', '/', '=', '?', '#', '&', '?', '|', '%', '$', '\\' ] then
                    '-'

                else if List.member c [ '@', '(', ')', '<', '>', '[', ']', '{', '}', '"', '`', '\'' ] then
                    '_'

                else
                    c
            )
        |> cleanDup "-"
        |> cleanDup "_"


contractIdCodec : String -> String -> String -> String -> String
contractIdCodec tid event_type old new =
    String.join "#" [ tid, event_type, old, new ]


voteIdCodec : String -> String -> String -> String
voteIdCodec contractid rootnameid username =
    String.join "#" [ contractid, memberIdCodec rootnameid username ]


nid2rootid : String -> String
nid2rootid nameid =
    nameid |> String.split "#" |> List.head |> withDefault ""


nearestCircleid : String -> String
nearestCircleid nameid =
    let
        path =
            String.split "#" nameid |> Array.fromList
    in
    case Array.length path of
        3 ->
            -- Role
            Array.slice 0 2 path |> Array.toList |> List.filter (\x -> x /= "") |> String.join "#"

        _ ->
            nameid


getRootids : List UserRole -> List String
getRootids roles =
    -- Return all rootnameid a user belongs to
    roles
        |> List.filter (\r -> r.role_type /= RoleType.Retired)
        |> List.filter (\r -> r.role_type /= RoleType.Pending)
        |> List.map (\r -> nid2rootid r.nameid)
        |> LE.unique


getOrgaRoles : List String -> List UserRole -> List UserRole
getOrgaRoles nameids roles =
    -- Return all roles of an user inside organisations given the nameids in those organisations
    let
        rootnameids =
            List.map (\nid -> nid2rootid nid) nameids
    in
    roles
        |> List.filter (\r -> r.role_type /= RoleType.Retired)
        |> List.filter (\r -> r.role_type /= RoleType.Pending)
        |> List.filter (\r -> List.member (nid2rootid r.nameid) rootnameids)


getRoles : UserCommon a -> List UserRole
getRoles user =
    user.roles
        |> List.filter (\r -> r.role_type /= RoleType.Retired)
        |> List.filter (\r -> r.role_type /= RoleType.Pending)


getCircleRoles : List String -> List UserRole -> List UserRole
getCircleRoles nameids roles =
    -- Return all roles of an user inside circles given the nameids of thoses circles (or the nearest circles if a role is given).
    let
        circleids =
            List.map (\nid -> nearestCircleid nid) nameids
    in
    List.filter (\r -> List.member (nearestCircleid r.nameid) circleids) roles


getCoordoRoles : List UserRole -> List UserRole
getCoordoRoles roles =
    List.filter (\r -> r.role_type == RoleType.Coordinator) roles


isOwner : UserCtx -> String -> Bool
isOwner uctx nameid =
    uctx.roles
        |> List.filter (\r -> nid2rootid r.nameid == nid2rootid nameid)
        |> List.any (\r -> r.role_type == RoleType.Owner)


isPending : UserCtx -> String -> Bool
isPending uctx nameid =
    uctx.roles
        |> List.filter (\r -> nid2rootid r.nameid == nid2rootid nameid)
        |> List.any (\r -> r.role_type == RoleType.Pending)


isMember : UserCtx -> String -> Bool
isMember uctx nameid =
    (List.length <|
        getOrgaRoles [ nameid ] uctx.roles
    )
        > 0


hasRole : UserCtx -> String -> Bool
hasRole uctx nameid =
    uctx.roles
        |> List.filter (\r -> r.role_type /= RoleType.Guest)
        |> List.map (\r -> nearestCircleid r.nameid)
        |> List.member nameid


playsRole : UserCtx -> String -> Bool
playsRole uctx nameid =
    uctx.roles
        |> List.map .nameid
        |> List.member nameid



{-
   Tension Codec
-}


type alias TensionCharac =
    { action_type : ActionType
    , doc_type : DocType
    , action : TensionAction.TensionAction
    }


type ActionType
    = NEW
    | EDIT
    | ARCHIVE


type DocType
    = NODE NodeType.NodeType
    | MD


getTensionCharac : TensionAction.TensionAction -> TensionCharac
getTensionCharac action =
    case action of
        TensionAction.NewRole ->
            { action = action, action_type = NEW, doc_type = NODE NodeType.Role }

        TensionAction.EditRole ->
            { action = action, action_type = EDIT, doc_type = NODE NodeType.Role }

        TensionAction.ArchivedRole ->
            { action = action, action_type = ARCHIVE, doc_type = NODE NodeType.Role }

        TensionAction.NewCircle ->
            { action = action, action_type = NEW, doc_type = NODE NodeType.Circle }

        TensionAction.EditCircle ->
            { action = action, action_type = EDIT, doc_type = NODE NodeType.Circle }

        TensionAction.ArchivedCircle ->
            { action = action, action_type = ARCHIVE, doc_type = NODE NodeType.Circle }

        TensionAction.NewMd ->
            { action = action, action_type = NEW, doc_type = MD }

        TensionAction.EditMd ->
            { action = action, action_type = EDIT, doc_type = MD }

        TensionAction.ArchivedMd ->
            { action = action, action_type = ARCHIVE, doc_type = MD }


type alias Node_ a =
    { a
        | type_ : NodeType.NodeType
    }


tensionCharacFromNode : Node_ a -> TensionCharac
tensionCharacFromNode node =
    -- @DEBUG/@FIX: archive circle can be query now...
    -- Action type should be queried with queryNodesSub !
    -- @TODO: special color/shape for archive circle.
    case node.type_ of
        NodeType.Circle ->
            { action = TensionAction.EditCircle, action_type = EDIT, doc_type = NODE NodeType.Circle }

        NodeType.Role ->
            { action = TensionAction.EditRole, action_type = EDIT, doc_type = NODE NodeType.Role }


tensionAction2NodeType : Maybe TensionAction.TensionAction -> Maybe NodeType.NodeType
tensionAction2NodeType action_m =
    action_m
        |> Maybe.map
            (\action ->
                case action of
                    TensionAction.NewRole ->
                        Just NodeType.Role

                    TensionAction.EditRole ->
                        Just NodeType.Role

                    TensionAction.ArchivedRole ->
                        Just NodeType.Role

                    TensionAction.NewCircle ->
                        Just NodeType.Circle

                    TensionAction.EditCircle ->
                        Just NodeType.Circle

                    TensionAction.ArchivedCircle ->
                        Just NodeType.Circle

                    _ ->
                        Nothing
            )
        |> withDefault Nothing


nodeFromFragment : String -> NodeFragment -> Node
nodeFromFragment parentid f =
    { name = withDefault "" f.name
    , nameid = nodeIdCodec parentid (withDefault "" f.nameid) (withDefault NodeType.Circle f.type_)
    , parent = Nothing
    , type_ = withDefault NodeType.Circle f.type_
    , role_type = f.role_type
    , color = f.color
    , visibility = withDefault NodeVisibility.Private f.visibility
    , mode = withDefault NodeMode.Coordinated f.mode
    , source = Nothing
    , userCanJoin = Nothing
    , first_link = Maybe.map (\fs -> { username = fs, name = Nothing }) f.first_link
    , n_open_tensions = 0
    , n_open_contracts = 0
    }
