module ModelCommon.Codecs exposing (..)

import Array exposing (Array)
import Components.Loading exposing (GqlData, RequestResult(..), withMaybeData)
import Dict
import Extra exposing (cleanDup, ternary)
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.NodeVisibility as NodeVisibility
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Generated.Route as Route exposing (Route)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelSchema
    exposing
        ( EmitterOrReceiver
        , FocusNode
        , LocalGraph
        , Node
        , NodeFragment
        , NodeId
        , NodesDict
        , TensionHead
        , User
        , UserCtx
        , UserRole
        )
import Url exposing (Url)



--
-- Routing types
--


type alias Flags_ =
    { param1 : String, param2 : Maybe String, param3 : Maybe String }


type FractalBaseRoute
    = OverviewBaseUri
    | TensionsBaseUri
    | TensionBaseUri
    | MembersBaseUri
    | SettingsBaseUri
    | UsersBaseUri



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


toString : FractalBaseRoute -> String
toString route =
    case route of
        OverviewBaseUri ->
            -- /overview
            "/o"

        TensionsBaseUri ->
            -- /tensions
            "/t"

        TensionBaseUri ->
            -- /tensions
            "/tension"

        MembersBaseUri ->
            -- /tensions
            "/m"

        SettingsBaseUri ->
            -- /settings
            "/s"

        UsersBaseUri ->
            -- /@username
            ""


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
            loc |> toString |> String.dropLeft 1

        base =
            url
                |> Maybe.map (\u -> u.path)
                |> withDefault "/"
                |> String.dropLeft 1
                |> String.split "/"
                |> List.head
                |> withDefault ""
    in
    base /= baseRef



{-
   Uri Codec
-}


uriFromNameid : FractalBaseRoute -> String -> String
uriFromNameid loc nameid =
    [ toString loc ]
        ++ String.split "#" nameid
        |> String.join "/"


uriFromUsername : FractalBaseRoute -> String -> String
uriFromUsername loc username =
    [ toString loc, username ]
        |> String.join "/"


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
            String.join "#" [ rootnameid, targetid ]

        NodeType.Role ->
            if rootnameid == parentid then
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


isCoordo : UserCtx -> String -> Bool
isCoordo uctx nameid =
    uctx.roles
        |> List.filter (\r -> r.role_type == RoleType.Coordinator)
        |> List.map (\r -> nearestCircleid r.nameid)
        |> List.member nameid



{-
   Tension Codec
-}


type alias TensionCharac =
    { action_type : ActionType
    , doc_type : DocType
    }


type ActionType
    = NEW
    | EDIT
    | ARCHIVE


type DocType
    = NODE
    | MD


getTensionCharac : TensionAction.TensionAction -> TensionCharac
getTensionCharac action =
    case action of
        TensionAction.NewRole ->
            { action_type = NEW, doc_type = NODE }

        TensionAction.EditRole ->
            { action_type = EDIT, doc_type = NODE }

        TensionAction.ArchivedRole ->
            { action_type = ARCHIVE, doc_type = NODE }

        TensionAction.NewCircle ->
            { action_type = NEW, doc_type = NODE }

        TensionAction.EditCircle ->
            { action_type = EDIT, doc_type = NODE }

        TensionAction.ArchivedCircle ->
            { action_type = ARCHIVE, doc_type = NODE }

        TensionAction.NewMd ->
            { action_type = NEW, doc_type = MD }

        TensionAction.EditMd ->
            { action_type = EDIT, doc_type = MD }

        TensionAction.ArchivedMd ->
            { action_type = ARCHIVE, doc_type = MD }


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
    { createdAt = ""
    , name = withDefault "" f.name
    , nameid = nodeIdCodec parentid (withDefault "" f.nameid) (withDefault NodeType.Circle f.type_)
    , parent = Nothing
    , type_ = withDefault NodeType.Circle f.type_
    , role_type = f.role_type
    , color = f.color
    , visibility = withDefault NodeVisibility.Private f.visibility
    , mode = withDefault NodeMode.Coordinated f.mode
    , source = Nothing
    , userCanJoin = Nothing
    , first_link = Nothing
    }
