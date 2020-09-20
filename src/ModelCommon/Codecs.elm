module ModelCommon.Codecs exposing (..)

import Array exposing (Array)
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Generated.Route as Route exposing (Route)
import Maybe exposing (withDefault)
import ModelSchema exposing (Node, NodeCharac, UserCtx, UserRole)
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
    | UsersBaseUri



--
-- Focus
--


type alias NodeFocus =
    { rootnameid : String
    , isRoot : Bool
    , nameid : String
    , type_ : NodeType.NodeType

    --, name : Maybe String // get the name when JS/D3 finished the rendering Job
    }


type alias FocusState =
    { isInit : Bool
    , orgChange : Bool
    , focusChange : Bool
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

        UsersBaseUri ->
            -- /@username
            ""


focusState : FractalBaseRoute -> Url -> Maybe NodeFocus -> NodeFocus -> FocusState
focusState baseUri referer maybeFocus newFocus =
    let
        oldFocus =
            maybeFocus |> withDefault newFocus

        isInit =
            maybeFocus == Nothing
    in
    { isInit = isInit
    , orgChange = (newFocus.rootnameid /= oldFocus.rootnameid) || isInit
    , focusChange = (newFocus.nameid /= oldFocus.nameid) || isInit
    , refresh = basePathChanged baseUri referer || isInit
    }


basePathChanged : FractalBaseRoute -> Url -> Bool
basePathChanged loc url =
    let
        baseRef =
            toString loc |> String.dropLeft 1

        base =
            url.path
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
                |> Url.percentDecode
                |> withDefault ""

        focusFragment =
            String.join "#"
                [ flags.param2
                    |> Maybe.map (\p -> p |> Url.percentDecode |> withDefault "")
                    |> withDefault ""
                , flags.param3
                    |> Maybe.map (\p -> p |> Url.percentDecode |> withDefault "")
                    |> withDefault ""
                ]
    in
    String.join "#" [ rootnameid, focusFragment ]



{-
   Node Codec
-}


nodeFromFocus : NodeFocus -> Node
nodeFromFocus focus =
    { id = ""
    , createdAt = ""
    , name = ""
    , nameid = focus.nameid
    , rootnameid = focus.rootnameid
    , parent = Nothing
    , type_ = focus.type_
    , role_type = Nothing
    , first_link = Nothing
    , charac = NodeCharac False NodeMode.Coordinated
    , isPrivate = False
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

        -- extra attribute
        isRoot =
            lastNode == "" && role == ""

        nodeType =
            if role == "" then
                NodeType.Circle

            else
                NodeType.Role

        -- build the node name ID
        nameid =
            if isRoot then
                rootid

            else if nodeType == NodeType.Circle then
                String.join "#" [ rootid, lastNode ]

            else
                String.join "#" [ rootid, lastNode, role ]
    in
    NodeFocus rootid isRoot nameid nodeType


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


nid2rootid : String -> String
nid2rootid nameid =
    nameid |> String.split "#" |> List.head |> withDefault ""


getOrgaRoles : List UserRole -> List String -> List UserRole
getOrgaRoles roles nameids =
    -- Return all roles of an user inside organisations given the nameids in those organisations
    let
        rootnameids =
            List.map (\nid -> nid2rootid nid) nameids
    in
    List.filter (\r -> List.member r.rootnameid rootnameids) roles


getCircleRoles : List UserRole -> List String -> List UserRole
getCircleRoles roles nameids =
    -- Return all roles of an user inside circles given the nameids of thoses circles (or the nearest circles if a role is given).
    let
        circleids =
            List.map (\nid -> nearestCircleid nid) nameids
    in
    List.filter (\r -> List.member (nearestCircleid r.nameid) circleids) roles


getCoordoRoles : List UserRole -> List UserRole
getCoordoRoles roles =
    List.filter (\r -> r.role_type == RoleType.Coordinator) roles


guestIdCodec : String -> String -> String
guestIdCodec rootnameid username =
    -- Returns the namid of a new Role given an username and a rootnameid
    String.join "#" [ rootnameid, "", "@" ++ username ]


nodeIdCodec : String -> String -> NodeType.NodeType -> String
nodeIdCodec parentid targetid nodeType =
    -- Returns the namid of a new Circle/Role given the parentid and the nameid fragment.
    let
        rootnameid =
            nid2rootid parentid
    in
    case nodeType of
        NodeType.Circle ->
            String.join "#" [ rootnameid, targetid ]

        NodeType.Role ->
            if rootnameid == parentid then
                String.join "#" [ rootnameid, "", targetid ]

            else
                String.join "#" [ parentid, targetid ]



{-
   Tension Codec
-}


type alias TensionCharac =
    { action_type : ActionType
    , doc_type : DocType
    }


type ActionType
    = EDIT
    | NEW


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

        TensionAction.NewCircle ->
            { action_type = NEW, doc_type = NODE }

        TensionAction.EditCircle ->
            { action_type = EDIT, doc_type = NODE }

        TensionAction.NewMd ->
            { action_type = NEW, doc_type = MD }

        TensionAction.EditMd ->
            { action_type = EDIT, doc_type = MD }
