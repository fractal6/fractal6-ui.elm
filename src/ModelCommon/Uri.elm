module ModelCommon.Uri exposing (Flags_, FractalBaseRoute(..), NodeFocus, NodePath, focusFromNameid, guestIdCodec, nameidFromFlags, toString, uriFromNameid)

import Array
import Fractal.Enum.NodeType as NodeType
import Generated.Route as Route exposing (Route)
import Maybe exposing (withDefault)



--
-- Routing types
--


type alias Flags_ =
    { param1 : String, param2 : Maybe String, param3 : Maybe String }


type FractalBaseRoute
    = OverviewBaseUri
    | TensionsBaseUri
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


type alias NodePath =
    Array.Array { nidjs : String, nameid : String, name : String }


toString : FractalBaseRoute -> String
toString route =
    case route of
        OverviewBaseUri ->
            -- /org
            "o"

        TensionsBaseUri ->
            -- /tensions
            "t"

        UsersBaseUri ->
            -- /users
            "u"


uriFromNameid : FractalBaseRoute -> String -> String
uriFromNameid loc nameid =
    let
        path =
            String.split "#" nameid

        b =
            toString loc
    in
    [ b ]
        ++ path
        |> String.join "/"
        |> String.append "/"


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


nameidFromFlags : Flags_ -> String
nameidFromFlags flags =
    let
        rootnameid =
            flags.param1

        focusFragment =
            String.join "#" [ flags.param2 |> withDefault "", flags.param3 |> withDefault "" ]
    in
    String.join "#" [ rootnameid, focusFragment ]


guestIdCodec : String -> String -> String
guestIdCodec rootnameid username =
    String.join "#" [ rootnameid, "", username ]
