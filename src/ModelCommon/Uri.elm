module ModelCommon.Uri exposing
    ( Flags_
    , FractalBaseRoute(..)
    , NodeFocus
    , NodePath
    , basePathChanged
    , circleIdCodec
    , focusFromNameid
    , guestIdCodec
    , nameidFromFlags
    , toString
    , uriFromNameid
    , uriFromUsername
    )

import Array exposing (Array)
import Fractal.Enum.NodeType as NodeType
import Generated.Route as Route exposing (Route)
import Maybe exposing (withDefault)
import Url exposing (Url)



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
    Array { nidjs : String, nameid : String, name : String }


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
            "user"


basePathChanged : FractalBaseRoute -> Url -> Bool
basePathChanged loc url =
    let
        base =
            url.path
                |> String.dropLeft 1
                |> String.split "/"
                |> List.head
                |> withDefault ""

        baseRef =
            toString loc
    in
    base /= baseRef


uriFromNameid : FractalBaseRoute -> String -> String
uriFromNameid loc nameid =
    [ toString loc ]
        ++ String.split "#" nameid
        |> String.join "/"
        |> String.append "/"


uriFromUsername : FractalBaseRoute -> String -> String
uriFromUsername loc username =
    [ toString loc, username ]
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


{-|

    Returns the namid of a new Role given an username and a rootnameid

-}
guestIdCodec : String -> String -> String
guestIdCodec rootnameid username =
    String.join "#" [ rootnameid, "", username ]


{-|

    Returns the namid of a new Circle given the parenid and the nameid fragment.

-}
circleIdCodec : String -> String -> String
circleIdCodec parentid targetid =
    let
        rootnameid =
            parentid |> String.split "#" |> List.head |> withDefault ""
    in
    String.join "#" [ rootnameid, targetid ]
