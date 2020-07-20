module ModelCommon.Uri exposing (..)

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
    String.join "#" [ rootnameid, "", "@" ++ username ]


{-|

    Returns the namid of a new Circle/Role given the parenid and the nameid fragment.

-}
nodeIdCodec : String -> String -> NodeType.NodeType -> String
nodeIdCodec parentid targetid nodeType =
    let
        rootnameid =
            parentid |> String.split "#" |> List.head |> withDefault ""
    in
    case nodeType of
        NodeType.Circle ->
            String.join "#" [ rootnameid, targetid ]

        NodeType.Role ->
            String.join "#" [ rootnameid, "", targetid ]
