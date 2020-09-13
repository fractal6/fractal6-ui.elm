module ModelCommon.Codecs exposing (..)

import Array exposing (Array)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.TensionAction as TensionAction
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


nid2pid : String -> String
nid2pid nameid =
    nameid |> String.split "#" |> List.head |> withDefault ""


{-|

    Returns the namid of a new Role given an username and a rootnameid

-}
guestIdCodec : String -> String -> String
guestIdCodec rootnameid username =
    String.join "#" [ rootnameid, "", "@" ++ username ]


{-|

    Returns the namid of a new Circle/Role given the parentid and the nameid fragment.

-}
nodeIdCodec : String -> String -> NodeType.NodeType -> String
nodeIdCodec parentid targetid nodeType =
    let
        rootnameid =
            nid2pid parentid
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
    { action_type : String
    , doc_type : String
    }


getTensionCharac : TensionAction.TensionAction -> TensionCharac
getTensionCharac action =
    case action of
        TensionAction.NewRole ->
            { action_type = "new", doc_type = "node" }

        TensionAction.EditRole ->
            { action_type = "edit", doc_type = "node" }

        TensionAction.NewCircle ->
            { action_type = "new", doc_type = "node" }

        TensionAction.EditCircle ->
            { action_type = "edit", doc_type = "node" }

        TensionAction.NewMd ->
            { action_type = "new", doc_type = "doc" }

        TensionAction.EditMd ->
            { action_type = "edit", doc_type = "doc" }
