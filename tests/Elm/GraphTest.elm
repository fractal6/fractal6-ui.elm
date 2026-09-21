module Elm.GraphTest exposing (..)

import Components.ActionPanel as ActionPanel
import Components.TreeMenu as TreeMenu
import Dict
import Expect
import Fractale.Codecs exposing (FractalBaseRoute(..), focusFromNameid, nid2eid, nodeIdCodec)
import Fractale.Graph exposing (isFreshOrga, withDescendants)
import Fractale.HotUpdate exposing (hotNodeMove)
import Fractale.User exposing (UserState(..))
import Html
import Json.Decode as JD
import Loading exposing (GqlData, RequestResult(..), withDefaultData)
import ModelSchema exposing (NodesDict, initNode, initUserctx)
import Org.Overview exposing (viewActionPanel)
import Ports
import Schema.Enum.Lang as Lang
import Schema.Enum.NodeType as NodeType
import Schema.Enum.RoleType as RoleType
import Session exposing (Apis, GlobalCmd(..), SessionCommon)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, id, text)
import Time
import Url


{-| Nameids follow the app codec: circles are flat (`org#name`, whatever the depth),
roles carry their parent circle (`org#c1#@bob`). The hierarchy only lives in `parent`.
-}
tree : List ( String, Maybe String ) -> GqlData NodesDict
tree nodes =
    nodes
        |> List.map
            (\( nameid, parentid ) ->
                ( nameid
                , { initNode
                    | nameid = nameid
                    , parent = Maybe.map (\p -> { nameid = p, source = Nothing }) parentid
                  }
                )
            )
        |> Dict.fromList
        |> Success


orga : GqlData NodesDict
orga =
    tree
        [ ( "org", Nothing )
        , ( "org#c1", Just "org" )
        , ( "org#c1#@bob", Just "org#c1" )
        , ( "org#c2", Just "org#c1" )
        , ( "org#c2#@alice", Just "org#c2" )
        , ( "org#c3", Just "org" )
        , ( "org#c3#@eve", Just "org#c3" )
        ]


withDescendantsTests : Test
withDescendantsTests =
    describe "withDescendants"
        [ test "pulls the sub-circles and their roles, whatever the depth" <|
            \_ ->
                withDescendants [ "org#c1" ] orga
                    |> List.sort
                    |> Expect.equal [ "org#c1", "org#c1#@bob", "org#c2", "org#c2#@alice" ]
        , test "leaves the other branches untouched" <|
            \_ ->
                withDescendants [ "org#c3" ] orga
                    |> List.sort
                    |> Expect.equal [ "org#c3", "org#c3#@eve" ]
        , test "keeps a leaf role alone" <|
            \_ ->
                withDescendants [ "org#c2#@alice" ] orga
                    |> Expect.equal [ "org#c2#@alice" ]
        , test "handles several targets at once" <|
            \_ ->
                withDescendants [ "org#c2", "org#c3" ] orga
                    |> List.sort
                    |> Expect.equal [ "org#c2", "org#c2#@alice", "org#c3", "org#c3#@eve" ]
        , test "is a no-op when the tree is not loaded" <|
            \_ ->
                withDescendants [ "org#c1" ] NotAsked
                    |> Expect.equal [ "org#c1" ]
        ]


freshOrgaTests : Test
freshOrgaTests =
    let
        withRole role_type =
            Dict.insert "org#@bob"
                { initNode | nameid = "org#@bob", parent = Just { nameid = "org", source = Nothing }, role_type = Just role_type }
    in
    describe "isFreshOrga"
        [ test "the root alone is fresh" <|
            \_ ->
                tree [ ( "org", Nothing ) ]
                    |> withDefaultData Dict.empty
                    |> isFreshOrga "org"
                    |> Expect.equal True
        , test "a child of the root is not" <|
            \_ ->
                orga
                    |> withDefaultData Dict.empty
                    |> isFreshOrga "org"
                    |> Expect.equal False
        , test "membership roles are not drawn, they keep the root fresh" <|
            \_ ->
                tree [ ( "org", Nothing ) ]
                    |> withDefaultData Dict.empty
                    |> withRole RoleType.Owner
                    |> isFreshOrga "org"
                    |> Expect.equal True
        , test "any other role fills the root" <|
            \_ ->
                tree [ ( "org", Nothing ) ]
                    |> withDefaultData Dict.empty
                    |> withRole RoleType.Peer
                    |> isFreshOrga "org"
                    |> Expect.equal False
        ]


moveTests : Test
moveTests =
    describe "confirmed graph moves"
        [ test "reparents a circle and explicitly retains all descendants, including flat sub-circle IDs" <|
            \_ ->
                let
                    ( data, renames ) =
                        hotNodeMove "org#c1" "org#c3" "org#c1" orga
                in
                Expect.all
                    [ \_ ->
                        Expect.equal
                            [ ( "org#c1", "org#c1" ), ( "org#c1#@bob", "org#c1#@bob" ), ( "org#c2", "org#c2" ), ( "org#c2#@alice", "org#c2#@alice" ) ]
                            (Dict.toList renames)
                    , \_ ->
                        Expect.equal (Just "org#c3")
                            (Dict.get "org#c1" data |> Maybe.andThen .parent |> Maybe.map .nameid)
                    , \_ ->
                        Expect.equal (Just "org#c1")
                            (Dict.get "org#c2" data |> Maybe.andThen .parent |> Maybe.map .nameid)
                    , \_ -> Expect.equal 7 (Dict.size data)
                    ]
                    ()
        , test "successive role moves carry their own rename map with the matching tree snapshot" <|
            \_ ->
                let
                    ( first, firstRenames ) =
                        hotNodeMove "org#c1#@bob" "org#c3" "org#c3#@bob" orga

                    ( second, secondRenames ) =
                        hotNodeMove "org#c3#@bob" "org" "org##@bob" (Success first)

                    payloadRenames data renames =
                        Ports.graphPackEncoder data "" renames
                            |> JD.decodeValue (JD.field "nodeRenames" (JD.dict JD.string))
                in
                Expect.all
                    [ \_ -> Expect.equal False (Dict.member "org#c1#@bob" first)
                    , \_ -> Expect.equal True (Dict.member "org#c3#@bob" first)
                    , \_ -> Expect.equal False (Dict.member "org#c3#@bob" second)
                    , \_ -> Expect.equal True (Dict.member "org##@bob" second)
                    , \_ -> Expect.equal (Ok (Dict.singleton "org#c1#@bob" "org#c3#@bob")) (payloadRenames first firstRenames)
                    , \_ -> Expect.equal (Ok (Dict.singleton "org#c3#@bob" "org##@bob")) (payloadRenames second secondRenames)
                    ]
                    ()
        , test "regular tensions and missing nodes cannot remove graph data or emit a rename" <|
            \_ ->
                [ hotNodeMove "org#c1" "org#c3" "" orga
                , hotNodeMove "missing" "org#c3" "org#c3#role" orga
                , hotNodeMove "org#c1" "missing" "org#c1" orga
                ]
                    |> List.map (\( data, renames ) -> ( Success data, renames ))
                    |> Expect.equal (List.repeat 3 ( orga, Dict.empty ))
        ]


session : SessionCommon
session =
    { user = LoggedOut
    , screen = { w = 1200, h = 800 }
    , theme = Session.SystemTheme
    , lang = Lang.En
    , now = Time.millisToPosix 0
    , url = Url.Url Url.Http "localhost" Nothing "/" Nothing Nothing
    , query = Dict.empty
    , viewMode = Session.DesktopView
    , node_focus = Nothing
    , path_data = Nothing
    , lexicon = Dict.empty
    , scrollPosition = Ports.ScrollTop
    , file_server_url = ""
    }


treeMenuCounterTest : Test
treeMenuCounterTest =
    test "node names, usernames and counters share inline flow rather than separate flex items" <|
        \_ ->
            let
                node =
                    { initNode
                        | nameid = "org"
                        , name = "Marketing/ Communication"
                        , first_link = Just { username = "bob", name = Nothing }
                        , n_open_tensions = 7
                        , n_open_contracts = 2
                    }

                label =
                    TreeMenu.init OverviewBaseUri Nothing (focusFromNameid "org")
                        (Just { isActive = True, expanded_lines = Dict.empty })
                        (Just (Dict.singleton "org" node))
                        session
                        |> TreeMenu.view {} False
                        |> Query.fromHtml
                        |> Query.find [ class "treeMenu-label" ]
            in
            Expect.all
                [ Query.has [ text node.name, text "@bob", text "7", text "2" ]
                , Query.findAll [ class "has-background-counter" ] >> Query.count (Expect.equal 2)
                ]
                label


nidCodecTests : Test
nidCodecTests =
    -- ActionPanel.openMoveOf feeds nid2eid to the move modal, MoveTension.buildOutResult
    -- rebuilds the nameid with nodeIdCodec: the roundtrip must hold.
    let
        roundtrip type_ parentid nameid =
            nodeIdCodec parentid (nid2eid nameid) type_
    in
    describe "nid2eid is the inverse of nodeIdCodec"
        [ test "a circle is rebuilt from its parent" <|
            \_ ->
                roundtrip NodeType.Circle "org#c1" "org#c2"
                    |> Expect.equal "org#c2"
        , test "a role nested in a circle is rebuilt from its parent" <|
            \_ ->
                roundtrip NodeType.Role "org#c1" "org#c1#@bob"
                    |> Expect.equal "org#c1#@bob"
        , test "a role hanging on the root keeps its empty middle segment" <|
            \_ ->
                roundtrip NodeType.Role "org" "org##role"
                    |> Expect.equal "org##role"
        ]


tokenTests : Test
tokenTests =
    -- Tree edits never refresh the token: the callers that can change the current user roles do.
    let
        apis =
            Apis "" "" "" "" "" "" "test"

        initial =
            TreeMenu.init OverviewBaseUri Nothing (focusFromNameid "org#c1") Nothing (Just (withDefaultData Dict.empty orga)) session

        gcmdsOf msg =
            TreeMenu.update apis msg initial |> Tuple.second |> .gcmds
    in
    describe "tree edits do not refresh the token"
        [ test "neither UpdateNode, DelNodes nor AddNodes ask for a new token" <|
            \_ ->
                [ TreeMenu.UpdateNode "org#c1" identity
                , TreeMenu.DelNodes [ "org#c2" ]
                , TreeMenu.AddNodes [ { initNode | nameid = "org#c4", parent = Just { nameid = "org", source = Nothing } } ]
                ]
                    |> List.concatMap gcmdsOf
                    |> List.member DoUpdateToken
                    |> Expect.equal False
        , test "UpdateNode on an unknown node is a no-op" <|
            \_ ->
                gcmdsOf (TreeMenu.UpdateNode "org#nope" identity)
                    |> Expect.equal []
        ]


refreshTests : Test
refreshTests =
    let
        apis =
            Apis "" "" "" "" "" "" "test"

        initial =
            TreeMenu.init OverviewBaseUri Nothing (focusFromNameid "org#c1") Nothing (Just (withDefaultData Dict.empty orga)) session

        ( moved, moveOut ) =
            TreeMenu.update apis (TreeMenu.MoveNode "org#c1" "org#c3" "org#c1") initial

        expected =
            TreeMenu.getOrgaData_ moved |> withDefaultData Dict.empty
    in
    describe "move reconciliation"
        [ test "the move is applied locally, without refetching the tree nor invalidating the token" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal [] moveOut.cmds
                    , \_ -> Expect.equal [ DoUpdateTree (Just expected) ] moveOut.gcmds
                    , \_ -> Expect.equal (Just "org#c1") (Dict.get "org#c1" moveOut.nodeRenames)
                    , \_ -> Expect.equal (Just "org#c3") (Dict.get "org#c1" expected |> Maybe.andThen .parent |> Maybe.map .nameid)
                    ]
                    ()
        , test "a renamed node (role move) refreshes the token" <|
            \_ ->
                TreeMenu.update apis (TreeMenu.MoveNode "org#c1#@bob" "org#c3" "org#c3#@bob") initial
                    |> Tuple.second
                    |> .gcmds
                    |> List.head
                    |> Expect.equal (Just DoUpdateToken)
        , test "open tension counters shift from the old parent to the new one, clamped at zero" <|
            \_ ->
                let
                    counted =
                        orga
                            |> withDefaultData Dict.empty
                            |> Dict.map
                                (\nid n ->
                                    { n
                                        | n_open_tensions =
                                            if nid == "org#c1" then
                                                3

                                            else
                                                0
                                    }
                                )
                            |> Success

                    ( data, _ ) =
                        hotNodeMove "org#c2" "org#c3" "org#c2" counted

                    counter nid =
                        Dict.get nid data |> Maybe.map .n_open_tensions
                in
                Expect.equal ( Just 2, Just 1 ) ( counter "org#c1", counter "org#c3" )
        , test "drag moves activate the move modal's close and contract subscriptions" <|
            \_ ->
                ActionPanel.init session
                    |> ActionPanel.update apis (ActionPanel.OnMoveTo "actionPanelHelper" initNode initNode)
                    |> Tuple.first
                    |> ActionPanel.getState_
                    |> Expect.equal ActionPanel.MoveAction
        , test "the tooltip menu renders inside its trigger's #domid, so its items are not outside clicks" <|
            -- A mouseup on an item must not fire OnClose: that would unlock the graphpack tooltip before the modal opens.
            \_ ->
                let
                    domid =
                        "actionPanelContentTooltip"

                    uctx =
                        { initUserctx | username = "bob" }

                    node =
                        { initNode | nameid = "org#c1", first_link = Just { username = "bob", name = Nothing } }

                    panel =
                        ActionPanel.init { session | user = LoggedIn uctx }
                            |> ActionPanel.update apis (ActionPanel.OnOpen domid node.nameid orga Nothing)
                            |> Tuple.first
                            |> ActionPanel.update apis (ActionPanel.OnOpen_ domid)
                            |> Tuple.first
                in
                Html.div [] [ viewActionPanel domid (LoggedIn uctx) node orga panel ]
                    |> Query.fromHtml
                    |> Query.find [ id domid ]
                    |> Query.findAll [ class "actionPanelStyle" ]
                    |> Query.count (Expect.equal 1)
        ]
