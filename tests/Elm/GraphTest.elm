module Elm.GraphTest exposing (..)

import Components.ActionPanel as ActionPanel
import Components.TreeMenu as TreeMenu
import Dict
import Expect
import Fractale.Codecs exposing (FractalBaseRoute(..), focusFromNameid)
import Fractale.Graph exposing (withDescendants)
import Fractale.User exposing (UserState(..))
import Fractale.HotUpdate exposing (hotNodeMove)
import Json.Decode as JD
import Loading exposing (GqlData, RequestResult(..), withDefaultData)
import ModelSchema exposing (NodesDict, initNode)
import Ports
import Schema.Enum.Lang as Lang
import Session exposing (Apis, GlobalCmd(..), SessionCommon)
import Test exposing (Test, describe, test)
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
        [ test "the confirmed move publishes its map once and fetches an authoritative snapshot" <|
            \_ ->
                let
                    fresh =
                        expected |> Dict.remove "org#c1" |> Dict.map (\_ n -> { n | n_open_tensions = 7 })

                    ( reconciled, out ) =
                        TreeMenu.update apis (TreeMenu.MovedTreeAck "org" expected (Success fresh)) moved
                in
                Expect.all
                    [ \_ -> Expect.equal 1 (List.length moveOut.cmds)
                    , \_ -> Expect.equal (Just "org#c1") (Dict.get "org#c1" moveOut.nodeRenames)
                    , \_ -> Expect.equal (Success fresh) (TreeMenu.getOrgaData_ reconciled)
                    , \_ -> Expect.equal [ DoUpdateTree (Just fresh) ] out.gcmds
                    , \_ -> Expect.equal Dict.empty out.nodeRenames
                    ]
                    ()
        , test "a stale response cannot undo a later edit and instead requests fresh data" <|
            \_ ->
                let
                    newer =
                        TreeMenu.update apis (TreeMenu.MoveNode "org#c1" "org" "org#c1") moved |> Tuple.first

                    ( after, out ) =
                        TreeMenu.update apis (TreeMenu.MovedTreeAck "org" expected orga) newer
                in
                Expect.equal ( newer, 1, [] ) ( after, List.length out.cmds, out.gcmds )
        , test "a no-data response clears cached authorization while other-org responses are ignored" <|
            \_ ->
                let
                    ( empty, out ) =
                        TreeMenu.update apis (TreeMenu.MovedTreeAck "org" expected (Failure [ "no data returned" ])) moved

                    ( unchanged, ignored ) =
                        TreeMenu.update apis (TreeMenu.MovedTreeAck "other" expected orga) moved
                in
                Expect.all
                    [ \_ -> Expect.equal (Success Dict.empty) (TreeMenu.getOrgaData_ empty)
                    , \_ -> Expect.equal [ DoUpdateTree (Just Dict.empty) ] out.gcmds
                    , \_ -> Expect.equal ( moved, [], [] ) ( unchanged, ignored.cmds, ignored.gcmds )
                    ]
                    ()
        , test "drag moves activate the move modal's close and contract subscriptions" <|
            \_ ->
                ActionPanel.init session
                    |> ActionPanel.update apis (ActionPanel.OnMoveTo "actionPanelHelper" "tension" initNode)
                    |> Tuple.first
                    |> ActionPanel.getState_
                    |> Expect.equal ActionPanel.MoveAction
        ]
