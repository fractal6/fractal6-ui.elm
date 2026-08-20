module Elm.GraphTest exposing (..)

import Dict
import Expect
import Fractale.Graph exposing (withDescendants)
import Loading exposing (GqlData, RequestResult(..))
import ModelSchema exposing (NodesDict, initNode)
import Test exposing (Test, describe, test)


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
