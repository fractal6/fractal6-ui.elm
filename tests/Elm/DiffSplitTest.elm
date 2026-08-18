module Elm.DiffSplitTest exposing (..)

import Components.NodeDoc exposing (DiffRow, Hunk(..), sideRuns, toSplitRows, withContext)
import Expect
import Maybe exposing (withDefault)
import Test exposing (Test, describe, test)
import Utils.Diff as Diff


splitRowsTests : Test
splitRowsTests =
    describe "toSplitRows"
        [ test "pairs removed/added hunks side by side" <|
            \_ ->
                toSplitRows [ Diff.NoChange "a", Diff.Removed "b", Diff.Added "c", Diff.NoChange "d" ]
                    |> Expect.equal
                        [ DiffRow (Just "a") (Just "a") False
                        , DiffRow (Just "b") (Just "c") True
                        , DiffRow (Just "d") (Just "d") False
                        ]
        , test "pads the shorter side of an uneven hunk" <|
            \_ ->
                toSplitRows [ Diff.Removed "a", Diff.Added "x", Diff.Added "y" ]
                    |> Expect.equal
                        [ DiffRow (Just "a") (Just "x") True
                        , DiffRow Nothing (Just "y") True
                        ]
        , test "flushes a trailing removed-only hunk" <|
            \_ ->
                toSplitRows [ Diff.NoChange "a", Diff.Removed "b" ]
                    |> Expect.equal
                        [ DiffRow (Just "a") (Just "a") False
                        , DiffRow (Just "b") Nothing True
                        ]
        , test "sideRuns groups changed chars into runs per side" <|
            \_ ->
                let
                    changes =
                        Diff.diff (String.toList "the cat sat") (String.toList "the dog sat")
                in
                ( sideRuns True changes, sideRuns False changes )
                    |> Expect.equal
                        ( [ ( False, "the " ), ( True, "cat" ), ( False, " sat" ) ]
                        , [ ( False, "the " ), ( True, "dog" ), ( False, " sat" ) ]
                        )
        , test "sideRuns keeps a pure insertion on the right only" <|
            \_ ->
                let
                    changes =
                        Diff.diff (String.toList "abc") (String.toList "abXc")
                in
                ( sideRuns True changes, sideRuns False changes )
                    |> Expect.equal
                        ( [ ( False, "abc" ) ]
                        , [ ( False, "ab" ), ( True, "X" ), ( False, "c" ) ]
                        )
        , test "withContext collapses runs beyond the context window" <|
            \_ ->
                let
                    same i =
                        DiffRow (Just (String.fromInt i)) (Just (String.fromInt i)) False

                    rows =
                        List.map same (List.range 1 10)
                            ++ [ DiffRow (Just "old") (Just "new") True ]
                            ++ List.map same (List.range 11 20)
                in
                withContext 4 rows
                    |> List.map
                        (\h ->
                            case h of
                                Skipped k ->
                                    "skip " ++ String.fromInt k

                                Line r ->
                                    withDefault "" r.left
                        )
                    |> Expect.equal
                        [ "skip 6", "7", "8", "9", "10", "old", "11", "12", "13", "14", "skip 6" ]
        , test "withContext keeps everything when changes are dense" <|
            \_ ->
                let
                    rows =
                        [ DiffRow (Just "a") (Just "a") False
                        , DiffRow (Just "b") (Just "B") True
                        , DiffRow (Just "c") (Just "c") False
                        ]
                in
                withContext 4 rows |> List.length |> Expect.equal 3
        , test "diffLines end to end" <|
            \_ ->
                Diff.diffLines "aaa\nbbb\nddd" "zzz\naaa\nccc\nddd"
                    |> toSplitRows
                    |> Expect.equal
                        [ DiffRow Nothing (Just "zzz") True
                        , DiffRow (Just "aaa") (Just "aaa") False
                        , DiffRow (Just "bbb") (Just "ccc") True
                        , DiffRow (Just "ddd") (Just "ddd") False
                        ]
        ]
