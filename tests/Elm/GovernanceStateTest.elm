module Elm.GovernanceStateTest exposing (..)

import Expect
import Fractale.Codecs exposing (getTensionNode)
import ModelSchema exposing (NodeLifecycle(..))
import Schema.Enum.NodeType as NodeType
import Test exposing (Test, describe, test)


governanceStateTests : Test
governanceStateTests =
    describe "getTensionNode"
        [ test "derives a Role draft from the draft node type" <|
            \_ ->
                getTensionNode { governed_node = Nothing, draft_node_type = Just NodeType.Role }
                    |> Expect.equal (Just { type_ = NodeType.Role, lifecycle = Draft })
        , test "derives an active Circle from the governed Node" <|
            \_ ->
                getTensionNode
                    { governed_node = Just { nameid = "org#circle", type_ = NodeType.Circle, isArchived = False, isRootArchived = Nothing }
                    , draft_node_type = Just NodeType.Role
                    }
                    |> Expect.equal (Just { type_ = NodeType.Circle, lifecycle = Active })
        , test "derives an archived Role from the governed Node" <|
            \_ ->
                getTensionNode
                    { governed_node = Just { nameid = "org##role", type_ = NodeType.Role, isArchived = True, isRootArchived = Nothing }
                    , draft_node_type = Nothing
                    }
                    |> Expect.equal (Just { type_ = NodeType.Role, lifecycle = Archived })
        , test "derives an archived root from isRootArchived" <|
            \_ ->
                getTensionNode
                    { governed_node = Just { nameid = "org", type_ = NodeType.Circle, isArchived = False, isRootArchived = Just True }
                    , draft_node_type = Nothing
                    }
                    |> Expect.equal (Just { type_ = NodeType.Circle, lifecycle = Archived })
        , test "an unarchived root stays active" <|
            \_ ->
                getTensionNode
                    { governed_node = Just { nameid = "org", type_ = NodeType.Circle, isArchived = False, isRootArchived = Just False }
                    , draft_node_type = Nothing
                    }
                    |> Expect.equal (Just { type_ = NodeType.Circle, lifecycle = Active })
        , test "ordinary tensions have no governed Node state" <|
            \_ ->
                getTensionNode { governed_node = Nothing, draft_node_type = Nothing }
                    |> Expect.equal Nothing
        ]
