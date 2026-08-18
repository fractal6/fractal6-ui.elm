module Elm.OrgFilterTest exposing (..)

import Expect
import Graphql.Document as Doc
import Query.QueryTension exposing (orgNodesFilter, orgTensionPayload)
import Schema.Enum.NodeType as NodeType
import Schema.Query as Query
import Test exposing (Test, describe, test)


serialize : { nameid : String, type_ : NodeType.NodeType, isArchived : Bool, noFirstLink : Bool, sort : Maybe String, first : Int, offset : Int } -> String
serialize q =
    Doc.serializeQuery (Query.queryNode (orgNodesFilter q) orgTensionPayload)


roles : { nameid : String, type_ : NodeType.NodeType, isArchived : Bool, noFirstLink : Bool, sort : Maybe String, first : Int, offset : Int }
roles =
    { nameid = "org", type_ = NodeType.Role, isArchived = False, noFirstLink = False, sort = Nothing, first = 15, offset = 0 }


orgFilterTests : Test
orgFilterTests =
    describe "orgNodesFilter"
        [ test "open roles: scoped subtree, non archived, no first_link, no source-less node" <|
            \_ ->
                serialize { roles | nameid = "org#circle", noFirstLink = True }
                    |> String.contains
                        ("queryNode(filter: {nameid: {regexp: \"/^org#circle(#|$)/\"}, type_: {eq: Role}, isArchived: false"
                            ++ ", not: {or: [{role_type: {in: [Owner, Member, Guest, Pending, Retired]}}, {has: [first_link]}]}}"
                        )
                    |> Expect.equal True
        , test "archived circles: no first_link constraint" <|
            \_ ->
                serialize { roles | type_ = NodeType.Circle, isArchived = True, offset = 30 }
                    |> String.contains "type_: {eq: Circle}, isArchived: true, not:"
                    |> Expect.equal True
        , test "walks back to the tension through the node source blob" <|
            \_ ->
                serialize { roles | type_ = NodeType.Circle }
                    |> String.contains "source {\n      tension {"
                    |> Expect.equal True
        , test "sort maps onto the node order" <|
            \_ ->
                List.map (\s -> serialize { roles | sort = s } |> String.contains "order: {desc: createdAt}")
                    [ Nothing, Just "newest" ]
                    |> Expect.equal [ True, True ]
        , test "oldest sorts ascending on creation" <|
            \_ ->
                serialize { roles | sort = Just "oldest" }
                    |> String.contains "order: {asc: createdAt}"
                    |> Expect.equal True
        , test "activity sorts on the last node update" <|
            \_ ->
                serialize { roles | sort = Just "activity" }
                    |> String.contains "order: {desc: updatedAt}"
                    |> Expect.equal True
        ]
