module Query.QueryNodes exposing (nodeOrgaPayload, queryGraphPack, queryLocalGraph)

import Dict exposing (Dict)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.InputObject as Input
import Fractal.Object
import Fractal.Object.Node
import Fractal.Object.NodeCharac
import Fractal.Object.User
import Fractal.Query as Query
import Fractal.Scalar
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import RemoteData exposing (RemoteData)



{-
   Query Organisation Nodes
-}
--- Response decoder


nodeOrgaDecoder : Maybe (List (Maybe Node)) -> Maybe (Dict String Node)
nodeOrgaDecoder data =
    data
        |> Maybe.map
            (\d ->
                if List.length d == 0 then
                    Nothing

                else
                    d
                        |> List.filterMap identity
                        |> List.map (\n -> ( n.nameid, n ))
                        |> Dict.fromList
                        |> Just
            )
        |> Maybe.withDefault Nothing


queryGraphPack rootid msg =
    makeGQLQuery
        (Query.queryNode
            (nodeOrgaFilter rootid)
            nodeOrgaPayload
        )
        (RemoteData.fromResult >> decodeResponse nodeOrgaDecoder >> msg)


nodeOrgaFilter : String -> Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
nodeOrgaFilter rootid a =
    { a
        | filter =
            Input.buildNodeFilter
                (\b ->
                    { b
                        | rootnameid = Present { eq = Present rootid }
                        , and =
                            Input.buildNodeFilter
                                (\c ->
                                    { c
                                        | not =
                                            Input.buildNodeFilter
                                                (\d ->
                                                    { d | role_type = Present { eq = Present RoleType.Member } }
                                                )
                                                |> Present
                                    }
                                )
                                |> Present
                    }
                )
                |> Present
    }


nodeOrgaPayload : SelectionSet Node Fractal.Object.Node
nodeOrgaPayload =
    SelectionSet.succeed Node
        |> with (Fractal.Object.Node.createdAt |> SelectionSet.map decodedTime)
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with Fractal.Object.Node.rootnameid
        |> with
            (Fractal.Object.Node.parent identity <| SelectionSet.map NodeId Fractal.Object.Node.nameid)
        |> with Fractal.Object.Node.type_
        |> with Fractal.Object.Node.role_type
        |> with (Fractal.Object.Node.first_link identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with (Fractal.Object.Node.charac <| nodeCharacPayload)


nodeCharacPayload : SelectionSet NodeCharac Fractal.Object.NodeCharac
nodeCharacPayload =
    SelectionSet.map2 NodeCharac
        Fractal.Object.NodeCharac.userCanJoin
        Fractal.Object.NodeCharac.mode



{-
   Query Local Graph
-}
--- Response decoder


type alias LocalNode =
    { name : String
    , nameid : String
    , charac : NodeCharac
    , type_ : NodeType.NodeType
    , children : Maybe (List NodeId)
    , parent : Maybe LocalRootNode
    }


type alias LocalRootNode =
    { name : String
    , nameid : String
    , charac : NodeCharac
    , isRoot : Bool
    }


lgDecoder : Maybe LocalNode -> Maybe LocalGraph
lgDecoder data =
    data
        |> Maybe.map
            (\n ->
                case n.parent of
                    Just p ->
                        let
                            focus =
                                FocusNode n.name n.nameid n.type_ (n.children |> withDefault [])

                            path =
                                [ PNode p.name p.nameid, PNode n.name n.nameid ]
                        in
                        if p.isRoot then
                            { root = RootNode p.name p.nameid p.charac |> Just
                            , path = path
                            , focus = focus
                            }

                        else
                            -- partial path
                            { root = Nothing, path = path, focus = focus }

                    Nothing ->
                        -- Assume Root node
                        { root = RootNode n.name n.nameid n.charac |> Just
                        , path = [ PNode n.name n.nameid ]
                        , focus = FocusNode n.name n.nameid n.type_ (n.children |> withDefault [])
                        }
            )


queryLocalGraph targetid msg =
    makeGQLQuery
        (Query.getNode
            (lgFilter targetid)
            lgPayload
        )
        (RemoteData.fromResult >> decodeResponse lgDecoder >> msg)


lgFilter : String -> Query.GetNodeOptionalArguments -> Query.GetNodeOptionalArguments
lgFilter nid a =
    { a | nameid = Present nid }


lgPayload : SelectionSet LocalNode Fractal.Object.Node
lgPayload =
    SelectionSet.succeed LocalNode
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with (Fractal.Object.Node.charac <| nodeCharacPayload)
        |> with Fractal.Object.Node.type_
        |> with
            (Fractal.Object.Node.children identity
                (SelectionSet.map NodeId Fractal.Object.Node.nameid)
            )
        |> with
            (Fractal.Object.Node.parent identity lg2Payload)


lg2Payload : SelectionSet LocalRootNode Fractal.Object.Node
lg2Payload =
    SelectionSet.succeed LocalRootNode
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with (Fractal.Object.Node.charac <| nodeCharacPayload)
        |> with Fractal.Object.Node.isRoot
