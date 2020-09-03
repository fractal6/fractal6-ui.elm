module Query.QueryNode exposing
    ( MemberNode
    , NodeExt
    , User
    , nodeCharacPayload
    , nodeOrgaPayload
    , queryGraphPack
    , queryLocalGraph
    , queryMembers
    , queryNodeExt
    , queryNodesSub
    , queryPublicOrga
    )

import Dict exposing (Dict)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.InputObject as Input
import Fractal.Object
import Fractal.Object.Node
import Fractal.Object.NodeCharac
import Fractal.Object.NodeStats
import Fractal.Object.User
import Fractal.Query as Query
import Fractal.Scalar
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import RemoteData exposing (RemoteData)



{-
   Query Public Orga / Explore
-}
--- Response decoder
--


type alias NodeExt =
    { id : String
    , createdAt : String
    , name : String
    , nameid : String
    , rootnameid : String
    , parent : Maybe NodeId -- see issue with recursive structure
    , type_ : NodeType.NodeType
    , role_type : Maybe RoleType.RoleType
    , first_link : Maybe Username
    , charac : NodeCharac
    , isPrivate : Bool
    , stats : Maybe NodeStats
    , about : Maybe String
    }


type alias NodeStats =
    { n_member : Maybe Int
    , n_guest : Maybe Int
    }


nodesDecoder : Maybe (List (Maybe node)) -> Maybe (List node)
nodesDecoder data =
    data
        |> Maybe.map
            (\d ->
                if List.length d == 0 then
                    Nothing

                else
                    d
                        |> List.filterMap identity
                        |> Just
            )
        |> Maybe.withDefault Nothing


queryPublicOrga url msg =
    makeGQLQuery url
        (Query.queryNode
            publicOrgaFilter
            nodeOrgaExtPayload
        )
        (RemoteData.fromResult >> decodeResponse nodesDecoder >> msg)


publicOrgaFilter : Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
publicOrgaFilter a =
    { a
        | filter =
            Input.buildNodeFilter
                (\b ->
                    { b
                        | isPrivate = Present False
                        , isRoot = Present True
                    }
                )
                |> Present
    }


nodeOrgaExtPayload : SelectionSet NodeExt Fractal.Object.Node
nodeOrgaExtPayload =
    SelectionSet.succeed NodeExt
        |> with (Fractal.Object.Node.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Node.createdAt |> SelectionSet.map decodedTime)
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with Fractal.Object.Node.rootnameid
        |> with
            (Fractal.Object.Node.parent identity <| SelectionSet.map NodeId Fractal.Object.Node.nameid)
        |> with Fractal.Object.Node.type_
        |> with Fractal.Object.Node.role_type
        |> with (Fractal.Object.Node.first_link identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with (Fractal.Object.Node.charac identity nodeCharacPayload)
        |> with Fractal.Object.Node.isPrivate
        |> with
            (Fractal.Object.Node.stats <|
                SelectionSet.map2 NodeStats
                    Fractal.Object.NodeStats.n_member
                    Fractal.Object.NodeStats.n_guest
            )
        |> with Fractal.Object.Node.about



{-
   Query Node Ext / Profile
-}
--- Response decoder
--


queryNodeExt url nameids msg =
    makeGQLQuery url
        (Query.queryNode
            (nodeExtFilter nameids)
            nodeOrgaExtPayload
        )
        (RemoteData.fromResult >> decodeResponse nodesDecoder >> msg)


nodeExtFilter : List String -> Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
nodeExtFilter nameids a =
    let
        nameidsRegxp_ =
            nameids
                |> List.map (\n -> "^" ++ n ++ "$")
                |> String.join "|"

        nameidsRegxp =
            "/" ++ nameidsRegxp_ ++ "/"
    in
    { a
        | filter =
            Input.buildNodeFilter
                (\b ->
                    { b
                        | nameid = { eq = Absent, regexp = Present nameidsRegxp } |> Present
                    }
                )
                |> Present
    }



{-
   Query Node and Sub Nodes / FetchNodes
-}


queryNodesSub url nameid msg =
    makeGQLQuery url
        (Query.queryNode
            (nodesSubFilter nameid)
            nodeOrgaPayload
        )
        (RemoteData.fromResult >> decodeResponse nodesDecoder >> msg)


nodesSubFilter : String -> Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
nodesSubFilter nameid a =
    let
        nameidRegxp =
            "/^" ++ nameid ++ "/"
    in
    { a
        | filter =
            Input.buildNodeFilter
                (\b ->
                    { b
                        | nameid = { eq = Absent, regexp = Present nameidRegxp } |> Present
                    }
                )
                |> Present
    }



{-
   Query Organisation Nodes / GraphPack
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


queryGraphPack url rootid msg =
    makeGQLQuery url
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
                        | rootnameid = Present { eq = Present rootid, regexp = Absent }
                        , not =
                            Input.buildNodeFilter
                                (\d ->
                                    { d | role_type = Present { eq = Present RoleType.Member } }
                                )
                                |> Present
                    }
                )
                |> Present
    }


nodeOrgaPayload : SelectionSet Node Fractal.Object.Node
nodeOrgaPayload =
    SelectionSet.succeed Node
        |> with (Fractal.Object.Node.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Node.createdAt |> SelectionSet.map decodedTime)
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with Fractal.Object.Node.rootnameid
        |> with
            (Fractal.Object.Node.parent identity <| SelectionSet.map NodeId Fractal.Object.Node.nameid)
        |> with Fractal.Object.Node.type_
        |> with Fractal.Object.Node.role_type
        |> with (Fractal.Object.Node.first_link identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with (Fractal.Object.Node.charac identity nodeCharacPayload)
        |> with Fractal.Object.Node.isPrivate


nodeCharacPayload : SelectionSet NodeCharac Fractal.Object.NodeCharac
nodeCharacPayload =
    SelectionSet.map2 NodeCharac
        Fractal.Object.NodeCharac.userCanJoin
        Fractal.Object.NodeCharac.mode



{-
   Query Local Graph / Path Data
-}
--- Response decoder


type alias LocalNode =
    { name : String
    , nameid : String
    , charac : NodeCharac
    , type_ : NodeType.NodeType
    , children : Maybe (List NodeId)
    , parent : Maybe LocalRootNode
    , id : String
    }


type alias LocalRootNode =
    { name : String
    , nameid : String
    , charac : NodeCharac
    , isRoot : Bool
    , id : String
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
                            { root = RootNode p.name p.nameid p.charac p.id |> Just
                            , path = path
                            , focus = focus
                            }

                        else
                            -- partial path
                            { root = Nothing, path = path, focus = focus }

                    Nothing ->
                        -- Assume Root node
                        { root = RootNode n.name n.nameid n.charac n.id |> Just
                        , path = [ PNode n.name n.nameid ]
                        , focus = FocusNode n.name n.nameid n.type_ (n.children |> withDefault [])
                        }
            )


queryLocalGraph url targetid msg =
    makeGQLQuery url
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
        |> with (Fractal.Object.Node.charac identity nodeCharacPayload)
        |> with Fractal.Object.Node.type_
        |> with
            (Fractal.Object.Node.children identity
                (SelectionSet.map NodeId Fractal.Object.Node.nameid)
            )
        |> with
            (Fractal.Object.Node.parent identity lg2Payload)
        |> with (Fractal.Object.Node.id |> SelectionSet.map decodedId)


lg2Payload : SelectionSet LocalRootNode Fractal.Object.Node
lg2Payload =
    SelectionSet.succeed LocalRootNode
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with (Fractal.Object.Node.charac identity nodeCharacPayload)
        |> with Fractal.Object.Node.isRoot
        |> with (Fractal.Object.Node.id |> SelectionSet.map decodedId)



{-
   Query Members
-}
--- Response decoder


type alias TopMemberNode =
    { createdAt : String
    , name : String
    , nameid : String
    , rootnameid : String
    , role_type : Maybe RoleType.RoleType
    , first_link : Maybe User
    , parent : Maybe NodeId
    , children : Maybe (List MemberNode)
    }


type alias MemberNode =
    { createdAt : String
    , name : String
    , nameid : String
    , rootnameid : String
    , role_type : Maybe RoleType.RoleType
    , first_link : Maybe User
    , parent : Maybe NodeId
    }


type alias User =
    { username : String
    , name : Maybe String
    }


membersDecoder : Maybe TopMemberNode -> Maybe (List Member)
membersDecoder data =
    let
        n2r n =
            UserRoleExtended n.name n.nameid n.rootnameid (n.role_type |> withDefault RoleType.Guest) n.createdAt n.parent
    in
    data
        |> Maybe.map
            (\n ->
                case n.first_link of
                    Just first_link ->
                        Just [ Member first_link.username first_link.name [ n2r n ] ]

                    Nothing ->
                        case n.children of
                            Just children ->
                                let
                                    toTuples : MemberNode -> List ( String, Member )
                                    toTuples m =
                                        case m.first_link of
                                            Just fs ->
                                                [ ( fs.username, Member fs.username fs.name [ n2r m ] ) ]

                                            Nothing ->
                                                []

                                    toDict : List ( String, Member ) -> Dict String Member
                                    toDict inputs =
                                        List.foldl
                                            (\( k, v ) dict -> Dict.update k (addParam v) dict)
                                            Dict.empty
                                            inputs

                                    addParam : Member -> Maybe Member -> Maybe Member
                                    addParam m maybeMember =
                                        case maybeMember of
                                            Just member ->
                                                Just { member | roles = member.roles ++ m.roles }

                                            Nothing ->
                                                Just m
                                in
                                List.concatMap toTuples children
                                    |> toDict
                                    |> Dict.values
                                    |> Just

                            Nothing ->
                                Nothing
            )
        |> withDefault Nothing


queryMembers url nid msg =
    makeGQLQuery url
        (Query.getNode
            (membersFilter nid)
            membersPayload
        )
        (RemoteData.fromResult >> decodeResponse membersDecoder >> msg)


membersFilter : String -> Query.GetNodeOptionalArguments -> Query.GetNodeOptionalArguments
membersFilter nid a =
    { a | nameid = Present nid }


membersPayload : SelectionSet TopMemberNode Fractal.Object.Node
membersPayload =
    SelectionSet.succeed TopMemberNode
        |> with (Fractal.Object.Node.createdAt |> SelectionSet.map decodedTime)
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with Fractal.Object.Node.rootnameid
        |> with Fractal.Object.Node.role_type
        |> with
            (Fractal.Object.Node.first_link identity <|
                SelectionSet.map2 User
                    Fractal.Object.User.username
                    Fractal.Object.User.name
            )
        |> hardcoded Nothing
        |> with
            (Fractal.Object.Node.children identity
                (SelectionSet.succeed MemberNode
                    |> with (Fractal.Object.Node.createdAt |> SelectionSet.map decodedTime)
                    |> with Fractal.Object.Node.name
                    |> with Fractal.Object.Node.nameid
                    |> with Fractal.Object.Node.rootnameid
                    |> with Fractal.Object.Node.role_type
                    |> with
                        (Fractal.Object.Node.first_link identity <|
                            SelectionSet.map2 User
                                Fractal.Object.User.username
                                Fractal.Object.User.name
                        )
                    |> hardcoded Nothing
                )
            )
