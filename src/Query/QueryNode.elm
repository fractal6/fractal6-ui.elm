module Query.QueryNode exposing
    ( MemberNode
    , NodeExt
    , blobIdPayload
    , emiterOrReceiverPayload
    , fetchNode
    , labelFullPayload
    , labelPayload
    , nodeCharacPayload
    , nodeIdPayload
    , nodeOrgaPayload
    , queryFocusNode
    , queryGraphPack
    , queryLabels
    , queryLabelsUp
    , queryLocalGraph
    , queryMembers
    , queryMembersTop
    , queryNodeExt
    , queryNodesSub
    , queryPublicOrga
    , userPayload
    )

import Dict exposing (Dict)
import Fractal.Enum.LabelOrderable as LabelOrderable
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.InputObject as Input
import Fractal.Object
import Fractal.Object.Blob
import Fractal.Object.Label
import Fractal.Object.Node
import Fractal.Object.NodeCharac
import Fractal.Object.NodeStats
import Fractal.Object.Tension
import Fractal.Object.User
import Fractal.Query as Query
import Fractal.Scalar
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon.Codecs exposing (nid2rootid)
import ModelSchema exposing (..)
import RemoteData exposing (RemoteData)
import String.Extra as SE



{-
   Query Public Orga / Explore
-}
--- Response decoder


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
                        | isRoot = Present True
                        , isPrivate = Present False
                        , not = Input.buildNodeFilter (\c -> { c | isPersonal = Present True }) |> Present
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
        |> with (Fractal.Object.Node.parent identity nodeIdPayload)
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
                        , not = Input.buildNodeFilter (\sd -> { sd | isArchived = Present True, or = matchAnyRoleType [ RoleType.Retired ] }) |> Present
                    }
                )
                |> Present
    }


matchAnyRoleType : List RoleType.RoleType -> OptionalArgument Input.NodeFilter
matchAnyRoleType alls =
    List.foldl
        (\x filter ->
            Input.buildNodeFilter
                (\d ->
                    { d
                        | role_type = Present { eq = Present x }
                        , or = filter
                    }
                )
                |> Present
        )
        Absent
        alls


nodeOrgaPayload : SelectionSet Node Fractal.Object.Node
nodeOrgaPayload =
    SelectionSet.succeed Node
        |> with (Fractal.Object.Node.createdAt |> SelectionSet.map decodedTime)
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with Fractal.Object.Node.rootnameid
        |> with (Fractal.Object.Node.parent identity nodeIdPayload)
        |> with Fractal.Object.Node.type_
        |> with Fractal.Object.Node.role_type
        |> with (Fractal.Object.Node.first_link identity userPayload)
        |> with (Fractal.Object.Node.charac identity nodeCharacPayload)
        |> with Fractal.Object.Node.isPrivate
        |> with
            (Fractal.Object.Node.source identity blobIdPayload)


nodeIdPayload : SelectionSet NodeId Fractal.Object.Node
nodeIdPayload =
    SelectionSet.map2 NodeId
        Fractal.Object.Node.nameid
        Fractal.Object.Node.isPrivate


blobIdPayload : SelectionSet BlobId Fractal.Object.Blob
blobIdPayload =
    SelectionSet.succeed BlobId
        |> with (Fractal.Object.Blob.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Blob.tension identity (SelectionSet.succeed IdPayload |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)))


userPayload : SelectionSet User Fractal.Object.User
userPayload =
    SelectionSet.map2 User
        Fractal.Object.User.username
        Fractal.Object.User.name


nodeCharacPayload : SelectionSet NodeCharac Fractal.Object.NodeCharac
nodeCharacPayload =
    SelectionSet.map2 NodeCharac
        Fractal.Object.NodeCharac.userCanJoin
        Fractal.Object.NodeCharac.mode



{-
   Get Node
-}


fetchNode url nid msg =
    makeGQLQuery url
        (Query.getNode
            (nidFilter nid)
            nodeOrgaPayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


nidFilter : String -> Query.GetNodeOptionalArguments -> Query.GetNodeOptionalArguments
nidFilter nid a =
    { a | nameid = Present nid }



{-
   Query FocusNode
-}


focusDecoder : Maybe LocalNode -> Maybe FocusNode
focusDecoder data =
    data
        |> Maybe.map
            (\n ->
                FocusNode n.name n.nameid n.type_ n.charac (n.children |> withDefault []) n.isPrivate
            )


queryFocusNode url nid msg =
    makeGQLQuery url
        (Query.getNode
            (nidFilter nid)
            lgPayload
        )
        (RemoteData.fromResult >> decodeResponse focusDecoder >> msg)



{-
   Query Local Graph / Path Data
-}
--- Response decoder


type alias LocalNode =
    { name : String
    , nameid : String
    , type_ : NodeType.NodeType
    , charac : NodeCharac
    , children : Maybe (List EmitterOrReceiver)
    , parent : Maybe LocalRootNode
    , isPrivate : Bool
    }


type alias LocalRootNode =
    { name : String
    , nameid : String
    , charac : NodeCharac
    , isPrivate : Bool
    , isRoot : Bool
    }


emiterOrReceiverPayload : SelectionSet EmitterOrReceiver Fractal.Object.Node
emiterOrReceiverPayload =
    SelectionSet.succeed EmitterOrReceiver
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with Fractal.Object.Node.role_type
        |> with (Fractal.Object.Node.charac identity nodeCharacPayload)
        |> with Fractal.Object.Node.isPrivate


lgDecoder : Maybe LocalNode -> Maybe LocalGraph
lgDecoder data =
    data
        |> Maybe.map
            (\n ->
                case n.parent of
                    Just p ->
                        let
                            focus =
                                FocusNode n.name n.nameid n.type_ n.charac (withDefault [] n.children) n.isPrivate

                            path =
                                [ PNode p.name p.nameid p.charac p.isPrivate, PNode n.name n.nameid n.charac n.isPrivate ]
                        in
                        if p.isRoot then
                            { root = PNode p.name p.nameid p.charac p.isPrivate |> Just
                            , path = path
                            , focus = focus
                            }

                        else
                            -- partial path
                            { root = Nothing, path = path, focus = focus }

                    Nothing ->
                        -- Assume Root node
                        { root = PNode n.name n.nameid n.charac n.isPrivate |> Just
                        , path = [ PNode n.name n.nameid n.charac n.isPrivate ]
                        , focus = FocusNode n.name n.nameid n.type_ n.charac (withDefault [] n.children) n.isPrivate
                        }
            )


queryLocalGraph url nid msg =
    makeGQLQuery url
        (Query.getNode
            (nidFilter nid)
            lgPayload
        )
        (RemoteData.fromResult >> decodeResponse lgDecoder >> msg)


lgPayload : SelectionSet LocalNode Fractal.Object.Node
lgPayload =
    SelectionSet.succeed LocalNode
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with Fractal.Object.Node.type_
        |> with (Fractal.Object.Node.charac identity nodeCharacPayload)
        |> with
            (Fractal.Object.Node.children nArchivedFilter emiterOrReceiverPayload)
        |> with
            (Fractal.Object.Node.parent identity lg2Payload)
        |> with Fractal.Object.Node.isPrivate


lg2Payload : SelectionSet LocalRootNode Fractal.Object.Node
lg2Payload =
    SelectionSet.succeed LocalRootNode
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with (Fractal.Object.Node.charac identity nodeCharacPayload)
        |> with Fractal.Object.Node.isPrivate
        |> with Fractal.Object.Node.isRoot


nArchivedFilter : Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
nArchivedFilter a =
    { a
        | filter =
            Input.buildNodeFilter
                (\b ->
                    { b | not = Input.buildNodeFilter (\sd -> { sd | isArchived = Present True, or = matchAnyRoleType [ RoleType.Retired ] }) |> Present }
                )
                |> Present
    }



{-
   Query  Members
-}
--- Response decoder


type alias NodeMembers =
    -- isPrivate and nameid need in the backend to check if the ressource is hidden
    { first_link : Maybe User, isPrivate : Bool, nameid : String }


membersDecoder : Maybe (List (Maybe NodeMembers)) -> Maybe (List User)
membersDecoder data =
    data
        |> Maybe.map
            (\d ->
                if List.length d == 0 then
                    Nothing

                else
                    d
                        |> List.filterMap identity
                        |> List.map (\x -> x.first_link)
                        |> List.filterMap identity
                        |> Just
            )
        |> withDefault Nothing


queryMembers url nids msg =
    let
        rootid =
            nids |> LE.last |> withDefault "" |> nid2rootid
    in
    makeGQLQuery url
        (Query.queryNode
            (membersFilter rootid)
            membersPayload
        )
        (RemoteData.fromResult >> decodeResponse membersDecoder >> msg)


membersFilter : String -> Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
membersFilter rootid a =
    { a
        | filter =
            Input.buildNodeFilter
                (\c ->
                    { c
                        | rootnameid = Present { eq = Present rootid, regexp = Absent }
                        , and = matchAnyRoleType [ RoleType.Member, RoleType.Owner, RoleType.Guest ]
                    }
                )
                |> Present
    }


membersPayload : SelectionSet NodeMembers Fractal.Object.Node
membersPayload =
    SelectionSet.succeed NodeMembers
        |> with
            (Fractal.Object.Node.first_link identity <|
                SelectionSet.map2 User
                    Fractal.Object.User.username
                    Fractal.Object.User.name
            )
        |> with Fractal.Object.Node.isPrivate
        |> with Fractal.Object.Node.nameid



{-
   Query Top  Members
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
    , isPrivate : Bool
    }


type alias MemberNode =
    { createdAt : String
    , name : String
    , nameid : String
    , rootnameid : String
    , role_type : Maybe RoleType.RoleType
    , first_link : Maybe User
    , parent : Maybe NodeId
    , isPrivate : Bool
    }


membersTopDecoder : Maybe TopMemberNode -> Maybe (List Member)
membersTopDecoder data =
    let
        n2r n =
            UserRoleExtended n.name n.nameid n.rootnameid (withDefault RoleType.Guest n.role_type) n.createdAt n.parent n.isPrivate
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


queryMembersTop url nid msg =
    makeGQLQuery url
        (Query.getNode
            (nidFilter nid)
            membersTopPayload
        )
        (RemoteData.fromResult >> decodeResponse membersTopDecoder >> msg)


membersTopPayload : SelectionSet TopMemberNode Fractal.Object.Node
membersTopPayload =
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
            (Fractal.Object.Node.children nArchivedFilter
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
                    |> with Fractal.Object.Node.isPrivate
                )
            )
        |> with Fractal.Object.Node.isPrivate



{-
   Query Labels (Full)
-}


type alias NodeLabelsFull =
    { labels : Maybe (List LabelFull), isPrivate : Bool }


labelsDecoder : Maybe NodeLabelsFull -> Maybe (List LabelFull)
labelsDecoder data =
    data
        |> Maybe.map (\d -> withDefault [] d.labels)


queryLabels url nid msg =
    makeGQLQuery url
        (Query.getNode
            (nidFilter nid)
            nodeLabelsPayload
        )
        (RemoteData.fromResult >> decodeResponse labelsDecoder >> msg)


nodeLabelsPayload : SelectionSet NodeLabelsFull Fractal.Object.Node
nodeLabelsPayload =
    SelectionSet.map2 NodeLabelsFull
        (Fractal.Object.Node.labels
            (\args ->
                { args
                    | order =
                        Input.buildLabelOrder (\b -> { b | asc = Present LabelOrderable.Name })
                            |> Present
                }
            )
            labelFullPayload
        )
        Fractal.Object.Node.isPrivate


labelPayload : SelectionSet Label Fractal.Object.Label
labelPayload =
    SelectionSet.map3 Label
        (Fractal.Object.Label.id |> SelectionSet.map decodedId)
        Fractal.Object.Label.name
        Fractal.Object.Label.color


labelFullPayload : SelectionSet LabelFull Fractal.Object.Label
labelFullPayload =
    SelectionSet.map5 LabelFull
        (Fractal.Object.Label.id |> SelectionSet.map decodedId)
        Fractal.Object.Label.name
        Fractal.Object.Label.color
        Fractal.Object.Label.description
        Fractal.Object.Label.n_nodes



{-
   Query Labels (Up)
-}


type alias NodeLabels =
    -- isPrivate and nameid need in the backend to check if the ressource is hidden
    { labels : Maybe (List Label), isPrivate : Bool, nameid : String }


labelsUpDecoder : Maybe (List (Maybe NodeLabels)) -> Maybe (List Label)
labelsUpDecoder data =
    data
        |> Maybe.map
            (\d ->
                if List.length d == 0 then
                    Nothing

                else
                    d
                        |> List.filterMap identity
                        |> List.map (\x -> x.labels |> withDefault [])
                        |> List.concat
                        |> Just
            )
        |> withDefault Nothing


queryLabelsUp url nids msg =
    makeGQLQuery url
        (Query.queryNode
            (nidUpFilter nids)
            nodeLabelsUpPayload
        )
        (RemoteData.fromResult >> decodeResponse labelsUpDecoder >> msg)


nidUpFilter : List String -> Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
nidUpFilter nids a =
    let
        nameidsRegxp =
            nids
                |> List.map (\n -> "^" ++ n ++ "$")
                |> String.join "|"
                |> SE.surround "/"
    in
    { a
        | filter =
            Input.buildNodeFilter
                (\c ->
                    { c | nameid = Present { eq = Absent, regexp = Present nameidsRegxp } }
                )
                |> Present
    }


nodeLabelsUpPayload : SelectionSet NodeLabels Fractal.Object.Node
nodeLabelsUpPayload =
    SelectionSet.map3 NodeLabels
        (Fractal.Object.Node.labels
            (\args ->
                { args
                    | order =
                        Input.buildLabelOrder (\b -> { b | asc = Present LabelOrderable.Name })
                            |> Present
                }
            )
            labelPayload
        )
        Fractal.Object.Node.isPrivate
        Fractal.Object.Node.nameid
