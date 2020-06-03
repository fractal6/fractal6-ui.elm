module ModelSchema exposing (..)

import Debug
import Dict exposing (Dict)
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionOrderable as TensionOrderable
import Fractal.Enum.TensionType as TensionType
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.AddNodePayload
import Fractal.Object.AddTensionPayload
import Fractal.Object.Label
import Fractal.Object.Node
import Fractal.Object.Tension
import Fractal.Object.User
import Fractal.Query as Query
import Fractal.Scalar
import Fractal.ScalarCodecs
import GqlClient exposing (..)
import Graphql.Http
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class, href)
import Iso8601 exposing (fromTime)
import List.Extra exposing (uniqueBy)
import Maybe exposing (withDefault)
import ModelCommon.Uri exposing (FractalBaseRoute, guestIdCodec, uriFromNameid)
import RemoteData exposing (RemoteData)



--
-- Queries Parameters
--


nLabelPerTension : Int
nLabelPerTension =
    3


nTensionPpg : Int
nTensionPpg =
    15



--
-- Frontend Data Structure
--


type alias OrgaData =
    GqlData NodesData


type alias NodesData =
    Dict String Node


type alias CircleTensionsData =
    GqlData TensionsData



-- Remote Data and Sinks


type alias GqlData a =
    RequestResult ErrorData a


type RequestResult errors data
    = Success data
    | Failure errors
    | Loading
    | LoadingSlowly
    | NotAsked


type alias ErrorData =
    List String


type alias Post =
    Dict String String



{-
   Schema Data Structure (fetch/Query)
-}
--
-- User
--


type alias UserCtx =
    { username : String
    , name : Maybe String
    , rights : UserRights
    , roles : List UserRole
    }


type alias UserRights =
    { canLogin : Bool
    , canCreateRoot : Bool
    }


type alias UserRole =
    { rootnameid : String
    , nameid : String
    , name : String
    , role_type : RoleType.RoleType
    }



--
-- Node
--


type alias Node =
    { id : String
    , createdAt : String
    , name : String
    , nameid : String
    , rootnameid : String
    , parent : Maybe ParentNode -- see issue with recursive structure
    , type_ : NodeType.NodeType
    , role_type : Maybe RoleType.RoleType
    , first_link : Maybe FirstLink
    }


type alias ParentNode =
    { nameid : String }



--
-- Tension
--


type alias NodeTensions =
    { tensions_in : Maybe (List Tension)
    , tensions_out : Maybe (List Tension)
    , children : Maybe (List SubNodeTensions)
    }


type alias SubNodeTensions =
    { tensions_in : Maybe (List Tension)
    , tensions_out : Maybe (List Tension)
    }


type alias Tension =
    { id : String
    , createdAt : String
    , title : String
    , type_ : TensionType.TensionType
    , labels : Maybe (List Label)
    , emitter : EmitterOrReceiver
    , receiver : EmitterOrReceiver
    , n_comments : Maybe Int
    }


type alias EmitterOrReceiver =
    { name : String
    , nameid : String
    , type_ : NodeType.NodeType
    , first_link : Maybe FirstLink
    }


type alias FirstLink =
    { username : String }


type alias Label =
    { name : String }



-- Response Data


type alias TensionsData =
    List Tension


type alias TensionsResponse =
    Maybe (List (Maybe Tension))



---------------------------------------
{-
   Query Organisation Nodes
-}
---------------------------------------


nodeOrgaFilter : String -> Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
nodeOrgaFilter rootid a =
    { a
        | filter =
            OptionalArgument.Present
                (Input.buildNodeFilter
                    (\b ->
                        { b | rootnameid = OptionalArgument.Present { eq = OptionalArgument.Present rootid } }
                    )
                )
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
            --(Fractal.Object.Node.parent identity (SelectionSet.map (ParentNode << decodedId) Fractal.Object.Node.id))
            (Fractal.Object.Node.parent identity (SelectionSet.map ParentNode Fractal.Object.Node.nameid))
        |> with Fractal.Object.Node.type_
        |> with Fractal.Object.Node.role_type
        |> with (Fractal.Object.Node.first_link identity (SelectionSet.map FirstLink Fractal.Object.User.username))


fetchNodesOrga rootid msg =
    makeGQLQuery
        (Query.queryNode
            (nodeOrgaFilter rootid)
            nodeOrgaPayload
        )
        (RemoteData.fromResult >> decodeResponse nodeOrgaDecoder >> msg)



---------------------------------------
{-
   Query Circle Tension
-}
---------------------------------------


circleTensionFilter : String -> Query.GetNodeOptionalArguments -> Query.GetNodeOptionalArguments
circleTensionFilter nid a =
    { a | nameid = OptionalArgument.Present nid }


tensionPgFilter : Fractal.Object.Node.TensionsInOptionalArguments -> Fractal.Object.Node.TensionsInOptionalArguments
tensionPgFilter a =
    { a
        | first = OptionalArgument.Present nTensionPpg

        -- we reorder it anyway !
        --, order = OptionalArgument.Present (Input.buildTensionOrder (\b -> { b | desc = OptionalArgument.Present TensionOrderable.CreatedAt }))
    }


tensionPgPayload : SelectionSet Tension Fractal.Object.Tension
tensionPgPayload =
    SelectionSet.succeed Tension
        |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Tension.createdAt |> SelectionSet.map decodedTime)
        |> with Fractal.Object.Tension.title
        |> with Fractal.Object.Tension.type_
        |> with
            (Fractal.Object.Tension.labels
                (\args -> { args | first = OptionalArgument.Present nLabelPerTension })
                (SelectionSet.succeed Label
                    |> with Fractal.Object.Label.name
                )
            )
        |> with
            (Fractal.Object.Tension.emitter identity
                (SelectionSet.succeed EmitterOrReceiver
                    |> with Fractal.Object.Node.name
                    |> with Fractal.Object.Node.nameid
                    |> with Fractal.Object.Node.type_
                    |> with
                        (Fractal.Object.Node.first_link identity (SelectionSet.map FirstLink Fractal.Object.User.username))
                )
            )
        |> with
            (Fractal.Object.Tension.receiver identity
                (SelectionSet.succeed EmitterOrReceiver
                    |> with Fractal.Object.Node.name
                    |> with Fractal.Object.Node.nameid
                    |> with Fractal.Object.Node.type_
                    |> with
                        (Fractal.Object.Node.first_link identity (SelectionSet.map FirstLink Fractal.Object.User.username))
                )
            )
        |> with Fractal.Object.Tension.n_comments


circleTensionPayload : SelectionSet NodeTensions Fractal.Object.Node
circleTensionPayload =
    SelectionSet.succeed NodeTensions
        |> with (Fractal.Object.Node.tensions_in tensionPgFilter tensionPgPayload)
        |> with (Fractal.Object.Node.tensions_out tensionPgFilter tensionPgPayload)
        |> with
            (Fractal.Object.Node.children identity
                (SelectionSet.succeed SubNodeTensions
                    |> with (Fractal.Object.Node.tensions_in tensionPgFilter tensionPgPayload)
                    |> with (Fractal.Object.Node.tensions_out tensionPgFilter tensionPgPayload)
                )
            )


fetchCircleTension targetid msg =
    --@DEBUG: Infered type...
    makeGQLQuery
        (Query.getNode
            (circleTensionFilter targetid)
            circleTensionPayload
        )
        (RemoteData.fromResult >> decodeResponse circleTensionDecoder >> msg)



---------------------------------------
{-
   Query Tension Page
-}
---------------------------------------
--tensionPgFilter : Query.QueryTensionOptionalArguments -> Query.QueryTensionOptionalArguments
--tensionPgFilter a =
--    { a
--        | first = OptionalArgument.Present nTensionPpg
--        , order =
--            OptionalArgument.Present
--                (Input.buildTensionOrder
--                    (\b ->
--                        { b | desc = OptionalArgument.Present TensionOrderable.CreatedAt }
--                    )
--                )
--    }
--
--
--tensionPgPayload : SelectionSet Tension Fractal.Object.Tension
--tensionPgPayload =
--    SelectionSet.succeed Tension
--        |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
--        |> with Fractal.Object.Tension.title
--        |> with Fractal.Object.Tension.type_
--        |> with
--            (Fractal.Object.Tension.labels
--                (\args -> { args | first = OptionalArgument.Present nLabelPerTension })
--                (SelectionSet.succeed Label
--                    |> with Fractal.Object.Label.name
--                )
--            )
--        |> with Fractal.Object.Tension.n_comments
--
--
--fetchTensionsPg msg =
--    makeGQLQuery
--        (Query.queryTension
--            tensionPgFilter
--            tensionPgPayload
--        )
--        (RemoteData.fromResult >> decodeResponse queryDecoder >> msg)
--
---------------------------------------
{-
   Mutation: Add One Tension
-}
---------------------------------------


type alias IdPayload =
    { id : String }


type alias AddTensionPayload =
    { tension : Maybe (List (Maybe IdPayload)) }


addOneTension uctx tension source target msg =
    --@DEBUG: Infered type...
    makeGQLMutation
        (Mutation.addTension
            (tensionInputEncoder uctx tension source target)
            (SelectionSet.map AddTensionPayload <|
                Fractal.Object.AddTensionPayload.tension identity <|
                    (SelectionSet.succeed IdPayload
                        |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
                    )
            )
        )
        (RemoteData.fromResult >> decodeResponse mutationDecoder >> msg)


tensionInputEncoder : UserCtx -> Post -> Node -> Node -> Mutation.AddTensionRequiredArguments
tensionInputEncoder uctx post source target =
    let
        title =
            Dict.get "title" post |> withDefault ""

        type_ =
            Dict.get "type_" post |> withDefault "" |> TensionType.fromString |> withDefault TensionType.Operational

        createdAt =
            Dict.get "createdAt" post |> withDefault ""

        tensionRequired =
            { createdAt = createdAt |> Fractal.Scalar.DateTime
            , createdBy =
                Input.buildUserRef
                    (\x -> { x | username = OptionalArgument.Present uctx.username })
            , title = title
            , type_ = type_
            , emitter =
                Input.buildNodeRef
                    (\x ->
                        { x
                            | nameid = OptionalArgument.Present source.nameid
                            , rootnameid = OptionalArgument.Present source.rootnameid
                        }
                    )
            , receiver =
                Input.buildNodeRef
                    (\x ->
                        { x
                            | nameid = OptionalArgument.Present target.nameid
                            , rootnameid = OptionalArgument.Present target.rootnameid
                        }
                    )
            }
    in
    { input =
        [ Input.buildAddTensionInput tensionRequired identity ]
    }



---------------------------------------
{-
   Mutation: Add a new Role or Cirlce
-}
---------------------------------------


type alias AddNodePayload =
    { node : Maybe (List (Maybe IdPayload)) }


addNewMember uctx post targetid msg =
    --@DEBUG: Infered type...
    makeGQLMutation
        (Mutation.addNode
            (newMemberInputEncoder uctx post targetid)
            (SelectionSet.map AddNodePayload <|
                Fractal.Object.AddNodePayload.node identity <|
                    (SelectionSet.succeed IdPayload
                        |> with (Fractal.Object.Node.id |> SelectionSet.map decodedId)
                    )
            )
        )
        (RemoteData.fromResult >> decodeResponse mutationDecoder >> msg)


newMemberInputEncoder : UserCtx -> Post -> String -> Mutation.AddNodeRequiredArguments
newMemberInputEncoder uctx post targetid =
    let
        createdAt =
            Dict.get "createdAt" post |> withDefault ""

        nodeRequired =
            { createdAt = createdAt |> Fractal.Scalar.DateTime
            , createdBy =
                Input.buildUserRef
                    (\u -> { u | username = OptionalArgument.Present uctx.username })
            , type_ = NodeType.Role
            , nameid = guestIdCodec targetid uctx.username
            , name = "Guest"
            , rootnameid = targetid
            , isRoot = False
            , charach = { userCanJoin = OptionalArgument.Present False, mode = OptionalArgument.Present NodeMode.Coordinated }
            }

        nodeOptional =
            \n ->
                { n
                    | role_type = OptionalArgument.Present RoleType.Guest
                    , parent =
                        Input.buildNodeRef
                            (\p -> { p | nameid = OptionalArgument.Present targetid })
                            |> OptionalArgument.Present
                    , first_link =
                        Input.buildUserRef
                            (\u -> { u | username = OptionalArgument.Present uctx.username })
                            |> OptionalArgument.Present
                }
    in
    { input =
        [ Input.buildAddNodeInput nodeRequired nodeOptional ]
    }



--
-- Response decoder
--


decodeResponse decoder response =
    --@DEBUG: Infered type...
    {-
       This decoder take two generic type of data:
       * `dataResponse` which is directlty related to Graphql data returned by the server.
       * `dataDecoded` which is the data Model used in Elm code.
       @DEBUG: how to set the universal type in the function signature.
    -}
    case response of
        RemoteData.Failure errors ->
            case errors of
                Graphql.Http.GraphqlError maybeParsedData err ->
                    -- Graphql error
                    err
                        |> List.map (\e -> e.message)
                        |> Failure

                Graphql.Http.HttpError httpError ->
                    [ "Http error: " ++ Debug.toString httpError ]
                        |> Failure

        RemoteData.Loading ->
            Loading

        RemoteData.NotAsked ->
            NotAsked

        RemoteData.Success data ->
            decoder data |> Success


circleTensionDecoder : Maybe NodeTensions -> List Tension
circleTensionDecoder data =
    case data of
        Just node ->
            let
                tin =
                    node.tensions_in |> withDefault []

                tout =
                    -- Empty for now (automatic tensions ?)
                    node.tensions_out |> withDefault []

                tchild =
                    node.children |> withDefault [] |> List.map subCircleTensionDecoder |> List.concat
            in
            List.sortBy .createdAt (tchild ++ tin ++ List.filter (\t -> t.emitter.nameid /= t.receiver.nameid) tout)
                |> List.reverse
                |> uniqueBy (\t -> t.id)
                |> List.take nTensionPpg

        Nothing ->
            []


subCircleTensionDecoder : SubNodeTensions -> List Tension
subCircleTensionDecoder child =
    let
        tin =
            child.tensions_in |> withDefault []

        tout =
            child.tensions_out |> withDefault []
    in
    tin ++ List.filter (\t -> t.emitter.nameid /= t.receiver.nameid) tout


nodeOrgaDecoder : Maybe (List (Maybe Node)) -> Dict String Node
nodeOrgaDecoder data =
    case data of
        Just d ->
            if List.length d == 0 then
                Dict.empty

            else
                d
                    |> List.filterMap identity
                    |> List.map (\n -> ( n.nameid, n ))
                    |> Dict.fromList

        Nothing ->
            Dict.empty


queryDecoder : Maybe (List (Maybe a)) -> List a
queryDecoder data =
    -- Convert empty data to empty list
    -- Standard decoder to get list of result from a gql query
    case data of
        Just d ->
            if List.length d == 0 then
                []

            else
                List.filterMap identity d

        Nothing ->
            []


mutationDecoder : Maybe a -> Maybe a
mutationDecoder =
    identity



--
-- Utils
--


decodedId : Fractal.ScalarCodecs.Id -> String
decodedId (Fractal.Scalar.Id id) =
    id


decodedTime : Fractal.ScalarCodecs.DateTime -> String
decodedTime (Fractal.Scalar.DateTime time) =
    time


tensionTypeColor : String -> TensionType.TensionType -> String
tensionTypeColor elt tt =
    case tt of
        TensionType.Governance ->
            "has-" ++ elt ++ "-info"

        TensionType.Operational ->
            "has-" ++ elt ++ "-success"

        TensionType.Personal ->
            "has-" ++ elt ++ "-warning"

        TensionType.Help ->
            "has-" ++ elt ++ "-link"



--
-- View
--
{-
   Tension
-}


tensionTypeSpan : String -> String -> Post -> Html msg
tensionTypeSpan cls elt post =
    let
        maybeTTypeString =
            Dict.get "type_" post

        ttDecoded =
            case maybeTTypeString of
                Just ts ->
                    case TensionType.fromString ts of
                        Just td ->
                            Just ( ts, td )

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing
    in
    case ttDecoded of
        Just tt ->
            span [ class <| cls ++ " " ++ tensionTypeColor elt (Tuple.second tt) ] [ text (Tuple.first tt) ]

        Nothing ->
            span [ class "" ] [ text "Unknown" ]


tensionTypeArrow : String -> Html msg -> Html msg -> List (Html msg)
tensionTypeArrow cls source target =
    [ span [ class <| cls ++ " is-small is-light is-inverted is-static" ] [ source ]
    , span [ class <| "right-arrow" ] []
    , span [ class <| cls ++ " is-small is-light is-inverted is-static" ] [ target ]
    ]



{-
   Node
-}


viewNodeRef : FractalBaseRoute -> EmitterOrReceiver -> Html msg
viewNodeRef baseUri n =
    case n.type_ of
        NodeType.Circle ->
            a [ href (uriFromNameid baseUri n.nameid) ] [ n.name |> text ]

        NodeType.Role ->
            a [ href (uriFromNameid baseUri n.nameid) ] [ n.name |> text ]
