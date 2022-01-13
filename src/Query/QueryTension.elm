module Query.QueryTension exposing
    ( blobPayload
    , commentPayload
    , eventPayload
    , getTensionBlobs
    , getTensionComments
    , getTensionHead
    , nodeFragmentPayload
    , queryAllTension
    , queryCircleTension
    , queryExtTension
    , queryIntTension
    , tensionHeadPayload
    , tensionPayload
    )

import Dict exposing (Dict)
import Fractal.Enum.BlobOrderable as BlobOrderable
import Fractal.Enum.ContractStatus as ContractStatus
import Fractal.Enum.ContractType as ContractType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionOrderable as TensionOrderable
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.Blob
import Fractal.Object.Comment
import Fractal.Object.CommentAggregateResult
import Fractal.Object.Contract
import Fractal.Object.ContractAggregateResult
import Fractal.Object.Event
import Fractal.Object.Label
import Fractal.Object.Mandate
import Fractal.Object.Node
import Fractal.Object.NodeFragment
import Fractal.Object.Tension
import Fractal.Object.User
import Fractal.Query as Query
import Fractal.Scalar
import Fractal.ScalarCodecs
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import List.Extra exposing (uniqueBy)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Query.QueryNode exposing (emiterOrReceiverPayload, labelPayload, mandatePayload, nidFilter, nodeDecoder, tidPayload, userPayload)
import RemoteData exposing (RemoteData)
import String.Extra as SE



{-
   Get one tension Head/Comments/Blobs/History
-}


nCommentPerTension : Int
nCommentPerTension =
    250


nBlobPerTension : Int
nBlobPerTension =
    50


getTensionHead url tensionid msg =
    makeGQLQuery url
        (Query.getTension { id = encodeId tensionid }
            tensionHeadPayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


getTensionComments url tensionid msg =
    makeGQLQuery url
        (Query.getTension { id = encodeId tensionid }
            tensionCommentsPayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


getTensionBlobs url tensionid msg =
    makeGQLQuery url
        (Query.getTension { id = encodeId tensionid }
            tensionBlobsPayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


tensionHeadPayload : SelectionSet TensionHead Fractal.Object.Tension
tensionHeadPayload =
    SelectionSet.succeed TensionHead
        |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Tension.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.Tension.createdBy identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with Fractal.Object.Tension.title
        |> with Fractal.Object.Tension.type_
        |> with (Fractal.Object.Tension.labels identity labelPayload)
        |> with (Fractal.Object.Tension.assignees identity userPayload)
        |> with (Fractal.Object.Tension.emitter identity emiterOrReceiverPayload)
        |> with (Fractal.Object.Tension.receiver identity emiterOrReceiverPayload)
        |> with Fractal.Object.Tension.action
        |> with Fractal.Object.Tension.status
        |> with
            (Fractal.Object.Tension.blobs
                (\args ->
                    { args
                        | first = Present 1
                        , order =
                            Input.buildBlobOrder
                                (\x -> { x | desc = Present BlobOrderable.CreatedAt })
                                |> Present
                    }
                )
                blobPayload
            )
        |> with
            (Fractal.Object.Tension.contracts
                (\args ->
                    { args
                        | first = Present 1
                        , filter =
                            Input.buildContractFilter
                                (\x -> { x | status = Present { eq = Present ContractStatus.Open, in_ = Absent } })
                                |> Present
                    }
                )
                contractPayloadId
            )
        |> with
            (Fractal.Object.Tension.history
                (\args ->
                    { args
                        | filter =
                            Input.buildEventFilter
                                (\x ->
                                    { x | not = Input.buildEventFilter (\e -> { e | event_type = Present { eq = Present TensionEvent.CommentPushed, in_ = Absent } }) |> Present }
                                )
                                |> Present
                    }
                )
                eventPayload
            )
        -- Aggregate
        |> with
            (SelectionSet.map (\x -> Maybe.map (\y -> y.count) x |> withDefault Nothing) <|
                -- @debug: filter OPEN contract
                Fractal.Object.Tension.contractsAggregate identity
                <|
                    SelectionSet.map Count Fractal.Object.ContractAggregateResult.count
            )


tensionBlobsPayload : SelectionSet TensionBlobs Fractal.Object.Tension
tensionBlobsPayload =
    SelectionSet.succeed TensionBlobs
        |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
        |> with
            (Fractal.Object.Tension.blobs
                (\args ->
                    { args
                        | first = Present nBlobPerTension
                        , order =
                            Input.buildBlobOrder
                                (\b -> { b | desc = Present BlobOrderable.CreatedAt })
                                |> Present
                    }
                )
                blobPayload
            )


tensionCommentsPayload : SelectionSet TensionComments Fractal.Object.Tension
tensionCommentsPayload =
    SelectionSet.succeed TensionComments
        |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
        |> with
            (Fractal.Object.Tension.comments
                (\args -> { args | first = Present nCommentPerTension })
                commentPayload
            )


commentPayload : SelectionSet Comment Fractal.Object.Comment
commentPayload =
    SelectionSet.succeed Comment
        |> with (Fractal.Object.Comment.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Comment.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.Comment.updatedAt |> SelectionSet.map (Maybe.map (\x -> decodedTime x)))
        |> with (Fractal.Object.Comment.createdBy identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with Fractal.Object.Comment.message


blobPayload : SelectionSet Blob Fractal.Object.Blob
blobPayload =
    SelectionSet.succeed Blob
        |> with (Fractal.Object.Blob.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Blob.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.Blob.createdBy identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with Fractal.Object.Blob.blob_type
        |> with (Fractal.Object.Blob.node identity nodeFragmentPayload)
        |> with Fractal.Object.Blob.md
        |> with (Fractal.Object.Blob.pushedFlag |> SelectionSet.map (Maybe.map (\x -> decodedTime x)))


eventPayload : SelectionSet Event Fractal.Object.Event
eventPayload =
    SelectionSet.succeed Event
        |> with (Fractal.Object.Event.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Event.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.Event.createdBy identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with Fractal.Object.Event.event_type
        |> with Fractal.Object.Event.old
        |> with Fractal.Object.Event.new


contractPayloadId : SelectionSet IdPayload Fractal.Object.Contract
contractPayloadId =
    SelectionSet.map IdPayload (Fractal.Object.Contract.id |> SelectionSet.map decodedId)


nodeFragmentPayload : SelectionSet NodeFragment Fractal.Object.NodeFragment
nodeFragmentPayload =
    SelectionSet.succeed NodeFragment
        |> with Fractal.Object.NodeFragment.name
        |> with Fractal.Object.NodeFragment.nameid
        |> with Fractal.Object.NodeFragment.type_
        |> with Fractal.Object.NodeFragment.role_type
        |> with Fractal.Object.NodeFragment.visibility
        |> with Fractal.Object.NodeFragment.mode
        |> with Fractal.Object.NodeFragment.about
        |> with (Fractal.Object.NodeFragment.mandate identity mandatePayload)
        |> with Fractal.Object.NodeFragment.first_link
        |> with
            (Fractal.Object.NodeFragment.children identity
                (SelectionSet.succeed SubNodeFragment
                    |> with Fractal.Object.NodeFragment.name
                    |> with Fractal.Object.NodeFragment.nameid
                    |> with Fractal.Object.NodeFragment.type_
                    |> with Fractal.Object.NodeFragment.role_type
                    |> with Fractal.Object.NodeFragment.visibility
                    |> with Fractal.Object.NodeFragment.mode
                    |> with Fractal.Object.NodeFragment.about
                    |> with (Fractal.Object.NodeFragment.mandate identity mandatePayload)
                    |> with Fractal.Object.NodeFragment.first_link
                )
            )



{-
   Query Circle Tension (all tension at depth 0 or 1 of a given node)
-}


nCircleTensionPpg : Int
nCircleTensionPpg =
    10


type alias NodeTensions =
    { nameid : String
    , tensions_in : Maybe (List Tension)
    , tensions_out : Maybe (List Tension)
    , children : Maybe (List SubNodeTensions)
    }


type alias SubNodeTensions =
    { tensions_in : Maybe (List Tension)
    , tensions_out : Maybe (List Tension)
    }



-- Response Decoder


circleTensionDecoder : Maybe NodeTensions -> Maybe (List Tension)
circleTensionDecoder data =
    data
        |> Maybe.map
            (\node ->
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
                    |> List.take nCircleTensionPpg
                    |> Just
            )
        |> Maybe.withDefault Nothing


subCircleTensionDecoder : SubNodeTensions -> List Tension
subCircleTensionDecoder child =
    let
        tin =
            child.tensions_in |> withDefault []

        tout =
            child.tensions_out |> withDefault []
    in
    tin ++ List.filter (\t -> t.emitter.nameid /= t.receiver.nameid) tout


queryCircleTension url targetid msg =
    --@DEBUG: Archived Nodes are not filtered
    makeGQLQuery url
        (Query.getNode
            (nidFilter targetid)
            circleTensionPayload
        )
        (RemoteData.fromResult >> decodeResponse circleTensionDecoder >> msg)


circleTensionFilter : Fractal.Object.Node.TensionsInOptionalArguments -> Fractal.Object.Node.TensionsInOptionalArguments
circleTensionFilter a =
    { a
        | first = Present (nCircleTensionPpg + nCircleTensionPpg // 2)
        , filter = Input.buildTensionFilter (\x -> { x | status = Present { eq = Present TensionStatus.Open, in_ = Absent } }) |> Present
        , order =
            Input.buildTensionOrder
                (\b -> { b | desc = Present TensionOrderable.CreatedAt })
                |> Present
    }


circleTensionPayload : SelectionSet NodeTensions Fractal.Object.Node
circleTensionPayload =
    SelectionSet.succeed NodeTensions
        |> with Fractal.Object.Node.nameid
        |> with (Fractal.Object.Node.tensions_in circleTensionFilter tensionPayload)
        |> with (Fractal.Object.Node.tensions_out circleTensionFilter tensionPayload)
        |> with
            (Fractal.Object.Node.children identity
                (SelectionSet.succeed SubNodeTensions
                    |> with (Fractal.Object.Node.tensions_in circleTensionFilter tensionPayload)
                    |> with (Fractal.Object.Node.tensions_out circleTensionFilter tensionPayload)
                )
            )


tensionPayload : SelectionSet Tension Fractal.Object.Tension
tensionPayload =
    SelectionSet.succeed Tension
        |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Tension.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.Tension.createdBy identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with Fractal.Object.Tension.title
        |> with Fractal.Object.Tension.type_
        |> with (Fractal.Object.Tension.labels identity labelPayload)
        |> with (Fractal.Object.Tension.emitter identity emiterOrReceiverPayload)
        |> with (Fractal.Object.Tension.receiver identity emiterOrReceiverPayload)
        |> with Fractal.Object.Tension.action
        |> with Fractal.Object.Tension.status
        |> with
            (SelectionSet.map (\x -> Maybe.map (\y -> y.count) x |> withDefault Nothing) <|
                Fractal.Object.Tension.commentsAggregate identity <|
                    SelectionSet.map Count Fractal.Object.CommentAggregateResult.count
            )


tensionPayloadFiltered : List User -> List Label -> SelectionSet Tension Fractal.Object.Tension
tensionPayloadFiltered authors labels =
    SelectionSet.succeed Tension
        |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Tension.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.Tension.createdBy (usersFilter authors) <| SelectionSet.map Username Fractal.Object.User.username)
        |> with Fractal.Object.Tension.title
        |> with Fractal.Object.Tension.type_
        |> with (Fractal.Object.Tension.labels identity labelPayload)
        |> with (Fractal.Object.Tension.emitter identity emiterOrReceiverPayload)
        |> with (Fractal.Object.Tension.receiver identity emiterOrReceiverPayload)
        |> with Fractal.Object.Tension.action
        |> with Fractal.Object.Tension.status
        |> with
            (SelectionSet.map (\x -> Maybe.map (\y -> y.count) x |> withDefault Nothing) <|
                Fractal.Object.Tension.commentsAggregate identity <|
                    SelectionSet.map Count Fractal.Object.CommentAggregateResult.count
            )



{- Match all users -}


usersFilter : List User -> Fractal.Object.Tension.CreatedByOptionalArguments -> Fractal.Object.Tension.CreatedByOptionalArguments
usersFilter authors a =
    { a | filter = matchAllUsers authors }


matchAllUsers : List User -> OptionalArgument Input.UserFilter
matchAllUsers alls =
    List.foldl
        (\x filter ->
            Input.buildUserFilter
                (\d ->
                    let
                        f =
                            case filter of
                                Present y ->
                                    Just y

                                _ ->
                                    Nothing
                    in
                    { d
                        | username = Present { eq = Present x.username, regexp = Absent, in_ = Absent }
                        , and = Present [ f ]
                    }
                )
                |> Present
        )
        Absent
        alls



{-
   Query Regexp Tension
-}


type alias SubNodeTensions2 =
    { tensions_in : Maybe (List Tension)
    , tensions_out : Maybe (List Tension)
    , children : Maybe (List NodeTensions)
    }



-- Response decoder


subTensionDecoder : Maybe (List (Maybe Tension)) -> Maybe (List Tension)
subTensionDecoder data =
    data
        |> Maybe.map
            (\ts ->
                List.filterMap identity ts
            )


queryAllTension url targetids first offset query_ status_ type_ msg =
    makeGQLQuery url
        (Query.queryTension
            (subTensionAllFilterByDate targetids first offset query_ status_ type_)
            tensionPayload
        )
        (RemoteData.fromResult >> decodeResponse subTensionDecoder >> msg)



{- queryIntTension and queryExtTension should support directive query to work with Dgraph....
   https://github.com/dillonkearns/elm-graphql/issues/482
-}


queryIntTension url targetids first offset query_ status_ authors labels type_ msg =
    makeGQLQuery url
        (Query.queryTension
            (subTensionIntFilterByDate targetids first offset query_ status_ type_)
            (tensionPayloadFiltered authors labels)
        )
        (RemoteData.fromResult >> decodeResponse subTensionDecoder >> msg)


queryExtTension url targetids first offset query_ status_ authors labels type_ msg =
    makeGQLQuery url
        (Query.queryTension
            (subTensionExtFilterByDate targetids first offset query_ status_ type_)
            (tensionPayloadFiltered authors labels)
        )
        (RemoteData.fromResult >> decodeResponse subTensionDecoder >> msg)


subTensionAllFilterByDate : List String -> Int -> Int -> Maybe String -> Maybe TensionStatus.TensionStatus -> Maybe TensionType.TensionType -> Query.QueryTensionOptionalArguments -> Query.QueryTensionOptionalArguments
subTensionAllFilterByDate nameids first offset query_ status_ type_ a =
    let
        nameidsRegxp =
            nameids
                |> List.map (\n -> "^" ++ n ++ "$")
                |> String.join "|"
                |> SE.surround "/"
    in
    { a
        | first = Present first
        , offset = Present offset
        , order =
            Input.buildTensionOrder
                (\b -> { b | desc = Present TensionOrderable.CreatedAt })
                |> Present
        , filter =
            Input.buildTensionFilter
                (\c ->
                    { c
                        | status = status_ |> Maybe.map (\s -> { eq = Present s, in_ = Absent }) |> fromMaybe
                        , type_ = type_ |> Maybe.map (\t -> { eq = Present t, in_ = Absent }) |> fromMaybe
                        , and =
                            Present
                                [ Input.buildTensionFilter
                                    (\d1 ->
                                        { d1
                                            | emitterid = { eq = Absent, in_ = Absent, regexp = Present nameidsRegxp } |> Present
                                            , or =
                                                Present
                                                    [ Input.buildTensionFilter
                                                        (\d2 ->
                                                            { d2 | receiverid = { eq = Absent, in_ = Absent, regexp = Present nameidsRegxp } |> Present }
                                                        )
                                                        |> Just
                                                    ]
                                        }
                                    )
                                    |> Just
                                ]
                    }
                )
                |> Present
    }


subTensionIntFilterByDate : List String -> Int -> Int -> Maybe String -> Maybe TensionStatus.TensionStatus -> Maybe TensionType.TensionType -> Query.QueryTensionOptionalArguments -> Query.QueryTensionOptionalArguments
subTensionIntFilterByDate nameids first offset query_ status_ type_ a =
    let
        nameidsRegxp =
            nameids
                |> List.map (\n -> "^" ++ n ++ "$")
                |> String.join "|"
                |> SE.surround "/"
    in
    { a
        | first = Present first
        , offset = Present offset
        , order =
            Input.buildTensionOrder
                (\b -> { b | desc = Present TensionOrderable.CreatedAt })
                |> Present
        , filter =
            Input.buildTensionFilter
                (\c ->
                    { c
                        | status = status_ |> Maybe.map (\s -> { eq = Present s, in_ = Absent }) |> fromMaybe
                        , type_ = type_ |> Maybe.map (\t -> { eq = Present t, in_ = Absent }) |> fromMaybe
                        , emitterid = { eq = Absent, in_ = Absent, regexp = Present nameidsRegxp } |> Present
                        , receiverid = { eq = Absent, in_ = Absent, regexp = Present nameidsRegxp } |> Present
                        , and =
                            query_
                                |> Maybe.map
                                    (\q ->
                                        [ Input.buildTensionFilter
                                            (\d2 ->
                                                { d2
                                                    | title = { alloftext = Absent, anyoftext = Present q } |> Present
                                                    , or =
                                                        Present
                                                            [ Input.buildTensionFilter
                                                                (\d3 ->
                                                                    { d3
                                                                        | message = { alloftext = Absent, anyoftext = Present q } |> Present
                                                                    }
                                                                )
                                                                |> Just
                                                            ]
                                                }
                                            )
                                            |> Just
                                        ]
                                    )
                                |> fromMaybe
                    }
                )
                |> Present
    }


subTensionExtFilterByDate : List String -> Int -> Int -> Maybe String -> Maybe TensionStatus.TensionStatus -> Maybe TensionType.TensionType -> Query.QueryTensionOptionalArguments -> Query.QueryTensionOptionalArguments
subTensionExtFilterByDate nameids first offset query_ status_ type_ a =
    let
        nameidsRegxp =
            nameids
                |> List.map (\n -> "^" ++ n ++ "$")
                |> String.join "|"
                |> SE.surround "/"
    in
    { a
        | first = Present first
        , offset = Present offset
        , order =
            Input.buildTensionOrder
                (\b ->
                    { b | desc = Present TensionOrderable.CreatedAt }
                )
                |> Present
        , filter =
            Input.buildTensionFilter
                (\c ->
                    { c
                        | status = status_ |> Maybe.map (\s -> { eq = Present s, in_ = Absent }) |> fromMaybe
                        , type_ = type_ |> Maybe.map (\t -> { eq = Present t, in_ = Absent }) |> fromMaybe
                        , title = query_ |> Maybe.map (\q -> { alloftext = Absent, anyoftext = Present q }) |> fromMaybe
                        , or =
                            Present
                                [ Input.buildTensionFilter
                                    (\d3 ->
                                        { d3
                                            | message = query_ |> Maybe.map (\q -> { alloftext = Absent, anyoftext = Present q }) |> fromMaybe
                                        }
                                    )
                                    |> Just
                                ]
                        , and =
                            Present
                                [ Input.buildTensionFilter
                                    (\d ->
                                        { d
                                            | receiverid = { eq = Absent, in_ = Absent, regexp = Present nameidsRegxp } |> Present
                                            , not =
                                                Input.buildTensionFilter
                                                    (\e ->
                                                        { e
                                                            | emitterid = { eq = Absent, in_ = Absent, regexp = Present nameidsRegxp } |> Present
                                                        }
                                                    )
                                                    |> Present
                                            , or =
                                                Present
                                                    [ Input.buildTensionFilter
                                                        (\d1 ->
                                                            { d1
                                                                | emitterid = { eq = Absent, in_ = Absent, regexp = Present nameidsRegxp } |> Present
                                                                , not =
                                                                    Input.buildTensionFilter
                                                                        (\e1 ->
                                                                            { e1
                                                                                | receiverid = { eq = Absent, in_ = Absent, regexp = Present nameidsRegxp } |> Present
                                                                            }
                                                                        )
                                                                        |> Present
                                                            }
                                                        )
                                                        |> Just
                                                    ]
                                        }
                                    )
                                    |> Just
                                ]
                    }
                )
                |> Present
    }
