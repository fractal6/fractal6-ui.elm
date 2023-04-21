{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2023 Fractale Co

   This file is part of Fractale.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
-}


module Query.QueryTension exposing
    ( blobPayload
    , commentPayload
    , eventPayload
    , getTensionBlobs
    , getTensionComments
    , getTensionHead
    , nodeFragmentPayload
    , queryAllTension
    , queryAssignedTensions
    , queryCircleTension
    , queryExtTension
    , queryIntTension
    , queryPinnedTensions
    , tensionPayload
    )

import Bulk.Codecs exposing (nid2rootid)
import Dict exposing (Dict)
import Extra exposing (ternary, unwrap2)
import Fractal.Enum.BlobOrderable as BlobOrderable
import Fractal.Enum.ContractStatus as ContractStatus
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionOrderable as TensionOrderable
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Fractal.InputObject as Input
import Fractal.Object
import Fractal.Object.Blob
import Fractal.Object.Comment
import Fractal.Object.CommentAggregateResult
import Fractal.Object.Contract
import Fractal.Object.ContractAggregateResult
import Fractal.Object.Event
import Fractal.Object.Node
import Fractal.Object.NodeFragment
import Fractal.Object.Reaction
import Fractal.Object.Tension
import Fractal.Object.User
import Fractal.Query as Query
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import List.Extra exposing (uniqueBy)
import Maybe exposing (withDefault)
import ModelSchema exposing (Blob, Comment, Count, EmitterOrReceiver, Event, IdPayload, Label, MentionedTension, NodeFragment, PinTension, Reaction, Tension, TensionBlobs, TensionComments, TensionHead, User, UserCtx, Username, decodeResponse, decodedId, decodedTime, encodeId)
import Query.QueryNode exposing (emiterOrReceiverPayload, emiterOrReceiverWithPinPayload, labelPayload, mandatePayload, nidFilter, pinPayload, userPayload)
import RemoteData
import String.Extra as SE



--
-- Get one tension Head/Comments/Blobs/History
--


nCommentPerTension : Int
nCommentPerTension =
    250


nBlobPerTension : Int
nBlobPerTension =
    50


getTensionHead url uctx tensionid msg =
    makeGQLQuery url
        (Query.getTension { id = encodeId tensionid }
            (tensionHeadPayload tensionid uctx)
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


tensionHeadPayload : String -> UserCtx -> SelectionSet TensionHead Fractal.Object.Tension
tensionHeadPayload tid uctx =
    SelectionSet.succeed
        (\a b c d e f g h i j k l m n o p ->
            let
                eor =
                    EmitterOrReceiver h.name h.nameid h.role_type h.color

                isPinned =
                    h.pinned == Just [ { id = tid } ]
            in
            TensionHead a b c d e f g eor i j k isPinned m n o p
        )
        |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Tension.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.Tension.createdBy identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with Fractal.Object.Tension.title
        |> with Fractal.Object.Tension.type_
        |> with (Fractal.Object.Tension.labels identity labelPayload)
        |> with (Fractal.Object.Tension.assignees identity userPayload)
        --|> with (Fractal.Object.Tension.emitter identity emiterOrReceiverPayload)
        |> with (Fractal.Object.Tension.receiver identity (emiterOrReceiverWithPinPayload tid))
        |> with Fractal.Object.Tension.action
        |> with Fractal.Object.Tension.status
        |> (\x ->
                case uctx.username of
                    "" ->
                        hardcoded False x

                    username ->
                        with
                            (Fractal.Object.Tension.subscribers
                                (\a ->
                                    { a
                                        | filter =
                                            Input.buildUserFilter
                                                (\d ->
                                                    { d | username = Present { eq = Present username, regexp = Absent, in_ = Absent } }
                                                )
                                                |> Present
                                    }
                                )
                                (SelectionSet.map identity Fractal.Object.User.username)
                                |> SelectionSet.map (Maybe.map (\y -> List.length y > 0) >> withDefault False)
                            )
                            x
           )
        |> hardcoded False
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
            (SelectionSet.map (\x -> unwrap2 0 .count x) <|
                Fractal.Object.Tension.contractsAggregate (\a -> { a | filter = Present <| Input.buildContractFilter (\x -> { x | status = Present { eq = Present ContractStatus.Open, in_ = Absent } }) }) <|
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
        |> with (Fractal.Object.Comment.createdBy identity (SelectionSet.map Username Fractal.Object.User.username))
        |> with Fractal.Object.Comment.message
        |> with
            -- Aggregate Reactions
            (Fractal.Object.Comment.reactions identity
                (SelectionSet.map2 (\x y -> { type_ = x, user = y.username })
                    Fractal.Object.Reaction.type_
                    (Fractal.Object.Reaction.user identity (SelectionSet.map Username Fractal.Object.User.username))
                )
                |> SelectionSet.map
                    (\x ->
                        let
                            addParam : { type_ : Int, user : String } -> Maybe Reaction -> Maybe Reaction
                            addParam { type_, user } maybeValue =
                                case maybeValue of
                                    Just value ->
                                        Just { value | users = user :: value.users }

                                    Nothing ->
                                        Just { type_ = type_, users = [ user ] }
                        in
                        withDefault [] x
                            |> List.foldr (\r dict -> Dict.update r.type_ (addParam r) dict)
                                Dict.empty
                            |> Dict.values
                    )
            )


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
        |> with
            (Fractal.Object.Event.mentioned identity <|
                SelectionSet.map4 MentionedTension
                    (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
                    Fractal.Object.Tension.status
                    Fractal.Object.Tension.title
                    Fractal.Object.Tension.receiverid
            )


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
        |> with Fractal.Object.NodeFragment.role_ext
        |> with Fractal.Object.NodeFragment.color
        |> with Fractal.Object.NodeFragment.visibility
        |> with Fractal.Object.NodeFragment.mode
        |> with Fractal.Object.NodeFragment.about
        |> with (Fractal.Object.NodeFragment.mandate identity mandatePayload)
        |> with (Fractal.Object.NodeFragment.first_link |> SelectionSet.map (Maybe.map (\x -> ternary (x == "") Nothing (Just x)) >> withDefault Nothing))



--
-- Query Circle Tension (all tension at depth 0 or 1 of a given node)
--


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
                        node.children |> withDefault [] |> List.concatMap subCircleTensionDecoder
                in
                (tin ++ tout ++ tchild)
                    |> uniqueBy .id
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
    tin ++ tout


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
        --|> with (Fractal.Object.Tension.emitter identity emiterOrReceiverPayload)
        |> with (Fractal.Object.Tension.receiver identity emiterOrReceiverPayload)
        |> with Fractal.Object.Tension.action
        |> with Fractal.Object.Tension.status
        |> with
            (SelectionSet.map (\x -> Maybe.map (\y -> y.count) x |> withDefault Nothing) <|
                Fractal.Object.Tension.commentsAggregate identity <|
                    SelectionSet.map Count Fractal.Object.CommentAggregateResult.count
            )
        |> hardcoded Nothing


tensionPayloadFiltered : List User -> List Label -> SelectionSet Tension Fractal.Object.Tension
tensionPayloadFiltered authors labels =
    SelectionSet.succeed Tension
        |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Tension.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.Tension.createdBy (usersFilter authors) <| SelectionSet.map Username Fractal.Object.User.username)
        |> with Fractal.Object.Tension.title
        |> with Fractal.Object.Tension.type_
        |> with (Fractal.Object.Tension.labels identity labelPayload)
        --|> with (Fractal.Object.Tension.emitter identity emiterOrReceiverPayload)
        |> with (Fractal.Object.Tension.receiver identity emiterOrReceiverPayload)
        |> with Fractal.Object.Tension.action
        |> with Fractal.Object.Tension.status
        |> with
            (SelectionSet.map (\x -> Maybe.map (\y -> y.count) x |> withDefault Nothing) <|
                Fractal.Object.Tension.commentsAggregate identity <|
                    SelectionSet.map Count Fractal.Object.CommentAggregateResult.count
            )
        |> hardcoded Nothing



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



--
-- Query Regexp Tension
--


type alias SubNodeTensions2 =
    { tensions_in : Maybe (List Tension)
    , tensions_out : Maybe (List Tension)
    , children : Maybe (List NodeTensions)
    }


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
                                            | receiverid = Present { eq = Absent, regexp = Absent, in_ = List.map Just nameids |> Present }
                                            , or =
                                                Present
                                                    [ Input.buildTensionFilter
                                                        (\d2 ->
                                                            { d2 | emitterid = Present { eq = Absent, regexp = Absent, in_ = List.map Just nameids |> Present } }
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
                        , emitterid = { eq = Absent, regexp = Absent, in_ = List.map Just nameids |> Present } |> Present
                        , receiverid = { eq = Absent, regexp = Absent, in_ = List.map Just nameids |> Present } |> Present
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
                                            | receiverid = { eq = Absent, regexp = Absent, in_ = List.map Just nameids |> Present } |> Present
                                            , not =
                                                Input.buildTensionFilter
                                                    (\e ->
                                                        { e
                                                            | emitterid = { eq = Absent, regexp = Absent, in_ = List.map Just nameids |> Present } |> Present
                                                        }
                                                    )
                                                    |> Present
                                            , or =
                                                Present
                                                    [ Input.buildTensionFilter
                                                        (\d1 ->
                                                            { d1
                                                                | emitterid = { eq = Absent, regexp = Absent, in_ = List.map Just nameids |> Present } |> Present
                                                                , not =
                                                                    Input.buildTensionFilter
                                                                        (\e1 ->
                                                                            { e1
                                                                                | receiverid = { eq = Absent, regexp = Absent, in_ = List.map Just nameids |> Present } |> Present
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



--
-- Query tension assigned to user
--


type alias AssignedTensions =
    { username : String
    , tensions_assigned : Maybe (List Tension)
    }


assignedTensionDecoder : Maybe AssignedTensions -> Maybe (Dict String (List Tension))
assignedTensionDecoder data =
    --
    -- Convert a list of tension into a Dict of tension by Receiverid
    --
    let
        addParam : Tension -> Maybe (List Tension) -> Maybe (List Tension)
        addParam value maybeValues =
            case maybeValues of
                Just values ->
                    Just (values ++ [ value ])

                Nothing ->
                    Just [ value ]

        toDict2 : List ( String, Tension ) -> Dict String (List Tension)
        toDict2 parameters =
            List.foldl
                (\( k, v ) dict -> Dict.update k (addParam v) dict)
                Dict.empty
                parameters
    in
    data
        |> Maybe.map
            (\x ->
                withDefault [] x.tensions_assigned
                    --|> List.sortBy .createdAt
                    |> List.map (\y -> ( nid2rootid y.receiver.nameid, y ))
                    |> toDict2
            )


queryAssignedTensions url form msg =
    --@DEBUG: Archived Nodes are not filtered
    makeGQLQuery url
        (Query.getUser
            (\a -> { a | username = Present form.uctx.username })
            (assignedTensionsPayload form.first)
        )
        (RemoteData.fromResult >> decodeResponse assignedTensionDecoder >> msg)


assignedTensionsPayload : Int -> SelectionSet AssignedTensions Fractal.Object.User
assignedTensionsPayload first =
    SelectionSet.succeed AssignedTensions
        |> with Fractal.Object.User.username
        |> with
            (Fractal.Object.User.tensions_assigned
                (\a ->
                    { a
                        | first = Present first
                        , filter = Input.buildTensionFilter (\x -> { x | status = Present { eq = Present TensionStatus.Open, in_ = Absent } }) |> Present
                        , order =
                            Input.buildTensionOrder
                                (\b -> { b | desc = Present TensionOrderable.CreatedAt })
                                |> Present
                    }
                )
                tensionPayload
            )



--
-- Query tension pinned
--


type alias PinnedTensions =
    { pinned : Maybe (List PinTension)
    }


pinnedTensionDecoder : Maybe PinnedTensions -> Maybe (Maybe (List PinTension))
pinnedTensionDecoder data =
    data
        |> Maybe.map .pinned


queryPinnedTensions url nameid msg =
    makeGQLQuery url
        (Query.getNode
            (\a -> { a | nameid = Present nameid })
            pinnedTensionsPayload
        )
        (RemoteData.fromResult >> decodeResponse pinnedTensionDecoder >> msg)


pinnedTensionsPayload : SelectionSet PinnedTensions Fractal.Object.Node
pinnedTensionsPayload =
    SelectionSet.map PinnedTensions
        (Fractal.Object.Node.pinned identity pinPayload)
