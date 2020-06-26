module Query.QueryTension exposing (getTension, queryCircleTension, queryExtTension, queryIntTension, tensionExtendedPayload, tensionPayload)

import Dict exposing (Dict)
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionOrderable as TensionOrderable
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.Comment
import Fractal.Object.Label
import Fractal.Object.Node
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
import RemoteData exposing (RemoteData)



{-
   Get one tension
-}


nCommentPerTension : Int
nCommentPerTension =
    100


getTension tensionid msg =
    makeGQLQuery
        (Query.getTension { id = encodeId tensionid }
            tensionExtendedPayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


tensionExtendedPayload : SelectionSet TensionExtended Fractal.Object.Tension
tensionExtendedPayload =
    SelectionSet.succeed TensionExtended
        |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Tension.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.Tension.createdBy identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with Fractal.Object.Tension.title
        |> with Fractal.Object.Tension.type_
        |> with
            (Fractal.Object.Tension.labels
                (\args -> { args | first = Present nLabelPerTension })
                (SelectionSet.map Label Fractal.Object.Label.name)
            )
        |> with
            (Fractal.Object.Tension.emitter identity
                (SelectionSet.succeed EmitterOrReceiver
                    |> with Fractal.Object.Node.name
                    |> with Fractal.Object.Node.nameid
                    |> with Fractal.Object.Node.role_type
                )
            )
        |> with
            (Fractal.Object.Tension.receiver identity
                (SelectionSet.succeed EmitterOrReceiver
                    |> with Fractal.Object.Node.name
                    |> with Fractal.Object.Node.nameid
                    |> with Fractal.Object.Node.role_type
                )
            )
        |> with Fractal.Object.Tension.action
        |> with Fractal.Object.Tension.status
        |> with Fractal.Object.Tension.message
        |> with
            (Fractal.Object.Tension.comments
                (\args -> { args | first = Present nCommentPerTension })
                (SelectionSet.succeed Comment
                    |> with (Fractal.Object.Comment.createdAt |> SelectionSet.map decodedTime)
                    |> with (Fractal.Object.Comment.createdBy identity <| SelectionSet.map Username Fractal.Object.User.username)
                    |> with Fractal.Object.Comment.message
                )
            )
        |> with Fractal.Object.Tension.n_comments



{-
   Query Circle Tension (all tension at depth 0 or 1 of a given node)
-}


nLabelPerTension : Int
nLabelPerTension =
    3


nCircleTensionPpg : Int
nCircleTensionPpg =
    15


type alias NodeTensions =
    { tensions_in : Maybe (List Tension)
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


queryCircleTension targetid msg =
    --@DEBUG: Infered type...
    makeGQLQuery
        (Query.getNode
            (circleFilter targetid)
            circleTensionPayload
        )
        (RemoteData.fromResult >> decodeResponse circleTensionDecoder >> msg)


circleFilter : String -> Query.GetNodeOptionalArguments -> Query.GetNodeOptionalArguments
circleFilter nid a =
    { a | nameid = Present nid }


circleTensionFilter : Fractal.Object.Node.TensionsInOptionalArguments -> Fractal.Object.Node.TensionsInOptionalArguments
circleTensionFilter a =
    { a
        | first = Present nCircleTensionPpg
        , filter = Input.buildTensionFilter (\x -> { x | status = Present { eq = TensionStatus.Open } }) |> Present

        -- we reorder it anyway !
    }


tensionPayload : SelectionSet Tension Fractal.Object.Tension
tensionPayload =
    SelectionSet.succeed Tension
        |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Tension.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.Tension.createdBy identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with Fractal.Object.Tension.title
        |> with Fractal.Object.Tension.type_
        |> with
            (Fractal.Object.Tension.labels
                (\args -> { args | first = Present nLabelPerTension })
                (SelectionSet.map Label Fractal.Object.Label.name)
            )
        |> with
            (Fractal.Object.Tension.emitter identity
                (SelectionSet.succeed EmitterOrReceiver
                    |> with Fractal.Object.Node.name
                    |> with Fractal.Object.Node.nameid
                    |> with Fractal.Object.Node.role_type
                )
            )
        |> with
            (Fractal.Object.Tension.receiver identity
                (SelectionSet.succeed EmitterOrReceiver
                    |> with Fractal.Object.Node.name
                    |> with Fractal.Object.Node.nameid
                    |> with Fractal.Object.Node.role_type
                )
            )
        |> with Fractal.Object.Tension.action
        |> with Fractal.Object.Tension.n_comments


circleTensionPayload : SelectionSet NodeTensions Fractal.Object.Node
circleTensionPayload =
    SelectionSet.succeed NodeTensions
        |> with (Fractal.Object.Node.tensions_in circleTensionFilter tensionPayload)
        |> with (Fractal.Object.Node.tensions_out circleTensionFilter tensionPayload)
        |> with
            (Fractal.Object.Node.children identity
                (SelectionSet.succeed SubNodeTensions
                    |> with (Fractal.Object.Node.tensions_in circleTensionFilter tensionPayload)
                    |> with (Fractal.Object.Node.tensions_out circleTensionFilter tensionPayload)
                )
            )


subCircleTensionDecoder : SubNodeTensions -> List Tension
subCircleTensionDecoder child =
    let
        tin =
            child.tensions_in |> withDefault []

        tout =
            child.tensions_out |> withDefault []
    in
    tin ++ List.filter (\t -> t.emitter.nameid /= t.receiver.nameid) tout



{-
   Query Sub Tension (all tension below a Node)
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


queryIntTension targetids first offset query_ status_ msg =
    makeGQLQuery
        (Query.queryTension
            (subTensionIntFilterByDate targetids first offset query_ status_)
            tensionPayload
        )
        (RemoteData.fromResult >> decodeResponse subTensionDecoder >> msg)


queryExtTension targetids first offset query_ status_ msg =
    makeGQLQuery
        (Query.queryTension
            (subTensionExtFilterByDate targetids first offset query_ status_)
            tensionPayload
        )
        (RemoteData.fromResult >> decodeResponse subTensionDecoder >> msg)


subTensionIntFilterByDate : List String -> Int -> Int -> Maybe String -> Maybe TensionStatus.TensionStatus -> Query.QueryTensionOptionalArguments -> Query.QueryTensionOptionalArguments
subTensionIntFilterByDate nameids first offset query_ status_ a =
    let
        nameidsRegxp_ =
            nameids
                |> List.map (\n -> "^" ++ n ++ "$")
                |> String.join "|"

        nameidsRegxp =
            "/" ++ nameidsRegxp_ ++ "/"
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
                        | status = status_ |> Maybe.map (\status -> { eq = status }) |> fromMaybe
                        , emitterid = { eq = Absent, regexp = Present nameidsRegxp } |> Present
                        , receiverid = { eq = Absent, regexp = Present nameidsRegxp } |> Present
                        , and =
                            query_
                                |> Maybe.map
                                    (\q ->
                                        Input.buildTensionFilter
                                            (\d2 ->
                                                { d2
                                                    | title = { alloftext = Absent, anyoftext = Present q } |> Present
                                                    , or =
                                                        Input.buildTensionFilter
                                                            (\d3 ->
                                                                { d3
                                                                    | message = { alloftext = Absent, anyoftext = Present q } |> Present
                                                                }
                                                            )
                                                            |> Present
                                                }
                                            )
                                    )
                                |> fromMaybe
                    }
                )
                |> Present
    }


subTensionExtFilterByDate : List String -> Int -> Int -> Maybe String -> Maybe TensionStatus.TensionStatus -> Query.QueryTensionOptionalArguments -> Query.QueryTensionOptionalArguments
subTensionExtFilterByDate nameids first offset query_ status_ a =
    let
        nameidsRegxp_ =
            nameids
                |> List.map (\n -> "^" ++ n ++ "$")
                |> String.join "|"

        nameidsRegxp =
            "/" ++ nameidsRegxp_ ++ "/"
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
                        | status = status_ |> Maybe.map (\status -> { eq = status }) |> fromMaybe
                        , title = query_ |> Maybe.map (\q -> { alloftext = Absent, anyoftext = Present q }) |> fromMaybe
                        , or =
                            Input.buildTensionFilter
                                (\d3 ->
                                    { d3
                                        | message = query_ |> Maybe.map (\q -> { alloftext = Absent, anyoftext = Present q }) |> fromMaybe
                                    }
                                )
                                |> Present
                        , and =
                            Input.buildTensionFilter
                                (\d ->
                                    { d
                                        | receiverid = { eq = Absent, regexp = Present nameidsRegxp } |> Present
                                        , not =
                                            Input.buildTensionFilter
                                                (\e ->
                                                    { e
                                                        | emitterid = { eq = Absent, regexp = Present nameidsRegxp } |> Present
                                                    }
                                                )
                                                |> Present
                                        , or =
                                            Input.buildTensionFilter
                                                (\d1 ->
                                                    { d1
                                                        | emitterid = { eq = Absent, regexp = Present nameidsRegxp } |> Present
                                                        , not =
                                                            Input.buildTensionFilter
                                                                (\e1 ->
                                                                    { e1
                                                                        | receiverid = { eq = Absent, regexp = Present nameidsRegxp } |> Present
                                                                    }
                                                                )
                                                                |> Present
                                                    }
                                                )
                                                |> Present
                                    }
                                )
                                |> Present
                    }
                )
                |> Present
    }
