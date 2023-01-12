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


module Query.PatchContract exposing (pushComment, sendVote)

import Bulk exposing (CommentPatchForm)
import Bulk.Codecs exposing (memberIdCodec)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Fractal.Enum.ContractStatus as ContractStatus
import Fractal.Enum.ContractType as ContractType
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.AddVotePayload
import Fractal.Object.Contract
import Fractal.Object.UpdateContractPayload
import Fractal.Object.Vote
import Fractal.Query as Query
import Fractal.Scalar
import Fractal.ScalarCodecs
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Query.AddTension exposing (buildComment)
import Query.PatchTension exposing (pushCommentFilter)
import Query.QueryContract exposing (contractPayload)
import Query.QueryNode exposing (cidPayload)
import Query.QueryTension exposing (commentPayload)
import RemoteData exposing (RemoteData)



{-
   update contract (Add Vote)
-}


type alias VotePayload =
    { vote : Maybe (List (Maybe VoteResult)) }


type alias VoteResult =
    { id : String
    , contract : ContractResult
    }


voteDecoder : Maybe VotePayload -> Maybe ContractResult
voteDecoder data =
    data
        |> Maybe.andThen
            (\d ->
                d.vote
                    |> Maybe.map (\x -> List.head x)
                    |> Maybe.withDefault Nothing
                    |> Maybe.withDefault Nothing
                    |> Maybe.map (\x -> x.contract)
            )


sendVote url form msg =
    makeGQLMutation url
        (Mutation.addVote
            (\q -> { q | upsert = Present True })
            (voteInputDecoder form)
            (SelectionSet.map VotePayload <|
                Fractal.Object.AddVotePayload.vote identity votePayload
            )
        )
        (RemoteData.fromResult >> decodeResponse voteDecoder >> msg)


voteInputDecoder form =
    let
        createdAt =
            Dict.get "createdAt" form.post |> withDefault "" |> Fractal.Scalar.DateTime

        nid =
            memberIdCodec form.rootnameid form.uctx.username

        inputReq =
            { createdAt = createdAt
            , createdBy =
                Input.buildUserRef
                    (\u -> { u | username = Present form.uctx.username })
            , voteid = form.contractid ++ "#" ++ nid
            , contract =
                Input.buildContractRef
                    (\x -> { x | contractid = Present form.contractid })
            , node =
                Input.buildNodeRef
                    (\x -> { x | nameid = Present nid })
            , data = [ form.vote ]
            }
    in
    { input = [ Input.buildAddVoteInput inputReq identity ] }


votePayload : SelectionSet VoteResult Fractal.Object.Vote
votePayload =
    SelectionSet.map2 VoteResult
        (Fractal.Object.Vote.id |> SelectionSet.map decodedId)
        (Fractal.Object.Vote.contract identity <|
            SelectionSet.map2 ContractResult
                (Fractal.Object.Contract.id |> SelectionSet.map decodedId)
                Fractal.Object.Contract.status
        )



{-
   update contract (Push Comment)
-}


type alias ContractCommentPayload =
    { contract : Maybe (List (Maybe Comments)) }


type alias Comments =
    { comments : Maybe (List Comment)
    }


contractCommentDecoder : Maybe ContractCommentPayload -> Maybe Comment
contractCommentDecoder data =
    case data of
        Just d ->
            d.contract
                |> Maybe.map (List.filterMap identity)
                |> withDefault []
                |> List.head
                |> Maybe.map
                    (\x ->
                        x.comments
                            |> withDefault []
                            |> List.head
                    )
                |> withDefault Nothing

        Nothing ->
            Nothing


pushComment url form msg =
    makeGQLMutation url
        (Mutation.updateContract
            (commentInputDecoder form)
            (SelectionSet.map ContractCommentPayload <|
                Fractal.Object.UpdateContractPayload.contract identity
                    (SelectionSet.map Comments
                        (Fractal.Object.Contract.comments pushCommentFilter
                            commentPayload
                        )
                    )
            )
        )
        (RemoteData.fromResult >> decodeResponse contractCommentDecoder >> msg)


commentInputDecoder : CommentPatchForm -> Mutation.UpdateContractRequiredArguments
commentInputDecoder f =
    let
        createdAt =
            Dict.get "createdAt" f.post |> withDefault "" |> Fractal.Scalar.DateTime

        message =
            Dict.get "message" f.post

        contractid =
            Dict.get "contractid" f.post |> withDefault ""

        inputReq =
            { filter =
                Input.buildContractFilter
                    (\ft -> { ft | id = Present [ encodeId contractid ] })
            }

        inputOpt =
            \_ ->
                { set =
                    Input.buildContractPatch
                        (\s ->
                            { s
                                | -- updatedAt = Present createdAt -- Only the creator of the the contract can update this field
                                  comments = buildComment createdAt f.uctx.username message
                            }
                        )
                        |> Present
                , remove = Absent
                }
    in
    { input = Input.buildUpdateContractInput inputReq inputOpt }
