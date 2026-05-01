{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2026 Fractale Co

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


module Query.QueryContract exposing
    ( cidPayload
    , contractPayload
    , getContract
    , getContractComments
    , getContractId
    , getContracts
    , queryOpenInvitation
    )

import Bulk.Codecs exposing (nid2rootid)
import Schema.Enum.BlobHasFilter as BlobHasFilter
import Schema.Enum.BlobOrderable as BlobOrderable
import Schema.Enum.ContractOrderable as ContractOrderable
import Schema.Enum.ContractStatus as ContractStatus
import Schema.Enum.NodeType as NodeType
import Schema.Enum.TensionEvent as TensionEvent
import Schema.InputObject as Input
import Schema.Object
import Schema.Object.Blob
import Schema.Object.Contract
import Schema.Object.EventFragment
import Schema.Object.Node
import Schema.Object.Tension
import Schema.Object.User
import Schema.Object.Vote
import Schema.Query as Query
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Query.QueryNode exposing (emiterOrReceiverPayload, tidPayload)
import Query.QueryTension exposing (commentPayload, nodeFragmentLightPayload, nodeFragmentPayload)
import RemoteData


nCommentPerContract : Int
nCommentPerContract =
    250



--
-- Query contract(s)
--


type alias TensionContracts =
    { --id : String
      contracts : Maybe (List Contract)

    --, n_contracts : Maybe Int
    , receiver : EmitterOrReceiver
    }


contractsDecoder : Maybe TensionContracts -> Maybe (List Contract)
contractsDecoder data =
    data
        |> Maybe.map .contracts
        |> withDefault Nothing


getContracts url form msg =
    makeGQLQuery url
        (Query.getTension { id = encodeId form.tid }
            (tensionContractsOpenPayload form)
        )
        (RemoteData.fromResult >> decodeResponse contractsDecoder >> msg)


getContract url form msg =
    makeGQLQuery url
        (Query.getContract (\x -> { x | id = encodeId form.cid |> Present, contractid = Absent })
            contractFullPayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


getContractComments url form msg =
    makeGQLQuery url
        (Query.getContract (\x -> { x | id = encodeId form.cid |> Present, contractid = Absent })
            contractCommentsPayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


getContractId url contractid msg =
    makeGQLQuery url
        (Query.getContract (\x -> { x | id = Absent, contractid = Present contractid })
            (SelectionSet.map IdPayload (SelectionSet.map decodedId Schema.Object.Contract.id))
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)



--getPendingInvitations url form msg =
--    makeGQLQuery url
--        (Query.queryContract { id = encodeId form.tid }
--            (tensionContractsOpenPayload form)
--        )
--        (RemoteData.fromResult >> decodeResponse contractsDecoder >> msg)


{-| tensionContractsOpenPayload : SelectionSet TensionContracts Schema.Object.Tension
-}
tensionContractsOpenPayload form =
    let
        first =
            form.page_len

        offset =
            form.page_len * form.page
    in
    SelectionSet.succeed TensionContracts
        --|> with (Schema.Object.Tension.id |> SelectionSet.map decodedId)
        |> with
            (Schema.Object.Tension.contracts
                (\args ->
                    { args
                        | first = Present first
                        , offset = Present offset
                        , order =
                            Input.buildContractOrder
                                (\x -> { x | desc = Present ContractOrderable.CreatedAt })
                                |> Present
                        , filter =
                            Input.buildContractFilter
                                (\x -> { x | status = Present { eq = Present ContractStatus.Open, in_ = Absent } })
                                |> Present
                    }
                )
                contractPayload
            )
        |> with (Schema.Object.Tension.receiver identity emiterOrReceiverPayload)


contractPayload : SelectionSet Contract Schema.Object.Contract
contractPayload =
    SelectionSet.succeed Contract
        |> with (Schema.Object.Contract.id |> SelectionSet.map decodedId)
        |> with (Schema.Object.Contract.createdAt |> SelectionSet.map decodedTime)
        |> with (Schema.Object.Contract.closedAt |> SelectionSet.map (Maybe.map decodedTime))
        |> with (Schema.Object.Contract.createdBy identity <| SelectionSet.map Username Schema.Object.User.username)
        |> with (Schema.Object.Contract.tension identity tidPayload)
        |> with (Schema.Object.Contract.event identity eventFragmentPayload)
        |> with Schema.Object.Contract.status
        |> with Schema.Object.Contract.contract_type
        |> with (Schema.Object.Contract.candidates identity <| SelectionSet.map Username Schema.Object.User.username)
        |> with (Schema.Object.Contract.participants identity votePayload)
        |> hardcoded Nothing


contractFullPayload : SelectionSet ContractFull Schema.Object.Contract
contractFullPayload =
    SelectionSet.succeed ContractFull
        |> with (Schema.Object.Contract.id |> SelectionSet.map decodedId)
        |> with (Schema.Object.Contract.createdAt |> SelectionSet.map decodedTime)
        |> with (Schema.Object.Contract.closedAt |> SelectionSet.map (Maybe.map decodedTime))
        |> with (Schema.Object.Contract.createdBy identity <| SelectionSet.map Username Schema.Object.User.username)
        |> with (Schema.Object.Contract.tension identity tensionForContractPayload)
        |> with (Schema.Object.Contract.event identity eventFragmentPayload)
        |> with Schema.Object.Contract.status
        |> with Schema.Object.Contract.contract_type
        |> with (Schema.Object.Contract.candidates identity <| SelectionSet.map Username Schema.Object.User.username)
        |> with (Schema.Object.Contract.participants identity votePayload)
        |> with Schema.Object.Contract.isValidator
        |> with
            (Schema.Object.Contract.comments
                (\args -> { args | first = Present nCommentPerContract })
                commentPayload
            )


type alias BlobNode =
    { node : Maybe NodeFragment }


tensionForContractPayload : SelectionSet TensionForContract Schema.Object.Tension
tensionForContractPayload =
    SelectionSet.succeed TensionForContract
        |> with (Schema.Object.Tension.id |> SelectionSet.map decodedId)
        |> with
            (Schema.Object.Tension.blobs identity
                (SelectionSet.map BlobNode (Schema.Object.Blob.node identity nodeFragmentPayload))
            )


eventFragmentPayload : SelectionSet EventFragment Schema.Object.EventFragment
eventFragmentPayload =
    SelectionSet.succeed EventFragment
        |> with Schema.Object.EventFragment.event_type
        |> with Schema.Object.EventFragment.old
        |> with Schema.Object.EventFragment.new


votePayload : SelectionSet Vote Schema.Object.Vote
votePayload =
    SelectionSet.succeed Vote
        |> with Schema.Object.Vote.voteid
        |> with (Schema.Object.Vote.node identity (SelectionSet.map NameidPayload Schema.Object.Node.nameid))
        |> with Schema.Object.Vote.data


contractCommentsPayload : SelectionSet ContractComments Schema.Object.Contract
contractCommentsPayload =
    SelectionSet.succeed ContractComments
        |> with (Schema.Object.Contract.tension identity tidPayload)
        |> with
            (Schema.Object.Contract.comments
                (\args -> { args | first = Present nCommentPerContract })
                commentPayload
            )


cidPayload : SelectionSet IdPayload Schema.Object.Contract
cidPayload =
    SelectionSet.map IdPayload
        (Schema.Object.Contract.id |> SelectionSet.map decodedId)



--
-- Query Open contracts
--


type alias NodeContracts =
    { contracts : Maybe (List { contract : ContractLight }) }


nodeContractsDecoder : Maybe (List (Maybe NodeContracts)) -> Maybe (List ContractLight)
nodeContractsDecoder data =
    data
        |> Maybe.map
            (\d ->
                if List.length d == 0 then
                    Nothing

                else
                    d
                        |> List.filterMap identity
                        |> List.concatMap (\x -> withDefault [] x.contracts)
                        |> List.map .contract
                        |> Just
            )
        |> withDefault Nothing


nodeContractsFilter : String -> Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
nodeContractsFilter rootid a =
    { a
        | filter =
            Input.buildNodeFilter
                (\b ->
                    { b
                        | rootnameid = Present { eq = Present rootid, in_ = Absent, regexp = Absent }
                        , type_ = Present { eq = Present NodeType.Role, in_ = Absent }
                    }
                )
                |> Present
    }


openContractFilter a =
    { a
        | filter =
            Input.buildContractFilter
                (\b ->
                    { b
                        | status = Present { eq = Present ContractStatus.Open, in_ = Absent }
                    }
                )
                |> Present
    }


invitationFilter a =
    { a
        | filter =
            Input.buildEventFragmentFilter
                (\b ->
                    { b
                        | event_type =
                            Present
                                { eq = Absent
                                , in_ = Present [ Just TensionEvent.UserJoined, Just TensionEvent.MemberLinked ]
                                }
                    }
                )
                |> Present
    }


lastBlobFilter a =
    { a
        | first = Present 1
        , order =
            Input.buildBlobOrder
                (\x -> { x | desc = Present BlobOrderable.CreatedAt })
                |> Present
        , filter =
            Input.buildBlobFilter
                (\x -> { x | has = Present [ Just BlobHasFilter.PushedFlag ] })
                |> Present
    }


queryOpenInvitation url nameid msg =
    let
        -- Work with the root for now since there is no easy way to recurse in GQL
        rootid =
            nid2rootid nameid
    in
    makeGQLQuery url
        (Query.queryNode (nodeContractsFilter rootid) nodeContractsPayload)
        (RemoteData.fromResult >> decodeResponse nodeContractsDecoder >> msg)


nodeContractsPayload =
    -- @warning : adding potential/oftent null value in the Nodefragment (such as color, role_ext etc) might lead
    -- to empty response due to the cascade_directive parameter.
    SelectionSet.succeed (\a b -> NodeContracts b)
        |> with Schema.Object.Node.cascade_directive
        |> with
            (Schema.Object.Node.contracts identity
                (SelectionSet.map (\x -> { contract = x })
                    (Schema.Object.Vote.contract openContractFilter contractLightPayload)
                )
            )


contractLightPayload =
    SelectionSet.succeed ContractLight
        |> with (Schema.Object.Contract.id |> SelectionSet.map decodedId)
        |> with (Schema.Object.Contract.createdAt |> SelectionSet.map decodedTime)
        |> with (Schema.Object.Contract.createdBy identity <| SelectionSet.map Username Schema.Object.User.username)
        |> with (Schema.Object.Contract.tension identity tensionNodeBlobPayload)
        |> with (Schema.Object.Contract.event invitationFilter eventFragmentPayload)
        |> with Schema.Object.Contract.status
        |> with Schema.Object.Contract.contract_type
        |> with (SelectionSet.map (withDefault []) <| Schema.Object.Contract.candidates identity <| SelectionSet.map Username Schema.Object.User.username)


tensionNodeBlobPayload =
    SelectionSet.succeed TensionNodeBlob
        |> with (Schema.Object.Tension.id |> SelectionSet.map decodedId)
        |> with Schema.Object.Tension.receiverid
        |> with
            (SelectionSet.map (withDefault [] >> List.head >> withDefault Nothing) <|
                Schema.Object.Tension.blobs lastBlobFilter <|
                    Schema.Object.Blob.node identity nodeFragmentLightPayload
            )
