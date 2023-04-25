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


module Query.AddContract exposing (addOneContract, deleteOneContract)

import Bulk exposing (ContractForm)
import Dict
import Extra exposing (listToMaybe)
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.AddContractPayload
import Fractal.Object.DeleteContractPayload
import Fractal.Scalar
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Query.AddTension exposing (buildComment)
import Query.QueryContract exposing (cidPayload)
import RemoteData exposing (RemoteData)



{-
   Add one contract
-}


type alias ContractsPayload =
    { contract : Maybe (List (Maybe Contract)) }


type alias ContractsPayloadId =
    { contract : Maybe (List (Maybe IdPayload)) }



-- Response Decoder


contractIdDecoder : Maybe ContractsPayloadId -> Maybe IdPayload
contractIdDecoder a =
    a
        |> Maybe.andThen
            (\b ->
                b.contract
                    |> Maybe.map List.head
                    |> Maybe.withDefault Nothing
                    |> Maybe.withDefault Nothing
            )


addOneContract url form msg =
    --@DEBUG: Infered type...
    makeGQLMutation url
        (Mutation.addContract
            identity
            (addContractInputEncoder form)
            (SelectionSet.map ContractsPayloadId <|
                Fractal.Object.AddContractPayload.contract identity cidPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse contractIdDecoder >> msg)



-- input Encoder


addContractInputEncoder : ContractForm -> Mutation.AddContractRequiredArguments
addContractInputEncoder f =
    let
        cat =
            Dict.get "createdAt" f.post |> withDefault "" |> Fractal.Scalar.DateTime

        cby =
            Input.buildUserRef
                (\u -> { u | username = Present f.uctx.username })
    in
    let
        inputReq =
            { createdAt = cat
            , createdBy = cby
            , tension =
                Input.buildTensionRef (\u -> { u | id = f.tid |> encodeId |> Present })
            , event =
                Input.buildEventFragmentRef
                    (\x ->
                        { x
                            | event_type = Present f.event.event_type
                            , old = fromMaybe f.event.old
                            , new = fromMaybe f.event.new
                        }
                    )
            , status = f.status
            , contract_type = f.contract_type
            , contractid = f.contractid
            , participants =
                f.participants
                    |> List.map
                        (\p ->
                            Input.buildVoteRef
                                (\v ->
                                    { v
                                        | createdAt = Present cat
                                        , createdBy = Present cby
                                        , voteid = Present p.voteid
                                        , node = Input.buildNodeRef (\n -> { n | nameid = Present p.node.nameid }) |> Present
                                        , data = Present p.data
                                    }
                                )
                        )
            }

        inputOpt =
            \x ->
                { x
                    | message = Dict.get "message" f.post |> fromMaybe
                    , comments = buildComment cat f.uctx.username (Dict.get "message" f.post)
                    , candidates =
                        f.candidates
                            |> List.map
                                (\p ->
                                    Input.buildUserRef (\v -> { v | username = Present p.username })
                                )
                            |> listToMaybe
                            |> fromMaybe
                    , pending_candidates =
                        f.pending_candidates
                            |> List.map
                                (\p ->
                                    Input.buildPendingUserRef (\v -> { v | username = Present p.email, email = Present p.email })
                                )
                            |> listToMaybe
                            |> fromMaybe
                }
    in
    { input =
        [ Input.buildAddContractInput inputReq inputOpt ]
    }



{-
   Remove contract
-}


deleteOneContract url form msg =
    makeGQLMutation url
        (Mutation.deleteContract
            (deleteContractInputEncoder form)
            (SelectionSet.map ContractsPayloadId <|
                Fractal.Object.DeleteContractPayload.contract identity cidPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse contractIdDecoder >> msg)


deleteContractInputEncoder form =
    { filter =
        Input.buildContractFilter (\i -> { i | id = Present [ encodeId form.cid ] })
    }
