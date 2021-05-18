module Query.AddContract exposing (addOneContract, deleteOneContract)

import Dict exposing (Dict)
import Extra exposing (ternary)
import Fractal.Enum.ContractStatus as ContractStatus
import Fractal.Enum.ContractType as ContractType
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.AddContractPayload
import Fractal.Object.Contract
import Fractal.Object.DeleteContractPayload
import Fractal.Query as Query
import Fractal.Scalar
import Fractal.ScalarCodecs
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Maybe exposing (withDefault)
import ModelCommon exposing (TensionForm, UserForm)
import ModelSchema exposing (..)
import Query.QueryContract exposing (cidPayload, contractPayload)
import RemoteData exposing (RemoteData)



{-
   Add one contract
-}


type alias ContractsPayload =
    { contract : Maybe (List (Maybe Contract)) }



-- Response Decoder


contractDecoder : Maybe ContractsPayload -> Maybe Contract
contractDecoder a =
    case a of
        Just b ->
            b.contract
                |> Maybe.map (\x -> List.head x)
                |> Maybe.withDefault Nothing
                |> Maybe.withDefault Nothing

        Nothing ->
            Nothing


addOneContract url form msg =
    --@DEBUG: Infered type...
    makeGQLMutation url
        (Mutation.addContract
            (addContractInputEncoder form)
            (SelectionSet.map ContractsPayload <|
                Fractal.Object.AddContractPayload.contract identity contractPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse contractDecoder >> msg)



-- input Encoder


addContractInputEncoder f =
    --addContractInputEncoder : ContractForm -> Mutation.AddContractRequiredArguments
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
            }

        inputOpt =
            \x ->
                { x
                    | participants =
                        f.participants
                            |> Maybe.map
                                (\ps ->
                                    List.map
                                        (\p ->
                                            Input.buildVoteRef
                                                (\v ->
                                                    { v
                                                        | node = Input.buildNodeRef (\n -> { n | nameid = Present p.node.nameid }) |> Present
                                                        , data = fromMaybe p.data
                                                    }
                                                )
                                        )
                                        ps
                                )
                            |> fromMaybe
                    , message = Dict.get "message" f.post |> fromMaybe
                }
    in
    { input =
        [ Input.buildAddContractInput inputReq inputOpt ]
    }



{-
   Remove contract
-}


type alias ContractsPayloadId =
    { contract : Maybe (List (Maybe IdPayload)) }


contractIdDecoder : Maybe ContractsPayloadId -> Maybe IdPayload
contractIdDecoder a =
    case a of
        Just b ->
            b.contract
                |> Maybe.map (\x -> List.head x)
                |> Maybe.withDefault Nothing
                |> Maybe.withDefault Nothing

        Nothing ->
            Nothing


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
