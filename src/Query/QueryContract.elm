module Query.QueryContract exposing (getContracts)

import Dict exposing (Dict)
import Extra exposing (ternary)
import Fractal.Enum.ContractOrderable as ContractOrderable
import Fractal.Enum.ContractStatus as ContractStatus
import Fractal.Enum.ContractType as ContractType
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.AddContractPayload
import Fractal.Object.Contract
import Fractal.Object.Tension
import Fractal.Query as Query
import Fractal.Scalar
import Fractal.ScalarCodecs
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Maybe exposing (withDefault)
import ModelCommon exposing (TensionForm, UserForm)
import ModelSchema exposing (..)
import Query.QueryNode exposing (emiterOrReceiverPayload, tidPayload)
import Query.QueryTension exposing (contractPayload)
import RemoteData exposing (RemoteData)


type alias TensionContracts =
    { --id : String
      contracts : Maybe (List Contract)

    --, n_contracts : Maybe Int
    , receiver : EmitterOrReceiver
    }


contractsDecoder : Maybe TensionContracts -> Maybe (List Contract)
contractsDecoder data =
    data
        |> Maybe.map (\x -> x.contracts)
        |> withDefault Nothing


getContracts url form msg =
    makeGQLQuery url
        (Query.getTension { id = encodeId form.tid }
            (tensionContractsPayload form)
        )
        (RemoteData.fromResult >> decodeResponse contractsDecoder >> msg)



--tensionContractsPayload : SelectionSet TensionContracts Fractal.Object.Tension


tensionContractsPayload form =
    let
        first =
            form.page_len

        offset =
            form.page_len * form.page
    in
    SelectionSet.succeed TensionContracts
        --|> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
        |> with
            (Fractal.Object.Tension.contracts
                (\args ->
                    { args
                        | first = Present first
                        , offset = Present offset
                        , order =
                            Input.buildContractOrder
                                (\b -> { b | desc = Present ContractOrderable.CreatedAt })
                                |> Present
                    }
                )
                contractPayload
            )
        --|> with Fractal.Object.Tension.n_blobs
        |> with (Fractal.Object.Tension.receiver identity emiterOrReceiverPayload)
