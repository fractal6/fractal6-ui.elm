module Query.QueryContract exposing
    ( cidPayload
    , contractPayload
    , getContract
    , getContractComments
    , getContracts
    )

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
import Fractal.Object.EventFragment
import Fractal.Object.Node
import Fractal.Object.Tension
import Fractal.Object.User
import Fractal.Object.Vote
import Fractal.Query as Query
import Fractal.Scalar
import Fractal.ScalarCodecs
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Maybe exposing (withDefault)
import ModelCommon exposing (TensionForm, UserForm)
import ModelSchema exposing (..)
import Query.QueryNode exposing (emiterOrReceiverPayload, tidPayload, userPayload)
import Query.QueryTension exposing (commentPayload)
import RemoteData exposing (RemoteData)


nCommentPerContract : Int
nCommentPerContract =
    250


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


getContract url form msg =
    makeGQLQuery url
        (Query.getContract { id = encodeId form.cid }
            contractPayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


getContractComments url form msg =
    makeGQLQuery url
        (Query.getContract { id = encodeId form.cid }
            contractCommentsPayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


{-| tensionContractsPayload : SelectionSet TensionContracts Fractal.Object.Tension
-}
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


contractPayload : SelectionSet Contract Fractal.Object.Contract
contractPayload =
    SelectionSet.succeed Contract
        |> with (Fractal.Object.Contract.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Contract.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.Contract.closedAt |> SelectionSet.map (Maybe.map (\x -> decodedTime x)))
        |> with (Fractal.Object.Contract.createdBy identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with (Fractal.Object.Contract.tension identity tidPayload)
        |> with (Fractal.Object.Contract.event identity eventFragmentPayload)
        |> with Fractal.Object.Contract.status
        |> with Fractal.Object.Contract.contract_type
        |> with (Fractal.Object.Contract.candidates identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with (Fractal.Object.Contract.participants identity votePayload)
        |> hardcoded Nothing


eventFragmentPayload : SelectionSet EventFragment Fractal.Object.EventFragment
eventFragmentPayload =
    SelectionSet.succeed EventFragment
        |> with Fractal.Object.EventFragment.event_type
        |> with Fractal.Object.EventFragment.old
        |> with Fractal.Object.EventFragment.new


votePayload : SelectionSet Vote Fractal.Object.Vote
votePayload =
    SelectionSet.succeed Vote
        |> with (Fractal.Object.Vote.node identity (SelectionSet.map NameidPayload Fractal.Object.Node.nameid))
        |> with Fractal.Object.Vote.data


contractCommentsPayload : SelectionSet ContractComments Fractal.Object.Contract
contractCommentsPayload =
    SelectionSet.succeed ContractComments
        |> with (Fractal.Object.Contract.tension identity tidPayload)
        |> with
            (Fractal.Object.Contract.comments
                (\args -> { args | first = Present nCommentPerContract })
                commentPayload
            )


cidPayload : SelectionSet IdPayload Fractal.Object.Contract
cidPayload =
    SelectionSet.map IdPayload
        (Fractal.Object.Contract.id |> SelectionSet.map decodedId)
