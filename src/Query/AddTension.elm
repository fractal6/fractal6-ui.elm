module Query.AddTension exposing (addCircleTension, addOneTension)

import Dict exposing (Dict)
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.AddTensionPayload
import Fractal.Object.Label
import Fractal.Object.Tension
import Fractal.Query as Query
import Fractal.Scalar
import Fractal.ScalarCodecs
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Maybe exposing (withDefault)
import ModelCommon exposing (TensionForm)
import ModelSchema exposing (..)
import Query.AddNode exposing (buildMandate, buildNodeFragmentRef)
import Query.QueryTension exposing (tensionPayload)
import RemoteData exposing (RemoteData)



{-
   Add a single tension
-}


type alias AddTensionPayload =
    { tension : Maybe (List (Maybe Tension)) }


type alias AddLabelPayload =
    { label : Maybe (List (Maybe IdPayload)) }



-- Response Decoder


tensionDecoder : Maybe AddTensionPayload -> Maybe Tension
tensionDecoder a =
    case a of
        Just b ->
            b.tension
                |> Maybe.map (\x -> List.head x)
                |> Maybe.withDefault Nothing
                |> Maybe.withDefault Nothing

        Nothing ->
            Nothing


addOneTension url form msg =
    --@DEBUG: Infered type...
    makeGQLMutation url
        (Mutation.addTension
            (addTensionInputEncoder form)
            (SelectionSet.map AddTensionPayload <|
                Fractal.Object.AddTensionPayload.tension identity tensionPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse tensionDecoder >> msg)



-- input Encoder


addTensionInputEncoder : TensionForm -> Mutation.AddTensionRequiredArguments
addTensionInputEncoder f =
    let
        title =
            Dict.get "title" f.post |> withDefault ""

        createdAt =
            Dict.get "createdAt" f.post |> withDefault ""

        message =
            Dict.get "message" f.post

        tensionRequired =
            { createdAt = createdAt |> Fractal.Scalar.DateTime
            , createdBy =
                Input.buildUserRef
                    (\x -> { x | username = Present f.uctx.username })
            , title = title
            , type_ = f.tension_type
            , status = TensionStatus.Open
            , emitter =
                Input.buildNodeRef (\n -> { n | nameid = Present f.source.nameid })
            , receiver =
                Input.buildNodeRef (\n -> { n | nameid = Present f.target.nameid })
            , emitterid = f.source.nameid
            , receiverid = f.target.nameid
            }

        tensionOpts =
            \t -> { t | message = message |> fromMaybe }
    in
    { input =
        [ Input.buildAddTensionInput tensionRequired tensionOpts ]
    }



{-
   Add a tension with with New Circle/Role Action
-}


addCircleTension url form msg =
    --@DEBUG: Infered type...
    makeGQLMutation url
        (Mutation.addTension
            (addCircleInputEncoder form)
            (SelectionSet.map AddTensionPayload
                (Fractal.Object.AddTensionPayload.tension identity tensionPayload)
            )
        )
        (RemoteData.fromResult >> decodeResponse tensionDecoder >> msg)


addCircleInputEncoder : TensionForm -> Mutation.AddTensionRequiredArguments
addCircleInputEncoder f =
    let
        title =
            Dict.get "title" f.post |> withDefault ""

        createdAt =
            Dict.get "createdAt" f.post |> withDefault ""

        status =
            Dict.get "status" f.post |> withDefault "" |> TensionStatus.fromString |> withDefault TensionStatus.Open

        message =
            Dict.get "message" f.post

        tensionRequired =
            { createdAt = createdAt |> Fractal.Scalar.DateTime
            , createdBy =
                Input.buildUserRef
                    (\x -> { x | username = Present f.uctx.username })
            , title = title
            , type_ = f.tension_type
            , status = status
            , emitterid = f.source.nameid
            , receiverid = f.target.nameid
            , emitter =
                Input.buildNodeRef (\x -> { x | nameid = Present f.source.nameid })
            , receiver =
                Input.buildNodeRef (\x -> { x | nameid = Present f.target.nameid })
            }

        tensionOpts =
            \t ->
                { t
                    | action = f.action |> fromMaybe
                    , message = message |> fromMaybe
                    , data = buildNodeFragmentRef f
                }
    in
    { input =
        [ Input.buildAddTensionInput tensionRequired tensionOpts ]
    }
