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
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Maybe exposing (withDefault)
import ModelCommon exposing (CircleForm, TensionForm)
import ModelSchema exposing (..)
import Query.QueryTension exposing (tensionPgPayload)
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


addOneTension form msg =
    --@DEBUG: Infered type...
    makeGQLMutation
        (Mutation.addTension
            (addTensionInputEncoder form)
            (SelectionSet.map AddTensionPayload <|
                Fractal.Object.AddTensionPayload.tension identity tensionPgPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse tensionDecoder >> msg)



-- input Encoder


addTensionInputEncoder : TensionForm -> Mutation.AddTensionRequiredArguments
addTensionInputEncoder { uctx, source, target, type_, post } =
    let
        title =
            Dict.get "title" post |> withDefault ""

        createdAt =
            Dict.get "createdAt" post |> withDefault ""

        tensionRequired =
            { createdAt = createdAt |> Fractal.Scalar.DateTime
            , createdBy =
                Input.buildUserRef
                    (\x -> { x | username = OptionalArgument.Present uctx.username })
            , title = title
            , type_ = type_
            , status = TensionStatus.Open
            , emitter =
                Input.buildNodeRef
                    (\x ->
                        { x
                            | nameid = OptionalArgument.Present source.nameid
                            , rootnameid = OptionalArgument.Present source.rootnameid
                        }
                    )
            , receiver =
                Input.buildNodeRef
                    (\x ->
                        { x
                            | nameid = OptionalArgument.Present target.nameid
                            , rootnameid = OptionalArgument.Present target.rootnameid
                        }
                    )
            }
    in
    { input =
        [ Input.buildAddTensionInput tensionRequired identity ]
    }



{-
   Add a tension with with "New Circle" Action
-}


addCircleTension form msg =
    --@DEBUG: Infered type...
    makeGQLMutation
        (Mutation.addTension
            (addCircleInputEncoder form)
            (SelectionSet.map AddTensionPayload <|
                Fractal.Object.AddTensionPayload.tension identity tensionPgPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse tensionDecoder >> msg)


addCircleInputEncoder : CircleForm -> Mutation.AddTensionRequiredArguments
addCircleInputEncoder { uctx, source, target, type_, tensionType, post } =
    let
        title =
            Dict.get "title" post |> withDefault ""

        createdAt =
            Dict.get "createdAt" post |> withDefault ""

        status =
            Dict.get "status" post |> withDefault "" |> TensionStatus.fromString |> withDefault TensionStatus.Open

        tensionRequired =
            { createdAt = createdAt |> Fractal.Scalar.DateTime
            , createdBy =
                Input.buildUserRef
                    (\x -> { x | username = OptionalArgument.Present uctx.username })
            , title = title
            , type_ = TensionType.Governance
            , status = status
            , emitter =
                Input.buildNodeRef
                    (\x ->
                        { x
                            | nameid = OptionalArgument.Present source.nameid
                            , rootnameid = OptionalArgument.Present source.rootnameid
                        }
                    )
            , receiver =
                Input.buildNodeRef
                    (\x ->
                        { x
                            | nameid = OptionalArgument.Present target.nameid
                            , rootnameid = OptionalArgument.Present target.rootnameid
                        }
                    )
            }

        tensionOpts =
            \t ->
                { t
                    | action = TensionAction.NewCircle |> OptionalArgument.Present
                    , mandate =
                        Input.buildMandateRef
                            (\m ->
                                { m
                                    | purpose = Dict.get "purpose" post |> withDefault "" |> OptionalArgument.Present
                                    , responsabilities = Dict.get "responsabilities" post |> withDefault "" |> OptionalArgument.Present
                                    , domains = Dict.get "domains" post |> withDefault "" |> OptionalArgument.Present
                                }
                            )
                            |> OptionalArgument.Present
                }
    in
    { input =
        [ Input.buildAddTensionInput tensionRequired tensionOpts ]
    }
