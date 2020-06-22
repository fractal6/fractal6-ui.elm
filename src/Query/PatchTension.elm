module Query.PatchTension exposing (pushTensionComment)

import Dict exposing (Dict)
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.Label
import Fractal.Object.Tension
import Fractal.Object.UpdateTensionPayload
import Fractal.Query as Query
import Fractal.Scalar
import Fractal.ScalarCodecs
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Maybe exposing (withDefault)
import ModelCommon exposing (TensionPatchForm)
import ModelSchema exposing (..)
import RemoteData exposing (RemoteData)



{-
   Patch a single tension
-}


type alias PatchTensionIdPayload =
    { tension : Maybe (List (Maybe IdPayload)) }


tensionPatchDecoder : Maybe PatchTensionIdPayload -> Maybe IdPayload
tensionPatchDecoder data =
    case data of
        Just d ->
            d.tension
                |> Maybe.map
                    (\ts ->
                        List.filterMap identity ts
                    )
                |> withDefault []
                |> List.head

        Nothing ->
            Nothing


pushTensionComment form msg =
    makeGQLMutation
        (Mutation.updateTension
            (pushTensionCommentInputEncoder form)
            (SelectionSet.map PatchTensionIdPayload <|
                Fractal.Object.UpdateTensionPayload.tension identity <|
                    (SelectionSet.succeed IdPayload
                        |> with (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
                    )
            )
        )
        (RemoteData.fromResult >> decodeResponse tensionPatchDecoder >> msg)


pushTensionCommentInputEncoder : TensionPatchForm -> Mutation.UpdateTensionRequiredArguments
pushTensionCommentInputEncoder form =
    let
        -- new comment
        createdAt =
            Dict.get "createdAt" form.post |> Maybe.map (\x -> Fractal.Scalar.DateTime x)

        message =
            Dict.get "message" form.post

        patchRequired =
            { filter =
                Input.buildTensionFilter
                    (\f ->
                        { f | id = Present [ encodeId form.id ] }
                    )
            }

        patchOpts =
            \x ->
                { set =
                    Input.buildTensionPatch
                        (\s ->
                            { s
                                | status = fromMaybe form.status
                                , comments =
                                    message
                                        |> Maybe.map
                                            (\msg ->
                                                [ Input.buildCommentRef
                                                    (\c ->
                                                        { c
                                                            | createdAt = fromMaybe createdAt
                                                            , createdBy =
                                                                Input.buildUserRef
                                                                    (\u -> { u | username = Present form.uctx.username })
                                                                    |> Present
                                                            , message = fromMaybe message
                                                        }
                                                    )
                                                ]
                                            )
                                        |> fromMaybe
                            }
                        )
                        |> Present
                , remove = Absent
                }
    in
    { input =
        Input.buildUpdateTensionInput patchRequired patchOpts
    }
