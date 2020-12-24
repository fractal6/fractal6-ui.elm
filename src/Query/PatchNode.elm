module Query.PatchNode exposing (addOneLabel)

import Dict exposing (Dict)
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.Node
import Fractal.Object.NodeCharac
import Fractal.Object.UpdateNodePayload
import Fractal.Object.User
import Fractal.Scalar
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Maybe exposing (withDefault)
import ModelCommon exposing (LabelForm)
import ModelSchema exposing (..)
import Query.AddTension exposing (buildMandate, tensionFromForm)
import RemoteData exposing (RemoteData)



--
-- Operatation define here (PatchNode) directly operate on a node. Usually only coordinator (or allowed role) can patch node.
-- Furthermore, those operation, as they do not pass trough the tension systems, they are "free" from the notification system.
--
{-
   Add one Label
-}


type alias PatchNodeIdPayload =
    { node : Maybe (List (Maybe IdPayload)) }


nodePatchDecoder : Maybe PatchNodeIdPayload -> Maybe IdPayload
nodePatchDecoder data =
    case data of
        Just d ->
            d.node
                |> Maybe.map
                    (\items ->
                        List.filterMap identity items
                    )
                |> withDefault []
                |> List.head

        Nothing ->
            Nothing


addOneLabel url form msg =
    makeGQLMutation url
        (Mutation.updateNode
            (labelInputEncoder form)
            (SelectionSet.map PatchNodeIdPayload <|
                Fractal.Object.UpdateNodePayload.node identity <|
                    (SelectionSet.succeed IdPayload
                        |> with (Fractal.Object.Node.id |> SelectionSet.map decodedId)
                    )
            )
        )
        (RemoteData.fromResult >> decodeResponse nodePatchDecoder >> msg)


labelInputEncoder : LabelForm -> Mutation.UpdateNodeRequiredArguments
labelInputEncoder form =
    let
        inputReq =
            { filter =
                Input.buildNodeFilter
                    (\i ->
                        { i | nameid = { eq = Present form.nameid, regexp = Absent } |> Present }
                    )
            }

        inputOpt =
            \x ->
                { set =
                    Input.buildNodePatch
                        (\i ->
                            { i
                                | labels =
                                    Present
                                        [ Input.buildLabelRef
                                            (\j ->
                                                { j
                                                    | name = fromMaybe (Dict.get "name" form.post)
                                                    , color = fromMaybe (Dict.get "color" form.post)
                                                    , description = fromMaybe (Dict.get "description" form.post)
                                                }
                                            )
                                        ]
                            }
                        )
                        |> Present
                , remove = Absent
                }
    in
    { input = Input.buildUpdateNodeInput inputReq inputOpt }
