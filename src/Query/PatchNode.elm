module Query.PatchNode exposing (addOneLabel, removeOneLabel, updateOneLabel)

import Dict exposing (Dict)
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.DeleteLabelPayload
import Fractal.Object.Node
import Fractal.Object.NodeCharac
import Fractal.Object.UpdateLabelPayload
import Fractal.Object.UpdateNodePayload
import Fractal.Object.User
import Fractal.Scalar
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Maybe exposing (withDefault)
import ModelCommon exposing (LabelForm)
import ModelCommon.Codecs exposing (nid2rootid)
import ModelSchema exposing (..)
import Query.AddTension exposing (buildMandate, tensionFromForm)
import Query.QueryNode exposing (labelFullPayload)
import RemoteData exposing (RemoteData)



--
-- We do not directtly operate on Node, but user referecne instead. (see @hasLink schema directive).
--
{-
   Add one Label
-}


type alias NodeLabels_ =
    { labels : Maybe (List LabelFull) }


type alias PatchLabelNodePayload =
    { node : Maybe (List (Maybe NodeLabels_)) }


nodePatchLabelDecoder : Maybe PatchLabelNodePayload -> Maybe LabelFull
nodePatchLabelDecoder data =
    case data of
        Just d ->
            d.node
                |> Maybe.map
                    (\items ->
                        List.filterMap identity items
                    )
                |> withDefault []
                |> List.head
                |> Maybe.map (\n -> n.labels)
                |> withDefault Nothing
                |> withDefault []
                |> List.head

        Nothing ->
            Nothing


addOneLabel url form msg =
    let
        name =
            Dict.get "name" form.post
    in
    makeGQLMutation url
        (Mutation.updateNode
            (labelInputEncoder form)
            (SelectionSet.map PatchLabelNodePayload <|
                Fractal.Object.UpdateNodePayload.node identity <|
                    SelectionSet.map NodeLabels_
                        (Fractal.Object.Node.labels
                            (\args ->
                                { args
                                    | filter =
                                        Input.buildLabelFilter (\i -> { i | name = Present { eq = fromMaybe name, allofterms = Absent, anyofterms = Absent } })
                                            |> Present
                                }
                            )
                            labelFullPayload
                        )
            )
        )
        (RemoteData.fromResult >> decodeResponse nodePatchLabelDecoder >> msg)


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
                                                    | rootnameid = Present (nid2rootid form.nameid)
                                                    , name = fromMaybe (Dict.get "name" form.post)
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



{-
   Update one Label (remove)
-}


type alias PatchNodeIdPayload =
    { node : Maybe (List (Maybe IdPayload)) }


nodePatchIDDecoder : Maybe PatchNodeIdPayload -> Maybe IdPayload
nodePatchIDDecoder data =
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


removeOneLabel url form msg =
    makeGQLMutation url
        (Mutation.updateNode
            (labelInputREncoder form)
            (SelectionSet.map PatchNodeIdPayload <|
                Fractal.Object.UpdateNodePayload.node identity <|
                    (SelectionSet.succeed IdPayload
                        |> with (Fractal.Object.Node.id |> SelectionSet.map decodedId)
                    )
            )
        )
        (RemoteData.fromResult >> decodeResponse nodePatchIDDecoder >> msg)


labelInputREncoder : LabelForm -> Mutation.UpdateNodeRequiredArguments
labelInputREncoder form =
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
                { set = Absent
                , remove =
                    Input.buildNodePatch
                        (\i ->
                            { i
                                | labels =
                                    Present
                                        [ Input.buildLabelRef (\j -> { j | id = Present (encodeId form.id) }) ]
                            }
                        )
                        |> Present
                }
    in
    { input = Input.buildUpdateNodeInput inputReq inputOpt }



{-
   Update Label
-}


type alias PatchLabelPayload =
    { label : Maybe (List (Maybe LabelFull)) }


labelPatchDecoder : Maybe PatchLabelPayload -> Maybe LabelFull
labelPatchDecoder data =
    case data of
        Just d ->
            d.label
                |> Maybe.map
                    (\items ->
                        List.filterMap identity items
                    )
                |> withDefault []
                |> List.head

        Nothing ->
            Nothing


updateOneLabel url form msg =
    makeGQLMutation url
        (Mutation.updateLabel
            (labelFullInputEncoder form)
            (SelectionSet.map PatchLabelPayload <|
                Fractal.Object.UpdateLabelPayload.label identity <|
                    labelFullPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse labelPatchDecoder >> msg)


labelFullInputEncoder : LabelForm -> Mutation.UpdateLabelRequiredArguments
labelFullInputEncoder form =
    let
        name =
            Dict.get "name" form.post

        inputReq =
            { filter =
                Input.buildLabelFilter (\i -> { i | id = Present [ encodeId form.id ] })
            }

        inputOpt =
            \x ->
                { set =
                    Input.buildLabelPatch
                        (\i ->
                            { i
                                | name = fromMaybe (Dict.get "name" form.post)
                                , color = fromMaybe (Dict.get "color" form.post)
                                , description = fromMaybe (Dict.get "description" form.post)
                            }
                        )
                        |> Present
                , remove = Absent
                }
    in
    { input = Input.buildUpdateLabelInput inputReq inputOpt }



{-
   Remove Label
-}


deleteOneLabel url form msg =
    makeGQLMutation url
        (Mutation.deleteLabel
            { filter =
                Input.buildLabelFilter (\i -> { i | id = Present [ encodeId form.id ] })
            }
            (SelectionSet.map PatchLabelPayload <|
                Fractal.Object.DeleteLabelPayload.label identity <|
                    labelFullPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse labelPatchDecoder >> msg)
