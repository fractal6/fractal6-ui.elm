module Query.PatchNode exposing (addOneLabel, removeOneLabel, updateOneLabel)

import Dict exposing (Dict)
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.AddLabelPayload
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
import ModelCommon exposing (LabelNodeForm)
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


type alias LabelsFullPayload =
    { label : Maybe (List (Maybe LabelFull)) }


labelFullDecoder : Maybe LabelsFullPayload -> Maybe LabelFull
labelFullDecoder data =
    case data of
        Just d ->
            d.label
                |> Maybe.map (\x -> List.head x)
                |> Maybe.withDefault Nothing
                |> Maybe.withDefault Nothing

        Nothing ->
            Nothing


addOneLabel url form msg =
    makeGQLMutation url
        (Mutation.addLabel
            (addLabelInputEncoder form)
            (SelectionSet.map LabelsFullPayload <|
                Fractal.Object.AddLabelPayload.label identity labelFullPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse labelFullDecoder >> msg)


addLabelInputEncoder : LabelNodeForm -> Mutation.AddLabelRequiredArguments
addLabelInputEncoder form =
    let
        inputReq =
            { rootnameid = nid2rootid form.nameid
            , name = Dict.get "name" form.post |> withDefault "" |> String.toLower
            }

        inputOpt =
            \x ->
                { x
                    | color = fromMaybe (Dict.get "color" form.post)
                    , description = fromMaybe (Dict.get "description" form.post)
                    , nodes =
                        Present
                            [ Input.buildNodeRef (\n -> { n | nameid = Present form.nameid }) ]
                }
    in
    { input = [ Input.buildAddLabelInput inputReq inputOpt ] }



{-
   Update Label
-}


updateOneLabel url form msg =
    makeGQLMutation url
        (Mutation.updateLabel
            (updateLabelInputEncoder form)
            (SelectionSet.map LabelsFullPayload <|
                Fractal.Object.UpdateLabelPayload.label identity <|
                    labelFullPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse labelFullDecoder >> msg)


updateLabelInputEncoder : LabelNodeForm -> Mutation.UpdateLabelRequiredArguments
updateLabelInputEncoder form =
    let
        inputReq =
            { filter =
                if form.id == "" then
                    -- assumes duplicate update
                    Input.buildLabelFilter
                        (\i ->
                            { i
                                | rootnameid = Present { eq = Present (nid2rootid form.nameid), in_ = Absent }
                                , name = Present { eq = fromMaybe (Dict.get "name" form.post), in_ = Absent, anyofterms = Absent, allofterms = Absent }
                            }
                        )

                else
                    Input.buildLabelFilter (\i -> { i | id = Present [ encodeId form.id ] })
            }

        post =
            if form.id == "" then
                -- Just register the label in the given node
                form.post |> Dict.remove "name" |> Dict.remove "color" |> Dict.remove "description"

            else if Dict.get "name" form.post == Dict.get "old_name" form.post then
                form.post |> Dict.remove "name"

            else
                form.post |> Dict.update "name" (\x -> Maybe.map (\n -> String.toLower n) x)

        inputOpt =
            \x ->
                { set =
                    Input.buildLabelPatch
                        (\i ->
                            { i
                                | name = fromMaybe (Dict.get "name" post)
                                , color = fromMaybe (Dict.get "color" post)
                                , description = fromMaybe (Dict.get "description" post)
                                , nodes = Present [ Input.buildNodeRef (\n -> { n | nameid = Present form.nameid }) ]
                            }
                        )
                        |> Present
                , remove = Absent
                }
    in
    { input = Input.buildUpdateLabelInput inputReq inputOpt }



{-
   Remove label
-}


removeOneLabel url form msg =
    makeGQLMutation url
        (Mutation.updateLabel
            (removeLabelInputEncoder form)
            (SelectionSet.map LabelsFullPayload <|
                Fractal.Object.UpdateLabelPayload.label identity <|
                    labelFullPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse labelFullDecoder >> msg)


removeLabelInputEncoder : LabelNodeForm -> Mutation.UpdateLabelRequiredArguments
removeLabelInputEncoder form =
    let
        inputReq =
            { filter =
                Input.buildLabelFilter (\i -> { i | id = Present [ encodeId form.id ] })
            }

        inputOpt =
            \x ->
                { set = Absent
                , remove =
                    Input.buildLabelPatch
                        (\i ->
                            { i
                                | nodes =
                                    Present
                                        [ Input.buildNodeRef (\j -> { j | nameid = Present form.nameid }) ]
                            }
                        )
                        |> Present
                }
    in
    { input = Input.buildUpdateLabelInput inputReq inputOpt }
