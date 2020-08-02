module Query.PatchNode exposing (patchNode)

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
import ModelCommon exposing (TensionForm)
import ModelSchema exposing (..)
import Query.AddTension exposing (buildMandate, tensionFromForm)
import RemoteData exposing (RemoteData)



{-
   Patch a node data
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


patchNode url form msg =
    makeGQLMutation url
        (Mutation.updateNode
            (nodePatchInputEncoder form)
            (SelectionSet.map PatchNodeIdPayload <|
                Fractal.Object.UpdateNodePayload.node identity <|
                    (SelectionSet.succeed IdPayload
                        |> with (Fractal.Object.Node.id |> SelectionSet.map decodedId)
                    )
            )
        )
        (RemoteData.fromResult >> decodeResponse nodePatchDecoder >> msg)


nodePatchInputEncoder : TensionForm -> Mutation.UpdateNodeRequiredArguments
nodePatchInputEncoder form =
    let
        createdAt =
            Dict.get "createdAt" form.post |> Maybe.map (\x -> Fractal.Scalar.DateTime x)

        patchRequired =
            { filter =
                Input.buildNodeFilter
                    (\f ->
                        { f | nameid = { eq = Present form.target.nameid, regexp = Absent } |> Present }
                    )
            }

        patchOpts =
            \x ->
                { set = buildNodePatch form
                , remove = Absent
                }
    in
    { input =
        Input.buildUpdateNodeInput patchRequired patchOpts
    }


buildNodePatch : TensionForm -> OptionalArgument Input.NodePatch
buildNodePatch f =
    let
        nf =
            f.node
    in
    Input.buildNodePatch
        (\n ->
            { n
                | name = fromMaybe nf.name
                , type_ = fromMaybe nf.type_
                , role_type = fromMaybe nf.role_type
                , about = fromMaybe nf.about
                , mandate = buildMandate nf.mandate
                , charac = nf.charac |> Maybe.map (\c -> { userCanJoin = Present c.userCanJoin, mode = Present c.mode, id = Absent }) |> fromMaybe
                , first_link =
                    nf.first_link
                        |> Maybe.map
                            (\uname ->
                                Input.buildUserRef
                                    (\u -> { u | username = Present uname })
                            )
                        |> fromMaybe
                , tensions_in =
                    [ Input.buildTensionRef (tensionFromForm f) ] |> Present
            }
        )
        |> Present
