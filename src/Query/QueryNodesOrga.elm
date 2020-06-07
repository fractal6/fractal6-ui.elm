module Query.QueryNodesOrga exposing (nodeOrgaPayload, queryGraphPack)

import Dict exposing (Dict)
import Fractal.InputObject as Input
import Fractal.Object
import Fractal.Object.Node
import Fractal.Object.NodeCharac
import Fractal.Object.User
import Fractal.Query as Query
import Fractal.Scalar
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import ModelSchema exposing (..)
import RemoteData exposing (RemoteData)



{-
   Query Organisation Nodes
-}


queryGraphPack rootid msg =
    makeGQLQuery
        (Query.queryNode
            (nodeOrgaFilter rootid)
            nodeOrgaPayload
        )
        (RemoteData.fromResult >> decodeResponse nodeOrgaDecoder >> msg)


nodeOrgaFilter : String -> Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
nodeOrgaFilter rootid a =
    { a
        | filter =
            OptionalArgument.Present
                (Input.buildNodeFilter
                    (\b ->
                        { b | rootnameid = OptionalArgument.Present { eq = OptionalArgument.Present rootid } }
                    )
                )
    }


nodeOrgaPayload : SelectionSet Node Fractal.Object.Node
nodeOrgaPayload =
    SelectionSet.succeed Node
        |> with (Fractal.Object.Node.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Node.createdAt |> SelectionSet.map decodedTime)
        |> with Fractal.Object.Node.name
        |> with Fractal.Object.Node.nameid
        |> with Fractal.Object.Node.rootnameid
        |> with
            --(Fractal.Object.Node.parent identity (SelectionSet.map (ParentNode << decodedId) Fractal.Object.Node.id))
            (Fractal.Object.Node.parent identity <| SelectionSet.map ParentNode Fractal.Object.Node.nameid)
        |> with Fractal.Object.Node.type_
        |> with Fractal.Object.Node.role_type
        |> with (Fractal.Object.Node.first_link identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with
            (Fractal.Object.Node.charac <|
                SelectionSet.map2 NodeCharac
                    Fractal.Object.NodeCharac.userCanJoin
                    Fractal.Object.NodeCharac.mode
            )



--- Response decoder


nodeOrgaDecoder : Maybe (List (Maybe Node)) -> Maybe (Dict String Node)
nodeOrgaDecoder data =
    data
        |> Maybe.map
            (\d ->
                if List.length d == 0 then
                    Nothing

                else
                    d
                        |> List.filterMap identity
                        |> List.map (\n -> ( n.nameid, n ))
                        |> Dict.fromList
                        |> Just
            )
        |> Maybe.withDefault Nothing
