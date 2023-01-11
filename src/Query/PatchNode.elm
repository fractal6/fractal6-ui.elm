{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2023 Fractale Co

   This file is part of Fractale.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
-}


module Query.PatchNode exposing
    ( addOneLabel
    , addOneRole
    , removeOneLabel
    , removeOneRole
    , updateOneLabel
    , updateOneRole
    )

import Bulk exposing (ArtefactNodeForm)
import Bulk.Codecs exposing (nid2rootid)
import Dict exposing (Dict)
import Fractal.Enum.RoleType as RoleType
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.AddLabelPayload
import Fractal.Object.AddRoleExtPayload
import Fractal.Object.DeleteLabelPayload
import Fractal.Object.DeleteRoleExtPayload
import Fractal.Object.Node
import Fractal.Object.UpdateLabelPayload
import Fractal.Object.UpdateNodePayload
import Fractal.Object.UpdateRoleExtPayload
import Fractal.Object.User
import Fractal.Scalar
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Query.AddTension exposing (buildMandate)
import Query.QueryNode exposing (labelFullPayload, roleFullPayload)
import RemoteData exposing (RemoteData)



--
-- We do not directtly operate on Node, but user referecne instead. (see @hasLink schema directive).
--
--
-- Node LABEL Operation
--
{-
   Add one Label
-}


type alias LabelsFullPayload =
    { label : Maybe (List (Maybe LabelFull)) }


labelFullDecoder : Maybe LabelsFullPayload -> Maybe LabelFull
labelFullDecoder data =
    data
        |> Maybe.andThen
            (\d ->
                d.label
                    |> Maybe.map (\x -> List.head x)
                    |> Maybe.withDefault Nothing
                    |> Maybe.withDefault Nothing
            )


addOneLabel url form msg =
    makeGQLMutation url
        (Mutation.addLabel
            (addLabelInputEncoder form)
            (SelectionSet.map LabelsFullPayload <|
                Fractal.Object.AddLabelPayload.label identity labelFullPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse labelFullDecoder >> msg)


addLabelInputEncoder : ArtefactNodeForm -> Mutation.AddLabelRequiredArguments
addLabelInputEncoder form =
    let
        inputReq =
            { rootnameid = nid2rootid form.nameid
            , name = Dict.get "name" form.post |> withDefault ""
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


updateLabelInputEncoder : ArtefactNodeForm -> Mutation.UpdateLabelRequiredArguments
updateLabelInputEncoder form =
    let
        inputReq =
            { filter =
                if form.id == "" then
                    -- assumes duplicate update
                    -- see the duplicate err handler
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
                -- see the duplicate err handler
                form.post
                    |> Dict.remove "name"
                    |> Dict.remove "color"
                    |> Dict.remove "description"

            else if Dict.get "name" form.post == Dict.get "old_name" form.post then
                -- remove name to avoid making extra request due to @unique
                form.post |> Dict.remove "name"

            else
                form.post

        inputOpt =
            \_ ->
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


removeLabelInputEncoder : ArtefactNodeForm -> Mutation.UpdateLabelRequiredArguments
removeLabelInputEncoder form =
    let
        inputReq =
            { filter =
                Input.buildLabelFilter (\i -> { i | id = Present [ encodeId form.id ] })
            }

        inputOpt =
            \_ ->
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



--
-- Node RoleExt Operation
--
{-
   Add one Role
-}


type alias RolesFullPayload =
    { role : Maybe (List (Maybe RoleExtFull)) }


roleFullDecoder : Maybe RolesFullPayload -> Maybe RoleExtFull
roleFullDecoder data =
    data
        |> Maybe.andThen
            (\d ->
                d.role
                    |> Maybe.map (\x -> List.head x)
                    |> Maybe.withDefault Nothing
                    |> Maybe.withDefault Nothing
            )


addOneRole url form msg =
    makeGQLMutation url
        (Mutation.addRoleExt
            (addRoleInputEncoder form)
            (SelectionSet.map RolesFullPayload <|
                Fractal.Object.AddRoleExtPayload.roleExt identity roleFullPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse roleFullDecoder >> msg)


addRoleInputEncoder : ArtefactNodeForm -> Mutation.AddRoleExtRequiredArguments
addRoleInputEncoder form =
    let
        inputReq =
            { rootnameid = nid2rootid form.nameid
            , name = Dict.get "name" form.post |> withDefault ""
            , role_type = form.role_type
            }

        inputOpt =
            \x ->
                { x
                    | color = fromMaybe (Dict.get "color" form.post)
                    , about = fromMaybe (Dict.get "about" form.post)
                    , mandate = buildMandate form.mandate |> Present
                    , nodes =
                        Present
                            [ Input.buildNodeRef (\n -> { n | nameid = Present form.nameid }) ]
                }
    in
    { input = [ Input.buildAddRoleExtInput inputReq inputOpt ] }



{-
   Update Role
-}


updateOneRole url form msg =
    makeGQLMutation url
        (Mutation.updateRoleExt
            (updateRoleInputEncoder form)
            (SelectionSet.map RolesFullPayload <|
                Fractal.Object.UpdateRoleExtPayload.roleExt identity <|
                    roleFullPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse roleFullDecoder >> msg)


updateRoleInputEncoder : ArtefactNodeForm -> Mutation.UpdateRoleExtRequiredArguments
updateRoleInputEncoder form =
    let
        inputReq =
            { filter =
                if form.id == "" then
                    -- assumes duplicate update
                    -- see the duplicate err handler
                    Input.buildRoleExtFilter
                        (\i ->
                            { i
                                | rootnameid = Present { eq = Present (nid2rootid form.nameid), in_ = Absent }
                                , name = Present { eq = fromMaybe (Dict.get "name" form.post), in_ = Absent, anyofterms = Absent, allofterms = Absent }
                            }
                        )

                else
                    Input.buildRoleExtFilter (\i -> { i | id = Present [ encodeId form.id ] })
            }

        post =
            if form.id == "" then
                -- Just register the role in the given node
                -- see the duplicate err handler
                form.post
                    |> Dict.remove "name"
                    |> Dict.remove "color"
                    |> Dict.remove "about"

            else if Dict.get "name" form.post == Dict.get "old_name" form.post then
                -- remove name to avoid making extra request due to @unique
                form.post |> Dict.remove "name"

            else
                form.post

        inputOpt =
            \_ ->
                { set =
                    Input.buildRoleExtPatch
                        (\i ->
                            { i
                                | name = fromMaybe (Dict.get "name" post)
                                , color = fromMaybe (Dict.get "color" post)
                                , about = fromMaybe (Dict.get "about" post)
                                , mandate =
                                    if form.id == "" then
                                        Absent

                                    else
                                        Present (buildMandate form.mandate)
                                , role_type = Present form.role_type
                                , nodes = Present [ Input.buildNodeRef (\n -> { n | nameid = Present form.nameid }) ]
                            }
                        )
                        |> Present
                , remove = Absent
                }
    in
    { input = Input.buildUpdateRoleExtInput inputReq inputOpt }



{-
   Remove role
-}


removeOneRole url form msg =
    makeGQLMutation url
        (Mutation.updateRoleExt
            (removeRoleInputEncoder form)
            (SelectionSet.map RolesFullPayload <|
                Fractal.Object.UpdateRoleExtPayload.roleExt identity <|
                    roleFullPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse roleFullDecoder >> msg)


removeRoleInputEncoder : ArtefactNodeForm -> Mutation.UpdateRoleExtRequiredArguments
removeRoleInputEncoder form =
    let
        inputReq =
            { filter =
                Input.buildRoleExtFilter (\i -> { i | id = Present [ encodeId form.id ] })
            }

        inputOpt =
            \_ ->
                { set = Absent
                , remove =
                    Input.buildRoleExtPatch
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
    { input = Input.buildUpdateRoleExtInput inputReq inputOpt }
