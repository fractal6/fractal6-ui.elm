{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2026 Fractale Co

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
    , addOneProject
    , addOneProjectTemplate
    , addOneRole
    , addOneTensionTemplate
    , removeOneLabel
    , removeOneProject
    , removeOneProjectTemplate
    , removeOneRole
    , removeOneTensionTemplate
    , updateOneLabel
    , updateOneProject
    , updateOneProjectTemplate
    , updateOneRole
    , updateOneTensionTemplate
    )

import Codecs exposing (encodeColumnsJson)
import Fractale.Form exposing (ArtefactNodeForm, ProjectForm, ProjectTemplateForm, TensionTemplateForm)
import Fractale.Codecs exposing (nid2rootid)
import Dict
import Schema.Enum.ProjectColumnType as ProjectColumnType
import Schema.Enum.ProjectStatus as ProjectStatus
import Schema.InputObject as Input
import Schema.Mutation as Mutation
import Schema.Object
import Schema.Object.AddLabelPayload
import Schema.Object.AddProjectPayload
import Schema.Object.AddProjectTemplatePayload
import Schema.Object.AddRoleExtPayload
import Schema.Object.AddTensionTemplatePayload
import Schema.Object.UpdateLabelPayload
import Schema.Object.UpdateProjectPayload
import Schema.Object.UpdateProjectTemplatePayload
import Schema.Object.UpdateRoleExtPayload
import Schema.Object.UpdateTensionTemplatePayload
import Schema.Scalar
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Query.AddTension exposing (buildMandate)
import Query.QueryNode exposing (labelFullPayload, projectFullPayload, projectTemplateFullPayload, roleFullPayload, tensionTemplateFullPayload)
import RemoteData exposing (RemoteData)



--
-- We do not directtly operate on Node, but user referecne instead. (see @hasLink schema directive).
--


{-| Decoder for remove mutations that select `numUids`.

After detaching the only Node link, @auth(query) and/or @hook\_ strip the
returned record. `numUids` is not auth-filtered, so we use it to confirm the
mutation ran.

-}
removeNumUidsDecoder : Maybe (Maybe Int) -> Maybe String
removeNumUidsDecoder data =
    data
        |> Maybe.andThen identity
        |> Maybe.map String.fromInt



--
-- Node Label Operation
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
                    |> Maybe.map List.head
                    |> Maybe.withDefault Nothing
                    |> Maybe.withDefault Nothing
            )


addOneLabel url form msg =
    makeGQLMutation url
        (Mutation.addLabel
            (addLabelInputEncoder form)
            (SelectionSet.map LabelsFullPayload <|
                Schema.Object.AddLabelPayload.label identity labelFullPayload
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
                Schema.Object.UpdateLabelPayload.label identity <|
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
   Remove Label
-}


removeOneLabel url form msg =
    makeGQLMutation url
        (Mutation.updateLabel
            (removeLabelInputEncoder form)
            Schema.Object.UpdateLabelPayload.numUids
        )
        (RemoteData.fromResult >> decodeResponse removeNumUidsDecoder >> msg)


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
                    |> Maybe.map List.head
                    |> Maybe.withDefault Nothing
                    |> Maybe.withDefault Nothing
            )


addOneRole url form msg =
    makeGQLMutation url
        (Mutation.addRoleExt
            (addRoleInputEncoder form)
            (SelectionSet.map RolesFullPayload <|
                Schema.Object.AddRoleExtPayload.roleExt identity roleFullPayload
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
                Schema.Object.UpdateRoleExtPayload.roleExt identity <|
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
   Remove Role
-}


removeOneRole url form msg =
    makeGQLMutation url
        (Mutation.updateRoleExt
            (removeRoleInputEncoder form)
            Schema.Object.UpdateRoleExtPayload.numUids
        )
        (RemoteData.fromResult >> decodeResponse removeNumUidsDecoder >> msg)


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



--
-- Node Project Operation
--


buildUserRefs : List String -> OptionalArgument (List Input.UserRef)
buildUserRefs usernames =
    if List.isEmpty usernames then
        Absent

    else
        Present (List.map (\u -> Input.buildUserRef (\r -> { r | username = Present u })) usernames)


buildLabelRefs : List Label -> OptionalArgument (List Input.LabelRef)
buildLabelRefs labels =
    if List.isEmpty labels then
        Absent

    else
        Present (List.map (\l -> Input.buildLabelRef (\r -> { r | id = Present (encodeId l.id) })) labels)


buildAssigneeRefs : List User -> OptionalArgument (List Input.UserRef)
buildAssigneeRefs users =
    if List.isEmpty users then
        Absent

    else
        Present (List.map (\u -> Input.buildUserRef (\r -> { r | username = Present u.username })) users)



{-
   Add one Project
-}


type alias ProjectsFullPayload =
    { project : Maybe (List (Maybe ProjectFull)) }


projectFullDecoder : Maybe ProjectsFullPayload -> Maybe ProjectFull
projectFullDecoder data =
    data
        |> Maybe.andThen
            (\d ->
                d.project
                    |> Maybe.map List.head
                    |> Maybe.withDefault Nothing
                    |> Maybe.withDefault Nothing
            )


addOneProject url form msg =
    makeGQLMutation url
        (Mutation.addProject
            (addProjectInputEncoder form)
            (SelectionSet.map ProjectsFullPayload <|
                Schema.Object.AddProjectPayload.project identity projectFullPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse projectFullDecoder >> msg)


addProjectInputEncoder : ProjectForm -> Mutation.AddProjectRequiredArguments
addProjectInputEncoder form =
    let
        createdAt =
            Dict.get "createdAt" form.post |> withDefault "" |> Schema.Scalar.DateTime

        inputReq =
            { createdBy =
                Input.buildUserRef
                    (\x -> { x | username = Present form.uctx.username })
            , createdAt = createdAt
            , updatedAt = createdAt
            , rootnameid = nid2rootid form.nameid
            , parentnameid = form.nameid
            , name = Dict.get "name" form.post |> withDefault ""
            , nameid = Dict.get "nameid" form.post |> withDefault ""
            , status = withDefault ProjectStatus.Open form.status
            , peerCanEditProject = form.peerCanEditProject |> withDefault False
            , guestCanEditProject = form.guestCanEditProject |> withDefault False
            }

        inputOpt =
            \x ->
                { x
                    | description = fromMaybe (Dict.get "description" form.post)
                    , columns =
                        form.columns
                            |> Maybe.map
                                (\a ->
                                    List.indexedMap
                                        (\i c ->
                                            Input.buildProjectColumnRef
                                                (\b ->
                                                    { b
                                                        | name = Present c.name
                                                        , description = Present c.description
                                                        , color = fromMaybe c.color
                                                        , pos = Present i
                                                        , col_type = Present c.col_type
                                                    }
                                                )
                                        )
                                        a
                                )
                            |> fromMaybe
                    , nodes =
                        Present
                            [ Input.buildNodeRef (\n -> { n | nameid = Present form.nameid }) ]
                    , collaborators = buildUserRefs form.collaborators_add
                }
    in
    { input = [ Input.buildAddProjectInput inputReq inputOpt ] }



{-
   Update Project
-}


updateOneProject url form msg =
    makeGQLMutation url
        (Mutation.updateProject
            (updateProjectInputEncoder form)
            (SelectionSet.map ProjectsFullPayload <|
                Schema.Object.UpdateProjectPayload.project identity <|
                    projectFullPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse projectFullDecoder >> msg)


updateProjectInputEncoder : ProjectForm -> Mutation.UpdateProjectRequiredArguments
updateProjectInputEncoder form =
    let
        inputReq =
            { filter =
                if form.id == "" then
                    -- assumes duplicate update
                    -- see the duplicate err handler
                    Input.buildProjectFilter
                        (\i ->
                            { i
                                | parentnameid = Present { eq = Present form.nameid, in_ = Absent }
                                , nameid = Present { eq = fromMaybe (Dict.get "nameid" form.post), in_ = Absent }
                            }
                        )

                else
                    Input.buildProjectFilter (\i -> { i | id = Present [ encodeId form.id ] })
            }

        post =
            if form.id == "" then
                -- Just register the project in the given node
                -- see the duplicate err handler
                form.post
                    |> Dict.remove "name"
                    |> Dict.remove "nameid"
                    |> Dict.remove "description"

            else if Dict.get "name" form.post == Dict.get "old_name" form.post then
                -- remove name to avoid making extra request due to @unique
                form.post |> Dict.remove "name"

            else if Dict.get "nameid" form.post == Dict.get "old_nameid" form.post then
                -- remove a duplicate error when update post
                form.post |> Dict.remove "nameid"

            else
                form.post

        moveFrom =
            Dict.get "move_from" form.post

        inputOpt =
            \_ ->
                { set =
                    Input.buildProjectPatch
                        (\i ->
                            { i
                                | name = fromMaybe (Dict.get "name" post)
                                , nameid = fromMaybe (Dict.get "nameid" post)
                                , description = fromMaybe (Dict.get "description" post)
                                , status = fromMaybe form.status
                                , nodes =
                                    if form.id == "" then
                                        -- Only set nodes for duplicate handler (registering project in a node)
                                        Present [ Input.buildNodeRef (\n -> { n | nameid = Present form.nameid }) ]

                                    else if moveFrom /= Nothing then
                                        -- Move: attach to new parent node
                                        Present [ Input.buildNodeRef (\n -> { n | nameid = Present form.nameid }) ]

                                    else
                                        Absent
                                , collaborators =
                                    if List.isEmpty form.collaborators_add then
                                        Absent

                                    else
                                        Present (List.map (\u -> Input.buildUserRef (\r -> { r | username = Present u })) form.collaborators_add)
                                , peerCanEditProject = fromMaybe form.peerCanEditProject
                                , guestCanEditProject = fromMaybe form.guestCanEditProject
                            }
                        )
                        |> Present
                , remove =
                    if moveFrom /= Nothing || not (List.isEmpty form.collaborators_remove) then
                        Input.buildProjectPatch
                            (\i ->
                                { i
                                    | nodes =
                                        case moveFrom of
                                            Just oldNameid ->
                                                Present
                                                    [ Input.buildNodeRef (\j -> { j | nameid = Present oldNameid }) ]

                                            Nothing ->
                                                Absent
                                    , collaborators = buildUserRefs form.collaborators_remove
                                }
                            )
                            |> Present

                    else
                        Absent
                }
    in
    { input = Input.buildUpdateProjectInput inputReq inputOpt }



{-
   Remove Project
-}


removeOneProject url form msg =
    makeGQLMutation url
        (Mutation.updateProject
            (removeProjectInputEncoder form)
            Schema.Object.UpdateProjectPayload.numUids
        )
        (RemoteData.fromResult >> decodeResponse removeNumUidsDecoder >> msg)


removeProjectInputEncoder : ProjectForm -> Mutation.UpdateProjectRequiredArguments
removeProjectInputEncoder form =
    let
        inputReq =
            { filter =
                Input.buildProjectFilter (\i -> { i | id = Present [ encodeId form.id ] })
            }

        inputOpt =
            \_ ->
                { set = Absent
                , remove =
                    Input.buildProjectPatch
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
    { input = Input.buildUpdateProjectInput inputReq inputOpt }



--
-- Node TensionTemplate Operation
--
{-
   Add one Tension Template
-}


type alias TensionTemplatesFullPayload =
    { tensionTemplate : Maybe (List (Maybe TensionTemplateFull)) }


tensionTemplateMutDecoder : Maybe TensionTemplatesFullPayload -> Maybe TensionTemplateFull
tensionTemplateMutDecoder data =
    data
        |> Maybe.andThen
            (\d ->
                d.tensionTemplate
                    |> Maybe.map List.head
                    |> Maybe.withDefault Nothing
                    |> Maybe.withDefault Nothing
            )


addOneTensionTemplate url form msg =
    makeGQLMutation url
        (Mutation.addTensionTemplate
            (addTensionTemplateInputEncoder form)
            (SelectionSet.map TensionTemplatesFullPayload <|
                Schema.Object.AddTensionTemplatePayload.tensionTemplate identity tensionTemplateFullPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse tensionTemplateMutDecoder >> msg)


addTensionTemplateInputEncoder : TensionTemplateForm -> Mutation.AddTensionTemplateRequiredArguments
addTensionTemplateInputEncoder form =
    let
        inputReq =
            { rootnameid = nid2rootid form.nameid
            , name = Dict.get "name" form.post |> withDefault ""
            , is_recursive = form.is_recursive
            , title = Dict.get "title" form.post |> withDefault ""
            , comment = Dict.get "comment" form.post |> withDefault ""
            , type_ = form.type_
            }

        inputOpt =
            \x ->
                { x
                    | nodes =
                        Present
                            [ Input.buildNodeRef (\n -> { n | nameid = Present form.nameid }) ]
                    , description = fromMaybe form.description
                    , labels = buildLabelRefs form.labels
                    , assignees = buildAssigneeRefs form.assignees
                }
    in
    { input = [ Input.buildAddTensionTemplateInput inputReq inputOpt ] }



{-
   Update Tension Template
-}


updateOneTensionTemplate url form msg =
    makeGQLMutation url
        (Mutation.updateTensionTemplate
            (updateTensionTemplateInputEncoder form)
            (SelectionSet.map TensionTemplatesFullPayload <|
                Schema.Object.UpdateTensionTemplatePayload.tensionTemplate identity <|
                    tensionTemplateFullPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse tensionTemplateMutDecoder >> msg)


updateTensionTemplateInputEncoder : TensionTemplateForm -> Mutation.UpdateTensionTemplateRequiredArguments
updateTensionTemplateInputEncoder form =
    let
        inputReq =
            { filter =
                Input.buildTensionTemplateFilter (\i -> { i | id = Present [ encodeId form.id ] })
            }

        post =
            if Dict.get "name" form.post == Dict.get "old_name" form.post then
                form.post |> Dict.remove "name"

            else
                form.post

        addedLabels =
            List.filter (\l -> not (List.any (\ol -> ol.id == l.id) form.orig_labels)) form.labels

        removedLabels =
            List.filter (\ol -> not (List.any (\l -> l.id == ol.id) form.labels)) form.orig_labels

        addedAssignees =
            List.filter (\u -> not (List.any (\ou -> ou.username == u.username) form.orig_assignees)) form.assignees

        removedAssignees =
            List.filter (\ou -> not (List.any (\u -> u.username == ou.username) form.assignees)) form.orig_assignees

        hasRemovals =
            not (List.isEmpty removedLabels && List.isEmpty removedAssignees)

        inputOpt =
            \_ ->
                { set =
                    Input.buildTensionTemplatePatch
                        (\i ->
                            { i
                                | name = fromMaybe (Dict.get "name" post)
                                , description = fromMaybe form.description
                                , title = fromMaybe (Dict.get "title" post)
                                , comment = fromMaybe (Dict.get "comment" post)
                                , type_ = Present form.type_
                                , is_recursive = Present form.is_recursive
                                , nodes = Present [ Input.buildNodeRef (\n -> { n | nameid = Present form.nameid }) ]
                                , labels = buildLabelRefs addedLabels
                                , assignees = buildAssigneeRefs addedAssignees
                            }
                        )
                        |> Present
                , remove =
                    if hasRemovals then
                        Input.buildTensionTemplatePatch
                            (\i ->
                                { i
                                    | labels = buildLabelRefs removedLabels
                                    , assignees = buildAssigneeRefs removedAssignees
                                }
                            )
                            |> Present

                    else
                        Absent
                }
    in
    { input = Input.buildUpdateTensionTemplateInput inputReq inputOpt }



{-
   Remove Tension Template
-}


removeOneTensionTemplate url form msg =
    makeGQLMutation url
        (Mutation.updateTensionTemplate
            (removeTensionTemplateInputEncoder form)
            Schema.Object.UpdateTensionTemplatePayload.numUids
        )
        (RemoteData.fromResult >> decodeResponse removeNumUidsDecoder >> msg)


removeTensionTemplateInputEncoder : TensionTemplateForm -> Mutation.UpdateTensionTemplateRequiredArguments
removeTensionTemplateInputEncoder form =
    let
        inputReq =
            { filter =
                Input.buildTensionTemplateFilter (\i -> { i | id = Present [ encodeId form.id ] })
            }

        inputOpt =
            \_ ->
                { set = Absent
                , remove =
                    Input.buildTensionTemplatePatch
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
    { input = Input.buildUpdateTensionTemplateInput inputReq inputOpt }



--
-- Node ProjectTemplate Operation
--
{-
   Add one Project Template
-}


type alias ProjectTemplatesFullPayload =
    { projectTemplate : Maybe (List (Maybe ProjectTemplateFull)) }


projectTemplateMutDecoder : Maybe ProjectTemplatesFullPayload -> Maybe ProjectTemplateFull
projectTemplateMutDecoder data =
    data
        |> Maybe.andThen
            (\d ->
                d.projectTemplate
                    |> Maybe.map List.head
                    |> Maybe.withDefault Nothing
                    |> Maybe.withDefault Nothing
            )


addOneProjectTemplate url form msg =
    makeGQLMutation url
        (Mutation.addProjectTemplate
            (addProjectTemplateInputEncoder form)
            (SelectionSet.map ProjectTemplatesFullPayload <|
                Schema.Object.AddProjectTemplatePayload.projectTemplate identity projectTemplateFullPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse projectTemplateMutDecoder >> msg)


addProjectTemplateInputEncoder : ProjectTemplateForm -> Mutation.AddProjectTemplateRequiredArguments
addProjectTemplateInputEncoder form =
    let
        inputReq =
            { rootnameid = nid2rootid form.nameid
            , name = Dict.get "name" form.post |> withDefault ""
            , is_recursive = form.is_recursive
            , columns_json = encodeColumnsJson form.columns
            }

        inputOpt =
            \x ->
                { x
                    | nodes =
                        Present
                            [ Input.buildNodeRef (\n -> { n | nameid = Present form.nameid }) ]
                    , description = fromMaybe form.description
                }
    in
    { input = [ Input.buildAddProjectTemplateInput inputReq inputOpt ] }



{-
   Update Project Template
-}


updateOneProjectTemplate url form msg =
    makeGQLMutation url
        (Mutation.updateProjectTemplate
            (updateProjectTemplateInputEncoder form)
            (SelectionSet.map ProjectTemplatesFullPayload <|
                Schema.Object.UpdateProjectTemplatePayload.projectTemplate identity <|
                    projectTemplateFullPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse projectTemplateMutDecoder >> msg)


updateProjectTemplateInputEncoder : ProjectTemplateForm -> Mutation.UpdateProjectTemplateRequiredArguments
updateProjectTemplateInputEncoder form =
    let
        inputReq =
            { filter =
                Input.buildProjectTemplateFilter (\i -> { i | id = Present [ encodeId form.id ] })
            }

        post =
            if Dict.get "name" form.post == Dict.get "old_name" form.post then
                form.post |> Dict.remove "name"

            else
                form.post

        inputOpt =
            \_ ->
                { set =
                    Input.buildProjectTemplatePatch
                        (\i ->
                            { i
                                | name = fromMaybe (Dict.get "name" post)
                                , description = fromMaybe form.description
                                , is_recursive = Present form.is_recursive
                                , columns_json = Present (encodeColumnsJson form.columns)
                                , nodes = Present [ Input.buildNodeRef (\n -> { n | nameid = Present form.nameid }) ]
                            }
                        )
                        |> Present
                , remove = Absent
                }
    in
    { input = Input.buildUpdateProjectTemplateInput inputReq inputOpt }



{-
   Remove Project Template
-}


removeOneProjectTemplate url form msg =
    makeGQLMutation url
        (Mutation.updateProjectTemplate
            (removeProjectTemplateInputEncoder form)
            Schema.Object.UpdateProjectTemplatePayload.numUids
        )
        (RemoteData.fromResult >> decodeResponse removeNumUidsDecoder >> msg)


removeProjectTemplateInputEncoder : ProjectTemplateForm -> Mutation.UpdateProjectTemplateRequiredArguments
removeProjectTemplateInputEncoder form =
    let
        inputReq =
            { filter =
                Input.buildProjectTemplateFilter (\i -> { i | id = Present [ encodeId form.id ] })
            }

        inputOpt =
            \_ ->
                { set = Absent
                , remove =
                    Input.buildProjectTemplatePatch
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
    { input = Input.buildUpdateProjectTemplateInput inputReq inputOpt }
