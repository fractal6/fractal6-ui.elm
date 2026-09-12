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


module Fractale.Graph exposing
    ( countOpenTensions
    , getChildren
    , getNode
    , getNodeName
    , getOwners
    , getParent
    , getParentFragmentFromRole
    , getParentId
    , getParents
    , getPath
    , getPathWithChildren
    , isFreshOrga
    , isPinnedRecursivelyOn
    , isRootArchivedOn
    , localGraphFromOrga
    , maxPinnedTensions
    , mergePinnedTensions
    , nodeFromTension
    , sortNode
    , tidFromPath
    , treeMatchesFocus
    , withDescendants
    )

import Dict
import Dict.Extra as DE
import Fractale.Codecs exposing (nearestCircleid, nid2rootid)
import List.Extra as LE
import Loading exposing (GqlData, RequestResult(..), withDefaultData, withMaybeData, withMaybeMapData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Schema.Enum.RoleType as RoleType
import Utils.Maybe exposing (mor)


{-| Nothing is packed inside the root circle: Owner/Member roles are not drawn (see formatGraph
in graphpack\_d3.js), and the orga query filters them out of the tree anyway.
-}
isFreshOrga : String -> NodesDict -> Bool
isFreshOrga rootnameid data =
    data
        |> Dict.filter
            (\_ n ->
                (Maybe.map .nameid n.parent == Just rootnameid)
                    && (n.role_type /= Just RoleType.Owner)
                    && (n.role_type /= Just RoleType.Member)
            )
        |> Dict.isEmpty


countOpenTensions : NodesDict -> Int
countOpenTensions =
    Dict.foldl (\_ n acc -> n.n_open_tensions + acc) 0


treeMatchesFocus : Maybe { a | rootnameid : String } -> Maybe NodesDict -> Bool
treeMatchesFocus maybeFocus maybeTree =
    -- Only block when focus and tree are both known AND disagree on the org.
    -- A missing focus (e.g. right after navigating from a non-org page) must
    -- not stall orgaInfo updates that race ahead of UpdateSessionFocus.
    case ( maybeFocus, maybeTree ) of
        ( Just f, Just t ) ->
            Dict.member f.rootnameid t

        _ ->
            True


getPath : GqlData LocalGraph -> List PNode
getPath lg =
    case lg of
        Success path ->
            path.path

        _ ->
            []


{-| Nameids in scope for focused-node search panels: path (root → focus)
plus direct children of focus.
-}
getPathWithChildren : GqlData LocalGraph -> List String
getPathWithChildren lg =
    case lg of
        Success g ->
            List.map .nameid g.path ++ List.map .nameid g.focus.children

        _ ->
            []


getParent : LocalGraph -> Maybe String
getParent lg =
    case List.reverse lg.path of
        _ :: x :: _ ->
            Just x.nameid

        _ ->
            Nothing


getNode : String -> GqlData NodesDict -> Maybe Node
getNode nameid orga =
    case orga of
        Success nodes ->
            Dict.get nameid nodes

        _ ->
            Nothing


getNodeName : String -> GqlData NodesDict -> String
getNodeName nameid orga =
    let
        errMsg =
            "Error: Node unknown"
    in
    case orga of
        Success nodes ->
            Dict.get nameid nodes
                |> Maybe.map .name
                |> withDefault errMsg

        _ ->
            errMsg


getParentId : String -> GqlData NodesDict -> Maybe String
getParentId nameid odata =
    case odata of
        Success data ->
            data
                |> Dict.get nameid
                |> Maybe.map .parent
                |> withDefault Nothing
                |> Maybe.map .nameid

        _ ->
            Nothing


getParents : String -> GqlData NodesDict -> List Node
getParents nameid odata =
    case odata of
        Success data ->
            case Maybe.map .parent (Dict.get nameid data) |> withDefault Nothing of
                Just p ->
                    case getNode p.nameid odata of
                        Just n ->
                            [ n ] ++ getParents p.nameid odata

                        Nothing ->
                            getParents p.nameid odata

                Nothing ->
                    []

        _ ->
            []


getChildren : String -> GqlData NodesDict -> List Node
getChildren nid odata =
    let
        parentid =
            nearestCircleid nid
    in
    odata
        |> withMaybeMapData
            (\x ->
                x |> Dict.values |> List.filter (\n -> n.first_link /= Nothing && (Just parentid == Maybe.map .nameid n.parent))
            )
        |> withDefault []


{-| Expand nameids with all their descendants, by walking the parent links.
(the nameid does not encode the path: circles are flat, only roles carry their parent circle)
-}
withDescendants : List String -> GqlData NodesDict -> List String
withDescendants nameids odata =
    case odata of
        Success data ->
            let
                childrenOf : List String -> List String -> List String
                childrenOf acc parents =
                    Dict.values data
                        |> List.filterMap
                            (\n ->
                                if List.member (Maybe.map .nameid n.parent |> withDefault "") parents && not (List.member n.nameid acc) then
                                    Just n.nameid

                                else
                                    Nothing
                            )

                go : List String -> List String -> List String
                go acc frontier =
                    case childrenOf acc frontier of
                        [] ->
                            acc

                        children ->
                            go (acc ++ children) children
            in
            go nameids nameids

        _ ->
            nameids


getOwners : GqlData NodesDict -> List Node
getOwners odata =
    odata
        |> withMaybeMapData
            (\x ->
                x |> Dict.values |> List.filter (\n -> n.role_type == Just RoleType.Owner)
            )
        |> withDefault []


getParentFragmentFromRole : { r | nameid : String } -> String
getParentFragmentFromRole r =
    case String.split "#" r.nameid of
        [ a, b, c ] ->
            if b == "" then
                a

            else
                b

        other ->
            LE.last other |> withDefault ""


nodeFromTension t =
    t.blobs
        |> withDefault []
        |> List.head
        |> Maybe.map .node
        |> withDefault Nothing
        |> withDefault (initNodeFragment Nothing)


tidFromPath : GqlData LocalGraph -> Maybe String
tidFromPath path =
    case path of
        Success p ->
            p.focus.source
                |> Maybe.map .tension
                |> Maybe.map .id

        _ ->
            Nothing


{-| Cap on the number of TaggedPin entries returned by mergePinnedTensions.
The view shows (max - 1) and uses the extra slot to detect overflow for a
discrete "more" indicator.
-}
maxPinnedTensions : Int
maxPinnedTensions =
    21


{-| The tree wins when loaded: it is patched in place on archive/unarchive, so the state flips
without a refetch. It is lazy-loaded outside of Overview though, hence the path fallback.
-}
isRootArchivedOn : String -> GqlData LocalGraph -> GqlData NodesDict -> Bool
isRootArchivedOn nameid path_data tree_data =
    mor
        (getNode (nid2rootid nameid) tree_data |> Maybe.andThen .isRootArchived)
        (withMaybeData path_data |> Maybe.andThen .root |> Maybe.andThen .isRootArchived)
        == Just True


isPinnedRecursivelyOn : GqlData LocalGraph -> Bool
isPinnedRecursivelyOn path_data =
    withMaybeData path_data
        |> Maybe.andThen .root
        |> Maybe.andThen .isPinnedTensionfetchRecursively
        |> (==) (Just True)


mergePinnedTensions : GqlData LocalGraph -> GqlData (List NodeWithPins) -> List TaggedPin
mergePinnedTensions path_data pinned_sub =
    let
        focusPins =
            withMaybeMapData (.focus >> .pinned >> withDefaultData Nothing) path_data
                |> Maybe.andThen identity
                |> withDefault []
                |> List.map (\p -> ( Nothing, p ))

        subPins =
            withMaybeData pinned_sub
                |> withDefault []
                |> List.concatMap
                    (\n ->
                        n.pinned
                            |> withDefault []
                            |> List.map (\p -> ( Just (nodeWithPinsToEoR n), p ))
                    )
    in
    (focusPins ++ subPins)
        |> LE.uniqueBy (\( _, p ) -> p.id)
        |> List.take maxPinnedTensions


localGraphFromOrga : String -> GqlData NodesDict -> Maybe LocalGraph
localGraphFromOrga nameid orga_d =
    case orga_d of
        Success orga ->
            let
                root_m =
                    Dict.get (nid2rootid nameid) orga
                        |> Maybe.map
                            (\n ->
                                { name = n.name
                                , nameid = n.nameid
                                , userCanJoin = n.userCanJoin
                                , mode = n.mode
                                , isTemplateTensionOnly = Nothing
                                , isPinnedTensionfetchRecursively = Nothing
                                , isRootArchived = n.isRootArchived
                                }
                            )

                focus_m =
                    Dict.get nameid orga
                        |> Maybe.map
                            (\n ->
                                { name = n.name
                                , nameid = n.nameid
                                , type_ = n.type_
                                , visibility = n.visibility
                                , mode = n.mode
                                , source = n.source
                                , children =
                                    DE.filterMap
                                        (\_ c ->
                                            Maybe.map
                                                (\p ->
                                                    if p.nameid == nameid then
                                                        Just { name = c.name, nameid = c.nameid, role_type = c.role_type, color = c.color }

                                                    else
                                                        Nothing
                                                )
                                                c.parent
                                                |> withDefault Nothing
                                        )
                                        orga
                                        |> Dict.values
                                , pinned = NotAsked
                                }
                            )

                buildPath : String -> List PNode
                buildPath nameid_ =
                    Dict.get nameid_ orga
                        |> Maybe.map
                            (\n ->
                                { name = n.name
                                , nameid = n.nameid
                                , source = n.source
                                }
                                    :: (case n.parent of
                                            Just p ->
                                                buildPath p.nameid

                                            Nothing ->
                                                []
                                       )
                            )
                        |> withDefault []
            in
            Maybe.map
                (\f ->
                    { root = root_m
                    , path = buildPath nameid |> List.reverse
                    , focus = f
                    }
                )
                focus_m

        _ ->
            Nothing


{-| First Circle then role, each group sorted alphabetically.
-}
sortNode a b =
    let
        len_a =
            List.length (String.split "#" a.nameid)

        len_b =
            List.length (String.split "#" b.nameid)
    in
    if len_a < len_b then
        LT

    else if len_a == len_b then
        if a.nameid < b.nameid then
            LT

        else
            GT

    else
        GT
