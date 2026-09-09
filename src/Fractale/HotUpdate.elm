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


module Fractale.HotUpdate exposing
    ( hotNodeInsert
    , hotNodeMove
    , hotNodePull
    , hotNodePush
    , hotTensionPush
    , hotTensionPush2
    , pushCommentReaction
    , removeCommentReaction
    )

import Dict
import Dict.Extra as DE
import Fractale.Graph exposing (withDescendants)
import List.Extra as LE
import Loading exposing (GqlData, RequestResult(..))
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Set


hotNodeInsert : Node -> GqlData NodesDict -> NodesDict
hotNodeInsert node odata =
    case odata of
        Success data ->
            Dict.insert node.nameid node data

        _ ->
            Dict.empty


hotNodePush : List Node -> GqlData NodesDict -> NodesDict
hotNodePush nodes odata =
    case odata of
        Success data ->
            Dict.union (List.map (\n -> ( n.nameid, n )) nodes |> Dict.fromList) data

        _ ->
            Dict.empty


hotNodePull : List String -> GqlData NodesDict -> ( NodesDict, Maybe Node )
hotNodePull nameids odata =
    -- return the first node removed
    case odata of
        Success data ->
            ( data |> DE.removeMany (Set.fromList nameids)
            , List.head nameids |> Maybe.map (\nid -> Dict.get nid data) |> withDefault Nothing
            )

        _ ->
            ( Dict.empty, Nothing )


hotNodeMove : String -> String -> String -> GqlData NodesDict -> ( NodesDict, Dict.Dict String String )
hotNodeMove oldNameid parentNameid newNameid odata =
    case odata of
        Success data ->
            case ( Dict.get oldNameid data, Dict.get parentNameid data ) of
                ( Just node, Just parent ) ->
                    if newNameid == "" then
                        ( data, Dict.empty )

                    else
                        let
                            -- Circles have flat IDs; moving within an org only renames a moved role.
                            renames =
                                withDescendants [ oldNameid ] odata
                                    |> List.map (\nid -> ( nid, nid ))
                                    |> Dict.fromList
                                    |> Dict.insert oldNameid newNameid

                            moved =
                                { node | nameid = newNameid, parent = Just { nameid = parentNameid, source = parent.source } }
                        in
                        ( data |> Dict.remove oldNameid |> Dict.insert newNameid moved, renames )

                _ ->
                    ( data, Dict.empty )

        _ ->
            ( Dict.empty, Dict.empty )


hotTensionPush : Tension -> GqlData (List Tension) -> List Tension
hotTensionPush tension tsData =
    case tsData of
        Success data ->
            tension :: data

        _ ->
            []


hotTensionPush2 : Tension -> GqlData TensionsDict -> TensionsDict
hotTensionPush2 tension tsData =
    case tsData of
        Success data ->
            let
                ts =
                    tension :: (Dict.get tension.receiver.nameid data |> withDefault [])
            in
            Dict.insert tension.receiver.nameid ts data

        _ ->
            Dict.empty


{-| Nested Remote Data Helpers (take too much space on main program, elm I still love you!)
-}
pushCommentReaction : String -> ReactionResponse -> List Comment -> List Comment
pushCommentReaction username r comments =
    LE.updateIf (\x -> x.id == r.cid)
        (\comment ->
            case LE.findIndex (\c -> c.type_ == r.type_) comment.reactions of
                Just i ->
                    { comment
                        | reactions =
                            LE.updateAt i
                                (\reaction -> { reaction | users = reaction.users ++ [ username ] })
                                comment.reactions
                    }

                Nothing ->
                    { comment | reactions = { type_ = r.type_, users = [ username ] } :: comment.reactions |> List.sortBy .type_ }
        )
        comments


removeCommentReaction : String -> ReactionResponse -> List Comment -> List Comment
removeCommentReaction username r comments =
    LE.updateIf (\x -> x.id == r.cid)
        (\comment ->
            case LE.findIndex (\c -> c.type_ == r.type_) comment.reactions of
                Just i ->
                    { comment
                        | reactions =
                            LE.updateAt i
                                (\reaction -> { reaction | users = LE.remove username reaction.users })
                                comment.reactions
                    }

                Nothing ->
                    comment
        )
        comments
