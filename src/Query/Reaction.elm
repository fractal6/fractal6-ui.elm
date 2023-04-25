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


module Query.Reaction exposing
    ( addReaction
    , deleteReaction
    )

import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.AddReactionPayload
import Fractal.Object.Comment
import Fractal.Object.DeleteReactionPayload
import Fractal.Object.Reaction
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Maybe
import ModelSchema exposing (IdPayload, ReactionResponse, decodeResponse, decodedId, encodeId)
import RemoteData exposing (RemoteData)



--
-- Add reaction
--


type alias ReactionPayload =
    { reaction : Maybe (List (Maybe ReactionResponse)) }


reactionDecoder : Maybe ReactionPayload -> Maybe ReactionResponse
reactionDecoder data =
    data
        |> Maybe.andThen
            (\d ->
                d.reaction
                    |> Maybe.map List.head
                    |> Maybe.withDefault Nothing
                    |> Maybe.withDefault Nothing
            )


addReaction url username cid type_ msg =
    makeGQLMutation url
        (Mutation.addReaction
            identity
            (addReactionInputEncoder username cid type_)
            (SelectionSet.map ReactionPayload <|
                Fractal.Object.AddReactionPayload.reaction identity
                    reactionPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse reactionDecoder >> msg)


addReactionInputEncoder : String -> String -> Int -> Mutation.AddReactionRequiredArguments
addReactionInputEncoder username cid type_ =
    let
        inputReq =
            { user =
                Input.buildUserRef (\u -> { u | username = Present username })
            , comment =
                Input.buildCommentRef (\u -> { u | id = cid |> encodeId |> Present })
            , type_ = type_
            , reactionid = "" -- set in the backend
            }

        --inputOpt =
        --    \x -> x
    in
    { input = [ Input.buildAddReactionInput inputReq ] }


reactionPayload : SelectionSet ReactionResponse Fractal.Object.Reaction
reactionPayload =
    SelectionSet.map2 ReactionResponse
        (Fractal.Object.Reaction.comment identity (SelectionSet.map IdPayload (Fractal.Object.Comment.id |> SelectionSet.map decodedId))
            |> SelectionSet.map .id
        )
        Fractal.Object.Reaction.type_



--
-- Delete reaction
--


deleteReaction url username cid type_ msg =
    makeGQLMutation url
        (Mutation.deleteReaction
            (deleteReactionInputEncoder username cid type_)
            (SelectionSet.map ReactionPayload <|
                Fractal.Object.DeleteReactionPayload.reaction identity
                    reactionPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse reactionDecoder >> msg)


deleteReactionInputEncoder : String -> String -> Int -> Mutation.DeleteReactionRequiredArguments
deleteReactionInputEncoder username cid type_ =
    let
        reactionid =
            String.join "#" [ username, cid, String.fromInt type_ ]
    in
    { filter =
        Input.buildReactionFilter (\i -> { i | reactionid = Present { eq = Present reactionid, in_ = Absent } })
    }
