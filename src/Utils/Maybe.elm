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


module Utils.Maybe exposing (mor, morElse, unwrap, unwrap2)


{-| Returns the first value that is present, like the boolean `||`.
-}
mor : Maybe a -> Maybe a -> Maybe a
mor ma mb =
    case ma of
        Nothing ->
            mb

        Just _ ->
            ma


{-| Pipe-friendly version of `mor`: the fallback comes first so it reads as
`maybeX |> morElse fallback` — analogous to `Maybe.Extra.orElse`.
-}
morElse : Maybe a -> Maybe a -> Maybe a
morElse fallback m =
    mor m fallback


{-| Like using `Maybe.map f a |> withDefault default`
-}
unwrap : b -> (a -> b) -> Maybe a -> b
unwrap default f m =
    case m of
        Nothing ->
            default

        Just a ->
            f a


{-| Like using `Maybe.map f a |> withDefault Nothing |> withDefault default`
-}
unwrap2 : b -> (a -> Maybe b) -> Maybe a -> b
unwrap2 default f m =
    case m of
        Nothing ->
            default

        Just a ->
            f a |> Maybe.withDefault default
