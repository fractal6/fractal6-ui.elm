----------------------------------------------------------------------
--
-- IdSearch.elm
-- Search for substrings in record identification strings.
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module QuickSearch exposing
    ( Table, makeTable
    , insert, insertList, lookup, remove
    , findSubstrings
    )

{-| Indexes a set of records by identifying strings.


## Create a table

@docs Table, makeTable


## Insert and lookup

@docs insert, insertList, lookup, remove


## Utilities

@docs findSubstrings

-}

import Dict exposing (Dict)
import Dict.Extra as DE
import List.Extra as LE


{-| The table type.
-}
type alias Table a =
    { getIdentifiers : a -> List String
    , dictCount : Int
    , dicts : List (Dict String (List a))
    }


{-| Make a table.

    makeTable dictCount getIdentifiers

`dictCount` is the number of dictionaries to populate. The dictionaries map prefixes of the search string to matches. If the search string is longer than `dictCount`, then a linear search is done on the results from the final dictionary.

This provides a tradeoff between insertion speed, storage space, and lookup speed.

A `dictCount` of 3 or 4 is probably right for most applications.

The list of strings returned by `getIdentifiers` are the indices for an inserted element. Usually, there will only be one.

-}
makeTable : Int -> (a -> List String) -> Table a
makeTable dictCount getIdentifiers =
    let
        cnt =
            max dictCount 1
    in
    { getIdentifiers = getIdentifiers
    , dictCount = cnt
    , dicts = List.map (\_ -> Dict.empty) <| List.range 1 cnt
    }


{-| Insert a record in the table.
-}
insert : a -> Table a -> Table a
insert a table =
    let
        identifiers =
            table.getIdentifiers a

        dictInsert substr d =
            let
                elts =
                    Dict.get substr d
                        |> Maybe.withDefault []
            in
            if List.member a elts then
                d

            else
                Dict.insert substr (a :: elts) d

        loop : Int -> List (Dict String (List a)) -> List (Dict String (List a)) -> List (Dict String (List a))
        loop idx dicts res =
            case dicts of
                [] ->
                    List.reverse res

                dict :: moreDicts ->
                    let
                        substrs =
                            List.map (findSubstrings idx) identifiers
                                |> List.concat

                        newDict =
                            List.foldl dictInsert dict substrs
                    in
                    loop (idx + 1) moreDicts (newDict :: res)
    in
    { table
        | dicts = loop 1 table.dicts []
    }


{-| Insert multiple elements into a table.
-}
insertList : List a -> Table a -> Table a
insertList list table =
    List.foldl insert table list


{-| Lookup a string in a table.
-}
lookup : String -> Table a -> List a
lookup string table =
    let
        len =
            String.length string

        dictCount =
            table.dictCount

        ( idx, key ) =
            if dictCount < len then
                ( dictCount - 1, String.left dictCount string )

            else
                ( len - 1, string )

        dict =
            LE.getAt idx table.dicts
                |> Maybe.withDefault Dict.empty
    in
    case Dict.get key dict of
        Nothing ->
            []

        Just hits ->
            if dictCount == len then
                hits

            else
                List.filter
                    (\hit ->
                        let
                            ids =
                                table.getIdentifiers hit
                        in
                        case LE.find (\id -> String.contains string id) ids of
                            Nothing ->
                                False

                            Just _ ->
                                True
                    )
                    hits


{-| Remove a record from a table.

This implementation is slow, looping over every element of every dictionary, since I don't expect it to be used much.

-}
remove : a -> Table a -> Table a
remove a table =
    let
        removeOne : Dict String (List a) -> Dict String (List a)
        removeOne dict =
            DE.filterMap
                (\_ l ->
                    if List.member a l then
                        case LE.remove a l of
                            [] ->
                                Nothing

                            l2 ->
                                Just l2

                    else
                        Just l
                )
                dict
    in
    { table
        | dicts = List.map removeOne table.dicts
    }


{-| Get a string's substrings of a given length.

If the length is 1, returns only the first character in the string.

None of the substrings will contain spaces. Each element in the returned list will be unique.

-}
findSubstrings : Int -> String -> List String
findSubstrings length string =
    if length == 0 then
        []

    else if length == 1 then
        if String.length string > 0 then
            [ String.left 1 string ]

        else
            []

    else
        let
            maxidx =
                String.length string - length

            loop : Int -> String -> List String -> List String
            loop idx tail res =
                if idx > maxidx then
                    List.reverse res

                else
                    let
                        substr =
                            String.left length tail
                    in
                    loop (idx + 1)
                        (String.dropLeft 1 tail)
                        (if
                            String.contains " " substr
                                || List.member substr res
                         then
                            res

                         else
                            String.left length tail :: res
                        )
        in
        loop 0 string []
