{- Vendored from https://github.com/miniBill/elm-diff (v1.1.2, BSD-3-Clause), a fork of jinjor/elm-diff. -}
module Utils.Diff exposing
    ( Change(..)
    , diff, diffLines, diffWith
    , diffLinesWith, DiffOptions, defaultOptions, ignoreLeadingWhitespace, equalIf, similarIf
    )

{-| Compares two list and returns how they have changed.
Each function internally uses Wu's [O(NP) algorithm](http://myerslab.mpi-cbg.de/wp-content/uploads/2014/06/np_diff.pdf).


# Types

@docs Change


# Diffing

@docs diff, diffLines, diffWith

@docs diffLinesWith, DiffOptions, defaultOptions, ignoreLeadingWhitespace, equalIf, similarIf

-}

import Array exposing (Array)
import List.Extra


{-| This describes how each line has changed and also contains its value.
-}
type Change similar a
    = Added a
    | Removed a
    | Similar a a similar
    | NoChange a


type StepResult
    = Continue (Array (List ( Int, Int )))
    | Found (List ( Int, Int ))



-- type BugReport
--     = CannotGetA Int
--     | CannotGetB Int
--     | UnexpectedPath ( Int, Int ) (List ( Int, Int ))


{-| Compares two text. Considers two lines similar if they are equivalent ignoring spaces.

Giving the following text

    a =
        """aaa
    bbb
    ddd"""

    b =
        """zzz
    aaa
    ccc
    ddd"""

results in

    [ Added "zzz"
    , NoChange "aaa"
    , Removed "bbb"
    , Added "ccc"
    , NoChange "ddd"
    ]

.

-}
diffLines : String -> String -> List (Change () String)
diffLines a b =
    diffWith
        (\l r ->
            if String.replace " " "" (String.trim l) == String.replace " " "" (String.trim r) then
                Just ()

            else
                Nothing
        )
        (String.lines a)
        (String.lines b)


{-| Compares general lists.

    diff [ 1, 3 ] [ 2, 3 ]
    --> [ Removed 1, Added 2, NoChange 3 ]

This will never produce `Similar`, so you can use `never` in that branch.

    diff a b
        |> List.map
            (\change ->
                case change of
                    Diff.Added _ ->
                        1

                    Diff.Removed _ ->
                        -1

                    Diff.NoChange _ ->
                        0

                    Diff.Similar _ _ ever ->
                        never ever
            )

-}
diff : List a -> List a -> List (Change Never a)
diff =
    diffWith (\_ _ -> Nothing)


{-| Compares general lists. Allows specifying when two elements are similar.

    diffWith (\l r -> if abs (l - r) <= 1 then Just () else Nothing) [ 1, 3 ] [ 2, 3 ]
    --> [ Similar 1 2 (), NoChange 3 ]

-}
diffWith : (a -> a -> Maybe similar) -> List a -> List a -> List (Change similar a)
diffWith areSimilar a b =
    testDiff (==) areSimilar a b
        |> Maybe.withDefault []


{-| Test the algorithm itself.
If it returns Err, it should be a bug.
-}
testDiff :
    (a -> a -> Bool)
    -> (a -> a -> Maybe similar)
    -> List a
    -> List a
    -- -> Result BugReport (List (Change similar a))
    -> Maybe (List (Change similar a))
testDiff areEqual areSimilar a b =
    let
        arrA : Array a
        arrA =
            Array.fromList a

        arrB : Array a
        arrB =
            Array.fromList b

        m : Int
        m =
            Array.length arrA

        n : Int
        n =
            Array.length arrB

        -- Elm's Array doesn't allow null element,
        -- so we'll use shifted index to access source.
        getA : Int -> Maybe a
        getA x =
            Array.get (x - 1) arrA

        getB : Int -> Maybe a
        getB y =
            Array.get (y - 1) arrB

        path : List ( Int, Int )
        path =
            onp areEqual areSimilar getA getB m n
    in
    makeChanges areSimilar getA getB path


makeChanges :
    (a -> a -> Maybe similar)
    -> (Int -> Maybe a)
    -> (Int -> Maybe a)
    -> List ( Int, Int )
    -- -> Result BugReport (List (Change similar a))
    -> Maybe (List (Change similar a))
makeChanges areSimilar getA getB path =
    case path of
        [] ->
            -- Ok []
            Just []

        latest :: tail ->
            makeChangesHelp areSimilar [] getA getB latest tail


makeChangesHelp :
    (a -> a -> Maybe similar)
    -> List (Change similar a)
    -> (Int -> Maybe a)
    -> (Int -> Maybe a)
    -> ( Int, Int )
    -> List ( Int, Int )
    -- -> Result BugReport (List (Change similar a))
    -> Maybe (List (Change similar a))
makeChangesHelp areSimilar changes getA getB ( x, y ) path =
    case path of
        [] ->
            -- Ok changes
            Just changes

        ( prevX, prevY ) :: tail ->
            let
                -- change : Result BugReport (Change similar a)
                change : Maybe (Change similar a)
                change =
                    if x - 1 == prevX && y - 1 == prevY then
                        case getA x of
                            Just a ->
                                case getB y of
                                    Nothing ->
                                        -- Err (CannotGetB y)
                                        Nothing

                                    Just b ->
                                        if a == b then
                                            -- Ok (NoChange a)
                                            Just (NoChange a)

                                        else
                                            case areSimilar a b of
                                                Just s ->
                                                    -- Ok (Similar a b s)
                                                    Just (Similar a b s)

                                                Nothing ->
                                                    -- This should not happen
                                                    -- Ok (NoChange a)
                                                    Just (NoChange a)

                            Nothing ->
                                -- Err (CannotGetA x)
                                Nothing

                    else if x == prevX then
                        case getB y of
                            Just b ->
                                -- Ok (Added b)
                                Just (Added b)

                            Nothing ->
                                -- Err (CannotGetB y)
                                Nothing

                    else if y == prevY then
                        case getA x of
                            Just a ->
                                -- Ok (Removed a)
                                Just (Removed a)

                            Nothing ->
                                -- Err (CannotGetA x)
                                Nothing

                    else
                        -- Err (UnexpectedPath ( x, y ) path)
                        Nothing
            in
            case change of
                -- Ok c ->
                Just c ->
                    makeChangesHelp areSimilar (c :: changes) getA getB ( prevX, prevY ) tail

                -- Err e ->
                Nothing ->
                    -- Err e
                    Nothing


{-| Wu's O(NP) algorithm (<http://myerslab.mpi-cbg.de/wp-content/uploads/2014/06/np_diff.pdf>)
-}
onp : (a -> a -> Bool) -> (a -> a -> Maybe similar) -> (Int -> Maybe a) -> (Int -> Maybe a) -> Int -> Int -> List ( Int, Int )
onp areEqual areSimilar getA getB m n =
    let
        v : Array (List ( Int, Int ))
        v =
            Array.repeat (m + n + 1) []

        delta : Int
        delta =
            n - m
    in
    onpLoopP (snake areEqual areSimilar getA getB) delta m 0 v


onpLoopP :
    (Int -> Int -> List ( Int, Int ) -> ( List ( Int, Int ), Bool ))
    -> Int
    -> Int
    -> Int
    -> Array (List ( Int, Int ))
    -> List ( Int, Int )
onpLoopP snake_ delta offset p v =
    let
        ks : List Int
        ks =
            if delta > 0 then
                List.reverse (List.range (delta + 1) (delta + p))
                    ++ List.range -p delta

            else
                List.reverse (List.range (delta + 1) p)
                    ++ List.range (-p + delta) delta
    in
    case onpLoopK snake_ offset ks v of
        Found path ->
            path

        Continue v_ ->
            onpLoopP snake_ delta offset (p + 1) v_


onpLoopK :
    (Int -> Int -> List ( Int, Int ) -> ( List ( Int, Int ), Bool ))
    -> Int
    -> List Int
    -> Array (List ( Int, Int ))
    -> StepResult
onpLoopK snake_ offset ks v =
    case ks of
        [] ->
            Continue v

        k :: ks_ ->
            case step snake_ offset k v of
                Found path ->
                    Found path

                Continue v_ ->
                    onpLoopK snake_ offset ks_ v_


step :
    (Int -> Int -> List ( Int, Int ) -> ( List ( Int, Int ), Bool ))
    -> Int
    -> Int
    -> Array (List ( Int, Int ))
    -> StepResult
step snake_ offset k v =
    let
        fromLeft : List ( Int, Int )
        fromLeft =
            Maybe.withDefault [] (Array.get (k - 1 + offset) v)

        fromTop : List ( Int, Int )
        fromTop =
            Maybe.withDefault [] (Array.get (k + 1 + offset) v)

        ( path, ( x, y ) ) =
            case ( fromLeft, fromTop ) of
                ( [], [] ) ->
                    ( [], ( 0, 0 ) )

                ( [], ( topX, topY ) :: _ ) ->
                    ( fromTop, ( topX + 1, topY ) )

                ( ( leftX, leftY ) :: _, [] ) ->
                    ( fromLeft, ( leftX, leftY + 1 ) )

                ( ( leftX, leftY ) :: _, ( topX, topY ) :: _ ) ->
                    -- this implies "remove" comes always earlier than "add"
                    if leftY + 1 >= topY then
                        ( fromLeft, ( leftX, leftY + 1 ) )

                    else
                        ( fromTop, ( topX + 1, topY ) )

        ( newPath, goal ) =
            snake_ (x + 1) (y + 1) (( x, y ) :: path)
    in
    if goal then
        Found newPath

    else
        Continue (Array.set (k + offset) newPath v)


snake :
    (a -> a -> Bool)
    -> (a -> a -> Maybe similar)
    -> (Int -> Maybe a)
    -> (Int -> Maybe a)
    -> Int
    -> Int
    -> List ( Int, Int )
    -> ( List ( Int, Int ), Bool )
snake areEqual areSimilar getA getB nextX nextY path =
    case ( getA nextX, getB nextY ) of
        ( Just a, Just b ) ->
            if areEqual a b || areSimilar a b /= Nothing then
                snake areEqual
                    areSimilar
                    getA
                    getB
                    (nextX + 1)
                    (nextY + 1)
                    (( nextX, nextY ) :: path)

            else
                ( path, False )

        -- reached bottom-right corner
        ( Nothing, Nothing ) ->
            ( path, True )

        _ ->
            ( path, False )


{-| Options for diffing multiline strings.
-}
type DiffOptions
    = DiffOptions
        { equalIf : String -> String -> Bool
        , similarIf : ( String, List Int ) -> ( String, List Int ) -> Maybe (List (Change Never Int))
        }


{-| Calculate the diff between two multiline strings.
-}
diffLinesWith : DiffOptions -> String -> String -> List (Change (List (Change Never Char)) String)
diffLinesWith options from to =
    let
        prepare : String -> List ( String, List Int )
        prepare s =
            s
                |> String.lines
                |> List.map (\line -> ( line, String.toList line |> List.map Char.toCode ))

        toChar : Change Never Int -> Change Never Char
        toChar change =
            case change of
                Added a ->
                    Added (Char.fromCode a)

                Removed r ->
                    Removed (Char.fromCode r)

                Similar _ _ ever ->
                    never ever

                NoChange n ->
                    NoChange (Char.fromCode n)
    in
    diffWith
        (\b a -> areStringsSimilar options b a)
        (prepare from)
        (prepare to)
        |> List.map
            (\change ->
                case change of
                    Added ( a, _ ) ->
                        Added a

                    Removed ( r, _ ) ->
                        Removed r

                    NoChange ( n, _ ) ->
                        NoChange n

                    Similar ( b, _ ) _ (Err ()) ->
                        NoChange b

                    Similar ( b, _ ) ( a, _ ) (Ok d) ->
                        Similar b a (List.map toChar d)
            )


areStringsSimilar :
    DiffOptions
    -> ( String, List Int )
    -> ( String, List Int )
    -> Maybe (Result () (List (Change Never Int)))
areStringsSimilar (DiffOptions options) (( bs, _ ) as b) (( as_, _ ) as a) =
    if options.equalIf bs as_ then
        Just (Err ())

    else
        options.similarIf b a
            |> Maybe.map Ok


{-| Default comparison options.
-}
defaultOptions : DiffOptions
defaultOptions =
    DiffOptions
        { equalIf = (==)
        , similarIf = smallDifference
        }


smallDifference : ( String, List Int ) -> ( String, List Int ) -> Maybe (List (Change Never Int))
smallDifference ( ls, ll ) ( rs, rl ) =
    let
        delta : List (Change Never Int)
        delta =
            testDiff (\l r -> l - r == 0) (\_ _ -> Nothing) ll rl
                |> Maybe.withDefault []

        minLength : Int
        minLength =
            min (String.length (String.trim ls)) (String.length (String.trim rs))
    in
    if List.Extra.count isChange delta < minLength // 2 then
        Just delta

    else
        Nothing


isChange : Change similar a -> Bool
isChange c =
    case c of
        NoChange _ ->
            False

        _ ->
            True


{-| Configure when to consider two lines equal.

As an example you can use `\l r -> String.trim l == String.trim r` to ignore whitespace around lines.

-}
equalIf : (String -> String -> Bool) -> DiffOptions -> DiffOptions
equalIf eq (DiffOptions options) =
    DiffOptions { options | equalIf = eq }


{-| Configure when to consider two lines similar.
-}
similarIf : (String -> String -> Bool) -> DiffOptions -> DiffOptions
similarIf similar (DiffOptions options) =
    DiffOptions
        { options
            | similarIf =
                \( ls, ll ) ( rs, rl ) ->
                    if similar ls rs then
                        Just (diff ll rl)

                    else
                        Nothing
        }


{-| Ignore leading whitespace when comparing multiline strings.

    diffLinesWith (defaultOptions |> ignoreLeadingWhitespace) "a" "  a"
    --> [ NoChange "a" ]

Equivalent to `equalIf (\a b -> String.trimLeft a == String.trimLeft b)`.

-}
ignoreLeadingWhitespace : DiffOptions -> DiffOptions
ignoreLeadingWhitespace options =
    equalIf (\a b -> String.trimLeft a == String.trimLeft b) options
