module Extra.Url exposing (queryBuilder, queryParser, toAnchor)

import Dict exposing (Dict)
import Url exposing (Url)


queryParser : Url -> Dict String (List String)
queryParser url =
    --queryParser : Url -> Dict String  String
    let
        toTuples : String -> List ( String, String )
        toTuples str =
            case String.split "=" str of
                key :: value ->
                    [ ( key, String.join "=" value ) ]

                [] ->
                    []

        addParam : String -> Maybe (List String) -> Maybe (List String)
        addParam value maybeValues =
            case maybeValues of
                Just values ->
                    Just (value :: values)

                Nothing ->
                    Just [ value ]

        toDict : List ( String, String ) -> Dict String String
        toDict parameters =
            Dict.fromList parameters

        toDict2 : List ( String, String ) -> Dict String (List String)
        toDict2 parameters =
            List.foldl
                (\( k, v ) dict -> Dict.update k (addParam v) dict)
                Dict.empty
                parameters
    in
    url.query
        |> Maybe.andThen Url.percentDecode
        --|> Maybe.map (String.split "&" >> List.concatMap toTuples >> toDict)
        |> Maybe.map (String.split "&" >> List.concatMap toTuples >> toDict2)
        |> Maybe.withDefault Dict.empty



--|> (\x ->
--        if Dict.size x == 1 && Dict.toList x == [ ( "", "" ) ] then
--            Dict.empty
--        else
--            x
--   )


queryBuilder : List ( String, String ) -> String
queryBuilder parameters =
    let
        toUri : ( String, String ) -> List String
        toUri ( k, v ) =
            String.join "=" [ k, Url.percentEncode v ]
                |> List.singleton
    in
    parameters
        |> List.filter (\( k, v ) -> v /= "")
        |> List.concatMap toUri
        |> String.join "&"


toAnchor : String -> String
toAnchor x =
    x
        |> String.replace " " "-"
        |> String.toLower
        |> String.append "#"
