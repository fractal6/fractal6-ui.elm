module Extra.Url exposing (queryBuilder, queryParser)

import Dict exposing (Dict)
import Url exposing (Url)


queryParser : Url -> Dict String String
queryParser url =
    --queryParser : Url -> Dict String (List String)
    let
        toTuples : String -> List ( String, String )
        toTuples str =
            case String.split "=" str of
                key :: value ->
                    [ ( key, String.join "=" value ) ]

                [] ->
                    []

        toDict : List ( String, String ) -> Dict String (List String)
        toDict parameters =
            List.foldl
                (\( k, v ) dict -> Dict.update k (addParam v) dict)
                Dict.empty
                parameters

        addParam : String -> Maybe (List String) -> Maybe (List String)
        addParam value maybeValues =
            case maybeValues of
                Just values ->
                    Just (value :: values)

                Nothing ->
                    Just [ value ]

        toDict2 : List ( String, String ) -> Dict String String
        toDict2 parameters =
            Dict.fromList parameters

        addParam2 : String -> Maybe (List String) -> Maybe String
        addParam2 value maybeValues =
            Just value
    in
    url.query
        |> Maybe.andThen Url.percentDecode
        --|> Maybe.map (String.split "&" >> List.concatMap toTuples >> toDict)
        |> Maybe.map (String.split "&" >> List.concatMap toTuples >> toDict2)
        |> Maybe.withDefault Dict.empty


queryBuilder : List ( String, String ) -> String
queryBuilder parameters =
    let
        toUri : ( String, String ) -> List String
        toUri ( k, v ) =
            String.join "=" [ k, Url.percentEncode v ]
                |> List.singleton
    in
    List.concatMap toUri parameters
        |> String.join "&"
