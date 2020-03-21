-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Enum.NodeType exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-|

  - Circle -
  - Role -

-}
type NodeType
    = Circle
    | Role


list : List NodeType
list =
    [ Circle, Role ]


decoder : Decoder NodeType
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "Circle" ->
                        Decode.succeed Circle

                    "Role" ->
                        Decode.succeed Role

                    _ ->
                        Decode.fail ("Invalid NodeType type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : NodeType -> String
toString enum =
    case enum of
        Circle ->
            "Circle"

        Role ->
            "Role"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe NodeType
fromString enumString =
    case enumString of
        "Circle" ->
            Just Circle

        "Role" ->
            Just Role

        _ ->
            Nothing
