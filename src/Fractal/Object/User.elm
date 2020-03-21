-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Object.User exposing (..)

import Fractal.InputObject
import Fractal.Interface
import Fractal.Object
import Fractal.Scalar
import Fractal.ScalarCodecs
import Fractal.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


{-| -}
id : SelectionSet Fractal.ScalarCodecs.Id Fractal.Object.User
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| -}
username : SelectionSet String Fractal.Object.User
username =
    Object.selectionForField "String" "username" [] Decode.string


{-| -}
password : SelectionSet String Fractal.Object.User
password =
    Object.selectionForField "String" "password" [] Decode.string


{-| -}
roles : SelectionSet decodesTo Fractal.Object.Role -> SelectionSet (Maybe (List decodesTo)) Fractal.Object.User
roles object_ =
    Object.selectionForCompositeField "roles" [] object_ (identity >> Decode.list >> Decode.nullable)
