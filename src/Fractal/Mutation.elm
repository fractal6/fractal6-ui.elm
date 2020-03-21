-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Mutation exposing (..)

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
import Json.Decode as Decode exposing (Decoder)


type alias AddUserRequiredArguments =
    { input : List Fractal.InputObject.AddUserInput }


{-|

  - input -

-}
addUser : AddUserRequiredArguments -> SelectionSet decodesTo Fractal.Object.AddUserPayload -> SelectionSet (Maybe decodesTo) RootMutation
addUser requiredArgs object_ =
    Object.selectionForCompositeField "addUser" [ Argument.required "input" requiredArgs.input (Fractal.InputObject.encodeAddUserInput |> Encode.list) ] object_ (identity >> Decode.nullable)


type alias AddTensionRequiredArguments =
    { input : List Fractal.InputObject.AddTensionInput }


{-|

  - input -

-}
addTension : AddTensionRequiredArguments -> SelectionSet decodesTo Fractal.Object.AddTensionPayload -> SelectionSet (Maybe decodesTo) RootMutation
addTension requiredArgs object_ =
    Object.selectionForCompositeField "addTension" [ Argument.required "input" requiredArgs.input (Fractal.InputObject.encodeAddTensionInput |> Encode.list) ] object_ (identity >> Decode.nullable)
