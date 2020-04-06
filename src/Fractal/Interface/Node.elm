-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Interface.Node exposing (..)

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
import Graphql.SelectionSet exposing (FragmentSelectionSet(..), SelectionSet(..))
import Json.Decode as Decode


type alias Fragments decodesTo =
    { onCircle : SelectionSet decodesTo Fractal.Object.Circle
    , onRole : SelectionSet decodesTo Fractal.Object.Role
    }


{-| Build an exhaustive selection of type-specific fragments.
-}
fragments :
    Fragments decodesTo
    -> SelectionSet decodesTo Fractal.Interface.Node
fragments selections =
    Object.exhaustiveFragmentSelection
        [ Object.buildFragment "Circle" selections.onCircle
        , Object.buildFragment "Role" selections.onRole
        ]


{-| Can be used to create a non-exhaustive set of fragments by using the record
update syntax to add `SelectionSet`s for the types you want to handle.
-}
maybeFragments : Fragments (Maybe decodesTo)
maybeFragments =
    { onCircle = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRole = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    }


{-| -}
id : SelectionSet Fractal.ScalarCodecs.Id Fractal.Interface.Node
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| -}
title : SelectionSet String Fractal.Interface.Node
title =
    Object.selectionForField "String" "title" [] Decode.string


{-| -}
mandate : SelectionSet (Maybe String) Fractal.Interface.Node
mandate =
    Object.selectionForField "(Maybe String)" "mandate" [] (Decode.string |> Decode.nullable)


{-| -}
createdAt : SelectionSet (Maybe Fractal.ScalarCodecs.DateTime) Fractal.Interface.Node
createdAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "createdAt" [] (Fractal.ScalarCodecs.codecs |> Fractal.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


{-| -}
createdBy : SelectionSet decodesTo Fractal.Object.User -> SelectionSet (Maybe decodesTo) Fractal.Interface.Node
createdBy object_ =
    Object.selectionForCompositeField "createdBy" [] object_ (identity >> Decode.nullable)


{-| -}
parent : SelectionSet decodesTo Fractal.Interface.Node -> SelectionSet (Maybe decodesTo) Fractal.Interface.Node
parent object_ =
    Object.selectionForCompositeField "parent" [] object_ (identity >> Decode.nullable)


{-| -}
children : SelectionSet decodesTo Fractal.Interface.Node -> SelectionSet (Maybe (List decodesTo)) Fractal.Interface.Node
children object_ =
    Object.selectionForCompositeField "children" [] object_ (identity >> Decode.list >> Decode.nullable)


{-| -}
tensions_out : SelectionSet decodesTo Fractal.Object.Tension -> SelectionSet (Maybe (List decodesTo)) Fractal.Interface.Node
tensions_out object_ =
    Object.selectionForCompositeField "tensions_out" [] object_ (identity >> Decode.list >> Decode.nullable)


{-| -}
tensions_in : SelectionSet decodesTo Fractal.Object.Tension -> SelectionSet (Maybe (List decodesTo)) Fractal.Interface.Node
tensions_in object_ =
    Object.selectionForCompositeField "tensions_in" [] object_ (identity >> Decode.list >> Decode.nullable)