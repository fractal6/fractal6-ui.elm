module ModelCommon.View exposing (edgeArrow, tensionTypeColor, tensionTypeSpan, viewNodeRef)

import Dict exposing (Dict)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.TensionType as TensionType
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class, href)
import ModelCommon.Uri exposing (FractalBaseRoute, uriFromNameid)
import ModelSchema exposing (EmitterOrReceiver, Post)



{-
   Tension
-}


tensionTypeColor : String -> TensionType.TensionType -> String
tensionTypeColor elt tt =
    case tt of
        TensionType.Governance ->
            "has-" ++ elt ++ "-info"

        TensionType.Operational ->
            "has-" ++ elt ++ "-success"

        TensionType.Personal ->
            "has-" ++ elt ++ "-warning"

        TensionType.Help ->
            "has-" ++ elt ++ "-link"


tensionTypeSpan : String -> String -> Post -> Html msg
tensionTypeSpan cls elt post =
    let
        tensionType_m =
            Dict.get "type_" post |> Maybe.withDefault "Governance" |> TensionType.fromString
    in
    case tensionType_m of
        Just tt ->
            span [ class <| cls ++ " " ++ tensionTypeColor elt tt ] [ text (TensionType.toString tt) ]

        Nothing ->
            span [ class "" ] [ text "Unknown" ]


edgeArrow : String -> Html msg -> Html msg -> List (Html msg)
edgeArrow cls source target =
    [ span [ class <| cls ++ " is-small is-light is-inverted is-static" ] [ source ]
    , span [ class <| "right-arrow" ] []
    , span [ class <| cls ++ " is-small is-light is-inverted is-static" ] [ target ]
    ]



{-
   Node
-}


viewNodeRef : FractalBaseRoute -> EmitterOrReceiver -> Html msg
viewNodeRef baseUri n =
    case n.type_ of
        NodeType.Circle ->
            a [ href (uriFromNameid baseUri n.nameid) ] [ n.name |> text ]

        NodeType.Role ->
            a [ href (uriFromNameid baseUri n.nameid) ] [ n.name |> text ]
