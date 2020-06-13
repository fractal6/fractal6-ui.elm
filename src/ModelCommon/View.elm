module ModelCommon.View exposing (..)

import Components.Fa as Fa
import Date exposing (formatTime)
import Dict exposing (Dict)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionType as TensionType
import Html exposing (Html, a, br, div, span, text)
import Html.Attributes exposing (attribute, class, href)
import Maybe exposing (withDefault)
import ModelCommon.Uri exposing (FractalBaseRoute(..), uriFromNameid, uriFromUsername)
import ModelSchema exposing (EmitterOrReceiver, Post, Tension)



{-
   User
-}


roleColor : RoleType.RoleType -> String
roleColor rt =
    case rt of
        RoleType.Guest ->
            "primary"

        RoleType.Member ->
            "primary"

        RoleType.Peer ->
            "primary"

        RoleType.Coordinator ->
            "orange"



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


tensionTypeSpan : String -> String -> TensionType.TensionType -> Html msg
tensionTypeSpan cls elt type_ =
    span [ class <| cls ++ " " ++ tensionTypeColor elt type_ ] [ text (TensionType.toString type_) ]


edgeArrow : String -> Html msg -> Html msg -> List (Html msg)
edgeArrow cls source target =
    [ span [ class <| cls ++ " is-small is-light is-inverted is-static" ] [ source ]
    , span [ class <| "right-arrow" ] []
    , span [ class <| cls ++ " is-small is-light is-inverted is-static" ] [ target ]
    ]


mediaTension : Tension -> Html msg
mediaTension tension =
    div [ class "media mediaTension" ]
        [ div [ class "media-left" ]
            [ div
                [ class "tooltip has-tooltip-top"
                , attribute "data-tooltip" ("type: " ++ TensionType.toString tension.type_)
                ]
                [ div [ class <| "Circle " ++ tensionTypeColor "text" tension.type_ ] [ text "" ] ]
            ]
        , div [ class "media-content" ]
            [ div [ class "content" ]
                [ div [ class "has-text-weight-semibold" ]
                    [ text tension.title ]
                ]
            , div [ class "labelsList" ] <|
                (tension.labels
                    |> withDefault []
                    |> List.map
                        (\label ->
                            span [ class "tag" ] [ text label.name ]
                        )
                )
            , br [ class "is-block" ] []
            , span [ class "columns" ]
                [ span [ class "column is-two-thirds" ] <| edgeArrow "has-text-weight-light" (viewNodeRef OverviewBaseUri tension.emitter) (viewNodeRef OverviewBaseUri tension.receiver)
                , span [ class "has-text-weight-light column" ]
                    [ span [ class "columns" ]
                        [ span [ class "column is-1", attribute "style" "padding-left: 0 !important;" ] <|
                            case tension.action of
                                Just TensionAction.NewCircle ->
                                    [ Fa.icon0 "far fa-circle" "" ]

                                Just TensionAction.NewRole ->
                                    [ Fa.icon0 "far fa-user" "" ]

                                Nothing ->
                                    []
                        , span [ class "column", attribute "style" "padding-right: 0 !important;" ]
                            [ span [ class "is-pulled-right" ]
                                [ "opened the " ++ formatTime tension.createdAt ++ " by " |> text
                                , a [ href (uriFromUsername UsersBaseUri tension.createdBy.username) ] [ "@" ++ tension.createdBy.username |> text ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , div
            [ class "media-right" ]
            [ let
                n_comments =
                    tension.n_comments |> withDefault 0
              in
              if n_comments > 0 then
                span
                    [ class "tooltip has-tooltip-top"
                    , attribute "data-tooltip" ("comments: " ++ String.fromInt n_comments)
                    ]
                    [ Fa.icon0 "fas fa-comment-dots" (String.fromInt n_comments)
                    ]

              else
                span [] []
            ]
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
