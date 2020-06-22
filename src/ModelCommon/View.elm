module ModelCommon.View exposing (..)

import Components.Fa as Fa
import Components.Text as Text
import Date exposing (formatTime)
import Dict exposing (Dict)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (toHref)
import Global
import Html exposing (Html, a, br, div, span, text)
import Html.Attributes exposing (attribute, class, classList, href)
import Html.Events exposing (onClick)
import Maybe exposing (withDefault)
import ModelCommon.Uri exposing (FractalBaseRoute(..), NodeFocus, uriFromNameid, uriFromUsername)
import ModelSchema exposing (EmitterOrReceiver, Post, Tension, TensionExtended, UserCtx, Username)



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


statusColor : TensionStatus.TensionStatus -> String
statusColor s =
    case s of
        TensionStatus.Open ->
            "success"

        TensionStatus.Closed ->
            "danger"



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


mediaTension : FractalBaseRoute -> NodeFocus -> Tension -> (String -> msg) -> Html msg
mediaTension baseUri focus tension navigate =
    let
        n_comments =
            tension.n_comments |> withDefault 0
    in
    div [ class "media mediaTension" ]
        [ div [ class "media-left" ]
            [ div
                [ class "tooltip has-tooltip-top"
                , attribute "data-tooltip" ("type: " ++ TensionType.toString tension.type_)
                ]
                [ div [ class <| "Circle " ++ tensionTypeColor "text" tension.type_ ] [ text "" ] ]
            ]
        , div [ class "media-content" ]
            [ div
                [ class "content"
                , onClick (Route.Tension_Dynamic_Dynamic { param1 = focus.rootnameid, param2 = tension.id } |> toHref |> navigate)
                ]
                [ a
                    [ classList [ ( "has-text-light", True ), ( "has-text-weight-semibold", True ), ( "is-size-6", baseUri == TensionsBaseUri ) ]
                    , href (Route.Tension_Dynamic_Dynamic { param1 = focus.rootnameid, param2 = tension.id } |> toHref)
                    ]
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
                [ span [ class "column is-two-thirds" ] [ viewTensionArrow "has-text-weight-light" tension.emitter tension.receiver ]
                , span [ class "has-text-weight-light column" ]
                    [ span [ class "columns" ]
                        [ span [ class "column is-1", attribute "style" "padding-left: 0 !important;" ] <|
                            case tension.action of
                                Just TensionAction.NewCircle ->
                                    [ Fa.fa "far fa-circle" ]

                                Just TensionAction.NewRole ->
                                    [ Fa.fa "far fa-user" ]

                                Nothing ->
                                    []
                        , span
                            [ class "column is-2 tooltip has-tooltip-top"
                            , attribute "data-tooltip" ("comments: " ++ String.fromInt n_comments)
                            , attribute "style" "padding-left: 0 !important; padding-right:0 !important;"
                            ]
                          <|
                            if n_comments > 0 then
                                [ Fa.icon0 "fas fa-comment-dots" (String.fromInt n_comments) ]

                            else
                                []
                        , span [ class "column" ]
                            [ span [ class "is-pulled-right" ]
                                [ viewTensionDateAndUser tension.createdAt tension.createdBy ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


edgeArrow : String -> Html msg -> Html msg -> List (Html msg)
edgeArrow cls source target =
    [ span [ class <| cls ++ " is-small is-light is-inverted is-static" ] [ source ]
    , span [ class <| "right-arrow" ] []
    , span [ class <| cls ++ " is-small is-light is-inverted is-static" ] [ target ]
    ]


viewTensionArrow : String -> EmitterOrReceiver -> EmitterOrReceiver -> Html msg
viewTensionArrow cls emitter receiver =
    span [ class cls ]
        [ span [ class "is-small is-light is-inverted is-static" ] [ viewNodeRef OverviewBaseUri emitter ]
        , span [ class "right-arrow" ] []
        , span [ class "is-small is-light is-inverted is-static" ] [ viewNodeRef OverviewBaseUri receiver ]
        ]


viewTensionArrowB : String -> EmitterOrReceiver -> EmitterOrReceiver -> Html msg
viewTensionArrowB cls emitter receiver =
    span [ class cls ]
        [ span [ class "is-small  is-inverted is-hovered button", attribute "style" "margin-top: -3px !important;" ] [ viewNodeRef OverviewBaseUri emitter ]
        , span [ class "right-arrow" ] []
        , span [ class "is-small  is-inverted is-hovered button", attribute "style" "margin-top: -3px !important;" ] [ viewNodeRef OverviewBaseUri receiver ]
        ]


viewUsernameLink : String -> Html msg
viewUsernameLink username =
    a [ href (uriFromUsername UsersBaseUri username) ] [ "@" ++ username |> text ]


viewOpenedDate : String -> Html msg
viewOpenedDate date =
    span []
        [ [ Text.openedThe, formatTime date ] |> String.join " " |> text ]


viewCommentedDate : String -> Html msg
viewCommentedDate date =
    span []
        [ [ Text.commentedThe, formatTime date ] |> String.join " " |> text ]


viewTensionDateAndUser : String -> Username -> Html msg
viewTensionDateAndUser createdAt createdBy =
    span []
        [ viewOpenedDate createdAt
        , text (" " ++ Text.by ++ " ")
        , viewUsernameLink createdBy.username
        ]


viewTensionDateAndUserC : String -> Username -> Html msg
viewTensionDateAndUserC createdAt createdBy =
    span []
        [ viewUsernameLink createdBy.username
        , text " "
        , viewCommentedDate createdAt
        ]



{-
   Node
-}


viewNodeRef : FractalBaseRoute -> EmitterOrReceiver -> Html msg
viewNodeRef baseUri n =
    let
        ref =
            if n.role_type == Just RoleType.Member then
                "#"

            else
                uriFromNameid baseUri n.nameid
    in
    a [ href ref ] [ n.name |> text ]



{-
   User
-}


getAvatar : String -> Html msg
getAvatar username =
    let
        initial =
            username
                |> String.slice 0 1
                |> String.toUpper
    in
    username
        |> String.slice 1 3
        |> String.toLower
        |> String.append initial
        |> text
