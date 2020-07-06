module ModelCommon.View exposing (..)

import Components.Fa as Fa
import Components.Text as T
import Date exposing (formatTime)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (toHref)
import Global
import Html exposing (Html, a, br, div, i, span, text)
import Html.Attributes exposing (attribute, class, classList, href)
import Html.Events exposing (onClick)
import Maybe exposing (withDefault)
import ModelCommon.Uri exposing (FractalBaseRoute(..), NodeFocus, uriFromNameid, uriFromUsername)
import ModelSchema exposing (EmitterOrReceiver, Label, Post, Tension, TensionExtended, UserCtx, Username)



{-
   Tension
-}


statusColor : TensionStatus.TensionStatus -> String
statusColor s =
    case s of
        TensionStatus.Open ->
            "success"

        TensionStatus.Closed ->
            "danger"


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

        labels_m =
            tension.labels |> Maybe.map (\ls -> ternary (List.length ls == 0) Nothing (Just ls)) |> withDefault Nothing
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
                , span [ class "is-pulled-right tooltip has-tooltip-top", attribute "data-tooltip" ("comments: " ++ String.fromInt n_comments) ] <|
                    if n_comments > 0 then
                        [ Fa.icon0 "fas fa-comment-dots" (String.fromInt n_comments) ]

                    else
                        []
                ]
            , case labels_m of
                Just labels ->
                    viewLabels labels

                Nothing ->
                    span [] []
            , br [ class "is-block" ] []
            , span [ class "columns" ]
                [ span [ class "column is-two-thirds" ] [ viewTensionArrow "has-text-weight-light" tension.emitter tension.receiver ]
                , span [ class "has-text-weight-light column" ]
                    [ span [ class "columns is-mobile mediaFragments" ]
                        [ span [ class "column is-1" ] <|
                            case tension.action of
                                Just TensionAction.NewCircle ->
                                    [ Fa.fa "far fa-circle" ]

                                Just TensionAction.NewRole ->
                                    [ Fa.fa "far fa-user" ]

                                Just TensionAction.UpdateCircleAbout ->
                                    [ span [ class "fa-stack", attribute "style" "font-size: 0.6em;" ]
                                        [ i [ class "fas fa-pen fa-stack-1x" ] []
                                        , i [ class "far fa-circle fa-stack-2x" ] []
                                        ]
                                    ]

                                Just TensionAction.UpdateCircleMandate ->
                                    [ span [ class "fa-stack", attribute "style" "font-size: 0.5em;" ]
                                        [ i [ class "fas fa-pen fa-stack-1x" ] []
                                        , i [ class "fas fa-scroll fa-stack-2x" ] []
                                        ]
                                    ]

                                Just TensionAction.UpdateRoleAbout ->
                                    [ span [ class "fa-stack", attribute "style" "font-size: 0.6em;" ]
                                        [ i [ class "fas fa-pen fa-stack-1x" ] []
                                        , i [ class "far fa-user fa-stack-2x" ] []
                                        ]
                                    ]

                                Just TensionAction.UpdateRoleMandate ->
                                    [ span [ class "fa-stack", attribute "style" "font-size: 0.6em;" ]
                                        [ i [ class "fas fa-pen fa-stack-1x" ] []
                                        , i [ class "far fa-user fa-stack-2x" ] []
                                        ]
                                    ]

                                Nothing ->
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


viewLabels : List Label -> Html msg
viewLabels labels =
    span [ class "labelsList" ] <|
        (labels
            |> List.map
                (\label ->
                    span [ class "tag" ] [ text label.name ]
                )
        )


viewUsernameLink : String -> Html msg
viewUsernameLink username =
    a [ href (uriFromUsername UsersBaseUri username) ] [ "@" ++ username |> text ]


viewOpenedDate : String -> Html msg
viewOpenedDate date =
    span []
        [ span [ class "is-itali" ] [ text T.openedThe ]
        , text " "
        , text (formatTime date)
        ]


viewCommentedDate : String -> Html msg
viewCommentedDate date =
    span []
        [ span [ class "is-itali" ] [ text T.commentedThe ]
        , text " "
        , text (formatTime date)
        ]


viewTensionDateAndUser : String -> Username -> Html msg
viewTensionDateAndUser createdAt createdBy =
    span []
        [ viewOpenedDate createdAt
        , text (" " ++ T.by ++ " ")
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
   Action
-}


type alias NewNodeText =
    { title : String
    , added : String
    , name_help : String
    , about_help : String
    , message_help : String
    , ph_purpose : String
    , ph_responsabilities : String
    , ph_domains : String
    , ph_policies : String
    , submit : String
    , close_submit : String
    , firstLink_help : String
    , tension_added : String
    }


actionNameStr : TensionAction.TensionAction -> String
actionNameStr action =
    case action of
        TensionAction.NewCircle ->
            T.newCircle

        TensionAction.NewRole ->
            T.newRole

        TensionAction.UpdateCircleAbout ->
            T.updateAbout

        TensionAction.UpdateRoleAbout ->
            T.updateAbout

        TensionAction.UpdateCircleMandate ->
            T.updateMandate

        TensionAction.UpdateRoleMandate ->
            T.updateMandate


action2SourceStr : Maybe TensionAction.TensionAction -> String
action2SourceStr action_m =
    case action_m of
        Nothing ->
            "create this Tension"

        Just action ->
            case action of
                TensionAction.NewCircle ->
                    "create this Circle"

                TensionAction.NewRole ->
                    "create this Role"

                TensionAction.UpdateCircleAbout ->
                    "edit this circle"

                TensionAction.UpdateRoleAbout ->
                    "edit this role"

                TensionAction.UpdateCircleMandate ->
                    "edit this mandate"

                TensionAction.UpdateRoleMandate ->
                    "edit this mandate"


getTensionText : NewNodeText
getTensionText =
    NewNodeText T.newTension T.tensionAdded T.tensionTitleHelp "" T.tensionMessageHelp "" "" "" "" T.tensionSubmit "" "" ""


getRoleEdit : NewNodeText
getRoleEdit =
    NewNodeText T.editRole T.roleEdited T.roleNameHelp T.roleAboutHelp T.roleMessageHelp T.phRolePurpose T.phRoleResponsabilities T.phRoleDomains T.phRolePolicies T.tensionSubmit T.editAndClose T.firstLinkRoleMessageHelp T.tensionRoleEdit


getCircleEdit : NewNodeText
getCircleEdit =
    NewNodeText T.editCircle T.circleEdited T.circleNameHelp T.circleAboutHelp T.circleMessageHelp T.phCirclePurpose T.phCircleResponsabilities T.phCircleDomains T.phCirclePolicies T.tensionSubmit T.editAndClose T.firstLinkCircleMessageHelp T.tensionCircleEdit


getNodeTextFromNodeType : NodeType.NodeType -> NewNodeText
getNodeTextFromNodeType type_ =
    case type_ of
        NodeType.Circle ->
            NewNodeText T.newCircle T.circleAdded T.circleNameHelp T.circleAboutHelp T.circleMessageHelp T.phCirclePurpose T.phCircleResponsabilities T.phCircleDomains T.phCirclePolicies T.tensionCircleSubmit T.tensionCircleCloseSubmit T.firstLinkCircleMessageHelp T.tensionCircleAdded

        NodeType.Role ->
            NewNodeText T.newRole T.roleAdded T.roleNameHelp T.roleAboutHelp T.roleMessageHelp T.phRolePurpose T.phRoleResponsabilities T.phRoleDomains T.phRolePolicies T.tensionRoleSubmit T.tensionRoleCloseSubmit T.firstLinkRoleMessageHelp T.tensionRoleAdded


getNodeTextFromAction : TensionAction.TensionAction -> NewNodeText
getNodeTextFromAction action =
    case action of
        TensionAction.NewCircle ->
            getNodeTextFromNodeType NodeType.Circle

        TensionAction.NewRole ->
            getNodeTextFromNodeType NodeType.Role

        TensionAction.UpdateCircleAbout ->
            getCircleEdit

        TensionAction.UpdateCircleMandate ->
            getCircleEdit

        TensionAction.UpdateRoleAbout ->
            getRoleEdit

        TensionAction.UpdateRoleMandate ->
            getRoleEdit
