module ModelCommon.View exposing (..)

import Components.Fa as Fa
import Components.Text as T
import Date exposing (formatTime)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (toHref)
import Global
import Html exposing (Html, a, br, div, i, span, text)
import Html.Attributes exposing (attribute, class, classList, href, id)
import Html.Events exposing (onClick)
import Maybe exposing (withDefault)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), NodeFocus, uriFromNameid, uriFromUsername)
import ModelSchema exposing (EmitterOrReceiver, Label, Post, Tension, UserCtx, Username)



{-
   Tension
   @DEBUG: Factor this file to appropriate places in Component.{Tension, Node, User...}
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
                [ class "tooltip has-tooltip-top "
                , attribute "data-tooltip" (TensionType.toString tension.type_)
                ]
                [ div [ class <| "Circle " ++ tensionTypeColor "text" tension.type_ ] [ text "" ] ]
            ]
        , div [ class "media-content" ]
            [ div
                [ class "content"
                , onClick (Route.Tension_Dynamic_Dynamic { param1 = focus.rootnameid, param2 = tension.id } |> toHref |> navigate)
                ]
                [ a
                    [ classList [ ( "has-text-weight-semibold", True ), ( "is-size-6", baseUri == TensionsBaseUri ) ]
                    , href (Route.Tension_Dynamic_Dynamic { param1 = focus.rootnameid, param2 = tension.id } |> toHref)
                    ]
                    [ text tension.title ]
                , if n_comments > 1 then
                    span [ class "is-pulled-right tooltip has-tooltip-top", attribute "data-tooltip" (String.fromInt (n_comments - 1) ++ " comments") ]
                        [ Fa.icon "fas fa-comment" (String.fromInt (n_comments - 1)) ]

                  else
                    span [] []
                ]
            , case labels_m of
                Just labels ->
                    viewLabels labels

                Nothing ->
                    span [] []
            , br [ class "is-block" ] []
            , span [ class "columns is-variable is-mobile" ]
                [ span [ class "column is-7 is-variable" ] [ viewTensionArrow "has-text-weight-light" tension.emitter tension.receiver ]
                , span [ class "column" ]
                    [ case tension.action of
                        Just action ->
                            viewActionIconLink action focus.rootnameid tension.id ""

                        Nothing ->
                            span [] []
                    , span [ class "is-pulled-right" ] [ viewTensionDateAndUser tension.createdAt tension.createdBy ]
                    ]
                ]
            ]
        ]


edgeArrow : String -> Html msg -> Html msg -> List (Html msg)
edgeArrow cls source target =
    [ span [ class <| cls ++ " is-small is-light is-inverted is-static" ] [ source ]
    , span [ class "right-arrow" ] []
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


viewUser : String -> Html msg
viewUser username =
    span [] [ a [ class "image circleBaseInline circle0", href (uriFromUsername UsersBaseUri username) ] [ getAvatar username ] ]


viewOpenedDate : String -> Html msg
viewOpenedDate date =
    span []
        [ span [] [ text T.openedThe ]
        , text " "
        , text (formatTime date)
        ]


viewUpdated : String -> Html msg
viewUpdated date =
    span [ class "is-grey-light" ]
        [ text " · "
        , span [] [ text T.editedThe ]
        , text " "
        , text (formatTime date)
        ]


viewCommentedDate : String -> Html msg
viewCommentedDate date =
    span [ class "is-grey-light" ]
        [ span [ class "is-itali" ] [ text T.commentedThe ]
        , text " "
        , text (formatTime date)
        ]


viewTensionDateAndUser : String -> Username -> Html msg
viewTensionDateAndUser createdAt createdBy =
    span [ class "is-grey-light" ]
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


byAt : Username -> String -> Html msg
byAt createdBy createdAt =
    span []
        [ text " by "
        , viewUsernameLink createdBy.username
        , text " the "
        , text (formatTime createdAt)
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


actionNameStr : TensionAction.TensionAction -> String
actionNameStr action =
    case action of
        TensionAction.NewCircle ->
            T.circleH

        TensionAction.EditCircle ->
            T.circleH

        TensionAction.NewRole ->
            T.roleH

        TensionAction.EditRole ->
            T.roleH

        TensionAction.NewMd ->
            T.documentH

        TensionAction.EditMd ->
            T.documentH


blobTypeStr : BlobType.BlobType -> String
blobTypeStr btype =
    case btype of
        BlobType.OnNode ->
            "Document created"

        BlobType.OnAbout ->
            "Description edited"

        BlobType.OnFirstLink ->
            "First link edited"

        BlobType.OnMandate ->
            "Mandate edited"

        BlobType.OnDoc ->
            "File edited"


viewActionIconLink : TensionAction.TensionAction -> String -> String -> String -> Html msg
viewActionIconLink action org tid words =
    a
        [ class "actionLink"
        , href (Route.Tension_Dynamic_Dynamic_Action { param1 = org, param2 = tid } |> toHref)
        ]
        [ span
            [ class "stackPen icon-padding tooltip"
            , attribute "data-tooltip" ("1 " ++ actionNameStr action ++ " attached")
            ]
            (case action of
                TensionAction.NewCircle ->
                    [ Fa.fa "far fa-circle" ]

                TensionAction.EditCircle ->
                    [ span [ class "fa-stack", attribute "style" "font-size: 0.5em;" ]
                        [ i [ class "fas fa-pen fa-stack-1x" ] []
                        , i [ class "far fa-circle fa-stack-2x" ] []
                        ]
                    ]

                TensionAction.NewRole ->
                    [ Fa.fa "fas fa-circle" ]

                TensionAction.EditRole ->
                    [ span [ class "fa-stack", attribute "style" "font-size: 0.5em;" ]
                        [ i [ class "fas fa-pen fa-stack-1x" ] []
                        , i [ class "fas fa-circle fa-stack-2x" ] []
                        ]
                    ]

                TensionAction.NewMd ->
                    [ Fa.fa "fas fa-markdown" ]

                TensionAction.EditMd ->
                    [ span [ class "fa-stack", attribute "style" "font-size: 0.5em;" ]
                        [ i [ class "fas fa-pen fa-stack-1x" ] []
                        , i [ class "fas fa-markdown fa-stack-2x" ] []
                        ]
                    ]
            )
        , text words
        ]


action2SourceStr : Maybe TensionAction.TensionAction -> String
action2SourceStr action_m =
    case action_m of
        Nothing ->
            "create this Tension"

        Just action ->
            case action of
                TensionAction.NewCircle ->
                    "create this Circle"

                TensionAction.EditCircle ->
                    "edit this circle"

                TensionAction.NewRole ->
                    "create this Role"

                TensionAction.EditRole ->
                    "edit this role"

                TensionAction.NewMd ->
                    "create this Document"

                TensionAction.EditMd ->
                    "edit this Document"


type alias FormText =
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


emptyFormText : FormText
emptyFormText =
    FormText "" "" "" "" "" "" "" "" "" "" "" "" ""


getTensionText : FormText
getTensionText =
    FormText T.newTension T.tensionAdded T.tensionTitleHelp "" T.tensionMessageHelp "" "" "" "" T.tensionSubmit "" "" ""


getNodeTextFromNodeType : NodeType.NodeType -> FormText
getNodeTextFromNodeType type_ =
    case type_ of
        NodeType.Circle ->
            FormText T.newCircle T.circleAdded T.circleNameHelp T.circleAboutHelp T.circleMessageHelp T.phCirclePurpose T.phCircleResponsabilities T.phCircleDomains T.phCirclePolicies T.tensionSubmit T.tensionCircleCloseSubmit T.firstLinkCircleMessageHelp T.tensionCircleAdded

        NodeType.Role ->
            FormText T.newRole T.roleAdded T.roleNameHelp T.roleAboutHelp T.roleMessageHelp T.phRolePurpose T.phRoleResponsabilities T.phRoleDomains T.phRolePolicies T.tensionSubmit T.tensionRoleCloseSubmit T.firstLinkRoleMessageHelp T.tensionRoleAdded
