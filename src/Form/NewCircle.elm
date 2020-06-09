module Form.NewCircle exposing (view)

import Components.Loading as Loading exposing (viewErrors)
import Components.Text as T
import Dict
import Extra.Events exposing (onClickPD2, onEnter, onKeydown, onTab)
import Form exposing (isPostSendable)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, li, nav, option, p, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, placeholder, rows, selected, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.View exposing (edgeArrow, roleColor, tensionTypeSpan)
import ModelSchema exposing (GqlData, Node, RequestResult(..), UserRole)


type alias NewNodeText =
    { title : String
    , added : String
    , name_help : String
    , message_help : String
    , ph_purpose : String
    , ph_responsabilities : String
    , ph_domains : String
    , ph_policies : String
    , submit : String
    , close_submit : String
    , firstLink_help : String
    }


{-| --view : TensionForm -> GqlData (Maybe Node) -> (String -> String -> msg) -> ((msg -> TensionFOrm) -> Time.Posix -> msg) -> (TensionForm -> Time.Posix -> msg) -> Html msg
What should be the signature ?!
-}
view form result changePostMsg closeModalMsg submitMsg submitNextMsg =
    let
        txt =
            case form.type_ of
                NodeType.Circle ->
                    NewNodeText T.newCircle T.tensionCircleAdded T.circleNameHelp T.circleMessageHelp T.phCirclePurpose T.phCircleResponsabilities T.phCircleDomains T.phCirclePolicies T.tensionCircleSubmit T.tensionCircleCloseSubmit T.firstLinkCircleMessageHelp

                NodeType.Role ->
                    NewNodeText T.newRole T.tensionRoleAdded T.roleNameHelp T.roleMessageHelp T.phRolePurpose T.phRoleResponsabilities T.phRoleDomains T.phRolePolicies T.tensionRoleSubmit T.tensionRoleCloseSubmit T.firstLinkRoleMessageHelp

        isSendable =
            isPostSendable [ "name", "purpose" ] form.post

        isLoading =
            result == LoadingSlowly
    in
    case result of
        Success _ ->
            div [ class "box has-background-success", onClick (closeModalMsg "") ] [ text txt.added ]

        other ->
            let
                title =
                    Dict.get "title" form.post |> withDefault ""

                nameid =
                    Dict.get "nameid" form.post |> withDefault ""

                firstLinks =
                    Dict.get "first_links" form.post |> withDefault "" |> String.split "@" |> List.filter (\x -> x /= "")
            in
            div [ class "modal-card finalModal" ]
                [ div [ class "modal-card-head" ]
                    [ div [ class "level modal-card-title" ]
                        [ div [ class "level-left" ] <|
                            List.intersperse (text "\u{00A0}")
                                [ span [ class "is-size-6 has-text-weight-semibold has-text-grey" ] [ text (txt.title ++ " |\u{00A0}"), tensionTypeSpan "has-text-weight-medium" "text" form.tension_type ] ]
                        , div [ class "level-right" ] <| edgeArrow "button" (text form.source.name) (text form.target.name)
                        ]
                    ]
                , div [ class "modal-card-body" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ input
                                [ class "input autofocus followFocus"
                                , attribute "data-nextfocus" "textAreaModal"
                                , type_ "text"
                                , placeholder "Name"
                                , onInput <| changePostMsg "name"
                                ]
                                []
                            ]
                        , p [ class "help-label" ] [ text txt.name_help ]
                        ]
                    , div [ class "box has-background-grey-lighter subForm" ]
                        [ div [ class "field is-horizontal" ]
                            [ div [ class "field-label is-small has-text-grey-darker" ] [ text "Tension title" ]
                            , div [ class "field-body control" ]
                                [ input
                                    [ class "input is-small"
                                    , type_ "text"
                                    , value title
                                    , onInput <| changePostMsg "title"
                                    ]
                                    []
                                ]
                            ]
                        , div [ class "field is-horizontal" ]
                            [ div [ class "field-label is-small has-text-grey-darker" ] [ text "Identifier" ]
                            , div [ class "field-body control" ]
                                [ input
                                    [ class "input is-small"
                                    , type_ "text"
                                    , value nameid
                                    , onInput <| changePostMsg "nameid"
                                    ]
                                    []
                                ]
                            ]
                        , p [ class "help-label is-pulled-left", attribute "style" "margin-top: 4px !important;" ] [ text T.autoFieldMessageHelp ]
                        ]
                    , br [] []
                    , case form.type_ of
                        NodeType.Circle ->
                            div [ class "box has-background-grey-lighter subForm" ] <|
                                (List.indexedMap
                                    (\i uname ->
                                        div [ class "field is-horizontal" ]
                                            [ div [ class "field-label is-small has-text-grey-darker" ]
                                                [ "Coordinator"
                                                    ++ (if i > 0 then
                                                            " " ++ String.fromInt i

                                                        else
                                                            ""
                                                       )
                                                    |> text
                                                ]
                                            , div [ class "field-body control" ]
                                                [ input
                                                    [ class "input is-small"
                                                    , type_ "text"
                                                    , value ("@" ++ uname)
                                                    , onInput <| changePostMsg "first_links"
                                                    ]
                                                    []
                                                ]
                                            ]
                                    )
                                    firstLinks
                                    ++ [ p [ class "help-label is-pulled-left", attribute "style" "margin-top: 4px !important;" ] [ text txt.firstLink_help ] ]
                                )

                        NodeType.Role ->
                            div [ class "box has-background-grey-lighter subForm" ] <|
                                (List.indexedMap
                                    (\i uname ->
                                        div [ class "field is-horizontal" ]
                                            [ div [ class "field-label is-small has-text-grey-darker control" ]
                                                [ div [ class ("select is-" ++ roleColor form.role_type) ]
                                                    [ RoleType.list
                                                        |> List.filter (\r -> r /= RoleType.Guest && r /= RoleType.Member)
                                                        |> List.map
                                                            (\r ->
                                                                option [ selected (form.role_type == r), value (RoleType.toString r) ] [ RoleType.toString r |> text ]
                                                            )
                                                        |> select [ class "has-text-dark", onInput <| changePostMsg "role_type" ]
                                                    ]
                                                ]
                                            , div [ class "field-body control" ]
                                                [ input
                                                    [ class "input is-small"
                                                    , type_ "text"
                                                    , value ("@" ++ uname)
                                                    , onInput <| changePostMsg "first_links"
                                                    ]
                                                    []
                                                ]
                                            ]
                                    )
                                    firstLinks
                                    ++ [ p [ class "help-label is-pulled-left", attribute "style" "margin-top: 4px !important;" ] [ text txt.firstLink_help ] ]
                                )
                    , br [] []
                    , div [ class "field" ]
                        [ div [ class "control" ]
                            [ textarea
                                [ id "textAreaModal"
                                , class "textarea"
                                , rows 5
                                , placeholder T.leaveComment
                                , onInput <| changePostMsg "message"
                                ]
                                []
                            ]
                        , p [ class "help-label" ] [ text txt.message_help ]
                        ]
                    , br [] []
                    , div [ class "card" ]
                        [ div [ class "cnard-header" ] [ div [ class "card-header-title" ] [ text T.mandateH ] ]
                        , div [ class "card-content" ]
                            [ div [ class "field" ]
                                [ div [ class "label" ] [ text T.purposeH ]
                                , div [ class "control" ]
                                    [ textarea
                                        [ id "textAreaModal"
                                        , class "textarea"
                                        , rows 5
                                        , placeholder txt.ph_purpose
                                        , onInput <| changePostMsg "purpose"
                                        ]
                                        []
                                    ]
                                ]
                            , div [ class "field" ]
                                [ div [ class "label" ] [ text T.responsabilitiesH ]
                                , div [ class "control" ]
                                    [ textarea
                                        [ id "textAreaModal"
                                        , class "textarea"
                                        , rows 5
                                        , placeholder txt.ph_responsabilities
                                        , onInput <| changePostMsg "responsabilities"
                                        ]
                                        []
                                    ]
                                ]
                            , div [ class "field" ]
                                [ div [ class "label" ] [ text T.domainsH ]
                                , div [ class "control" ]
                                    [ textarea
                                        [ id "textAreaModal"
                                        , class "textarea"
                                        , rows 5
                                        , placeholder txt.ph_domains
                                        , onInput <| changePostMsg "domains"
                                        ]
                                        []
                                    ]
                                ]
                            , div [ class "field" ]
                                [ div [ class "label" ] [ text T.policiesH ]
                                , div [ class "control" ]
                                    [ textarea
                                        [ id "textAreaModal"
                                        , class "textarea"
                                        , rows 5
                                        , placeholder txt.ph_policies
                                        , onInput <| changePostMsg "policies"
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        ]
                    , br [] []
                    ]
                , div [ class "modal-card-foot", attribute "style" "display: block;" ]
                    [ case other of
                        Failure err ->
                            viewErrors err

                        _ ->
                            div [] []
                    , div [ class "field is-grouped is-grouped-right" ]
                        [ div [ class "control" ]
                            [ if isSendable then
                                div [ class "buttons" ]
                                    [ button
                                        [ class "button is-success has-text-weight-semibold"
                                        , classList [ ( "is-loading", isLoading ) ]
                                        , onClickPD2 (submitMsg <| submitNextMsg form False)
                                        ]
                                        [ text txt.submit ]
                                    , button
                                        [ class "button is-warning is-small has-text-weight-semibold"
                                        , classList [ ( "is-loading", isLoading ) ]
                                        , onClickPD2 (submitMsg <| submitNextMsg form True)
                                        ]
                                        [ text txt.close_submit ]
                                    ]

                              else
                                div [ class "buttons" ]
                                    [ button [ class "button has-text-weight-semibold", disabled True ]
                                        [ text txt.submit ]
                                    , button [ class "button is-small has-text-weight-semibold", disabled True ]
                                        [ text txt.close_submit ]
                                    ]
                            ]
                        ]
                    ]
                ]
