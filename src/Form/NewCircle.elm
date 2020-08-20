module Form.NewCircle exposing (view)

import Components.Fa as Fa
import Components.Loading as Loading exposing (viewGqlErrors)
import Components.Text as T
import Dict
import Extra exposing (ternary, withMaybeData)
import Extra.Events exposing (onClickPD, onClickPD2, onEnter, onKeydown, onTab)
import Form exposing (isPostSendable)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Generated.Route as Route exposing (toHref)
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, li, nav, option, p, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.View exposing (edgeArrow, getNodeTextFromNodeType, roleColor, tensionTypeSpan)
import ModelSchema exposing (GqlData, Node, RequestResult(..), UserRole)


{-| --view : TensionForm -> GqlData data -> subData -> Html msg
What should be the signature ?!
-}
view form result sd =
    let
        nodeType =
            form.node.type_ |> withDefault NodeType.Role

        roleType =
            form.node.role_type |> withDefault RoleType.Peer

        txt =
            getNodeTextFromNodeType nodeType

        isLoading =
            result == LoadingSlowly

        isSendable =
            form.node.name /= Nothing && (form.node.mandate |> Maybe.map (\x -> x.purpose)) /= Nothing

        submitTension =
            ternary isSendable [ onClickPD2 (sd.submitMsg <| sd.submitNextMsg form False) ] []

        submitCloseTension =
            ternary isSendable [ onClickPD2 (sd.submitMsg <| sd.submitNextMsg form True) ] []
    in
    case result of
        Success _ ->
            div [ class "box is-light" ]
                [ Fa.icon "fas fa-check fa-2x has-text-success" " "
                , case sd.tensionId of
                    Just id ->
                        let
                            link =
                                Route.Tension_Dynamic_Dynamic { param1 = form.target.rootnameid, param2 = id } |> toHref
                        in
                        span []
                            [ text (txt.tension_added ++ " ")
                            , a
                                [ href link
                                , onClickPD (sd.closeModalMsg link)
                                , target "_blank"
                                ]
                                [ text T.checkItOut ]
                            ]

                    Nothing ->
                        span [] [ text txt.added ]
                ]

        other ->
            let
                title =
                    Dict.get "title" form.post |> withDefault ""

                nameid =
                    form.node.nameid |> withDefault ""

                firstLinks_ =
                    form.node.first_link |> withDefault "" |> String.split "@" |> List.filter (\x -> x /= "")

                firstLinks =
                    ternary (firstLinks_ == []) [ "" ] firstLinks_
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
                                , attribute "data-nextfocus" "aboutField"
                                , type_ "text"
                                , placeholder "Name*"
                                , required True
                                , onInput <| sd.changePostMsg "name"
                                ]
                                []
                            ]
                        , p [ class "help-label" ] [ text txt.name_help ]
                        ]
                    , div [ class "field" ]
                        [ div [ class "control" ]
                            [ input
                                [ id "aboutField"
                                , class "input followFocus"
                                , attribute "data-nextfocus" "textAreaModal"
                                , type_ "text"
                                , placeholder "About"
                                , onInput <| sd.changePostMsg "about"
                                ]
                                []
                            ]
                        , p [ class "help-label" ] [ text txt.about_help ]
                        , br [] []
                        ]
                    , div [ class "box has-background-grey-lighter subForm" ]
                        [ div [ class "field is-horizontal" ]
                            [ div [ class "field-label is-small has-text-grey-darker" ] [ text "Tension title" ]
                            , div [ class "field-body control" ]
                                [ input
                                    [ class "input is-small"
                                    , type_ "text"
                                    , value title
                                    , onInput <| sd.changePostMsg "title"
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
                                    , onInput <| sd.changePostMsg "nameid"
                                    ]
                                    []
                                ]
                            ]
                        , p [ class "help-label is-pulled-left", attribute "style" "margin-top: 4px !important;" ] [ text T.autoFieldMessageHelp ]
                        ]
                    , br [] []
                    , div [ class "card cardForm" ]
                        [ div [ class "card-header" ] [ div [ class "card-header-title" ] [ text T.firstLinkH ] ]
                        , div [ class "card-content" ]
                            (List.indexedMap
                                (\i uname ->
                                    div [ class "field is-horizontal" ]
                                        [ div [ class "field-label is-small has-text-grey-darker control" ]
                                            [ case nodeType of
                                                NodeType.Circle ->
                                                    let
                                                        r =
                                                            RoleType.Coordinator
                                                    in
                                                    div [ class ("select is-" ++ roleColor r) ]
                                                        [ select [ class "has-text-dark", onInput <| sd.changePostMsg "role_type" ]
                                                            [ option [ selected True, value (RoleType.toString r) ] [ RoleType.toString r |> text ]
                                                            ]
                                                        ]

                                                NodeType.Role ->
                                                    div [ class ("select is-" ++ roleColor roleType) ]
                                                        [ RoleType.list
                                                            |> List.filter (\r -> r /= RoleType.Guest && r /= RoleType.Member)
                                                            |> List.map
                                                                (\r ->
                                                                    option [ selected (roleType == r), value (RoleType.toString r) ] [ RoleType.toString r |> text ]
                                                                )
                                                            |> select [ class "has-text-dark", onInput <| sd.changePostMsg "role_type" ]
                                                        ]
                                            ]
                                        , div [ class "field-body control" ]
                                            [ input
                                                [ class "input is-small"
                                                , type_ "text"
                                                , value ("@" ++ uname)
                                                , onInput <| sd.changePostMsg "first_link"
                                                ]
                                                []
                                            ]
                                        ]
                                )
                                firstLinks
                                ++ [ p [ class "help-label", attribute "style" "margin-top: 4px !important;" ] [ text txt.firstLink_help ] ]
                            )
                        ]
                    , br [] []
                    , div [ class "card cardForm" ]
                        [ div [ class "card-header" ] [ div [ class "card-header-title" ] [ text T.mandateH ] ]
                        , div [ class "card-content" ]
                            [ div [ class "field" ]
                                [ div [ class "label" ] [ text T.purposeH ]
                                , div [ class "control" ]
                                    [ textarea
                                        [ id "textAreaModal"
                                        , class "textarea"
                                        , rows 5
                                        , placeholder (txt.ph_purpose ++ "*")
                                        , required True
                                        , onInput <| sd.changePostMsg "purpose"
                                        ]
                                        []
                                    ]
                                ]
                            , div [ class "field" ]
                                [ div [ class "label" ] [ text T.responsabilitiesH ]
                                , div [ class "control" ]
                                    [ textarea
                                        [ class "textarea"
                                        , rows 5
                                        , placeholder txt.ph_responsabilities
                                        , onInput <| sd.changePostMsg "responsabilities"
                                        ]
                                        []
                                    ]
                                ]
                            , div [ class "field" ]
                                [ div [ class "label" ] [ text T.domainsH ]
                                , div [ class "control" ]
                                    [ textarea
                                        [ class "textarea"
                                        , rows 5
                                        , placeholder txt.ph_domains
                                        , onInput <| sd.changePostMsg "domains"
                                        ]
                                        []
                                    ]
                                ]
                            , div [ class "field" ]
                                [ div [ class "label" ] [ text T.policiesH ]
                                , div [ class "control" ]
                                    [ textarea
                                        [ class "textarea"
                                        , rows 5
                                        , placeholder txt.ph_policies
                                        , onInput <| sd.changePostMsg "policies"
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        ]
                    , br [] []
                    , div [ class "field" ]
                        [ div [ class "control" ]
                            [ textarea
                                [ class "textarea"
                                , rows 5
                                , placeholder T.leaveComment
                                , onInput <| sd.changePostMsg "message"
                                ]
                                []
                            ]
                        , p [ class "help-label" ] [ text txt.message_help ]
                        ]
                    , br [] []
                    ]
                , div [ class "modal-card-foot", attribute "style" "display: block;" ]
                    [ case other of
                        Failure err ->
                            viewGqlErrors err

                        _ ->
                            div [] []
                    , div [ class "field is-grouped is-grouped-right" ]
                        [ div [ class "control" ]
                            [ div [ class "buttons" ]
                                [ button
                                    ([ class "button is-small has-text-weight-semibold"
                                     , classList [ ( "is-warning", isSendable ), ( "is-loading", isLoading ) ]
                                     , disabled (not isSendable)
                                     ]
                                        ++ submitCloseTension
                                    )
                                    [ text txt.close_submit ]
                                , button
                                    ([ class "button  has-text-weight-semibold"
                                     , classList [ ( "is-success", isSendable ), ( "is-loading", isLoading ) ]
                                     , disabled (not isSendable)
                                     ]
                                        ++ submitTension
                                    )
                                    [ text txt.submit ]
                                ]
                            ]
                        ]
                    ]
                ]
