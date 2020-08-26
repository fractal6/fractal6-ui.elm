module Components.Node exposing (nodeFragmentFromOrga, viewNodeInfo)

import Components.Fa as Fa
import Components.Loading as Loading exposing (viewGqlErrors)
import Components.Markdown exposing (renderMarkdown)
import Components.Text as T
import Dict
import Extra exposing (withMaybeData)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Maybe exposing (withDefault)
import ModelCommon.Uri exposing (FractalBaseRoute(..), NodeFocus, uriFromUsername)
import ModelCommon.View exposing (getAvatar)
import ModelSchema exposing (..)


type alias NodeInfoMsg msg1 msg2 =
    { editAbout : String -> msg1
    , editMandate : String -> msg2
    }


type alias OrgaNodeData data =
    { data : GqlData data -- payload
    , node : NodeFragment
    , isLazy : Bool
    , source : FractalBaseRoute
    , tid : String
    , focus : NodeFocus
    }


viewNodeInfo : OrgaNodeData d -> Html msg
viewNodeInfo data =
    div [ id "DocContainer", class "hero is-small is-light" ]
        [ div [ class "hero-body" ]
            [ case data.data of
                Failure err ->
                    if data.node.role_type == Just RoleType.Guest then
                        let
                            fs =
                                data.node.first_link |> withDefault "[Unknown]"
                        in
                        div [] [ [ "No mandate for Guest ", fs, "." ] |> String.join "" |> text ]

                    else
                        viewGqlErrors err

                LoadingSlowly ->
                    div [ class "spinner" ] []

                Success _ ->
                    viewNodeDoc data

                other ->
                    div [] []
            ]
        ]


viewNodeDoc : OrgaNodeData d -> Html msg
viewNodeDoc data =
    let
        node =
            data.node

        links =
            case node.type_ |> withDefault NodeType.Role of
                NodeType.Circle ->
                    node.children
                        |> Maybe.map
                            (\c ->
                                List.map (\c_ -> c_.first_link) c
                            )
                        |> withDefault []
                        |> List.filterMap identity

                NodeType.Role ->
                    [ node.first_link ] |> List.filterMap identity

        name =
            node.name |> withDefault ""
    in
    div [ classList [ ( "is-lazy", data.isLazy ) ] ]
        [ div [ class "aboutDoc" ]
            [ div [ class "columns is-mobile" ]
                [ div [ class "column is-9 subtitle is-5" ]
                    [ span [ class "fa-stack", attribute "style" "font-size: 0.6em;" ]
                        [ i [ class "fas fa-info fa-stack-1x" ] []
                        , i [ class "far fa-circle fa-stack-2x" ] []
                        ]
                    , span [ class "nodeName" ] [ text "\u{00A0}", text " ", text name ]
                    ]
                , case data.source of
                    OverviewBaseUri ->
                        div [ class "column is-3" ]
                            [ span
                                [ class "is-pulled-right field has-addons docButtons" ]
                                [ a
                                    [ class "control"
                                    , href (Route.Tension_Dynamic_Dynamic_Action { param1 = data.focus.rootnameid, param2 = data.tid } |> toHref)
                                    ]
                                    [ div [ class "button is-small is-rounded" ] [ Fa.icon0 "fas fa-eye" "" ] ]
                                , a
                                    [ class "control"
                                    , href (Route.Tension_Dynamic_Dynamic_Action { param1 = data.focus.rootnameid, param2 = data.tid } |> toHref)
                                    , (Route.Tension_Dynamic_Dynamic_Action { param1 = data.focus.rootnameid, param2 = data.tid } |> toHref) ++ "?v=edit" |> href
                                    ]
                                    [ div [ class "button is-small" ] [ Fa.icon0 "fas fa-pen" "" ] ]
                                , a
                                    [ class "control"
                                    , (Route.Tension_Dynamic_Dynamic_Action { param1 = data.focus.rootnameid, param2 = data.tid } |> toHref) ++ "?v=history" |> href
                                    ]
                                    [ div [ class "button is-small is-rounded" ] [ Fa.icon0 "fas fa-history" "" ] ]
                                ]
                            ]

                    _ ->
                        div [] []
                ]
            , case node.about of
                Just ab ->
                    p [ class "column" ] [ text ab ]

                Nothing ->
                    div [] []
            , hr [ class "has-background-grey-light" ] []
            ]
        , div [ class "linksDoc" ]
            [ div [ class "subtitle is-5" ]
                [ Fa.icon "fas fa-users fa-sm" T.linksH
                , links
                    |> List.map
                        (\l ->
                            span [] [ a [ class "image circleBaseInline circle1", href (uriFromUsername UsersBaseUri l) ] [ getAvatar l ] ]
                        )
                    |> span [ attribute "style" "margin-left:20px;" ]
                ]
            , if List.length links == 0 then
                span [ class "is-italic" ] [ text T.noFirstLinks ]

              else
                div [] []
            ]
        , hr [ class "has-background-grey-light" ] []
        , case node.mandate of
            Just mandate ->
                div [ class "mandateDoc" ]
                    [ div [ class "subtitle is-5" ]
                        [ Fa.icon "fas fa-scroll fa-sm" T.mandateH ]
                    , viewMandateSection T.purposeH (Just mandate.purpose)
                    , viewMandateSection T.responsabilitiesH mandate.responsabilities
                    , viewMandateSection T.domainsH mandate.domains
                    , viewMandateSection T.policiesH mandate.policies
                    ]

            Nothing ->
                div [ class "is-italic" ]
                    [ text "No mandate for this circle." ]
        ]


viewMandateSection : String -> Maybe String -> Html msg
viewMandateSection name maybePara =
    case maybePara of
        Just para ->
            div [ class "message" ]
                [ div [ class "message-header" ] [ text name ]
                , p [ class "message-body" ] [ renderMarkdown para "is-dark" ]
                ]

        Nothing ->
            div [] []



--    let
--        txt =
--            getNodeTextFromAction action
--    in
--    div []
--        [ case nf.mandate of
--            Just mandate ->
--                div [ class "card" ]
--                    [ div [ class "card-header" ] [ div [ class "card-header-title" ] [ text T.mandateH ] ]
--                    , div [ class "card-content" ]
--                        [ div [ class "field" ]
--                            [ div [ class "label" ] [ text T.purposeH ]
--                            , div [ class "control" ]
--                                [ textarea
--                                    [ id "textAreaModal"
--                                    , class "textarea"
--                                    , rows 5
--                                    , readonly True
--                                    , value mandate.purpose
--
--                                    --, placeholder (txt.ph_purpose ++ "*")
--                                    --, onInput <| changePostMsg "purpose"
--                                    ]
--                                    []
--                                ]
--                            ]
--                        , div [ class "field" ]
--                            [ div [ class "label" ] [ text T.responsabilitiesH ]
--                            , div [ class "control" ]
--                                [ textarea
--                                    [ id "textAreaModal"
--                                    , class "textarea"
--                                    , rows 5
--                                    , readonly True
--                                    , value (mandate.responsabilities |> withDefault ("<" ++ T.noResponsabilities ++ ">"))
--
--                                    --, placeholder txt.ph_responsabilities
--                                    --, onInput <| changePostMsg "responsabilities"
--                                    ]
--                                    []
--                                ]
--                            ]
--                        , div [ class "field" ]
--                            [ div [ class "label" ] [ text T.domainsH ]
--                            , div [ class "control" ]
--                                [ textarea
--                                    [ id "textAreaModal"
--                                    , class "textarea"
--                                    , rows 5
--                                    , readonly True
--                                    , value (mandate.domains |> withDefault ("<" ++ T.noDomains ++ ">"))
--
--                                    --, placeholder txt.ph_domains
--                                    --, onInput <| changePostMsg "domains"
--                                    ]
--                                    []
--                                ]
--                            ]
--                        , div [ class "field" ]
--                            [ div [ class "label" ] [ text T.policiesH ]
--                            , div [ class "control" ]
--                                [ textarea
--                                    [ id "textAreaModal"
--                                    , class "textarea"
--                                    , rows 5
--                                    , readonly True
--                                    , value (mandate.policies |> withDefault ("<" ++ T.noPolicies ++ ">"))
--
--                                    --, placeholder txt.ph_policies
--                                    --, onInput <| changePostMsg "policies"
--                                    ]
--                                    []
--                                ]
--                            ]
--                        ]
--                    ]
--
--            Nothing ->
--                div [] []
--        ]
-- Utils


nodeFragmentFromOrga : Maybe Node -> GqlData NodeData -> List NodeId -> NodesData -> NodeFragment
nodeFragmentFromOrga node_m nodeData c ndata =
    let
        children =
            node_m
                |> Maybe.map
                    (\node ->
                        case node.type_ of
                            NodeType.Circle ->
                                c
                                    |> List.map (\n -> Dict.get n.nameid ndata)
                                    |> List.filterMap identity
                                    |> List.filter (\n -> n.role_type == Just RoleType.Coordinator)
                                    |> List.map
                                        (\n ->
                                            { name = Just n.name
                                            , nameid = Just n.nameid
                                            , type_ = Just n.type_
                                            , isPrivate = Just n.isPrivate
                                            , charac = Just n.charac
                                            , role_type = n.role_type
                                            , about = Nothing
                                            , mandate = Nothing
                                            , first_link = n.first_link |> Maybe.map (\u -> u.username)
                                            }
                                        )
                                    |> Just

                            --|> List.map (\n -> n.first_link)
                            --|> List.filterMap identity
                            NodeType.Role ->
                                Nothing
                    )
    in
    { name = Maybe.map (\n -> n.name) node_m
    , nameid = Maybe.map (\n -> n.nameid) node_m
    , type_ = Maybe.map (\n -> n.type_) node_m
    , isPrivate = Maybe.map (\n -> n.isPrivate) node_m
    , charac = Maybe.map (\n -> n.charac) node_m
    , role_type = Maybe.map (\n -> n.role_type) node_m |> withDefault Nothing
    , about = Maybe.map (\n -> n.about) (withMaybeData nodeData) |> withDefault Nothing
    , mandate = Maybe.map (\n -> n.mandate) (withMaybeData nodeData) |> withDefault Nothing
    , first_link = Maybe.map (\n -> n.first_link |> Maybe.map (\u -> u.username)) node_m |> withDefault Nothing
    , children = children |> withDefault Nothing
    }
