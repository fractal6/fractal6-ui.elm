module Components.Node exposing (NodeInfoMsg, viewNodeInfo)

import Components.Fa as Fa
import Components.Loading as Loading exposing (viewGqlErrors)
import Components.Markdown exposing (renderMarkdown)
import Components.Text as T
import Dict
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Maybe exposing (withDefault)
import ModelCommon.Uri exposing (FractalBaseRoute(..), uriFromUsername)
import ModelCommon.View exposing (getAvatar)
import ModelSchema exposing (..)


type alias NodeInfoMsg msg1 msg2 =
    { editAbout : String -> msg1
    , editMandate : String -> msg2
    }


type alias Data_ =
    { o : GqlData NodesData -- orga
    , f : Maybe Node -- focus
    , c : List NodeId -- children
    , source : FractalBaseRoute
    }


viewNodeInfo : GqlData NodeData -> Bool -> Data_ -> Html msg
viewNodeInfo nodeData isLazy data_ =
    div [ id "DocContainer", class "hero is-small is-light" ]
        [ div [ class "hero-body" ]
            [ case data_.f of
                Just focus ->
                    case focus.role_type of
                        Just r ->
                            let
                                fs =
                                    focus.first_link |> Maybe.map (\u -> u.username) |> withDefault "[Unknown]"
                            in
                            case nodeData of
                                Failure err ->
                                    case r of
                                        RoleType.Guest ->
                                            div [] [ [ "No mandate for Guest ", fs, "." ] |> String.join "" |> text ]

                                        other ->
                                            viewGqlErrors err

                                LoadingSlowly ->
                                    div [ class "spinner" ] []

                                Success data ->
                                    viewNodeDoc data isLazy focus data_

                                other ->
                                    div [] []

                        Nothing ->
                            case nodeData of
                                Failure err ->
                                    viewGqlErrors err

                                LoadingSlowly ->
                                    div [ class "spinner" ] []

                                Success data ->
                                    viewNodeDoc data isLazy focus data_

                                other ->
                                    div [] []

                Nothing ->
                    div [] []
            ]
        ]


viewNodeDoc : NodeData -> Bool -> Node -> Data_ -> Html msg
viewNodeDoc data isLazy focus data_ =
    let
        links =
            case data_.o of
                Success ndata ->
                    case focus.type_ of
                        NodeType.Circle ->
                            data_.c
                                |> List.map (\n -> Dict.get n.nameid ndata)
                                |> List.filterMap identity
                                |> List.filter (\n -> n.role_type == Just RoleType.Coordinator)
                                |> List.map (\n -> n.first_link)
                                |> List.filterMap identity

                        NodeType.Role ->
                            focus.first_link |> Maybe.map (\fs -> [ fs ]) |> withDefault []

                _ ->
                    []
    in
    div []
        [ div [ class "aboutDoc" ]
            [ div [ class "subtitle is-5" ]
                [ span [ class "fa-stack", attribute "style" "font-size: 0.6em;" ]
                    [ i [ class "fas fa-info fa-stack-1x" ] []
                    , i [ class "far fa-circle fa-stack-2x" ] []
                    ]
                , span [ class "nodeName" ] [ text "\u{00A0}", text " ", text focus.name ]
                , span
                    [ class "is-pulled-right button-light" ]
                    [ Fa.icon0 "fas fa-xs fa-pen" "" ]
                ]
            , case data.about of
                Just ab ->
                    p [ classList [ ( "is-lazy", isLazy ) ] ] [ text ab ]

                Nothing ->
                    div [] []
            , hr [ class "has-background-grey-light" ] []
            ]
        , div [ class "linksDoc", classList [ ( "is-lazy", isLazy ) ] ]
            [ div [ class "subtitle is-5" ]
                [ Fa.icon "fas fa-users fa-sm" T.linksH
                , links
                    |> List.map
                        (\l ->
                            span [] [ a [ class "image circleBaseInline circle1", href (uriFromUsername UsersBaseUri l.username) ] [ getAvatar l.username ] ]
                        )
                    |> span [ attribute "style" "margin-left:20px;" ]
                ]
            , if List.length links == 0 then
                span [ class "is-italic" ] [ text T.noFirstLinks ]

              else
                div [] []
            ]
        , hr [ class "has-background-grey-light" ] []
        , case data.mandate of
            Just mandate ->
                div [ class "mandateDoc" ]
                    [ div [ class "subtitle is-5" ]
                        [ Fa.icon "fas fa-scroll fa-sm" T.mandateH ]
                    , viewMandateSection isLazy T.purposeH (Just mandate.purpose)
                    , viewMandateSection isLazy T.responsabilitiesH mandate.responsabilities
                    , viewMandateSection isLazy T.domainsH mandate.domains
                    , viewMandateSection isLazy T.policiesH mandate.policies
                    ]

            Nothing ->
                div [ class "is-italic" ]
                    [ text "No mandate for this circle." ]
        ]


viewMandateSection : Bool -> String -> Maybe String -> Html msg
viewMandateSection isLazy name maybePara =
    case maybePara of
        Just para ->
            div [ class "message", classList [ ( "is-lazy", isLazy ) ] ]
                [ div [ class "message-header" ] [ text name ]
                , p [ class "message-body" ] [ renderMarkdown para "is-dark" ]
                ]

        Nothing ->
            div [] []
