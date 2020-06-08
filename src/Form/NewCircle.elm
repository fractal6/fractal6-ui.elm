module Form.NewCircle exposing (view)

import Components.Loading as Loading exposing (viewErrors)
import Dict
import Extra.Events exposing (onClickPD2, onEnter, onKeydown, onTab)
import Form exposing (isPostSendable)
import Fractal.Enum.RoleType as RoleType
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, li, nav, option, p, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, placeholder, rows, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.View exposing (edgeArrow, tensionTypeSpan)
import ModelSchema exposing (GqlData, Node, RequestResult(..), UserRole)


{-| --view : CircleForm -> GqlData (Maybe Node) -> (String -> String -> msg) -> ((msg -> CircleForm) -> Time.Posix -> msg) -> (CircleForm -> Time.Posix -> msg) -> Html msg
hat should be the signature ?!
-}
view form result changePostMsg closeModalMsg submitMsg submitNextMsg =
    let
        isSendable =
            isPostSendable [ "name", "purpose" ] form.post

        isLoading =
            result == LoadingSlowly
    in
    case result of
        Success _ ->
            div [ class "box has-background-success", onClick (closeModalMsg "") ] [ text "Tension added (new Circle)." ]

        other ->
            let
                title =
                    Dict.get "title" form.post |> withDefault ""

                nameid =
                    Dict.get "nameid" form.post |> withDefault ""

                coordos =
                    Dict.get "first_links" form.post |> withDefault "" |> String.split "@" |> List.filter (\x -> x /= "")
            in
            div [ class "modal-card finalModal" ]
                [ div [ class "modal-card-head" ]
                    [ div [ class "level modal-card-title" ]
                        [ div [ class "level-left" ] <|
                            List.intersperse (text "\u{00A0}")
                                [ span [ class "is-size-6 has-text-weight-semibold has-text-grey" ] [ text "New circle |\u{00A0}", tensionTypeSpan "has-text-weight-medium" "text" form.tensionType ] ]
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
                        , p [ class "help-label" ] [ text "Name of the circle." ]
                        ]
                    , div [ class "box has-background-grey-lighter subForm" ]
                        [ div [ class "field is-horizontal" ]
                            [ div [ class "field-label is-small has-text-grey-darker" ] [ text "Title" ]
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
                        ]
                    , coordos
                        |> List.indexedMap
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
                        |> div [ class "box has-background-grey-lighter subForm" ]
                    , div [ class "field" ]
                        [ div [ class "control" ]
                            [ textarea
                                [ id "textAreaModal"
                                , class "textarea"
                                , rows 5
                                , placeholder "Leave a comment"
                                , onInput <| changePostMsg "message"
                                ]
                                []
                            ]
                        , p [ class "help-label" ] [ text "Add a description to help others understand why a new circle should be created." ]
                        ]
                    , br [] []
                    , div [ class "card" ]
                        [ div [ class "card-header" ] [ div [ class "card-header-title" ] [ text "Mandate" ] ]
                        , div [ class "card-content" ]
                            [ div [ class "field" ]
                                [ div [ class "label" ] [ text "Purpose" ]
                                , div [ class "control" ]
                                    [ textarea
                                        [ id "textAreaModal"
                                        , class "textarea"
                                        , rows 5
                                        , placeholder "Define the purpose of the circle."
                                        , onInput <| changePostMsg "purpose"
                                        ]
                                        []
                                    ]
                                ]
                            , div [ class "field" ]
                                [ div [ class "label" ] [ text "Responsabilities" ]
                                , div [ class "control" ]
                                    [ textarea
                                        [ id "textAreaModal"
                                        , class "textarea"
                                        , rows 5
                                        , placeholder "Define the circle responsabilities."
                                        , onInput <| changePostMsg "responsabilities"
                                        ]
                                        []
                                    ]
                                ]
                            , div [ class "field" ]
                                [ div [ class "label" ] [ text "Domains" ]
                                , div [ class "control" ]
                                    [ textarea
                                        [ id "textAreaModal"
                                        , class "textarea"
                                        , rows 5
                                        , placeholder "Define the circle domains."
                                        , onInput <| changePostMsg "responsabilities"
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
                                        [ text "Submit new Circle" ]
                                    , button
                                        [ class "button is-warning is-small has-text-weight-semibold"
                                        , classList [ ( "is-loading", isLoading ) ]
                                        , onClickPD2 (submitMsg <| submitNextMsg form True)
                                        ]
                                        [ text "Create Circle and close tension" ]
                                    ]

                              else
                                div [ class "buttons" ]
                                    [ button [ class "button has-text-weight-semibold", disabled True ]
                                        [ text "Submit new Circle" ]
                                    , button [ class "button is-small has-text-weight-semibold", disabled True ]
                                        [ text "Create Circle and close tension" ]
                                    ]
                            ]
                        ]
                    ]
                ]
