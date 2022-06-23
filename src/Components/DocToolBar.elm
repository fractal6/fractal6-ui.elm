module Components.DocToolBar exposing (ActionView(..), viewStatus, viewToolbar)

import Assets as A
import Components.Loading exposing (GqlData, RequestResult(..), loadingSpin)
import Extra exposing (ternary)
import Extra.Date exposing (formatDate)
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Maybe exposing (withDefault)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), NodeFocus, uriFromUsername)
import ModelSchema exposing (..)
import Text as T exposing (textH, textT, upH)
import Time


type alias Op =
    { focus : NodeFocus
    , tid : String
    , actionView : Maybe ActionView
    }


type alias Op2 msg =
    { focus : NodeFocus
    , tid : String
    , actionView : Maybe ActionView
    , blob : Blob
    , isAdmin : Bool
    , publish_result : GqlData BlobFlag
    , now : Time.Posix
    , onSubmit : (Time.Posix -> msg) -> msg
    , onPushBlob : String -> Time.Posix -> msg
    }


type ActionView
    = DocView
    | DocEdit
    | DocVersion
    | NoView


viewToolbar : Op -> Html msg
viewToolbar op =
    let
        iconOpts =
            ternary (op.actionView == Nothing) "icon-xs" ""
    in
    div [ class "field has-addons docToolbar" ]
        [ p
            [ class "control tooltip has-tooltip-arrow"
            , attribute "data-tooltip" (upH T.view)
            ]
            [ a
                [ class "button is-small is-rounded is-discrete"
                , classList [ ( "is-active", op.actionView == Just DocView ) ]
                , href
                    (Route.Tension_Dynamic_Dynamic_Action { param1 = op.focus.rootnameid, param2 = op.tid } |> toHref)
                ]
                [ A.icon ("icon-eye " ++ iconOpts) ]
            ]
        , p
            [ class "control tooltip has-tooltip-arrow"
            , attribute "data-tooltip" (upH T.edit)
            ]
            [ a
                [ class "button is-small is-rounded  is-discrete"
                , classList [ ( "is-active", op.actionView == Just DocEdit ) ]
                , href
                    ((Route.Tension_Dynamic_Dynamic_Action { param1 = op.focus.rootnameid, param2 = op.tid } |> toHref) ++ "?v=edit")
                ]
                [ A.icon ("icon-edit-2 " ++ iconOpts) ]
            ]
        , p
            [ class "control tooltip has-tooltip-arrow"
            , attribute "data-tooltip" (upH T.revisions)
            ]
            [ a
                [ class "button is-small is-rounded  is-discrete"
                , classList [ ( "is-active", op.actionView == Just DocVersion ) ]
                , href
                    ((Route.Tension_Dynamic_Dynamic_Action { param1 = op.focus.rootnameid, param2 = op.tid } |> toHref) ++ "?v=history")
                ]
                [ A.icon ("icon-history " ++ iconOpts) ]
            ]
        ]


viewStatus : Op2 msg -> Html msg
viewStatus op =
    case op.blob.pushedFlag of
        Just flag ->
            div [ class "has-text-success is-italic" ]
                [ textH (T.published ++ " " ++ formatDate op.now flag) ]

        Nothing ->
            let
                isLoading =
                    op.publish_result == LoadingSlowly
            in
            div [ class "field has-addons" ]
                [ div [ class "has-text-warning is-italic mr-3" ]
                    [ textH T.revisionNotPublished ]
                , if op.isAdmin then
                    div
                        [ class "button is-small is-success has-text-weight-semibold"
                        , onClick (op.onSubmit <| op.onPushBlob op.blob.id)
                        ]
                        [ A.icon1 "icon-share" (upH T.publish)
                        , loadingSpin isLoading
                        ]

                  else
                    text ""
                ]
