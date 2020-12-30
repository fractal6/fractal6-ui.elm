module Components.LabelSearchPanel exposing (..)

import Components.Fa as Fa
import Components.Loading as Loading exposing (GqlData, RequestResult(..), viewGqlErrors, withMapData, withMaybeData)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Fractal.Enum.TensionEvent as TensionEvent
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, selected, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (UserState(..))
import ModelCommon.Codecs exposing (nearestCircleid)
import ModelCommon.View exposing (viewLabel)
import ModelSchema exposing (..)
import Text as T
import Time


type alias LabelSearchPanel =
    { isOpen : Bool
    , form : LabelForm
    , click_result : GqlData IdPayload
    }


type alias LabelForm =
    { uctx : UserCtx
    , pattern : String
    , nameid : String
    , isNew : Bool
    , label : Label
    , events_type : Maybe (List TensionEvent.TensionEvent)
    , post : Post
    }


initLabelForm : UserState -> String -> LabelForm
initLabelForm user nameid =
    { uctx =
        case user of
            LoggedIn uctx ->
                uctx

            LoggedOut ->
                UserCtx "" Nothing (UserRights False False) []
    , pattern = ""
    , nameid = nameid
    , isNew = False
    , label = Label "" "" Nothing
    , events_type = Nothing
    , post = Dict.empty
    }


init : UserState -> String -> LabelSearchPanel
init user nid =
    { isOpen = False
    , form = initLabelForm user nid
    , click_result = NotAsked
    }



-- State control


open : LabelSearchPanel -> LabelSearchPanel
open data =
    { data | isOpen = True }


close : LabelSearchPanel -> LabelSearchPanel
close data =
    let
        form =
            data.form
    in
    { data | isOpen = False, click_result = NotAsked, form = { form | pattern = "" } }


click : Label -> Bool -> LabelSearchPanel -> LabelSearchPanel
click label isNew data =
    let
        form =
            data.form
    in
    { data | form = { form | label = label, isNew = isNew } }


setClickResult : GqlData IdPayload -> LabelSearchPanel -> LabelSearchPanel
setClickResult result data =
    { data | click_result = result }



-- Update Form


setEvents : List TensionEvent.TensionEvent -> LabelSearchPanel -> LabelSearchPanel
setEvents events data =
    let
        f =
            data.form
    in
    { data | form = { f | events_type = Just events } }


post : String -> String -> LabelSearchPanel -> LabelSearchPanel
post field value data =
    let
        f =
            data.form
    in
    { data | form = { f | post = Dict.insert field value f.post } }


setPattern : String -> LabelSearchPanel -> LabelSearchPanel
setPattern pattern data =
    let
        form =
            data.form
    in
    { data | form = { form | pattern = pattern } }


type alias Op msg =
    { selectedLabels : List Label
    , targets : List String
    , labels_data : GqlData LabelssData
    , lookup : List Label
    , data : LabelSearchPanel
    , onChangePattern : String -> msg
    , onLabelClick : Label -> Bool -> Time.Posix -> msg
    , onSubmit : (Time.Posix -> msg) -> msg
    }


view : Op msg -> Html msg
view op =
    nav [ id "userSearchPanel", class "panel" ]
        [ case op.labels_data of
            Success ud ->
                let
                    linked_labels =
                        List.foldl
                            (\a b ->
                                List.append (Dict.get a ud |> withDefault []) b
                            )
                            []
                            op.targets

                    labels =
                        if op.data.form.pattern == "" then
                            op.selectedLabels
                                ++ linked_labels
                                |> LE.uniqueBy (\u -> u.name)

                        else
                            LE.uniqueBy (\u -> u.name) op.lookup
                in
                div []
                    [ div [ class "panel-block" ]
                        [ p [ class "control has-icons-left" ]
                            [ input
                                [ id "userInput"
                                , class "input autofocus"
                                , type_ "text"
                                , placeholder T.searchLabels
                                , value op.data.form.pattern
                                , onInput op.onChangePattern
                                ]
                                []
                            , span [ class "icon is-left" ] [ i [ attribute "aria-hidden" "true", class "fas fa-search" ] [] ]
                            ]
                        ]
                    , case op.data.click_result of
                        Failure err ->
                            viewGqlErrors err

                        _ ->
                            div [] []
                    , viewLabelSelectors labels op
                    ]

            Loading ->
                div [ class "spinner" ] [ text "" ]

            LoadingSlowly ->
                div [ class "spinner" ] [ text "" ]

            NotAsked ->
                div [] []

            Failure err ->
                viewGqlErrors err
        ]


viewLabelSelectors : List Label -> Op msg -> Html msg
viewLabelSelectors labels op =
    div [ class "selectors" ] <|
        if labels == [] then
            [ p [ class "panel-block" ] [ text T.noResultsFound ] ]

        else
            labels
                |> List.map
                    (\l ->
                        let
                            isActive =
                                List.member l op.selectedLabels

                            faCls =
                                ternary isActive "fa-check-square" "fa-square"
                        in
                        p
                            [ class "panel-block"
                            , classList [ ( "is-active", isActive ) ]
                            , onClick (op.onSubmit <| op.onLabelClick l (isActive == False))
                            ]
                            [ span [ class "panel-icon" ] [ Fa.icon0 ("far " ++ faCls) "" ]
                            , viewLabel "" l
                            ]
                    )
