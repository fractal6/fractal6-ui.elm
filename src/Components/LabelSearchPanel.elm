{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2023 Fractale Co

   This file is part of Fractale.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
-}


module Components.LabelSearchPanel exposing (..)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Browser.Events as Events
import Bulk exposing (Ev, LabelForm, UserState(..), encodeLabel, initLabelForm)
import Bulk.Codecs exposing (FractalBaseRoute(..), uriFromNameid)
import Bulk.Error exposing (viewGqlErrors)
import Bulk.View exposing (viewLabel, viewLabels)
import Codecs exposing (LookupResult)
import Dict
import Dom
import Extra exposing (ternary)
import Fractal.Enum.TensionEvent as TensionEvent
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, div, i, input, label, nav, p, span, text)
import Html.Attributes exposing (attribute, class, classList, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, RequestResult(..), loadingSpin, rest2Gql, withMaybeData, withMaybeMapData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Ports
import Query.PatchTension exposing (setLabel)
import Query.QueryNode exposing (queryLabels, queryLabelsDown)
import Requests exposing (fetchLabelsTop)
import Session exposing (Apis, GlobalCmd(..), LabelSearchPanelOnClickAction(..))
import Text as T
import Time



{-
   A panel for multi-label selection
-}


type alias Model =
    Session.LabelSearchPanelModel


type alias OnClickAction =
    Session.LabelSearchPanelOnClickAction


type State
    = State Model


id_target_name : String
id_target_name =
    "labelsPanelContent"


init : String -> OnClickAction -> UserState -> State
init tid action user =
    initModel tid action user |> State


initModel : String -> OnClickAction -> UserState -> Model
initModel tid action user =
    { isOpen = False
    , form = initLabelForm tid user
    , click_result = NotAsked
    , action = action

    -- Lookup
    , lookup = []
    , pattern = ""
    , labels_data = NotAsked

    -- Common
    , refresh_trial = 0
    }


load : Maybe Model -> UserState -> State
load model user =
    case model of
        Just m ->
            State m

        Nothing ->
            init "" SelectLabel user


getModel : State -> Model
getModel (State model) =
    model



-- Global methods


isOpen_ : State -> Bool
isOpen_ (State model) =
    model.isOpen


isEmpty_ : State -> Bool
isEmpty_ (State model) =
    (withMaybeData model.labels_data |> withDefault [] |> List.length) == 0



-- State control


open : List String -> Model -> Model
open targets data =
    let
        form =
            data.form
    in
    { data | isOpen = True, form = { form | targets = targets } }


close : Model -> Model
close data =
    { data | isOpen = False, click_result = NotAsked, pattern = "" }


click : Label -> Bool -> Model -> Model
click label isNew data =
    let
        form =
            data.form
    in
    { data | form = { form | label = label, isNew = isNew } }


setClickResult : GqlData IdPayload -> Model -> Model
setClickResult result data =
    { data | click_result = result }



-- Update Form


setEvents : List Ev -> Model -> Model
setEvents events data =
    let
        f =
            data.form
    in
    { data | form = { f | events = events } }


updatePost : String -> String -> Model -> Model
updatePost field value data =
    let
        f =
            data.form
    in
    { data | form = { f | post = Dict.insert field value f.post } }


setPattern : String -> Model -> Model
setPattern pattern data =
    { data | pattern = pattern }



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = OnOpen (List PNode) (Maybe Bool)
    | OnClose
    | OnClose_
    | OnChangePattern String
    | ChangeLabelLookup (LookupResult Label)
    | OnLabelClick Label Bool Time.Posix
    | OnLabelClickInt Label Bool
    | OnLabelAck (GqlData IdPayload)
    | OnSubmit (Time.Posix -> Msg)
    | OnGotLabels (GqlData (List Label))
    | SetLabel LabelForm
    | ResetClickResult
      --
    | Navigate String
    | OnModalAsk String String


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe ( Bool, Label )
    }


noOut : Out
noOut =
    Out [] [] Nothing


out0 : List (Cmd Msg) -> Out
out0 cmds =
    Out cmds [] Nothing


out1 : List GlobalCmd -> Out
out1 cmds =
    Out [] cmds Nothing


out2 : List (Cmd Msg) -> List GlobalCmd -> Out
out2 cmds gcmds =
    Out cmds gcmds Nothing


update : Apis -> Msg -> State -> ( State, Out )
update apis message (State model) =
    update_ apis message model
        |> Tuple.mapFirst State


update_ : Apis -> Msg -> Model -> ( Model, Out )
update_ apis message model =
    case message of
        OnOpen targets isDepth ->
            -- case isDepth of
            -- Just True: fetch label recurcively in children.
            -- Just False: fetch label in parents until root.
            -- Nothing: stick to given targets
            if not model.isOpen then
                let
                    nameids =
                        List.map (\x -> x.nameid) targets

                    hasChanged =
                        nameids /= model.form.targets

                    ( newModel, cmd ) =
                        if hasChanged then
                            case isDepth of
                                Just True ->
                                    ( { model | labels_data = LoadingSlowly }, [ queryLabelsDown apis nameids OnGotLabels ] )

                                Just False ->
                                    ( { model | labels_data = LoadingSlowly }, [ fetchLabelsTop apis (List.head nameids |> withDefault "") True (rest2Gql >> OnGotLabels) ] )

                                Nothing ->
                                    ( { model | labels_data = LoadingSlowly }, [ queryLabels apis nameids OnGotLabels ] )

                        else
                            ( model, [] )
                in
                ( open nameids newModel
                , out0 <|
                    [ Ports.inheritWith "labelSearchPanel"
                    , Ports.focusOn "userInput"
                    ]
                        ++ cmd
                )

            else
                ( model, noOut )

        OnClose ->
            -- The delay is used to be able to toggle (close) the panel on clicking the button.
            ( model, out0 [ sendSleep OnClose_ 50 ] )

        OnClose_ ->
            ( close model, noOut )

        OnGotLabels result ->
            ( { model | labels_data = result }
            , case result of
                Success r ->
                    out0 [ Ports.initLabelSearch r ]

                _ ->
                    noOut
            )

        OnChangePattern pattern ->
            ( setPattern pattern model
            , out0 [ Ports.searchLabel pattern ]
            )

        ChangeLabelLookup data ->
            case data of
                Ok d ->
                    ( { model | lookup = d }, noOut )

                Err err ->
                    ( model, out0 [ Ports.logErr err ] )

        OnLabelClick label isNew time ->
            if model.click_result == LoadingSlowly then
                -- wait here !
                ( model, noOut )

            else
                let
                    newModel =
                        click label isNew model
                in
                case model.action of
                    AssignLabel ->
                        let
                            data =
                                newModel
                                    |> updatePost "createdAt" (fromTime time)
                                    |> setEvents
                                        [ ternary
                                            isNew
                                            (Ev TensionEvent.LabelAdded "" (encodeLabel label))
                                            (Ev TensionEvent.LabelRemoved (encodeLabel label) "")
                                        ]
                                    |> setClickResult LoadingSlowly
                        in
                        ( data
                        , out0 [ send (SetLabel data.form) ]
                        )

                    SelectLabel ->
                        let
                            data =
                                newModel
                                    |> setClickResult LoadingSlowly

                            labels =
                                withMaybeData model.labels_data
                                    |> withDefault []
                                    |> (\x ->
                                            if isNew then
                                                x ++ [ label ]

                                            else
                                                LE.remove label x
                                       )
                        in
                        ( { data | labels_data = Success labels }
                        , Out [ sendSleep ResetClickResult 333 ] [] (Just ( data.form.isNew, data.form.label ))
                        )

        OnLabelClickInt label isNew ->
            let
                data =
                    click label isNew model
            in
            ( data, Out [] [] (Just ( data.form.isNew, data.form.label )) )

        OnLabelAck result ->
            let
                data =
                    setClickResult result model
            in
            case parseErr result data.refresh_trial of
                Authenticate ->
                    ( setClickResult NotAsked data
                    , out0 [ Ports.raiseAuthModal data.form.uctx ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep (SetLabel data.form) 500 ] [ DoUpdateToken ] )

                OkAuth _ ->
                    ( data, Out [] [] (Just ( data.form.isNew, data.form.label )) )

                _ ->
                    ( data, noOut )

        OnSubmit next ->
            ( model
            , out0 [ sendNow next ]
            )

        SetLabel form ->
            ( model
            , out0 [ setLabel apis form OnLabelAck ]
            )

        ResetClickResult ->
            ( setClickResult NotAsked model, noOut )

        Navigate link ->
            ( model, out1 [ DoNavigate link ] )

        OnModalAsk link onCloseTxt ->
            ( model, out1 [ DoModalAsk link onCloseTxt ] )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    if model.isOpen then
        [ Ports.lookupLabelFromJs ChangeLabelLookup
        , Events.onMouseUp (Dom.outsideClickClose id_target_name OnClose)
        , Events.onKeyUp (Dom.key "Escape" OnClose)
        ]

    else
        []



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    { selectedLabels : List Label
    , targets : List PNode
    , isRight : Bool
    }


view_ : Bool -> Op -> Model -> Html Msg
view_ isInternal op model =
    nav [ id "labelSearchPanel", class "panel sidePanel", classList [ ( "is-right", op.isRight ) ] ]
        [ case model.labels_data of
            Success labels_d ->
                let
                    --selection =
                    --    List.map .name op_.selectedLabels
                    --op =
                    --    { op_ | selectedLabels = List.filter (\x -> List.member x.name selection) labels_d }
                    labels =
                        if model.pattern == "" then
                            List.sortBy .name op.selectedLabels
                                ++ List.sortBy .name (List.take 42 labels_d)
                                |> LE.uniqueBy .name

                        else
                            LE.uniqueBy .name model.lookup
                in
                div [] <|
                    ternary isInternal List.reverse identity <|
                        [ div [ class "panel-block" ]
                            [ p [ class "control has-icons-left" ]
                                [ input
                                    [ id "userInput"
                                    , class "input autofocus is-small"
                                    , type_ "text"
                                    , placeholder T.searchLabels
                                    , value model.pattern
                                    , onInput OnChangePattern
                                    ]
                                    []
                                , span [ class "icon is-left" ] [ i [ attribute "aria-hidden" "true", class "icon-search" ] [] ]
                                ]
                            ]
                        , case model.click_result of
                            Failure err ->
                                viewGqlErrors err

                            _ ->
                                text ""
                        , viewLabelSelectors isInternal labels op model
                        ]

            Loading ->
                div [ class "spinner" ] []

            LoadingSlowly ->
                div [ class "spinner" ] []

            NotAsked ->
                text ""

            Failure err ->
                viewGqlErrors err
        ]


viewLabelSelectors : Bool -> List Label -> Op -> Model -> Html Msg
viewLabelSelectors isInternal labels op model =
    let
        viewEdit =
            let
                editLink =
                    uriFromNameid SettingsBaseUri (List.map .nameid op.targets |> List.head |> withDefault "") [] ++ "?m=labels&a=new"
            in
            p
                [ class "panel-block is-md is-w discrete-link"
                , if isInternal then
                    attribute "style" "border-bottom: 1px solid;"

                  else
                    attribute "style" "border-top: 1px solid;"
                , if isInternal then
                    onClick (OnModalAsk editLink "")

                  else
                    onClick (Navigate editLink)
                ]
                [ A.icon1 "icon-edit-2" T.editLabels ]
    in
    div []
        [ ternary isInternal viewEdit (text "")
        , div [ class "selectors" ] <|
            if labels == [] then
                [ p [ class "panel-block" ] [ text T.noResultsFound ] ]

            else
                labels
                    |> List.map
                        (\l ->
                            let
                                isActive =
                                    List.member l.name (List.map .name op.selectedLabels)

                                -- Map label has atttribute (such has color, is lost when params
                                -- object from url.
                                l_ =
                                    if isActive then
                                        model.labels_data
                                            |> withMaybeMapData
                                                (List.filter (\x -> x.name == l.name) >> List.head)
                                            |> withDefault Nothing
                                            |> withDefault l

                                    else
                                        l

                                iconCls =
                                    ternary isActive "icon-check-square" "icon-square"

                                isLoading =
                                    model.click_result == LoadingSlowly && l.id == model.form.label.id
                            in
                            p
                                [ class "panel-block"
                                , classList [ ( "is-active", isActive ) ]
                                , ternary isInternal
                                    (onClick (OnLabelClickInt l (not isActive)))
                                    (onClick (OnSubmit <| OnLabelClick l (not isActive)))
                                ]
                                [ span [ class "panel-icon" ] [ A.icon iconCls ]
                                , viewLabel "" Nothing l_
                                , loadingSpin isLoading
                                ]
                        )
        , ternary isInternal (text "") viewEdit
        ]



--
-- Input View
--


view : Op -> State -> Html Msg
view op (State model) =
    div [ id id_target_name ]
        [ if model.isOpen then
            view_ False op model

          else
            text ""
        ]


viewNew : Op -> State -> Html Msg
viewNew op (State model) =
    div []
        [ div [ id id_target_name, class "is-reversed" ]
            [ if model.isOpen then
                view_ True op model

              else
                text ""
            ]
        , div
            [ class "button is-small  mr-2"
            , onClick (OnOpen op.targets Nothing)
            ]
            [ A.icon1 "icon-1x icon-tag" "", text T.labels ]
        , if List.length op.selectedLabels > 0 then
            viewLabels Nothing op.selectedLabels

          else
            text ""
        ]
