module Components.LabelSearchPanel exposing (..)

import Auth exposing (ErrState(..), parseErr)
import Browser.Events as Events
import Codecs exposing (LookupResult)
import Components.Loading as Loading exposing (GqlData, RequestResult(..), loadingSpin, viewGqlErrors, withMapData, withMaybeData, withMaybeDataMap)
import Dict exposing (Dict)
import Dom
import Extra exposing (ternary)
import Fractal.Enum.TensionEvent as TensionEvent
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, selected, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Icon as I
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (LabelForm, UserState(..), initLabelForm)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), nearestCircleid, uriFromNameid)
import ModelCommon.View exposing (viewLabel, viewLabels)
import ModelSchema exposing (..)
import Ports
import Query.PatchTension exposing (setLabel)
import Query.QueryNode exposing (queryLabelsUp)
import Session exposing (Apis, GlobalCmd(..), LabelSearchPanelOnClickAction(..))
import Task
import Text as T exposing (textH, textT, upH)
import Time


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


setEvents : List TensionEvent.TensionEvent -> Model -> Model
setEvents events data =
    let
        f =
            data.form
    in
    { data | form = { f | events_type = Just events } }


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
    = OnOpen (List String)
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
        OnOpen targets ->
            if model.isOpen == False then
                let
                    ( newModel, cmd ) =
                        ternary (targets /= model.form.targets)
                            ( { model | labels_data = LoadingSlowly }, [ queryLabelsUp apis.gql targets OnGotLabels ] )
                            ( model, [] )
                in
                ( open targets newModel
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
                                    |> updatePost (ternary isNew "new" "old") (label.name ++ "§" ++ withDefault "" label.color)
                                    |> setEvents [ ternary isNew TensionEvent.LabelAdded TensionEvent.LabelRemoved ]
                                    |> setClickResult LoadingSlowly
                        in
                        ( data
                        , out0 [ send (SetLabel data.form) ]
                        )

                    SelectLabel ->
                        let
                            data =
                                newModel
                                    |> updatePost (ternary isNew "new" "old") (label.name ++ "§" ++ withDefault "" label.color)
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
                        |> updatePost "new" (label.name ++ "§" ++ withDefault "" label.color)
                        |> setEvents [ TensionEvent.LabelAdded ]
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
                    , out1 [ DoAuth data.form.uctx ]
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
            , out0 [ setLabel apis.gql form OnLabelAck ]
            )

        ResetClickResult ->
            ( setClickResult NotAsked model, noOut )

        Navigate link ->
            ( model, out1 [ DoNavigate link ] )

        OnModalAsk link onCloseTxt ->
            ( model, out1 [ DoModalAsk link onCloseTxt ] )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    [ Ports.lookupLabelFromJs ChangeLabelLookup
    ]
        ++ (if model.isOpen then
                [ Events.onMouseUp (Dom.outsideClickClose id_target_name OnClose)
                , Events.onKeyUp (Dom.key "Escape" OnClose)
                ]

            else
                []
           )



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    { selectedLabels : List Label
    , targets : List String
    }


view_ : Bool -> Op -> State -> Html Msg
view_ isInternal op (State model) =
    nav [ id "labelSearchPanel", class "panel sidePanel" ]
        [ case model.labels_data of
            Success labels_d ->
                let
                    --selection =
                    --    List.map (\x -> x.name) op_.selectedLabels
                    --op =
                    --    { op_ | selectedLabels = List.map (\x -> ternary (List.member x.name selection) x ) selection }
                    labels =
                        if model.pattern == "" then
                            op.selectedLabels
                                ++ labels_d
                                |> LE.uniqueBy (\u -> u.name)

                        else
                            LE.uniqueBy (\u -> u.name) model.lookup
                in
                div [] <|
                    ternary isInternal List.reverse identity <|
                        [ div [ class "panel-block" ]
                            [ p [ class "control has-icons-left" ]
                                [ input
                                    [ id "userInput"
                                    , class "input autofocus is-small"
                                    , type_ "text"
                                    , placeholder (upH T.searchLabels)
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
                                div [] []
                        , viewLabelSelectors isInternal labels op model
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


viewLabelSelectors : Bool -> List Label -> Op -> Model -> Html Msg
viewLabelSelectors isInternal labels op model =
    let
        viewEdit =
            p
                [ class "panel-block is-md"
                , attribute "style" "border-top: 1px solid;"
                , if isInternal then
                    onClick (OnModalAsk (uriFromNameid SettingsBaseUri (List.head op.targets |> withDefault "")) "")

                  else
                    onClick (Navigate (uriFromNameid SettingsBaseUri (List.head op.targets |> withDefault "")))
                ]
                [ I.icon1 "icon-edit-2" <| upH T.editLabels ]
    in
    div []
        [ ternary isInternal viewEdit (text "")
        , div [ class "selectors" ] <|
            if labels == [] then
                [ p [ class "panel-block" ] [ textH T.noResultsFound ] ]

            else
                labels
                    |> List.map
                        (\l ->
                            let
                                isActive =
                                    List.member l op.selectedLabels

                                -- Map label has atttribuyte (such has color, is lost when params
                                -- object from url.
                                l_ =
                                    if isActive then
                                        model.labels_data
                                            |> withMaybeDataMap
                                                (\labels_d ->
                                                    labels_d
                                                        |> List.filter (\x -> x.name == l.name)
                                                        |> List.head
                                                )
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
                                    (onClick (OnLabelClickInt l (isActive == False)))
                                    (onClick (OnSubmit <| OnLabelClick l (isActive == False)))
                                ]
                                [ span [ class "panel-icon" ] [ I.icon iconCls ]
                                , viewLabel "" l_
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
            view_ False op (State model)

          else
            text ""
        ]


viewNew : Op -> State -> Html Msg
viewNew op (State model) =
    div []
        [ div [ id id_target_name, class "is-reversed" ]
            [ if model.isOpen then
                view_ True op (State model)

              else
                text ""
            ]
        , div [ class "button is-small is-primary mr-2", onClick (OnOpen op.targets) ]
            [ I.icon1 "icon-1x icon-plus" "", text "Label" ]
        , if List.length op.selectedLabels > 0 then
            viewLabels op.selectedLabels

          else
            text ""
        ]
