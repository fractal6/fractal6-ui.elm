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


module Form.Help exposing (Model, Msg(..), State, init, subscriptions, update, view, viewFix)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Bulk exposing (Ev, UserState(..))
import Bulk.Codecs exposing (ActionType(..), DocType(..), nid2rootid)
import Bulk.Error exposing (viewGqlErrors, viewHttpErrors)
import Bulk.View exposing (viewUser)
import Codecs exposing (QuickDoc)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Dict exposing (Dict)
import Extra exposing (space_, ternary, textH, textT, upH)
import Extra.Events exposing (onClickPD)
import Form exposing (isPostSendable)
import Form.NewTension as NT
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, header, hr, i, input, label, li, nav, option, p, pre, section, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), RestData, isSuccessRest, loadingDiv, withMaybeData)
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Ports
import Query.AddTension exposing (addOneTension)
import RemoteData
import Requests exposing (getQuickDoc)
import Session exposing (Apis, Conf, GlobalCmd(..), isMobile)
import Text as T
import Time



-- ------------------------------
-- M O D E L
-- ------------------------------


type State
    = State Model


type alias Model =
    { isActive : Bool
    , isActive2 : Bool
    , activeTab : HelpTab
    , withChoice : Bool -- fix view
    , doc : RestData QuickDoc
    , type_ : FeedbackType
    , formAsk : NT.Model
    , formFeedback : NT.Model

    -- Common
    , conf : Conf
    , refresh_trial : Int
    , modal_confirm : ModalConfirm Msg
    }


type HelpTab
    = QuickHelp
    | AskQuestion
    | Feedback


fromString : String -> Maybe HelpTab
fromString tab =
    case tab of
        "QuickHelp" ->
            Just QuickHelp

        "AskQuestion" ->
            Just AskQuestion

        "Feedback" ->
            Just Feedback

        _ ->
            Nothing


type FeedbackType
    = BugReport
    | FeatureRequest
    | Praise


labelCodec : FeedbackType -> Label
labelCodec type_ =
    case type_ of
        BugReport ->
            Label "0xc5f2" "bug" (Just "#ff4136") []

        FeatureRequest ->
            Label "0xc5f3" "feature request" (Just "#0074d9") []

        Praise ->
            Label "0xc5f4" "Praise" (Just "#dddddd") []


init : UserState -> Conf -> State
init user conf =
    initModel user conf |> State


initModel : UserState -> Conf -> Model
initModel user conf =
    let
        form =
            NT.initModel user conf
                |> NT.setSourceShort "f6#feedback#help-bot"
                |> NT.setTargetShort "f6#feedback"

        formAsk =
            initFormAsk form

        formFeedback =
            initFormFeedback form
    in
    { isActive = False
    , isActive2 = False
    , activeTab = QuickHelp
    , withChoice = True
    , doc = RemoteData.NotAsked
    , type_ = BugReport
    , formAsk = formAsk
    , formFeedback = formFeedback

    -- Common
    , conf = conf
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }


initFormAsk : NT.Model -> NT.Model
initFormAsk form =
    form
        |> NT.setTensionType TensionType.Help
        |> NT.setEvents [ Ev TensionEvent.Created "" "" ]
        |> NT.setResult NotAsked
        |> NT.resetPost


initFormFeedback : NT.Model -> NT.Model
initFormFeedback form =
    form
        |> NT.setTensionType TensionType.Operational
        |> NT.setEvents [ Ev TensionEvent.Created "" "" ]
        |> NT.setResult NotAsked
        |> NT.resetPost



-- State Controls


changeTab : HelpTab -> Model -> Model
changeTab tab data =
    { data | activeTab = tab }


changeLabel : FeedbackType -> Model -> Model
changeLabel type_ data =
    { data | type_ = type_ }


resetForm : HelpTab -> Model -> Model
resetForm tab data =
    case tab of
        QuickHelp ->
            data

        AskQuestion ->
            { data | formAsk = initFormAsk data.formAsk }

        Feedback ->
            { data | formFeedback = initFormFeedback data.formFeedback }


resetModel : Model -> Model
resetModel model =
    initModel model.formAsk.user model.conf


setDocResult : RestData QuickDoc -> Model -> Model
setDocResult result data =
    { data | doc = result }


setResultAsk : GqlData Tension -> Model -> Model
setResultAsk result data =
    { data | formAsk = NT.setResult result data.formAsk }


setResultFeedback : GqlData Tension -> Model -> Model
setResultFeedback result data =
    { data | formFeedback = NT.setResult result data.formFeedback }



-- Update Form


postAsk : String -> String -> Model -> Model
postAsk field value data =
    { data | formAsk = NT.post field value data.formAsk }


postFeedback : String -> String -> Model -> Model
postFeedback field value data =
    { data | formFeedback = NT.post field value data.formFeedback }


setLabelsFeedback : Model -> Model
setLabelsFeedback data =
    { data | formFeedback = NT.setLabels [ labelCodec data.type_ ] data.formFeedback }



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = SetIsActive2 Bool
    | OnOpen String
    | OnClose ModalData
    | OnCloseSafe String String
    | OnReset
    | OnResetForm HelpTab
    | OnGotQuickDoc (RestData QuickDoc)
    | OnChangeTab HelpTab
    | OnChangePostAsk String String
    | OnChangePostFeedback String String
    | OnChangeLabel FeedbackType
    | OnSubmit Bool (Time.Posix -> Msg)
    | PushTension NT.Model (GqlData Tension -> Msg)
    | OnSubmitAsk Time.Posix
    | OnSubmitFeedback Time.Posix
    | OnAskAck (GqlData Tension)
    | OnAskFeedback (GqlData Tension)
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Common
    | NoMsg
    | LogErr String


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe ( Bool, User )
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


update_ apis message model =
    case message of
        SetIsActive2 v ->
            -- Prevent elm from computing the VDOM
            if v then
                ( { model | isActive = model.isActive2 }, out0 [ Ports.open_modal "helpModal" ] )

            else
                ( { model | isActive2 = model.isActive }, noOut )

        OnOpen tab_s ->
            let
                tab =
                    withDefault QuickHelp (fromString tab_s)
            in
            ( { model | isActive2 = True, doc = RemoteData.Loading, activeTab = tab }
            , out0
                [ sendSleep (SetIsActive2 True) 10
                , if not (isSuccessRest model.doc) && tab == QuickHelp then
                    getQuickDoc apis "en" OnGotQuickDoc

                  else
                    send NoMsg
                ]
            )

        OnClose data ->
            let
                ( newModel, gcmds ) =
                    if data.link == "" then
                        ( model, [] )

                    else
                        ( { model | isActive2 = True }, [ DoNavigate data.link ] )
            in
            ( { newModel | isActive = False }
            , out2
                [ Ports.close_modal
                , ternary data.reset (sendSleep OnReset 333) Cmd.none
                , sendSleep (SetIsActive2 False) 500
                ]
                gcmds
            )

        OnReset ->
            ( resetModel model, noOut )

        OnResetForm tab ->
            ( resetForm tab model, noOut )

        OnCloseSafe link onCloseTxt ->
            let
                doClose =
                    (NT.hasData model.formFeedback && withMaybeData model.formFeedback.result == Nothing)
                        || (NT.hasData model.formAsk && withMaybeData model.formFeedback.result == Nothing)
            in
            if doClose then
                ( model
                , out0 [ send (DoModalConfirmOpen (OnClose { reset = True, link = link }) { message = Nothing, txts = [ ( T.confirmUnsaved, onCloseTxt ) ] }) ]
                )

            else
                ( model, out0 [ send (OnClose { reset = True, link = link }) ] )

        OnGotQuickDoc result ->
            ( setDocResult result model, noOut )

        OnChangeTab tab ->
            ( changeTab tab model |> (\x -> { x | withChoice = False })
            , if not (isSuccessRest model.doc) && tab == QuickHelp then
                out0 [ getQuickDoc apis "en" OnGotQuickDoc ]

              else
                noOut
            )

        OnChangePostAsk field value ->
            ( postAsk field value model, noOut )

        OnChangePostFeedback field value ->
            ( postFeedback field value model, noOut )

        OnChangeLabel type_ ->
            ( changeLabel type_ model, noOut )

        OnSubmit isLoading next ->
            if isLoading then
                ( model, noOut )

            else
                ( model, out0 [ sendNow next ] )

        PushTension form ack ->
            ( model, out0 [ addOneTension apis form.nodeDoc.form ack ] )

        OnSubmitAsk time ->
            let
                newModel =
                    model
                        |> postAsk "createdAt" (fromTime time)
                        |> setResultAsk LoadingSlowly
            in
            ( newModel
            , out0 [ send (PushTension newModel.formAsk OnAskAck) ]
            )

        OnSubmitFeedback time ->
            let
                newModel =
                    model
                        |> postFeedback "createdAt" (fromTime time)
                        |> setLabelsFeedback
                        |> setResultFeedback LoadingSlowly
            in
            ( newModel
            , out0 [ send (PushTension newModel.formFeedback OnAskFeedback) ]
            )

        OnAskAck result ->
            let
                form =
                    model.formAsk
            in
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( setResultAsk NotAsked model
                    , out0 [ Ports.raiseAuthModal form.nodeDoc.form.uctx ]
                    )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, out2 [ sendSleep (PushTension form OnAskAck) 500 ] [ DoUpdateToken ] )

                OkAuth _ ->
                    ( setResultAsk result { model | formAsk = NT.resetPost model.formAsk }, noOut )

                _ ->
                    ( setResultAsk result model, noOut )

        OnAskFeedback result ->
            let
                form =
                    model.formFeedback
            in
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( setResultFeedback NotAsked model
                    , out0 [ Ports.raiseAuthModal form.nodeDoc.form.uctx ]
                    )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, out2 [ sendSleep (PushTension form OnAskFeedback) 500 ] [ DoUpdateToken ] )

                OkAuth _ ->
                    ( setResultFeedback result { model | formAsk = NT.resetPost model.formFeedback }, noOut )

                _ ->
                    ( setResultFeedback result model, noOut )

        -- Confirm Modal
        DoModalConfirmOpen msg mess ->
            ( { model | modal_confirm = ModalConfirm.open msg mess model.modal_confirm }, noOut )

        DoModalConfirmClose _ ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, noOut )

        DoModalConfirmSend ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, out0 [ send model.modal_confirm.msg ] )

        -- Common
        NoMsg ->
            ( model, noOut )

        LogErr err ->
            ( model, out0 [ Ports.logErr err ] )


subscriptions : List (Sub Msg)
subscriptions =
    [ Ports.triggerHelpFromJs OnOpen
    , Ports.mcPD Ports.closeModalTensionFromJs LogErr OnClose
    , Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    ]



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    {}


view : Op -> State -> Html Msg
view op (State model) =
    if model.isActive2 then
        div []
            [ viewModal op (State model)
            , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
            ]

    else
        text ""


viewModal : Op -> State -> Html Msg
viewModal op (State model) =
    div
        [ id "helpModal"
        , class "modal is-light modal-fx-fadeIn"
        , classList [ ( "is-active", model.isActive ) ]
        , attribute "data-modal-close" "closeModalTensionFromJs"
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "helpModal"
            , onClick (OnCloseSafe "" "")
            ]
            []
        , div [ class "modal-content" ] [ viewModalContent True op (State model) ]
        , button [ class "modal-close is-large", onClick (OnCloseSafe "" "") ] []
        ]


viewModalContent : Bool -> Op -> State -> Html Msg
viewModalContent fromModal op (State model) =
    div [ classList [ ( "modal-card", fromModal ) ] ]
        [ div [ classList [ ( "modal-card-head", fromModal ) ] ]
            [ div [ class "tabs is-centered is-medium is-fullwidth" ]
                [ ul []
                    [ li
                        [ classList [ ( "is-active", model.activeTab == QuickHelp ) ]
                        , onClick (OnChangeTab QuickHelp)
                        ]
                        [ span [] [ text T.quickHelp ] ]
                    , li
                        [ classList [ ( "is-active", model.activeTab == AskQuestion ) ]
                        , onClick (OnChangeTab AskQuestion)
                        ]
                        [ span [] [ text T.askQuestion ] ]
                    , li
                        [ classList [ ( "is-active", model.activeTab == Feedback ) ]
                        , onClick (OnChangeTab Feedback)
                        ]
                        [ span [] [ text T.giveFeedback ] ]
                    ]
                ]
            ]
        , div [ classList [ ( "modal-card-body", fromModal ) ] ]
            [ case model.activeTab of
                QuickHelp ->
                    viewQuickHelp fromModal op (State model)

                AskQuestion ->
                    viewAskQuestion fromModal op (State model)

                Feedback ->
                    viewFeedback fromModal op (State model)
            ]
        ]


viewQuickHelp : Bool -> Op -> State -> Html Msg
viewQuickHelp fromModal op (State model) =
    case model.doc of
        RemoteData.Success docs ->
            docs
                |> List.concatMap
                    (\doc ->
                        [ header [ class "acc" ] [ label [ class "acc-title" ] [ textT doc.name ] ] ]
                            ++ (doc.tasks
                                    |> List.indexedMap
                                        (\i task ->
                                            let
                                                did =
                                                    "acc" ++ task.header ++ String.fromInt i
                                            in
                                            [ input [ id did, name "accordion", type_ "radio" ] []
                                            , section [ class "acc" ]
                                                [ label [ class "acc-title", for did ] [ textH task.header ]
                                                , label [ class "acc-close", for "acc-close" ] []
                                                , div [ class "acc-content" ] [ task.content |> upH |> renderMarkdown (ternary fromModal "is-light" "box") ]
                                                ]
                                            ]
                                        )
                                    |> List.concat
                               )
                    )
                |> List.append [ input [ id "acc-close", name "accordion", type_ "radio" ] [] ]
                |> nav [ class "accordion arrows-left", classList [ ( "quickHelp", fromModal ) ] ]

        RemoteData.Failure err ->
            viewHttpErrors err

        RemoteData.Loading ->
            loadingDiv

        RemoteData.NotAsked ->
            text ""


viewAskQuestion : Bool -> Op -> State -> Html Msg
viewAskQuestion fromModal op (State model) =
    let
        form =
            model.formAsk.nodeDoc.form

        title =
            Dict.get "title" form.post |> withDefault ""

        message =
            Dict.get "message" form.post |> withDefault ""

        isLoading =
            model.formAsk.result == LoadingSlowly

        isSendable =
            isPostSendable [ "title", "message" ] form.post
    in
    case model.formAsk.result of
        Success res ->
            let
                link =
                    Route.Tension_Dynamic_Dynamic { param1 = nid2rootid form.target.nameid, param2 = res.id } |> toHref
            in
            div []
                [ div [ class "box is-light" ]
                    [ A.icon1 "icon-check icon-2x has-text-success" " "
                    , text T.messageSent
                    , text ". "
                    , a
                        [ href link
                        , onClickPD (OnClose { reset = True, link = link })
                        , target "_blank"
                        ]
                        [ text T.checkItOut_masc ]
                    ]
                , a [ onClickPD (OnResetForm AskQuestion), target "_blank" ] [ text T.askAnotherQuestion ]
                ]

        other ->
            let
                line_len =
                    List.length <| String.lines message

                ( max_len, min_len ) =
                    if isMobile model.conf.screen then
                        ( 5, 2 )

                    else
                        ( 10, 5 )
            in
            div [ class "section pt-0" ]
                [ p [ class "field" ]
                    [ text T.haveYouCheckQuickHelp
                    , text space_
                    , span [ class "button-light has-text-info has-text-weight-semibold", onClick (OnChangeTab QuickHelp) ] [ text T.quickHelp, text "?" ]
                    ]
                , div [ class "field is-horizontal pt-2" ]
                    [ div [ class "field-label is-normal" ] [ label [ class "label" ] [ text T.subject ] ]
                    , div [ class "field-body" ]
                        [ div [ class "field" ]
                            [ div [ class "control is-expanded" ]
                                [ input
                                    [ class "input autofocus followFocus"
                                    , attribute "data-nextfocus" "textAreaModal"
                                    , type_ "text"
                                    , placeholder T.questionTitlePH
                                    , required True
                                    , value title
                                    , onInput (OnChangePostAsk "title")
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                , div [ class "field is-horizontal" ]
                    [ div [ class "field-label is-normal" ] [ label [ class "label" ] [ text T.question ] ]
                    , div [ class "field-body" ]
                        [ div [ class "field" ]
                            [ div [ class "control is-expanded" ]
                                [ textarea
                                    [ id "textAreaModal"
                                    , class "textarea"
                                    , rows (min max_len (max line_len min_len))
                                    , placeholder T.questionTextPH
                                    , required True
                                    , value message
                                    , onInput (OnChangePostAsk "message")
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                , case other of
                    Failure err ->
                        viewGqlErrors err

                    _ ->
                        text ""
                , div [ class "field is-grouped is-grouped-right" ]
                    [ div [ class "control" ]
                        [ button
                            [ class "button is-success"
                            , classList [ ( "is-loading", isLoading ) ]
                            , disabled (not isSendable)
                            , onClick (OnSubmit isLoading <| OnSubmitAsk)
                            ]
                            [ text T.send ]
                        ]
                    ]
                ]


viewFeedback : Bool -> Op -> State -> Html Msg
viewFeedback fromModal op (State model) =
    let
        form =
            model.formFeedback.nodeDoc.form

        title =
            Dict.get "title" form.post |> withDefault ""

        message =
            Dict.get "message" form.post |> withDefault ""

        isLoading =
            model.formFeedback.result == LoadingSlowly

        isSendable =
            isPostSendable [ "title", "message" ] form.post
    in
    case model.formFeedback.result of
        Success res ->
            let
                link =
                    Route.Tension_Dynamic_Dynamic { param1 = nid2rootid form.target.nameid, param2 = res.id } |> toHref
            in
            div []
                [ div [ class "box is-light" ]
                    [ A.icon1 "icon-check icon-2x has-text-success" " "
                    , text T.messageSent
                    , text ". "
                    , a
                        [ href link
                        , onClickPD (OnClose { reset = True, link = link })
                        , target "_blank"
                        ]
                        [ text T.checkItOut_masc ]
                    ]
                , span [ class "is-italic" ] [ text T.thankyouFeedback ]
                , br [] []
                , a [ onClickPD (OnResetForm Feedback), target "_blank" ] [ text T.giveAnotherFeedback ]
                ]

        other ->
            let
                line_len =
                    List.length <| String.lines message

                ( max_len, min_len ) =
                    if isMobile model.conf.screen then
                        ( 5, 2 )

                    else
                        ( 10, 5 )
            in
            div [ class "section pt-0" ]
                [ div [ class "field is-horizontal" ]
                    [ div [ class "field-label is-norma" ] [ label [ class "label" ] [ text "Type" ] ]
                    , div [ class "field-body" ]
                        [ div [ class "field is-narrow" ]
                            [ div [ class "control" ]
                                [ label [ class "radio mr-3" ]
                                    [ input
                                        [ type_ "radio"
                                        , name "type"
                                        , onClick (OnChangeLabel BugReport)
                                        , checked (model.type_ == BugReport)
                                        ]
                                        []
                                    , text " Bug report"
                                    ]
                                , label [ class "radio mr-3" ]
                                    [ input
                                        [ type_ "radio"
                                        , name "type"
                                        , onClick (OnChangeLabel FeatureRequest)
                                        , checked (model.type_ == FeatureRequest)
                                        ]
                                        []
                                    , text " Feature request"
                                    ]
                                , label [ class "radio" ]
                                    [ input
                                        [ type_ "radio"
                                        , name "type"
                                        , onClick (OnChangeLabel Praise)
                                        , checked (model.type_ == Praise)
                                        ]
                                        []
                                    , text " Praise"
                                    ]
                                ]
                            ]
                        ]
                    ]
                , div [ class "field is-horizontal pt-2" ]
                    [ div [ class "field-label is-normal" ] [ label [ class "label" ] [ text T.subject ] ]
                    , div [ class "field-body" ]
                        [ div [ class "field" ]
                            [ div [ class "control is-expanded" ]
                                [ input
                                    [ class "input autofocus followFocus"
                                    , attribute "data-nextfocus" "textAreaModal"
                                    , type_ "text"
                                    , placeholder T.feedbackTitlePH
                                    , required True
                                    , value title
                                    , onInput (OnChangePostFeedback "title")
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                , div [ class "field is-horizontal" ]
                    [ div [ class "field-label is-normal" ] [ label [ class "label" ] [ text T.feedback ] ]
                    , div [ class "field-body" ]
                        [ div [ class "field" ]
                            [ div [ class "control is-expanded" ]
                                [ textarea
                                    [ id "textAreaModal"
                                    , class "textarea"
                                    , rows (min max_len (max line_len min_len))
                                    , placeholder T.feedbackTextPH
                                    , required True
                                    , value message
                                    , onInput (OnChangePostFeedback "message")
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                , case other of
                    Failure err ->
                        viewGqlErrors err

                    _ ->
                        text ""
                , div [ class "field is-grouped is-grouped-right" ]
                    [ div [ class "control" ]
                        [ button
                            [ class "button is-success"
                            , classList [ ( "is-loading", isLoading ) ]
                            , disabled (not isSendable)
                            , onClick (OnSubmit isLoading <| OnSubmitFeedback)
                            ]
                            [ text T.send ]
                        ]
                    ]
                ]


viewFix : Op -> State -> Html Msg
viewFix op (State model) =
    div [ id "helpModal", class "columns is-centered top-section" ]
        [ div [ class "column is-5-fullhd is-6-desktop" ]
            [ if model.withChoice then
                [ ( T.quickHelp, QuickHelp ), ( T.askQuestion, AskQuestion ), ( T.giveFeedback, Feedback ) ]
                    |> List.map
                        (\x ->
                            div
                                [ class "card has-border column is-paddingless m-3 is-h"
                                , onClick (OnChangeTab (Tuple.second x))
                                ]
                                [ div [ class "card-content p-4" ]
                                    [ h2 [ class "is-strong is-size-5" ]
                                        [ text (Tuple.first x) ]

                                    --, div [ class "content is-small" ]
                                    --    [ text description ]
                                    ]
                                ]
                        )
                    |> div [ class "columns is-multiline section" ]

              else
                viewModalContent False op (State model)
            ]
        ]
