module Form.Help exposing (Msg, State, init, subscriptions, update, view)

import Auth exposing (AuthState(..), doRefreshToken)
import Codecs exposing (QuickDoc)
import Components.Loading as Loading exposing (GqlData, ModalData, RequestResult(..), WebData, loadingDiv, loadingSpin, viewGqlErrors, viewHttpErrors, withMapData, withMaybeData)
import Components.Markdown exposing (renderMarkdown)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm)
import Dict exposing (Dict)
import Extra exposing (ternary)
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
import Icon as I
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (Apis, GlobalCmd(..), UserState(..))
import ModelCommon.Codecs exposing (ActionType(..), DocType(..), NodeFocus, TensionCharac, nearestCircleid, nid2rootid, typeFromNameid)
import ModelCommon.Requests exposing (getQuickDoc)
import ModelCommon.View exposing (viewUser)
import ModelSchema exposing (..)
import Ports
import Query.AddTension exposing (addOneTension)
import RemoteData
import String.Format as Format
import Text as T exposing (textH, textT, upH)
import Time


type State
    = State Model


type alias Model =
    { isModalActive : Bool
    , activeTab : HelpTab
    , doc : WebData QuickDoc
    , type_ : FeedbackType
    , formAsk : NT.Model
    , formFeedback : NT.Model

    -- Common
    , refresh_trial : Int
    , modal_confirm : ModalConfirm Msg
    }


type HelpTab
    = QuickHelp
    | AskQuestion
    | Feedback


type FeedbackType
    = BugReport
    | FeatureRequest
    | Praise


labelCodec : FeedbackType -> Label
labelCodec type_ =
    case type_ of
        BugReport ->
            Label "0xc5f2" "bug" Nothing

        FeatureRequest ->
            Label "0xc5f3" "feature request" Nothing

        Praise ->
            Label "0xc5f4" "Praise" Nothing


init : UserState -> State
init user =
    initModel user |> State


initModel : UserState -> Model
initModel user =
    let
        uctx =
            case user of
                LoggedIn u ->
                    u

                LoggedOut ->
                    UserCtx "" Nothing (UserRights False False) []

        form =
            NT.initModel user
                |> NT.setTargetShort { rootnameid = "f6", nameid = "f6#feedback", type_ = NodeType.Circle } Nothing
                |> NT.setSource
                    { rootnameid = "f6"
                    , nameid = "f6#feedback#help-bot"
                    , name = "Help Bot"
                    , role_type = RoleType.Bot
                    }

        formAsk =
            initFormAsk form

        formFeedback =
            initFormFeedback form
    in
    { isModalActive = False
    , activeTab = QuickHelp
    , doc = RemoteData.NotAsked
    , type_ = BugReport
    , formAsk = formAsk
    , formFeedback = formFeedback

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }


initFormAsk : NT.Model -> NT.Model
initFormAsk form =
    form
        |> NT.setTensionType TensionType.Help
        |> NT.setEvents [ TensionEvent.Created ]
        |> NT.setResult NotAsked
        |> NT.resetPost


initFormFeedback : NT.Model -> NT.Model
initFormFeedback form =
    form
        |> NT.setTensionType TensionType.Operational
        |> NT.setEvents [ TensionEvent.Created ]
        |> NT.setResult NotAsked
        |> NT.resetPost



-- State Controls


open : Model -> Model
open data =
    { data | isModalActive = True, doc = RemoteData.Loading }


close : Model -> Model
close data =
    { data | isModalActive = False }


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
    initModel model.formAsk.user


setDocResult : WebData QuickDoc -> Model -> Model
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
    = OnOpen
    | OnClose ModalData
    | OnCloseSafe String String
    | OnReset HelpTab
    | OnResetModel
    | OnGotQuickDoc (WebData QuickDoc)
    | OnChangeTab HelpTab
    | OnChangePostAsk String String
    | OnChangePostFeedback String String
    | OnChangeLabel FeedbackType
    | OnSubmit (Time.Posix -> Msg)
    | PushTension NT.Model (GqlData Tension -> Msg)
    | OnSubmitAsk Time.Posix
    | OnSubmitFeedback Time.Posix
    | OnAskAck (GqlData Tension)
    | OnAskFeedback (GqlData Tension)
      -- Confirm Modal
    | DoModalConfirmOpen Msg (List ( String, String ))
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


out1 : List (Cmd Msg) -> Out
out1 cmds =
    Out cmds [] Nothing


out2 : List GlobalCmd -> Out
out2 cmds =
    Out [] cmds Nothing


update : Apis -> Msg -> State -> ( State, Out )
update apis message (State model) =
    update_ apis message model
        |> Tuple.mapFirst State


update_ apis message model =
    case message of
        OnOpen ->
            ( open model
            , out1 [ getQuickDoc apis.data "en" OnGotQuickDoc, Ports.open_modal "helpModal" ]
            )

        OnClose data ->
            let
                cmds =
                    ternary data.reset [ sendSleep OnResetModel 333 ] []

                gcmds =
                    ternary (data.link /= "") [ DoNavigate data.link ] []
            in
            ( close model, Out ([ Ports.close_modal ] ++ cmds) gcmds Nothing )

        OnReset tab ->
            ( resetForm tab model, noOut )

        OnResetModel ->
            ( resetModel model, noOut )

        OnCloseSafe link onCloseTxt ->
            let
                doClose =
                    NT.hasData model.formFeedback
                        && withMaybeData model.formFeedback.result
                        == Nothing
                        || NT.hasData model.formAsk
                        && withMaybeData model.formFeedback.result
                        == Nothing
            in
            if doClose then
                ( model
                , out1 [ send (DoModalConfirmOpen (OnClose { reset = True, link = link }) [ ( upH T.confirmUnsaved, onCloseTxt ) ]) ]
                )

            else
                ( model, out1 [ send (OnClose { reset = True, link = link }) ] )

        OnGotQuickDoc result ->
            ( setDocResult result model, noOut )

        OnChangeTab tab ->
            ( changeTab tab model, noOut )

        OnChangePostAsk field value ->
            ( postAsk field value model, noOut )

        OnChangePostFeedback field value ->
            ( postFeedback field value model, noOut )

        OnChangeLabel type_ ->
            ( changeLabel type_ model, noOut )

        OnSubmit next ->
            ( model, out1 [ sendNow next ] )

        PushTension form ack ->
            ( model, out1 [ addOneTension apis.gql form.form ack ] )

        OnSubmitAsk time ->
            let
                newModel =
                    model
                        |> postAsk "createdAt" (fromTime time)
                        |> setResultAsk LoadingSlowly
            in
            ( newModel
            , out1 [ send (PushTension newModel.formAsk OnAskAck) ]
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
            , out1 [ send (PushTension newModel.formFeedback OnAskFeedback) ]
            )

        OnAskAck result ->
            let
                form =
                    model.formAsk
            in
            case doRefreshToken result model.refresh_trial of
                Authenticate ->
                    ( setResultAsk NotAsked model
                    , out2 [ DoAuth form.form.uctx ]
                    )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, Out [ sendSleep (PushTension form OnAskAck) 500 ] [ DoUpdateToken ] Nothing )

                OkAuth tension ->
                    ( setResultAsk result { model | formAsk = NT.resetPost model.formAsk }, noOut )

                NoAuth ->
                    ( setResultAsk result model, noOut )

        OnAskFeedback result ->
            let
                form =
                    model.formFeedback
            in
            case doRefreshToken result model.refresh_trial of
                Authenticate ->
                    ( setResultFeedback NotAsked model
                    , out2 [ DoAuth form.form.uctx ]
                    )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, Out [ sendSleep (PushTension form OnAskFeedback) 500 ] [ DoUpdateToken ] Nothing )

                OkAuth tension ->
                    ( setResultFeedback result { model | formAsk = NT.resetPost model.formFeedback }, noOut )

                NoAuth ->
                    ( setResultFeedback result model, noOut )

        -- Confirm Modal
        DoModalConfirmOpen msg txts ->
            ( { model | modal_confirm = ModalConfirm.open msg txts model.modal_confirm }, noOut )

        DoModalConfirmClose _ ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, noOut )

        DoModalConfirmSend ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, out1 [ send model.modal_confirm.msg ] )

        -- Common
        NoMsg ->
            ( model, noOut )

        LogErr err ->
            ( model, out1 [ Ports.logErr err ] )


subscriptions =
    [ Ports.triggerHelpFromJs (always OnOpen)
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
    div []
        [ viewModal op (State model)
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        ]


viewModal : Op -> State -> Html Msg
viewModal op (State model) =
    div
        [ id "helpModal"
        , class "modal modal-fx-fadeIn"
        , classList [ ( "is-active", model.isModalActive ) ]
        , attribute "data-modal-close" "closeModalTensionFromJs"
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "helpModal"
            , onClick (OnCloseSafe "" "")
            ]
            []
        , div [ class "modal-content" ] [ viewModalContent op (State model) ]
        , button [ class "modal-close is-large", onClick (OnCloseSafe "" "") ] []
        ]


viewModalContent : Op -> State -> Html Msg
viewModalContent op (State model) =
    div [ class "modal-card" ]
        [ div [ class "modal-card-head" ]
            [ div [ class "tabs is-centered is-medium is-fullwidth" ]
                [ ul []
                    [ li
                        [ classList [ ( "is-active", model.activeTab == QuickHelp ) ]
                        , onClick (OnChangeTab QuickHelp)
                        ]
                        [ span [] [ text "Quick help" ] ]
                    , li
                        [ classList [ ( "is-active", model.activeTab == AskQuestion ) ]
                        , onClick (OnChangeTab AskQuestion)
                        ]
                        [ span [] [ text "Ask a question" ] ]
                    , li
                        [ classList [ ( "is-active", model.activeTab == Feedback ) ]
                        , onClick (OnChangeTab Feedback)
                        ]
                        [ span [] [ text "Give feedback" ] ]
                    ]
                ]
            ]
        , div [ class "modal-card-body" ]
            [ case model.activeTab of
                QuickHelp ->
                    viewQuickHelp op (State model)

                AskQuestion ->
                    viewAskQuestion op (State model)

                Feedback ->
                    viewFeedback op (State model)
            ]
        ]


viewQuickHelp : Op -> State -> Html Msg
viewQuickHelp op (State model) =
    case model.doc of
        RemoteData.Success docs ->
            docs
                |> List.map
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
                                                , div [ class "acc-content" ] [ task.content |> upH |> renderMarkdown "is-dark" ]
                                                ]
                                            ]
                                        )
                                    |> List.concat
                               )
                    )
                |> List.concat
                |> List.append [ input [ id "acc-close", name "accordion", type_ "radio" ] [] ]
                |> nav [ class "accordion arrows" ]

        RemoteData.Failure err ->
            viewHttpErrors err

        RemoteData.Loading ->
            loadingDiv

        RemoteData.NotAsked ->
            text ""


viewAskQuestion : Op -> State -> Html Msg
viewAskQuestion op (State model) =
    let
        form =
            model.formAsk.form

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
                    Route.Tension_Dynamic_Dynamic { param1 = form.target.rootnameid, param2 = res.id } |> toHref
            in
            div []
                [ div [ class "box is-light" ]
                    [ I.icon1 "icon-check icon-2x has-text-success" " "
                    , textH T.messageSent
                    , text ". "
                    , a
                        [ href link
                        , onClickPD (OnClose { reset = True, link = link })
                        , target "_blank"
                        ]
                        [ textH T.checkItOut ]
                    ]
                , a [ onClickPD (OnReset AskQuestion), target "_blank" ] [ textH T.askAnotherQuestion ]
                ]

        other ->
            div [ class "section pt-0" ]
                [ p [ class "field" ]
                    [ text "Have you checked if your question is answered in the "
                    , span [ class "button-light has-text-info has-text-weight-semibold", onClick (OnChangeTab QuickHelp) ] [ text "Quick help?" ]
                    ]
                , div [ class "field is-horizontal pt-2" ]
                    [ div [ class "field-label is-normal" ] [ label [ class "label" ] [ text "Subject" ] ]
                    , div [ class "field-body" ]
                        [ div [ class "field" ]
                            [ div [ class "control is-expanded" ]
                                [ input
                                    [ class "input autofocus followFocus"
                                    , attribute "data-nextfocus" "textAreaModal"
                                    , type_ "text"
                                    , placeholder "Subject of your question"
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
                    [ div [ class "field-label is-normal" ] [ label [ class "label" ] [ text "Question" ] ]
                    , div [ class "field-body" ]
                        [ div [ class "field" ]
                            [ div [ class "control is-expanded" ]
                                [ textarea
                                    [ id "textAreaModal"
                                    , class "textarea"
                                    , rows 5
                                    , placeholder "Write your question here..."
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
                            , onClick (OnSubmit <| OnSubmitAsk)
                            ]
                            [ text "Send question" ]
                        ]
                    ]
                ]


viewFeedback : Op -> State -> Html Msg
viewFeedback op (State model) =
    let
        form =
            model.formFeedback.form

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
                    Route.Tension_Dynamic_Dynamic { param1 = form.target.rootnameid, param2 = res.id } |> toHref
            in
            div []
                [ div [ class "box is-light" ]
                    [ I.icon1 "icon-check icon-2x has-text-success" " "
                    , textH T.messageSent
                    , text ". "
                    , a
                        [ href link
                        , onClickPD (OnClose { reset = True, link = link })
                        , target "_blank"
                        ]
                        [ textH T.checkItOut ]
                    ]
                , a [ onClickPD (OnReset Feedback), target "_blank" ] [ textH T.giveAnotherFeedback ]
                ]

        other ->
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
                    [ div [ class "field-label is-normal" ] [ label [ class "label" ] [ text "Subject" ] ]
                    , div [ class "field-body" ]
                        [ div [ class "field" ]
                            [ div [ class "control is-expanded" ]
                                [ input
                                    [ class "input autofocus followFocus"
                                    , attribute "data-nextfocus" "textAreaModal"
                                    , type_ "text"
                                    , placeholder "Subject of your feedback"
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
                    [ div [ class "field-label is-normal" ] [ label [ class "label" ] [ text "Feedback" ] ]
                    , div [ class "field-body" ]
                        [ div [ class "field" ]
                            [ div [ class "control is-expanded" ]
                                [ textarea
                                    [ id "textAreaModal"
                                    , class "textarea"
                                    , rows 5
                                    , placeholder "Write your feedback here..."
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
                            , onClick (OnSubmit <| OnSubmitFeedback)
                            ]
                            [ text "Send feedback" ]
                        ]
                    ]
                ]
