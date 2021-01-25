module Components.Help exposing (..)

import Codecs exposing (QuickDoc)
import Components.I as I
import Components.Loading as Loading exposing (GqlData, RequestResult(..), WebData, loadingDiv, loadingSpin, viewGqlErrors, viewHttpErrors, withMapData, withMaybeData)
import Components.Markdown exposing (renderMarkdown)
import Dict exposing (Dict)
import Extra exposing (ternary, up0, up1)
import Extra.Events exposing (onClickPD)
import Form exposing (isPostSendable)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, header, hr, i, input, label, li, nav, option, p, pre, section, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (TensionForm, UserState(..), initTensionForm)
import ModelCommon.Codecs exposing (ActionType(..), DocType(..), NodeFocus, TensionCharac, nearestCircleid, nid2rootid, typeFromNameid)
import ModelCommon.View exposing (viewUser)
import ModelSchema exposing (..)
import RemoteData
import String.Format as Format
import Text as T
import Time


type alias Help =
    { isModalActive : Bool
    , activeTab : HelpTab
    , doc : WebData QuickDoc
    , type_ : FeedbackType
    , formAsk : TensionForm
    , formFeedback : TensionForm
    , resultAsk : GqlData Tension
    , resultFeedback : GqlData Tension
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



-- State Controls


create : UserState -> Help
create user =
    let
        uctx =
            case user of
                LoggedIn u ->
                    u

                LoggedOut ->
                    UserCtx "" Nothing (UserRights False False) []

        f =
            initTensionForm { rootnameid = "f6", nameid = "f6#feedback", type_ = NodeType.Circle }

        form =
            { f
                | uctx = uctx
                , source =
                    { rootnameid = "f6"
                    , nameid = "f6#feedback#help-bot"
                    , name = "Help Bot"
                    , role_type = RoleType.Bot
                    }
            }

        formAsk =
            { form
                | tension_type = TensionType.Help
                , events_type = Just [ TensionEvent.Created ]
            }

        formFeedback =
            { form
                | tension_type = TensionType.Operational
                , events_type = Just [ TensionEvent.Created ]
            }
    in
    { isModalActive = False
    , activeTab = QuickHelp
    , doc = RemoteData.NotAsked
    , type_ = BugReport
    , formAsk = formAsk
    , formFeedback = formFeedback
    , resultAsk = NotAsked
    , resultFeedback = NotAsked
    }


open : Help -> Help
open data =
    { data | isModalActive = True, doc = RemoteData.Loading }


changeTab : HelpTab -> Help -> Help
changeTab tab data =
    { data | activeTab = tab }


changeLabel : FeedbackType -> Help -> Help
changeLabel type_ data =
    { data | type_ = type_ }


close : Help -> Help
close data =
    create (LoggedIn data.formAsk.uctx)


setDocResult : WebData QuickDoc -> Help -> Help
setDocResult result data =
    { data | doc = result }


setResultAsk : GqlData Tension -> Help -> Help
setResultAsk result data =
    { data | resultAsk = result }


setResultFeedback : GqlData Tension -> Help -> Help
setResultFeedback result data =
    { data | resultFeedback = result }



-- Update Form


postAsk : String -> String -> Help -> Help
postAsk field value data =
    let
        f =
            data.formAsk

        newForm =
            { f | post = Dict.insert field value f.post }
    in
    { data | formAsk = newForm }


postFeedback : String -> String -> Help -> Help
postFeedback field value data =
    let
        f =
            data.formFeedback

        newForm =
            { f | post = Dict.insert field value f.post }
    in
    { data | formFeedback = newForm }


setLabelsFeedback : Help -> Help
setLabelsFeedback data =
    let
        form =
            data.formFeedback
    in
    { data | formFeedback = { form | labels = [ labelCodec data.type_ ] } }


type alias Op msg =
    { data : Help
    , onSubmit : (Time.Posix -> msg) -> msg
    , onCloseModal : String -> msg
    , onNavigate : String -> msg
    , onChangeTab : HelpTab -> msg
    , onChangePostAsk : String -> String -> msg
    , onChangePostFeedback : String -> String -> msg
    , onChangeLabel : FeedbackType -> msg
    , onSubmitAsk : Time.Posix -> msg
    , onSubmitFeedback : Time.Posix -> msg
    }


view : Op msg -> Html msg
view op =
    if op.data.isModalActive then
        viewModal op

    else
        text ""


viewModal : Op msg -> Html msg
viewModal op =
    div
        [ id "helpModal"
        , class "modal modal-fx-fadeIn elmModal"
        , classList [ ( "is-active", op.data.isModalActive ) ]
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "helpModal"
            , onClick (op.onCloseModal "")
            ]
            []
        , div [ class "modal-content" ] [ viewModalContent op ]
        , button [ class "modal-close is-large", onClick (op.onCloseModal "") ] []
        ]


viewModalContent : Op msg -> Html msg
viewModalContent op =
    div [ class "modal-card" ]
        [ div [ class "modal-card-head" ]
            [ div [ class "tabs is-centered is-medium is-fullwidth" ]
                [ ul []
                    [ li
                        [ classList [ ( "is-active", op.data.activeTab == QuickHelp ) ]
                        , onClick (op.onChangeTab QuickHelp)
                        ]
                        [ span [] [ text "Quick help" ] ]
                    , li
                        [ classList [ ( "is-active", op.data.activeTab == AskQuestion ) ]
                        , onClick (op.onChangeTab AskQuestion)
                        ]
                        [ span [] [ text "Ask a question" ] ]
                    , li
                        [ classList [ ( "is-active", op.data.activeTab == Feedback ) ]
                        , onClick (op.onChangeTab Feedback)
                        ]
                        [ span [] [ text "Give feedback" ] ]
                    ]
                ]
            ]
        , div [ class "modal-card-body" ]
            [ case op.data.activeTab of
                QuickHelp ->
                    viewQuickHelp op

                AskQuestion ->
                    viewAskQuestion op

                Feedback ->
                    viewFeedback op
            ]
        ]


viewQuickHelp : Op msg -> Html msg
viewQuickHelp op =
    case op.data.doc of
        RemoteData.Success docs ->
            docs
                |> List.map
                    (\doc ->
                        [ header [ class "acc" ] [ label [ class "acc-title" ] [ text (up1 doc.name) ] ] ]
                            ++ (doc.tasks
                                    |> List.indexedMap
                                        (\i task ->
                                            let
                                                did =
                                                    "acc" ++ task.header ++ String.fromInt i
                                            in
                                            [ input [ id did, name "accordion", type_ "radio" ] []
                                            , section [ class "acc" ]
                                                [ label [ class "acc-title", for did ] [ text (up0 task.header) ]
                                                , label [ class "acc-close", for "acc-close" ] []
                                                , div [ class "acc-content" ] [ up0 task.content |> up0 |> renderMarkdown "is-dark" ]
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


viewAskQuestion : Op msg -> Html msg
viewAskQuestion op =
    let
        form =
            op.data.formAsk

        title =
            Dict.get "title" form.post |> withDefault ""

        message =
            Dict.get "message" form.post |> withDefault ""

        isLoading =
            op.data.resultAsk == LoadingSlowly

        isSendable =
            isPostSendable [ "title", "message" ] form.post
    in
    case op.data.resultAsk of
        Success res ->
            let
                link =
                    Route.Tension_Dynamic_Dynamic { param1 = form.target.rootnameid, param2 = res.id } |> toHref
            in
            div [ class "box is-light" ]
                [ I.icon1 "icon-check icon-2x has-text-success" " "
                , text (T.messageSent ++ ". ")
                , a
                    [ href link
                    , onClickPD (op.onCloseModal link)
                    , target "_blank"
                    ]
                    [ text T.checkItOut ]
                ]

        other ->
            div [ class "section pt-0" ]
                [ p [ class "field" ]
                    [ text "Have you checked if your question is answered in the "
                    , span [ class "button-light has-text-info has-text-weight-semibold", onClick (op.onChangeTab QuickHelp) ] [ text "Quick help?" ]
                    ]
                , div [ class "field is-horizontal pt-2" ]
                    [ div [ class "field-label is-normal" ] [ label [ class "label" ] [ text "Subject" ] ]
                    , div [ class "field-body" ]
                        [ div [ class "field" ]
                            [ div [ class "control is-expanded" ]
                                [ input
                                    [ class "input autofocus"
                                    , type_ "text"
                                    , placeholder "Subject of your question."
                                    , required True
                                    , value title
                                    , onInput (op.onChangePostAsk "title")
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
                                    [ class "textarea"
                                    , rows 5
                                    , placeholder "Write your question here..."
                                    , required True
                                    , value message
                                    , onInput (op.onChangePostAsk "message")
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
                            , onClick (op.onSubmit <| op.onSubmitAsk)
                            ]
                            [ text "Send question" ]
                        ]
                    ]
                ]


viewFeedback : Op msg -> Html msg
viewFeedback op =
    let
        form =
            op.data.formFeedback

        title =
            Dict.get "title" form.post |> withDefault ""

        message =
            Dict.get "message" form.post |> withDefault ""

        isLoading =
            op.data.resultFeedback == LoadingSlowly

        isSendable =
            isPostSendable [ "title", "message" ] form.post
    in
    case op.data.resultFeedback of
        Success res ->
            let
                link =
                    Route.Tension_Dynamic_Dynamic { param1 = form.target.rootnameid, param2 = res.id } |> toHref
            in
            div [ class "box is-light" ]
                [ I.icon1 "icon-check icon-2x has-text-success" " "
                , text (T.messageSent ++ ". ")
                , a
                    [ href link
                    , onClickPD (op.onCloseModal link)
                    , target "_blank"
                    ]
                    [ text T.checkItOut ]
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
                                        , onClick (op.onChangeLabel BugReport)
                                        , checked (op.data.type_ == BugReport)
                                        ]
                                        []
                                    , text " Bug report"
                                    ]
                                , label [ class "radio mr-3" ]
                                    [ input
                                        [ type_ "radio"
                                        , name "type"
                                        , onClick (op.onChangeLabel FeatureRequest)
                                        , checked (op.data.type_ == FeatureRequest)
                                        ]
                                        []
                                    , text " Feature request"
                                    ]
                                , label [ class "radio" ]
                                    [ input
                                        [ type_ "radio"
                                        , name "type"
                                        , onClick (op.onChangeLabel Praise)
                                        , checked (op.data.type_ == Praise)
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
                                    [ class "input autofocus"
                                    , type_ "text"
                                    , placeholder "Subject of your feedback."
                                    , required True
                                    , value title
                                    , onInput (op.onChangePostFeedback "title")
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
                                    [ class "textarea"
                                    , rows 5
                                    , placeholder "Write your feedback here..."
                                    , required True
                                    , value message
                                    , onInput (op.onChangePostFeedback "message")
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
                            , onClick (op.onSubmit <| op.onSubmitFeedback)
                            ]
                            [ text "Send feedback" ]
                        ]
                    ]
                ]
