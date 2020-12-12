module Components.Help exposing (..)

import Codecs exposing (QuickDoc)
import Components.Fa as Fa
import Components.Loading as Loading exposing (GqlData, RequestResult(..), WebData, loadingDiv, loadingSpin, viewGqlErrors, viewHttpErrors, withMapData, withMaybeData)
import Components.Markdown exposing (renderMarkdown)
import Dict exposing (Dict)
import Extra exposing (ternary, up0, up1)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, header, hr, i, input, label, li, nav, option, p, section, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (ActionForm, UserState(..), initActionForm)
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
    }


type HelpTab
    = QuickHelp
    | AskQuestion
    | Feedback



-- State Controls


create : Help
create =
    { isModalActive = False
    , activeTab = QuickHelp
    , doc = RemoteData.NotAsked
    }


open : Help -> Help
open data =
    { data | isModalActive = True, doc = RemoteData.Loading }


changeTab : HelpTab -> Help -> Help
changeTab tab data =
    { data | activeTab = tab }


close : Help -> Help
close data =
    { data | isModalActive = False, activeTab = QuickHelp, doc = RemoteData.NotAsked }


setDocResult : WebData QuickDoc -> Help -> Help
setDocResult result data =
    { data | doc = result }


type alias Op msg =
    { data : Help
    , onSubmit : (Time.Posix -> msg) -> msg
    , onCloseModal : msg
    , onNavigate : String -> msg
    , onChangeTab : HelpTab -> msg
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
            , onClick op.onCloseModal
            ]
            []
        , div [ class "modal-content" ] [ viewModalContent op ]
        , button [ class "modal-close is-large", onClick op.onCloseModal ] []
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
                    text "Work in progress..."

                Feedback ->
                    text "Work in progress..."
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
