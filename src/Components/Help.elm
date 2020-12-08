module Components.Help exposing (..)

import Components.Fa as Fa
import Components.Loading as Loading exposing (GqlData, RequestResult(..), loadingSpin, viewGqlErrors, withMapData, withMaybeData)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, selected, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (ActionForm, UserState(..), initActionForm)
import ModelCommon.Codecs exposing (ActionType(..), DocType(..), NodeFocus, TensionCharac, nearestCircleid, nid2rootid, typeFromNameid)
import ModelCommon.View exposing (viewUser)
import ModelSchema exposing (..)
import String.Format as Format
import Text as T
import Time


type alias Help =
    { isModalActive : Bool
    , activeTab : HelpTab
    }


type HelpTab
    = QuickHelp
    | AskQuestion
    | Feedback


create : Help
create =
    { isModalActive = False
    , activeTab = QuickHelp
    }


open : Help -> Help
open data =
    { data | isModalActive = True }


changeTab : HelpTab -> Help -> Help
changeTab tab data =
    { data | activeTab = tab }


close : Help -> Help
close data =
    { data | isModalActive = False }


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
            [ text "Work in progress..."
            ]
        ]
