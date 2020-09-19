module Form.NewTension exposing
    ( NewTensionForm
    , Op
    , cancelUser
    , create
    , initCircle
    , post
    , postNode
    , selectUser
    , setActiveButton
    , setEvents
    , setForm
    , setResult
    , setSource
    , setStatus
    , setTarget
    , setViewMode
    , updateUserPattern
    , updateUserRole
    , view
    )

import Components.Fa as Fa
import Components.Loading as Loading exposing (viewGqlErrors)
import Components.Markdown exposing (renderMarkdown)
import Components.NodeDoc as NodeDoc
import Components.Text as T
import Dict
import Extra exposing (ternary, withDefaultData, withMaybeData)
import Extra.Events exposing (onClickPD, onClickPD2, onEnter, onKeydown, onTab)
import Form exposing (isPostSendable)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (toHref)
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, li, nav, option, p, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, placeholder, required, rows, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Maybe exposing (withDefault)
import ModelCommon exposing (InputViewMode(..), TensionForm)
import ModelCommon.Codecs exposing (NodeFocus, nodeFromFocus)
import ModelCommon.View exposing (edgeArrow, getTensionText, tensionTypeSpan)
import ModelSchema exposing (..)
import Time


type alias NewTensionForm =
    { form : TensionForm
    , result : GqlData Tension
    , activeButton : Maybe Int
    , viewMode : InputViewMode
    , isLookupOpen : Bool
    }


create : NodeFocus -> NewTensionForm
create focus =
    { form = initTension_ focus
    , result = NotAsked
    , activeButton = Nothing
    , viewMode = Write
    , isLookupOpen = False
    }


{-|

    Create tension a the current focus

-}
initTension_ : NodeFocus -> TensionForm
initTension_ focus =
    { uctx = UserCtx "" Nothing (UserRights False False) []
    , source = UserRole "" "" "" RoleType.Guest
    , target = nodeFromFocus focus
    , targetData = initNodeData
    , status = TensionStatus.Open
    , tension_type = TensionType.Operational
    , action = Nothing
    , post = Dict.empty
    , users = []
    , events_type = Nothing
    , blob_type = Nothing
    , node = initNodeFragment Nothing
    }


{-|

    Create New Circle tension

-}
initCircle : Node -> NodeType.NodeType -> NewTensionForm -> NewTensionForm
initCircle target type_ ntf =
    let
        form =
            ntf.form

        action =
            case type_ of
                NodeType.Role ->
                    TensionAction.NewRole

                NodeType.Circle ->
                    TensionAction.NewCircle

        node =
            initNodeFragment (Just type_)

        newForm =
            { form
                | target = target
                , tension_type = TensionType.Governance
                , action = Just action
                , blob_type = Just BlobType.OnNode
                , node = { node | charac = Just target.charac } -- inherit charac
            }
    in
    { ntf | form = newForm, result = NotAsked }


setForm : TensionForm -> NewTensionForm -> NewTensionForm
setForm form ntf =
    { ntf | form = form }


setResult : GqlData Tension -> NewTensionForm -> NewTensionForm
setResult result ntf =
    { ntf | result = result }


setSource : UserRole -> NewTensionForm -> NewTensionForm
setSource source ntf =
    let
        f =
            ntf.form

        newForm =
            { f | source = source }
    in
    { ntf | form = newForm }


setTarget : Node -> Maybe NodeData -> NewTensionForm -> NewTensionForm
setTarget target node_data ntf =
    let
        f =
            ntf.form

        newForm =
            { f | target = target, targetData = node_data |> withDefault initNodeData }
    in
    { ntf | form = newForm }


setStatus : TensionStatus.TensionStatus -> NewTensionForm -> NewTensionForm
setStatus status ntf =
    let
        f =
            ntf.form

        newForm =
            { f | status = status }
    in
    { ntf | form = newForm }


setEvents : List TensionEvent.TensionEvent -> NewTensionForm -> NewTensionForm
setEvents events ntf =
    let
        f =
            ntf.form

        newForm =
            { f | events_type = Just events }
    in
    { ntf | form = newForm }


post : String -> String -> NewTensionForm -> NewTensionForm
post field value ntf =
    let
        f =
            ntf.form

        newForm =
            { f | post = Dict.insert field value f.post }
    in
    { ntf | form = newForm }


postNode : String -> String -> NewTensionForm -> NewTensionForm
postNode field value ntf =
    { ntf | form = NodeDoc.updateNodeForm field value ntf.form }



-- User Lookup


updateUserPattern : Int -> String -> NewTensionForm -> NewTensionForm
updateUserPattern pos pattern ntf =
    let
        f =
            ntf.form

        newForm =
            { f | users = NodeDoc.updateUserPattern pos pattern f.users }
    in
    { ntf | form = newForm }


updateUserRole : Int -> String -> NewTensionForm -> NewTensionForm
updateUserRole pos role ntf =
    let
        f =
            ntf.form

        newForm =
            { f | users = NodeDoc.updateUserRole pos role f.users }
    in
    { ntf | form = newForm }


selectUser : Int -> String -> NewTensionForm -> NewTensionForm
selectUser pos username ntf =
    let
        f =
            ntf.form

        newForm =
            { f | users = NodeDoc.selectUser pos username f.users }
    in
    { ntf | form = newForm }


cancelUser : Int -> NewTensionForm -> NewTensionForm
cancelUser pos ntf =
    let
        f =
            ntf.form

        newForm =
            { f | users = NodeDoc.cancelUser pos f.users }
    in
    { ntf | form = newForm }


openLookup : NewTensionForm -> NewTensionForm
openLookup ntf =
    { ntf | isLookupOpen = True }


closeLookup : NewTensionForm -> NewTensionForm
closeLookup ntf =
    { ntf | isLookupOpen = False }



--- Form Buttons


setActiveButton : Bool -> NewTensionForm -> NewTensionForm
setActiveButton doClose ntf =
    if doClose then
        { ntf | activeButton = Just 0 }

    else
        { ntf | activeButton = Just 1 }


setViewMode : InputViewMode -> NewTensionForm -> NewTensionForm
setViewMode viewMode ntf =
    { ntf | viewMode = viewMode }


type alias Op msg =
    { lookup : List User

    -- modal control
    , onChangeInputViewMode : InputViewMode -> msg
    , onSubmitTension : TensionForm -> Bool -> Time.Posix -> msg
    , onSubmit : (Time.Posix -> msg) -> msg
    , onCloseModal : String -> msg

    -- doc change
    , onChangeNode : String -> String -> msg

    -- user selectors
    , onChangeUserPattern : Int -> String -> msg
    , onChangeUserRole : Int -> String -> msg
    , onSelectUser : Int -> String -> msg
    , onCancelUser : Int -> msg
    }


view : NewTensionForm -> Op msg -> Html msg
view data op =
    let
        form =
            data.form

        txt =
            getTensionText

        isLoading =
            data.result == LoadingSlowly

        isSendable =
            isPostSendable [ "title" ] form.post

        submitTension =
            ternary isSendable [ onClick (op.onSubmit <| op.onSubmitTension form False) ] []

        message =
            Dict.get "message" form.post |> withDefault ""
    in
    case data.result of
        Success res ->
            let
                link =
                    Route.Tension_Dynamic_Dynamic { param1 = form.target.rootnameid, param2 = res.id } |> toHref
            in
            div [ class "box is-light" ]
                [ Fa.icon "fas fa-check fa-2x has-text-success" " "
                , text (txt.added ++ " ")
                , a
                    [ href link
                    , onClickPD (op.onCloseModal link)
                    , target "_blank"
                    ]
                    [ text T.checkItOut ]
                ]

        other ->
            div [ class "modal-card finalModal" ]
                [ div [ class "modal-card-head" ]
                    [ div [ class "level modal-card-title" ]
                        [ div [ class "level-left" ] <|
                            List.intersperse (text "\u{00A0}")
                                [ span [ class "is-size-6 has-text-weight-semibold has-text-grey" ] [ text (txt.title ++ " | "), tensionTypeSpan "has-text-weight-medium" "text" form.tension_type ] ]
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
                                , placeholder "Title*"
                                , required True
                                , onInput (op.onChangeNode "title")
                                ]
                                []
                            ]
                        , p [ class "help-label" ] [ text txt.name_help ]
                        , br [] []
                        ]
                    , div [ class "message" ]
                        [ div [ class "message-header" ]
                            [ div [ class "tabs is-boxed is-small" ]
                                [ ul []
                                    [ li [ classList [ ( "is-active", data.viewMode == Write ) ] ] [ a [ onClickPD2 (op.onChangeInputViewMode Write), target "_blank" ] [ text "Write" ] ]
                                    , li [ classList [ ( "is-active", data.viewMode == Preview ) ] ] [ a [ onClickPD2 (op.onChangeInputViewMode Preview), target "_blank" ] [ text "Preview" ] ]
                                    ]
                                ]
                            ]
                        , div [ class "message-body" ]
                            [ div [ class "field" ]
                                [ div [ class "control" ]
                                    [ case data.viewMode of
                                        Write ->
                                            textarea
                                                [ id "textAreaModal"
                                                , class "textarea"
                                                , rows 10
                                                , placeholder "Leave a comment"
                                                , value message
                                                , onInput (op.onChangeNode "message")
                                                ]
                                                []

                                        Preview ->
                                            div [] [ renderMarkdown message "is-dark", hr [] [] ]
                                    ]
                                , p [ class "help-label" ] [ text txt.message_help ]
                                , br [] []
                                ]
                            ]
                        ]
                    ]
                , div [ class "modal-card-foot", attribute "style" "display: block;" ]
                    [ case other of
                        Failure err ->
                            viewGqlErrors err

                        _ ->
                            div [] []
                    , div [ class "field is-grouped is-grouped-right" ]
                        [ div [ class "control" ]
                            [ div [ class "buttons" ]
                                [ button
                                    ([ class "button has-text-weight-semibold"
                                     , classList [ ( "is-success", isSendable ), ( "is-loading", isLoading ) ]
                                     , disabled (not isSendable)
                                     ]
                                        ++ submitTension
                                    )
                                    [ text txt.submit ]
                                ]
                            ]
                        ]
                    ]
                ]
