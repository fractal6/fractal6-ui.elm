port module Components.Org.Overview exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Array
import Browser.Navigation as Nav
import Components.Fa as Fa
import Components.Loading as Loading exposing (viewErrors)
import Date exposing (formatTime)
import Dict exposing (Dict)
import Extra exposing (onClickLink, onClickPD)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.TensionType as TensionType
import Global exposing (Msg(..))
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, autofocus, class, classList, disabled, href, id, placeholder, rows, type_)
import Html.Events exposing (on, onClick, onInput)
import Http
import Iso8601 exposing (fromTime)
import Json.Decode as JD exposing (Value, decodeValue)
import Json.Encode as JE
import Json.Encode.Extra as JEE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelOrg exposing (..)
import Page exposing (Document, Page)
import Ports
import RemoteData exposing (RemoteData)
import Task
import Time


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--
-- Model
--


type alias Model =
    { node_focus : NodeFocus
    , node_path : Maybe NodePath
    , orga_data : OrgaData
    , circle_tensions : CircleTensionsData
    , node_action : ActionState
    }



--
-- INIT
--


type alias Flags =
    { param1 : String, param2 : Maybe String }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        -- init Flags and Session
        session =
            global.session

        rootid =
            flags.param1

        focusid =
            flags.param2 |> withDefault rootid

        newFocus =
            NodeFocus rootid focusid (flags.param2 == Nothing)

        -- What has changed
        oldFocus =
            session.node_focus |> withDefault newFocus

        isInit =
            session.node_focus == Nothing

        orgChange =
            (rootid /= oldFocus.rootid) || isInit

        focusChange =
            (focusid /= oldFocus.nameid) || isInit

        -- init Model
        model =
            { orga_data =
                session.orga_data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , circle_tensions =
                session.circle_tensions
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , node_action = session.node_action |> withDefault NotAsk
            , node_focus = newFocus
            , node_path = session.node_path
            }

        cmds =
            if orgChange then
                [ fetchNodesOrga newFocus.rootid GotOrga
                , fetchCircleTension newFocus.nameid GotTensions
                , Task.perform (\_ -> PassedSlowLoadTreshold) Loading.slowTreshold
                ]

            else if focusChange then
                [ Ports.focusGraphPack newFocus.nameid
                , fetchCircleTension newFocus.nameid GotTensions
                , Task.perform (\_ -> PassedSlowLoadTreshold) Loading.slowTreshold
                ]

            else
                []
    in
    ( model, Cmd.batch cmds, Cmd.none )



--
-- UPDATE
--


type Msg
    = PassedSlowLoadTreshold -- timer
    | GotOrga (RequestResult ErrorData NodesData) -- graphql
    | GotTensions (RequestResult ErrorData TensionsData) -- graphql
    | DoNodeAction NodeTarget -- ports receive
    | DoTensionStep1
    | DoTensionStep2 String
    | ChangeTensionPost String String
    | Submit (Time.Posix -> Msg)
    | SubmitTension TensionForm Time.Posix
    | TensionAck (RequestResult ErrorData (Maybe AddTensionPayload)) -- decode beter to get IdPayload
    | NodeClicked NodeFocus -- ports receive
    | NodeFocused NodePath -- ports receive
    | ToggleGraphReverse -- ports send
    | ToggleTooltips -- ports send -- Not implemented @DEBUG multiple tooltip/ see name of circle


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global msg model =
    case msg of
        PassedSlowLoadTreshold ->
            let
                orga_data =
                    case model.orga_data of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other

                circle_tensions =
                    case model.circle_tensions of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other
            in
            ( { model | orga_data = orga_data, circle_tensions = circle_tensions }, Cmd.none, Cmd.none )

        GotOrga result ->
            case result of
                Success data ->
                    if Dict.size data > 0 then
                        ( { model | orga_data = Success data }
                        , Ports.initGraphPack <| JE.encode 0 <| nodesDataEncoder data model.node_focus.nameid
                        , updateGlobalOrga data
                        )

                    else
                        --( model, MoveTo404, Cmd.none )
                        ( model, Cmd.none, Cmd.none )

                default ->
                    ( { model | orga_data = result }, Cmd.none, Cmd.none )

        GotTensions result ->
            ( { model | circle_tensions = result }, Cmd.none, Cmd.none )

        DoNodeAction action ->
            let
                newAction =
                    case action of
                        Ok node ->
                            Ask <| FirstStep node

                        Err err ->
                            AskErr <| JD.errorToString err
            in
            ( { model | node_action = newAction }, Cmd.none, Cmd.none )

        DoTensionStep1 ->
            let
                modelUpdated =
                    updateTensionStep model TensionTypeForm Nothing
            in
            ( modelUpdated, Cmd.none, Ports.bulma_driver "actionModal" )

        DoTensionStep2 tensionType ->
            let
                maybeForm =
                    updateTensionPost model "type_" tensionType

                modelUpdated =
                    updateTensionStep model TensionFinalForm maybeForm
            in
            ( modelUpdated, Cmd.none, Ports.bulma_driver "actionModal" )

        ChangeTensionPost field content ->
            let
                maybeForm =
                    updateTensionPost model field content

                modelUpdated =
                    updateTensionStep model TensionFinalForm maybeForm
            in
            -- No need to reactivate the Bulma drivers here.
            ( modelUpdated, Cmd.none, Cmd.none )

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        SubmitTension form time ->
            let
                postUpdated =
                    Dict.insert "createdAt" (fromTime time) form.post
            in
            ( model, addOneTension form.source form.target postUpdated TensionAck, Cmd.none )

        TensionAck result ->
            let
                form =
                    case getTensionForm model of
                        Just f ->
                            Just { f | result = result }

                        Nothing ->
                            Nothing

                modelUpdated =
                    updateTensionStep model TensionValidation form
            in
            ( modelUpdated, Cmd.none, Cmd.none )

        NodeClicked focus ->
            ( model
              --( { model | node_focus = focus }
            , Cmd.none
            , Nav.replaceUrl global.key (uriFromFocus focus)
            )

        NodeFocused path ->
            ( { model | node_path = Just path }, Cmd.none, Cmd.none )

        ToggleGraphReverse ->
            ( model, () |> sendToggleGraphReverse, Cmd.none )

        ToggleTooltips ->
            ( model, () |> sendToggleTooltips, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ _ =
    Sub.batch
        [ nodeClickedFromJs NodeClicked
        , nodeFocusedFromJs NodeFocused
        , nodeDataFromJs DoNodeAction
        ]



-- Receive to Javascript


port nodeClickedFromJs : (NodeFocus -> msg) -> Sub msg


port nodeFocusedFromJs : (NodePath -> msg) -> Sub msg


port rawNodeDataFromJs : (JD.Value -> a) -> Sub a


nodeDataFromJs : (NodeTarget -> msg) -> Sub msg
nodeDataFromJs rawNode =
    rawNodeDataFromJs (rawNode << JD.decodeValue nodeDecoder)



-- Send to JS


port sendToggleGraphReverse : () -> Cmd msg


port sendToggleTooltips : () -> Cmd msg



-- VIEW
--


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Org.Dynamic" -- get title from flag
    , body = [ view_ global model ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div
        [ class "columns is-centered" ]
        [ -- div [ class "column is-1 is-fullheight is-hidden-mobile", id "leftPane" ] [ viewLeftPane model ]
          div [ class "column is-10", id "mainPane" ]
            [ div [ class "columns" ]
                [ viewHelperBar global model ]
            , div [ class "columns is-variable is-4" ]
                [ div [ class "column is-6" ]
                    [ viewCanvas global model
                    , br [] []
                    , viewMandate global model
                    , setupActionModal global model
                    ]
                , div [ class "column is-6" ]
                    [ div [ class "columns is-gapless" ]
                        [ div [ class "column is-12", id "nextToChart" ]
                            [ viewActivies global model ]
                        ]
                    ]
                ]
            ]
        ]


viewLeftPane : Model -> Html Msg
viewLeftPane model =
    nav [ class "menu" ]
        [ p [ class "menu-label" ]
            [ div [ class "hero is-small is-primary is-bold" ]
                [ div [ class "hero-body has-text-centered" ] [ text model.node_focus.rootid ] ]
            ]
        , ul [ class "menu-list" ]
            [ li [ class "menu-label" ]
                [ div [ class "hero is-small is-info is-bold" ]
                    [ div [ class "hero-body" ]
                        [ Fa.icon "far fa-circle fa-lg" model.node_focus.nameid ]
                    ]
                ]
            , li []
                [ ul [ class "menu-list" ]
                    [ li []
                        [ a []
                            [ Fa.icon "fas fa-scroll fa-xs" "Mandates" ]
                        ]
                    , li []
                        [ a []
                            --  fa-exclamation-circle
                            [ Fa.icon "fas fa-exchange-alt fa-xs" "Tensions" ]
                        ]
                    , li []
                        [ a []
                            [ Fa.icon "fas fa-history fa-xs" "Journal" ]
                        ]
                    , li []
                        [ a []
                            [ Fa.icon "fas fa-user fa-xs" "Members" ]
                        ]
                    ]
                ]
            ]
        ]


viewHelperBar : Global.Model -> Model -> Html Msg
viewHelperBar global model =
    let
        nodePath =
            model.node_path |> withDefault (Array.fromList [])
    in
    nav
        [ class "column is-full breadcrumb"
        , attribute "aria-label" "breadcrumbs"
        ]
        [ Fa.icon "fas fa-angle-right" ""
        , Array.indexedMap
            (\i p ->
                let
                    hereFocus =
                        NodeFocus model.node_focus.rootid p.nameid (i == 0)
                in
                if i < (Array.length nodePath - 1) then
                    li [] [ a [ href (uriFromFocus hereFocus), onClickPD (NodeClicked hereFocus), attribute "target" "_self" ] [ text p.name ] ]

                else
                    li [ class "is-active has-text-weight-semibold" ]
                        [ a [ attribute "aria-current" "page", href "#" ] [ text p.name ] ]
            )
            nodePath
            |> Array.toList
            |> ul [ attribute "style" "display: inline-flex;" ]
        ]


viewCanvas : Global.Model -> Model -> Html Msg
viewCanvas global model =
    let
        isLoading =
            case model.orga_data of
                LoadingSlowly ->
                    True

                default ->
                    False

        error =
            case model.orga_data of
                Failure err ->
                    ( True, err )

                default ->
                    ( False, "" )

        hasErr =
            Tuple.first error

        errMsg =
            Tuple.second error
    in
    div []
        [ div
            [ id "canvasParent"
            , classList [ ( "spinner", isLoading ) ]
            ]
          <|
            if hasErr then
                [ viewErrors errMsg ]

            else
                []

        -- Hidden class use in circlepacking_d3.js
        , div [ id "canvasButtons", class "buttons are-small is-invisible" ]
            [ div
                [ id "invGraph_cvbtn"
                , class "button buttonToggle tooltip has-tooltip-right"
                , attribute "data-tooltip" "Reverse the organisation graph."
                , onClick ToggleGraphReverse
                ]
                [ Fa.icon0 "fas fa-sort-amount-up" "" ]

            --, div
            --    [ id "showLabel_cvbtn"
            --    , class "button buttonToggle tooltip has-tooltip-right"
            --    , attribute "data-tooltip" "Show/Hide circle tooltips."
            --    , onClick ToggleTooltips
            --    ]
            --    [ Fa.icon0 "fas fa-caret-square-down" "" ]
            ]
        , div
            [ id "nodeTooltip"
            , class "modalTrigger is-invisible"
            , attribute "data-modal" "actionModal"
            ]
            [ span [] [ text "void" ] -- Node name
            , span [ class "fa-stack fa-sm ellipsisArt" ]
                [ i [ class "fas fa-ellipsis-h fa-stack-1x" ] []

                -- To be copied before fa-ellipis !
                --, i[class "far fa-circle fa-stack-2x"][]
                --, i[class "fas fa-circle fa-stack-2x"][]
                ]
            ]
        ]


viewMandate : Global.Model -> Model -> Html Msg
viewMandate global model =
    div [ id "mandateContainer", class "hero is-small is-light heroViewer box" ]
        [ div [ class "hero-body" ]
            [ h1 [ class "title is-3" ]
                [ Fa.icon "fas fa-scroll fa-xs" "Mandate" ]
            , hr [ class "has-background-grey-light" ] []
            , div [ class "content" ]
                [ h2 [ class "title is-5" ] [ text "Purpose" ]
                , p [] [ text "Helping people and human organisations to find resillient, efficient and anti alienating models and praxis for self organisation." ]
                , h2 [ class "title is-5" ] [ text "Responsabilities" ]
                , p []
                    [ ul []
                        [ li [] [ text "Develop, maintains and push forward Fractal6." ]
                        , li [] [ text "Find a business model for fractal6." ]
                        ]
                    ]
                , h2 [ class "title is-5" ] [ text "Domains" ]
                , p [] [ text "See sub domains." ]
                ]
            ]
        ]


viewActivies : Global.Model -> Model -> Html Msg
viewActivies global model =
    let
        isLoading =
            case model.circle_tensions of
                LoadingSlowly ->
                    True

                default ->
                    False
    in
    div
        [ class "box"
        , attribute "style" "flex-grow: 1;"
        ]
        [ div [ class "title" ]
            [ div [ class "tabs" ]
                [ ul []
                    [ li [ class "is-active" ]
                        [ a []
                            [ Fa.icon "fas fa-exchange-alt fa-sm" "Tensions" ]
                        ]
                    , li []
                        [ a [ class "is-" ]
                            [ Fa.icon "fas fa-history fa-sm" "Journal" ]
                        ]
                    ]
                ]
            ]
        , div [ classList [ ( "content", True ), ( "spinner", isLoading ) ] ]
            [ case model.circle_tensions of
                Success tensions ->
                    if List.length tensions > 0 then
                        List.map (\t -> mediaTension t) tensions
                            |> div [ class "is-size-7", id "tensionsTab" ]

                    else
                        -- @DEBUG: for this role/circle
                        div [] [ text "No tensions for this circle yet." ]

                Failure err ->
                    viewErrors err

                default ->
                    div [] []
            ]

        --, a [ class "Footer has-text-centered" ] [ text "See more" ]
        --]
        ]


mediaTension : Tension -> Html Msg
mediaTension tension =
    div [ class "media Box" ]
        [ div [ class "media-left" ]
            [ div
                [ class "tooltip has-tooltip-top"
                , attribute "data-tooltip" ("type: " ++ TensionType.toString tension.type_)
                ]
                [ div [ class <| "Circle " ++ tensionTypeColor "text" tension.type_ ] [ text "" ] ]
            ]
        , div [ class "media-content" ]
            [ div [ class "content" ]
                [ div [ class "has-text-weight-semibold" ]
                    [ text tension.title ]
                ]
            , div [ class "labelsList" ] <|
                (tension.labels
                    |> withDefault []
                    |> List.map
                        (\label ->
                            span [ class "tag" ] [ text label.name ]
                        )
                )
            , br [ class "is-block" ] []
            , span [] <| tensionTypeArrow "has-text-weight-light" tension.emitter.name tension.receiver.name
            , span [ class "is-pulled-right has-text-weight-light" ] [ text <| " opened the " ++ formatTime tension.createdAt ]
            ]
        , div
            [ class "media-right" ]
            [ let
                n_comments =
                    tension.n_comments |> withDefault 0
              in
              if n_comments > 0 then
                div
                    [ class "tooltip has-tooltip-top"
                    , attribute "data-tooltip" ("comments: " ++ String.fromInt n_comments)
                    ]
                    [ Fa.icon0 "fas fa-comment-dots" (String.fromInt n_comments)
                    ]

              else
                text ""
            ]
        ]


setupActionModal : Global.Model -> Model -> Html Msg
setupActionModal global model =
    div []
        [ div [ id "actionModal", class "modal modal-fx-fadeIn" ]
            [ div [ class "modal-background" ] []
            , div [ class "modal-content" ]
                [ case model.node_action of
                    Ask step ->
                        viewActionStep step

                    NotAsk ->
                        text ""

                    AskErr err ->
                        div [ classList [ ( "box", True ), ( "has-background-danger", True ) ] ]
                            [ text <| "Unexpected error:" ++ err ]
                ]
            , button [ class "modal-close is-large" ] []
            ]
        ]


viewActionStep : ActionStep Node -> Html Msg
viewActionStep step =
    case step of
        FirstStep node ->
            div [ class "card" ]
                [ div [ class "card-header" ]
                    [ div [ class "card-header-title" ] <|
                        List.intersperse (text "\u{00A0}")
                            [ span [ class "has-text-weight-medium" ] [ text "What action do you want to do with the" ]
                            , span [ class "has-text-weight-bold is-underline-dotted" ] [ text node.name ]
                            , span [ class "is-lowercase has-text-weight-semibold" ] [ text <| NodeType.toString node.type_ ]
                            , text "?"
                            ]
                    ]
                , div [ class "card-content" ]
                    [ div [ class "level" ] <|
                        if node.type_ == NodeType.Circle then
                            [ div [ class "level-item" ] [ div [ class "button is-primary", onClick DoTensionStep1 ] [ text "New tension" ] ]
                            , div [ class "level-item" ] [ div [ class "button is-info" ] [ text "New Role" ] ]
                            , div [ class "level-item" ] [ div [ class "button is-link" ] [ text "New Sub-Circle" ] ]
                            ]

                        else
                            [ div [ class "level-item" ] [ div [ class "button is-primary", onClick DoTensionStep1 ] [ text "New tension" ] ] ]
                    ]
                ]

        AddTensionStep form ->
            viewTensionStep form


viewTensionStep : TensionForm -> Html Msg
viewTensionStep form =
    case form.step of
        TensionTypeForm ->
            div [ class "card" ]
                [ div [ class "card-header" ]
                    [ div [ class "card-header-title" ]
                        [ span [ class "has-text-weight-medium" ] [ text "Choose the type of tension to communicate:" ] ]
                    ]
                , div [ class "card-content" ]
                    [ div [ class "level buttonRadio" ] <|
                        List.map
                            (\tensionType ->
                                let
                                    tensionTypeStr =
                                        TensionType.toString tensionType
                                in
                                div [ class "level-item" ]
                                    [ div
                                        [ class <| "button " ++ tensionTypeColor "background" tensionType
                                        , onClick (DoTensionStep2 tensionTypeStr)
                                        ]
                                        [ text tensionTypeStr ]
                                    ]
                            )
                        <|
                            TensionType.list
                    ]
                ]

        TensionFinalForm ->
            let
                isSendable =
                    isTensionSendable form
            in
            div [ class "card" ]
                [ div [ class "card-header" ]
                    [ div [ class "card-header-title" ]
                        [ div [] <|
                            List.intersperse (text "\u{00A0}")
                                [ span [ class "is-size-6" ] [ text "Create tension | ", tensionTypeSpan "has-text-weight-medium" "text" form.post ]
                                ]
                        ]
                    , div [ class "card-content" ] <| tensionTypeArrow "button" form.target.name form.target.name
                    ]
                , div [ class "card-content" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ input
                                [ class "input autofocus followFocus"
                                , attribute "data-nextfocus" "textAreaModal"
                                , type_ "text"
                                , placeholder "Title"
                                , onInput (ChangeTensionPost "title")
                                ]
                                []
                            ]
                        , p [ class "help" ] [ text "Title that sumarize your tension." ]
                        ]
                    , br [] []
                    , div [ class "field" ]
                        [ div [ class "control" ]
                            [ textarea
                                [ id "textAreaModal"
                                , class "textarea"
                                , rows 12
                                , placeholder "Leave a comment"
                                , onInput (ChangeTensionPost "message")
                                ]
                                []
                            ]
                        , p [ class "help" ] [ text "Add a description to help others understand your issue." ]
                        ]
                    , br [] []
                    , div [ class "field is-grouped is-grouped-right" ]
                        [ div [ class "control" ]
                            [ if isSendable then
                                button
                                    [ class "button is-success has-text-weight-semibold"
                                    , onClick (Submit <| SubmitTension form)
                                    ]
                                    [ text "Submit new tension" ]

                              else
                                button [ class "button has-text-weight-semibold", disabled True ]
                                    [ text "Submit new tension" ]
                            ]
                        ]
                    ]
                ]

        TensionValidation ->
            case form.result of
                Success res ->
                    div [ class "box has-background-success" ] [ text "Tension added." ]

                Failure err ->
                    div [ class "box has-background-danger" ] [ text err ]

                default ->
                    -- @TODO: better handle this with an uniform interface with slowRemoteLoading
                    div [ class "box is-loading" ] [ text "Loading..." ]



-------------------------------------------------
-- Model Getters and Setters
-------------------------------------------------
-- Global Setters


updateGlobalFocus : NodeFocus -> Cmd Global.Msg
updateGlobalFocus focus =
    Task.perform (always (UpdateSessionFocus focus)) (Task.succeed ())


updateGlobalOrga : NodesData -> Cmd Global.Msg
updateGlobalOrga data =
    Task.perform (always (UpdateSessionOrga data)) (Task.succeed ())



-- Setters


initTensionForm : Node -> TensionForm
initTensionForm node =
    { step = TensionTypeForm
    , post = Dict.fromList [ ( "username", "clara" ) ]
    , result = NotAsked
    , target = node
    , source = node
    }


updateTensionStep : Model -> TensionStep -> Maybe TensionForm -> Model
updateTensionStep model newStep maybeForm =
    let
        newAction =
            case model.node_action of
                Ask step ->
                    Ask <|
                        case step of
                            FirstStep target ->
                                AddTensionStep (initTensionForm target)

                            AddTensionStep form ->
                                let
                                    newForm =
                                        case maybeForm of
                                            Just aform ->
                                                { aform | step = newStep }

                                            Nothing ->
                                                { form | step = newStep }
                                in
                                AddTensionStep newForm

                passing ->
                    passing
    in
    { model | node_action = newAction }


updateTensionPost : Model -> String -> String -> Maybe TensionForm
updateTensionPost model field value =
    case getTensionForm model of
        Just form ->
            Just { form | post = Dict.insert field value form.post }

        Nothing ->
            Nothing



-- getters


isTensionSendable : TensionForm -> Bool
isTensionSendable form =
    let
        title =
            Dict.get "title" form.post |> withDefault ""

        isSendable =
            String.length title > 0
    in
    isSendable


uriFromFocus : NodeFocus -> String
uriFromFocus focus =
    if focus.isRoot then
        String.join "/" [ "/org", focus.rootid ]

    else
        String.join "/" [ "/org", focus.rootid, focus.nameid ]


getTensionForm : Model -> Maybe TensionForm
getTensionForm model =
    case model.node_action of
        Ask step ->
            case step of
                FirstStep target ->
                    Nothing

                AddTensionStep form ->
                    Just form

        passing ->
            Nothing



-- Json encoder/decoder --When receiving data from Javascript


decodeNested maybe field =
    case maybe of
        Just x ->
            Just x.id

        Nothing ->
            Nothing


nodesDataEncoder : NodesData -> String -> JE.Value
nodesDataEncoder data focus =
    JE.object
        [ ( "data", nodesEncoder data )
        , ( "focus", JE.string focus )
        ]


nodesEncoder : NodesData -> JE.Value
nodesEncoder nodes =
    JE.list JE.object <| List.map nodeEncoder <| Dict.values nodes


nodeEncoder : Node -> List ( String, JE.Value )
nodeEncoder node =
    [ ( "ID", JE.string node.id )
    , ( "name", JE.string node.name )
    , ( "nameid", JE.string node.nameid )
    , ( "parentID", JEE.maybe JE.string <| decodeNested node.parent "id" )
    , ( "type_", JE.string <| NodeType.toString node.type_ )
    ]


nodeDecoder : JD.Decoder Node
nodeDecoder =
    -- @DEBUG: Use a dict structure instead to get back node from ID only.
    JD.map5 Node
        (JD.field "ID" JD.string)
        (JD.field "name" JD.string)
        (JD.field "nameid" JD.string)
        (JD.maybe (JD.map ParentNode <| JD.field "parentID" JD.string))
        (JD.field "type_" NodeType.decoder)
