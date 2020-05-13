port module Components.Org.Overview exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Array
import Browser.Navigation as Nav
import Components.Fa as Fa
import Components.Loading as Loading exposing (viewErrors)
import Date exposing (formatTime)
import Dict exposing (Dict)
import Extra exposing (onClickLink, onClickPD)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionType as TensionType
import Global exposing (Msg(..))
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, type_)
import Html.Events exposing (on, onClick, onInput)
import Iso8601 exposing (fromTime)
import Json.Decode as JD exposing (Value, decodeValue)
import Json.Encode as JE
import Json.Encode.Extra as JEE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelOrg exposing (..)
import Page exposing (Document, Page)
import Ports
import Process
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
    , user : UserState
    }



--
-- INIT
--


type alias Flags =
    { param1 : String, param2 : Maybe String, param3 : Maybe String }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        -- init Flags and Session
        session =
            global.session

        rootnameid =
            flags.param1

        focusFragment =
            String.join "#" [ flags.param2 |> withDefault "", flags.param3 |> withDefault "" ]

        newFocus =
            focusFromNameid <| String.join "#" [ rootnameid, focusFragment ]

        -- What has changed
        oldFocus =
            session.node_focus |> withDefault newFocus

        isInit =
            session.node_focus == Nothing

        orgChange =
            (newFocus.rootnameid /= oldFocus.rootnameid) || isInit

        focusChange =
            (newFocus.nameid /= oldFocus.nameid) || isInit

        --d1 = Debug.log "isInit, orgChange, focuChange" [ isInit, orgChange, focusChange ]
        --d2 = Debug.log "newfocus" [ newFocus ]
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
            , user = session.user
            }

        cmds =
            if orgChange then
                [ fetchNodesOrga newFocus.rootnameid GotOrga
                , fetchCircleTension newFocus.nameid GotTensions
                , Task.perform (\_ -> PassedSlowLoadTreshold) (Process.sleep 500)
                ]

            else if focusChange then
                [ Ports.focusGraphPack newFocus.nameid
                , fetchCircleTension newFocus.nameid GotTensions
                , Task.perform (\_ -> PassedSlowLoadTreshold) (Process.sleep 500)
                ]

            else
                []
    in
    ( model
    , Cmd.batch cmds
    , Global.send (UpdateSessionFocus newFocus)
    )



--
-- UPDATE
--


type Msg
    = PassedSlowLoadTreshold -- timer
      --
    | GotOrga (GqlData NodesData) -- graphql
    | GotTensions (GqlData TensionsData) -- graphql
      --
    | DoNodeAction NodeTarget -- ports receive / tooltip click
    | DoTensionTypeForm
    | DoTensionSourceForm String -- <- {tensionType}
    | DoTensionFinalForm Node -- <- {source}
    | ChangeTensionPost String String -- { field value}
    | Submit (Time.Posix -> Msg)
    | SubmitTension TensionForm Time.Posix -- model ends
    | TensionAck (GqlData (Maybe AddTensionPayload)) -- decode beter to get IdPayload
      --
    | NodeClicked String -- ports receive
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
                        , Ports.initGraphPack data model.node_focus.nameid
                        , Global.send (UpdateSessionOrga data)
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

        DoTensionTypeForm ->
            let
                modelUpdated =
                    updateTensionStep model TensionTypeForm Nothing
            in
            ( modelUpdated, Cmd.none, Ports.bulma_driver "actionModal" )

        DoTensionSourceForm tensionType ->
            let
                maybeForm =
                    updateTensionPost model "type_" tensionType

                nextStep =
                    maybeForm
                        |> Maybe.map
                            (\form ->
                                case form.user.roles of
                                    [] ->
                                        TensionNotAuthorized "You are not a member of this organisation. Please, Join this organisation to be able to create a tension."

                                    [ r ] ->
                                        TensionFinalForm (Just r)

                                    roles ->
                                        TensionSourceForm roles
                            )
                        |> withDefault TensionTypeForm

                modelUpdated =
                    updateTensionStep model nextStep maybeForm
            in
            ( modelUpdated, Cmd.none, Ports.bulma_driver "actionModal" )

        DoTensionFinalForm source ->
            let
                maybeForm =
                    getTensionForm model
                        |> Maybe.map
                            (\f ->
                                { f | source = Just source }
                            )

                modelUpdated =
                    updateTensionStep model (TensionFinalForm Nothing) maybeForm
            in
            ( modelUpdated, Cmd.none, Ports.bulma_driver "actionModal" )

        ChangeTensionPost field value ->
            let
                maybeForm =
                    updateTensionPost model field value

                modelUpdated =
                    updateTensionStep model (TensionFinalForm Nothing) maybeForm
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
            case form.source of
                Just source ->
                    ( model, addOneTension source form.target postUpdated TensionAck, Cmd.none )

                Nothing ->
                    ( model, Cmd.none, Cmd.none )

        TensionAck result ->
            let
                maybeForm =
                    getTensionForm model
                        |> Maybe.map
                            (\f ->
                                { f | result = result }
                            )

                modelUpdated =
                    updateTensionStep model TensionValidation maybeForm
            in
            ( modelUpdated, Cmd.none, Cmd.none )

        NodeClicked nameid ->
            ( model
            , Cmd.none
            , Nav.replaceUrl global.key (uriFromNameid nameid)
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


port nodeClickedFromJs : (String -> msg) -> Sub msg


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
    let
        nodePathErr =
            case model.node_path of
                Just path ->
                    if Array.length path > 0 then
                        ( Just path, False )

                    else
                        ( Nothing, True )

                Nothing ->
                    ( Nothing, False )

        nodePath =
            Tuple.first nodePathErr

        pathErr =
            Tuple.second nodePathErr
    in
    if pathErr then
        div [] [ text "Sorry, this node doesn exist yet." ]

    else
        div
            [ class "columns is-centered" ]
            [ -- div [ class "column is-1 is-fullheight is-hidden-mobile", id "leftPane" ] [ viewLeftPane model ]
              div [ class "column is-10", id "mainPane" ]
                [ div [ class "columns" ]
                    [ viewHelperBar nodePath ]
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
                [ div [ class "hero-body has-text-centered" ] [ text model.node_focus.rootnameid ] ]
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


viewHelperBar : Maybe NodePath -> Html Msg
viewHelperBar maybeNodePath =
    let
        nodePath =
            maybeNodePath |> withDefault (Array.fromList [])
    in
    nav
        [ class "column is-full breadcrumb"
        , attribute "aria-label" "breadcrumbs"
        ]
        [ Fa.icon "fas fa-angle-right" ""
        , Array.indexedMap
            (\i p ->
                if i < (Array.length nodePath - 1) then
                    li [] [ a [ href (uriFromNameid p.nameid), onClickPD (NodeClicked p.nameid), attribute "target" "_self" ] [ text p.name ] ]

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

        -- Hidden class use in graphpack_d3.js
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
                        [ li [] [ text "Develop, and push forward Fractal6." ]
                        , li [] [ text "Maintain the security of the platform." ]

                        --, li [] [ text "Find a business model for fractal6." ]
                        ]
                    ]
                , h2 [ class "title is-5" ] [ text "Domains" ]
                , p []
                    [ ul []
                        [ li [] [ text "Pubic repo of fractal6." ]
                        , li [] [ text "Public machine database of fractal6." ]

                        --, li [] [ text "Find a business model for fractal6." ]
                        ]
                    ]
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

        focus =
            model.node_focus
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
                        div [] [ text <| "No tensions for this " ++ NodeType.toString focus.type_ ++ " yet." ]

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
                        viewErrors err
                ]
            , button [ class "modal-close is-large" ] []
            ]
        ]


viewActionStep : ActionStep Node -> Html Msg
viewActionStep step =
    case step of
        AuthNeeded ->
            div [ class "box has-background-info" ]
                [ p [] [ text "Please login or create an account to perform this action." ]
                ]

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
                            [ div [ class "level-item" ] [ div [ class "button is-primary", onClick DoTensionTypeForm ] [ text "New tension" ] ]
                            , div [ class "level-item" ] [ div [ class "button is-info" ] [ text "New Role" ] ]
                            , div [ class "level-item" ] [ div [ class "button is-link" ] [ text "New Sub-Circle" ] ]
                            ]

                        else
                            [ div [ class "level-item" ] [ div [ class "button is-primary", onClick DoTensionTypeForm ] [ text "New tension" ] ] ]
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
                                        , onClick (DoTensionSourceForm tensionTypeStr)
                                        ]
                                        [ text tensionTypeStr ]
                                    ]
                            )
                        <|
                            TensionType.list
                    ]
                ]

        TensionSourceForm roles ->
            div [ class "card" ]
                [ div [ class "card-header" ]
                    [ div [ class "card-header-title" ]
                        [ span [ class "has-text-weight-medium" ] [ text "You have several roles in this organisation. Please select the role from which you want to create this tension:" ] ]
                    ]
                , div [ class "card-content" ]
                    [ div [ class "level buttonRadio" ] <|
                        List.map
                            (\role ->
                                div [ class "level-item" ]
                                    [ div
                                        [ class "button"
                                        , onClick (DoTensionFinalForm <| nodeSourceFromRole role)
                                        ]
                                        [ text role.name ]

                                    -- get the parent namid from the dict !!
                                    ]
                            )
                        <|
                            roles
                    ]
                ]

        TensionFinalForm maybeRole ->
            let
                role =
                    maybeRole |> withDefault (UserRole "" "" "" RoleType.Guest)

                source =
                    form.source |> withDefault (nodeSourceFromRole role)

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
                , div
                    [ class "card-content"
                    ]
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
                    viewErrors err

                default ->
                    -- @TODO: better handle this with an uniform interface with slowRemoteLoading
                    div [ class "box is-loading" ] [ text "Loading..." ]

        TensionNotAuthorized msg ->
            viewErrors msg



-------------------------------------------------
-- Model Getters and Setters
-------------------------------------------------
-- Setters


initTensionForm : String -> UserCtx -> Node -> TensionForm
initTensionForm rootnameid uctx node =
    let
        -- filter role that are not in this orga
        roles =
            uctx.roles |> List.filter (\r -> r.rootnameid == rootnameid)
    in
    { step = TensionTypeForm
    , post = Dict.empty
    , result = NotAsked
    , source = Nothing
    , target = node
    , user = { uctx | roles = roles }
    , rootnameid = rootnameid
    }


updateTensionStep : Model -> TensionStep -> Maybe TensionForm -> Model
updateTensionStep model newStep maybeForm =
    let
        rootnameid =
            model.node_focus.nameid

        newAction =
            case model.node_action of
                Ask step ->
                    Ask <|
                        case step of
                            FirstStep target ->
                                -- TensionStep is ignored here is reinitialized t firsst step
                                case model.user of
                                    LoggedIn uctx ->
                                        AddTensionStep (initTensionForm rootnameid uctx target)

                                    LoggedOut ->
                                        AuthNeeded

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

                            AuthNeeded ->
                                AuthNeeded

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



-- Getters


isTensionSendable : TensionForm -> Bool
isTensionSendable form =
    let
        title =
            Dict.get "title" form.post |> withDefault ""

        isSendable =
            String.length title > 0
    in
    isSendable


getTensionForm : Model -> Maybe TensionForm
getTensionForm model =
    case model.node_action of
        Ask step ->
            case step of
                AddTensionStep form ->
                    Just form

                default ->
                    Nothing

        default ->
            Nothing



-- Json encoder/decoder --When receiving data from Javascript


nodeSourceFromRole : UserRole -> Node
nodeSourceFromRole ur =
    { id = "" -- obsolete ?
    , nameid = ur.nameid
    , name = ur.name
    , parent = Nothing
    , type_ = NodeType.Role
    }


nodeDecoder : JD.Decoder Node
nodeDecoder =
    -- @DEBUG: Use a dict structure instead to get back node from ID only.
    JD.map5 Node
        (JD.field "id" JD.string)
        (JD.field "name" JD.string)
        (JD.field "nameid" JD.string)
        (JD.maybe (JD.map ParentNode <| JD.field "parentid" JD.string))
        (JD.field "type_" NodeType.decoder)
