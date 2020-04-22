port module Pages.Org.Dynamic exposing (Model, Msg, page)

import Array
import Components.Fa as Fa
import Components.Loading as Loading exposing (Status(..), showMaybeError)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.TensionType as TensionType
import Generated.Org.Params as Params
import Generated.Routes exposing (Route)
import Global exposing (NID)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, li, nav, p, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, href, id)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode as JD exposing (Value, decodeValue)
import Json.Encode as JE
import Json.Encode.Extra as JEE
import Model exposing (..)
import Ports
import RemoteData exposing (RemoteData)
import Spa.Page
import Task
import Utils.Spa exposing (Page, PageContext)


page : Page Params.Dynamic Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = \{ model } -> String.join " | " [ model.asked_orga ]
        , init = init
        , update = always update
        , subscriptions = always subscriptions
        , view = always view
        }



--
-- Model
--
{-
   Main model data
-}


type alias ErrorData =
    String


type alias Model =
    { route : Route
    , asked_orga : String
    , orga_data : Status ErrorData NodesData
    , circle_tensions : Status ErrorData TensionsData
    , node_focus : NodeFocusState
    , node_action : ActionState
    }



{-
   Session Data
-}


type alias NodeFragment =
    Result JD.Error Node


type alias NodeAction =
    { n : NodeFragment
    , step : ActionStep
    }


type ActionState
    = Ask ActionStep Node
    | NotAsk
    | AskErr String


type ActionStep
    = FirstStep
    | TensionStep


type alias NodeFocusState =
    { nidjs : NID
    , name : String
    , nodeType : String
    , path : Array.Array { name : String, nidjs : NID }
    }



{-
   Json encoder/decoder
-}


decodeNested maybe field =
    case maybe of
        Just x ->
            Just x.id

        Nothing ->
            Nothing


nodesEncoder : NodesData -> JE.Value
nodesEncoder nodes =
    JE.list JE.object <| List.map nodeEncoder nodes


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



--
-- INIT
--


init : PageContext -> Params.Dynamic -> ( Model, Cmd Msg, Cmd Global.Msg )
init { route } params =
    let
        orga_name =
            params.param1

        focus =
            { name = orga_name
            , nidjs = ""
            , nodeType = ""
            , path = Array.fromList [ { name = orga_name, nidjs = "" } ]
            }

        model =
            { route = route
            , asked_orga = orga_name
            , orga_data = Loading
            , circle_tensions = Loading
            , node_focus = focus
            , node_action = NotAsk
            }
    in
    ( model
      --, Cmd.batch
      --    [ Http.get { url = "/data/" ++ model.asked_orga ++ ".json", expect = Http.expectString GotText }
      --    , Http.get { url = "/data/tensions1.json", expect = Http.expectJson GotTensions tensionsDecoder }
      --    ]
    , Cmd.batch
        [ fetchNodesOrga model.asked_orga GotOrga
        , fetchTensionsBunch GotTensions
        , Task.perform (\_ -> PassedSlowLoadTreshold) Loading.slowTreshold
        ]
    , Cmd.none
    )



--
-- UPDATE
--


type Msg
    = GotOrga (RequestResult ErrorData NodesData) -- graphql
    | GotTensions (RequestResult ErrorData TensionsData) -- graphql
    | DoNodeAction NodeFragment -- ports receive
    | NodeClick NodeFocusState -- ports receive
    | ChangeNodeFocus Int -- ports send
    | ToggleGraphReverse -- ports send
    | ToggleTooltips -- ports send -- Not implemented @DEBUG multiple tooltip/ see name of circle
    | DoTensionStep
    | PassedSlowLoadTreshold -- timer


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    case msg of
        GotOrga result ->
            case result of
                Success data ->
                    ( { model | orga_data = Loaded data }
                    , Cmd.none
                    , Ports.init_circlePacking <| JE.encode 0 <| nodesEncoder data
                    )

                Failure err ->
                    ( { model | orga_data = Failed err }
                    , Cmd.none
                    , Cmd.none
                    )

                RemoteLoading ->
                    ( { model | orga_data = Loading }
                    , Cmd.none
                    , Cmd.none
                    )

                NotAsked ->
                    ( model, Cmd.none, Cmd.none )

        GotTensions result ->
            case result of
                Success data ->
                    ( { model | circle_tensions = Loaded data }
                    , Cmd.none
                    , Cmd.none
                    )

                Failure err ->
                    ( { model | circle_tensions = Failed err }
                    , Cmd.none
                    , Cmd.none
                    )

                RemoteLoading ->
                    ( { model | circle_tensions = Loading }
                    , Cmd.none
                    , Cmd.none
                    )

                NotAsked ->
                    ( model, Cmd.none, Cmd.none )

        DoNodeAction res ->
            let
                nodeAction =
                    case res of
                        Ok node ->
                            Ask FirstStep node

                        Err err ->
                            AskErr <| JD.errorToString err
            in
            ( { model | node_action = nodeAction }, Cmd.none, Cmd.none )

        NodeClick focus ->
            ( { model | node_focus = focus }
            , Cmd.none
            , Cmd.none
            )

        ChangeNodeFocus pos ->
            let
                nidjs =
                    case Array.get pos model.node_focus.path of
                        Just x ->
                            x.nidjs

                        Nothing ->
                            ""
            in
            ( model, sendNodeFocus nidjs, Cmd.none )

        ToggleGraphReverse ->
            ( model, () |> sendToggleGraphReverse, Cmd.none )

        ToggleTooltips ->
            ( model, () |> sendToggleTooltips, Cmd.none )

        DoTensionStep ->
            let
                newAction =
                    case model.node_action of
                        Ask step node ->
                            Ask TensionStep node

                        default ->
                            default
            in
            ( { model | node_action = newAction }, Cmd.none, Ports.bulma_driver )

        PassedSlowLoadTreshold ->
            let
                orga_data =
                    case model.orga_data of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other
            in
            ( { model | orga_data = orga_data }
            , Cmd.none
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ nodeFocusFromJs NodeClick
        , nodeDataFromJs DoNodeAction
        ]



-- Receive to Javascript


port nodeFocusFromJs : (NodeFocusState -> msg) -> Sub msg


port rawNodeDataFromJs : (JD.Value -> a) -> Sub a


nodeDataFromJs : (NodeFragment -> msg) -> Sub msg
nodeDataFromJs rawNode =
    rawNodeDataFromJs (rawNode << JD.decodeValue nodeDecoder)


port sendNodeFocus : NID -> Cmd msg


port sendToggleGraphReverse : () -> Cmd msg


port sendToggleTooltips : () -> Cmd msg



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "columns" ]
        [ div [ class "column is-1 is-fullheight is-hidden-mobile", id "leftPane" ]
            [ viewLeftPane model ]
        , div [ class "column is-10", id "mainPane" ]
            [ div [ class "columns" ]
                [ viewHelperBar model ]
            , div [ class "columns is-variable is-4" ]
                [ div [ class "column is-6" ]
                    [ viewCanvas model
                    , br [] []
                    , viewMandate model
                    , setupActionModal model
                    ]
                , div [ class "column is-6" ]
                    [ div [ class "columns is-gapless" ]
                        [ div [ class "column is-11", id "nextToChart" ]
                            [ viewActivies model ]
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
                [ div [ class "hero-body has-text-centered" ] [ text model.asked_orga ] ]
            ]
        , ul [ class "menu-list" ]
            [ li [ class "menu-label" ]
                [ div [ class "hero is-small is-info is-bold" ]
                    [ div [ class "hero-body" ]
                        [ Fa.icon "far fa-circle fa-lg" model.node_focus.name ]
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


viewHelperBar : Model -> Html Msg
viewHelperBar model =
    nav
        [ class "column is-full breadcrumb"
        , attribute "aria-label" "breadcrumbs"
        ]
        [ Fa.icon "fas fa-angle-right" ""
        , Array.indexedMap
            (\i x ->
                if i < (Array.length model.node_focus.path - 1) then
                    li [] [ a [ href "#", onClick (ChangeNodeFocus i) ] [ text x.name ] ]

                else
                    li [ class "is-active has-text-weight-semibold" ]
                        [ a [ attribute "aria-current" "page", href "#" ] [ text x.name ] ]
            )
            model.node_focus.path
            |> Array.toList
            |> ul [ attribute "style" "display: inline-flex;" ]
        ]


viewCanvas : Model -> Html Msg
viewCanvas model =
    div []
        [ div [ id "canvasParent" ] [ showMaybeError model.orga_data ]

        -- Hidden class use in circlepacking_d3.js
        , div [ id "canvasButtons", class "buttons are-small is-invisible" ]
            [ div
                [ id "inv_cvbtn"
                , class "button buttonToggle tooltip has-tooltip-right"
                , attribute "data-tooltip" "Reverse the organisation graph."
                , onClick ToggleGraphReverse
                ]
                [ Fa.icon0 "fas fa-sort-amount-up" "" ]

            --, div
            --    [ id "label_cvbtn"
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


viewMandate : Model -> Html Msg
viewMandate model =
    div [ class "hero is-small is-light heroViewer box" ]
        [ div [ class "hero-body" ]
            [ h1 [ class "title is-3" ]
                [ Fa.icon "fas fa-scroll fa-xs" "Mandate" ]
            , hr [ class "has-background-grey-light" ] []
            , div [ class "content" ]
                [ h2 [ class "title is-4" ] [ text "Purpose" ]
                , div [] [ text "Helping people and human organisations to find resillient, efficient and anti alienating models and praxis for self organisation." ]
                , h2 [ class "title is-4" ] [ text "Responsabilities" ]
                , div []
                    [ ul []
                        [ li [] [ text "Develop, maintains and push forward Fractal6." ]
                        , li [] [ text "Find a business model for fractal6." ]
                        ]
                    ]
                , h2 [ class "title is-4" ] [ text "Domains" ]
                , div [] [ text "See sub domains." ]
                ]
            ]
        ]


viewActivies : Model -> Html Msg
viewActivies model =
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
        , div [ class "content" ]
            [ case model.circle_tensions of
                Loaded tensions ->
                    List.map (\t -> vTension t) tensions
                        |> div [ class "is-size-7", id "tensionsTab" ]

                -- why it doesnt work?
                other ->
                    [ showMaybeError other ]
                        |> div []
            ]

        --, a [ class "Footer has-text-centered" ] [ text "See more" ]
        --]
        ]


vTension : Tension -> Html Msg
vTension tension =
    div [ class "media Box" ]
        [ div [ class "media-left" ]
            [ div
                [ class "tooltip has-tooltip-top"
                , attribute "data-tooltip" ("type: " ++ TensionType.toString tension.type_)
                ]
                [ case tension.type_ of
                    TensionType.Personal ->
                        div [ class "Circle has-text-danger" ] [ text "" ]

                    TensionType.Governance ->
                        div [ class "Circle has-text-info" ] [ text "" ]

                    TensionType.Operational ->
                        div [ class "Circle has-text-warning" ] [ text "" ]

                    TensionType.Help ->
                        div [ class "Circle has-text-success" ] [ text "" ]

                    TensionType.Alert ->
                        div [ class "Circle has-text-alert" ] [ text "" ]
                ]
            ]
        , div [ class "media-content" ]
            [ div [ class "content" ]
                [ div [ class "has-text-weight-semibold" ]
                    [ text tension.title ]
                ]
            , div [ class "labelsList" ]
                (case tension.labels of
                    Just labels ->
                        List.map
                            (\label ->
                                span [ class "tag" ] [ text label.name ]
                            )
                            labels

                    Nothing ->
                        []
                )
            ]
        , div
            [ class "media-right" ]
            [ let
                n_comments =
                    case tension.n_comments of
                        Just n ->
                            n

                        Nothing ->
                            0
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


setupActionModal : Model -> Html Msg
setupActionModal model =
    div [ class "container is-clipped" ]
        [ div [ id "actionModal", class "modal" ]
            [ div [ class "modal-background" ] []
            , div [ class "modal-content" ]
                [ case model.node_action of
                    Ask step node ->
                        viewActionStep step node

                    NotAsk ->
                        text ""

                    AskErr err ->
                        div [ classList [ ( "box", True ), ( "has-background-danger", True ) ] ]
                            [ text <| "Unexpected error:" ++ err ]
                ]
            , button [ class "modal-close is-large" ] []
            ]
        ]


viewActionStep : ActionStep -> Node -> Html Msg
viewActionStep step node =
    case step of
        FirstStep ->
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
                            [ div [ class "level-item" ] [ div [ class "button", onClick DoTensionStep ] [ text "New tension" ] ]
                            , div [ class "level-item" ] [ div [ class "button" ] [ text "New Sub-Circle" ] ]
                            , div [ class "level-item" ] [ div [ class "button" ] [ text "New Role" ] ]
                            ]

                        else
                            [ div [ class "level-item" ] [ div [ class "button", onClick DoTensionStep ] [ text "New tension" ] ] ]
                    ]
                ]

        TensionStep ->
            div [ class "card" ]
                [ div [ class "card-header" ]
                    [ div [ class "card-header-title" ] <|
                        List.intersperse (text "\u{00A0}")
                            [ span [ class "has-text-weight-medium" ] [ text "Choose the type of tension to communicate" ]
                            , text ":"
                            ]
                    ]
                , div [ class "card-content" ]
                    [ div [ class "level buttonRadio" ]
                      -- @DEBUG: bulma driver won't works since element doesnt exist at creation!
                      <|
                        List.map
                            (\tensionType ->
                                div [ class "level-item" ]
                                    [ div
                                        [ class "button"

                                        --, onClick (DoTensionFormStep tensionType)
                                        ]
                                        [ text (TensionType.toString tensionType) ]
                                    ]
                            )
                        <|
                            TensionType.list
                    ]
                ]
