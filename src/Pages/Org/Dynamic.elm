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
import Html.Attributes exposing (attribute, class, href, id)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD exposing (Decoder, field, int, string)
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
    , circle_focus : CircleFocusState
    }



{-
   Session Data
-}


type alias CircleFocusState =
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
            , circle_focus = focus
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
    | CircleClick CircleFocusState -- Ports receive
    | ChangeNodeFocus Int -- Ports send
    | ToggleGraphReverse -- Ports send
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

        CircleClick focus ->
            ( { model | circle_focus = focus }
            , Cmd.none
            , Cmd.none
            )

        ChangeNodeFocus pos ->
            let
                nidjs =
                    case Array.get pos model.circle_focus.path of
                        Just x ->
                            x.nidjs

                        Nothing ->
                            ""
            in
            ( model, sendNodeFocus nidjs, Cmd.none )

        ToggleGraphReverse ->
            ( model, () |> sendToggleGraphReverse, Cmd.none )

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
    receiveData CircleClick



-- Receive to Javascript


port receiveData : (CircleFocusState -> msg) -> Sub msg



-- Send to Javascript


port sendNodeFocus : NID -> Cmd msg


port sendToggleGraphReverse : () -> Cmd msg



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
                        [ Fa.icon1 "far fa-circle fa-lg" model.circle_focus.name ]
                    ]
                ]
            , li []
                [ ul [ class "menu-list" ]
                    [ li []
                        [ a []
                            [ Fa.icon1 "fas fa-scroll fa-xs" "Mandates" ]
                        ]
                    , li []
                        [ a []
                            --  fa-exclamation-circle
                            [ Fa.icon1 "fas fa-exchange-alt fa-xs" "Tensions" ]
                        ]
                    , li []
                        [ a []
                            [ Fa.icon1 "fas fa-history fa-xs" "Journal" ]
                        ]
                    , li []
                        [ a []
                            [ Fa.icon1 "fas fa-user fa-xs" "Members" ]
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
        [ Fa.icon1 "fas fa-angle-right" ""
        , Array.indexedMap
            (\i x ->
                if i < (Array.length model.circle_focus.path - 1) then
                    li [] [ a [ href "#", onClick (ChangeNodeFocus i) ] [ text x.name ] ]

                else
                    li [ class "is-active has-text-weight-semibold" ]
                        [ a [ attribute "aria-current" "page", href "#" ] [ text x.name ] ]
            )
            model.circle_focus.path
            |> Array.toList
            |> ul [ attribute "style" "display: inline-flex;" ]
        ]


viewMandate : Model -> Html Msg
viewMandate model =
    div [ class "hero is-small is-light heroViewer box" ]
        [ div [ class "hero-body" ]
            [ h1 [ class "title is-3" ]
                [ Fa.icon1 "fas fa-scroll fa-xs" "Mandate" ]
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


viewCanvas : Model -> Html Msg
viewCanvas model =
    div []
        [ div [ id "canvasParent" ] [ showMaybeError model.orga_data ]
        , div [ id "canvasButtons", class "buttons are-small is-hidden " ]
            [ div
                [ id "inv_cvbtn"
                , class "button tooltip has-tooltip-left "
                , attribute "data-tooltip" "Reverse the organisation graph."
                , onClick ToggleGraphReverse
                ]
                [ Fa.icon "fas fa-sort-amount-up" "" ]
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
                            [ Fa.icon1 "fas fa-exchange-alt fa-sm" "Tensions" ]
                        ]
                    , li []
                        [ a [ class "is-" ]
                            [ Fa.icon1 "fas fa-history fa-sm" "Journal" ]
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
                [ class "tooltip has-tooltip-top has-tooltip-light"
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
                    [ class "tooltip has-tooltip-top has-tooltip-light"
                    , attribute "data-tooltip" ("comments: " ++ String.fromInt n_comments)
                    ]
                    [ Fa.icon "fas fa-comment-dots" (String.fromInt n_comments)
                    ]

              else
                text ""
            ]
        ]
