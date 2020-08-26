module Pages.Explore exposing (Flags, Model, Msg, page)

import Components.Fa as Fa
import Components.HelperBar as HelperBar
import Components.Loading as Loading exposing (viewAuthNeeded, viewGqlErrors, viewHttpErrors)
import Components.Text as Text exposing (..)
import Date exposing (formatTime)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Fractal.Enum.NodeType as NodeType
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, type_)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Uri exposing (FractalBaseRoute(..), uriFromNameid)
import ModelCommon.View exposing (getAvatar)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.QueryNode exposing (NodeExt, queryPublicOrga)
import Task
import Time



---- PROGRAM ----


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Flags =
    ()



---- MODEL----


type alias Model =
    { orgas : GqlData (List Node)
    }


type alias Node =
    NodeExt



---- MSG ----


type Msg
    = PassedSlowLoadTreshold -- timer
    | GotOrga (GqlData (List Node))



-- INIT --


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        apis =
            global.session.apis

        model =
            { orgas = Loading
            }

        cmds =
            [ queryPublicOrga apis.gql GotOrga
            , sendSleep PassedSlowLoadTreshold 500
            ]
    in
    ( model
    , Cmd.batch cmds
    , Cmd.none
    )



---- UPDATE ----


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global msg model =
    case msg of
        PassedSlowLoadTreshold ->
            let
                orgas =
                    ternary (model.orgas == Loading) LoadingSlowly model.orgas
            in
            ( { model | orgas = orgas }, Cmd.none, Cmd.none )

        -- Gql queries
        GotOrga result ->
            ( { model | orgas = result }, Cmd.none, Cmd.none )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.none



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Explore"
    , body = [ view_ global model ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ id "explore", class "columns" ]
        [ div [ class "column is-offset-2 is-6" ]
            [ div [ class "section" ]
                [ viewOrgas model ]
            ]
        ]


viewOrgas : Model -> Html Msg
viewOrgas model =
    div [ class "section" ] <|
        case model.orgas of
            Loading ->
                [ div [] [] ]

            NotAsked ->
                [ div [] [] ]

            LoadingSlowly ->
                [ div [ class "spinner" ] [] ]

            Failure err ->
                [ viewGqlErrors err ]

            Success nodes ->
                nodes
                    |> List.map (\n -> viewOrgaMedia n)
                    |> List.append [ div [ class "subtitle" ] [ text "Public Organisation" ], br [] [] ]


viewOrgaMedia : Node -> Html Msg
viewOrgaMedia node =
    let
        n_member =
            node.stats |> Maybe.map (\s -> s.n_member |> withDefault 0) |> withDefault 0 |> String.fromInt

        n_guest =
            node.stats |> Maybe.map (\s -> s.n_guest |> withDefault 0) |> withDefault 0 |> String.fromInt
    in
    div [ class "media" ]
        [ div [ class "media-left" ] [ a [ class "image circleBase circle2", href (uriFromNameid OverviewBaseUri node.nameid) ] [ getAvatar node.name ] ]
        , div [ class "media-content" ]
            [ div [ class "columns" ]
                [ div [ class "column is-10" ]
                    [ a [ href (uriFromNameid OverviewBaseUri node.nameid) ] [ text node.name ]
                    , case node.about of
                        Just ab ->
                            div [ class "is-italic" ] [ text ab ]

                        Nothing ->
                            div [ class "is-italic" ] [ text "" ]
                    ]
                , span [ class "column is-2" ]
                    [ div [ class "level levelExplore" ]
                        [ div [ class "level-item" ]
                            [ span [ class "tags has-addons" ]
                                [ span [ class "tag is-light" ] [ text "member" ], span [ class "tag is-white" ] [ text n_member ] ]
                            ]
                        , div [ class "level-item" ]
                            [ span [ class "tags has-addons" ]
                                [ span [ class "tag is-light" ] [ text "guest" ], span [ class "tag is-white" ] [ text n_guest ] ]
                            ]
                        ]
                    ]
                ]
            ]

        --, div [ class "media-right" ]
        --    []
        ]
