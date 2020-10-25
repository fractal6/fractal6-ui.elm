module Pages.Explore exposing (Flags, Model, Msg, page)

import Components.Fa as Fa
import Components.HelperBar as HelperBar
import Components.Loading as Loading exposing (GqlData, RequestResult(..), viewAuthNeeded, viewGqlErrors, viewHttpErrors)
import Components.Text as T
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
import ModelCommon.Codecs exposing (FractalBaseRoute(..), uriFromNameid)
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
    div [ id "explore", class "section" ]
        [ div [ class "columns is-centered" ]
            [ div [ class "column is-7" ]
                [ viewPublicOrgas model
                ]
            ]
        ]


viewPublicOrgas : Model -> Html Msg
viewPublicOrgas model =
    div [ class "" ] <|
        case model.orgas of
            Loading ->
                [ text "" ]

            NotAsked ->
                [ text "" ]

            LoadingSlowly ->
                [ div [ class "spinner" ] [] ]

            Failure err ->
                [ viewGqlErrors err ]

            Success nodes ->
                nodes
                    |> List.map (\n -> viewOrgaMedia n)
                    |> List.append [ div [ class "subtitle" ] [ text T.exploreOrganisations ], br [] [] ]


viewOrgaMedia : Node -> Html Msg
viewOrgaMedia node =
    let
        n_member =
            node.stats |> Maybe.map (\s -> s.n_member |> withDefault 0) |> withDefault 0 |> String.fromInt

        n_guest =
            node.stats |> Maybe.map (\s -> s.n_guest |> withDefault 0) |> withDefault 0 |> String.fromInt
    in
    div [ class "media box nodesList" ]
        [ div [ class "media-left" ]
            [ a
                [ class "image circleBase circle2"
                , href (uriFromNameid OverviewBaseUri node.nameid)
                ]
                [ getAvatar node.name ]
            ]
        , div [ class "media-content" ]
            [ div [ class "columns" ]
                [ div [ class "column is-8" ]
                    [ a [ href (uriFromNameid OverviewBaseUri node.nameid) ] [ text node.name ]
                    , case node.about of
                        Just ab ->
                            p [ class "is-italic pt-1" ] [ text ab ]

                        Nothing ->
                            text ""
                    ]
                , span [ class "column is-4" ]
                    [ div [ class "field is-grouped is-grouped-multiline is-pulled-right" ]
                        [ div [ class "control" ]
                            [ div [ class "tags has-addons" ]
                                [ span [ class "tag is-light" ] [ text "member" ]
                                , span [ class "tag is-white" ] [ text n_member ]
                                ]
                            ]
                        , div [ class "control" ]
                            [ div [ class "tags has-addons" ]
                                [ span [ class "tag is-light" ] [ text "guest" ]
                                , span [ class "tag is-white" ] [ text n_guest ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
