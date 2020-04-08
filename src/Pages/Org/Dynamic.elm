port module Pages.Org.Dynamic exposing (Model, Msg, page)

import Array
import Components.Fa as Fa
import Components.Loading as Loading exposing (Status(..), showWhatsup)
import Dict exposing (Dict)
import Fractal.Enum.TensionType as TensionEnum exposing (TensionType, toString)
import Fractal.InputObject
import Fractal.Object
import Fractal.Object.Label
import Fractal.Object.Tension
import Fractal.Query as Q
import Fractal.ScalarCodecs
import Generated.Org.Params as Params
import Generated.Routes exposing (Route)
import Global exposing (NID)
import GqlClient exposing (GQLResponse, decodeGQLResponse, makeGQLMutation, makeGQLQuery)
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD exposing (Decoder, field, int, string)
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



-- Model


type alias CircleFocusState =
    { nidjs : NID
    , name : String
    , nodeType : String
    , path : Array.Array { name : String, nidjs : NID }
    }


type alias OrgaGraph =
    String


type alias Label =
    { name : String }


type alias Tension =
    { id : Fractal.ScalarCodecs.Id
    , title : String
    , type_ : TensionType
    , labels : Maybe (List Label)

    --, labels : (\opts -> Fractal.Object.Tension.LabelsOptionalArguments) (Fractal.Object.Tension Fractal.Object.Label)
    , n_comments : Maybe Int

    --, emitter : String
    --, receivers : String
    }


type alias Model =
    { route : Route
    , asked_orga : String

    -- Loaded indepedently from server
    , circle_focus : CircleFocusState
    , orga_data : Status OrgaGraph
    , circle_tensions : Status (List Tension)
    }



-- GraphQL decoder


tensionOverviewQ : SelectionSet Tension Fractal.Object.Tension
tensionOverviewQ =
    SelectionSet.succeed Tension
        |> with Fractal.Object.Tension.id
        |> with Fractal.Object.Tension.title
        |> with Fractal.Object.Tension.type_
        |> with
            (Fractal.Object.Tension.labels
                labelFilter
                (SelectionSet.succeed Label
                    |> with Fractal.Object.Label.name
                )
            )
        |> with Fractal.Object.Tension.n_comments


tensionsPageArg : Q.QueryTensionOptionalArguments -> Q.QueryTensionOptionalArguments
tensionsPageArg args =
    args


labelFilter : Fractal.Object.Tension.LabelsOptionalArguments -> Fractal.Object.Tension.LabelsOptionalArguments
labelFilter args =
    { args | first = OptionalArgument.Present 3 }


fetchTensionsPage : Cmd Msg
fetchTensionsPage =
    makeGQLQuery
        (Q.queryTension tensionsPageArg tensionOverviewQ)
        (RemoteData.fromResult >> TensionsSuccess)



{-
   Schema Type Utils (should be auto generated !)
-}


type alias TensionsData =
    Maybe (List (Maybe Tension))


type alias TensionsResult =
    RemoteData (Graphql.Http.Error TensionsData) TensionsData



-- HTTP and Json Decoder
--
-- tsDecoder : Decoder Tension
-- tsDecoder =
--     JD.map7
--         Tension
--         (field "title" string)
--         (field "type_" string)
--         (field "emitter" string)
--         (field "receivers" string)
--         (field "n_comments" int)
--
--
-- tensionsDecoder : Decoder Tensions
-- tensionsDecoder =
--     JD.list tsDecoder
-- INIT


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
            , circle_focus = focus
            , circle_tensions = Loading
            }
    in
    ( model
      --, Cmd.batch
      --    [ Http.get { url = "/data/" ++ model.asked_orga ++ ".json", expect = Http.expectString GotText }
      --    , Http.get { url = "/data/tensions1.json", expect = Http.expectJson GotTensions tensionsDecoder }
      --    ]
    , Cmd.batch
        [ fetchTensionsPage
        , Task.perform (\_ -> PassedSlowLoadTreshold) Loading.slowTreshold
        ]
    , Cmd.none
    )



-- UPDATE


type Msg
    = --GotText (Result Http.Error String)
      --| GotTensions (Result Http.Error Tensions)
      TensionsSuccess TensionsResult
    | CircleClick CircleFocusState
    | ChangeNodeFocus Int
    | PassedSlowLoadTreshold


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    case msg of
        --GotText result ->
        --    case result of
        --        Ok data ->
        --            ( { model | orga_data = Loaded data }
        --            , Cmd.none
        --            , Ports.init_circlePacking data
        --            )
        --        Err _ ->
        --            ( { model | orga_data = Failed }
        --            , Cmd.none
        --            , Cmd.none
        --            )
        --GotTensions result ->
        --    case result of
        --        Ok data ->
        --            ( { model | circle_tensions = Loaded data }
        --            , Cmd.none
        --            , Cmd.none
        --            )
        --        Err errmsg ->
        --            let
        --                c =
        --                    Debug.log "dede" errmsg
        --            in
        --            ( { model | circle_tensions = Failed }
        --            , Cmd.none
        --            , Cmd.none
        --            )
        TensionsSuccess result ->
            case result of
                RemoteData.Success data ->
                    let
                        decodedData =
                            decodeGQLResponse data
                    in
                    ( { model | circle_tensions = Loaded decodedData }
                    , Cmd.none
                    , Cmd.none
                    )

                --RemoteData.Failure err ->
                --    ( model, Cmd.none, Cmd.none )
                --RemoteData.Loading ->
                --    ( model, Cmd.none, Cmd.none )
                --RemoteData.NotAsked ->
                --    ( model, Cmd.none, Cmd.none )
                _ ->
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


port receiveData : (CircleFocusState -> msg) -> Sub msg


port sendNodeFocus : NID -> Cmd msg



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
                    [ div [ id "chart" ] [ showWhatsup (text "") model.orga_data ]
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
                            [ Fa.icon1 "fas fa-exchange-alt fa-xs" "Tensions" ]

                        --  fa-exclamation-circle
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
        [ class "column is-full breadcrumb has-succeeds-separator"
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


viewMandate : Model -> Html mgs
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
                    List.map (\t -> mTension t) tensions
                        |> div [ class "is-size-7", id "tensionsTab" ]

                -- why it doesnt work?
                other ->
                    [ showWhatsup (text "") other ]
                        |> div []
            ]

        --, a [ class "Footer has-text-centered" ] [ text "See more" ]
        --]
        ]


mTension : Tension -> Html msg
mTension tension =
    div [ class "media Box" ]
        [ div [ class "media-left" ]
            [ div
                [ class "tooltip has-tooltip-top has-tooltip-light"
                , attribute "data-tooltip" ("type: " ++ TensionEnum.toString tension.type_)
                ]
                [ case tension.type_ of
                    TensionEnum.Personal ->
                        div [ class "Circle has-text-danger" ] [ text "" ]

                    TensionEnum.Governance ->
                        div [ class "Circle has-text-info" ] [ text "" ]

                    TensionEnum.Operational ->
                        div [ class "Circle has-text-warning" ] [ text "" ]

                    TensionEnum.Help ->
                        div [ class "Circle has-text-success" ] [ text "" ]

                    TensionEnum.Alert ->
                        div [ class "Circle has-text-alert" ] [ text "" ]
                ]
            ]
        , div [ class "media-content" ]
            [ div [ class "" ]
                [ div [ class "has-text-weight-semibold" ]
                    [ text tension.title ]
                ]
            ]
        , div [ class "media-right" ]
            [ div [] []

            --[ class "tooltip has-tooltip-top has-tooltip-light"
            --, attribute "data-tooltip" ("severity: " ++ String.fromInt tension.severity)
            --]
            --[ Fa.icon_ "fas fa-fire" (String.fromInt tension.severity)
            --]
            , case tension.n_comments of
                Just n_comments ->
                    div
                        [ class "tooltip has-tooltip-top has-tooltip-light"
                        , attribute "data-tooltip" ("comments: " ++ String.fromInt n_comments)
                        ]
                        [ Fa.icon "fas fa-comment-dots" (String.fromInt n_comments)
                        ]

                Nothing ->
                    text ""
            ]
        ]
