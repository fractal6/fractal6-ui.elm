port module Pages.Org.Dynamic exposing (Model, Msg, page)

import Array
import Dict exposing (Dict)
import Generated.Org.Params as Params
import Generated.Routes exposing (Route)
import Global exposing (NID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Encode exposing (string)
import Ports
import Spa.Page
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
{-
   type alias Organisation =
       { graph : OrgaGraph }


   type alias OrgaGraph =
       Dict NID Node


   type NodeType
       = Circle
       | Role


   type alias Node =
       -- A node is either a circle or a role
       { display_name : String
       , children : List NID
       , parent : NID
       , ispublic : Bool
       , ntype : NodeType
       , root : Bool
       }


   type alias Model =
       { asked_orga : String
       , orga : Maybe Organisation
       }
-}


type alias CircleFocusState =
    { nidjs : NID
    , name : String
    , nodeType : String
    , path : Array.Array { name : String, nidjs : NID }
    }


type alias Model =
    { asked_orga : String
    , data : String
    , circle_focus : CircleFocusState
    , isLoading : Bool
    , route : Route
    }



-- INIT


init : PageContext -> Params.Dynamic -> ( Model, Cmd Msg, Cmd Global.Msg )
init { route } params =
    let
        focus =
            { nidjs = "545687"
            , name = "Sku Root"
            , nodeType = "Circle"
            , path = Array.fromList [ { name = "root", nidjs = "" } ]
            }

        model =
            { asked_orga = params.param1
            , data = ""
            , circle_focus = focus
            , isLoading = True
            , route = route
            }
    in
    ( model
    , Http.get { url = "/data1.json", expect = Http.expectString GotText }
    , Cmd.none
      --Ports.init_circlePacking data_json
    )



-- UPDATE


type Msg
    = GotText (Result Http.Error String)
    | CircleClick CircleFocusState
    | ChangeNodeFocus Int


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok data ->
                    ( model
                    , Cmd.none
                    , Ports.init_circlePacking data
                    )

                Err _ ->
                    ( model
                    , Cmd.none
                    , Cmd.none
                    )

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
                    [ div [ id "chart" ] []
                    , br [] []
                    , viewMandate model
                    ]
                , div [ class "column is-5", attribute "style" "width: 44%;", id "nextToChart" ]
                    [ viewActivies model ]
                ]
            ]
        ]


viewHelperBar : Model -> Html Msg
viewHelperBar model =
    nav
        [ class "column is-full breadcrumb has-succeeds-separator"
        , attribute "aria-label" "breadcrumbs"
        ]
        [ i [ class "fas fa-angle-right" ] [ text "\u{00A0} " ]
        , Array.indexedMap
            (\i x ->
                if i == (Array.length model.circle_focus.path - 1) then
                    li [ class "is-active" ] [ a [ attribute "aria-current" "page", href "#" ] [ text x.name ] ]

                else
                    li [] [ a [ href "#", onClick (ChangeNodeFocus i) ] [ text x.name ] ]
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
                [ i [ class "fas fa-scroll fa-xs" ] []
                , text ("\u{00A0} " ++ "Mandate")
                ]
            , hr [ class "has-background-grey-light" ] []
            , div [ class "content" ]
                [ h2 [ class "title is-4" ] [ text "Purpose" ]
                , div [] [ text "Devellop fractal6 and find a business model." ]
                , h2 [ class "title is-4" ] [ text "Responsabilities" ]
                , div [] [ text "Propose new feature, fix security bugs." ]
                , h2 [ class "title is-4" ] [ text "Domains" ]
                , div [] [ text "the code, the platform." ]
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
                            [ i [ class "fas fa-exchange-alt fa-sm" ] []
                            , text ("\u{00A0} " ++ "Tensions")
                            ]
                        ]
                    , li []
                        [ a [ class "is-" ]
                            [ i [ class "fas fa-history fa-sm" ] []
                            , text ("\u{00A0} " ++ "Journal")
                            ]
                        ]
                    ]
                ]
            ]
        , div [ class "content" ] [ text "Salut" ]
        , a [] [ text "See more" ]
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
                        [ i [ class "far fa-circle fa-lg" ] []
                        , text ("\u{00A0} " ++ model.circle_focus.name)
                        ]
                    ]
                ]
            , li []
                [ ul [ class "menu-list" ]
                    [ li []
                        [ a []
                            [ i [ class "fas fa-scroll fa-xs" ] []
                            , text ("\u{00A0} " ++ "Mandates")
                            ]
                        ]
                    , li []
                        [ a []
                            [ i [ class "fas fa-exchange-alt fa-xs" ] []

                            --[ i [ class "fas fa-exclamation-circle fa-fw" ] []
                            , text ("\u{00A0} " ++ "Tensions")
                            ]
                        ]
                    , li []
                        [ a []
                            [ i [ class "fas fa-history fa-xs" ] []
                            , text ("\u{00A0} " ++ "Journal")
                            ]
                        ]
                    , li []
                        [ a []
                            [ i [ class "fas fa-user fa-xs" ] []
                            , text ("\u{00A0} " ++ "Members")
                            ]
                        ]
                    ]
                ]
            ]
        ]
