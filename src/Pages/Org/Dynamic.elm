port module Pages.Org.Dynamic exposing (Model, Msg, page)

import Dict exposing (Dict)
import Generated.Org.Params as Params
import Generated.Routes exposing (Route)
import Global exposing (NID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
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
    { nid : NID
    , name : String
    , nodeType : String
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
            { nid = "545687"
            , name = "Sku Root"
            , nodeType = "Circle"
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveData CircleClick


port receiveData : (CircleFocusState -> msg) -> Sub msg



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "columns" ]
        [ div [ id "leftPane", class "column is-2" ] [ viewLeftPane model ]
        , div [ class "column is-10" ]
            [ div [ class "columns" ]
                [ div [ class "column is-1" ] []
                , div [ class "column is-7" ]
                    [ nav
                        [ class "breadcrumb"
                        , attribute "aria-label" "breadcrumbs"
                        ]
                        [ ul []
                            [ li []
                                [ a [ href "../" ] [ text "Bulma" ]
                                ]
                            , li []
                                [ a [ href "../" ] [ text "Templates" ]
                                ]
                            , li []
                                [ a [ href "../" ] [ text "Examples" ]
                                ]
                            , li [ class "is-active" ]
                                [ a [ attribute "aria-current" "page", href "#" ] [ text "Admin" ]
                                ]
                            ]
                        ]
                    , div [ id "chart" ] []
                    ]
                ]
            ]
        ]


viewLeftPane : Model -> Html msg
viewLeftPane model =
    nav [ class "menu is-hidden-mobile" ]
        [ p [ class "menu-label" ]
            [ div [ class "hero is-small is-primary is-bold" ]
                [ div [ class "hero-body has-text-centered" ] [ text model.asked_orga ] ]
            ]
        , ul [ class "menu-list" ]
            [ li [ class "menu-label" ]
                [ div [ class "hero is-small is-info is-bold" ]
                    [ div [ class "hero-body" ] [ text model.circle_focus.name ] ]
                ]
            , li []
                [ ul [ class "menu-list" ]
                    [ li []
                        [ a [ class "is-active" ]
                            [ text "Mandate" ]
                        ]
                    , li []
                        [ a [] [ text "Tensions" ] ]
                    , li []
                        [ a [] [ text "Journal" ] ]
                    , li []
                        [ a [] [ text "Members" ] ]
                    ]
                ]
            ]
        ]
