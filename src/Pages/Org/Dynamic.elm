module Pages.Org.Dynamic exposing (Model, Msg, page)

import Dict exposing (Dict)
import Generated.Org.Params as Params
import Global exposing (NID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Ports
import Spa.Page
import Utils.Spa exposing (Page)


page : Page Params.Dynamic Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = \{ model } -> String.join " | " [ model.asked_orga ]
        , init = always init
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


type alias Model =
    { asked_orga : String
    , data : String
    , isLoading : Bool
    }



-- INIT


init : Params.Dynamic -> ( Model, Cmd Msg, Cmd Global.Msg )
init params =
    --let
    --    data_json =
    --        "{\"name\" :\"occupation\",\"type\":\"circle\", \"children\" : [{\"name\": \"gr1\", \"type\":\"circle\", \"children\" : [{\"name\":\"azdad\",\"type\":\"role\", \"ID\":1, \"size\":20},{\"name\":\"qfqsf\",\"type\":\"role\", \"ID\":2, \"size\":25}]}]}"
    --in
    ( { asked_orga = params.param1, data = "", isLoading = True }
    , Http.get { url = "/data1.json", expect = Http.expectString GotText }
    , Cmd.none
      --Ports.init_circlePacking data_json
    )



-- UPDATE


type Msg
    = GotText (Result Http.Error String)


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "columns" ]
        [ div [ class "column is-2" ] []
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
