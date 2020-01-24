module Pages.Human.Dynamic exposing (Model, Msg, page)

import Generated.Human.Params as Params
import Global
import Html exposing (..)
import Html.Attributes exposing (..)
import Ports
import Spa.Page
import Utils.Spa exposing (Page)


page : Page Params.Dynamic Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = \{ model } -> String.join " | " [ model.asked_user ]
        , init = always init
        , update = always update
        , subscriptions = always subscriptions
        , view = always view
        }



-- INIT


type alias Model =
    { asked_user : String }


init : Params.Dynamic -> ( Model, Cmd Msg, Cmd Global.Msg )
init params =
    ( { asked_user = params.param1 }
    , Cmd.none
    , Ports.bulma_driver
    )



-- UPDATE


type Msg
    = Msg


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
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
    div []
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
        , div [ class "hero is-info is-small" ]
            [ div [ class "hero-body" ]
                [ div [ class "container" ]
                    [ h1 [ class "title" ]
                        [ text (String.concat [ "Hello, ", model.asked_user, "." ]) ]
                    , h2 [ class "subtitle" ]
                        [ text "I hope you are having a great day!" ]
                    ]
                ]
            ]
        , div [ class "tile is-ancestor has-text-centered" ]
            [ div [ class "tile is-parent" ]
                [ article [ class "tile is-child box" ]
                    [ p [ class "title" ] [ text "439k" ]
                    , p [ class "subtitle" ] [ text "Users" ]
                    ]
                ]
            , div [ class "tile is-parent" ]
                [ article [ class "tile is-child box" ]
                    [ p [ class "title" ] [ text "59k" ]
                    , p [ class "subtitle" ] [ text "Products" ]
                    ]
                ]
            , div [ class "tile is-parent" ]
                [ article [ class "tile is-child box" ]
                    [ p [ class "title" ] [ text "3.4k" ]
                    , p [ class "subtitle" ] [ text "Open Orders" ]
                    ]
                ]
            , div [ class "tile is-parent" ]
                [ article [ class "tile is-child box" ]
                    [ p [ class "title" ] [ text "19" ]
                    , p [ class "subtitle" ] [ text "Exceptions" ]
                    ]
                ]
            ]
        ]
