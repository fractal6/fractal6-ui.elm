module Components.User.Profile exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Components.Fa as Fa
import Components.HelperBar as HelperBar
import Components.Loading as Loading exposing (viewAuthNeeded, viewGqlErrors, viewHttpErrors, viewWarnings)
import Components.Text as Text exposing (..)
import Date exposing (formatTime)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Fractal.Enum.NodeType as NodeType
import Global exposing (Msg(..))
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, type_)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Uri exposing (FractalBaseRoute(..), NodeFocus)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.AddNode exposing (addNewMember)
import Task
import Time


type alias Flags =
    { param1 : String }


type alias Model =
    { username : String }


type Msg
    = NoOp


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        model =
            { username = flags.param1 }
    in
    ( model, Cmd.none, Cmd.none )


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, Cmd.none )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.none


view : Global.Model -> Model -> Document Msg
view global model =
    { title = model.username
    , body = [ view_ global model ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div []
        [ "Welcome " ++ model.username |> text ]
