module Components.Org.Tensions exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Array
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav
import Components.Fa as Fa
import Components.HelperBar as HelperBar
import Components.Loading as Loading exposing (viewAuthNeeded, viewErrors, viewWarnings)
import Components.Text as Text exposing (..)
import Date exposing (formatTime)
import Debug
import Dict exposing (Dict)
import Extra.Events exposing (onEnter, onKeydown, onTab)
import Form
import Form.NewCircle
import Form.NewTension
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Global exposing (Msg(..))
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, li, nav, option, p, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, placeholder, rows, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import Json.Decode as JD exposing (Value, decodeValue)
import Json.Encode as JE
import Json.Encode.Extra as JEE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Uri exposing (Flags_, FractalBaseRoute(..), NodeFocus, NodePath, basePathChanged, focusFromNameid, nameidFromFlags)
import ModelCommon.View exposing (mediaTension)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.AddNode exposing (addNewMember, addOneCircle)
import Query.AddTension exposing (addCircleTension, addOneTension)
import Query.QueryTension exposing (queryCircleTension, queryPageTension)
import Task
import Time


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--
-- Model
--


type alias Model =
    { node_focus : NodeFocus
    , node_path : Maybe NodePath
    , tensions : GqlData TensionsData
    , node_action : ActionState
    , isModalActive : Bool -- Only use by JoinOrga for now. (other actions rely on Bulma drivers)
    }



--
-- INIT
--


type alias Flags =
    Flags_


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        -- init Flags and Session
        session =
            global.session

        newFocus =
            flags
                |> nameidFromFlags
                |> focusFromNameid

        -- What has changed
        oldFocus =
            session.node_focus |> withDefault newFocus

        isInit =
            session.node_focus == Nothing

        orgChange =
            (newFocus.rootnameid /= oldFocus.rootnameid) || isInit

        focusChange =
            (newFocus.nameid /= oldFocus.nameid) || isInit

        --d1 = Debug.log "isInit, orgChange, focuChange" [ isInit, orgChange, focusChange ]
        --d2 = Debug.log "newfocus" [ newFocus ]
        refresh =
            basePathChanged TensionsBaseUri global.session.referer

        model =
            { tensions = NotAsked
            , node_focus = newFocus
            , node_path = session.node_path
            , node_action = NoOp
            , isModalActive = False
            }

        cmds =
            if orgChange || refresh then
                [ queryPageTension newFocus.nameid GotTensions
                , Global.sendSleep PassedSlowLoadTreshold 500
                ]

            else if focusChange then
                [ queryPageTension newFocus.nameid GotTensions
                , Global.sendSleep PassedSlowLoadTreshold 500
                ]

            else
                []
    in
    ( model
    , Cmd.batch cmds
    , Global.send (UpdateSessionFocus newFocus)
    )


type Msg
    = PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
      -- Gql Data Queries
    | GotTensions (GqlData TensionsData) -- graphql
      -- JoinOrga Action
    | DoJoinOrga String Time.Posix
    | JoinAck (GqlData Node)
      -- JS Interop
    | DoCloseModal String -- ports receive / Close modal
    | DoOpenModal -- ports receive / Open  modal


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global msg model =
    case msg of
        PassedSlowLoadTreshold ->
            let
                tensions =
                    case model.tensions of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other
            in
            ( { model | tensions = tensions }, Cmd.none, Cmd.none )

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        -- Gql queries
        GotTensions result ->
            case result of
                Success data ->
                    ( { model | tensions = result }, Cmd.none, Cmd.none )

                other ->
                    ( { model | tensions = result }, Cmd.none, Cmd.none )

        -- Join
        DoJoinOrga rootnameid time ->
            case global.session.user of
                LoggedOut ->
                    ( { model | node_action = JoinOrga JoinAuthNeeded }, Global.send DoOpenModal, Cmd.none )

                LoggedIn uctx ->
                    let
                        form =
                            { uctx = uctx
                            , rootnameid = rootnameid
                            , post = Dict.fromList [ ( "createdAt", fromTime time ) ]
                            }

                        newModel =
                            { model | node_action = JoinOrga (JoinInit form) }
                    in
                    ( newModel, Cmd.batch [ addNewMember form JoinAck, Global.send DoOpenModal ], Cmd.none )

        JoinAck result ->
            case model.node_action of
                JoinOrga (JoinInit form) ->
                    case result of
                        Success n ->
                            ( { model | node_action = JoinOrga (JoinValidation form result) }
                            , Cmd.none
                            , Cmd.batch [ Global.send UpdateUserToken ]
                            )

                        other ->
                            ( { model | node_action = JoinOrga (JoinValidation form result) }, Cmd.none, Cmd.none )

                default ->
                    ( model, Cmd.none, Cmd.none )

        -- Modal
        DoOpenModal ->
            ( { model | isModalActive = True }, Cmd.none, Ports.open_modal )

        DoCloseModal _ ->
            ( { model | isModalActive = False }, Cmd.none, Cmd.none )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.none


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Tensions · " ++ String.join "/" [ model.node_focus.rootnameid, model.node_focus.nameid |> String.split "#" |> List.reverse |> List.head |> withDefault "" ]
    , body = [ view_ global model ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ id "mainPane" ]
        [ HelperBar.view TensionsBaseUri
            global.session.user
            model.node_path
            (Submit <| DoJoinOrga model.node_focus.rootnameid)
        , div [ class "columns is-centered is-variable is-4" ]
            [ div [ class "column is-10-desktop is-10-widescreen is-9-fullhd" ]
                [ viewTensions model.node_focus model.tensions
                , setupActionModal model.isModalActive model.node_action
                ]
            ]
        ]


viewTensions : NodeFocus -> GqlData TensionsData -> Html Msg
viewTensions focus tensionsData =
    div [ classList [ ( "spinner", tensionsData == LoadingSlowly ) ] ]
        [ case tensionsData of
            Success tensions ->
                if List.length tensions > 0 then
                    List.map (\t -> mediaTension t) tensions
                        |> div [ class "is-size-7", id "tensionsTab" ]

                else
                    case focus.type_ of
                        NodeType.Role ->
                            div [] [ text Text.noTensionRole ]

                        NodeType.Circle ->
                            div [] [ text Text.noTensionCircle ]

            Failure err ->
                viewErrors err

            default ->
                div [] []
        ]



-- Actions


setupActionModal : Bool -> ActionState -> Html Msg
setupActionModal isModalActive action =
    div
        [ id "actionModal"
        , classList
            [ ( "modal", True )
            , ( "modal-fx-fadeIn", True )
            , ( "is-active", isModalActive )
            , ( "protected_", isModalActive )
            ]
        ]
        [ div
            [ classList
                [ ( "modal-background", True )
                , ( "protected_", isModalActive )
                ]
            ]
            []
        , div [ class "modal-content" ]
            [ case action of
                JoinOrga step ->
                    viewJoinOrgaStep step

                other ->
                    div [] [ text "Action not implemented." ]
            ]
        , button
            [ classList
                [ ( "modal-close", True )
                , ( "is-large", True )
                , ( "protected_", isModalActive )
                ]
            ]
            []
        ]


viewJoinOrgaStep : JoinStep JoinOrgaForm -> Html Msg
viewJoinOrgaStep step =
    case step of
        JoinInit _ ->
            div [ class "box spinner" ] [ text Text.loading ]

        JoinAuthNeeded ->
            viewAuthNeeded

        JoinNotAuthorized errMsg ->
            viewErrors errMsg

        JoinValidation form result ->
            case result of
                Success _ ->
                    div [ class "box has-background-success" ] [ "Welcome in " ++ (form.rootnameid |> String.split "#" |> List.head |> withDefault "Unknonwn") |> text ]

                Failure err ->
                    viewErrors err

                default ->
                    div [ class "box spinner" ] [ text Text.loading ]
