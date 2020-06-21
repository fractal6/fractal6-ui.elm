module Components.Org.Tension exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

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
import ModelCommon.Uri exposing (FractalBaseRoute(..), NodeFocus, focusState)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.AddNode exposing (addNewMember)
import Query.QueryNodes exposing (queryLocalGraph)
import Query.QueryTension exposing (getTension)
import Task
import Time


type alias Flags =
    { param1 : String
    , param2 : String
    }



--
-- Model
--


type alias Model =
    { -- Focus
      node_focus : NodeFocus
    , path_data : GqlData LocalGraph

    -- Common
    , node_action : ActionState
    , isModalActive : Bool -- Only use by JoinOrga for now. (other actions rely on Bulma drivers)

    -- Page
    , tensionid : String
    , tension : GqlData TensionExtended
    }



--
-- Msg
--


type Msg
    = PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
      -- Gql Data Queries
    | GotPath (GqlData LocalGraph) -- GraphQL
    | GotPath2 (GqlData LocalGraph) -- GraphQL
    | GotTension (GqlData TensionExtended)
      -- JoinOrga Action
    | DoJoinOrga String Time.Posix
    | JoinAck (GqlData Node)
      -- JS Interop
    | DoCloseModal String -- ports receive / Close modal
    | DoOpenModal -- ports receive / Open  modal


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
        -- Focus
        newFocus =
            NodeFocus flags.param1 True flags.param1 NodeType.Circle

        -- What has changed
        fs =
            focusState TensionsBaseUri global.session.referer global.session.node_focus newFocus

        model =
            { node_focus = newFocus
            , path_data =
                global.session.path_data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , tensionid = flags.param2
            , tension = Loading
            , node_action = NoOp
            , isModalActive = False
            }

        cmds =
            [ ternary fs.focusChange (queryLocalGraph newFocus.nameid GotPath) Cmd.none
            , getTension model.tensionid GotTension
            ]
    in
    ( model
    , Cmd.batch cmds
    , Global.send (UpdateSessionFocus (Just newFocus))
    )


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global msg model =
    case msg of
        PassedSlowLoadTreshold ->
            let
                tension =
                    ternary (model.tension == Loading) LoadingSlowly model.tension
            in
            ( { model | tension = tension }, Cmd.none, Cmd.none )

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        -- Data queries
        GotPath result ->
            let
                newModel =
                    { model | path_data = result }
            in
            case result of
                Success path ->
                    case path.root of
                        Just root ->
                            ( newModel, Cmd.none, Global.send (UpdateSessionPath (Just path)) )

                        Nothing ->
                            let
                                nameid =
                                    List.head path.path |> Maybe.map (\p -> p.nameid) |> withDefault ""
                            in
                            ( newModel, queryLocalGraph nameid GotPath2, Cmd.none )

                _ ->
                    ( newModel, Cmd.none, Cmd.none )

        GotPath2 result ->
            case model.path_data of
                Success prevPath ->
                    case result of
                        Success path ->
                            case path.root of
                                Just root ->
                                    let
                                        newPath =
                                            { prevPath | root = Just root, path = path.path ++ (List.tail prevPath.path |> withDefault []) }
                                    in
                                    ( { model | path_data = Success newPath }, Cmd.none, Global.send (UpdateSessionPath (Just newPath)) )

                                Nothing ->
                                    let
                                        nameid =
                                            List.head path.path |> Maybe.map (\p -> p.nameid) |> withDefault ""

                                        newPath =
                                            { prevPath | path = path.path ++ (List.tail prevPath.path |> withDefault []) }
                                    in
                                    ( { model | path_data = Success newPath }, queryLocalGraph nameid GotPath2, Cmd.none )

                        _ ->
                            ( model, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        GotTension result ->
            let
                newModel =
                    { model | tension = result }
            in
            ( newModel, Cmd.none, Cmd.none )

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
    { title =
        case model.tension of
            Success t ->
                t.title

            _ ->
                "Loading tension"
    , body = [ view_ global model ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ id "mainPane" ]
        [ HelperBar.view TensionsBaseUri
            global.session.user
            global.session.path_data
            (Submit <| DoJoinOrga model.node_focus.rootnameid)
        , div [ class "columns is-centered" ]
            [ div [ class "column is-9" ]
                [ div [ class "columns" ] <|
                    case model.tension of
                        Success t ->
                            [ div [ class "column is tree-quarter" ] [ viewTension t ]
                            , div [ class "column " ] [ viewSidePane t ]
                            ]

                        Failure err ->
                            [ viewGqlErrors err ]

                        LoadingSlowly ->
                            [ div [ class "spinner" ] [] ]

                        other ->
                            []
                ]
            ]
        , setupActionModal model.isModalActive model.node_action
        ]


viewTension : TensionExtended -> Html Msg
viewTension t =
    div []
        [ h1 [] [ text t.title ]
        , case t.message of
            Just m ->
                div [ class "" ] [ text m ]

            Nothing ->
                div [ class "" ] [ text "No message here." ]
        ]


viewSidePane : TensionExtended -> Html Msg
viewSidePane t =
    div [] []



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
            viewGqlErrors errMsg

        JoinValidation form result ->
            case result of
                Success _ ->
                    div [ class "box has-background-success" ] [ "Welcome in " ++ (form.rootnameid |> String.split "#" |> List.head |> withDefault "Unknonwn") |> text ]

                Failure err ->
                    viewGqlErrors err

                default ->
                    div [ class "box spinner" ] [ text Text.loading ]
