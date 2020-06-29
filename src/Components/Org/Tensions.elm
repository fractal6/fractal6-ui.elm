module Components.Org.Tensions exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Array
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav
import Components.Fa as Fa
import Components.HelperBar as HelperBar
import Components.Loading as Loading exposing (WebData, viewAuthNeeded, viewGqlErrors, viewHttpErrors, viewWarnings)
import Components.Text as Text exposing (..)
import Date exposing (formatTime)
import Dict exposing (Dict)
import Extra exposing (ternary, withDefaultData, withMaybeData)
import Extra.Events exposing (onClickPD, onEnter, onKeydown, onTab)
import Extra.Url exposing (queryBuilder, queryParser)
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
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, li, nav, option, p, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, placeholder, rows, selected, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Requests exposing (fetchChildren)
import ModelCommon.Uri exposing (Flags_, FractalBaseRoute(..), NodeFocus, basePathChanged, focusFromNameid, focusState, nameidFromFlags, uriFromNameid)
import ModelCommon.View exposing (mediaTension)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.AddNode exposing (addNewMember, addOneCircle)
import Query.AddTension exposing (addCircleTension, addOneTension)
import Query.QueryNodes exposing (queryLocalGraph)
import Query.QueryTension exposing (queryCircleTension, queryExtTension, queryIntTension)
import RemoteData exposing (RemoteData)
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
    , path_data : GqlData LocalGraph
    , children : WebData (List NodeId)
    , tensions_int : GqlData TensionsData
    , tensions_ext : GqlData TensionsData
    , offset : Int
    , load_more_int : Bool
    , load_more_ext : Bool
    , pattern : Maybe String
    , initPattern : Maybe String
    , viewMode : ViewModeTensions
    , depthFilter : DepthFilter
    , statusFilter : StatusFilter
    , node_action : ActionState
    , isModalActive : Bool -- Only use by JoinOrga for now. (other actions rely on Bulma drivers)
    }


type TensionDirection
    = InternalTension
    | ExternalTension
    | ListTension



-- Query parameters


type ViewModeTensions
    = ListView
    | IntExtView


viewModeDecoder : String -> ViewModeTensions
viewModeDecoder x =
    case x of
        "intext" ->
            IntExtView

        default ->
            ListView


viewModeEncoder : ViewModeTensions -> String
viewModeEncoder x =
    case x of
        ListView ->
            ""

        IntExtView ->
            "intext"


type DepthFilter
    = SelectedNode
    | AllSubChildren


depthFilterDecoder : String -> DepthFilter
depthFilterDecoder x =
    case x of
        "selected" ->
            SelectedNode

        default ->
            AllSubChildren


depthFilterEncoder : DepthFilter -> String
depthFilterEncoder x =
    case x of
        AllSubChildren ->
            ""

        SelectedNode ->
            "selected"


type StatusFilter
    = OpenStatus
    | ClosedStatus
    | AllStatus


statusFilterDecoder : String -> StatusFilter
statusFilterDecoder x =
    case x of
        "all" ->
            AllStatus

        "closed" ->
            ClosedStatus

        default ->
            OpenStatus


statusFilterEncoder : StatusFilter -> String
statusFilterEncoder x =
    case x of
        AllStatus ->
            "all"

        ClosedStatus ->
            "closed"

        OpenStatus ->
            ""


nfirst : Int
nfirst =
    15



--
-- Msg
--


type Msg
    = PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
      -- Data Queries
    | GotPath (GqlData LocalGraph) -- GraphQL
    | GotPath2 (GqlData LocalGraph) -- GraphQL
    | GotChildren (WebData (List NodeId)) -- HTTP/Json
    | GotTensionsInt (GqlData TensionsData) -- GraphQL
    | GotTensionsExt (GqlData TensionsData) -- GraphQL
      -- Page Action
    | DoLoad
    | ChangePattern String
    | ChangeDepthFilter String
    | ChangeStatusFilter String
    | SearchKeyDown Int
    | SubmitSearch
    | GoView ViewModeTensions
      -- JoinOrga Action
    | DoJoinOrga String Time.Posix
    | JoinAck (GqlData Node)
      -- JS Interop
    | DoCloseModal String -- ports receive / Close modal
    | DoOpenModal -- ports receive / Open  modal
      -- Util
    | Navigate String



--
-- INIT
--


type alias Flags =
    Flags_


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        -- Query parameters
        query =
            queryParser global.url

        -- Focus
        newFocus =
            flags
                |> nameidFromFlags
                |> focusFromNameid

        -- What has changed
        fs =
            focusState TensionsBaseUri global.session.referer global.session.node_focus newFocus

        -- Model init
        model =
            { node_focus = newFocus
            , path_data =
                global.session.path_data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , children = RemoteData.Loading
            , tensions_int = Loading
            , tensions_ext = Loading
            , offset = 0
            , load_more_int = False
            , load_more_ext = False
            , pattern = Dict.get "q" query
            , initPattern = Dict.get "q" query
            , viewMode = Dict.get "v" query |> withDefault "" |> viewModeDecoder
            , depthFilter = Dict.get "d" query |> withDefault "" |> depthFilterDecoder
            , statusFilter = Dict.get "s" query |> withDefault "" |> statusFilterDecoder
            , node_action = NoOp
            , isModalActive = False
            }

        cmds =
            [ --ternary fs.focusChange
              --  (queryLocalGraph newFocus.nameid GotPath)
              --  (ternary (model.depthFilter == SelectedNode) (Global.send DoLoad) Cmd.none)
              queryLocalGraph newFocus.nameid GotPath
            , ternary (model.depthFilter == AllSubChildren) (fetchChildren newFocus.nameid GotChildren) Cmd.none
            , Global.sendSleep PassedSlowLoadTreshold 500
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
                tensions_int =
                    ternary (model.tensions_int == Loading) LoadingSlowly model.tensions_int

                tensions_ext =
                    ternary (model.tensions_ext == Loading) LoadingSlowly model.tensions_ext
            in
            ( { model | tensions_int = tensions_int, tensions_ext = tensions_ext }, Cmd.none, Cmd.none )

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
                            let
                                cmd =
                                    ternary (model.depthFilter == SelectedNode) (Global.send DoLoad) Cmd.none
                            in
                            ( newModel, cmd, Global.send (UpdateSessionPath (Just path)) )

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

                                        cmd =
                                            ternary (model.depthFilter == SelectedNode) (Global.send DoLoad) Cmd.none
                                    in
                                    ( { model | path_data = Success newPath }, cmd, Global.send (UpdateSessionPath (Just newPath)) )

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

        GotChildren result ->
            let
                newModel =
                    { model | children = result }
            in
            case result of
                RemoteData.Success children ->
                    ( newModel, Global.send DoLoad, Cmd.none )

                _ ->
                    ( newModel, Cmd.none, Cmd.none )

        GotTensionsInt result ->
            let
                load_more =
                    case result of
                        Success ts ->
                            List.length ts == nfirst

                        other ->
                            False

                newResult =
                    case model.tensions_int of
                        Success tsOld ->
                            case result of
                                Success ts ->
                                    tsOld ++ ts |> Success

                                other ->
                                    tsOld |> Success

                        other ->
                            result
            in
            ( { model | tensions_int = newResult, load_more_int = load_more, offset = model.offset + 1 }, Cmd.none, Cmd.none )

        GotTensionsExt result ->
            let
                load_more =
                    case result of
                        Success ts ->
                            List.length ts == nfirst

                        other ->
                            False

                newResult =
                    case model.tensions_ext of
                        Success tsOld ->
                            case result of
                                Success ts ->
                                    tsOld ++ ts |> Success

                                other ->
                                    tsOld |> Success

                        other ->
                            result
            in
            ( { model | tensions_ext = newResult, load_more_ext = load_more }, Cmd.none, Cmd.none )

        DoLoad ->
            case model.depthFilter of
                AllSubChildren ->
                    case model.children of
                        RemoteData.Success children ->
                            let
                                status =
                                    case model.statusFilter of
                                        AllStatus ->
                                            Nothing

                                        OpenStatus ->
                                            Just TensionStatus.Open

                                        ClosedStatus ->
                                            Just TensionStatus.Closed

                                nameids =
                                    children |> List.map (\x -> x.nameid) |> List.append [ model.node_focus.nameid ]

                                cmds =
                                    [ queryIntTension nameids nfirst (model.offset * nfirst) model.pattern status GotTensionsInt
                                    , queryExtTension nameids nfirst (model.offset * nfirst) model.pattern status GotTensionsExt
                                    ]
                            in
                            ( model, Cmd.batch cmds, Cmd.none )

                        other ->
                            ( model, Cmd.none, Cmd.none )

                SelectedNode ->
                    case model.path_data of
                        Success path ->
                            let
                                status =
                                    case model.statusFilter of
                                        AllStatus ->
                                            Nothing

                                        OpenStatus ->
                                            Just TensionStatus.Open

                                        ClosedStatus ->
                                            Just TensionStatus.Closed

                                nameids =
                                    path.focus.children |> List.map (\x -> x.nameid) |> List.append [ path.focus.nameid ]

                                cmds =
                                    [ queryIntTension nameids nfirst (model.offset * nfirst) model.pattern status GotTensionsInt
                                    , queryExtTension nameids nfirst (model.offset * nfirst) model.pattern status GotTensionsExt
                                    ]
                            in
                            ( model, Cmd.batch cmds, Cmd.none )

                        other ->
                            ( model, Cmd.none, Cmd.none )

        ChangePattern value ->
            ( { model | pattern = Just value }, Cmd.none, Cmd.none )

        ChangeDepthFilter value ->
            let
                newModel =
                    { model | depthFilter = depthFilterDecoder value }
            in
            ( newModel, Global.send SubmitSearch, Cmd.none )

        ChangeStatusFilter value ->
            let
                newModel =
                    { model | statusFilter = statusFilterDecoder value }
            in
            ( newModel, Global.send SubmitSearch, Cmd.none )

        SearchKeyDown key ->
            case key of
                13 ->
                    --ENTER
                    ( model, Global.send SubmitSearch, Cmd.none )

                27 ->
                    --ESC
                    ( model, Global.send (ChangePattern ""), Cmd.none )

                other ->
                    ( model, Cmd.none, Cmd.none )

        SubmitSearch ->
            case model.path_data of
                Success path ->
                    let
                        query =
                            queryBuilder
                                [ ( "q", model.pattern |> withDefault "" |> String.trim )
                                , ( "v", model.viewMode |> viewModeEncoder )
                                , ( "d", model.depthFilter |> depthFilterEncoder )
                                , ( "s", model.statusFilter |> statusFilterEncoder )
                                ]
                    in
                    ( model, Cmd.none, Nav.pushUrl global.key (uriFromNameid TensionsBaseUri path.focus.nameid ++ "?" ++ query) )

                other ->
                    ( model, Cmd.none, Cmd.none )

        GoView viewMode ->
            ( { model | viewMode = viewMode }, Cmd.none, Cmd.none )

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
                            , id = model.path_data |> withMaybeData |> Maybe.map (\pd -> pd.root |> Maybe.map (\r -> r.id) |> withDefault "")
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

        -- Common
        DoOpenModal ->
            ( { model | isModalActive = True }, Cmd.none, Ports.open_modal )

        DoCloseModal _ ->
            ( { model | isModalActive = False }, Cmd.none, Cmd.none )

        Navigate url ->
            ( model, Cmd.none, Nav.pushUrl global.key url )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.batch
        [ Ports.closeModalFromJs DoCloseModal ]


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Tensions · " ++ (String.join "/" <| LE.unique [ model.node_focus.rootnameid, model.node_focus.nameid |> String.split "#" |> List.reverse |> List.head |> withDefault "" ])
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
            [ div [ class "column is-10-desktop is-10-widescreen is-9-fullhd" ]
                [ div [ class "columns is-centered" ]
                    [ div [ class "column is-8-desktop is-6-fullhd" ] [ viewSearchBar model.pattern model.depthFilter model.statusFilter model.viewMode ] ]
                , div [] <|
                    case model.children of
                        RemoteData.Failure err ->
                            [ viewHttpErrors err ]

                        other ->
                            []
                , case model.viewMode of
                    ListView ->
                        viewListTensions model

                    IntExtView ->
                        viewIntExtTensions model
                , div [ class "column is-12  is-aligned-center", attribute "style" "margin-left: 0.5rem;" ]
                    [ if model.load_more_int || model.load_more_ext then
                        button [ class "button is-small", onClick DoLoad ]
                            [ text "Load more" ]

                      else
                        div [] []
                    ]
                ]
            ]
        , setupActionModal model.isModalActive model.node_action
        ]


viewSearchBar : Maybe String -> DepthFilter -> StatusFilter -> ViewModeTensions -> Html Msg
viewSearchBar pattern depthFilter statusFilter viewMode =
    div [ id "searchBarTensions", class "searchBar" ]
        [ div [ class "field has-addons" ]
            [ div [ class "control has-icons-left is-expanded dropdown" ]
                [ input
                    [ class "input is-small autofocus"
                    , type_ "text"
                    , placeholder "Search tensions"
                    , value (pattern |> withDefault "")
                    , onInput ChangePattern
                    , onKeydown SearchKeyDown
                    ]
                    []
                , span [ class "icon is-left" ] [ i [ class "fas fa-search" ] [] ]
                ]
            , div [ class "control" ]
                [ div [ class "is-small select" ]
                    [ select [ onInput ChangeDepthFilter ]
                        [ option [ class "dropdown-item", value (depthFilterEncoder AllSubChildren), selected (depthFilter == AllSubChildren) ] [ text "All sub-circles" ]
                        , option [ class "dropdown-item", value (depthFilterEncoder SelectedNode), selected (depthFilter == SelectedNode) ] [ text "Selected circle" ]
                        ]
                    ]
                ]
            , div [ class "control" ]
                [ div [ class "is-small select" ]
                    [ select [ onInput ChangeStatusFilter ]
                        [ option [ class "dropdown-item", value (statusFilterEncoder OpenStatus), selected (statusFilter == OpenStatus) ] [ text "Open" ]
                        , option [ class "dropdown-item", value (statusFilterEncoder ClosedStatus), selected (statusFilter == ClosedStatus) ] [ text "Closed" ]
                        , option [ class "dropdown-item", value (statusFilterEncoder AllStatus), selected (statusFilter == AllStatus) ] [ text "All" ]
                        ]
                    ]
                ]
            ]
        , div [ class "tabs is-small" ]
            [ ul []
                [ li [ classList [ ( "is-active", viewMode == ListView ) ] ] [ a [ onClickPD (GoView ListView), target "_blank" ] [ text "List" ] ]
                , li [ classList [ ( "is-active", viewMode == IntExtView ) ] ] [ a [ onClickPD (GoView IntExtView), target "_blank" ] [ text "Internal/External" ] ]
                ]
            ]
        ]


viewListTensions : Model -> Html Msg
viewListTensions model =
    let
        t1 =
            model.tensions_int |> withDefaultData []

        t2 =
            model.tensions_ext |> withDefaultData []

        tensions_d =
            case t1 ++ t2 of
                [] ->
                    model.tensions_int

                other ->
                    other |> List.sortBy .createdAt |> List.reverse |> Success
    in
    div [ class "columns is-centered" ]
        [ div [ class "column is-10-desktop is-8-fullhd" ]
            [ viewTensions model.node_focus model.initPattern tensions_d ListTension
            ]
        ]


viewIntExtTensions : Model -> Html Msg
viewIntExtTensions model =
    div [ class "columns" ]
        [ div [ class "column is-6" ]
            [ h2 [ class "subtitle has-text-weight-semibold has-text-centered" ] [ text Text.internalTensions ]
            , viewTensions model.node_focus model.initPattern model.tensions_int InternalTension
            ]
        , div [ class "vline" ] []
        , div [ class "column is-6" ]
            [ h2 [ class "subtitle has-text-weight-semibold has-text-centered" ] [ text Text.externalTensions ]
            , viewTensions model.node_focus model.initPattern model.tensions_ext ExternalTension
            ]
        ]


viewTensions : NodeFocus -> Maybe String -> GqlData TensionsData -> TensionDirection -> Html Msg
viewTensions focus pattern tensionsData tensionDir =
    div [ classList [ ( "box", True ), ( "spinner", tensionsData == LoadingSlowly ) ] ]
        [ case tensionsData of
            Success tensions ->
                if List.length tensions > 0 then
                    List.map (\t -> mediaTension TensionsBaseUri focus t Navigate) tensions
                        |> div [ class "is-size-7", id "tensionsTab" ]

                else if pattern /= Nothing then
                    div [] [ "No results for: " ++ (pattern |> withDefault "") |> text ]

                else
                    case focus.type_ of
                        NodeType.Role ->
                            case tensionDir of
                                InternalTension ->
                                    div [] [ text Text.noIntTensionRole ]

                                ExternalTension ->
                                    div [] [ text Text.noExtTensionRole ]

                                ListTension ->
                                    div [] [ text Text.noTensionRole ]

                        NodeType.Circle ->
                            case tensionDir of
                                InternalTension ->
                                    div [] [ text Text.noIntTensionCircle ]

                                ExternalTension ->
                                    div [] [ text Text.noExtTensionCircle ]

                                ListTension ->
                                    div [] [ text Text.noTensionCircle ]

            Failure err ->
                viewGqlErrors err

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
            viewGqlErrors errMsg

        JoinValidation form result ->
            case result of
                Success _ ->
                    div [ class "box has-background-success" ] [ "Welcome in " ++ (form.rootnameid |> String.split "#" |> List.head |> withDefault "Unknonwn") |> text ]

                Failure err ->
                    viewGqlErrors err

                default ->
                    div [ class "box spinner" ] [ text Text.loading ]
