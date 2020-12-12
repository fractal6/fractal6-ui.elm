module Components.Org.Tensions exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Array
import Auth exposing (AuthState(..), doRefreshToken, refreshAuthModal)
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav
import Components.Fa as Fa
import Components.HelperBar as HelperBar exposing (HelperBar)
import Components.Loading as Loading exposing (GqlData, RequestResult(..), WebData, viewAuthNeeded, viewGqlErrors, viewHttpErrors, withDefaultData, withMaybeData)
import Date exposing (formatTime)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Extra.Events exposing (onClickPD, onEnter, onKeydown, onTab)
import Extra.Url exposing (queryBuilder, queryParser)
import Form exposing (isPostSendable)
import Form.NewCircle
import Form.NewTension
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, li, nav, option, p, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, autocomplete, autofocus, class, classList, disabled, href, id, list, placeholder, rows, selected, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (Flags_, FractalBaseRoute(..), NodeFocus, basePathChanged, focusFromNameid, focusState, nameidFromFlags, uriFromNameid)
import ModelCommon.Requests exposing (fetchChildren, fetchMembers, login)
import ModelCommon.View exposing (mediaTension, tensionTypeColor)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.PatchTension exposing (actionRequest)
import Query.QueryNode exposing (fetchNode, queryLocalGraph)
import Query.QueryTension exposing (queryExtTension, queryIntTension)
import RemoteData exposing (RemoteData)
import Task
import Text as T
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



---- MODEL --


type alias Model =
    { -- Focus
      node_focus : NodeFocus
    , path_data : GqlData LocalGraph
    , children : WebData (List NodeId)

    -- Pages
    , tensions_int : GqlData TensionsData
    , tensions_ext : GqlData TensionsData
    , offset : Int
    , load_more_int : Bool
    , load_more_ext : Bool
    , pattern : Maybe String
    , initPattern : Maybe String
    , viewMode : TensionsView
    , statusFilter : StatusFilter
    , typeFilter : TypeFilter
    , depthFilter : DepthFilter

    -- Common
    , node_action : ActionState
    , isModalActive : Bool -- Only use by JoinOrga for now. (other actions rely on Bulma drivers)
    , modalAuth : ModalAuth
    , helperBar : HelperBar
    , refresh_trial : Int
    }


type TensionDirection
    = InternalTension
    | ExternalTension
    | ListTension



-- Query parameters


type TensionsView
    = ListView
    | IntExtView


viewModeEncoder : TensionsView -> String
viewModeEncoder x =
    case x of
        ListView ->
            ""

        IntExtView ->
            "intext"


viewModeDecoder : String -> TensionsView
viewModeDecoder x =
    case x of
        "intext" ->
            IntExtView

        default ->
            ListView


type DepthFilter
    = SelectedNode
    | AllSubChildren


depthFilterEncoder : DepthFilter -> String
depthFilterEncoder x =
    case x of
        AllSubChildren ->
            ""

        SelectedNode ->
            "selected"


depthFilterDecoder : String -> DepthFilter
depthFilterDecoder x =
    case x of
        "selected" ->
            SelectedNode

        default ->
            AllSubChildren


type StatusFilter
    = OpenStatus
    | ClosedStatus
    | AllStatus


statusFilterEncoder : StatusFilter -> String
statusFilterEncoder x =
    case x of
        AllStatus ->
            "all"

        ClosedStatus ->
            "closed"

        OpenStatus ->
            ""


statusFilterDecoder : String -> StatusFilter
statusFilterDecoder x =
    case x of
        "all" ->
            AllStatus

        "closed" ->
            ClosedStatus

        default ->
            OpenStatus


type TypeFilter
    = AllTypes
    | GovernanceType
    | OperationalType
    | HelpType


typeFilterEncoder : TypeFilter -> String
typeFilterEncoder x =
    case x of
        GovernanceType ->
            "governance"

        OperationalType ->
            "operational"

        HelpType ->
            "help"

        AllTypes ->
            ""


typeFilterDecoder : String -> TypeFilter
typeFilterDecoder x =
    case x of
        "governance" ->
            GovernanceType

        "operational" ->
            OperationalType

        "help" ->
            HelpType

        default ->
            AllTypes


nfirst : Int
nfirst =
    15



---- MSG ----


type Msg
    = PassedSlowLoadTreshold -- timer
    | PushGuest ActionForm
    | Submit (Time.Posix -> Msg) -- Get Current Time
      -- Data Queries
    | GotPath (GqlData LocalGraph) -- GraphQL
    | GotPath2 (GqlData LocalGraph) -- GraphQL
    | GotChildren (WebData (List NodeId)) -- HTTP/Json
    | GotTensionsInt (GqlData TensionsData) -- GraphQL
    | GotTensionsExt (GqlData TensionsData) -- GraphQL
      -- Page Action
    | DoLoad -- query tensions
    | ChangePattern String
    | ChangeStatusFilter String
    | ChangeTypeFilter String
    | ChangeDepthFilter String
    | SearchKeyDown Int
    | SubmitSearch
    | GoView TensionsView
      -- JoinOrga Action
    | DoJoinOrga String
    | DoJoinOrga2 (GqlData Node)
    | DoJoinOrga3 Node Time.Posix
    | JoinAck (GqlData ActionResult)
      -- Token refresh
    | DoOpenAuthModal UserCtx -- ports receive / Open  modal
    | DoCloseAuthModal -- ports receive / Close modal
    | ChangeAuthPost String String
    | SubmitUser UserAuthForm
    | GotSignin (WebData UserCtx)
    | SubmitKeyDown Int -- Detect Enter (for form sending)
      -- Common
    | Navigate String
    | DoOpenModal -- ports receive / Open  modal
    | DoCloseModal String -- ports receive / Close modal
    | ExpandRoles
    | CollapseRoles



---- INIT ----


type alias Flags =
    Flags_


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        apis =
            global.session.apis

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
            , statusFilter = Dict.get "s" query |> withDefault "" |> statusFilterDecoder
            , typeFilter = Dict.get "t" query |> withDefault "" |> typeFilterDecoder
            , depthFilter = Dict.get "d" query |> withDefault "" |> depthFilterDecoder

            -- Common
            , node_action = NoOp
            , isModalActive = False
            , modalAuth = Inactive
            , helperBar = HelperBar.create
            , refresh_trial = 0
            }

        cmds =
            [ --ternary fs.focusChange
              --  (queryLocalGraph newFocus.nameid GotPath)
              --  (ternary (model.depthFilter == SelectedNode) (send DoLoad) Cmd.none)
              queryLocalGraph apis.gql newFocus.nameid GotPath
            , ternary (model.depthFilter == AllSubChildren) (fetchChildren apis.rest newFocus.nameid GotChildren) Cmd.none
            , sendSleep PassedSlowLoadTreshold 500
            ]
    in
    ( model
    , Cmd.batch cmds
    , send (UpdateSessionFocus (Just newFocus))
    )



---- UPDATE ----


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global msg model =
    let
        apis =
            global.session.apis
    in
    case msg of
        PushGuest form ->
            ( model, actionRequest apis.gql form JoinAck, Cmd.none )

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
                                    ternary (model.depthFilter == SelectedNode) (send DoLoad) Cmd.none
                            in
                            ( newModel, cmd, send (UpdateSessionPath (Just path)) )

                        Nothing ->
                            let
                                nameid =
                                    List.head path.path |> Maybe.map (\p -> p.nameid) |> withDefault ""
                            in
                            ( newModel, queryLocalGraph apis.gql nameid GotPath2, Cmd.none )

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
                                            ternary (model.depthFilter == SelectedNode) (send DoLoad) Cmd.none
                                    in
                                    ( { model | path_data = Success newPath }, cmd, send (UpdateSessionPath (Just newPath)) )

                                Nothing ->
                                    let
                                        nameid =
                                            List.head path.path |> Maybe.map (\p -> p.nameid) |> withDefault ""

                                        newPath =
                                            { prevPath | path = path.path ++ (List.tail prevPath.path |> withDefault []) }
                                    in
                                    ( { model | path_data = Success newPath }, queryLocalGraph apis.gql nameid GotPath2, Cmd.none )

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
                    ( newModel, send DoLoad, Cmd.none )

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
            let
                status =
                    case model.statusFilter of
                        AllStatus ->
                            Nothing

                        OpenStatus ->
                            Just TensionStatus.Open

                        ClosedStatus ->
                            Just TensionStatus.Closed

                type_ =
                    case model.typeFilter of
                        AllTypes ->
                            Nothing

                        GovernanceType ->
                            Just TensionType.Governance

                        OperationalType ->
                            Just TensionType.Operational

                        HelpType ->
                            Just TensionType.Help
            in
            case model.depthFilter of
                AllSubChildren ->
                    case model.children of
                        RemoteData.Success children ->
                            let
                                nameids =
                                    children |> List.map (\x -> x.nameid) |> List.append [ model.node_focus.nameid ]

                                cmds =
                                    [ queryIntTension apis.gql nameids nfirst (model.offset * nfirst) model.pattern status type_ GotTensionsInt
                                    , queryExtTension apis.gql nameids nfirst (model.offset * nfirst) model.pattern status type_ GotTensionsExt
                                    ]
                            in
                            ( model, Cmd.batch cmds, Cmd.none )

                        other ->
                            ( model, Cmd.none, Cmd.none )

                SelectedNode ->
                    case model.path_data of
                        Success path ->
                            let
                                nameids =
                                    path.focus.children |> List.map (\x -> x.nameid) |> List.append [ path.focus.nameid ]

                                cmds =
                                    [ queryIntTension apis.gql nameids nfirst (model.offset * nfirst) model.pattern status type_ GotTensionsInt
                                    , queryExtTension apis.gql nameids nfirst (model.offset * nfirst) model.pattern status type_ GotTensionsExt
                                    ]
                            in
                            ( model, Cmd.batch cmds, Cmd.none )

                        other ->
                            ( model, Cmd.none, Cmd.none )

        ChangePattern value ->
            ( { model | pattern = Just value }, Cmd.none, Cmd.none )

        ChangeStatusFilter value ->
            let
                newModel =
                    { model | statusFilter = statusFilterDecoder value }
            in
            ( newModel, send SubmitSearch, Cmd.none )

        ChangeTypeFilter value ->
            let
                newModel =
                    { model | typeFilter = typeFilterDecoder value }
            in
            ( newModel, send SubmitSearch, Cmd.none )

        ChangeDepthFilter value ->
            let
                newModel =
                    { model | depthFilter = depthFilterDecoder value }
            in
            ( newModel, send SubmitSearch, Cmd.none )

        SearchKeyDown key ->
            case key of
                13 ->
                    --ENTER
                    ( model, send SubmitSearch, Cmd.none )

                27 ->
                    --ESC
                    ( model, send (ChangePattern ""), Cmd.none )

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
                                , ( "t", model.typeFilter |> typeFilterEncoder )
                                ]
                    in
                    ( model, Cmd.none, Nav.pushUrl global.key (uriFromNameid TensionsBaseUri path.focus.nameid ++ "?" ++ query) )

                other ->
                    ( model, Cmd.none, Cmd.none )

        GoView viewMode ->
            ( { model | viewMode = viewMode }, Cmd.none, Cmd.none )

        -- Join
        DoJoinOrga rootnameid ->
            case global.session.user of
                LoggedOut ->
                    ( { model | node_action = ActionAuthNeeded }
                    , send DoOpenModal
                    , Cmd.none
                    )

                LoggedIn uctx ->
                    ( { model | node_action = JoinOrga (JoinInit LoadingSlowly) }
                    , Cmd.batch [ fetchNode apis.gql rootnameid DoJoinOrga2, send DoOpenModal ]
                    , Cmd.none
                    )

        DoJoinOrga2 result ->
            case result of
                Success n ->
                    ( { model | node_action = JoinOrga (JoinInit LoadingSlowly) }
                    , send (Submit <| DoJoinOrga3 n)
                    , Cmd.none
                    )

                other ->
                    ( { model | node_action = JoinOrga (JoinInit result) }, Cmd.none, Cmd.none )

        DoJoinOrga3 node time ->
            let
                ( tid, bid ) =
                    node.source
                        |> Maybe.map (\b -> ( b.tension.id, b.id ))
                        |> withDefault ( "", "" )

                f =
                    initActionForm global.session.user tid

                form =
                    { f
                        | bid = "" -- do no set bid to pass the backend
                        , events_type = Just [ TensionEvent.UserJoin ]
                        , post =
                            Dict.fromList
                                [ ( "createdAt", fromTime time )
                                , ( "old", f.uctx.username )
                                , ( "new", node.nameid )
                                ]
                        , node = node
                    }
            in
            ( { model | node_action = JoinOrga (JoinValidation form LoadingSlowly) }
            , Cmd.batch [ send (PushGuest form), send DoOpenModal ]
            , Cmd.none
            )

        JoinAck result ->
            case model.node_action of
                JoinOrga (JoinValidation form _) ->
                    case doRefreshToken result model.refresh_trial of
                        Authenticate ->
                            ( model, send (DoOpenAuthModal form.uctx), Cmd.none )

                        RefreshToken i ->
                            ( { model | refresh_trial = i }, sendSleep (PushGuest form) 500, send UpdateUserToken )

                        OkAuth n ->
                            ( { model | node_action = JoinOrga (JoinValidation form result) }
                            , Cmd.none
                            , Cmd.none
                            )

                        NoAuth ->
                            ( { model | node_action = JoinOrga (JoinValidation form result) }, Cmd.none, Cmd.none )

                default ->
                    ( model, Cmd.none, Cmd.none )

        -- Token Refresh
        DoOpenAuthModal uctx ->
            ( { model
                | modalAuth =
                    Active
                        { post = Dict.fromList [ ( "username", uctx.username ) ]
                        , result = RemoteData.NotAsked
                        }
              }
            , Cmd.none
            , Ports.open_auth_modal
            )

        DoCloseAuthModal ->
            case model.node_action of
                JoinOrga _ ->
                    ( { model | modalAuth = Inactive }, send (DoCloseModal ""), Ports.close_auth_modal )

                _ ->
                    ( { model | modalAuth = Inactive }, Cmd.none, Ports.close_auth_modal )

        ChangeAuthPost field value ->
            case model.modalAuth of
                Active form ->
                    let
                        newForm =
                            { form | post = Dict.insert field value form.post }
                    in
                    ( { model | modalAuth = Active newForm }, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        SubmitUser form ->
            ( model, login apis.auth form.post GotSignin, Cmd.none )

        GotSignin result ->
            case result of
                RemoteData.Success uctx ->
                    let
                        cmd =
                            case model.modalAuth of
                                Active f ->
                                    case Dict.get "msg" f.post of
                                        Just "GotOrga" ->
                                            sendSleep (Navigate (uriFromNameid OverviewBaseUri model.node_focus.rootnameid)) 500

                                        _ ->
                                            Cmd.none

                                _ ->
                                    Cmd.none
                    in
                    ( { model | modalAuth = Inactive }
                    , Cmd.batch [ send DoCloseAuthModal, cmd ]
                    , send (UpdateUserSession uctx)
                    )

                other ->
                    case model.modalAuth of
                        Active form ->
                            ( { model | modalAuth = Active { form | result = result } }, Cmd.none, Cmd.none )

                        Inactive ->
                            ( model, Cmd.none, Cmd.none )

        SubmitKeyDown key ->
            case key of
                13 ->
                    let
                        form =
                            case model.modalAuth of
                                Active f ->
                                    f

                                Inactive ->
                                    UserAuthForm Dict.empty RemoteData.NotAsked
                    in
                    --ENTER
                    if isPostSendable [ "password" ] form.post then
                        ( model, send (SubmitUser form), Cmd.none )

                    else
                        ( model, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        -- Common
        DoOpenModal ->
            ( { model | isModalActive = True }, Cmd.none, Ports.open_modal )

        DoCloseModal link ->
            let
                gcmd =
                    if link /= "" then
                        send (Navigate link)

                    else
                        Cmd.none
            in
            ( { model | isModalActive = False }, gcmd, Ports.close_modal )

        Navigate url ->
            ( model, Cmd.none, Nav.pushUrl global.key url )

        ExpandRoles ->
            ( { model | helperBar = HelperBar.expand model.helperBar }, Cmd.none, Cmd.none )

        CollapseRoles ->
            ( { model | helperBar = HelperBar.collapse model.helperBar }, Cmd.none, Cmd.none )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.batch
        [ Ports.closeModalFromJs DoCloseModal ]



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Tensions · " ++ (String.join "/" <| LE.unique [ model.node_focus.rootnameid, model.node_focus.nameid |> String.split "#" |> List.reverse |> List.head |> withDefault "" ])
    , body = [ view_ global model ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    let
        helperData =
            { onJoin = DoJoinOrga model.node_focus.rootnameid
            , onExpand = ExpandRoles
            , onCollapse = CollapseRoles
            , user = global.session.user
            , path_data = global.session.path_data
            , baseUri = TensionsBaseUri
            , data = model.helperBar
            }
    in
    div [ id "mainPane" ]
        [ HelperBar.view helperData
        , div [ class "columns is-centered" ]
            [ div [ class "column is-10-desktop is-10-widescreen is-9-fullhd" ]
                [ div [ class "columns is-centered" ]
                    [ div [ class "column is-10-desktop is-8-fullhd" ] [ viewSearchBar model.pattern model.depthFilter model.statusFilter model.typeFilter model.viewMode ] ]
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


viewSearchBar : Maybe String -> DepthFilter -> StatusFilter -> TypeFilter -> TensionsView -> Html Msg
viewSearchBar pattern depthFilter statusFilter typeFilter viewMode =
    div [ id "searchBarTensions", class "searchBar" ]
        [ div [ class "field has-addons" ]
            [ div [ class "control has-icons-left is-expanded dropdown" ]
                [ input
                    [ class "input is-small autofocus"
                    , type_ "search"
                    , autocomplete False
                    , autofocus True
                    , placeholder "Search tensions"
                    , value (pattern |> withDefault "")
                    , onInput ChangePattern
                    , onKeydown SearchKeyDown
                    ]
                    []
                , span [ class "icon is-left" ] [ i [ class "fas fa-search" ] [] ]
                ]
            , div [ class "buttons" ]
                [ div [ class "control" ]
                    [ div [ class "is-small select" ]
                        [ select [ onInput ChangeStatusFilter ]
                            [ option [ class "dropdown-item", value (statusFilterEncoder OpenStatus), selected (statusFilter == OpenStatus) ] [ text "Open" ]
                            , option [ class "dropdown-item", value (statusFilterEncoder ClosedStatus), selected (statusFilter == ClosedStatus) ] [ text "Closed" ]
                            , option [ class "dropdown-item", value (statusFilterEncoder AllStatus), selected (statusFilter == AllStatus) ] [ text "All" ]
                            ]
                        ]
                    ]
                , div [ class "control" ]
                    [ div [ class "is-small select" ]
                        [ select [ onInput ChangeTypeFilter ]
                            [ option [ class "dropdown-item", value (typeFilterEncoder AllTypes), selected (typeFilter == AllTypes) ] [ text "All types" ]
                            , option [ class "dropdown-item", value (typeFilterEncoder GovernanceType), selected (typeFilter == GovernanceType) ] [ text "Governance" ]
                            , option [ class "dropdown-item", value (typeFilterEncoder OperationalType), selected (typeFilter == OperationalType) ] [ text "Operational" ]
                            , option [ class "dropdown-item", value (typeFilterEncoder HelpType), selected (typeFilter == HelpType) ] [ text "Help" ]
                            ]
                        ]
                    ]
                , div [ class "control" ]
                    [ div [ class "is-small select" ]
                        [ select [ onInput ChangeDepthFilter ]
                            [ option [ class "dropdown-item", value (depthFilterEncoder AllSubChildren), selected (depthFilter == AllSubChildren) ] [ text "All sub-circles" ]
                            , option [ class "dropdown-item", value (depthFilterEncoder SelectedNode), selected (depthFilter == SelectedNode) ] [ text "Selected circle" ]
                            ]
                        ]
                    ]
                ]
            ]
        , div [ class "tabs is-md" ]
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
            [ h2 [ class "subtitle has-text-weight-semibold has-text-centered" ] [ text T.internalTensions ]
            , viewTensions model.node_focus model.initPattern model.tensions_int InternalTension
            ]
        , div [ class "vline" ] []
        , div [ class "column is-6" ]
            [ h2 [ class "subtitle has-text-weight-semibold has-text-centered" ] [ text T.externalTensions ]
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
                                    div [] [ text T.noIntTensionRole ]

                                ExternalTension ->
                                    div [] [ text T.noExtTensionRole ]

                                ListTension ->
                                    div [] [ text T.noTensionRole ]

                        NodeType.Circle ->
                            case tensionDir of
                                InternalTension ->
                                    div [] [ text T.noIntTensionCircle ]

                                ExternalTension ->
                                    div [] [ text T.noExtTensionCircle ]

                                ListTension ->
                                    div [] [ text T.noTensionCircle ]

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
        , class "modal modal-fx-fadeIn elmModal"
        , classList [ ( "is-active", isModalActive ) ]
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "actionModal"
            , onClick (DoCloseModal "")
            ]
            []
        , div [ class "modal-content" ]
            [ case action of
                JoinOrga step ->
                    viewJoinOrgaStep step

                NoOp ->
                    text ""

                AskErr err ->
                    viewGqlErrors [ err ]

                ActionAuthNeeded ->
                    viewAuthNeeded DoCloseModal

                other ->
                    div [] [ text "Action not implemented." ]
            ]
        , button [ class "modal-close is-large", onClick (DoCloseModal "") ] []
        ]


viewJoinOrgaStep : JoinStep ActionForm -> Html Msg
viewJoinOrgaStep step =
    case step of
        JoinInit _ ->
            div [ class "box spinner" ] [ text "" ]

        JoinNotAuthorized errMsg ->
            viewGqlErrors errMsg

        JoinValidation form result ->
            case result of
                Success _ ->
                    div [ class "box is-light", onClick (DoCloseModal "") ]
                        [ Fa.icon "fas fa-check fa-2x has-text-success" " "
                        , text (T.welcomIn ++ " ")
                        , span [ class "has-font-weight-semibold" ] [ text form.node.name ]
                        ]

                Failure err ->
                    viewGqlErrors err

                default ->
                    div [ class "box spinner" ] [ text "" ]
