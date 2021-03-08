module Org.Tensions exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Array
import Auth exposing (AuthState(..), doRefreshToken, refreshAuthModal)
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav
import Codecs exposing (QuickDoc)
import Components.HelperBar as HelperBar exposing (HelperBar)
import Components.Loading as Loading exposing (GqlData, ModalData, RequestResult(..), WebData, viewAuthNeeded, viewGqlErrors, viewHttpErrors, withDefaultData, withMaybeData, withMaybeDataMap)
import Components.UserSearchPanel as UserSearchPanel exposing (Msg(..), OnClickAction(..))
import Date exposing (formatTime)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Extra.Events exposing (onClickPD, onEnter, onKeydown, onTab)
import Extra.Url exposing (queryBuilder, queryParser)
import Form exposing (isPostSendable)
import Form.Help as Help
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
import Icon as I
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (Flags_, FractalBaseRoute(..), NodeFocus, basePathChanged, focusFromNameid, focusState, nameidFromFlags, uriFromNameid)
import ModelCommon.Requests exposing (fetchChildren, fetchMembers, getQuickDoc, login)
import ModelCommon.View exposing (mediaTension, tensionTypeColor)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.AddTension exposing (addOneTension)
import Query.PatchTension exposing (actionRequest)
import Query.QueryNode exposing (fetchNode, queryLocalGraph)
import Query.QueryTension exposing (queryExtTension, queryIntTension)
import RemoteData exposing (RemoteData)
import Task
import Text as T exposing (textH, textT)
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


mapGlobalOutcmds : List GlobalCmd -> ( List (Cmd Msg), List (Cmd Global.Msg) )
mapGlobalOutcmds gcmds =
    gcmds
        |> List.map
            (\m ->
                case m of
                    DoNavigate link ->
                        ( send (Navigate link), Cmd.none )

                    DoAuth uctx ->
                        ( send (DoOpenAuthModal uctx), Cmd.none )

                    DoUpdateToken ->
                        ( Cmd.none, send UpdateUserToken )

                    _ ->
                        ( Cmd.none, Cmd.none )
            )
        |> List.unzip



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
    , queryIsEmpty : Bool
    , authors : List User
    , authorsPanel : UserSearchPanel.State

    -- Common
    , node_action : ActionState
    , isModalActive : Bool -- Only use by JoinOrga for now. (other actions rely on Bulma drivers)
    , modalAuth : ModalAuth
    , helperBar : HelperBar
    , refresh_trial : Int
    , help : Help.State
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
            "list"

        IntExtView ->
            "intext"


viewModeDecoder : String -> TensionsView
viewModeDecoder x =
    case x of
        "intext" ->
            IntExtView

        _ ->
            ListView


defaultView : String
defaultView =
    "list"


type DepthFilter
    = SelectedNode
    | AllSubChildren


depthFilterEncoder : DepthFilter -> String
depthFilterEncoder x =
    case x of
        AllSubChildren ->
            "all"

        SelectedNode ->
            "current"


depthFilterDecoder : String -> DepthFilter
depthFilterDecoder x =
    case x of
        "current" ->
            SelectedNode

        _ ->
            AllSubChildren


defaultDepth : String
defaultDepth =
    "all"


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
            "open"


statusFilterDecoder : String -> StatusFilter
statusFilterDecoder x =
    case x of
        "all" ->
            AllStatus

        "closed" ->
            ClosedStatus

        _ ->
            OpenStatus


defaultStatus : String
defaultStatus =
    "open"


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
            "all"


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


defaultType : String
defaultType =
    "all"



{- Authors parameters -}


authorsEncoder : List User -> List ( String, String )
authorsEncoder authors =
    authors |> List.map (\x -> ( "u", x.username ))



--authorsDecoder : List ( String, String ) -> List User
--authorsDecoder authors =
--    authors |> List.map (\x -> ( "u", x.username ))


nfirst : Int
nfirst =
    15



---- MSG ----


type Msg
    = PassedSlowLoadTreshold -- timer
    | PushTension TensionForm (GqlData Tension -> Msg)
    | PushGuest ActionForm
    | Submit (Time.Posix -> Msg) -- Get Current Time
      -- Data Queries
    | GotPath (GqlData LocalGraph) -- GraphQL
    | GotPath2 (GqlData LocalGraph) -- GraphQL
    | GotChildren (WebData (List NodeId)) -- HTTP/Json
    | GotTensionsInt Int (GqlData TensionsData) -- GraphQL
    | GotTensionsExt (GqlData TensionsData) -- GraphQL
      -- Page Action
    | DoLoad Int -- query tensions
    | OnFilterClick
    | ChangePattern String
    | ChangeStatusFilter StatusFilter
    | ChangeTypeFilter TypeFilter
    | ChangeDepthFilter DepthFilter
    | ChangeAuthor
    | SearchKeyDown Int
    | OnClearFilter
    | SubmitSearch
    | GoView TensionsView
      -- Authors
    | UserSearchPanelMsg UserSearchPanel.Msg
      -- JoinOrga Action
    | DoJoinOrga String
    | DoJoinOrga2 (GqlData Node)
    | DoJoinOrga3 Node Time.Posix
    | JoinAck (GqlData ActionResult)
      -- Token refresh
    | DoOpenAuthModal UserCtx
    | DoCloseAuthModal String
    | ChangeAuthPost String String
    | SubmitUser UserAuthForm
    | GotSignin (WebData UserCtx)
    | SubmitKeyDown Int -- Detect Enter (for form sending)
      -- Common
    | NoMsg
    | LogErr String
    | Navigate String
    | DoOpenModal -- ports receive / Open  modal
    | DoCloseModal ModalData -- ports receive / Close modal
    | ExpandRoles
    | CollapseRoles
      -- Help
    | HelpMsg Help.Msg



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

        f =
            Debug.log "authors" (Dict.get "u" query)

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
            , pattern = Dict.get "q" query |> withDefault [] |> List.head
            , initPattern = Dict.get "q" query |> withDefault [] |> List.head
            , viewMode = Dict.get "v" query |> withDefault [] |> List.head |> withDefault "" |> viewModeDecoder
            , statusFilter = Dict.get "s" query |> withDefault [] |> List.head |> withDefault "" |> statusFilterDecoder
            , typeFilter = Dict.get "t" query |> withDefault [] |> List.head |> withDefault "" |> typeFilterDecoder
            , depthFilter = Dict.get "d" query |> withDefault [] |> List.head |> withDefault "" |> depthFilterDecoder
            , authors = Dict.get "u" query |> withDefault [] |> List.map (\x -> User x Nothing)
            , queryIsEmpty = Dict.isEmpty query
            , authorsPanel = UserSearchPanel.init "" SelectUser global.session.user

            -- Common
            , node_action = NoOp
            , isModalActive = False
            , modalAuth = Inactive
            , helperBar = HelperBar.create
            , help = Help.init global.session.user
            , refresh_trial = 0
            }

        cmds =
            [ queryLocalGraph apis.gql newFocus.nameid GotPath
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
update global message model =
    let
        apis =
            global.session.apis
    in
    case message of
        PushTension form ack ->
            ( model, addOneTension apis.gql form ack, Cmd.none )

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
                                    ternary (model.depthFilter == SelectedNode) (send (DoLoad 1)) Cmd.none
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
                                            ternary (model.depthFilter == SelectedNode) (send (DoLoad 1)) Cmd.none
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
                    ( newModel, send (DoLoad 1), Cmd.none )

                _ ->
                    ( newModel, Cmd.none, Cmd.none )

        GotTensionsInt inc result ->
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
            ( { model | tensions_int = newResult, load_more_int = load_more, offset = model.offset + inc }, Cmd.none, Cmd.none )

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

        DoLoad inc ->
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

                nameids =
                    case model.depthFilter of
                        AllSubChildren ->
                            case model.children of
                                RemoteData.Success children ->
                                    children |> List.map (\x -> x.nameid) |> List.append [ model.node_focus.nameid ]

                                _ ->
                                    []

                        SelectedNode ->
                            case model.path_data of
                                Success path ->
                                    path.focus.children |> List.map (\x -> x.nameid) |> List.append [ path.focus.nameid ]

                                _ ->
                                    []
            in
            case nameids of
                [] ->
                    ( model, Cmd.none, Cmd.none )

                _ ->
                    ( model
                    , Cmd.batch
                        [ queryIntTension apis.gql nameids nfirst (model.offset * nfirst) model.pattern status model.authors [] type_ (GotTensionsInt inc)
                        , queryExtTension apis.gql nameids nfirst (model.offset * nfirst) model.pattern status model.authors [] type_ GotTensionsExt
                        ]
                    , Cmd.none
                    )

        OnFilterClick ->
            -- Fix component dropdowns close beaviour
            if UserSearchPanel.isOpen_ model.authorsPanel then
                -- @debug: this one doesnt work every time (race condition ?)
                --( model, Cmd.map UserSearchPanelMsg (send OnClose), Cmd.none )
                ( model, Ports.click "body", Cmd.none )

            else
                ( model, Cmd.none, Cmd.none )

        ChangePattern value ->
            ( { model | pattern = Just value }, Cmd.none, Cmd.none )

        ChangeStatusFilter value ->
            let
                newModel =
                    { model | statusFilter = value }
            in
            ( newModel, send SubmitSearch, Cmd.none )

        ChangeTypeFilter value ->
            let
                newModel =
                    { model | typeFilter = value }
            in
            ( newModel, send SubmitSearch, Cmd.none )

        ChangeDepthFilter value ->
            let
                newModel =
                    { model | depthFilter = value }
            in
            ( newModel, send SubmitSearch, Cmd.none )

        ChangeAuthor ->
            let
                targets =
                    model.path_data |> withMaybeDataMap (\x -> List.map (\y -> y.nameid) x.path) |> withDefault []
            in
            ( model, Cmd.map UserSearchPanelMsg (send (OnOpen targets)), Cmd.none )

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

        OnClearFilter ->
            let
                query =
                    queryBuilder
                        [ ( "v", viewModeEncoder model.viewMode |> (\x -> ternary (x == defaultView) "" x) ) ]
                        |> (\q -> ternary (q == "") "" ("?" ++ q))
            in
            ( model, Nav.pushUrl global.key (uriFromNameid TensionsBaseUri model.node_focus.nameid ++ query), Cmd.none )

        SubmitSearch ->
            let
                query =
                    queryBuilder
                        ([ ( "q", model.pattern |> withDefault "" |> String.trim )
                         , ( "v", viewModeEncoder model.viewMode |> (\x -> ternary (x == defaultView) "" x) )
                         , ( "s", statusFilterEncoder model.statusFilter |> (\x -> ternary (x == defaultStatus) "" x) )
                         , ( "t", typeFilterEncoder model.typeFilter |> (\x -> ternary (x == defaultType) "" x) )
                         , ( "d", depthFilterEncoder model.depthFilter |> (\x -> ternary (x == defaultDepth) "" x) )
                         ]
                            ++ authorsEncoder model.authors
                        )
                        |> (\q -> ternary (q == "") "" ("?" ++ q))
            in
            ( model, Cmd.none, Nav.pushUrl global.key (uriFromNameid TensionsBaseUri model.node_focus.nameid ++ query) )

        GoView viewMode ->
            ( { model | viewMode = viewMode }, Cmd.none, Cmd.none )

        -- Authors
        UserSearchPanelMsg msg ->
            let
                isAssigneeOpen1 =
                    UserSearchPanel.isOpen_ model.authorsPanel

                ( panel, out ) =
                    UserSearchPanel.update apis msg model.authorsPanel

                authors =
                    Maybe.map
                        (\r ->
                            if Tuple.first r == True then
                                model.authors ++ [ Tuple.second r ]

                            else
                                LE.remove (Tuple.second r) model.authors
                        )
                        out.result
                        |> withDefault model.authors

                ( cmds_, gcmds ) =
                    mapGlobalOutcmds out.gcmds

                cmds =
                    if model.authors /= authors then
                        cmds_ ++ [ send (DoLoad 0) ]

                    else
                        cmds_
            in
            ( { model | authorsPanel = panel, authors = authors }, out.cmds |> List.map (\m -> Cmd.map UserSearchPanelMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

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
                    initActionForm tid global.session.user

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
                            , send UpdateUserToken
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

        DoCloseAuthModal link ->
            let
                cmd =
                    ternary (link /= "") (send (Navigate link)) Cmd.none
            in
            case model.node_action of
                JoinOrga _ ->
                    ( { model | modalAuth = Inactive }, Cmd.batch [ cmd, send (DoCloseModal { reset = True, link = "" }) ], Ports.close_auth_modal )

                _ ->
                    ( { model | modalAuth = Inactive }, cmd, Ports.close_auth_modal )

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
                    , Cmd.batch [ send (DoCloseAuthModal ""), cmd ]
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
        NoMsg ->
            ( model, Cmd.none, Cmd.none )

        LogErr err ->
            ( model, Ports.logErr err, Cmd.none )

        Navigate url ->
            ( model, Cmd.none, Nav.pushUrl global.key url )

        DoOpenModal ->
            ( { model | isModalActive = True }, Ports.open_modal "actionModal", Cmd.none )

        DoCloseModal data ->
            let
                gcmd =
                    if data.link /= "" then
                        send (Navigate data.link)

                    else
                        Cmd.none
            in
            ( { model | isModalActive = False }, gcmd, Ports.close_modal )

        ExpandRoles ->
            ( { model | helperBar = HelperBar.expand model.helperBar }, Cmd.none, Cmd.none )

        CollapseRoles ->
            ( { model | helperBar = HelperBar.collapse model.helperBar }, Cmd.none, Cmd.none )

        -- Help
        HelpMsg msg ->
            let
                ( help, out ) =
                    Help.update apis msg model.help

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | help = help }, out.cmds |> List.map (\m -> Cmd.map HelpMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    [ Ports.mcPD Ports.closeModalFromJs LogErr DoCloseModal
    ]
        ++ (UserSearchPanel.subscriptions |> List.map (\s -> Sub.map UserSearchPanelMsg s))
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        |> Sub.batch



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Tensions · " ++ (String.join "/" <| LE.unique [ model.node_focus.rootnameid, model.node_focus.nameid |> String.split "#" |> List.reverse |> List.head |> withDefault "" ])
    , body =
        [ view_ global model
        , Help.view {} model.help |> Html.map HelpMsg
        , refreshAuthModal model.modalAuth { closeModal = DoCloseAuthModal, changePost = ChangeAuthPost, submit = SubmitUser, submitEnter = SubmitKeyDown }
        ]
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
                    [ div [ class "column is-10-desktop is-9-fullhd" ] [ viewSearchBar model ] ]
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
                        button [ class "button is-small", onClick (DoLoad 1) ]
                            [ text "Load more" ]

                      else
                        div [] []
                    ]
                ]
            ]
        , setupActionModal model.isModalActive model.node_action
        ]


viewSearchBar : Model -> Html Msg
viewSearchBar model =
    let
        checked =
            I.icon1 "icon-check has-text-success" ""

        unchecked =
            I.icon1 "icon-check has-text-success is-invisible" ""

        clearFilter =
            if model.queryIsEmpty then
                text ""

            else
                span
                    [ class "tag is-rounded is-small is-danger is-light button-light"
                    , attribute "style" "margin: 0.35rem;"
                    , onClick OnClearFilter
                    ]
                    [ text "Clear filters" ]
    in
    div [ id "searchBarTensions", class "searchBar" ]
        [ div [ class "columns mt-0 mb-0" ]
            [ div [ class "column is-6" ]
                [ div [ class "field has-addons" ]
                    [ div [ class "control has-icons-left is-expanded" ]
                        [ input
                            [ class "is-rounded input is-small autofocus"
                            , type_ "search"
                            , autocomplete False
                            , autofocus True
                            , placeholder "Search tensions"
                            , value (withDefault "" model.pattern)
                            , onInput ChangePattern
                            , onKeydown SearchKeyDown
                            ]
                            []
                        , span [ class "icon is-left" ] [ i [ class "icon-search" ] [] ]
                        ]
                    ]
                ]
            , div [ class "column is-6 flex-gap" ]
                [ div [ class "field has-addons filterBar mb-0" ]
                    [ div [ class "control dropdown", onClick OnFilterClick ]
                        [ div [ class "is-small button dropdown-trigger", attribute "aria-controls" "status-filter" ] [ textH T.status, i [ class "ml-2 icon-chevron-down icon-tiny" ] [] ]
                        , div [ id "status-filter", class "dropdown-menu", attribute "role" "menu" ]
                            [ div
                                [ class "dropdown-content" ]
                                [ div [ class "dropdown-item button-light", onClick <| ChangeStatusFilter AllStatus ]
                                    [ ternary (model.statusFilter == AllStatus) checked unchecked, textH (statusFilterEncoder AllStatus) ]
                                , div [ class "dropdown-item button-light", onClick <| ChangeStatusFilter OpenStatus ]
                                    [ ternary (model.statusFilter == OpenStatus) checked unchecked, textH (statusFilterEncoder OpenStatus) ]
                                , div [ class "dropdown-item button-light", onClick <| ChangeStatusFilter ClosedStatus ]
                                    [ ternary (model.statusFilter == ClosedStatus) checked unchecked, textH (statusFilterEncoder ClosedStatus) ]
                                ]
                            ]
                        ]
                    , div [ class "control", onClick ChangeAuthor ]
                        [ div [ class "is-small button" ] [ text "Author", i [ class "ml-2 icon-chevron-down icon-tiny" ] [] ]
                        , UserSearchPanel.view
                            { selectedAssignees = model.authors
                            , targets = model.path_data |> withMaybeDataMap (\x -> List.map (\y -> y.nameid) x.path) |> withDefault []
                            }
                            model.authorsPanel
                            |> Html.map UserSearchPanelMsg
                        ]
                    , div [ class "control dropdown", onClick OnFilterClick ]
                        [ div [ class "is-small button dropdown-trigger", attribute "aria-controls" "type-filter" ] [ textH T.type_, i [ class "ml-2 icon-chevron-down icon-tiny" ] [] ]
                        , div [ id "type-filter", class "dropdown-menu is-right", attribute "role" "menu" ]
                            [ div
                                [ class "dropdown-content" ]
                                [ div [ class "dropdown-item button-light", onClick <| ChangeTypeFilter AllTypes ]
                                    [ ternary (model.typeFilter == AllTypes) checked unchecked, textH (typeFilterEncoder AllTypes) ]
                                , div [ class "dropdown-item button-light", onClick <| ChangeTypeFilter OperationalType ]
                                    [ ternary (model.typeFilter == OperationalType) checked unchecked, span [ class (tensionTypeColor "text" TensionType.Operational) ] [ textH (typeFilterEncoder OperationalType) ] ]
                                , div [ class "dropdown-item button-light", onClick <| ChangeTypeFilter GovernanceType ]
                                    [ ternary (model.typeFilter == GovernanceType) checked unchecked, span [ class (tensionTypeColor "text" TensionType.Governance) ] [ textH (typeFilterEncoder GovernanceType) ] ]
                                , div [ class "dropdown-item button-light", onClick <| ChangeTypeFilter HelpType ]
                                    [ ternary (model.typeFilter == HelpType) checked unchecked, span [ class (tensionTypeColor "text" TensionType.Help) ] [ textH (typeFilterEncoder HelpType) ] ]
                                ]
                            ]
                        ]
                    , div [ class "control dropdown", onClick OnFilterClick ]
                        [ div [ class "is-small button dropdown-trigger", attribute "aria-controls" "depth-filter" ] [ textH T.depth, i [ class "ml-2 icon-chevron-down icon-tiny" ] [] ]
                        , div [ id "depth-filter", class "dropdown-menu is-right", attribute "role" "menu" ]
                            [ div
                                [ class "dropdown-content" ]
                                [ div [ class "dropdown-item button-light", onClick <| ChangeDepthFilter AllSubChildren ]
                                    [ ternary (model.depthFilter == AllSubChildren) checked unchecked, textH (depthFilterEncoder AllSubChildren) ]
                                , div [ class "dropdown-item button-light", onClick <| ChangeDepthFilter SelectedNode ]
                                    [ ternary (model.depthFilter == SelectedNode) checked unchecked, textH (depthFilterEncoder SelectedNode) ]
                                ]
                            ]
                        ]
                    ]
                , clearFilter
                ]
            ]
        , div [ class "tabs is-md" ]
            [ ul []
                [ li [ classList [ ( "is-active", model.viewMode == ListView ) ] ] [ a [ onClickPD (GoView ListView), target "_blank" ] [ text "List" ] ]
                , li [ classList [ ( "is-active", model.viewMode == IntExtView ) ] ] [ a [ onClickPD (GoView IntExtView), target "_blank" ] [ text "Internal/External" ] ]
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
        [ div [ class "column is-10-desktop is-9-fullhd" ]
            [ viewTensions model.node_focus model.initPattern tensions_d ListTension
            ]
        ]


viewIntExtTensions : Model -> Html Msg
viewIntExtTensions model =
    div [ class "columns" ]
        [ div [ class "column is-6" ]
            [ h2 [ class "subtitle has-text-weight-semibold has-text-centered" ] [ textH T.internalTensions ]
            , viewTensions model.node_focus model.initPattern model.tensions_int InternalTension
            ]
        , div [ class "vline" ] []
        , div [ class "column is-6" ]
            [ h2 [ class "subtitle has-text-weight-semibold has-text-centered" ] [ textH T.externalTensions ]
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
                    div [] [ textH T.noResultsFor, text ": ", text (pattern |> withDefault "") ]

                else
                    case focus.type_ of
                        NodeType.Role ->
                            case tensionDir of
                                InternalTension ->
                                    div [] [ textH T.noIntTensionRole ]

                                ExternalTension ->
                                    div [] [ textH T.noExtTensionRole ]

                                ListTension ->
                                    div [] [ textH T.noTensionRole ]

                        NodeType.Circle ->
                            case tensionDir of
                                InternalTension ->
                                    div [] [ textH T.noIntTensionCircle ]

                                ExternalTension ->
                                    div [] [ textH T.noExtTensionCircle ]

                                ListTension ->
                                    div [] [ textH T.noTensionCircle ]

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
        , class "modal modal-fx-fadeIn"
        , classList [ ( "is-active", isModalActive ) ]
        , attribute "data-modal-close" "closeModalFromJs"
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "actionModal"
            , onClick (DoCloseModal { reset = True, link = "" })
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
            ]
        , button [ class "modal-close is-large", onClick (DoCloseModal { reset = True, link = "" }) ] []
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
                    div [ class "box is-light", onClick (DoCloseModal { reset = True, link = "" }) ]
                        [ I.icon1 "icon-check icon-2x has-text-success" " "
                        , textH T.welcomIn
                        , text " "
                        , span [ class "has-font-weight-semibold" ] [ text form.node.name ]
                        ]

                Failure err ->
                    viewGqlErrors err

                default ->
                    div [ class "box spinner" ] [ text "" ]
