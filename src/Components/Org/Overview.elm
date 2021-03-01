port module Components.Org.Overview exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Array
import Auth exposing (AuthState(..), doRefreshToken, refreshAuthModal)
import Browser.Navigation as Nav
import Codecs exposing (LocalGraph_, LookupResult, Node_, QuickDoc, WindowPos, localGraphDecoder, nodeDecoder)
import Components.ActionPanel as ActionPanel exposing (ActionPanel, ActionPanelState(..), ActionStep(..))
import Components.DocToolBar as DocToolBar
import Components.Help as Help
import Components.HelperBar as HelperBar exposing (HelperBar)
import Components.I as I
import Components.Loading as Loading exposing (GqlData, ModalData, RequestResult(..), WebData, isFailure, viewAuthNeeded, viewGqlErrors, viewHttpErrors, withDefaultData, withMapData, withMaybeData, withMaybeDataMap)
import Components.NodeDoc as NodeDoc exposing (nodeFragmentFromOrga)
import Debug
import Dict exposing (Dict)
import Extra exposing (ternary)
import Extra.Events exposing (onClickPD, onKeydown)
import Form exposing (isPostSendable)
import Form.NewTension as NTF exposing (Msg(..), TensionTab(..))
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Global exposing (Msg(..), send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, span, table, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, autocomplete, class, classList, disabled, href, id, list, name, placeholder, required, rows, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import Json.Decode as JD
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs
    exposing
        ( ActionType(..)
        , DocType(..)
        , Flags_
        , FractalBaseRoute(..)
        , NodeFocus
        , TensionCharac
        , focusFromNameid
        , focusFromPath
        , focusState
        , getCircleRoles
        , getCoordoRoles
        , getOrgaRoles
        , guestIdCodec
        , isOwner
        , nameidFromFlags
        , nearestCircleid
        , nid2rootid
        , uriFromNameid
        , uriFromUsername
        )
import ModelCommon.Requests exposing (login)
import ModelCommon.View exposing (getAvatar, mediaTension, tensionTypeColor, viewUsernameLink)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.AddTension exposing (addOneTension)
import Query.PatchTension exposing (actionRequest)
import Query.QueryNode exposing (fetchNode, queryGraphPack, queryNodesSub)
import Query.QueryNodeData exposing (queryNodeData)
import Query.QueryTension exposing (queryAllTension)
import RemoteData exposing (RemoteData)
import Task
import Text as T exposing (textH, textT, upH)
import Time



---- PROGRAM ----


type alias Flags =
    Flags_


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

                    DoFetchNode nameid ->
                        ( send (FetchNewNode nameid), Cmd.none )

                    DoPushTension tension ->
                        ( send (PushTension tension), Cmd.none )

                    _ ->
                        ( Cmd.none, Cmd.none )
            )
        |> List.unzip



---- MODEL----


type alias Model =
    { node_focus : NodeFocus
    , path_data : Maybe LocalGraph
    , users_data : GqlData UsersData
    , orga_data : GqlData NodesData
    , tensions_data : GqlData TensionsData
    , node_data : GqlData NodeData
    , init_tensions : Bool
    , init_data : Bool
    , node_quickSearch : NodesQuickSearch
    , window_pos : WindowPos

    -- Form
    , tensionForm : NTF.State

    -- Node Action
    , actionPanel : ActionPanel

    -- common
    , node_action : ActionState
    , isModalActive : Bool
    , modalAuth : ModalAuth
    , helperBar : HelperBar
    , help : Help.State
    , refresh_trial : Int
    }


nfirstTensions : Int
nfirstTensions =
    10



---- MSG ----


type Msg
    = PassedSlowLoadTreshold -- timer
    | LoadOrga
    | PushAction ActionForm ActionPanelState
    | PushGuest ActionForm
    | PushTension Tension
    | Submit (Time.Posix -> Msg) -- Get the current time
      -- Gql Data Queries
    | GotOrga (GqlData NodesData)
    | GotTensions (GqlData TensionsData)
    | GotData (GqlData NodeData)
      -- Page
    | SwitchWindow
      -- Quick search
    | LookupFocus String (Maybe LocalGraph)
    | LookupBlur
    | ChangePattern String
    | ChangeNodeLookup (LookupResult Node)
    | SearchKeyDown Int
      -- New Tension triggers
    | DoNodeAction Node_ -- ports receive / tooltip click
    | DoNewTension Node -- {target}
      -- New tension
    | NewTensionMsg NTF.Msg
      -- Node Settings
    | DoActionEdit Node
    | CancelAction
    | OpenActionPanelModal ActionPanelState
    | CloseActionPanelModal String
      --| ActionStep1 XXX
    | ActionSubmit Time.Posix
    | ArchiveDocAck (GqlData ActionResult)
    | LeaveRoleAck (GqlData ActionResult)
    | UpdateActionPost String String
      -- JoinOrga Action
    | DoJoinOrga String
    | DoJoinOrga2 (GqlData Node)
    | DoJoinOrga3 Node Time.Posix
    | JoinAck (GqlData ActionResult)
      -- Graphpack
    | FetchNewNode String
    | NewNodesAck (GqlData (List Node))
    | AddNodes (List Node)
    | UpdateNode (Maybe Node)
    | DelNodes (List String)
      -- GP JS Interop
    | NodeClicked String -- ports receive / Node clicked
    | NodeFocused LocalGraph_ -- ports receive / Node focused
    | DoClearTooltip -- ports send
    | ToggleGraphReverse -- ports send
      -- Token refresh
    | DoOpenAuthModal UserCtx -- ports receive / Open  modal
    | DoCloseAuthModal -- ports receive / Close modal
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


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        session =
            global.session

        apis =
            session.apis

        -- Focus
        newFocus =
            flags
                |> nameidFromFlags
                |> focusFromNameid

        -- What has changed
        fs_ =
            focusState OverviewBaseUri session.referer session.node_focus newFocus

        isInit =
            session.orga_data == Nothing

        fs =
            { fs_ | isInit = fs_.isInit || isInit }

        --d1 = Debug.log "isInit, orgChange, focuChange, refresh" [ fs.isInit, fs.orgChange, fs.focusChange, fs.refresh ]
        --d2 = Debug.log "newfocus" [ newFocus ]
        -- QuickSearch
        qs =
            session.node_quickSearch |> withDefault { pattern = "", lookup = Array.empty, idx = 0, visible = False }

        -- Model init
        model =
            { node_focus = newFocus
            , path_data = ternary fs.orgChange Nothing global.session.path_data -- Loaded from GraphPack
            , users_data =
                global.session.users_data
                    |> Maybe.map (\x -> ternary fs.orgChange Loading (Success x))
                    |> withDefault Loading
            , orga_data =
                session.orga_data
                    |> Maybe.map (\x -> ternary fs.orgChange Loading (Success x))
                    |> withDefault Loading
            , tensions_data =
                session.tensions_data
                    |> Maybe.map (\x -> ternary fs.orgChange Loading (Success x))
                    |> withDefault Loading
            , node_data =
                session.node_data
                    |> Maybe.map (\x -> ternary fs.orgChange Loading (Success x))
                    |> withDefault Loading
            , init_tensions = True
            , init_data = True
            , node_quickSearch = { qs | pattern = "", idx = 0 }
            , window_pos =
                global.session.window_pos
                    |> withDefault { two = "doc", three = "activities" }

            -- Form
            , tensionForm = NTF.init global.session.user

            -- Node Action
            , actionPanel = ActionPanel.init "" global.session.user

            -- Common
            , node_action = withDefault NoOp global.session.node_action
            , isModalActive = False
            , modalAuth = Inactive
            , helperBar = HelperBar.create
            , refresh_trial = 0
            , help = Help.init global.session.user
            }

        cmds =
            if fs.orgChange || isInit then
                [ send LoadOrga
                , Ports.initGraphPack Dict.empty "" -- canvas loading effet

                --, queryCircleTension apis.gql newFocus.nameid GotTensions
                , queryNodeData apis.gql newFocus.nameid GotData
                ]

            else if fs.focusChange then
                [ queryNodeData apis.gql newFocus.nameid GotData

                --queryCircleTension apis.gql newFocus.nameid GotTensions
                ]
                    ++ (if fs.refresh then
                            case session.orga_data of
                                Just ndata ->
                                    [ send LoadOrga
                                    , Ports.initGraphPack Dict.empty "" --canvas loading effect
                                    ]

                                Nothing ->
                                    []

                        else
                            [ Ports.focusGraphPack newFocus.nameid ]
                       )

            else if fs.refresh then
                [ send LoadOrga
                , Ports.initGraphPack Dict.empty "" --canvas loading effect

                --, queryCircleTension apis.gql newFocus.nameid GotTensions
                , queryNodeData apis.gql newFocus.nameid GotData
                ]

            else
                []

        model2 =
            ternary (cmds == []) { model | init_tensions = False, init_data = False } model
    in
    ( model2
    , Cmd.batch (cmds ++ [ sendSleep PassedSlowLoadTreshold 500 ])
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
        LoadOrga ->
            ( model, queryGraphPack apis.gql model.node_focus.rootnameid GotOrga, Cmd.none )

        PushAction form state ->
            let
                ackMsg =
                    case state of
                        ArchiveAction ->
                            ArchiveDocAck

                        UnarchiveAction ->
                            ArchiveDocAck

                        LeaveAction ->
                            LeaveRoleAck

                        NoAction ->
                            \x -> NoMsg
            in
            ( model, actionRequest apis.gql form ackMsg, Cmd.none )

        PushGuest form ->
            ( model, actionRequest apis.gql form JoinAck, Cmd.none )

        PassedSlowLoadTreshold ->
            let
                orga_data =
                    ternary (model.orga_data == Loading) LoadingSlowly model.orga_data

                tensions_data =
                    ternary (model.tensions_data == Loading) LoadingSlowly model.tensions_data

                node_data =
                    ternary (model.node_data == Loading) LoadingSlowly model.node_data
            in
            ( { model | orga_data = orga_data, tensions_data = tensions_data, node_data = node_data }, Cmd.none, Cmd.none )

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        --  Page
        SwitchWindow ->
            let
                win =
                    model.window_pos

                newWin =
                    { win | two = win.three, three = win.two }
            in
            ( { model | window_pos = newWin }, Cmd.none, send (UpdateSessionWindow (Just newWin)) )

        -- Data queries
        GotOrga result ->
            case doRefreshToken result model.refresh_trial of
                Authenticate ->
                    let
                        uctx =
                            case global.session.user of
                                LoggedIn u ->
                                    u

                                LoggedOut ->
                                    initUserctx
                    in
                    ( model, send (DoOpenAuthModal uctx), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep LoadOrga 500, send UpdateUserToken )

                OkAuth data ->
                    let
                        users =
                            orgaToUsersData data

                        users_l =
                            Dict.values users |> List.concat |> LE.uniqueBy (\u -> u.username)
                    in
                    if Dict.size data > 0 then
                        ( { model | orga_data = Success data, users_data = Success users }
                        , Cmd.batch [ Ports.initGraphPack data model.node_focus.nameid, Ports.initUserSearch users_l ]
                        , send (UpdateSessionOrga (Just data))
                        )

                    else
                        ( { model | orga_data = Failure [ T.nodeNotExist ] }, Cmd.none, Cmd.none )

                NoAuth ->
                    ( { model | orga_data = result }, Cmd.none, send (UpdateSessionOrga Nothing) )

        GotTensions result ->
            case result of
                Success data ->
                    ( { model | tensions_data = result, init_tensions = False }, Cmd.none, send (UpdateSessionTensions (Just data)) )

                other ->
                    ( { model | tensions_data = result }, Cmd.none, send (UpdateSessionTensions Nothing) )

        GotData result ->
            case result of
                Success data ->
                    ( { model | node_data = result, init_data = False }, Cmd.none, send (UpdateSessionData (Just data)) )

                other ->
                    ( { model | node_data = result }, Cmd.none, Cmd.none )

        -- Search
        LookupFocus pattern path_m ->
            case pattern of
                "" ->
                    case path_m of
                        Just path ->
                            case model.orga_data of
                                Success data ->
                                    let
                                        qs =
                                            model.node_quickSearch

                                        newLookup =
                                            path.focus.children
                                                |> List.map (\n -> Dict.get n.nameid data)
                                                |> List.filterMap identity
                                                |> Array.fromList
                                    in
                                    ( { model | node_quickSearch = { qs | lookup = newLookup, visible = True, idx = 0 } }, Cmd.none, Cmd.none )

                                _ ->
                                    ( model, Cmd.none, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        LookupBlur ->
            let
                qs =
                    model.node_quickSearch
            in
            ( { model | node_quickSearch = { qs | visible = False } }, Cmd.none, Cmd.none )

        ChangePattern pattern ->
            let
                qs =
                    model.node_quickSearch

                cmd =
                    if pattern == "" then
                        LookupFocus pattern model.path_data |> send

                    else
                        Ports.searchNode pattern
            in
            ( { model | node_quickSearch = { qs | pattern = pattern, visible = True } }
            , cmd
            , Cmd.none
            )

        ChangeNodeLookup nodes_ ->
            let
                qs =
                    model.node_quickSearch
            in
            case nodes_ of
                Ok nodes ->
                    ( { model | node_quickSearch = { qs | lookup = Array.fromList nodes } }, Cmd.none, Cmd.none )

                Err err ->
                    ( model, Ports.logErr err, Cmd.none )

        SearchKeyDown key ->
            let
                qs =
                    model.node_quickSearch

                len =
                    Array.length qs.lookup
            in
            case key of
                13 ->
                    --ENTER
                    case Array.get model.node_quickSearch.idx model.node_quickSearch.lookup of
                        Just n ->
                            ( model, send (NodeClicked n.nameid), Cmd.none )

                        Nothing ->
                            ( model, Cmd.none, Cmd.none )

                27 ->
                    --ESC
                    ( model, Cmd.batch [ send LookupBlur ], Cmd.none )

                40 ->
                    --DOWN
                    let
                        newIdx =
                            if len > 0 && qs.idx < len - 1 then
                                qs.idx + 1

                            else
                                0
                    in
                    ( { model | node_quickSearch = { qs | idx = newIdx } }, Cmd.none, Cmd.none )

                38 ->
                    --UP
                    let
                        newIdx =
                            if len > 0 && qs.idx > 0 then
                                qs.idx - 1

                            else if len > 0 && qs.idx == 0 then
                                len - 1

                            else
                                0
                    in
                    ( { model | node_quickSearch = { qs | idx = newIdx } }, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        -- New tension triggers
        DoNodeAction node_ ->
            case node_ of
                Ok node ->
                    ( model, send (DoNewTension node), Cmd.none )

                Err err ->
                    ( { model | node_action = AskErr err }, send DoOpenModal, Cmd.none )

        DoNewTension target ->
            let
                tf =
                    model.tensionForm |> NTF.setTarget_ target (withMaybeData model.node_data)
            in
            ( { model | tensionForm = tf }, Cmd.map NewTensionMsg (send OnOpen), Cmd.none )

        -- New tension
        NewTensionMsg msg ->
            let
                ( tf, out ) =
                    NTF.update apis msg model.tensionForm

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | tensionForm = tf }, out.cmds |> List.map (\m -> Cmd.map NewTensionMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        -- Node Action
        DoActionEdit node ->
            if model.actionPanel.isEdit == False then
                let
                    rootSource =
                        getNode node.rootnameid model.orga_data |> Maybe.map (\n -> n.source) |> withDefault Nothing

                    ( tid, bid ) =
                        node.source
                            |> Maybe.map (\b -> ( b.tension.id, b.id ))
                            |> withDefault
                                (rootSource
                                    |> Maybe.map (\b -> ( b.tension.id, b.id ))
                                    |> withDefault ( "", "" )
                                )

                    panel =
                        model.actionPanel
                            |> ActionPanel.open bid
                            |> ActionPanel.setTid tid
                            |> ActionPanel.setNode node
                in
                ( { model | actionPanel = panel }
                , Ports.outsideClickClose "cancelActionFromJs" "actionPanelContent"
                , Cmd.none
                )

            else
                ( model, send CancelAction, Cmd.none )

        CancelAction ->
            ( { model | actionPanel = ActionPanel.close model.actionPanel }, Cmd.none, Cmd.none )

        OpenActionPanelModal action ->
            let
                panel =
                    model.actionPanel
                        |> ActionPanel.activateModal
                        |> ActionPanel.setAction action
                        |> ActionPanel.setStep StepOne
            in
            ( { model | actionPanel = panel }, Ports.open_modal "actionPanelModal", Cmd.none )

        CloseActionPanelModal link ->
            let
                gcmds =
                    if link /= "" then
                        [ send (Navigate link) ]

                    else if ActionPanel.isSuccess model.actionPanel then
                        [ case model.actionPanel.state of
                            ArchiveAction ->
                                send (DelNodes [ model.actionPanel.form.node.nameid ])

                            UnarchiveAction ->
                                Cmd.none

                            LeaveAction ->
                                let
                                    newNode =
                                        getNode model.actionPanel.form.node.nameid model.orga_data
                                            |> Maybe.map (\n -> { n | first_link = Nothing })
                                in
                                case newNode |> Maybe.map (\n -> n.role_type) |> withDefault Nothing of
                                    Just RoleType.Guest ->
                                        send (DelNodes [ model.actionPanel.form.node.nameid ])

                                    _ ->
                                        send (UpdateNode newNode)

                            NoAction ->
                                Cmd.none
                        ]

                    else
                        [ Cmd.none ]
            in
            ( { model | actionPanel = ActionPanel.terminate model.actionPanel }, Cmd.batch gcmds, Ports.close_modal )

        ActionSubmit time ->
            let
                panel =
                    model.actionPanel
                        |> ActionPanel.post "createdAt" (fromTime time)
                        |> ActionPanel.setActionResult LoadingSlowly
            in
            ( { model | actionPanel = panel }
            , send (PushAction panel.form panel.state)
            , Cmd.none
            )

        ArchiveDocAck result ->
            let
                panel =
                    model.actionPanel
                        |> ActionPanel.close
                        |> ActionPanel.setActionResult result
            in
            case doRefreshToken result model.refresh_trial of
                Authenticate ->
                    ( { model | actionPanel = ActionPanel.setActionResult NotAsked model.actionPanel }, send (DoOpenAuthModal panel.form.uctx), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep (PushAction panel.form panel.state) 500, send UpdateUserToken )

                OkAuth t ->
                    ( { model | actionPanel = panel }, Cmd.none, Cmd.none )

                NoAuth ->
                    ( { model | actionPanel = panel }, Cmd.none, Cmd.none )

        LeaveRoleAck result ->
            let
                panel =
                    model.actionPanel
                        |> ActionPanel.close
                        |> ActionPanel.setActionResult result

                gcmds =
                    [ Ports.click "body" ]
            in
            case doRefreshToken result model.refresh_trial of
                Authenticate ->
                    ( { model | actionPanel = ActionPanel.setActionResult NotAsked model.actionPanel }, send (DoOpenAuthModal panel.form.uctx), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep (PushAction panel.form panel.state) 500, send UpdateUserToken )

                OkAuth t ->
                    ( { model | actionPanel = panel }, Cmd.none, Cmd.none )

                NoAuth ->
                    ( { model | actionPanel = panel }, Cmd.batch gcmds, Cmd.none )

        UpdateActionPost field value ->
            ( { model | actionPanel = model.actionPanel |> ActionPanel.post field value }, Cmd.none, Cmd.none )

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
                                , ( "old", "" )
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
                            , queryNodesSub apis.gql (guestIdCodec form.node.nameid form.uctx.username) NewNodesAck
                            , Cmd.none
                            )

                        NoAuth ->
                            ( { model | node_action = JoinOrga (JoinValidation form result) }, Cmd.none, Cmd.none )

                default ->
                    ( model, Cmd.none, Cmd.none )

        -- Graphpack
        PushTension tension ->
            let
                tensions =
                    hotTensionPush tension model.tensions_data

                --
            in
            ( { model | tensions_data = Success tensions }, Cmd.none, send (UpdateSessionTensions (Just tensions)) )

        FetchNewNode nameid ->
            ( model, queryNodesSub apis.gql nameid NewNodesAck, Cmd.none )

        NewNodesAck result ->
            case result of
                Success nodes ->
                    ( model, send (AddNodes nodes), Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        AddNodes nodes ->
            let
                ndata =
                    hotNodePush nodes model.orga_data
            in
            ( { model | orga_data = Success ndata }
            , Cmd.batch [ Ports.addQuickSearchNodes nodes, nodes |> List.map (\n -> n.first_link) |> List.filterMap identity |> Ports.addQuickSearchUsers ]
            , Cmd.batch [ send UpdateUserToken, send (UpdateSessionOrga (Just ndata)) ]
            )

        UpdateNode node_m ->
            case node_m of
                Just n ->
                    let
                        ndata =
                            hotNodeInsert n model.orga_data
                    in
                    ( { model | orga_data = Success ndata }
                    , Cmd.none
                    , Cmd.batch [ send UpdateUserToken, send (UpdateSessionOrga (Just ndata)) ]
                    )

                Nothing ->
                    ( model, Cmd.none, Cmd.none )

        DelNodes nameids ->
            let
                ndata =
                    hotNodePull nameids model.orga_data

                newFocus =
                    nameids
                        |> List.head
                        |> Maybe.map (\nid -> getParentId nid model.orga_data)
                        |> withDefault Nothing
                        |> withDefault model.node_focus.rootnameid
            in
            ( { model | orga_data = Success ndata }
              --, Cmd.batch [ Ports.addQuickSearchNodes nodes, nodes |> List.map (\n -> n.first_link) |> List.filterMap identity |> Ports.addQuickSearchUsers ]
            , Cmd.batch [ send (NodeClicked newFocus) ]
            , Cmd.batch [ send UpdateUserToken, send (UpdateSessionOrga (Just ndata)) ]
            )

        -- JS interop
        NodeClicked nameid ->
            ( model
            , Cmd.none
            , Nav.replaceUrl global.key (uriFromNameid OverviewBaseUri nameid)
            )

        NodeFocused path_ ->
            case path_ of
                Ok path ->
                    -- May change the node_focus var
                    let
                        nameids =
                            path.focus.children |> List.map (\x -> x.nameid) |> List.append [ path.focus.nameid ]

                        cmd =
                            queryAllTension apis.gql nameids nfirstTensions 0 Nothing (Just TensionStatus.Open) Nothing GotTensions
                    in
                    ( { model | path_data = Just path }
                    , Cmd.batch [ Ports.drawButtonsGraphPack, cmd, Ports.bulma_driver "" ]
                    , send (UpdateSessionPath (Just path))
                    )

                Err err ->
                    ( model, Ports.logErr err, Cmd.none )

        ToggleGraphReverse ->
            ( model, () |> sendToggleGraphReverse, Cmd.none )

        DoClearTooltip ->
            ( model, Cmd.none, Ports.clearTooltip )

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
                    ( { model | modalAuth = Inactive }, send (DoCloseModal { reset = True, link = "" }), Ports.close_auth_modal )

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
subscriptions _ _ =
    [ nodeClickedFromJs NodeClicked
    , nodeFocusedFromJs_ NodeFocused
    , nodeDataFromJs_ DoNodeAction
    , Ports.lookupNodeFromJs ChangeNodeLookup
    , Ports.cancelActionFromJs (always CancelAction)
    , Ports.mcPD Ports.closeModalFromJs LogErr DoCloseModal
    ]
        ++ (NTF.subscriptions |> List.map (\s -> Sub.map NewTensionMsg s))
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        |> Sub.batch



-- Receive from Javascript


port nodeClickedFromJs : (String -> msg) -> Sub msg


port nodeFocusedFromJs : (JD.Value -> msg) -> Sub msg


port nodeDataFromJs : (JD.Value -> a) -> Sub a


nodeDataFromJs_ : (Node_ -> msg) -> Sub msg
nodeDataFromJs_ object =
    nodeDataFromJs
        (object
            << (\x ->
                    case x of
                        Ok n ->
                            Ok n

                        Err err ->
                            Err (JD.errorToString err)
               )
            << JD.decodeValue nodeDecoder
        )


nodeFocusedFromJs_ : (LocalGraph_ -> msg) -> Sub msg
nodeFocusedFromJs_ object =
    nodeFocusedFromJs
        (object
            << (\x ->
                    case x of
                        Ok n ->
                            Ok n

                        Err err ->
                            Err (JD.errorToString err)
               )
            << JD.decodeValue localGraphDecoder
        )



-- Send to Javascript


port sendToggleGraphReverse : () -> Cmd msg



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Overview · " ++ (String.join "/" <| LE.unique [ model.node_focus.rootnameid, model.node_focus.nameid |> String.split "#" |> List.reverse |> List.head |> withDefault "" ])
    , body =
        [ view_ global model
        , refreshAuthModal model.modalAuth { closeModal = DoCloseAuthModal, changePost = ChangeAuthPost, submit = SubmitUser, submitEnter = SubmitKeyDown }
        , Help.view {} model.help |> Html.map HelpMsg
        , NTF.view
            { users_data = model.users_data
            , targets = getParents (NTF.getTargetid model.tensionForm) model.orga_data
            }
            model.tensionForm
            |> Html.map NewTensionMsg
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    let
        focus_m =
            getNode model.node_focus.nameid model.orga_data

        tid =
            focus_m |> Maybe.map (\nd -> nd.source |> Maybe.map (\b -> b.tension.id)) |> withDefault Nothing |> withDefault ""

        roletype =
            focus_m |> Maybe.map (\n -> n.role_type) |> withDefault Nothing

        nodeData_ =
            { data = withMapData (\x -> tid) model.node_data
            , node = initNodeFragment Nothing
            , isLazy = model.init_data
            , source = OverviewBaseUri
            , hasBeenPushed = True
            , toolbar = ternary (roletype /= Just RoleType.Guest) (Just (DocToolBar.view { focus = model.node_focus, tid = tid, actionView = Nothing })) Nothing
            , receiver = nearestCircleid model.node_focus.nameid
            }

        nodeData =
            case model.orga_data of
                Success d ->
                    let
                        node =
                            Dict.get model.node_focus.nameid d
                    in
                    case model.path_data of
                        Just path ->
                            if List.length path.path > 0 then
                                { nodeData_ | node = nodeFragmentFromOrga node model.node_data path.focus.children d }

                            else
                                { nodeData_ | data = Failure [ T.nodeNotExist ] }

                        Nothing ->
                            nodeData_

                other ->
                    nodeData_

        helperData =
            { onJoin = DoJoinOrga model.node_focus.rootnameid
            , onExpand = ExpandRoles
            , onCollapse = CollapseRoles
            , user = global.session.user
            , path_data = model.path_data
            , baseUri = OverviewBaseUri
            , data = model.helperBar
            }

        viewFromPos : String -> Html Msg
        viewFromPos pos =
            case pos of
                "doc" ->
                    NodeDoc.view nodeData Nothing

                "activities" ->
                    viewActivies model

                _ ->
                    text "Wrong position"
    in
    -- [div [ class "column is-1 is-fullheight is-hidden-mobile", id "leftPane" ] [ viewLeftPane model ]
    div [ id "mainPane" ]
        [ HelperBar.view helperData
        , div [ class "columns is-centered is-variable is-4" ]
            [ div [ class "column is-6-desktop is-5-widescreen is-4-fullhd" ]
                [ viewSearchBar global.session.user model
                , viewCanvas global.session.user model
                , br [] []
                , viewFromPos model.window_pos.two
                ]
            , div [ class "divider is-vertical is-hidden-mobile", onClick SwitchWindow ] [ text "⇋" ]
            , div
                [ class "column is-5-desktop is-6-widescreen is-5-fullhd" ]
                [ div [ class "columns is-gapless" ]
                    [ div [ class "column is-12", id "nextToChart" ]
                        [ viewFromPos model.window_pos.three ]
                    ]
                ]
            ]
        , setupActionModal model
        ]


viewLeftPane : Model -> Html Msg
viewLeftPane model =
    nav [ class "menu" ]
        [ p [ class "menu-label" ]
            [ div [ class "hero is-small is-primary is-bold" ]
                [ div [ class "hero-body has-text-centered" ] [ text model.node_focus.rootnameid ] ]
            ]
        , ul [ class "menu-list" ]
            [ li [ class "menu-label" ]
                [ div [ class "hero is-small is-info is-bold" ]
                    [ div [ class "hero-body" ]
                        [ I.icon1 "icon-circle icon-lg" model.node_focus.nameid ]
                    ]
                ]
            , li []
                [ ul [ class "menu-list" ]
                    [ li [] [ a [] [ I.icon1 "icon-book-open icon-xs" "Mandates" ] ]
                    , li [] [ a [] [ I.icon1 "icon-exchange icon-xs" "Tensions" ] ]
                    , li [] [ a [] [ I.icon1 "icon-history icon-xs" "Journal" ] ]
                    , li []
                        [ a [] [ I.icon1 "icon-user icon-xs" "Members" ]
                        ]
                    ]
                ]
            ]
        ]


viewSearchBar : UserState -> Model -> Html Msg
viewSearchBar us model =
    let
        node_ =
            model.path_data
                |> Maybe.map
                    (\p ->
                        case model.orga_data of
                            Success d ->
                                Dict.get p.focus.nameid d
                                    |> Maybe.map (\n -> Ok n)
                                    |> withDefault (Err "Node not found")

                            other ->
                                Err "No nodes data"
                    )
                |> withDefault (Err "No path returned")

        qs =
            model.node_quickSearch
    in
    div
        [ id "searchBarOverview" ]
        [ div
            [ class "field has-addons searchBar"
            , onMouseEnter DoClearTooltip
            ]
            ([ div [ class "control has-icons-left is-expanded" ]
                [ input
                    [ class "input is-small"
                    , type_ "search"
                    , autocomplete False
                    , placeholder (upH T.phQS)
                    , value qs.pattern
                    , onInput ChangePattern
                    , onFocus (LookupFocus qs.pattern model.path_data)
                    , onClick (LookupFocus qs.pattern model.path_data)
                    , onBlur LookupBlur
                    , onKeydown SearchKeyDown

                    --, list "searchList" -- impossible interaction !
                    ]
                    []
                , span [ class "icon is-left" ] [ i [ class "icon-search" ] [] ]
                ]
             ]
                ++ (case node_ of
                        Ok node ->
                            [ div [ class "control controlButtons" ]
                                [ span
                                    [ class "button is-small is-info is-ellipsis"
                                    , attribute "data-modal" "actionModal"
                                    , onClick (DoNodeAction node_)
                                    ]
                                    [ span [ class "has-text-weight-bold is-ellipsis" ] [ text node.name ]
                                    , i [ class "icon-plus1 ellipsisArt" ] []
                                    ]
                                ]
                            , case us of
                                LoggedIn uctx ->
                                    let
                                        isAdmin =
                                            List.length (getNewNodeRights uctx node model.orga_data) > 0

                                        hasRole =
                                            Just uctx.username == Maybe.map (\fs -> fs.username) node.first_link

                                        hasConfig =
                                            isAdmin || hasRole
                                    in
                                    if hasConfig then
                                        let
                                            panelData =
                                                { tc = Just { action_type = EDIT, doc_type = NODE }
                                                , isAdmin = isAdmin
                                                , hasRole = hasRole
                                                , isRight = True
                                                , data = model.actionPanel
                                                , onSubmit = Submit
                                                , onOpenModal = OpenActionPanelModal
                                                , onCloseModal = CloseActionPanelModal
                                                , onNavigate = Navigate
                                                , onActionSubmit = ActionSubmit
                                                , onUpdatePost = UpdateActionPost
                                                }
                                        in
                                        div [ id "actionPanelContent", class "control" ]
                                            [ span [ class "button is-small is-info", onClick (DoActionEdit node) ]
                                                [ i [ class "icon-ellipsis-v" ] [] ]
                                            , ActionPanel.view panelData
                                            ]

                                    else
                                        text ""

                                LoggedOut ->
                                    text ""
                            ]

                        Err err ->
                            []
                   )
            )
        , div [ class "control" ]
            [ viewSearchList us model
            ]
        ]


viewSearchList : UserState -> Model -> Html Msg
viewSearchList us model =
    let
        qs =
            model.node_quickSearch

        sortedLookup =
            qs.lookup
                |> Array.toList
                |> List.sortWith
                    (\n1 n2 ->
                        case ( n1.type_, n2.type_ ) of
                            ( NodeType.Circle, NodeType.Role ) ->
                                LT

                            ( NodeType.Role, NodeType.Circle ) ->
                                GT

                            _ ->
                                compare n1.name n2.name
                    )

        isHidden =
            (qs.visible == False) || (qs.pattern == "" && sortedLookup == [])
    in
    div
        [ id "searchList", classList [ ( "is-hidden", isHidden ) ] ]
        [ table [ class "table is-fullwidth" ] <|
            if sortedLookup == [] then
                [ tbody [] [ td [] [ textH T.noResultsFound ] ] ]

            else
                sortedLookup
                    |> List.indexedMap
                        (\i n ->
                            [ tr
                                [ class "button-light"
                                , classList [ ( "is-active", i == qs.idx ) ]
                                , onClickPD (NodeClicked n.nameid)
                                ]
                              <|
                                [ th [] [ text n.name ] ]
                                    ++ (case n.type_ of
                                            NodeType.Circle ->
                                                [ td [] [ n.parent |> Maybe.map (\p -> p.nameid |> String.split "#" |> List.reverse |> List.head |> withDefault "") |> withDefault "" |> text ]
                                                , td [] [ n.first_link |> Maybe.map (\p -> "@" ++ p.username) |> withDefault "--" |> text ]

                                                --, td [] [ n.first_link |> Maybe.map (\p -> viewUsernameLink p.username) |> withDefault (text "--") ]
                                                ]

                                            NodeType.Role ->
                                                [ td [] [ n.parent |> Maybe.map (\p -> p.nameid |> String.split "#" |> List.reverse |> List.head |> withDefault "") |> withDefault "" |> text ]
                                                , td [] [ n.first_link |> Maybe.map (\p -> "@" ++ p.username) |> withDefault "--" |> text ]
                                                ]
                                       )
                            ]
                                |> List.append
                                    (if i == 0 && n.type_ == NodeType.Circle then
                                        [ td [ class "is-grey is-aligned-center is-size-6" ] [ text (" " ++ upH T.circle ++ " ") ] ]

                                     else if i == 0 || n.type_ == NodeType.Role && (Array.get (i - 1) (Array.fromList sortedLookup) |> Maybe.map (\x -> x.type_ == NodeType.Circle) |> withDefault False) == True then
                                        [ td [ class "is-grey is-aligned-center is-size-6" ] [ text (" " ++ upH T.role ++ " ") ] ]

                                     else
                                        []
                                    )
                        )
                    |> List.concat
                    |> tbody []
                    |> List.singleton
                    |> List.append [ thead [] [ tr [] [ th [] [ textH T.name ], th [] [ textH T.parent ], th [] [ textH T.firstLink ] ] ] ]
        ]


viewCanvas : UserState -> Model -> Html Msg
viewCanvas us model =
    div [ id "canvasParent", classList [ ( "spinner", model.orga_data == LoadingSlowly ) ] ]
        [ case model.orga_data of
            Failure err ->
                viewGqlErrors err

            Success d ->
                if Dict.get model.node_focus.nameid d == Nothing then
                    viewGqlErrors [ "Node is archived, hidden or has moved." ]

                else
                    text ""

            _ ->
                text ""
        , canvas [ id "canvasOrga", class "is-invisible" ] []
        , div [ id "canvasButtons", class "buttons are-small is-invisible" ]
            -- Hidden class use in graphpack_d3.js
            [ div
                [ id "invGraph_cvbtn"
                , class "button buttonToggle tooltip has-tooltip-right"
                , attribute "data-tooltip" (upH T.reverseTooltip)
                , onClick ToggleGraphReverse
                ]
                [ I.icon "icon-sort-amount-desc icon-xs" ]
            ]
        , div
            [ id "nodeTooltip"
            , class "is-invisible"
            , attribute "data-modal" "actionModal"
            ]
            [ span [] [ text "void" ] -- Node name
            , i [ class "icon-plus1 ellipsisArt" ] []

            --, div [ id "actionPanelContent", class "control" ]
            --    [ span
            --        [ class "button is-small is-info" ]
            --        [ i [ class "icon-ellipsis-v" ] [] ]
            --    ]
            ]
        ]


viewActivies : Model -> Html Msg
viewActivies model =
    div
        [ id "activities", class "box is-flex-grow" ]
        [ div [ class "title" ]
            [ div
                [ class "level" ]
                [ div [ class "level-left" ]
                    [ div
                        [ class "tooltip"
                        , case model.path_data of
                            Just p ->
                                attribute "data-tooltip" ([ "Recent activities for the", NodeType.toString p.focus.type_, p.focus.name ] |> List.intersperse " " |> String.join "")

                            Nothing ->
                                class ""
                        ]
                        [ span [ class "help" ] [ text "Recent activities:" ] ]
                    ]
                , div [ class "level-right" ]
                    [ div [ class "tabs is-small" ]
                        [ ul []
                            [ li [ class "is-active" ] [ a [ href "#" ] [ I.icon1 "icon-exchange icon-sm" (upH T.tension) ] ]
                            , li [] [ a [ class "has-text-grey", href "#" ] [ I.icon1 "icon-history icon-sm" (upH T.journal) ] ]
                            ]
                        ]
                    ]
                ]
            ]
        , div [ classList [ ( "content", True ), ( "spinner", model.tensions_data == LoadingSlowly ), ( "is-lazy", model.init_tensions ) ] ]
            [ case model.tensions_data of
                Success tensions ->
                    if List.length tensions > 0 then
                        List.map (\t -> mediaTension OverviewBaseUri model.node_focus t Navigate) tensions
                            ++ ternary (List.length tensions > 5)
                                [ div [ class "is-aligned-center", attribute "style" "margin-top:10px;" ]
                                    [ a [ href (uriFromNameid TensionsBaseUri model.node_focus.nameid) ] [ textH T.seeMore ] ]
                                ]
                                []
                            |> div [ class "is-size-7", id "tensionsTab" ]

                    else
                        case model.node_focus.type_ of
                            NodeType.Role ->
                                div [] [ textH T.noOpenTensionRole ]

                            NodeType.Circle ->
                                div [] [ textH T.noOpenTensionCircle ]

                Failure err ->
                    viewGqlErrors err

                _ ->
                    text ""
            ]
        ]



-- Actions


setupActionModal : Model -> Html Msg
setupActionModal model =
    let
        onClose =
            DoCloseModal { reset = True, link = "" }
    in
    div
        [ id "actionModal"
        , class "modal modal-fx-fadeIn"
        , classList [ ( "is-active", model.isModalActive ) ]
        , attribute "data-modal-close" "closeModalFromJs"
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "actionModal"
            , onClick onClose
            ]
            []
        , div [ class "modal-content" ]
            [ viewActionStep model model.node_action ]
        , button [ class "modal-close is-large", onClick onClose ] []
        ]


viewActionStep : Model -> ActionState -> Html Msg
viewActionStep model action =
    case action of
        JoinOrga step ->
            viewJoinOrgaStep model.orga_data step

        NoOp ->
            text ""

        AskErr err ->
            viewGqlErrors [ err ]

        ActionAuthNeeded ->
            viewAuthNeeded DoCloseModal


viewJoinOrgaStep : GqlData NodesData -> JoinStep ActionForm -> Html Msg
viewJoinOrgaStep orga step =
    case step of
        JoinInit _ ->
            div [ class "box spinner" ] [ text "" ]

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
                    div [ class "box  spinner" ] [ text "" ]

        JoinNotAuthorized errMsg ->
            viewGqlErrors errMsg



---- Utils


{-| Get Auth Step and init form based on user roles for new circle
-}
getNewNodeStepAuth : TensionForm -> GqlData NodesData -> ( NodeStep, TensionForm )
getNewNodeStepAuth form odata =
    case getNewNodeRights form.uctx form.target odata of
        [] ->
            let
                isMember =
                    form.uctx.roles
                        |> List.map (\r -> r.rootnameid)
                        |> List.member form.target.rootnameid
            in
            if isMember then
                ( NodeNotAuthorized [ T.askCoordo ], form )

            else
                ( NodeNotAuthorized [ T.notOrgMember, T.joinForCircle ], form )

        [ r ] ->
            ( NodeFinal, { form | source = r } )

        roles ->
            ( NodeSource roles, form )


notAuthorizedNode : NodeStep -> Bool
notAuthorizedNode step =
    case step of
        NodeNotAuthorized _ ->
            True

        _ ->
            False


getNewNodeRights : UserCtx -> Node -> GqlData NodesData -> List UserRole
getNewNodeRights uctx target odata =
    let
        orgaRoles =
            getOrgaRoles uctx.roles [ target.rootnameid ]
    in
    if List.length orgaRoles == 0 then
        []

    else if isOwner orgaRoles then
        List.filter (\r -> r.role_type == RoleType.Owner) orgaRoles

    else
        let
            childrenRoles =
                getChildrenLeaf target.nameid odata

            childrenCoordos =
                List.filter (\n -> n.role_type == Just RoleType.Coordinator) childrenRoles

            circleRoles =
                getCircleRoles orgaRoles [ target.nameid ]

            allCoordoRoles =
                getCoordoRoles orgaRoles

            coordoRoles =
                getCoordoRoles circleRoles
        in
        case target.charac.mode of
            NodeMode.Agile ->
                case circleRoles of
                    [] ->
                        if List.length childrenRoles == 0 then
                            orgaRoles

                        else
                            []

                    circleRoles_ ->
                        circleRoles_

            NodeMode.Coordinated ->
                case coordoRoles of
                    [] ->
                        if List.length childrenCoordos == 0 && List.length allCoordoRoles > 0 then
                            allCoordoRoles

                        else
                            []

                    coordoRoles_ ->
                        coordoRoles_


getChildrenLeaf : String -> GqlData NodesData -> List Node
getChildrenLeaf nid odata =
    odata
        |> withMaybeDataMap
            (\x ->
                x |> Dict.values |> List.filter (\n -> n.type_ == NodeType.Role && Just (nearestCircleid nid) == Maybe.map (\m -> m.nameid) n.parent)
            )
        |> withDefault []
