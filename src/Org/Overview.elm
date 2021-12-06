port module Org.Overview exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Array
import Auth exposing (ErrState(..), parseErr, refreshAuthModal)
import Browser.Navigation as Nav
import Codecs exposing (LookupResult, QuickDoc, WindowPos, nodeDecoder)
import Components.ActionPanel as ActionPanel
import Components.DocToolBar as DocToolBar
import Components.HelperBar as HelperBar exposing (HelperBar)
import Components.Loading as Loading
    exposing
        ( GqlData
        , ModalData
        , RequestResult(..)
        , WebData
        , fromMaybeData
        , isFailure
        , viewAuthNeeded
        , viewGqlErrors
        , viewHttpErrors
        , withDefaultData
        , withMapData
        , withMaybeData
        , withMaybeDataMap
        )
import Components.NodeDoc as NodeDoc exposing (nodeFragmentFromOrga)
import Debug
import Dict exposing (Dict)
import Extra exposing (ternary)
import Extra.Events exposing (onClickPD, onKeydown)
import Form exposing (isPostSendable)
import Form.Help as Help
import Form.NewTension as NTF exposing (TensionTab(..))
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
import Html.Attributes exposing (attribute, autocomplete, class, classList, disabled, href, id, list, name, placeholder, required, rows, style, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Icon as I
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
        , isOwner
        , memberIdCodec
        , nameidFromFlags
        , nearestCircleid
        , nid2rootid
        , uriFromNameid
        , uriFromUsername
        )
import ModelCommon.Requests exposing (login)
import ModelCommon.View exposing (mediaTension, viewUsernameLink)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.AddTension exposing (addOneTension)
import Query.PatchTension exposing (actionRequest)
import Query.QueryNode exposing (fetchNode, queryGraphPack, queryNodesSub)
import Query.QueryNodeData exposing (queryNodeData)
import Query.QueryTension exposing (queryAllTension)
import RemoteData exposing (RemoteData)
import Session exposing (GlobalCmd(..), NodesQuickSearch)
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

                    DoAddNodes nodes ->
                        ( send (AddNodes nodes), Cmd.none )

                    DoUpdateNode nameid fun ->
                        ( send (UpdateNode nameid fun), Cmd.none )

                    DoDelNodes nameids ->
                        ( send (DelNodes nameids), Cmd.none )

                    DoMoveNode a b c ->
                        ( send (MoveNode a b c), Cmd.none )

                    _ ->
                        ( Cmd.none, Cmd.none )
            )
        |> List.unzip



---- MODEL----


type alias Model =
    { node_focus : NodeFocus
    , path_data : Maybe LocalGraph
    , orga_data : GqlData NodesDict
    , users_data : GqlData UsersDict
    , tensions_data : GqlData TensionsList
    , node_data : GqlData NodeData
    , init_tensions : Bool
    , init_data : Bool
    , node_quickSearch : NodesQuickSearch
    , window_pos : WindowPos
    , node_hovered : Maybe Node
    , next_focus : Maybe String

    -- common
    , node_action : ActionState
    , isModalActive : Bool
    , modalAuth : ModalAuth
    , helperBar : HelperBar
    , refresh_trial : Int
    , now : Time.Posix

    -- Components
    , help : Help.State
    , tensionForm : NTF.State
    , actionPanel : ActionPanel.State
    }


nfirstTensions : Int
nfirstTensions =
    10



---- MSG ----


type Msg
    = PassedSlowLoadTreshold -- timer
    | LoadOrga
    | PushGuest ActionForm
    | PushTension Tension
    | Submit (Time.Posix -> Msg) -- Get the current time
      -- Gql Data Queries
    | GotOrga (GqlData NodesDict)
    | GotTensions (GqlData TensionsList)
    | GotData (GqlData NodeData)
      -- Page
    | SwitchWindow
      -- Quick search
    | LookupFocus String
    | ToggleLookup
    | LookupBlur
    | LookupBlur_
    | ChangePattern String
    | ChangeNodeLookup (LookupResult Node)
    | SearchKeyDown Int
      -- New Tension
    | DoCreateTension LocalGraph
      -- Node Action
    | DoActionEdit String Node
      -- JoinOrga Action
    | DoJoinOrga String
    | DoJoinOrga2 (GqlData Node)
    | DoJoinOrga3 Node Time.Posix
    | JoinAck (GqlData ActionResult)
      -- Graphpack
    | FetchNewNode String
    | NewNodesAck (GqlData (List Node))
    | AddNodes (List Node)
    | UpdateNode String (Node -> Node)
    | DelNodes (List String)
    | MoveNode String String String
      -- GP JS Interop
    | NodeClicked String
    | NodeHovered String
    | NodeFocused LocalGraph
    | DoClearTooltip
    | ToggleGraphReverse
      -- Token refresh
    | DoOpenAuthModal UserCtx
    | DoCloseAuthModal String
    | ChangeAuthPost String String
    | SubmitUser UserAuthForm
    | GotSignin (WebData UserCtx)
    | SubmitKeyDown Int -- Detect Enter (for form sending)
      -- Common
    | NoMsg
    | InitModals
    | LogErr String
    | Navigate String
    | DoOpenModal
    | DoCloseModal ModalData
    | ExpandRoles
    | CollapseRoles
      -- Components
    | HelpMsg Help.Msg
    | NewTensionMsg NTF.Msg
    | ActionPanelMsg ActionPanel.Msg



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
            focusState OverviewBaseUri session.referer global.url session.node_focus newFocus

        isInit =
            session.orga_data == Nothing

        fs =
            { fs_ | isInit = fs_.isInit || isInit }

        --d1 = Debug.log "isInit, orgChange, focuChange, menuChange" [ fs.isInit, fs.orgChange, fs.focusChange, fs.menuChange ]
        --d2 = Debug.log "newfocus" [ newFocus ]
        -- QuickSearch
        qs =
            session.node_quickSearch |> withDefault { pattern = "", lookup = Array.empty, idx = 0, visible = False }

        -- Model init
        model =
            { node_focus = newFocus
            , path_data = ternary fs.orgChange Nothing global.session.path_data -- Loaded from GraphPack
            , orga_data = fromMaybeData session.orga_data Loading
            , users_data = fromMaybeData session.users_data Loading
            , tensions_data = fromMaybeData session.tensions_data Loading
            , node_data = fromMaybeData session.node_data Loading
            , init_tensions = True
            , init_data = True
            , node_quickSearch = { qs | pattern = "", idx = 0 }
            , window_pos =
                global.session.window_pos
                    |> withDefault { topRight = "doc", bottomLeft = "activities" }
            , node_hovered = Nothing
            , next_focus = Nothing

            -- Node Action
            -- Common
            , node_action = withDefault NoOp global.session.node_action
            , isModalActive = False
            , modalAuth = Inactive
            , helperBar = HelperBar.create
            , help = Help.init global.session.user
            , tensionForm = NTF.init global.session.user
            , actionPanel = ActionPanel.init global.session.user
            , refresh_trial = 0
            , now = global.now
            }

        cmds_ =
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
                    ++ (if fs.menuChange then
                            case session.orga_data of
                                Just _ ->
                                    [ send LoadOrga
                                    , Ports.initGraphPack Dict.empty "" --canvas loading effect
                                    ]

                                Nothing ->
                                    []

                        else
                            [ Ports.focusGraphPack newFocus.nameid ]
                       )

            else if fs.menuChange then
                [ send LoadOrga
                , Ports.initGraphPack Dict.empty "" --canvas loading effect

                --, queryCircleTension apis.gql newFocus.nameid GotTensions
                , queryNodeData apis.gql newFocus.nameid GotData
                ]

            else
                []

        model2 =
            ternary (cmds_ == []) { model | init_tensions = False, init_data = False } model

        cmds =
            cmds_
                ++ [ sendSleep PassedSlowLoadTreshold 500
                   , sendSleep InitModals 400
                   ]
    in
    ( model2
    , Cmd.batch cmds
    , if fs.refresh then
        send (UpdateSessionFocus (Just newFocus))

      else
        Cmd.none
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
                    { win | bottomLeft = win.topRight, topRight = win.bottomLeft }
            in
            ( { model | window_pos = newWin }, Cmd.none, send (UpdateSessionWindow (Just newWin)) )

        -- Data queries
        GotOrga result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( model, send (DoOpenAuthModal (uctxFromUser global.session.user)), Cmd.none )

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

                _ ->
                    ( { model | orga_data = result }, Cmd.none, send (UpdateSessionOrga Nothing) )

        GotTensions result ->
            case result of
                Success data ->
                    ( { model | tensions_data = result, init_tensions = False }, Cmd.none, send (UpdateSessionTensions (Just data)) )

                _ ->
                    ( { model | tensions_data = result }, Cmd.none, send (UpdateSessionTensions Nothing) )

        GotData result ->
            case result of
                Success data ->
                    ( { model | node_data = result, init_data = False }, Cmd.none, send (UpdateSessionData (Just data)) )

                _ ->
                    ( { model | node_data = result }, Cmd.none, Cmd.none )

        -- Search
        ToggleLookup ->
            if model.node_quickSearch.visible then
                ( model, send LookupBlur, Cmd.none )

            else
                ( model, send (LookupFocus model.node_quickSearch.pattern), Cmd.none )

        LookupFocus pattern ->
            let
                qs =
                    model.node_quickSearch
            in
            case pattern of
                "" ->
                    case model.path_data of
                        Just path ->
                            case model.orga_data of
                                Success data ->
                                    let
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
                    ( { model | node_quickSearch = { qs | visible = True } }, Cmd.none, Cmd.none )

        LookupBlur ->
            ( model, sendSleep LookupBlur_ 150, Cmd.none )

        LookupBlur_ ->
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
                        LookupFocus pattern |> send

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
        DoCreateTension lg ->
            let
                tf =
                    model.tensionForm
                        |> NTF.setUser_ global.session.user
                        |> NTF.setPath_ lg
            in
            ( { model | tensionForm = tf }, Cmd.map NewTensionMsg (send NTF.OnOpen), Cmd.none )

        -- Node Action
        DoActionEdit domid node ->
            let
                rootSource =
                    getNode (nid2rootid node.nameid) model.orga_data |> Maybe.map (\n -> n.source) |> withDefault Nothing

                ( tid, bid ) =
                    node.source
                        |> Maybe.map (\b -> ( b.tension.id, b.id ))
                        |> withDefault
                            (rootSource
                                |> Maybe.map (\b -> ( b.tension.id, b.id ))
                                |> withDefault ( "", "" )
                            )
            in
            ( model, Cmd.map ActionPanelMsg (send <| ActionPanel.OnOpen domid tid bid node), Cmd.none )

        -- Join
        DoJoinOrga rootnameid ->
            case global.session.user of
                LoggedOut ->
                    ( { model | node_action = ActionAuthNeeded }
                    , send DoOpenModal
                    , Cmd.none
                    )

                LoggedIn _ ->
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

                _ ->
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
                        , events_type = Just [ TensionEvent.UserJoined ]
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
                    case parseErr result model.refresh_trial of
                        Authenticate ->
                            ( model, send (DoOpenAuthModal form.uctx), Cmd.none )

                        RefreshToken i ->
                            ( { model | refresh_trial = i }, sendSleep (PushGuest form) 500, send UpdateUserToken )

                        OkAuth _ ->
                            ( { model | node_action = JoinOrga (JoinValidation form result) }
                            , queryNodesSub apis.gql (memberIdCodec form.node.nameid form.uctx.username) NewNodesAck
                            , Cmd.none
                            )

                        _ ->
                            ( { model | node_action = JoinOrga (JoinValidation form result) }, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        -- Graphpack
        PushTension tension ->
            let
                tensions =
                    hotTensionPush tension model.tensions_data
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

                cmds =
                    model.next_focus
                        |> Maybe.map
                            (\nid -> [ send (NodeClicked nid) ])
                        |> withDefault [ Cmd.none ]
            in
            ( { model | orga_data = Success ndata, next_focus = Nothing }
            , Cmd.batch ([ Ports.addQuickSearchNodes nodes, List.map (\n -> n.first_link) nodes |> List.filterMap identity |> Ports.addQuickSearchUsers ] ++ cmds)
            , Cmd.batch [ sendSleep UpdateUserToken 300, send (UpdateSessionOrga (Just ndata)) ]
            )

        UpdateNode nameid fun ->
            let
                node_m =
                    getNode nameid model.orga_data
                        |> Maybe.map fun
            in
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
                ( ndata, _ ) =
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

        MoveNode nameid_old parentid_new nameid_new ->
            let
                ( ndata, _ ) =
                    hotNodePull [ nameid_old ] model.orga_data
            in
            ( { model | orga_data = Success ndata, next_focus = Just parentid_new }
              --, Cmd.batch [ Ports.addQuickSearchNodes nodes, nodes |> List.map (\n -> n.first_link) |> List.filterMap identity |> Ports.addQuickSearchUsers ]
            , Cmd.batch [ send (FetchNewNode nameid_new) ]
            , Cmd.none
            )

        -- JS interop
        NodeClicked nameid ->
            ( model, Cmd.none, ReplaceUrl (uriFromNameid OverviewBaseUri nameid) |> send )

        NodeHovered nid ->
            let
                ( node, cmd ) =
                    case getNode nid model.orga_data of
                        Just n ->
                            ( Just n, Cmd.none )

                        Nothing ->
                            ( Nothing, Cmd.map ActionPanelMsg (send ActionPanel.OnClose) )
            in
            ( { model | node_hovered = node }, cmd, Cmd.none )

        NodeFocused path ->
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

        ToggleGraphReverse ->
            ( model, () |> sendToggleGraphReverse, Cmd.none )

        DoClearTooltip ->
            ( model, Cmd.none, Ports.clearTooltip )

        -- Common
        NoMsg ->
            ( model, Cmd.none, Cmd.none )

        InitModals ->
            ( { model | tensionForm = NTF.fixGlitch_ model.tensionForm }, Cmd.none, Cmd.none )

        LogErr err ->
            ( model, Ports.logErr err, Cmd.none )

        Navigate url ->
            ( model, Cmd.none, Nav.pushUrl global.key url )

        DoOpenModal ->
            ( { model | isModalActive = True }, Ports.open_modal "actionModal", Cmd.none )

        DoCloseModal data ->
            let
                cmd =
                    if data.link /= "" then
                        send (Navigate data.link)

                    else
                        Cmd.none
            in
            ( { model | isModalActive = False }, cmd, Ports.close_modal )

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

                _ ->
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

        -- Components
        HelpMsg msg ->
            let
                ( help, out ) =
                    Help.update apis msg model.help

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | help = help }, out.cmds |> List.map (\m -> Cmd.map HelpMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        NewTensionMsg msg ->
            let
                ( tf, out ) =
                    NTF.update apis msg model.tensionForm

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | tensionForm = tf }, out.cmds |> List.map (\m -> Cmd.map NewTensionMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        ActionPanelMsg msg ->
            let
                ( data, out ) =
                    ActionPanel.update apis msg model.actionPanel

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | actionPanel = data }, out.cmds |> List.map (\m -> Cmd.map ActionPanelMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ model =
    [ nodeClickedFromJs NodeClicked
    , nodeHoveredFromJs NodeHovered
    , Ports.lgPD nodeFocusedFromJs LogErr NodeFocused
    , Ports.lgPD nodeDataFromJs LogErr DoCreateTension
    , Ports.lookupNodeFromJs ChangeNodeLookup
    , Ports.mcPD Ports.closeModalFromJs LogErr DoCloseModal
    ]
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        ++ (NTF.subscriptions model.tensionForm |> List.map (\s -> Sub.map NewTensionMsg s))
        ++ (ActionPanel.subscriptions model.actionPanel |> List.map (\s -> Sub.map ActionPanelMsg s))
        |> Sub.batch



-- Receive from Javascript


port nodeClickedFromJs : (String -> msg) -> Sub msg


port nodeHoveredFromJs : (String -> msg) -> Sub msg


port nodeFocusedFromJs : (JD.Value -> msg) -> Sub msg


port nodeDataFromJs : (JD.Value -> a) -> Sub a



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
        , NTF.view { users_data = model.users_data } model.tensionForm |> Html.map NewTensionMsg
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
            { data = withMapData (\_ -> tid) model.node_data
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

                _ ->
                    nodeData_

        helperData =
            { user = global.session.user
            , uriQuery = global.url.query
            , path_data = model.path_data
            , baseUri = OverviewBaseUri
            , data = model.helperBar
            , onJoin = DoJoinOrga model.node_focus.rootnameid
            , onExpand = ExpandRoles
            , onCollapse = CollapseRoles
            , onCreateTension = DoCreateTension
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
    div [ id "mainPane" ]
        [ --div [ class "column is-1 is-fullheight is-hidden-mobile", id "leftPane" ] [ viewLeftPane model ]
          HelperBar.view helperData
        , div [ class "columns is-centered is-variable is-4" ]
            [ div [ class "column is-6-desktop is-5-widescreen is-5-fullhd" ]
                [ viewSearchBar global.session.user model
                , viewCanvas global.session.user model
                , br [] []
                , viewFromPos model.window_pos.bottomLeft
                ]
            , div [ class "divider is-vertical is-hidden-mobile", onClick SwitchWindow ] [ text "⇋" ]
            , div
                [ class "column is-5-desktop is-5-widescreen is-5-fullhd" ]
                [ div [ class "columns is-gapless" ]
                    [ div [ class "column is-12", id "nextToChart" ]
                        [ viewFromPos model.window_pos.topRight ]
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
                        [ I.icon1 "icon-git-branch icon-lg" model.node_focus.nameid ]
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
    div
        [ id "searchBarOverview" ]
        [ div
            [ class "field has-addons searchBar"

            --, onMouseEnter DoClearTooltip
            ]
            ([ div [ class "control has-icons-left is-expanded" ]
                [ input
                    [ class "input is-small"
                    , type_ "search"
                    , autocomplete False
                    , placeholder (upH T.phQS)
                    , value model.node_quickSearch.pattern
                    , onInput ChangePattern
                    , onClick ToggleLookup
                    , onBlur LookupBlur
                    , onKeydown SearchKeyDown

                    --, list "searchList" -- impossible interaction !
                    ]
                    []
                , span [ class "icon is-left" ] [ i [ class "icon-search" ] [] ]
                ]
             ]
                ++ (case model.path_data of
                        Just p ->
                            let
                                node =
                                    getNode p.focus.nameid model.orga_data |> withDefault initNode
                            in
                            [ div [ class "control controlButtons" ]
                                [ span
                                    [ class "button is-small is-info is-wrapped"
                                    , attribute "data-modal" "actionModal"
                                    , onClick (DoCreateTension p)
                                    ]
                                    [ span [ class "has-text-weight-bold is-wrapped" ] [ text p.focus.name ]

                                    --, i [ class "icon-plus1 custom-style" ] []
                                    , i [ class "icon-send custom-style" ] []
                                    ]
                                ]
                            , viewActionPanel "actionPanelContent1" us node model.orga_data model.actionPanel
                            ]

                        _ ->
                            []
                   )
            )
        , div [ class "control" ]
            [ viewSearchList us model ]
        ]


viewActionPanel : String -> UserState -> Node -> GqlData NodesDict -> ActionPanel.State -> Html Msg
viewActionPanel domid us node o actionPanel =
    case us of
        LoggedIn uctx ->
            let
                isAdmin =
                    List.length (getNodeRights uctx node o) > 0

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
                        , domid = domid
                        , orga_data = o
                        }
                in
                span [ id domid, class "control actionPanelStyle" ]
                    [ span
                        [ class "button is-small is-info"
                        , classList [ ( "is-light", domid == "actionPanelContent2" ) ]
                        , onClick (DoActionEdit domid node)
                        ]
                        [ i [ class "icon-ellipsis-v" ] [] ]
                    , ActionPanel.view panelData actionPanel |> Html.map ActionPanelMsg
                    ]

            else
                text ""

        LoggedOut ->
            text ""


viewSearchList : UserState -> Model -> Html Msg
viewSearchList _ model =
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
                if isFailure model.node_data && Dict.get model.node_focus.nameid d == Nothing then
                    viewGqlErrors [ T.nodeNotFound ]

                else
                    text ""

            _ ->
                text ""
        , canvas [ id "canvasOrga", class "is-invisible" ] []
        , div [ id "canvasButtons", class "buttons are-small is-invisible" ]
            -- Hidden class use in graphpack_d3.js
            [ div
                [ id "root_cvbtn"
                , class "button tooltip has-tooltip-arrow has-tooltip-right"
                , attribute "data-tooltip" (upH T.goRoot)
                , onClick (NodeClicked model.node_focus.rootnameid)
                ]
                [ I.icon "icon-chevrons-up" ]
            , div
                [ id "root_cvbtn"
                , class "button tooltip has-tooltip-arrow has-tooltip-right"
                , attribute "data-tooltip" (upH T.goParent)
                , case model.path_data of
                    Just g ->
                        LE.getAt 1 (List.reverse g.path)
                            |> Maybe.map (\x -> x.nameid)
                            |> withDefault g.focus.nameid
                            |> NodeClicked
                            |> onClick

                    Nothing ->
                        onClick NoMsg
                ]
                [ I.icon "icon-chevron-up" ]
            , div
                [ id "invGraph_cvbtn"
                , class "button buttonToggle tooltip has-tooltip-arrow has-tooltip-right"
                , attribute "data-tooltip" (upH T.reverseTooltip)
                , onClick ToggleGraphReverse
                ]
                [ span [ style "padding" "2px" ] [ I.icon "icon-sort-amount-desc icon-xs" ] ]
            ]
        , div
            [ id "nodeTooltip"
            , class "is-invisible"
            , attribute "data-modal" "actionModal"
            , attribute "data-event-click" "doTension"
            , attribute "data-event-hover" "doAction"
            ]
            [ span [ id "doTension" ]
                [ span [] [ text "void" ] -- Node name

                --, i [ class "icon-plus1 custom-style" ] []
                , i [ class "icon-send custom-style" ] []
                ]
            , span [ id "doAction" ]
                [ case model.node_hovered of
                    Just node ->
                        viewActionPanel "actionPanelContent2" us node model.orga_data model.actionPanel

                    Nothing ->
                        text ""
                ]
            ]
        ]


viewActivies : Model -> Html Msg
viewActivies model =
    div
        [ id "activities", class "box is-shrinked2 is-flex-grow" ]
        [ div [ class "title" ]
            [ div
                [ class "level" ]
                [ div [ class "level-left" ]
                    [ div
                        [ class "tooltip has-tooltip-arrow"
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
        , div [ class "content is-size-7", classList [ ( "spinner", model.tensions_data == LoadingSlowly ), ( "is-lazy", model.init_tensions ) ] ]
            [ case model.tensions_data of
                Success tensions ->
                    if List.length tensions > 0 then
                        List.map (\t -> mediaTension model.now model.node_focus t False True "is-size-7" Navigate) tensions
                            ++ ternary (List.length tensions > 5)
                                [ div [ class "is-aligned-center mt-1 mb-2" ]
                                    [ a [ href (uriFromNameid TensionsBaseUri model.node_focus.nameid) ] [ textH T.seeMore ] ]
                                ]
                                []
                            |> div [ id "tensionsTab" ]

                    else
                        case model.node_focus.type_ of
                            NodeType.Role ->
                                div [ class "ml-4" ] [ textH T.noOpenTensionRole ]

                            NodeType.Circle ->
                                div [ class "ml-4" ] [ textH T.noOpenTensionCircle ]

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
            viewJoinOrgaStep step

        NoOp ->
            text ""

        AskErr err ->
            viewGqlErrors [ err ]

        ActionAuthNeeded ->
            viewAuthNeeded DoCloseModal


viewJoinOrgaStep : JoinStep ActionForm -> Html Msg
viewJoinOrgaStep step =
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

                _ ->
                    div [ class "box  spinner" ] [ text "" ]

        JoinNotAuthorized errMsg ->
            viewGqlErrors errMsg



---- Utils
