port module Components.Org.Overview exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Array
import Auth exposing (doRefreshToken, refreshAuthModal)
import Browser.Navigation as Nav
import Components.DocToolBar as DocToolBar
import Components.Fa as Fa
import Components.HelperBar as HelperBar exposing (HelperBar)
import Components.Loading as Loading exposing (GqlData, RequestResult(..), WebData, viewAuthNeeded, viewGqlErrors, viewHttpErrors, viewWarnings)
import Components.NodeDoc as NodeDoc exposing (nodeFragmentFromOrga)
import Components.Text as T
import Debug
import Dict exposing (Dict)
import Extra exposing (ternary, withDefaultData, withMapData, withMaybeData, withMaybeDataMap)
import Extra.Events exposing (onClickPD, onKeydown)
import Form exposing (isPostSendable)
import Form.NewCircle as NewCircleForm
import Form.NewTension as NewTensionForm exposing (NewTensionForm)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import Json.Decode as JD
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs
    exposing
        ( Flags_
        , FractalBaseRoute(..)
        , NodeFocus
        , focusFromNameid
        , focusState
        , getCircleRoles
        , getCoordoRoles
        , getOrgaRoles
        , nameidFromFlags
        , nearestCircleid
        , nodeIdCodec
        , uriFromNameid
        , uriFromUsername
        )
import ModelCommon.Requests exposing (login)
import ModelCommon.View exposing (action2SourceStr, getAvatar, mediaTension, roleColor, tensionTypeColor, viewUsernameLink)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.AddNode exposing (addNewMember, addOneCircle)
import Query.AddTension exposing (addOneTension)
import Query.PatchNode exposing (patchNode)
import Query.QueryNode exposing (queryGraphPack, queryNodesSub)
import Query.QueryNodeData exposing (queryNodeData)
import Query.QueryTension exposing (queryAllTension)
import RemoteData exposing (RemoteData)
import Task
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


type alias Flags =
    Flags_



---- MODEL----


type alias Model =
    { node_focus : NodeFocus
    , path_data : Maybe LocalGraph
    , users_data : GqlData UsersData
    , lookup_users : List User
    , orga_data : GqlData NodesData
    , tensions_data : GqlData TensionsData
    , node_data : GqlData NodeData
    , init_tensions : Bool
    , init_data : Bool
    , node_quickSearch : NodesQuickSearch

    -- Form
    --, joinForm : JoinOrgaForm
    , tensionForm : NewTensionForm

    -- common
    , node_action : ActionState
    , isModalActive : Bool
    , modalAuth : ModalAuth
    , helperBar : HelperBar
    }


nfirstTensions : Int
nfirstTensions =
    10



---- MSG ----


type Msg
    = PassedSlowLoadTreshold -- timer
      -- Gql Data Queries
    | GotOrga (GqlData NodesData) -- graphql
    | GotTensions (GqlData TensionsData) -- graphql
    | GotData (GqlData NodeData) -- graphql
      -- Quick search
    | LookupFocus String (Maybe LocalGraph)
    | LookupBlur
    | ChangePattern String
    | ChangeNodeLookup (LookupResult Node)
    | SearchKeyDown Int
      -- User quick search
    | ChangeNodeUserPattern Int String
    | ChangeNodeUserRole Int String
    | SelectUser Int String
    | CancelUser Int
    | ShowLookupFs
    | CancelLookupFs
      -- Node Actions
    | DoNodeAction Node_ -- ports receive / tooltip click
    | Submit (Time.Posix -> Msg) -- Get Current Time
    | SubmitTension NewTensionForm Bool Time.Posix -- Send form
    | SubmitCircle NewTensionForm Bool Time.Posix -- Send form
      -- New Tension Action
    | DoTensionInit Node -- {target}
    | DoTensionSource TensionType.TensionType -- {type}
    | DoTensionFinal UserRole --  {source}
    | TensionAck (GqlData Tension)
      -- New Circle Action
    | DoCircleInit Node NodeType.NodeType -- {target}
    | DoCircleSource -- String -- {nodeMode} @DEBUG: node mode is inherited by default.
    | DoCircleFinal UserRole -- {source}
    | ChangeNodePost String String -- {field value}
    | AddLinks
    | AddResponsabilities
    | AddDomains
    | AddPolicies
    | NewNodesAck (GqlData (List Node))
      -- CircleAck === TensionAck
      -- JoinOrga Action
    | DoJoinOrga String Time.Posix
    | JoinAck (GqlData Node)
      -- JS Interop
    | NodeClicked String -- ports receive / Node clicked
    | NodeFocused LocalGraph_ -- ports receive / Node focused
    | DoClearTooltip -- ports send
    | ToggleGraphReverse -- ports send
    | ToggleTooltips -- ports send / Not implemented @DEBUG multiple tooltip/ see name of circle
      -- Common
    | Navigate String
    | DoOpenModal -- ports receive / Open  modal
    | DoCloseModal String -- ports receive / Close modal
    | DoCloseAuthModal -- ports receive / Close modal
    | ChangeAuthPost String String
    | SubmitUser UserAuthForm
    | GotSignin (WebData UserCtx)
    | SubmitKeyDown Int -- Detect Enter (for form sending)
    | ChangeInputViewMode InputViewMode
    | ExpandRoles
    | CollapseRoles
    | ChangeUserLookup (LookupResult User)



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
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , lookup_users = []
            , orga_data =
                session.orga_data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , tensions_data =
                session.tensions_data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , node_data =
                session.node_data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , init_tensions = True
            , init_data = True
            , node_quickSearch = { qs | pattern = "", idx = 0 }

            -- Form
            , tensionForm = NewTensionForm.create newFocus

            -- Common
            , node_action = session.node_action |> withDefault NoOp
            , isModalActive = False
            , modalAuth = Inactive
            , helperBar = HelperBar.create
            }

        cmds =
            if fs.orgChange || isInit then
                [ queryGraphPack apis.gql newFocus.rootnameid GotOrga
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
                                    --[ Ports.initGraphPack ndata model.node_focus.nameid ]
                                    [ queryGraphPack apis.gql newFocus.rootnameid GotOrga
                                    , Ports.initGraphPack Dict.empty ""
                                    ]

                                Nothing ->
                                    []

                        else
                            [ Ports.focusGraphPack newFocus.nameid ]
                       )

            else if fs.refresh then
                [ queryGraphPack apis.gql newFocus.rootnameid GotOrga
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
update global msg model =
    let
        apis =
            global.session.apis
    in
    case msg of
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

        -- Gql queries
        GotOrga result ->
            case result of
                Success data ->
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

                other ->
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
                    ( { model | node_data = result }, Cmd.none, send (UpdateSessionData Nothing) )

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
                    ( model, Cmd.none, Cmd.none )

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
                            ( model
                            , Cmd.none
                            , Nav.replaceUrl global.key (uriFromNameid OverviewBaseUri n.nameid)
                            )

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

        -- User quick search
        ChangeNodeUserPattern pos pattern ->
            ( { model | tensionForm = NewTensionForm.updateUserPattern pos pattern model.tensionForm }
            , Ports.searchUser pattern
            , Cmd.none
            )

        ChangeNodeUserRole pos role ->
            ( { model | tensionForm = NewTensionForm.updateUserRole pos role model.tensionForm }
            , Cmd.none
            , Cmd.none
            )

        SelectUser pos username ->
            ( { model | tensionForm = NewTensionForm.selectUser pos username model.tensionForm }
            , Cmd.none
            , Cmd.none
            )

        CancelUser pos ->
            ( { model | tensionForm = NewTensionForm.cancelUser pos model.tensionForm }
            , Cmd.none
            , Cmd.none
            )

        ShowLookupFs ->
            ( { model | tensionForm = NewTensionForm.openLookup model.tensionForm }
            , if model.tensionForm.isLookupOpen == False then
                Cmd.batch [ Ports.outsideClickClose "cancelLookupFsFromJs" "userSearchPanel" ]

              else
                Cmd.none
            , Cmd.none
            )

        CancelLookupFs ->
            ( { model | tensionForm = NewTensionForm.closeLookup model.tensionForm }, Cmd.none, Cmd.none )

        -- Action
        DoNodeAction node_ ->
            let
                newAction =
                    case node_ of
                        Ok node ->
                            ActionChoice node

                        Err err ->
                            AskErr err
            in
            ( { model | node_action = newAction }, send DoOpenModal, Cmd.none )

        -- New Tension
        DoTensionInit target ->
            let
                tf =
                    NewTensionForm.create model.node_focus
                        |> NewTensionForm.setTarget target (withMaybeData model.node_data)
            in
            ( { model | node_action = AddTension TensionInit, tensionForm = tf }
            , Cmd.none
            , Ports.bulma_driver "actionModal"
            )

        DoTensionSource tensionType ->
            case global.session.user of
                LoggedOut ->
                    ( { model | node_action = ActionAuthNeeded }, Cmd.none, Cmd.none )

                LoggedIn uctx ->
                    case model.node_action of
                        AddTension TensionInit ->
                            let
                                form_ =
                                    model.tensionForm.form

                                form =
                                    { form_ | uctx = uctx, tension_type = tensionType }

                                ( newStep, newForm ) =
                                    getNewTensionStepAuth form
                            in
                            ( { model | node_action = AddTension newStep, tensionForm = NewTensionForm.setForm newForm model.tensionForm }
                            , Cmd.none
                            , Ports.bulma_driver "actionModal"
                            )

                        _ ->
                            ( { model | node_action = AskErr "Step moves not implemented" }, Cmd.none, Cmd.none )

        DoTensionFinal source ->
            case model.node_action of
                AddTension (TensionSource _) ->
                    ( { model | node_action = AddTension TensionFinal, tensionForm = NewTensionForm.setSource source model.tensionForm }
                    , Cmd.none
                    , Ports.bulma_driver "actionModal"
                    )

                _ ->
                    ( { model | node_action = AskErr "Step moves not implemented" }, Cmd.none, Cmd.none )

        SubmitTension data _ time ->
            let
                newTensionForm =
                    data
                        |> NewTensionForm.post "createdAt" (fromTime time)
                        |> NewTensionForm.setEvents [ TensionEvent.Created ]
                        |> NewTensionForm.setResult LoadingSlowly
            in
            ( { model | tensionForm = newTensionForm }
            , addOneTension apis.gql newTensionForm.form TensionAck
            , Cmd.none
            )

        TensionAck result ->
            let
                -- Redirect Success new node* tension to the AddTension pipeline
                -- @Debug: Make the form (and the result) accessible from the model (as for the Tension page,
                --         in order to avoid doing several "switch/case" to get the form ?
                form =
                    model.tensionForm.form

                na =
                    if withMaybeData result /= Nothing && model.node_action == AddCircle NodeFinal && form.status == TensionStatus.Open then
                        AddTension TensionFinal

                    else
                        model.node_action

                tf =
                    NewTensionForm.setResult result model.tensionForm
            in
            case na of
                AddTension TensionFinal ->
                    case result of
                        Success t ->
                            let
                                tensions =
                                    hotTensionPush t model.tensions_data
                            in
                            ( { model | node_action = na, tensionForm = tf, tensions_data = Success tensions }
                            , Cmd.none
                            , send (UpdateSessionTensions (Just tensions))
                            )

                        other ->
                            if doRefreshToken other then
                                let
                                    tf2 =
                                        NewTensionForm.setResult NotAsked model.tensionForm
                                in
                                ( { model | modalAuth = Active { post = Dict.fromList [ ( "username", form.uctx.username ) ], result = RemoteData.NotAsked }, tensionForm = tf2 }
                                , Cmd.none
                                , Ports.open_auth_modal
                                )

                            else
                                ( { model | tensionForm = tf }, Cmd.none, Cmd.none )

                AddCircle NodeFinal ->
                    case result of
                        Success t ->
                            let
                                nameid =
                                    form.node.nameid
                                        |> Maybe.map (\nid -> nodeIdCodec form.target.nameid nid (withDefault NodeType.Role form.node.type_))
                                        |> withDefault ""
                            in
                            ( { model | tensionForm = tf }
                            , queryNodesSub apis.gql nameid NewNodesAck
                            , Cmd.none
                            )

                        other ->
                            if doRefreshToken other then
                                let
                                    tf2 =
                                        NewTensionForm.setResult NotAsked model.tensionForm
                                in
                                ( { model | modalAuth = Active { post = Dict.fromList [ ( "username", form.uctx.username ) ], result = RemoteData.NotAsked }, tensionForm = tf2 }
                                , Cmd.none
                                , Ports.open_auth_modal
                                )

                            else
                                ( { model | tensionForm = tf }, Cmd.none, Cmd.none )

                other ->
                    ( { model | node_action = AskErr "Query method implemented from TensionAck" }, Cmd.none, Cmd.none )

        -- New Circle
        DoCircleInit node nodeType ->
            -- We do not load users_data here because it assumes GotOrga is called at init.
            ( { model | node_action = AddCircle NodeInit, tensionForm = NewTensionForm.create model.node_focus |> NewTensionForm.initCircle node nodeType }
            , send DoCircleSource
            , Ports.bulma_driver "actionModal"
            )

        DoCircleSource ->
            case global.session.user of
                LoggedOut ->
                    ( { model | node_action = ActionAuthNeeded }, Cmd.none, Cmd.none )

                LoggedIn uctx ->
                    case model.node_action of
                        AddCircle NodeInit ->
                            let
                                form_ =
                                    model.tensionForm.form

                                form =
                                    { form_ | uctx = uctx }

                                ( newStep, newForm ) =
                                    getNewNodeStepAuth form model.orga_data
                            in
                            ( { model | node_action = AddCircle newStep, tensionForm = NewTensionForm.setForm newForm model.tensionForm }
                            , Cmd.none
                            , Ports.bulma_driver "actionModal"
                            )

                        _ ->
                            ( { model | node_action = AskErr "Step moves not implemented" }, Cmd.none, Cmd.none )

        DoCircleFinal source ->
            case model.node_action of
                AddCircle (NodeSource _) ->
                    ( { model | node_action = AddCircle NodeFinal, tensionForm = NewTensionForm.setSource source model.tensionForm }
                    , Cmd.none
                    , Ports.bulma_driver "actionModal"
                    )

                _ ->
                    ( { model | node_action = AskErr "Step moves not implemented" }, Cmd.none, Cmd.none )

        ChangeNodePost field value ->
            case model.node_action of
                AddTension TensionFinal ->
                    ( { model | tensionForm = NewTensionForm.post field value model.tensionForm }, Cmd.none, Cmd.none )

                AddCircle NodeFinal ->
                    ( { model | tensionForm = NewTensionForm.postNode field value model.tensionForm }, Cmd.none, Cmd.none )

                _ ->
                    ( { model | node_action = AskErr "Step moves not implemented" }, Cmd.none, Cmd.none )

        AddLinks ->
            ( { model | tensionForm = NewTensionForm.addLinks model.tensionForm }, Cmd.none, Cmd.none )

        AddResponsabilities ->
            ( { model | tensionForm = NewTensionForm.addResponsabilities model.tensionForm }, Cmd.none, Cmd.none )

        AddDomains ->
            ( { model | tensionForm = NewTensionForm.addDomains model.tensionForm }, Cmd.none, Cmd.none )

        AddPolicies ->
            ( { model | tensionForm = NewTensionForm.addPolicies model.tensionForm }, Cmd.none, Cmd.none )

        SubmitCircle data doClose time ->
            let
                events =
                    if doClose == True then
                        [ TensionEvent.Created, TensionEvent.BlobCreated, TensionEvent.BlobPushed ]

                    else
                        [ TensionEvent.Created, TensionEvent.BlobCreated ]

                newTensionForm =
                    data
                        |> NewTensionForm.post "createdAt" (fromTime time)
                        |> NewTensionForm.setEvents events
                        |> NewTensionForm.setStatus (ternary (doClose == True) TensionStatus.Closed TensionStatus.Open)
                        |> NewTensionForm.setActiveButton doClose
                        |> NewTensionForm.setResult LoadingSlowly
            in
            ( { model | tensionForm = newTensionForm }
            , addOneTension apis.gql newTensionForm.form TensionAck
            , Cmd.none
            )

        NewNodesAck result ->
            case result of
                Success nodes ->
                    let
                        first_nameid =
                            List.head nodes |> Maybe.map (\n -> n.nameid) |> withDefault ""

                        ndata =
                            hotNodePush nodes model.orga_data
                    in
                    ( { model | orga_data = Success ndata }
                    , Cmd.batch [ Ports.addQuickSearchNodes nodes, nodes |> List.map (\n -> n.first_link) |> List.filterMap identity |> Ports.addQuickSearchUsers ]
                    , Cmd.batch [ send UpdateUserToken, send (UpdateSessionOrga (Just ndata)) ]
                    )

                other ->
                    ( model, Cmd.none, Cmd.none )

        -- Join
        DoJoinOrga rootnameid time ->
            case global.session.user of
                LoggedOut ->
                    ( { model | node_action = ActionAuthNeeded }
                    , send DoOpenModal
                    , Cmd.none
                    )

                LoggedIn uctx ->
                    let
                        form =
                            { uctx = uctx
                            , rootnameid = rootnameid
                            , id = model.path_data |> Maybe.map (\pd -> pd.root |> Maybe.map (\r -> r.id) |> withDefault "")
                            , post = Dict.fromList [ ( "createdAt", fromTime time ) ]
                            }

                        newModel =
                            { model | node_action = JoinOrga (JoinInit form) }
                    in
                    ( newModel
                    , Cmd.batch [ addNewMember apis.gql form JoinAck, send DoOpenModal ]
                    , Cmd.none
                    )

        JoinAck result ->
            case model.node_action of
                JoinOrga (JoinInit form) ->
                    case result of
                        Success n ->
                            let
                                ndata =
                                    hotNodePush [ n ] model.orga_data
                            in
                            ( { model | node_action = JoinOrga (JoinValidation form result), orga_data = Success ndata }
                            , Cmd.batch [ Ports.addQuickSearchNodes [ n ], Maybe.map (\fs -> Ports.addQuickSearchUsers [ fs ]) n.first_link |> withDefault Cmd.none ]
                            , Cmd.batch [ send UpdateUserToken, send (UpdateSessionOrga (Just ndata)) ]
                            )

                        other ->
                            if doRefreshToken other then
                                ( { model | modalAuth = Active { post = Dict.fromList [ ( "username", form.uctx.username ) ], result = RemoteData.NotAsked } }, Cmd.none, Ports.open_auth_modal )

                            else
                                ( { model | node_action = JoinOrga (JoinValidation form result) }, Cmd.none, Cmd.none )

                default ->
                    ( model, Cmd.none, Cmd.none )

        -- JS interop
        NodeClicked nameid ->
            ( model
            , Cmd.none
            , Nav.replaceUrl global.key (uriFromNameid OverviewBaseUri nameid)
            )

        NodeFocused path_ ->
            case path_ of
                Ok path ->
                    let
                        nameids =
                            path.focus.children |> List.map (\x -> x.nameid) |> List.append [ path.focus.nameid ]

                        cmd =
                            queryAllTension apis.gql nameids nfirstTensions 0 Nothing (Just TensionStatus.Open) Nothing GotTensions
                    in
                    ( { model | path_data = Just path }
                    , Cmd.batch [ Ports.drawButtonsGraphPack, cmd ]
                    , send (UpdateSessionPath (Just path))
                    )

                Err err ->
                    ( model, Cmd.none, Cmd.none )

        ToggleGraphReverse ->
            ( model, () |> sendToggleGraphReverse, Cmd.none )

        ToggleTooltips ->
            ( model, () |> sendToggleTooltips, Cmd.none )

        DoClearTooltip ->
            ( model, Cmd.none, Ports.clearTooltip )

        -- Common
        Navigate url ->
            ( model, Cmd.none, Nav.pushUrl global.key url )

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
                    ( { model | modalAuth = Inactive }
                    , send DoCloseAuthModal
                    , send (UpdateUserSession uctx)
                    )

                other ->
                    case model.modalAuth of
                        Active form ->
                            let
                                newForm =
                                    { form | result = result }
                            in
                            ( { model | modalAuth = Active newForm }, Cmd.none, Cmd.none )

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

        ChangeInputViewMode viewMode ->
            ( { model | tensionForm = NewTensionForm.setViewMode viewMode model.tensionForm }, Cmd.none, Cmd.none )

        ExpandRoles ->
            ( { model | helperBar = HelperBar.expand model.helperBar }, Cmd.none, Cmd.none )

        CollapseRoles ->
            ( { model | helperBar = HelperBar.collapse model.helperBar }, Cmd.none, Cmd.none )

        ChangeUserLookup users_ ->
            case users_ of
                Ok users ->
                    ( { model | lookup_users = users }, Cmd.none, Cmd.none )

                Err err ->
                    ( model, Cmd.none, Cmd.none )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ _ =
    Sub.batch
        [ Ports.closeModalFromJs DoCloseModal
        , nodeClickedFromJs NodeClicked
        , nodeFocusedFromJs_ NodeFocused
        , nodeDataFromJs_ DoNodeAction
        , Ports.lookupNodeFromJs ChangeNodeLookup
        , Ports.lookupUserFromJs ChangeUserLookup
        , Ports.cancelLookupFsFromJs (always CancelLookupFs)
        ]



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


port sendToggleTooltips : () -> Cmd msg



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Overview · " ++ (String.join "/" <| LE.unique [ model.node_focus.rootnameid, model.node_focus.nameid |> String.split "#" |> List.reverse |> List.head |> withDefault "" ])
    , body = [ view_ global model ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    let
        tid =
            model.node_data |> withMaybeData |> Maybe.map (\nd -> nd.source |> Maybe.map (\t -> t.id)) |> withDefault Nothing |> withDefault ""

        roletype =
            getNode model.node_focus.nameid model.orga_data |> Maybe.map (\n -> n.role_type) |> withDefault Nothing

        nodeData_ =
            { data = withMapData (\x -> tid) model.node_data
            , node = initNodeFragment Nothing
            , isLazy = model.init_data
            , source = OverviewBaseUri
            , focus = model.node_focus
            , hasBeenPushed = True
            , toolbar = ternary (roletype /= Just RoleType.Guest) (Just (DocToolBar.view model.node_focus tid Nothing)) Nothing
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
            { onJoin = Submit <| DoJoinOrga model.node_focus.rootnameid
            , onExpand = ExpandRoles
            , onCollapse = CollapseRoles
            , user = global.session.user
            , path_data = model.path_data
            , baseUri = OverviewBaseUri
            , data = model.helperBar
            }
    in
    -- [div [ class "column is-1 is-fullheight is-hidden-mobile", id "leftPane" ] [ viewLeftPane model ]
    div [ id "mainPane" ]
        [ HelperBar.view helperData
        , div [ class "columns is-centered is-variable is-4" ]
            [ div [ class "column is-5-desktop is-5-widescreen is-4-fullhd" ]
                [ viewSearchBar model.orga_data model.path_data model.node_quickSearch
                , viewCanvas model.node_focus model.orga_data
                , br [] []
                , NodeDoc.view nodeData Nothing
                , setupActionModal model
                ]
            , div [ class "column is-6-desktop is-6-widescreen is-5-fullhd" ]
                [ div [ class "columns is-gapless" ]
                    [ div [ class "column is-12", id "nextToChart" ]
                        [ viewActivies model ]
                    ]
                ]
            ]
        , refreshAuthModal model.modalAuth { closeModal = DoCloseAuthModal, changePost = ChangeAuthPost, submit = SubmitUser, submitEnter = SubmitKeyDown }
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
                        [ Fa.icon "far fa-circle fa-lg" model.node_focus.nameid ]
                    ]
                ]
            , li []
                [ ul [ class "menu-list" ]
                    [ li [] [ a [] [ Fa.icon "fas fa-scroll fa-xs" "Mandates" ] ]
                    , li [] [ a [] [ Fa.icon "fas fa-exchange-alt fa-xs" "Tensions" ] ]
                    , li [] [ a [] [ Fa.icon "fas fa-history fa-xs" "Journal" ] ]
                    , li []
                        [ a [] [ Fa.icon "fas fa-user fa-xs" "Members" ]
                        ]
                    ]
                ]
            ]
        ]


viewSearchBar : GqlData NodesData -> Maybe LocalGraph -> NodesQuickSearch -> Html Msg
viewSearchBar odata maybePath qs =
    let
        node_ =
            maybePath
                |> Maybe.map
                    (\p ->
                        case odata of
                            Success d ->
                                Dict.get p.focus.nameid d
                                    |> Maybe.map (\n -> Ok n)
                                    |> withDefault (Err "Node not found")

                            other ->
                                Err "No nodes data"
                    )
                |> withDefault (Err "No path returned")

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
    in
    div
        [ id "searchBarOverview"
        , class "field has-addons searchBar"
        , onMouseEnter DoClearTooltip
        ]
        [ div [ class "control has-icons-left is-expanded" ]
            [ input
                [ class "input is-small"
                , type_ "text"
                , placeholder "Find a Role or Circle"
                , value qs.pattern
                , onInput ChangePattern
                , onFocus (LookupFocus qs.pattern maybePath)
                , onClick (LookupFocus qs.pattern maybePath)
                , onBlur LookupBlur
                , onKeydown SearchKeyDown

                --, list "searchList" -- impossible interaction !
                ]
                []
            , span [ class "icon is-left" ] [ i [ class "fas fa-search" ] [] ]
            , div [ id "searchList", classList [ ( "is-hidden", qs.visible == False ) ] ]
                [ div [ class "table is-fullwidth" ] <|
                    if sortedLookup == [] then
                        [ tbody [] [ td [] [ text T.noResultsFound ] ] ]

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
                                                [ td [ class "is-grey is-aligned-center is-size-6" ] [ text (" " ++ T.circleH ++ " ") ] ]

                                             else if i == 0 || n.type_ == NodeType.Role && (Array.get (i - 1) (Array.fromList sortedLookup) |> Maybe.map (\x -> x.type_ == NodeType.Circle) |> withDefault False) == True then
                                                [ td [ class "is-grey is-aligned-center is-size-6" ] [ text (" " ++ T.roleH ++ " ") ] ]

                                             else
                                                []
                                            )
                                )
                            |> List.concat
                            |> tbody []
                            |> List.singleton
                            |> List.append [ thead [] [ tr [] [ th [] [ text T.nameH ], th [] [ text T.parentH ], th [] [ text T.firstLinkH ] ] ] ]
                ]
            , case node_ of
                Ok node ->
                    div
                        [ class "control controlButton button is-small is-info _modalTrigger_"
                        , attribute "data-modal" "actionModal"
                        , onClick (DoNodeAction node_)
                        ]
                        [ span [ class "has-text-weight-bold text" ] [ text node.name ]
                        , span [ class "fa-stack ellipsisArt" ]
                            --[ i [ class "fas fa-ellipsis-h fa-stack-1x" ] [] ]
                            [ i [ class "fas fa-plus fa-stack-1x" ] [] ]
                        ]

                Err err ->
                    text ""
            ]
        ]


viewCanvas : NodeFocus -> GqlData NodesData -> Html Msg
viewCanvas focus odata =
    div [ id "canvasParent", classList [ ( "spinner", odata == LoadingSlowly ) ] ]
        [ case odata of
            Failure err ->
                viewGqlErrors err

            Success d ->
                case Dict.get focus.nameid d of
                    Just _ ->
                        text ""

                    Nothing ->
                        viewGqlErrors [ "Node archived or hidden" ]

            _ ->
                text ""
        , canvas [ id "canvasOrga", class "is-invisible" ] []
        , div [ id "canvasButtons", class "buttons are-small is-invisible" ]
            -- Hidden class use in graphpack_d3.js
            [ div
                [ id "invGraph_cvbtn"
                , class "button buttonToggle tooltip has-tooltip-right"
                , attribute "data-tooltip" T.reverseTooltip
                , onClick ToggleGraphReverse
                ]
                [ Fa.icon0 "fas fa-sort-amount-up" "" ]
            ]
        , div
            [ id "nodeTooltip"
            , class "_modalTrigger_ is-invisible"
            , attribute "data-modal" "actionModal"
            ]
            [ span [] [ text "void" ] -- Node name
            , span [ class "fa-stack ellipsisArt" ]
                [ i [ class "fas fa-plus fa-stack-1x" ] [] ]
            ]
        ]


viewActivies : Model -> Html Msg
viewActivies model =
    div
        [ class "box is-flex-grow"
        , attribute "style" "padding-top: 0px;"
        ]
        [ div [ class "title" ]
            [ span
                [ class "help has-text-weight-semibold"
                , attribute "style" "top: 20px; position: relative;" -- @DEBUG: How to Jump line when flex overflow occurs?
                ]
                [ text "Recent activities:" ]
            , div [ class "tabs is-right is-small" ]
                [ ul []
                    [ li [ class "is-active" ] [ a [] [ Fa.icon "fas fa-exchange-alt fa-sm" T.tensionH ] ]
                    , li [] [ a [ class "has-text-grey" ] [ Fa.icon "fas fa-history fa-sm" T.journalH ] ]
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
                                    [ a [ href (uriFromNameid TensionsBaseUri model.node_focus.nameid) ] [ text T.seeMore ] ]
                                ]
                                []
                            |> div [ class "is-size-7", id "tensionsTab" ]

                    else
                        case model.node_focus.type_ of
                            NodeType.Role ->
                                div [] [ text T.noOpenTensionRole ]

                            NodeType.Circle ->
                                div [] [ text T.noOpenTensionCircle ]

                Failure err ->
                    viewGqlErrors err

                _ ->
                    text ""
            ]
        ]



-- Actions


setupActionModal : Model -> Html Msg
setupActionModal model =
    div
        [ id "actionModal"
        , class "modal modal-fx-fadeIn elmModal"
        , classList [ ( "is-active", model.isModalActive ) ]
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "actionModal"
            , onClick (DoCloseModal "")
            ]
            []
        , div [ class "modal-content" ]
            [ viewActionStep model model.node_action ]
        , button [ class "modal-close is-large", onClick (DoCloseModal "") ] []
        ]


viewActionStep : Model -> ActionState -> Html Msg
viewActionStep model action =
    case action of
        ActionChoice node ->
            div [ class "modal-card" ]
                [ div [ class "modal-card-head" ]
                    [ div [] <|
                        List.intersperse (text "\u{00A0}")
                            [ span [ class "has-text-weight-medium" ] [ text "What action do you want to do with the" ]
                            , span [ class "has-text-weight-bold is-underline-dotted" ] [ text node.name ]
                            , span [ class "is-lowercase has-text-weight-semibold" ] [ text <| NodeType.toString node.type_ ]
                            , text "?"
                            ]
                    ]
                , div [ class "modal-card-body" ]
                    [ div [ class "level" ] <|
                        if node.type_ == NodeType.Circle then
                            [ div [ class "level-item" ] [ div [ class "button has-background-primary", onClick (DoTensionInit node) ] [ text "New Tension" ] ]
                            , div [ class "level-item" ] [ div [ class "button has-background-info", onClick (DoCircleInit node NodeType.Role) ] [ text "New Role" ] ]
                            , div [ class "level-item" ] [ div [ class "button has-background-turquoise", onClick (DoCircleInit node NodeType.Circle) ] [ text "New Sub-Circle" ] ]
                            ]

                        else
                            [ div [ class "level-item" ] [ div [ class "button is-primary", onClick (DoTensionInit node) ] [ text "New Tension" ] ] ]
                    ]
                ]

        AddTension step ->
            viewTensionStep step model.tensionForm model

        AddCircle step ->
            viewCircleStep step model.tensionForm model

        JoinOrga step ->
            viewJoinOrgaStep model.orga_data step

        NoOp ->
            text ""

        AskErr err ->
            viewGqlErrors [ err ]

        ActionAuthNeeded ->
            viewAuthNeeded DoCloseModal


makeNewTensionFormOp : Model -> NewTensionForm.Op Msg
makeNewTensionFormOp model =
    { lookup = model.lookup_users
    , users_data = model.users_data
    , targets = [ model.tensionForm.form.source.nameid, model.tensionForm.form.target.nameid ]
    , data = model.tensionForm
    , onChangeInputViewMode = ChangeInputViewMode
    , onChangeNode = ChangeNodePost
    , onAddLinks = AddLinks
    , onAddResponsabilities = AddResponsabilities
    , onAddDomains = AddDomains
    , onAddPolicies = AddPolicies
    , onCloseModal = DoCloseModal
    , onSubmitTension = SubmitTension
    , onSubmit = Submit
    , onChangeUserPattern = ChangeNodeUserPattern
    , onChangeUserRole = ChangeNodeUserRole
    , onSelectUser = SelectUser
    , onCancelUser = CancelUser
    , onShowLookupFs = ShowLookupFs
    , onCancelLookupFs = CancelLookupFs
    }


makeNewCircleFormOp : Model -> NewTensionForm.Op Msg
makeNewCircleFormOp model =
    let
        op =
            makeNewTensionFormOp model
    in
    { op | onSubmitTension = SubmitCircle }


viewTensionStep : TensionStep -> NewTensionForm -> Model -> Html Msg
viewTensionStep step ntf model =
    case step of
        TensionInit ->
            div [ class "modal-card" ]
                [ div [ class "modal-card-head" ]
                    [ span [ class "has-text-weight-medium" ] [ text "Choose the type of tension to communicate:" ] ]
                , div [ class "modal-card-body" ]
                    [ div [ class "level buttonRadio" ] <|
                        List.map
                            (\tensionType ->
                                div [ class "level-item" ]
                                    [ div
                                        [ class <| "button " ++ tensionTypeColor "background" tensionType
                                        , onClick (DoTensionSource tensionType)
                                        ]
                                        [ TensionType.toString tensionType |> text ]
                                    ]
                            )
                            TensionType.list
                    ]
                ]

        TensionSource roles ->
            viewSourceRoles ntf.form roles DoTensionFinal

        TensionFinal ->
            case ntf.form.blob_type of
                Just blob_type ->
                    case blob_type of
                        BlobType.OnNode ->
                            NewCircleForm.view (makeNewCircleFormOp model)

                        _ ->
                            div [] [ text "Not implemented" ]

                Nothing ->
                    NewTensionForm.view (makeNewTensionFormOp model)

        TensionNotAuthorized errMsg ->
            viewWarnings errMsg


viewCircleStep : NodeStep -> NewTensionForm -> Model -> Html Msg
viewCircleStep step ntf model =
    case step of
        NodeInit ->
            -- Node mode selection not implemented yet.
            div [] [ text "" ]

        NodeSource roles ->
            viewSourceRoles ntf.form roles DoCircleFinal

        NodeFinal ->
            NewCircleForm.view (makeNewCircleFormOp model)

        NodeNotAuthorized errMsg ->
            viewWarnings errMsg


viewJoinOrgaStep : GqlData NodesData -> JoinStep JoinOrgaForm -> Html Msg
viewJoinOrgaStep orga step =
    case step of
        JoinInit _ ->
            div [ class "box spinner" ] [ text T.loading ]

        JoinValidation form result ->
            case result of
                Success _ ->
                    div [ class "box is-light", onClick (DoCloseModal "") ]
                        [ Fa.icon "fas fa-check fa-2x has-text-success" " "
                        , text (T.welcomIn ++ " ")
                        , span [ class "has-font-weight-semibold" ] [ getNodeName form.rootnameid orga |> text ]
                        ]

                Failure err ->
                    viewGqlErrors err

                default ->
                    div [ class "box spinner" ] [ text T.loading ]

        JoinNotAuthorized errMsg ->
            viewGqlErrors errMsg



---- Common View


viewSourceRoles : TensionForm -> List UserRole -> (UserRole -> Msg) -> Html Msg
viewSourceRoles form roles nextStep =
    div [ class "modal-card" ]
        [ div [ class "modal-card-head" ]
            [ span [ class "has-text-weight-medium" ] [ "You have several roles in this organisation. Please select the role from which you want to " ++ action2SourceStr form.action |> text ] ]
        , div [ class "modal-card-body" ]
            [ div [ class "buttons buttonRadio", attribute "style" "margin-bottom: 2em; margin-right: 2em; margin-left: 2em;" ] <|
                List.map
                    (\r ->
                        button
                            [ class ("button buttonRole has-text-weight-semiboldtooltip has-tooltip-bottom is-" ++ roleColor r.role_type)
                            , attribute "data-tooltip" ([ r.name, "of", getParentFragmentFromRole r ] |> String.join " ")
                            , onClick (nextStep r)
                            ]
                            [ text r.name ]
                    )
                    roles
            ]
        ]



---- Utils


{-| Get Auth Step and init form based on user roles for new tension
-}
getNewTensionStepAuth : TensionForm -> ( TensionStep, TensionForm )
getNewTensionStepAuth form =
    case getOrgaRoles form.uctx.roles [ form.target.rootnameid ] of
        [] ->
            ( TensionNotAuthorized [ T.notOrgMember, T.joinForTension ], form )

        [ r ] ->
            ( TensionFinal, { form | source = r } )

        roles ->
            ( TensionSource roles, form )


{-| Get Auth Step and init form based on user roles for new circle
-}
getNewNodeStepAuth : TensionForm -> GqlData NodesData -> ( NodeStep, TensionForm )
getNewNodeStepAuth form odata =
    case getNewNodeRights form odata of
        [] ->
            ( NodeNotAuthorized [ T.notOrgMember, T.joinForCircle ], form )

        [ r ] ->
            ( NodeFinal, { form | source = r } )

        roles ->
            ( NodeSource roles, form )


getNewNodeRights : TensionForm -> GqlData NodesData -> List UserRole
getNewNodeRights form odata =
    let
        orgaRoles =
            getOrgaRoles form.uctx.roles [ form.target.rootnameid ]
    in
    case orgaRoles of
        [] ->
            []

        roles ->
            let
                childrenRoles =
                    getChildrenLeaf form.target.nameid odata

                childrenCoordos =
                    List.filter (\n -> n.role_type == Just RoleType.Coordinator) childrenRoles

                circleRoles =
                    getCircleRoles roles [ form.target.nameid ]

                allCoordoRoles =
                    getCoordoRoles roles

                coordoRoles =
                    getCoordoRoles circleRoles
            in
            case form.target.charac.mode of
                NodeMode.Chaos ->
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
                x |> Dict.values |> List.filter (\n -> n.type_ == NodeType.Role && Just (NodeId (nearestCircleid nid)) == n.parent)
            )
        |> withDefault []
