port module Components.Org.Overview exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Array
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav
import Components.Fa as Fa
import Components.HelperBar as HelperBar
import Components.Loading as Loading exposing (WebData, errorDecoder, viewAuthNeeded, viewGqlErrors, viewHttpErrors, viewWarnings)
import Components.Markdown exposing (renderMarkdown)
import Components.Text as T
import Debug
import Dict exposing (Dict)
import Extra exposing (ternary, withDefaultData, withMaybeData)
import Extra.Events exposing (onClickPD, onEnter, onKeydown, onTab)
import Form
import Form.EditCircle
import Form.NewCircle
import Form.NewTension
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (Route)
import Global exposing (Msg(..), send)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import Json.Decode as JD exposing (Value, decodeValue)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Requests exposing (login)
import ModelCommon.Uri exposing (Flags_, FractalBaseRoute(..), NodeFocus, focusFromNameid, focusState, nameidFromFlags, uriFromNameid)
import ModelCommon.View exposing (action2SourceStr, actionNameStr, mediaTension, roleColor, tensionTypeColor)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.AddNode exposing (addNewMember, addOneCircle)
import Query.AddTension exposing (addCircleTension, addOneTension)
import Query.PatchNode exposing (patchNode)
import Query.QueryNode exposing (queryGraphPack)
import Query.QueryNodeData exposing (queryNodeData)
import Query.QueryTension exposing (queryCircleTension)
import RemoteData exposing (RemoteData)
import String.Extra as SE
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
    , orga_data : GqlData NodesData
    , tensions_circle : GqlData TensionsData
    , data : GqlData NodeData
    , subData : SubmitCircleData

    -- common
    , node_action : ActionState
    , isModalActive : Bool
    , modalAuth : ModalAuth
    , node_quickSearch : NodesQuickSearch
    }


type alias SubmitCircleData =
    { changePostMsg : String -> String -> Msg
    , submitMsg : (Time.Posix -> Msg) -> Msg
    , submitNextMsg : TensionForm -> Bool -> Time.Posix -> Msg
    , closeModalMsg : String -> Msg
    , changeInputMsg : InputViewMode -> Msg

    -- data
    , viewMode : InputViewMode
    , tensionId : Maybe String
    }


initSubData : SubmitCircleData
initSubData =
    { submitNextMsg = SubmitCircle
    , tensionId = Nothing

    --
    , changePostMsg = ChangeNodePost

    -- Constant
    , closeModalMsg = DoCloseModal
    , changeInputMsg = ChangeInputViewMode
    , submitMsg = Submit
    , viewMode = Write
    }



---- MSG ----


type Msg
    = PassedSlowLoadTreshold -- timer
      -- Gql Data Queries
    | GotOrga (GqlData NodesData) -- graphql
    | GotTensions (GqlData TensionsData) -- graphql
    | GotData (GqlData NodeData) -- graphql
      -- Node Actions
    | DoNodeAction Node_ -- ports receive / tooltip click
    | Submit (Time.Posix -> Msg) -- Get Current Time
    | SubmitTension TensionForm Bool Time.Posix -- Send form
    | SubmitCircle TensionForm Bool Time.Posix -- Send form
    | SubmitNodePatch TensionForm Bool Time.Posix
    | ChangeNodePost String String -- {field value}
      -- New Tension Action
    | DoTensionInit Node -- {target}
    | DoTensionSource TensionType.TensionType -- {type}
    | DoTensionFinal UserRole --  {source}
    | TensionAck (GqlData Tension) -- decode better to get IdPayload
      -- New Circle Action
    | DoCircleInit Node NodeType.NodeType -- {target}
    | DoCircleSource -- String -- {nodeMode} @DEBUG: node mode is inherited by default.
    | DoCircleFinal UserRole -- {source}
    | CircleAck (GqlData (List Node)) -- decode better to get IdPayload
      -- Edit About action
    | DoEditAbout Node
    | DoEditAboutFinal UserRole
    | AboutAck (GqlData IdPayload)
      -- Edit Mandate Action
    | DoEditMandate Node
    | DoEditMandateFinal UserRole
    | MandateAck (GqlData IdPayload)
      -- JoinOrga Action
    | DoJoinOrga String Time.Posix
    | JoinAck (GqlData Node)
      -- Quick search
    | LookupFocus String (Maybe LocalGraph)
    | LookupBlur
    | ChangePattern String
    | ChangeLookup Nodes_
    | SearchKeyDown Int
      -- JS Interop
    | NodeClicked String -- ports receive / Node clicked
    | NodeFocused LocalGraph_ -- ports receive / Node focused
    | DoClearTooltip -- ports send
    | ToggleGraphReverse -- ports send
    | ToggleTooltips -- ports send / Not implemented @DEBUG multiple tooltip/ see name of circle
      -- Util
    | Navigate String
    | ChangeInputViewMode InputViewMode
    | DoOpenModal -- ports receive / Open  modal
    | DoCloseModal String -- ports receive / Close modal
    | DoCloseAuthModal -- ports receive / Close modal
    | ChangeAuthPost String String
    | SubmitUser UserForm
    | GotSignin (WebData UserCtx)



-- INIT --


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
            , path_data = global.session.path_data -- Loaded from GraphPack
            , orga_data =
                session.orga_data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , tensions_circle =
                session.tensions_circle
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , data =
                session.data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , node_action = session.node_action |> withDefault NoOp
            , isModalActive = False
            , modalAuth = Inactive
            , node_quickSearch = { qs | pattern = "", idx = 0 }
            , subData = initSubData
            }

        cmds =
            if fs.orgChange || isInit then
                [ queryGraphPack apis.gql newFocus.rootnameid GotOrga
                , queryCircleTension apis.gql newFocus.nameid GotTensions
                , queryNodeData apis.gql newFocus.nameid GotData
                ]

            else if fs.focusChange then
                [ queryCircleTension apis.gql newFocus.nameid GotTensions
                , queryNodeData apis.gql newFocus.nameid GotData
                ]
                    ++ (if fs.refresh then
                            case session.orga_data of
                                Just ndata ->
                                    --[ Ports.initGraphPack ndata model.node_focus.nameid ]
                                    [ queryGraphPack apis.gql newFocus.rootnameid GotOrga ]

                                Nothing ->
                                    []

                        else
                            [ Ports.focusGraphPack newFocus.nameid ]
                       )

            else if fs.refresh then
                case session.orga_data of
                    Just ndata ->
                        --[ Ports.initGraphPack ndata model.node_focus.nameid ]
                        [ queryGraphPack apis.gql newFocus.rootnameid GotOrga
                        , queryCircleTension apis.gql newFocus.nameid GotTensions
                        ]

                    Nothing ->
                        [ queryGraphPack apis.gql newFocus.rootnameid GotOrga
                        , queryCircleTension apis.gql newFocus.nameid GotTensions
                        , queryNodeData apis.gql newFocus.nameid GotData
                        ]

            else
                []
    in
    ( model
    , Cmd.batch (cmds ++ [ Global.sendSleep PassedSlowLoadTreshold 500 ])
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

                tensions_circle =
                    ternary (model.tensions_circle == Loading) LoadingSlowly model.tensions_circle

                data =
                    ternary (model.data == Loading) LoadingSlowly model.data
            in
            ( { model | orga_data = orga_data, tensions_circle = tensions_circle, data = data }, Cmd.none, Cmd.none )

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        -- Gql queries
        GotOrga result ->
            case result of
                Success data ->
                    if Dict.size data > 0 then
                        ( { model | orga_data = Success data }
                        , Ports.initGraphPack data model.node_focus.nameid
                        , send (UpdateSessionOrga (Just data))
                        )

                    else
                        ( { model | orga_data = Failure [ T.nodeNotExist ] }, Cmd.none, Cmd.none )

                other ->
                    ( { model | orga_data = result }, Cmd.none, send (UpdateSessionOrga Nothing) )

        GotTensions result ->
            case result of
                Success data ->
                    ( { model | tensions_circle = result }, Cmd.none, send (UpdateSessionTensions (Just data)) )

                other ->
                    ( { model | tensions_circle = result }, Cmd.none, send (UpdateSessionTensions Nothing) )

        GotData result ->
            case result of
                Success data ->
                    ( { model | data = result }, Cmd.none, send (UpdateSessionData (Just data)) )

                other ->
                    ( { model | data = result }, Cmd.none, send (UpdateSessionData Nothing) )

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
                                    ( { model | node_quickSearch = { qs | lookup = newLookup, visible = True } }, Cmd.none, Cmd.none )

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

                newIdx =
                    if pattern == "" then
                        0

                    else
                        qs.idx
            in
            ( { model | node_quickSearch = { qs | pattern = pattern, idx = newIdx, visible = True } }
            , Cmd.none
            , Ports.searchNode pattern
            )

        ChangeLookup nodes_ ->
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
                    ( model, send (ChangePattern ""), Cmd.none )

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

                other ->
                    ( model, Cmd.none, Cmd.none )

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
        DoTensionInit node ->
            let
                form =
                    { uctx = UserCtx "" Nothing (UserRights False False) []
                    , source = UserRole "" "" "" RoleType.Guest
                    , target = node
                    , targetData = model.data |> withDefaultData (NodeData node.nameid Nothing Nothing)
                    , tension_type = TensionType.Governance
                    , action = Nothing
                    , post = Dict.empty
                    , data = initNodeFragment
                    }

                newStep =
                    TensionInit form
            in
            ( { model | node_action = AddTension newStep }, Cmd.none, Ports.bulma_driver "actionModal" )

        DoTensionSource tensionType ->
            case global.session.user of
                LoggedOut ->
                    ( { model | node_action = ActionAuthNeeded }, Cmd.none, Cmd.none )

                LoggedIn uctx ->
                    case model.node_action of
                        AddTension (TensionInit form) ->
                            let
                                newForm =
                                    { form | uctx = uctx, tension_type = tensionType }

                                orgaRoles =
                                    uctx.roles |> List.filter (\r -> r.rootnameid == form.target.rootnameid)

                                newStep =
                                    case orgaRoles of
                                        [] ->
                                            TensionNotAuthorized [ T.notOrgMember, T.joinForTension ]

                                        [ r ] ->
                                            TensionFinal { newForm | source = r } NotAsked

                                        roles ->
                                            TensionSource newForm roles
                            in
                            ( { model | node_action = AddTension newStep }, Cmd.none, Ports.bulma_driver "actionModal" )

                        _ ->
                            ( { model | node_action = AskErr "Step moves not implemented" }, Cmd.none, Cmd.none )

        DoTensionFinal source ->
            case model.node_action of
                AddTension (TensionSource form roles) ->
                    let
                        newForm =
                            { form | source = source }
                    in
                    ( { model | node_action = AddTension <| TensionFinal newForm NotAsked }, Cmd.none, Ports.bulma_driver "actionModal" )

                _ ->
                    ( { model | node_action = AskErr "Step moves not implemented" }, Cmd.none, Cmd.none )

        SubmitTension form _ time ->
            let
                newForm =
                    { form | post = Dict.insert "createdAt" (fromTime time) form.post }
            in
            ( model, addOneTension apis.gql newForm TensionAck, Cmd.none )

        TensionAck result ->
            let
                maybeForm =
                    case model.node_action of
                        AddTension (TensionFinal form _) ->
                            Just form

                        AddCircle (NodeFinal form _) ->
                            Just form

                        EditAbout (NodeFinal form _) ->
                            Just form

                        EditMandate (NodeFinal form _) ->
                            Just form

                        other ->
                            Nothing

                subData =
                    model.subData

                tensionId =
                    result |> withMaybeData |> Maybe.map (\t -> t.id)

                tensions =
                    case result of
                        Success t ->
                            hotTensionPush t model.tensions_circle

                        other ->
                            []
            in
            case maybeForm of
                Just form ->
                    ( { model
                        | node_action = AddTension <| TensionFinal form result
                        , tensions_circle = Success tensions
                        , subData = { subData | tensionId = tensionId }
                      }
                    , Cmd.none
                    , send (UpdateSessionTensions (Just tensions))
                    )

                Nothing ->
                    ( { model | node_action = AskErr "Query method implemented from TensionAck" }, Cmd.none, Cmd.none )

        -- New Circle
        DoCircleInit node nodeType ->
            let
                form =
                    { uctx = UserCtx "" Nothing (UserRights False False) []
                    , source = UserRole "" "" "" RoleType.Guest
                    , target = node
                    , targetData = model.data |> withDefaultData (NodeData node.nameid Nothing Nothing)
                    , tension_type = TensionType.Governance
                    , post = Dict.empty
                    , action =
                        case nodeType of
                            NodeType.Circle ->
                                Just TensionAction.NewCircle

                            NodeType.Role ->
                                Just TensionAction.NewRole
                    , data = initNodeFragmentCircle nodeType RoleType.Peer
                    }

                newStep =
                    NodeInit form
            in
            ( { model | node_action = AddCircle newStep }, send DoCircleSource, Ports.bulma_driver "actionModal" )

        DoCircleSource ->
            case global.session.user of
                LoggedOut ->
                    ( { model | node_action = ActionAuthNeeded }, Cmd.none, Cmd.none )

                LoggedIn uctx ->
                    case model.node_action of
                        AddCircle (NodeInit form) ->
                            let
                                data =
                                    form.data

                                newForm =
                                    { form
                                        | uctx = uctx
                                        , data = { data | charac = Just form.target.charac, first_link = Just uctx.username }
                                    }

                                newStep =
                                    getNewNodeStepFromAuthForm newForm
                            in
                            ( { model | node_action = AddCircle newStep }, Cmd.none, Ports.bulma_driver "actionModal" )

                        _ ->
                            ( { model | node_action = AskErr "Step moves not implemented" }, Cmd.none, Cmd.none )

        DoCircleFinal source ->
            case model.node_action of
                AddCircle (NodeSource form roles) ->
                    let
                        newForm =
                            { form | source = source }
                    in
                    ( { model | node_action = AddCircle <| NodeFinal newForm NotAsked }, Cmd.none, Ports.bulma_driver "actionModal" )

                _ ->
                    ( { model | node_action = AskErr "Step moves not implemented" }, Cmd.none, Cmd.none )

        ChangeNodePost field value ->
            case model.node_action of
                AddTension (TensionFinal form result) ->
                    let
                        newForm =
                            { form | post = Dict.insert field value form.post }
                    in
                    ( { model | node_action = AddTension <| TensionFinal newForm result }, Cmd.none, Cmd.none )

                AddCircle (NodeFinal form result) ->
                    ( { model | node_action = AddCircle <| NodeFinal (updateNodeForm field value form) result }
                    , Cmd.none
                    , Cmd.none
                    )

                EditAbout (NodeFinal form result) ->
                    ( { model | node_action = EditAbout <| NodeFinal (updateNodeForm field value form) result }
                    , Cmd.none
                    , Cmd.none
                    )

                EditMandate (NodeFinal form result) ->
                    ( { model | node_action = EditMandate <| NodeFinal (updateNodeForm field value form) result }
                    , Cmd.none
                    , Cmd.none
                    )

                _ ->
                    ( { model | node_action = AskErr "Step moves not implemented" }, Cmd.none, Cmd.none )

        SubmitCircle form doClose time ->
            let
                newForm =
                    { form | post = Dict.union (Dict.fromList [ ( "createdAt", fromTime time ), ( "status", statusFromDoClose doClose ) ]) form.post }
            in
            if doClose == True then
                ( model, addOneCircle apis.gql newForm CircleAck, Cmd.none )

            else
                ( model, addCircleTension apis.gql newForm TensionAck, Cmd.none )

        CircleAck result ->
            let
                maybeForm =
                    case model.node_action of
                        AddCircle (NodeFinal form _) ->
                            Just form

                        AddTension (TensionFinal form _) ->
                            Just form

                        _ ->
                            Nothing
            in
            case maybeForm of
                Just form ->
                    case result of
                        Success nodes ->
                            let
                                ndata =
                                    hotNodePush nodes model.orga_data
                            in
                            ( { model | node_action = AddCircle <| NodeFinal form result, orga_data = Success ndata }
                            , Cmd.none
                            , Cmd.batch [ send UpdateUserToken, send (UpdateSessionOrga (Just ndata)) ]
                            )

                        other ->
                            ( { model | node_action = AddCircle <| NodeFinal form result }, Cmd.none, Cmd.none )

                Nothing ->
                    ( { model | node_action = AskErr "Query method not implemented from CircleAck" }, Cmd.none, Cmd.none )

        -- Edit About
        DoEditAbout node ->
            case global.session.user of
                LoggedOut ->
                    ( { model | node_action = ActionAuthNeeded }
                    , send DoOpenModal
                    , Cmd.none
                    )

                LoggedIn uctx ->
                    let
                        action =
                            case node.type_ of
                                NodeType.Role ->
                                    TensionAction.UpdateRoleAbout

                                NodeType.Circle ->
                                    TensionAction.UpdateCircleAbout

                        nodeData =
                            model.data |> withDefaultData (NodeData node.nameid Nothing Nothing)

                        form =
                            { uctx = uctx
                            , source = UserRole "" "" "" RoleType.Guest
                            , target = node
                            , targetData = nodeData
                            , tension_type = TensionType.Governance
                            , post = Dict.fromList [ ( "title", "[" ++ actionNameStr action ++ "] " ++ node.name ) ]
                            , action = Just action
                            , data = { initNodeFragment | name = Just node.name, about = nodeData.about }
                            }

                        newStep =
                            getNewNodeStepFromAuthForm form
                    in
                    ( { model | node_action = EditAbout newStep }
                    , send DoOpenModal
                    , Cmd.none
                    )

        DoEditAboutFinal source ->
            case model.node_action of
                EditAbout (NodeSource form roles) ->
                    let
                        newForm =
                            { form | source = source }
                    in
                    ( { model | node_action = EditAbout <| NodeFinal newForm NotAsked }, Cmd.none, Ports.bulma_driver "actionModal" )

                _ ->
                    ( { model | node_action = AskErr "Step moves not implemented" }, Cmd.none, Cmd.none )

        SubmitNodePatch form doClose time ->
            let
                newForm =
                    { form | post = Dict.union (Dict.fromList [ ( "createdAt", fromTime time ), ( "status", statusFromDoClose doClose ) ]) form.post }

                action =
                    form.action |> withDefault TensionAction.UpdateRoleAbout
            in
            if doClose == True then
                case action of
                    TensionAction.UpdateRoleAbout ->
                        ( model, patchNode apis.gql newForm AboutAck, Cmd.none )

                    TensionAction.UpdateCircleAbout ->
                        ( model, patchNode apis.gql newForm AboutAck, Cmd.none )

                    TensionAction.UpdateRoleMandate ->
                        ( model, patchNode apis.gql newForm MandateAck, Cmd.none )

                    TensionAction.UpdateCircleMandate ->
                        ( model, patchNode apis.gql newForm MandateAck, Cmd.none )

                    _ ->
                        ( model, addCircleTension apis.gql newForm TensionAck, Cmd.none )

            else
                ( model, addCircleTension apis.gql newForm TensionAck, Cmd.none )

        AboutAck result ->
            let
                maybeForm =
                    case model.node_action of
                        EditAbout (NodeFinal form _) ->
                            Just form

                        _ ->
                            Nothing
            in
            case maybeForm of
                Just form ->
                    case result of
                        Success nodes ->
                            let
                                data =
                                    model.data |> withDefaultData (NodeData form.target.nameid Nothing Nothing)
                            in
                            ( { model | node_action = EditAbout <| NodeFinal form result, data = Success { data | about = form.data.about } }
                            , Cmd.none
                            , Cmd.none
                            )

                        other ->
                            let
                                doRefreshToken =
                                    case other of
                                        Failure err ->
                                            if List.length err == 1 then
                                                case List.head err of
                                                    Just err_ ->
                                                        let
                                                            gqlErr =
                                                                err_
                                                                    |> String.replace "\n" ""
                                                                    |> SE.rightOf "{"
                                                                    |> SE.insertAt "{" 0
                                                                    |> JD.decodeString errorDecoder
                                                        in
                                                        case gqlErr of
                                                            Ok errGql ->
                                                                case List.head errGql.errors of
                                                                    Just e ->
                                                                        if e.message == "token is expired" then
                                                                            True

                                                                        else
                                                                            False

                                                                    Nothing ->
                                                                        False

                                                            Err errJD ->
                                                                False

                                                    Nothing ->
                                                        False

                                            else
                                                False

                                        _ ->
                                            False
                            in
                            if doRefreshToken then
                                ( { model | modalAuth = Active { post = Dict.fromList [ ( "username", form.uctx.username ) ], result = RemoteData.NotAsked } }, Cmd.none, Ports.open_auth_modal )

                            else
                                ( { model | node_action = EditAbout <| NodeFinal form result }, Cmd.none, Cmd.none )

                Nothing ->
                    ( { model | node_action = AskErr "Query method not implemented from AboutAck" }, Cmd.none, Cmd.none )

        -- Edit Mandate
        DoEditMandate node ->
            case global.session.user of
                LoggedOut ->
                    ( { model | node_action = ActionAuthNeeded }
                    , send DoOpenModal
                    , Cmd.none
                    )

                LoggedIn uctx ->
                    let
                        action =
                            case node.type_ of
                                NodeType.Role ->
                                    TensionAction.UpdateRoleMandate

                                NodeType.Circle ->
                                    TensionAction.UpdateCircleMandate

                        nodeData =
                            model.data |> withDefaultData (NodeData node.nameid Nothing Nothing)

                        form =
                            { uctx = uctx
                            , source = UserRole "" "" "" RoleType.Guest
                            , target = node
                            , targetData = nodeData
                            , tension_type = TensionType.Governance
                            , post = Dict.fromList [ ( "title", "[" ++ actionNameStr action ++ "] " ++ node.name ) ]
                            , action =
                                case node.type_ of
                                    NodeType.Role ->
                                        Just TensionAction.UpdateRoleMandate

                                    NodeType.Circle ->
                                        Just TensionAction.UpdateCircleMandate
                            , data = { initNodeFragment | mandate = nodeData.mandate }
                            }

                        newStep =
                            getNewNodeStepFromAuthForm form
                    in
                    ( { model | node_action = EditMandate newStep }
                    , send DoOpenModal
                    , Cmd.none
                    )

        DoEditMandateFinal source ->
            case model.node_action of
                EditMandate (NodeSource form roles) ->
                    let
                        newForm =
                            { form | source = source }
                    in
                    ( { model | node_action = EditMandate <| NodeFinal newForm NotAsked }, Cmd.none, Ports.bulma_driver "actionModal" )

                _ ->
                    ( { model | node_action = AskErr "Step moves not implemented" }, Cmd.none, Cmd.none )

        MandateAck result ->
            let
                maybeForm =
                    case model.node_action of
                        EditMandate (NodeFinal form _) ->
                            Just form

                        _ ->
                            Nothing
            in
            case maybeForm of
                Just form ->
                    case result of
                        Success nodes ->
                            let
                                data =
                                    model.data |> withDefaultData (NodeData form.target.nameid Nothing Nothing)
                            in
                            ( { model | node_action = EditMandate <| NodeFinal form result, data = Success { data | mandate = form.data.mandate } }
                            , Cmd.none
                            , Cmd.none
                            )

                        other ->
                            ( { model | node_action = EditMandate <| NodeFinal form result }, Cmd.none, Cmd.none )

                Nothing ->
                    ( { model | node_action = AskErr "Query method not implemented from MandateAck" }, Cmd.none, Cmd.none )

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
                            , Cmd.none
                            , Cmd.batch [ send UpdateUserToken, send (UpdateSessionOrga (Just ndata)) ]
                            )

                        other ->
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
                    ( { model | path_data = Just path }, Ports.drawButtonsGraphPack, send (UpdateSessionPath (Just path)) )

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

        ChangeInputViewMode viewMode ->
            let
                subData =
                    model.subData
            in
            ( { model | subData = { subData | viewMode = viewMode } }, Cmd.none, Cmd.none )

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
            ( { model | isModalActive = False, subData = initSubData }, gcmd, Ports.close_modal )

        DoCloseAuthModal ->
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
                    ( { model | modalAuth = Inactive }, Cmd.none, Global.send (UpdateUserSession uctx) )

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


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ _ =
    Sub.batch
        [ Ports.closeModalFromJs DoCloseModal
        , nodeClickedFromJs NodeClicked
        , nodeFocusedFromJs_ NodeFocused
        , nodeDataFromJs_ DoNodeAction
        , lookupFromJs_ ChangeLookup
        ]



-- Receive from Javascript


port nodeClickedFromJs : (String -> msg) -> Sub msg


port nodeFocusedFromJs : (JD.Value -> msg) -> Sub msg


port nodeDataFromJs : (JD.Value -> a) -> Sub a


port lookupFromJs : (JD.Value -> a) -> Sub a


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


lookupFromJs_ : (Nodes_ -> msg) -> Sub msg
lookupFromJs_ object =
    lookupFromJs
        (object
            << (\x ->
                    case x of
                        Ok n ->
                            Ok n

                        Err err ->
                            Err (JD.errorToString err)
               )
            << JD.decodeValue (JD.list nodeDecoder)
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
        maybeOrgFocus =
            case model.orga_data of
                Success d ->
                    let
                        focus =
                            Dict.get model.node_focus.nameid d
                    in
                    case model.path_data of
                        Just path ->
                            if List.length path.path > 0 then
                                ( Success d, focus )

                            else
                                ( Failure [ T.nodeNotExist ], Nothing )

                        Nothing ->
                            ( Success d, focus )

                other ->
                    ( other, Nothing )

        maybeOrg =
            Tuple.first maybeOrgFocus

        nodeFocus =
            Tuple.second maybeOrgFocus
    in
    -- [div [ class "column is-1 is-fullheight is-hidden-mobile", id "leftPane" ] [ viewLeftPane model ]
    div [ id "mainPane" ]
        [ HelperBar.view OverviewBaseUri
            global.session.user
            model.path_data
            (Submit <| DoJoinOrga model.node_focus.rootnameid)
        , div [ class "columns is-centered is-variable is-4" ]
            [ div [ class "column is-5-desktop is-5-widescreen is-4-fullhd" ]
                [ viewSearchBar model.orga_data model.path_data model.node_quickSearch
                , viewCanvas maybeOrg
                , br [] []
                , viewNodeInfo model.data nodeFocus
                , setupActionModal model
                ]
            , div [ class "column is-5" ]
                [ div [ class "columns is-gapless" ]
                    [ div [ class "column is-12", id "nextToChart" ]
                        [ viewActivies model ]
                    ]
                ]
            ]
        , refreshAuthModal model.modalAuth
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
                    [ li []
                        [ a []
                            [ Fa.icon "fas fa-scroll fa-xs" "Mandates" ]
                        ]
                    , li []
                        [ a []
                            --  fa-exclamation-circle
                            [ Fa.icon "fas fa-exchange-alt fa-xs" "Tensions" ]
                        ]
                    , li []
                        [ a []
                            [ Fa.icon "fas fa-history fa-xs" "Journal" ]
                        ]
                    , li []
                        [ a []
                            [ Fa.icon "fas fa-user fa-xs" "Members" ]
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

        isActive =
            ternary (True && qs.visible && Array.length qs.lookup > 0) " is-active " ""

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
        [ div [ class ("control has-icons-left is-expanded dropdown" ++ isActive) ]
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
            , div [ id "searchList", class "dropdown-menu" ]
                [ sortedLookup
                    |> List.indexedMap
                        (\i n ->
                            let
                                isSelected =
                                    ternary (i == qs.idx) " is-active " ""
                            in
                            [ tr
                                [ class ("drpdwn-item" ++ isSelected), onClickPD (NodeClicked n.nameid) ]
                              <|
                                [ th [] [ text n.name ] ]
                                    ++ (case n.type_ of
                                            NodeType.Circle ->
                                                [ td [] [ n.parent |> Maybe.map (\p -> p.nameid |> String.split "#" |> List.reverse |> List.head |> withDefault "") |> withDefault "" |> text ]
                                                , td [] [ n.first_link |> Maybe.map (\p -> "@" ++ p.username) |> withDefault "" |> text ]
                                                ]

                                            NodeType.Role ->
                                                [ td [] [ n.parent |> Maybe.map (\p -> p.nameid |> String.split "#" |> List.reverse |> List.head |> withDefault "") |> withDefault "" |> text ]
                                                , td [] [ n.first_link |> Maybe.map (\p -> "@" ++ p.username) |> withDefault "--" |> text ]
                                                ]
                                       )
                            ]
                                |> List.append
                                    (if i == 0 && n.type_ == NodeType.Circle then
                                        [ p [ class "help is-aligned-center is-size-6" ] [ text " Circle " ] ]

                                     else if n.type_ == NodeType.Role && (Array.get (i - 1) (Array.fromList sortedLookup) |> Maybe.map (\x -> x.type_ == NodeType.Circle) |> withDefault False) == True then
                                        [ p [ class "help is-aligned-center is-size-6" ] [ text " Role " ] ]

                                     else
                                        []
                                    )
                        )
                    |> List.concat
                    |> tbody []
                    |> List.singleton
                    |> List.append [ thead [] [ tr [] [ th [] [ text T.nameH ], th [] [ text T.parentH ], th [] [ text T.firstLinkH ] ] ] ]
                    |> div [ class "dropdown-content table is-fullwidth" ]
                ]
            , case node_ of
                Ok node ->
                    div
                        [ class "control controlButton" ]
                        [ div
                            [ class "button is-small is-info _modalTrigger_"
                            , attribute "data-modal" "actionModal"
                            , onClick (DoNodeAction node_)
                            ]
                            [ span [ class "has-text-weight-semibold text" ] [ node.name |> text ]
                            , span [ class "fa-stack  ellipsisArt" ]
                                [ i [ class "fas fa-ellipsis-h fa-stack-1x" ] [] ]
                            ]
                        ]

                Err err ->
                    div [] []
            ]
        ]


viewCanvas : GqlData NodesData -> Html Msg
viewCanvas odata =
    div [ id "canvasParent", classList [ ( "spinner", odata == LoadingSlowly ) ] ]
        [ case odata of
            Failure err ->
                viewGqlErrors err

            default ->
                div [] []
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

            --, div
            --    [ id "showLabel_cvbtn"
            --    , class "button buttonToggle tooltip has-tooltip-right"
            --    , attribute "data-tooltip" "Show/Hide circle tooltips."
            --    , onClick ToggleTooltips
            --    ]
            --    [ Fa.icon0 "fas fa-caret-square-down" "" ]
            ]
        , div
            [ id "nodeTooltip"
            , class "_modalTrigger_ is-invisible"
            , attribute "data-modal" "actionModal"
            ]
            [ span [] [ text "void" ] -- Node name
            , span [ class "fa-stack fa-sm ellipsisArt" ]
                [ i [ class "fas fa-ellipsis-h fa-stack-1x" ] [] ]
            ]
        ]


viewNodeInfo : GqlData NodeData -> Maybe Node -> Html Msg
viewNodeInfo nodeData maybeFocus =
    div [ id "mandateContainer", class "hero is-small is-light" ]
        [ div [ class "hero-body" ]
            [ case maybeFocus of
                Just focus ->
                    case focus.role_type of
                        Just r ->
                            let
                                fs =
                                    focus.first_link |> Maybe.map (\u -> u.username) |> withDefault "[Unknown]"
                            in
                            case nodeData of
                                Failure err ->
                                    case r of
                                        RoleType.Guest ->
                                            div [] [ [ "No mandate for Guest ", fs, "." ] |> String.join "" |> text ]

                                        other ->
                                            viewGqlErrors err

                                LoadingSlowly ->
                                    div [ class "spinner" ] []

                                Success data ->
                                    viewNodeDoc data focus

                                other ->
                                    div [] []

                        Nothing ->
                            case nodeData of
                                Failure err ->
                                    viewGqlErrors err

                                LoadingSlowly ->
                                    div [ class "spinner" ] []

                                Success data ->
                                    viewNodeDoc data focus

                                other ->
                                    div [] []

                Nothing ->
                    div [] []
            ]
        ]


viewNodeDoc : NodeData -> Node -> Html Msg
viewNodeDoc data focus =
    let
        viewMandateSection : String -> Maybe String -> Html Msg
        viewMandateSection name maybePara =
            case maybePara of
                Just para ->
                    div [ class "message" ]
                        [ div [ class "message-header" ] [ text name ]
                        , p [ class "message-body" ] [ renderMarkdown para "is-dark" ]
                        ]

                Nothing ->
                    div [] []
    in
    div []
        [ div [ class "aboutDoc" ]
            [ h1 [ class "subtitle is-5" ]
                [ span [ class "fa-stack", attribute "style" "font-size: 0.6em;" ]
                    [ i [ class "fas fa-info fa-stack-1x" ] []
                    , i [ class "far fa-circle fa-stack-2x" ] []
                    ]
                , span [ class "nodeName" ] [ text "\u{00A0}", text " ", text focus.name ]
                , span [ class "is-pulled-right button-light", onClick (DoEditAbout focus) ] [ Fa.icon0 "fas fa-xs fa-pen" "" ]
                ]
            , case data.about of
                Just ab ->
                    p [] [ text ab ]

                Nothing ->
                    div [] []
            , hr [ class "has-background-grey-light" ] []
            ]
        , case data.mandate of
            Just mandate ->
                div [ class "mandateDoc" ]
                    [ h1 [ class "subtitle is-5" ]
                        [ Fa.icon "fas fa-scroll fa-sm" T.mandateH, span [ class "is-pulled-right button-light", onClick (DoEditMandate focus) ] [ Fa.icon0 "fas fa-xs fa-pen" "" ] ]
                    , viewMandateSection T.purposeH (Just mandate.purpose)
                    , viewMandateSection T.responsabilitiesH mandate.responsabilities
                    , viewMandateSection T.domainsH mandate.domains
                    , viewMandateSection T.policiesH mandate.policies
                    ]

            Nothing ->
                div [ class "is-italic" ]
                    [ text "No mandate for this circle.", span [ class "is-pulled-right button-light", onClick (DoEditMandate focus) ] [ Fa.icon0 "fas fa-md fa-pen" "" ] ]
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
                    [ li [ class "is-active" ]
                        [ a [] [ Fa.icon "fas fa-exchange-alt fa-sm" T.tensionH ] ]
                    , li []
                        [ a [ class "has-text-grey" ] [ Fa.icon "fas fa-history fa-sm" T.journalH ] ]
                    ]
                ]
            ]
        , div [ classList [ ( "content", True ), ( "spinner", model.tensions_circle == LoadingSlowly ) ] ]
            [ case model.tensions_circle of
                Success tensions ->
                    if List.length tensions > 0 then
                        List.map (\t -> mediaTension OverviewBaseUri model.node_focus t Navigate) tensions
                            ++ ternary (List.length tensions > 5)
                                [ div [ class "is-aligned-center" ] [ a [ href (uriFromNameid TensionsBaseUri model.node_focus.nameid) ] [ text T.seeMore ] ] ]
                                []
                            |> div [ class "is-size-7", id "tensionsTab" ]

                    else
                        case model.node_focus.type_ of
                            NodeType.Role ->
                                div [] [ text T.noTensionRole ]

                            NodeType.Circle ->
                                div [] [ text T.noTensionCircle ]

                Failure err ->
                    viewGqlErrors err

                default ->
                    div [] []
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
            [ case model.node_action of
                action ->
                    viewActionStep model action
            ]
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
            viewTensionStep step model.subData

        AddCircle step ->
            viewCircleStep step model.subData

        EditAbout step ->
            viewEditAboutStep step model.subData

        EditMandate step ->
            viewEditMandateStep step model.subData

        JoinOrga step ->
            viewJoinOrgaStep model.orga_data step

        NoOp ->
            text ""

        AskErr err ->
            viewGqlErrors [ err ]

        ActionAuthNeeded ->
            viewAuthNeeded (DoCloseModal (Route.toHref Route.Login))


viewTensionStep : TensionStep TensionForm -> SubmitCircleData -> Html Msg
viewTensionStep step subData =
    case step of
        TensionInit form ->
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
                        <|
                            TensionType.list
                    ]
                ]

        TensionSource form roles ->
            viewSourceRoles form roles DoTensionFinal

        TensionFinal form result ->
            case form.action of
                Just action ->
                    case action of
                        TensionAction.NewRole ->
                            Form.NewCircle.view form result { subData | submitNextMsg = SubmitCircle }

                        TensionAction.NewCircle ->
                            Form.NewCircle.view form result { subData | submitNextMsg = SubmitCircle }

                        TensionAction.UpdateRoleAbout ->
                            Form.EditCircle.viewAbout form result { subData | submitNextMsg = SubmitNodePatch }

                        TensionAction.UpdateCircleAbout ->
                            Form.EditCircle.viewAbout form result { subData | submitNextMsg = SubmitNodePatch }

                        TensionAction.UpdateRoleMandate ->
                            Form.EditCircle.viewMandate form result { subData | submitNextMsg = SubmitNodePatch }

                        TensionAction.UpdateCircleMandate ->
                            Form.EditCircle.viewMandate form result { subData | submitNextMsg = SubmitNodePatch }

                Nothing ->
                    Form.NewTension.view form result { subData | submitNextMsg = SubmitTension }

        TensionNotAuthorized errMsg ->
            viewWarnings errMsg


viewCircleStep : NodeStep TensionForm data -> SubmitCircleData -> Html Msg
viewCircleStep step subData =
    case step of
        NodeInit form ->
            -- Node mode selection not implemented yet.
            div [] [ text "" ]

        NodeSource form roles ->
            viewSourceRoles form roles DoCircleFinal

        NodeFinal form result ->
            Form.NewCircle.view form result { subData | submitNextMsg = SubmitCircle }

        NodeNotAuthorized errMsg ->
            viewWarnings errMsg


viewEditAboutStep : NodeStep TensionForm data -> SubmitCircleData -> Html Msg
viewEditAboutStep step subData =
    case step of
        NodeInit form ->
            -- Node mode selection not implemented yet.
            div [] [ text "" ]

        NodeSource form roles ->
            viewSourceRoles form roles DoEditAboutFinal

        NodeFinal form result ->
            Form.EditCircle.viewAbout form result { subData | submitNextMsg = SubmitNodePatch }

        NodeNotAuthorized errMsg ->
            viewWarnings errMsg


viewEditMandateStep : NodeStep TensionForm data -> SubmitCircleData -> Html Msg
viewEditMandateStep step subData =
    case step of
        NodeInit form ->
            -- Node mode selection not implemented yet.
            div [] [ text "" ]

        NodeSource form roles ->
            viewSourceRoles form roles DoEditMandateFinal

        NodeFinal form result ->
            Form.EditCircle.viewMandate form result { subData | submitNextMsg = SubmitNodePatch }

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
                        [ Fa.icon0 "fas fa-check fa-2x has-text-success" " "
                        , text (T.welcomIn ++ " ")
                        , span [ class "has-font-weight-semibold" ] [ getNodeName form.rootnameid orga |> text ]
                        ]

                Failure err ->
                    viewGqlErrors err

                default ->
                    div [ class "box spinner" ] [ text T.loading ]

        JoinNotAuthorized errMsg ->
            viewGqlErrors errMsg



-- Utils View


refreshAuthModal : ModalAuth -> Html Msg
refreshAuthModal modalAuth =
    let
        form_m =
            case modalAuth of
                Active f ->
                    Just f

                _ ->
                    Nothing
    in
    div
        [ id "refreshAuthModal"
        , class "modal modal2 modal-pos-top modal-fx-fadeIn elmModal"
        , classList [ ( "is-active", form_m /= Nothing ) ]
        ]
        [ div [ class "modal-background", onClick DoCloseAuthModal ] []
        , div [ class "modal-content" ]
            [ div [ class "box" ]
                [ p [] [ text "Your session expired. Please, confirm your password:" ]
                , div [ class "field is-horizntl" ]
                    [ div [ class "field-lbl" ] [ label [ class "label" ] [ text "Password" ] ]
                    , div [ class "field-body" ]
                        [ div [ class "field" ]
                            [ div [ class "control" ]
                                [ input
                                    [ id "passwordInput"
                                    , class "input followFocus"
                                    , attribute "data-nextfocus" "submitButton"
                                    , type_ "password"
                                    , placeholder "password"
                                    , name "password"
                                    , attribute "autocomplete" "password"
                                    , required True
                                    , onInput (ChangeAuthPost "password")
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                , div [ class "field is-grouped is-grouped-right" ]
                    [ div [ class "control" ]
                        [ case form_m of
                            Just form ->
                                if Form.isPostSendable [ "password" ] form.post then
                                    button
                                        [ id "submitButton"
                                        , class "button is-success has-text-weight-semibold"
                                        , onClick (SubmitUser form)
                                        ]
                                        [ text "Refresh" ]

                                else
                                    button [ class "button has-text-weight-semibold", disabled True ]
                                        [ text "Refresh" ]

                            Nothing ->
                                div [] []
                        ]
                    ]
                , div []
                    [ case form_m |> Maybe.map (\f -> f.result) |> withDefault RemoteData.NotAsked of
                        RemoteData.Failure err ->
                            viewHttpErrors err

                        default ->
                            text ""
                    ]
                ]
            ]
        , button [ class "modal-close is-large", onClick DoCloseAuthModal ] []
        ]


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



-- Methods


{-| get Auth Step and init form based on user roles
-}
getNewNodeStepFromAuthForm : TensionForm -> NodeStep TensionForm d
getNewNodeStepFromAuthForm form =
    let
        orgaRoles =
            form.uctx.roles |> List.filter (\r -> r.rootnameid == form.target.rootnameid)
    in
    case orgaRoles of
        [] ->
            NodeNotAuthorized [ T.notOrgMember, T.joinForCircle ]

        roles ->
            let
                nearestNid =
                    ternary (form.target.parent == Nothing)
                        form.target.nameid
                        (form.target.parent |> Maybe.map (\n -> n.nameid) |> withDefault form.target.nameid)

                circleRoles =
                    roles |> List.filter (\r -> getParentidFromRole r == nearestNid)
            in
            case circleRoles of
                [] ->
                    NodeNotAuthorized [ T.notCircleMember, T.askCoordo ]

                subRoles ->
                    case form.target.charac.mode of
                        NodeMode.Chaos ->
                            case subRoles of
                                [ r ] ->
                                    let
                                        newForm =
                                            { form | source = r }
                                    in
                                    NodeFinal newForm NotAsked

                                subRoles2 ->
                                    NodeSource form subRoles2

                        NodeMode.Coordinated ->
                            let
                                coordoRoles =
                                    subRoles |> List.filter (\r -> r.role_type == RoleType.Coordinator)
                            in
                            case coordoRoles of
                                [] ->
                                    NodeNotAuthorized [ T.notCircleCoordo, T.askCoordo ]

                                [ r ] ->
                                    let
                                        newForm =
                                            { form | source = r }
                                    in
                                    NodeFinal newForm NotAsked

                                subRoles2 ->
                                    NodeSource form subRoles2


updateNodeForm : String -> String -> TensionForm -> TensionForm
updateNodeForm field value form =
    let
        data =
            form.data

        mandate =
            data.mandate |> withDefault initMandate
    in
    case field of
        -- Node data
        "role_type" ->
            { form | data = { data | role_type = value |> RoleType.fromString |> withDefault RoleType.Peer |> Just } }

        "name" ->
            case form.action of
                Nothing ->
                    { form | post = Dict.insert field value form.post }

                Just action ->
                    if List.member action [ TensionAction.NewRole, TensionAction.NewCircle ] then
                        let
                            newPost =
                                Dict.insert "title" ("[" ++ actionNameStr action ++ "] " ++ value) form.post

                            newData =
                                { data
                                    | name = Just value
                                    , nameid = makeNewNodeId value
                                }
                        in
                        { form | post = newPost, data = newData }

                    else
                        { form | data = { data | name = Just value } }

        "first_link" ->
            { form | data = { data | first_link = Just value } }

        "about" ->
            { form | data = { data | about = Just value } }

        -- Mandate data
        "purpose" ->
            { form | data = { data | mandate = Just { mandate | purpose = value } } }

        "responsabilities" ->
            { form | data = { data | mandate = Just { mandate | responsabilities = Just value } } }

        "domains" ->
            { form | data = { data | mandate = Just { mandate | domains = Just value } } }

        "policies" ->
            { form | data = { data | mandate = Just { mandate | policies = Just value } } }

        other ->
            -- title, message...
            { form | post = Dict.insert field value form.post }


makeNewNodeId : String -> Maybe String
makeNewNodeId name =
    name
        |> String.toLower
        |> String.trim
        |> String.map
            (\c ->
                if List.member c [ ' ', '/', '=', '?', '#', '&', '?', '|', '%', '$', '\\' ] then
                    '-'

                else if List.member c [ '@', '(', ')', '<', '>', '[', ']', '{', '}', '"', '`', '\'' ] then
                    '_'

                else
                    c
            )
        |> Just


statusFromDoClose : Bool -> String
statusFromDoClose doClose =
    ternary (doClose == True)
        (TensionStatus.toString TensionStatus.Closed)
        (TensionStatus.toString TensionStatus.Open)
