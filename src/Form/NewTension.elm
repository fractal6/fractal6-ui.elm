module Form.NewTension exposing (..)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Codecs exposing (LookupResult)
import Components.LabelSearchPanel as LabelSearchPanel
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.MoveTension exposing (viewNodeSelect)
import Components.NodeDoc as NodeDoc
    exposing
        ( NodeDoc
        , NodeView(..)
        , viewAboutInput2
        , viewMandateInput
        )
import Components.UserInput as UserInput
import Dict
import Extra exposing (ternary)
import Extra.Events exposing (onClickPD, onClickPD2, onEnter, onKeydown, onTab)
import Extra.Views exposing (showMsg)
import Form exposing (isPostEmpty, isPostSendable, isUsersSendable)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.NodeVisibility as NodeVisibility
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (toHref)
import Global exposing (Msg(..), send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, autofocus, class, classList, contenteditable, disabled, href, id, list, placeholder, required, rows, spellcheck, style, tabindex, target, title, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading
    exposing
        ( ErrorData
        , GqlData
        , ModalData
        , RequestResult(..)
        , isSuccess
        , viewAuthNeeded
        , viewGqlErrors
        , viewRoleNeeded
        , withDefaultData
        , withMaybeData
        )
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelCommon
    exposing
        ( Ev
        , InputViewMode(..)
        , TensionForm
        , UserState(..)
        , getChildren
        , getCircles
        , getNode
        , getParentFragmentFromRole
        , initTensionForm
        , isSelfContract
        , localGraphFromOrga
        , makeCandidateContractForm
        , sortNode
        , tensionToActionForm
        )
import ModelCommon.Codecs exposing (DocType(..), FractalBaseRoute(..), getOrgaRoles, nearestCircleid, nid2rootid, nid2type, nodeIdCodec, uriFromNameid)
import ModelCommon.View exposing (FormText, action2icon, getNodeTextFromNodeType, getTensionText, roleColor, tensionIcon2, tensionTypeColor, viewRoleExt)
import ModelSchema exposing (..)
import Ports
import Query.AddContract exposing (addOneContract)
import Query.AddTension exposing (addOneTension)
import Query.PatchTension exposing (actionRequest)
import Query.QueryNode exposing (queryLocalGraph, queryRoles)
import Session exposing (Apis, GlobalCmd(..), LabelSearchPanelOnClickAction(..))
import Text as T exposing (textH, textT, upH)
import Time


type State
    = State Model


type alias Model =
    { user : UserState
    , nodeDoc : NodeDoc -- form
    , result : GqlData Tension
    , sources : List EmitterOrReceiver
    , step : TensionStep
    , isActive : Bool
    , isActive2 : Bool -- Let minimze VDOM load + prevent glitch while keeping css effects
    , activeTab : TensionTab
    , viewMode : InputViewMode
    , activeButton : Maybe Int -- 0: creating role, 1: creating tension (no pushing blob)
    , path_data : GqlData LocalGraph
    , action_result : GqlData IdPayload
    , doInvite : Bool

    -- Role/Circle
    , nodeStep : NodeStep
    , txt : FormText
    , roles_result : GqlData (List RoleExtFull)
    , force_init : Bool

    -- Common
    , refresh_trial : Int
    , modal_confirm : ModalConfirm Msg

    -- Components
    , labelsPanel : LabelSearchPanel.State
    , userInput : UserInput.State
    }


type TensionTab
    = NewTensionTab
    | NewRoleTab
    | NewCircleTab


type TensionStep
    = TensionTypes
    | TensionSource
    | TensionFinal
    | TensionNotAuthorized ErrorData
    | AuthNeeded


type NodeStep
    = RoleAuthorityStep
    | CircleVisibilityStep
    | NodeValidateStep
    | InviteStep


type NewTensionInput
    = FromNameid String
    | FromPath LocalGraph


nodeStepToString : TensionForm -> NodeStep -> String
nodeStepToString form step =
    case step of
        RoleAuthorityStep ->
            case form.node.role_type of
                Just x ->
                    T.role ++ " (" ++ RoleType.toString x ++ ")"

                Nothing ->
                    T.role

        CircleVisibilityStep ->
            case form.node.visibility of
                Just x ->
                    T.visibility ++ " (" ++ NodeVisibility.toString x ++ ")"

                Nothing ->
                    T.visibility

        NodeValidateStep ->
            T.reviewAndValidate

        InviteStep ->
            T.invite


init : UserState -> State
init user =
    initModel user |> State


initModel : UserState -> Model
initModel user =
    { user = user
    , result = NotAsked
    , sources = []
    , step = TensionFinal
    , isActive = False
    , isActive2 = False
    , activeTab = NewTensionTab
    , viewMode = Write
    , activeButton = Nothing
    , nodeDoc = NodeDoc.init "" NodeEdit user
    , path_data = NotAsked
    , action_result = NotAsked
    , doInvite = False
    , force_init = False

    -- Role/Circle
    , nodeStep = RoleAuthorityStep -- will change
    , txt = getTensionText
    , roles_result = Loading

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg

    -- Components
    , labelsPanel = LabelSearchPanel.init "" SelectLabel user
    , userInput = UserInput.init False user
    }


initTensionTab : Model -> Model
initTensionTab model =
    let
        form =
            model.nodeDoc.form

        newForm =
            { form
                | type_ = Just TensionType.Operational
                , action = Nothing
                , blob_type = Nothing
                , users = []
            }
    in
    { model
        | activeTab = NewTensionTab
        , nodeDoc = NodeDoc.setForm newForm model.nodeDoc
        , result = NotAsked
        , txt = getTensionText
        , force_init = False
    }


initCircleTab : NodeType.NodeType -> Model -> Model
initCircleTab type_ model =
    let
        form =
            model.nodeDoc.form

        node =
            form.node

        newForm =
            { form
                | type_ = Just TensionType.Governance
                , blob_type = Just BlobType.OnNode
                , node = { node | type_ = Just type_ }
                , users = []
            }
                |> NodeDoc.updateNodeForm "name" (withDefault "" node.name)
    in
    case type_ of
        NodeType.Role ->
            { model
                | activeTab = NewRoleTab
                , nodeDoc = NodeDoc.setForm { newForm | action = Just TensionAction.NewRole } model.nodeDoc
                , result = NotAsked
                , nodeStep = RoleAuthorityStep
                , txt = getNodeTextFromNodeType type_
                , force_init = False
            }

        NodeType.Circle ->
            { model
                | activeTab = NewCircleTab
                , nodeDoc = NodeDoc.setForm { newForm | action = Just TensionAction.NewCircle } model.nodeDoc
                , result = NotAsked
                , nodeStep = CircleVisibilityStep
                , txt = getNodeTextFromNodeType type_
                , force_init = False
            }



-- Global methods
--  nothing here
--- State Controls


setPath : LocalGraph -> Model -> Model
setPath p model =
    let
        sources =
            getOrgaRoles [ nid2rootid p.focus.nameid ] model.nodeDoc.form.uctx.roles
                |> List.filter (\r -> r.role_type /= RoleType.Owner)
                |> List.map (\r -> { nameid = r.nameid, name = r.name, role_type = Just r.role_type })

        extras =
            getOrgaRoles [ nid2rootid p.focus.nameid ] model.nodeDoc.form.uctx.roles
                |> List.filter (\r -> r.role_type == RoleType.Owner)
                |> List.map (\r -> { nameid = r.nameid, name = r.name, role_type = Just r.role_type })

        default_source =
            case List.filter (\r -> nearestCircleid r.nameid == p.focus.nameid) sources |> List.head of
                Just r ->
                    -- First roles in target
                    r

                Nothing ->
                    case
                        List.filter
                            (\r ->
                                nearestCircleid r.nameid
                                    == (List.reverse p.path
                                            |> List.tail
                                            |> Maybe.map List.head
                                            |> withDefault Nothing
                                            |> Maybe.map .nameid
                                            |> withDefault ""
                                       )
                            )
                            sources
                            |> List.head
                    of
                        Just r ->
                            -- or first roles in parent
                            r

                        Nothing ->
                            -- or first role in orga
                            (sources ++ extras) |> List.head |> withDefault model.nodeDoc.form.source
    in
    { model
        | sources = sources ++ extras
        , path_data = Success p
    }
        |> setSource default_source
        |> setTarget (shrinkNode p.focus)


switchTab : TensionTab -> Model -> Model
switchTab tab model =
    if tab == model.activeTab && not model.force_init then
        model

    else
        case tab of
            NewTensionTab ->
                initTensionTab model

            NewRoleTab ->
                initCircleTab NodeType.Role model

            NewCircleTab ->
                initCircleTab NodeType.Circle model


changeNodeStep : NodeStep -> Model -> Model
changeNodeStep step model =
    let
        nodeDoc =
            model.nodeDoc

        nd =
            if model.activeTab == NewRoleTab && step == NodeValidateStep && nodeDoc.form.node.role_type == Nothing then
                let
                    form =
                        nodeDoc.form

                    node =
                        form.node
                in
                { nodeDoc | form = { form | node = { node | role_type = Just RoleType.Peer } } }

            else
                nodeDoc
    in
    { model | nodeStep = step, nodeDoc = nd }


close : Model -> Model
close data =
    { data | isActive = False }


setActiveButton : Bool -> Model -> Model
setActiveButton doClose data =
    if doClose then
        { data | activeButton = Just 0 }

    else
        { data | activeButton = Just 1 }


setViewMode : InputViewMode -> Model -> Model
setViewMode viewMode data =
    { data | viewMode = viewMode }


setStep : TensionStep -> Model -> Model
setStep step data =
    { data | step = step }


setResult : GqlData Tension -> Model -> Model
setResult result data =
    { data | result = result }



-- Update Form


setUctx : UserCtx -> Model -> Model
setUctx uctx data =
    { data | user = LoggedIn uctx, nodeDoc = NodeDoc.setUctx uctx data.nodeDoc }


setTensionType : TensionType.TensionType -> Model -> Model
setTensionType type_ data =
    { data | nodeDoc = NodeDoc.setTensionType type_ data.nodeDoc }


setSource : EmitterOrReceiver -> Model -> Model
setSource source data =
    { data | nodeDoc = NodeDoc.setSource source data.nodeDoc }


setTarget : PNode -> Model -> Model
setTarget target data =
    { data | nodeDoc = NodeDoc.setTarget target data.nodeDoc }


setSourceShort : String -> Model -> Model
setSourceShort nameid data =
    { data | nodeDoc = NodeDoc.setSourceShort nameid data.nodeDoc }


setTargetShort : String -> Model -> Model
setTargetShort nameid data =
    { data | nodeDoc = NodeDoc.setTargetShort nameid data.nodeDoc }


setStatus : TensionStatus.TensionStatus -> Model -> Model
setStatus status data =
    { data | nodeDoc = NodeDoc.setStatus status data.nodeDoc }


setEvents : List Ev -> Model -> Model
setEvents events data =
    { data | nodeDoc = NodeDoc.setEvents events data.nodeDoc }


setLabels : List Label -> Model -> Model
setLabels labels data =
    { data | nodeDoc = NodeDoc.setLabels labels data.nodeDoc }


addLabel : Label -> Model -> Model
addLabel label data =
    { data | nodeDoc = NodeDoc.addLabel label data.nodeDoc }


removeLabel : Label -> Model -> Model
removeLabel label data =
    { data | nodeDoc = NodeDoc.removeLabel label data.nodeDoc }


post : String -> String -> Model -> Model
post field value data =
    let
        f =
            data.nodeDoc.form

        newForm =
            { f | post = Dict.insert field value f.post }
    in
    { data | nodeDoc = NodeDoc.setForm newForm data.nodeDoc }


resetPost : Model -> Model
resetPost data =
    { data | nodeDoc = NodeDoc.resetPost data.nodeDoc }


resetModel : Model -> Model
resetModel data =
    initModel data.user



-- User Lookup
--updateUserPattern : Int -> String -> Model -> Model
--updateUserPattern pos pattern data =
--    let
--        f =
--            data.form
--
--        newForm =
--            { f | users = NodeDoc.updateUserPattern_ pos pattern f.users }
--    in
--    { data | form = newForm }
--
--
--cancelUser : Int -> Model -> Model
--cancelUser pos data =
--    let
--        f =
--            data.form
--
--        newForm =
--            { f | users = NodeDoc.cancelUser_ pos f.users }
--    in
--    { data | form = newForm, isLookupOpen = False }
--
--
--openLookup : Model -> Model
--openLookup data =
--    { data | isLookupOpen = True }
--
--
--closeLookup : Model -> Model
--closeLookup data =
--    { data | isLookupOpen = False }
--
-- utils


canExitSafe : Model -> Bool
canExitSafe data =
    not (hasData data) || isSuccess data.result


hasData : Model -> Bool
hasData data =
    isPostEmpty [ "title", "message" ] data.nodeDoc.form.post == False



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = -- Data control
      PushTension (GqlData Tension -> Msg)
    | OnSubmit (Time.Posix -> Msg)
    | GotPath Bool (GqlData LocalGraph) -- GraphQL
      -- Modal control
    | SetIsActive2 Bool
    | OnOpen NewTensionInput
    | OnOpenRole NewTensionInput
    | OnOpenCircle NewTensionInput
    | OnReset
    | OnClose ModalData
    | OnCloseSafe String String
    | OnSwitchTab TensionTab
    | OnGotRoles (GqlData (List RoleExtFull))
    | OnChangeNodeStep NodeStep
    | OnTensionStep TensionStep
    | OnChangeInputViewMode InputViewMode
    | OnTargetClick
    | DoInvite
    | OnInvite Time.Posix
    | PushAck (GqlData IdPayload)
      -- Doc change
    | OnChangeTensionType TensionType.TensionType
    | OnChangeTensionSource EmitterOrReceiver
    | OnChangeTensionTarget Node
    | OnChangePost String String
    | OnSelectRoleExt RoleExtFull
    | OnSelectVisibility NodeVisibility.NodeVisibility
    | OnAddDomains
    | OnAddPolicies
    | OnAddResponsabilities
    | OnSubmitTension Bool Time.Posix
    | OnTensionAck (GqlData Tension)
      -- User Quick Search
      --| OnChangeUserPattern Int String
      --| OnChangeUserLookup (LookupResult User)
      --| OnSelectUser Int String
      --| OnCancelUser Int
      --| OnShowLookupFs
      --| OnCancelLookupFs
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Common
    | NoMsg
    | LogErr String
    | UpdateUctx UserCtx
      -- Components
    | LabelSearchPanelMsg LabelSearchPanel.Msg
    | UserInputMsg UserInput.Msg


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe Bool
    }


noOut : Out
noOut =
    Out [] [] Nothing


out0 : List (Cmd Msg) -> Out
out0 cmds =
    Out cmds [] Nothing


out1 : List GlobalCmd -> Out
out1 cmds =
    Out [] cmds Nothing


out2 : List (Cmd Msg) -> List GlobalCmd -> Out
out2 cmds gcmds =
    Out cmds gcmds Nothing


mapGlobalOutcmds : List GlobalCmd -> ( List (Cmd Msg), List (Cmd Global.Msg) )
mapGlobalOutcmds gcmds =
    gcmds
        |> List.map
            (\m ->
                case m of
                    DoModalAsk link reset ->
                        ( send (OnCloseSafe link reset), Cmd.none )

                    _ ->
                        ( Cmd.none, Cmd.none )
            )
        |> List.unzip


update : Apis -> Msg -> State -> ( State, Out )
update apis message (State model) =
    update_ apis message model
        |> Tuple.mapFirst State


update_ : Apis -> Msg -> Model -> ( Model, Out )
update_ apis message model =
    case message of
        -- Data control
        PushTension ack ->
            ( model, out0 [ addOneTension apis model.nodeDoc.form ack ] )

        OnSubmit next ->
            ( model, out0 [ sendNow next ] )

        GotPath isInit result ->
            case result of
                Success path ->
                    let
                        prevPath =
                            if isInit then
                                { path | path = [] }

                            else
                                withDefaultData path model.path_data
                    in
                    case path.root of
                        Just root ->
                            let
                                newPath =
                                    { prevPath | root = Just root, path = path.path ++ (List.tail prevPath.path |> withDefault []) }
                            in
                            ( model, out0 [ send (OnOpen (FromPath newPath)) ] )

                        Nothing ->
                            let
                                newPath =
                                    { prevPath | path = path.path ++ (List.tail prevPath.path |> withDefault []) }

                                nameid =
                                    List.head path.path |> Maybe.map (\p -> p.nameid) |> withDefault ""
                            in
                            ( { model | path_data = Success newPath }, out0 [ queryLocalGraph apis nameid (GotPath False) ] )

                Failure err ->
                    ( { model | path_data = result }, out0 [ Ports.logErr (String.join " | " err) ] )

                _ ->
                    ( { model | path_data = result }, noOut )

        -- Modal control
        SetIsActive2 v ->
            -- Prevent elm from computing the VDOM
            if v then
                ( { model | isActive = model.isActive2 }, out0 [ Ports.open_modal "tensionModal" ] )

            else
                ( { model | isActive2 = model.isActive }, noOut )

        OnOpen t ->
            case model.user of
                LoggedIn uctx ->
                    case t of
                        FromNameid nameid ->
                            ( model, out0 [ queryLocalGraph apis nameid (GotPath True) ] )

                        FromPath p ->
                            let
                                data =
                                    setPath p model
                            in
                            if data.sources == [] && data.refresh_trial == 0 then
                                ( { data | refresh_trial = 1 }, Out [ sendSleep (OnOpen (FromPath p)) 500 ] [ DoUpdateToken ] Nothing )

                            else if data.sources == [] then
                                ( { data | isActive2 = True } |> setStep (TensionNotAuthorized [ T.notOrgMember, T.joinForTension ])
                                , out0 [ sendSleep (SetIsActive2 True) 10 ]
                                )

                            else
                                ( { data | isActive2 = True } |> setUctx uctx
                                , out0 [ sendSleep (SetIsActive2 True) 10 ]
                                )

                LoggedOut ->
                    ( { model | isActive2 = True } |> setStep AuthNeeded, out0 [ send (SetIsActive2 True) ] )

        OnOpenRole t ->
            ( { model | activeTab = NewRoleTab, force_init = True }, out0 [ sendSleep (OnSwitchTab NewRoleTab) 333, send (OnOpen t) ] )

        OnOpenCircle t ->
            ( { model | activeTab = NewCircleTab, force_init = True }, out0 [ send (OnSwitchTab NewCircleTab), send (OnOpen t) ] )

        OnClose data ->
            let
                ( newModel, gcmds ) =
                    if data.link == "" then
                        ( model, [] )

                    else
                        ( { model | isActive2 = True }, [ DoNavigate data.link ] )
            in
            ( { newModel | isActive = False }
            , out2
                [ Ports.close_modal
                , ternary data.reset (sendSleep OnReset 333) Cmd.none
                , sendSleep (SetIsActive2 False) 500
                ]
                gcmds
            )

        OnReset ->
            ( resetModel model, noOut )

        OnCloseSafe link onCloseTxt ->
            if canExitSafe model then
                ( model, out0 [ send (OnClose { reset = True, link = link }) ] )

            else
                ( model
                , out0
                    [ send
                        (DoModalConfirmOpen (OnClose { reset = True, link = link })
                            { message = Nothing, txts = [ ( upH T.confirmUnsaved, onCloseTxt ) ] }
                        )
                    ]
                )

        OnSwitchTab tab ->
            let
                cmds =
                    case tab of
                        NewRoleTab ->
                            if withMaybeData model.roles_result == Nothing then
                                let
                                    nameids =
                                        getCircles model.path_data |> List.map .nameid
                                in
                                [ queryRoles apis nameids OnGotRoles ]

                            else
                                []

                        _ ->
                            []
            in
            ( switchTab tab model, out0 ([ Ports.bulma_driver "tensionModal" ] ++ cmds) )

        OnGotRoles result ->
            ( { model | roles_result = result }, noOut )

        OnChangeNodeStep step ->
            ( changeNodeStep step model, out0 [ Ports.bulma_driver "tensionModal" ] )

        OnTensionStep step ->
            ( setStep step model, out0 [ Ports.bulma_driver "tensionModal" ] )

        OnChangeInputViewMode viewMode ->
            ( setViewMode viewMode model, noOut )

        OnTargetClick ->
            ( model, out0 [ Ports.requireTreeData ] )

        DoInvite ->
            ( { model | doInvite = True }, noOut )

        OnInvite time ->
            let
                form =
                    model.nodeDoc.form

                newModel =
                    model
                        |> post "createdAt" (fromTime time)
                        |> setEvents
                            [ Ev TensionEvent.MemberLinked
                                ""
                                ((List.head form.users |> Maybe.map (\x -> ternary (x.email == "") x.username x.email))
                                    |> withDefault ""
                                )
                            ]

                aform =
                    tensionToActionForm newModel.nodeDoc.form
            in
            ( { newModel | action_result = LoadingSlowly }
            , if isSelfContract form.uctx form.users then
                out0 [ actionRequest apis aform PushAck ]

              else
                let
                    contractForms =
                        makeCandidateContractForm aform
                in
                out0 (List.map (\c -> addOneContract apis c PushAck) contractForms)
            )

        PushAck result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | action_result = NotAsked }, out0 [ Ports.raiseAuthModal model.nodeDoc.form.uctx ] )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, out2 [ sendSleep (OnSubmit OnInvite) 500 ] [ DoUpdateToken ] )

                OkAuth _ ->
                    let
                        form =
                            model.nodeDoc.form
                    in
                    ( { model | action_result = result }
                    , ternary (isSelfContract form.uctx form.users) (out1 [ DoUpdateToken ]) noOut
                    )

                DuplicateErr ->
                    ( { model | action_result = Failure [ "Duplicate Error: A similar contract already exists, please check it out." ] }, noOut )

                _ ->
                    ( { model | action_result = result }, noOut )

        -- Doc change
        OnChangeTensionType type_ ->
            ( setTensionType type_ model, noOut )

        OnChangeTensionSource source ->
            ( setSource source model, noOut )

        OnChangeTensionTarget target ->
            ( setTarget (shrinkNode target) model, noOut )

        OnChangePost field value ->
            ( { model | nodeDoc = NodeDoc.updatePost field value model.nodeDoc }, noOut )

        OnSelectRoleExt role ->
            ( { model | nodeDoc = NodeDoc.updateFromRoleExt role model.nodeDoc }, out0 [ send (OnChangeNodeStep NodeValidateStep) ] )

        OnSelectVisibility visibility ->
            ( { model | nodeDoc = NodeDoc.updatePost "visibility" (NodeVisibility.toString visibility) model.nodeDoc }, out0 [ send (OnChangeNodeStep NodeValidateStep) ] )

        OnAddResponsabilities ->
            ( { model | nodeDoc = NodeDoc.addResponsabilities model.nodeDoc }, noOut )

        OnAddDomains ->
            ( { model | nodeDoc = NodeDoc.addDomains model.nodeDoc }, noOut )

        OnAddPolicies ->
            ( { model | nodeDoc = NodeDoc.addPolicies model.nodeDoc }, noOut )

        OnSubmitTension doClose time ->
            let
                newModel =
                    if model.activeTab == NewTensionTab then
                        let
                            form =
                                model.nodeDoc.form
                        in
                        { model | nodeDoc = NodeDoc.resetNode model.nodeDoc }

                    else
                        model

                events =
                    case model.activeTab of
                        NewTensionTab ->
                            [ Ev TensionEvent.Created "" "" ]

                        NewRoleTab ->
                            if doClose == True then
                                [ Ev TensionEvent.Created "" "", Ev TensionEvent.BlobCreated "" "", Ev TensionEvent.BlobPushed "" "" ]

                            else
                                [ Ev TensionEvent.Created "" "", Ev TensionEvent.BlobCreated "" "" ]

                        NewCircleTab ->
                            if doClose == True then
                                [ Ev TensionEvent.Created "" "", Ev TensionEvent.BlobCreated "" "", Ev TensionEvent.BlobPushed "" "" ]

                            else
                                [ Ev TensionEvent.Created "" "", Ev TensionEvent.BlobCreated "" "" ]
            in
            ( newModel
                |> post "createdAt" (fromTime time)
                |> setEvents events
                |> setStatus (ternary (doClose == True) TensionStatus.Closed TensionStatus.Open)
                |> setActiveButton doClose
                |> setResult LoadingSlowly
            , out0 [ send (PushTension OnTensionAck) ]
            )

        OnTensionAck result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( setResult NotAsked model
                    , out0 [ Ports.raiseAuthModal model.nodeDoc.form.uctx ]
                    )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, out2 [ sendSleep (PushTension OnTensionAck) 500 ] [ DoUpdateToken ] )

                OkAuth tension ->
                    let
                        data =
                            { model | nodeDoc = NodeDoc.setId tension.id model.nodeDoc }
                    in
                    case model.activeTab of
                        NewTensionTab ->
                            ( setResult result data, out1 [ DoPushTension tension ] )

                        NewRoleTab ->
                            let
                                newNameid =
                                    model.nodeDoc.form.node.nameid
                                        |> Maybe.map (\nid -> nodeIdCodec model.nodeDoc.form.target.nameid nid (withDefault NodeType.Role model.nodeDoc.form.node.type_))
                                        |> withDefault ""
                            in
                            ( setResult result data, out1 [ DoPushTension tension, DoFetchNode newNameid ] )

                        NewCircleTab ->
                            let
                                newNameid =
                                    model.nodeDoc.form.node.nameid
                                        |> Maybe.map (\nid -> nodeIdCodec model.nodeDoc.form.target.nameid nid (withDefault NodeType.Circle model.nodeDoc.form.node.type_))
                                        |> withDefault ""
                            in
                            ( setResult result data, out1 [ DoPushTension tension, DoFetchNode newNameid ] )

                DuplicateErr ->
                    ( setResult (Failure [ "Duplicate Error: this name (URL) is already taken." ]) model, noOut )

                _ ->
                    ( setResult result model, noOut )

        -- User Quick Search
        --OnChangeUserPattern pos pattern ->
        --    ( updateUserPattern pos pattern model
        --    , out0 [ Ports.searchUser pattern ]
        --    )
        --OnChangeUserLookup users_ ->
        --    case users_ of
        --        Ok users ->
        --            ( { model | lookup_users = users }, noOut )
        --        Err err ->
        --            ( model, out0 [ Ports.logErr err ] )
        --OnSelectUser pos username ->
        --    ( { model | nodeDoc = NodeDoc.selectUser pos username model.nodeDoc }, noOut )
        --OnCancelUser pos ->
        --    ( cancelUser pos model, noOut )
        --OnShowLookupFs ->
        --    ( openLookup model
        --    , if model.isLookupOpen == False then
        --        out0 [ Ports.outsideClickClose "cancelLookupFsFromJs" "usersSearchPanel" ]
        --      else
        --        noOut
        --    )
        --OnCancelLookupFs ->
        --    ( closeLookup model, noOut )
        LabelSearchPanelMsg msg ->
            let
                ( panel, out ) =
                    LabelSearchPanel.update apis msg model.labelsPanel

                newModel =
                    Maybe.map
                        (\r ->
                            if Tuple.first r == True then
                                addLabel (Tuple.second r) model

                            else
                                removeLabel (Tuple.second r) model
                        )
                        out.result
                        |> withDefault model

                ( cmds, _ ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { newModel | labelsPanel = panel }
            , out2 (out.cmds |> List.map (\m -> Cmd.map LabelSearchPanelMsg m) |> List.append cmds) out.gcmds
            )

        UserInputMsg msg ->
            let
                ( data, out ) =
                    UserInput.update apis msg model.userInput

                users =
                    out.result
                        |> Maybe.map (\( selected, u ) -> ternary selected u [])
                        |> withDefault model.nodeDoc.form.users

                ( cmds, gcmds ) =
                    ( [], [] )
            in
            ( { model | userInput = data, nodeDoc = NodeDoc.setUsers users model.nodeDoc }
            , out2 (List.map (\m -> Cmd.map UserInputMsg m) out.cmds |> List.append cmds) (out.gcmds ++ gcmds)
            )

        -- Confirm Modal
        DoModalConfirmOpen msg mess ->
            ( { model | modal_confirm = ModalConfirm.open msg mess model.modal_confirm }, noOut )

        DoModalConfirmClose _ ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, noOut )

        DoModalConfirmSend ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, out0 [ send model.modal_confirm.msg ] )

        -- Common
        NoMsg ->
            ( model, noOut )

        LogErr err ->
            ( model, out0 [ Ports.logErr err ] )

        UpdateUctx uctx ->
            ( { model | user = LoggedIn uctx, nodeDoc = NodeDoc.setUctx uctx model.nodeDoc }, noOut )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    [ Ports.mcPD Ports.closeModalTensionFromJs LogErr OnClose
    , Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    , Ports.uctxPD Ports.loadUserCtxFromJs LogErr UpdateUctx

    --, Ports.lookupUserFromJs OnChangeUserLookup
    --, Ports.cancelLookupFsFromJs (always OnCancelLookupFs)
    ]
        ++ (LabelSearchPanel.subscriptions model.labelsPanel |> List.map (\s -> Sub.map LabelSearchPanelMsg s))
        ++ (UserInput.subscriptions |> List.map (\s -> Sub.map UserInputMsg s))



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    { path_data : GqlData LocalGraph
    , tree_data : GqlData NodesDict
    }


view : Op -> State -> Html Msg
view op (State model) =
    div []
        (if model.isActive2 then
            [ viewModal op (State model)
            , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
            ]

         else
            []
                ++ [ viewButton op model ]
        )


viewButton : Op -> Model -> Html Msg
viewButton op model =
    div [ class "tensionButton", classList [ ( "is-invisible", not (isSuccess op.path_data) || model.isActive ) ] ]
        [ button ([ class "button is-success" ] ++ (withMaybeData op.path_data |> Maybe.map (\p -> [ onClick (OnOpen (FromPath p)) ]) |> withDefault []))
            [ A.icon "icon-plus icon-2x" ]
        ]


viewModal : Op -> State -> Html Msg
viewModal op (State model) =
    div
        [ id "tensionModal"
        , class "modal is-light modal-fx-slideTop"
        , classList [ ( "is-active", model.isActive ), ( "fixed-top", model.step == TensionFinal && withMaybeData model.result == Nothing ) ]
        , attribute "data-modal-close" "closeModalTensionFromJs"
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "tensionModal"
            , onClick (OnCloseSafe "" "")
            ]
            []
        , div [ class "modal-content" ] [ viewStep op (State model) ]

        --, button [ class "modal-close is-large", onClick (OnCloseSafe "" "") ] []
        ]


viewStep : Op -> State -> Html Msg
viewStep op (State model) =
    case model.step of
        TensionTypes ->
            -- See also Components.SelectType
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
                                        , onClick (OnTensionStep TensionSource)
                                        ]
                                        [ TensionType.toString tensionType |> text ]
                                    ]
                            )
                            TensionType.list
                    ]
                ]

        TensionSource ->
            text "not implemented; remove in commit 7169bad"

        TensionFinal ->
            case model.activeTab of
                NewTensionTab ->
                    viewTension op model

                NewRoleTab ->
                    viewCircle op model

                NewCircleTab ->
                    viewCircle op model

        TensionNotAuthorized errMsg ->
            viewRoleNeeded errMsg

        AuthNeeded ->
            viewAuthNeeded OnClose


viewSuccess : Tension -> Model -> Html Msg
viewSuccess res model =
    let
        link =
            Route.Tension_Dynamic_Dynamic { param1 = nid2rootid model.nodeDoc.form.target.nameid, param2 = res.id } |> toHref
    in
    div [ class "box is-light", autofocus True, tabindex 0, onEnter (OnClose { reset = True, link = "" }) ]
        [ A.icon1 "icon-check icon-2x has-text-success" " "
        , textH model.txt.added
        , text " "
        , a
            [ href link
            , onClickPD (OnClose { reset = True, link = link })
            , target "_blank"
            ]
            [ textH T.checkItOut ]
        , if model.activeTab == NewRoleTab && model.activeButton == Just 0 then
            case model.action_result of
                Success _ ->
                    div []
                        [ A.icon1 "icon-check icon-2x has-text-success" " "
                        , if isSelfContract model.nodeDoc.form.uctx model.nodeDoc.form.users then
                            text "Welcome to your new role."

                          else
                            text "User has been invited."
                        ]

                _ ->
                    if model.doInvite then
                        viewInviteRole model

                    else
                        span [ class "m-2" ] [ text "Or ", a [ class "button is-primary", onClick DoInvite, target "_blank" ] [ text "invite someone" ], text " to this role." ]

          else
            text ""
        ]


viewHeader : Op -> Model -> Html Msg
viewHeader op model =
    div [ class "panel-heading pt-4 pb-3" ]
        [ div [ class "level modal-card-title is-size-6" ]
            [ div [ class "level-left is-hidden-mobile" ]
                [ div [ class "has-text-weight-semibold", style "margin-left" "-8px" ] [ textT model.txt.title ]
                ]
            , div [ class "level-item" ]
                [ viewTensionType model ]
            , div [ class "level-item" ]
                [ viewRecipients op model ]
            ]
        ]


viewTensionTabs : TensionTab -> PNode -> Html Msg
viewTensionTabs tab targ =
    let
        type_ =
            nid2type targ.nameid
    in
    div [ id "tensionTabTop", class "tabs bulma-issue-33 is-boxed" ]
        [ ul []
            [ li [ classList [ ( "is-active", tab == NewTensionTab ) ] ]
                [ a [ class "tootltip has-tooltip-bottom has-tooltip-arrow", attribute "data-tooltip" "Create a new tension.", onClickPD (OnSwitchTab NewTensionTab), target "_blank" ]
                    [ A.icon1 "icon-exchange" "Tension" ]
                ]
            , if type_ == NodeType.Circle then
                li [ classList [ ( "is-active", tab == NewCircleTab ) ] ]
                    [ a [ class "tootltip has-tooltip-bottom has-tooltip-arrow", attribute "data-tooltip" "Create or propose a new circle.", onClickPD (OnSwitchTab NewCircleTab), target "_blank" ]
                        [ A.icon1 "icon-git-branch" "Circle" ]
                    ]

              else
                text ""
            , if type_ == NodeType.Circle then
                li [ classList [ ( "is-active", tab == NewRoleTab ) ] ]
                    [ a [ class "tootltip has-tooltip-bottom has-tooltip-arrow", attribute "data-tooltip" "Create or propose a new role.", onClickPD (OnSwitchTab NewRoleTab), target "_blank" ]
                        [ A.icon1 "icon-leaf" "Role" ]
                    ]

              else
                text ""
            ]
        ]


viewTensionType : Model -> Html Msg
viewTensionType model =
    let
        form =
            model.nodeDoc.form

        tension_type =
            withDefault TensionType.Operational form.type_
    in
    div []
        [ span [ class "has-text-grey-light" ] [ textH ("type" ++ ":" ++ T.space_) ]
        , if model.activeTab == NewTensionTab then
            span [ class "dropdown", style "vertical-align" "unset" ]
                [ span [ class "dropdown-trigger button-light" ]
                    [ span [ attribute "aria-controls" "type-menu" ]
                        [ span [ class <| "has-text-weight-medium " ++ tensionTypeColor "text" tension_type ]
                            [ tensionIcon2 tension_type ]
                        , span [ class "ml-2 arrow down" ] []
                        ]
                    ]
                , div [ id "type-menu", class "dropdown-menu is-right", attribute "role" "menu" ]
                    [ div [ class "dropdown-content" ] <|
                        List.map
                            (\t ->
                                div
                                    [ class <| "dropdown-item button-light " ++ tensionTypeColor "text" t
                                    , onClick (OnChangeTensionType t)
                                    ]
                                    [ tensionIcon2 t ]
                            )
                            TensionType.list
                    ]
                ]

          else
            span [ class <| "has-text-weight-medium " ++ tensionTypeColor "text" tension_type ]
                [ tensionIcon2 tension_type ]
        ]


viewRecipients : Op -> Model -> Html Msg
viewRecipients op model =
    let
        form =
            model.nodeDoc.form
    in
    div []
        [ -- @DEBUG: emitter is ignored now...
          --span [ class "has-text-grey-light" ] [ textH (T.from ++ T.space_) ]
          --, span [ class "dropdown" ]
          --    [ span [ class "dropdown-trigger " ]
          --        [ span [ attribute "aria-controls" "source-menu" ]
          --            [ span
          --                [ class "button is-small is-light is-rounded", attribute "style" "border:1px solid black;" ]
          --                [ text form.source.name, span [ class "ml-2 icon-chevron-down1" ] [] ]
          --            ]
          --        ]
          --    , div [ id "source-menu", class "dropdown-menu", attribute "role" "menu" ]
          --        [ div [ class "dropdown-content" ] <|
          --            List.map
          --                (\t ->
          --                    div
          --                        [ class <| "dropdown-item has-text-weight-semibold button-light has-text-" ++ (roleColor t.role_type |> String.replace "primary" "info")
          --                        , onClick (OnChangeTensionSource t)
          --                        ]
          --                        [ A.icon1 "icon-user" t.name ]
          --                )
          --                (List.filter (\n -> n.nameid /= model.nodeDoc.form.source.nameid) model.sources)
          --        ]
          --    ]
          span [ class "has-text-grey-light", attribute "style" "position:relative;top:7px;" ] [ textH (T.to ++ ":" ++ T.space_) ]
        , span [ class "dropdown " ]
            [ span [ class "dropdown-trigger", onClick OnTargetClick, attribute "style" "max-width: 280px;" ]
                [ span [ class "is-wrapped-50", attribute "aria-controls" "target-menu" ]
                    [ span [ class "button is-small is-rounded has-border", style "display" "inline-block" ]
                        [ text form.target.name, span [ class "ml-2 icon-chevron-down1" ] [] ]
                    ]
                ]
            , div [ id "target-menu", class "dropdown-menu is-center", attribute "role" "menu" ]
                [ div [ class "dropdown-content has-border", style "max-height" "420px" ] <|
                    case op.tree_data of
                        Success data ->
                            List.map
                                (\n -> Lazy.lazy2 viewNodeSelect n OnChangeTensionTarget)
                                (Dict.values data
                                    |> List.filter (\n -> n.nameid /= model.nodeDoc.form.target.nameid)
                                    |> List.sortWith sortNode
                                )

                        _ ->
                            [ div [ class "spinner" ] [] ]
                ]
            ]
        ]



---
--- Final Views
---


viewTension : Op -> Model -> Html Msg
viewTension op model =
    let
        form =
            model.nodeDoc.form

        title =
            Dict.get "title" form.post |> withDefault ""

        message =
            Dict.get "message" form.post |> withDefault ""

        txt =
            getTensionText

        isLoading =
            model.result == LoadingSlowly

        isSendable =
            isPostSendable [ "title" ] form.post

        submitTension =
            ternary isSendable [ onClick (OnSubmit <| OnSubmitTension False) ] []
    in
    case model.result of
        Success res ->
            viewSuccess res model

        other ->
            div [ class "panel modal-card submitFocus" ]
                [ viewHeader op model
                , Lazy.lazy2 viewTensionTabs model.activeTab form.target
                , div [ class "modal-card-body" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ input
                                [ class "input autofocus followFocus"
                                , attribute "data-nextfocus" "textAreaModal"
                                , type_ "text"
                                , placeholder (upH T.title)
                                , spellcheck True
                                , required True
                                , value title
                                , onInput (OnChangePost "title")
                                ]
                                []
                            ]
                        , p [ class "help-label" ] [ textH model.txt.name_help ]
                        , br [] []
                        ]
                    , div [ class "message" ]
                        [ div [ class "message-header" ]
                            [ div [ class "tabs is-boxed is-small pl-1" ]
                                [ ul []
                                    [ li [ classList [ ( "is-active", model.viewMode == Write ) ] ]
                                        [ a [ onClickPD2 (OnChangeInputViewMode Write), target "_blank" ] [ text "Write" ] ]
                                    , li
                                        [ classList [ ( "is-active", model.viewMode == Preview ) ] ]
                                        [ a [ onClickPD2 (OnChangeInputViewMode Preview), target "_blank" ] [ text "Preview" ] ]
                                    ]
                                ]
                            ]
                        , div [ class "message-body" ]
                            [ div [ class "field" ]
                                [ div [ class "control" ]
                                    [ case model.viewMode of
                                        Write ->
                                            let
                                                line_len =
                                                    List.length <| String.lines message
                                            in
                                            textarea
                                                [ id "textAreaModal"
                                                , class "textarea"
                                                , rows (min 10 (max line_len 4))
                                                , placeholder (upH T.leaveCommentOpt)
                                                , value message
                                                , onInput (OnChangePost "message")
                                                ]
                                                []

                                        Preview ->
                                            div [ class "mt-4 mx-3" ] [ renderMarkdown "is-light is-human" message, hr [ class "has-background-grey-lighter" ] [] ]
                                    ]
                                , p [ class "help-label" ] [ textH model.txt.message_help ]
                                , div [ class "is-pulled-right help", style "font-size" "10px" ] [ text "Tips: <C+Enter> to submit" ]
                                , br [] []
                                ]
                            ]
                        ]
                    , div [ class "field" ]
                        [ div [ class "control" ]
                            [ LabelSearchPanel.viewNew
                                { selectedLabels = form.labels
                                , targets = getCircles model.path_data
                                , isRight = False
                                }
                                model.labelsPanel
                                |> Html.map LabelSearchPanelMsg
                            ]
                        ]
                    ]
                , div [ class "modal-card-foot", attribute "style" "display: block;" ]
                    [ case other of
                        Failure err ->
                            viewGqlErrors err

                        _ ->
                            text ""
                    , div [ class "field level is-mobile" ]
                        [ div [ class "level-left" ]
                            [ button
                                [ class "button is-light"
                                , onClick (OnCloseSafe "" "")
                                ]
                                [ textH T.cancel ]
                            ]
                        , div [ class "level-right" ]
                            [ div [ class "buttons" ]
                                [ button
                                    ([ class "button is-success defaultSubmit"
                                     , classList [ ( "is-loading", isLoading ) ]
                                     , disabled (not isSendable)
                                     ]
                                        ++ submitTension
                                    )
                                    [ textH model.txt.submit ]
                                ]
                            ]
                        ]
                    ]
                ]


viewCircle : Op -> Model -> Html Msg
viewCircle op model =
    let
        form =
            model.nodeDoc.form

        node_type =
            withDefault NodeType.Role form.node.type_

        isLoading =
            model.result == LoadingSlowly

        isSendable =
            form.node.name /= Nothing && (form.node.mandate |> Maybe.map (\x -> x.purpose)) /= Nothing

        submitTension =
            ternary isSendable [ onClickPD2 (OnSubmit <| OnSubmitTension False) ] []

        submitCloseTension =
            ternary isSendable [ onClickPD2 (OnSubmit <| OnSubmitTension True) ] []
    in
    case model.result of
        Success res ->
            viewSuccess res model

        other ->
            div [ class "panel modal-card submitFocus" ] <|
                [ viewHeader op model
                , Lazy.lazy2 viewTensionTabs model.activeTab form.target
                ]
                    ++ (case model.nodeStep of
                            RoleAuthorityStep ->
                                [ viewRolesExt model
                                , div [ class "modal-card-foot", attribute "style" "display: block;" ]
                                    [ div [ class "field" ]
                                        [ div [ class "is-pulled-left" ]
                                            [ button [ class "button is-light", onClick (OnCloseSafe "" "") ] [ textH T.cancel ] ]
                                        ]
                                    ]
                                ]

                            CircleVisibilityStep ->
                                [ viewCircleVisibility model
                                , div [ class "modal-card-foot", attribute "style" "display: block;" ]
                                    [ div [ class "field" ]
                                        [ div [ class "is-pulled-left" ]
                                            [ button [ class "button is-light", onClick (OnCloseSafe "" "") ] [ textH T.cancel ] ]
                                        ]
                                    ]
                                ]

                            NodeValidateStep ->
                                [ viewNodeValidate model
                                , div [ class "modal-card-foot", attribute "style" "display: block;" ]
                                    [ case other of
                                        Failure err ->
                                            viewGqlErrors err

                                        _ ->
                                            text ""
                                    , div [ class "field level is-mobile" ]
                                        [ div [ class "level-left" ]
                                            [ button [ class "button is-light", onClick <| OnChangeNodeStep (ternary (model.activeTab == NewRoleTab) RoleAuthorityStep CircleVisibilityStep) ]
                                                [ A.icon0 "icon-chevron-left", textH T.back ]
                                            ]
                                        , div [ class "level-right" ]
                                            [ div [ class "buttons" ]
                                                [ button
                                                    ([ class "button is-success defaultSubmit"
                                                     , classList
                                                        [ ( "is-loading", isLoading && model.activeButton == Just 0 ) ]
                                                     , disabled (not isSendable || isLoading)
                                                     ]
                                                        ++ submitCloseTension
                                                    )
                                                    [ textH model.txt.close_submit ]
                                                , button
                                                    ([ class "button is-warning"
                                                     , classList
                                                        [ ( "is-loading", isLoading && model.activeButton == Just 1 ) ]
                                                     , disabled (not isSendable || isLoading)
                                                     ]
                                                        ++ submitTension
                                                    )
                                                    [ textH model.txt.submit ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]

                            InviteStep ->
                                -- This View is implemented in the success view !
                                []
                       )


viewNodeBreadcrumb : TensionForm -> NodeStep -> Html Msg
viewNodeBreadcrumb form step =
    let
        node_type =
            withDefault NodeType.Role form.node.type_

        path =
            case node_type of
                NodeType.Role ->
                    [ RoleAuthorityStep, NodeValidateStep, InviteStep ]

                NodeType.Circle ->
                    [ CircleVisibilityStep, NodeValidateStep ]
    in
    nav [ class "breadcrumb has-succeeds-separator is-small", attribute "aria-labels" "breadcrumbs" ]
        [ ul [] <|
            List.map
                (\x ->
                    li [ classList [ ( "is-active", x == step ) ] ] [ a [ onClickPD NoMsg, target "_blank" ] [ text (nodeStepToString form x) ] ]
                )
                path
        ]


viewNodeValidate : Model -> Html Msg
viewNodeValidate model =
    let
        form =
            model.nodeDoc.form

        op =
            { data = model.nodeDoc
            , result = model.result
            , onChangePost = OnChangePost
            , onAddDomains = OnAddDomains
            , onAddPolicies = OnAddPolicies
            , onAddResponsabilities = OnAddResponsabilities
            }

        n =
            Debug.log "form.node" form.node.type_

        n1 =
            Debug.log "op.node" op.data.node.type_
    in
    div [ class "modal-card-body" ]
        [ viewNodeBreadcrumb form model.nodeStep
        , viewAboutInput2 False OverviewBaseUri model.txt form.node op
        , viewMandateInput model.txt form.node.mandate op

        --, br [] []
        --, br [] []
        --, div [ class "field" ]
        --    [ div [ class "control" ]
        --        [ textarea
        --            [ class "textarea"
        --            , rows 3
        --            , placeholder (upH T.leaveCommentOpt)
        --            , value (Dict.get "message" form.post |> withDefault "")
        --            , onInput <| OnChangePost "message"
        --            ]
        --            []
        --        ]
        --    , p [ class "help-label" ] [ textH model.txt.message_help ]
        --    ]
        , br [] []
        ]


viewRolesExt : Model -> Html Msg
viewRolesExt model =
    let
        form =
            model.nodeDoc.form
    in
    div [ class "modal-card-body" ]
        [ viewNodeBreadcrumb form model.nodeStep

        -- Show the help information
        --showMsg "roleAuthority-0" "is-info is-light" "icon-info" T.roleAuthorityHeader ""
        , div [ class "subtitle" ] [ text "Select a role template" ]
        , case model.roles_result of
            Success roles ->
                List.map
                    (\role ->
                        div
                            [ class "card has-border column is-paddingless m-3 is-h"
                            , classList [ ( "is-selected", Just role.id == form.node.role_ext ) ]
                            , attribute "style" "min-width: 150px;"
                            ]
                            [ div [ class "card-content p-4", onClick (OnSelectRoleExt role) ]
                                [ h2 [ class "level mb-3 is-size-5" ]
                                    [ div [ class "level-left" ] [ viewRoleExt "" role ] ]
                                , div [ class "content is-small" ] [ text (withDefault "" role.about) ]
                                ]
                            ]
                    )
                    roles
                    |> (\l ->
                            l
                                ++ [ div [ class "card-content", attribute "style" (ternary (List.length l == 0) "margin-top: -1rem;" "") ]
                                        [ if List.length l == 0 then
                                            span [ class "content is-small" ] [ text "No template role yet.", br [ class "mb-4" ] [], text "You can create a " ]

                                          else
                                            text ""
                                        , span
                                            [ class "button is-small has-text-link"
                                            , title "A template role is a generic role that you can reuse in your organisation."
                                            , onClick (OnCloseSafe (uriFromNameid SettingsBaseUri form.target.nameid [] ++ "?m=roles&a=new") "")
                                            ]
                                            [ textH "template role" ]
                                        , span [ class "content is-small px-2" ] [ text "or make an" ]
                                        , span
                                            [ class "button is-small has-text-link"
                                            , title "An ad-hoc role is a role that you create from scratch."
                                            , onClick (OnChangeNodeStep NodeValidateStep)
                                            ]
                                            [ textH "Ad-hoc role" ]
                                        ]
                                   ]
                       )
                    |> div [ class "columns is-multiline" ]

            Failure err ->
                viewGqlErrors err

            _ ->
                div [ class "spinner" ] []
        ]


viewCircleVisibility : Model -> Html Msg
viewCircleVisibility model =
    let
        form =
            model.nodeDoc.form
    in
    div [ class "modal-card-body" ]
        [ viewNodeBreadcrumb form model.nodeStep
        , div [ class "subtitle" ] [ text "Choose the circle visibility" ]

        -- Show the choices as card.
        , NodeVisibility.list
            |> List.map
                (\x ->
                    let
                        isSelected =
                            Just x == form.node.visibility

                        ( icon, description ) =
                            case x of
                                NodeVisibility.Public ->
                                    ( "icon-globe", T.visibilityPublic )

                                NodeVisibility.Private ->
                                    ( "icon-users", T.visibilityPrivate )

                                NodeVisibility.Secret ->
                                    ( "icon-lock", T.visibilitySeccret )
                    in
                    div
                        [ class "card has-border column is-paddingless m-3 is-h"
                        , classList [ ( "is-selected is-selectable", isSelected ) ]

                        -- @debug: onCLick here do not work sometimes (for the 2nd element of the list ???
                        ]
                        [ div [ class "card-content p-4", onClick (OnSelectVisibility x) ]
                            [ h2 [ class "is-strong is-size-5" ] [ A.icon1 (icon ++ " icon-bg") (NodeVisibility.toString x) ]
                            , div [ class "content is-small" ] [ text description ]
                            ]
                        ]
                )
            |> div [ class "columns" ]
        ]


viewInviteRole : Model -> Html Msg
viewInviteRole model =
    let
        form =
            model.nodeDoc.form

        isLoading =
            model.action_result == LoadingSlowly
    in
    div [ class "columns is-centered mt-2" ]
        [ div [ class "column is-8" ]
            [ UserInput.view { label_text = text "Invite someone to this role (or link yourself):" } model.userInput |> Html.map UserInputMsg
            , viewComment model
            , case model.action_result of
                Failure err ->
                    div [ class "field" ] [ viewGqlErrors err ]

                _ ->
                    text ""
            , div [ class "field" ]
                [ div [ class "is-pulled-right" ]
                    [ button
                        [ class "button is-light is-link"
                        , classList [ ( "is-loading", isLoading ) ]
                        , disabled (not (isUsersSendable form.users) || isLoading)
                        , onClick (OnSubmit OnInvite)
                        ]
                        [ ternary (isSelfContract form.uctx form.users) T.link T.invite |> textH ]
                    ]
                ]
            ]
        ]


viewComment : Model -> Html Msg
viewComment model =
    let
        message =
            Dict.get "message" model.nodeDoc.form.post |> withDefault ""
    in
    div [ class "field" ]
        [ div [ class "control submitFocus" ]
            [ textarea
                [ class "textarea"
                , rows 3
                , placeholder (upH T.leaveCommentOpt)
                , value message
                , onInput <| OnChangePost "message"
                ]
                []
            ]
        , p [ class "help-label" ] [ textH T.invitationMessageHelp ]
        ]
