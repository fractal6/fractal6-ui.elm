{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2026 Fractale Co

   This file is part of Fractale.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
-}


module Form.NewTension exposing (..)

import Assets as A
import Auth exposing (ErrState(..), hasLazyAdminRole, parseErr)
import Browser.Events as Events
import Bulk exposing (Ev, FormText, InputViewMode(..), TensionForm, UserState(..), getPath, getPathWithChildren, initFormText, isSelfContract, localGraphFromOrga, makeCandidateContractForm, tensionToActionForm)
import Bulk.Bulma as B
import Bulk.Codecs exposing (DocType(..), FractalBaseRoute(..), getOrgaRoles, nearestCircleid, nid2rootid, nid2type, nodeIdCodec, toLink, ur2eor)
import Bulk.Error exposing (viewAuthNeeded, viewGqlErrors, viewJoinForTensionNeeded)
import Bulk.View exposing (tensionIcon2, tensionType2descr, tensionType2notif, tensionTypeColor, viewRoleExt, visibility2descr)
import Codecs exposing (DraftUpdate(..), TensionDraft)
import Components.Comments as Comments exposing (OutType(..))
import Components.LabelSearchPanel as LabelSearchPanel
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.NodeDoc as NodeDoc exposing (NodeDoc, NodeView(..), viewAboutInput2, viewMandateInput)
import Components.ProjectSearchPanel as ProjectSearchPanel exposing (ProjectActionResult(..))
import Components.TreeMenu exposing (viewSelectorTree)
import Components.UserInput as UserInput
import Components.UserSearchPanel as UserSearchPanel
import Dict
import Dom
import Extra exposing (showIf, space_, ternary, textH, unwrap, unwrap2)
import Extra.Events exposing (onClickPD, onClickSafe, onEnter)
import Form exposing (isPostEmpty, isPostSendable, isUsersSendable)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.NodeVisibility as NodeVisibility
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (toHref)
import Global exposing (Msg(..), send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, h2, hr, i, input, label, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, autofocus, class, classList, disabled, href, id, placeholder, required, rows, spellcheck, style, tabindex, target, title, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import Json.Decode as JD
import Loading exposing (GqlData, ModalData, RequestResult(..), RestData, isSuccess, withDefaultData, withMapData, withMaybeData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Ports
import Query.AddContract exposing (addOneContract)
import Query.AddTension exposing (addOneTension)
import Query.PatchTension exposing (actionRequest)
import Query.QueryNode exposing (getTensionTemplateById, getTensionTemplates, queryLocalGraph, queryRolesFull)
import Query.QueryProject exposing (addProjectCard)
import RemoteData
import Requests exposing (fetchTensionTemplatesTop)
import Schemas.TreeMenu exposing (ExpandedLines)
import Session exposing (Apis, CommonMsg, GlobalCmd(..), LabelSearchPanelOnClickAction(..), ProjectSearchPanelOnClickAction(..), SessionCommon, UserSearchPanelOnClickAction(..))
import Text as T
import Time



-- ------------------------------
-- M O D E L
-- ------------------------------


type State
    = State Model


type alias Model =
    { nodeDoc : NodeDoc -- form
    , result : GqlData Tension
    , sources : List EmitterOrReceiver
    , step : TensionStep
    , isActive : Bool
    , isActive2 : Bool -- Let minimze VDOM load + prevent glitch while keeping css effects
    , path_data : GqlData LocalGraph
    , action_result : GqlData IdPayload
    , draft : Maybe ProjectDraft
    , currentDraft : Maybe TensionDraft
    , isTargetOpen : String
    , isTypeOpen : String
    , expanded_lines : ExpandedLines
    , freezeOutsideClick : Bool

    -- switching tab
    , activeTab : TensionTab
    , force_init : Bool

    -- Role/Circle
    , activeButton : Maybe Int -- 0: creating role, 1: creating tension (no pushing blob)
    , nodeStep : NodeStep
    , roles_result : GqlData (List RoleExtFull)
    , doInvite : Bool
    , withUsers : List String
    , simplifiedView : Bool

    -- Templates
    , templates : RestData (List TensionTemplateLite)
    , selectedTemplate : Maybe TensionTemplateFull
    , templateLoading : Bool
    , templatesLoadingSlow : Bool
    , isTemplateTensionOnly : Bool
    , showTemplatePicker : Bool

    -- Draft persistence (debounce timer for saving)
    , draftSaveTimer : Int

    -- Common
    , session : SessionCommon
    , refresh_trial : Int
    , modal_confirm : ModalConfirm Msg
    , modal_confirm3_link : String
    , commonOp : CommonMsg Msg

    -- Components
    , labelsPanel : LabelSearchPanel.State
    , assigneesPanel : UserSearchPanel.State
    , projectsPanel : ProjectSearchPanel.State
    , selectedProjects : List TensionProject
    , inviteInput : UserInput.State
    , comments : Comments.State
    }


type TensionTab
    = NewTensionTab
    | NewRoleTab
    | NewCircleTab


type TensionStep
    = TensionSource
    | TensionFinal
    | TensionNotAuthorized
    | AuthNeeded


type NodeStep
    = RoleAuthorityStep
    | CircleVisibilityStep
    | NodeValidateStep
    | InviteStep


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


init : SessionCommon -> State
init session =
    initModel session |> State


initModel : SessionCommon -> Model
initModel session =
    { result = NotAsked
    , sources = []
    , step = TensionFinal
    , isActive = False
    , isActive2 = False
    , activeTab = NewTensionTab
    , activeButton = Nothing
    , nodeDoc = NodeDoc.init session.lexicon "" Nothing NodeEdit session.user
    , path_data = NotAsked -- may be different than the current path_data (op.path_data)
    , action_result = NotAsked
    , doInvite = False
    , withUsers = []
    , simplifiedView = False
    , draft = Nothing
    , currentDraft = Nothing
    , isTargetOpen = ""
    , isTypeOpen = ""
    , expanded_lines = Dict.empty
    , freezeOutsideClick = False

    -- Role/Circle
    , nodeStep = RoleAuthorityStep -- will change
    , roles_result = Loading
    , force_init = False

    -- Templates
    , templates = RemoteData.NotAsked
    , selectedTemplate = Nothing
    , templateLoading = False
    , templatesLoadingSlow = False
    , isTemplateTensionOnly = False
    , showTemplatePicker = False

    -- Draft persistence
    , draftSaveTimer = 0

    -- Common
    , session = session
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    , modal_confirm3_link = ""
    , commonOp = CommonMsg NoMsg LogErr

    -- Components
    , labelsPanel = LabelSearchPanel.init "" SelectLabel session.user
    , assigneesPanel = UserSearchPanel.init "" SelectUser session.user
    , projectsPanel = ProjectSearchPanel.init "" SelectProject session.user
    , selectedProjects = []
    , inviteInput = UserInput.init [] True False session
    , comments = Comments.init "" "" session
    }



-- Global methods
--  nothing here
-- State Controls


setCurrentDraft : Maybe TensionDraft -> State -> State
setCurrentDraft draft (State model) =
    State { model | currentDraft = draft }


setSessionTemplates : RestData (List TensionTemplateLite) -> State -> State
setSessionTemplates tpls (State model) =
    State { model | templates = tpls }


setPath : LocalGraph -> Model -> Model
setPath p model =
    let
        sources =
            getOrgaRoles [ nid2rootid p.focus.nameid ] model.nodeDoc.form.uctx.roles
                |> List.filter (\r -> r.role_type /= RoleType.Owner)
                |> List.map ur2eor

        extras =
            getOrgaRoles [ nid2rootid p.focus.nameid ] model.nodeDoc.form.uctx.roles
                |> List.filter (\r -> r.role_type == RoleType.Owner)
                |> List.map ur2eor

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
        let
            form =
                model.nodeDoc.form

            node =
                form.node

            newForm =
                case tab of
                    NewTensionTab ->
                        { form
                            | type_ = Just TensionType.Operational
                            , action = Nothing
                            , blob_type = Nothing
                            , users = []
                            , txt = initFormText model.session.lexicon Nothing
                        }

                    NewRoleTab ->
                        { form
                            | type_ = Just TensionType.Governance
                            , blob_type = Just BlobType.OnNode
                            , node = { node | type_ = Just NodeType.Role }
                            , action = Just TensionAction.NewRole
                            , users = []
                            , txt = initFormText model.session.lexicon (Just NodeType.Role)
                        }
                            |> NodeDoc.updateNodeForm "name" (Dict.get "title" form.post |> withDefault "")

                    NewCircleTab ->
                        { form
                            | type_ = Just TensionType.Governance
                            , blob_type = Just BlobType.OnNode
                            , node = { node | type_ = Just NodeType.Circle }
                            , action = Just TensionAction.NewCircle
                            , users = []
                            , txt = initFormText model.session.lexicon (Just NodeType.Circle)
                        }
                            |> NodeDoc.updateNodeForm "name" (Dict.get "title" form.post |> withDefault "")

            step =
                case tab of
                    NewTensionTab ->
                        -- ignored
                        RoleAuthorityStep

                    NewRoleTab ->
                        RoleAuthorityStep

                    NewCircleTab ->
                        CircleVisibilityStep
        in
        { model
            | activeTab = tab
            , nodeStep = step
            , nodeDoc = NodeDoc.setForm newForm model.nodeDoc
            , result = NotAsked
            , force_init = False
        }


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


setActiveButton : Bool -> Model -> Model
setActiveButton doClose data =
    if doClose then
        { data | activeButton = Just 0 }

    else
        { data | activeButton = Just 1 }


setStep : TensionStep -> Model -> Model
setStep step data =
    { data | step = step }


setResult : GqlData Tension -> Model -> Model
setResult result data =
    { data | result = result }



-- Update Form


setUctx : UserCtx -> Model -> Model
setUctx uctx data =
    let
        session =
            data.session
    in
    { data | session = { session | user = LoggedIn uctx }, nodeDoc = NodeDoc.setUctx uctx data.nodeDoc }


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


setAssignees : List User -> Model -> Model
setAssignees assignees data =
    { data | nodeDoc = NodeDoc.setAssignees assignees data.nodeDoc }


addAssignee : User -> Model -> Model
addAssignee assignee data =
    { data | nodeDoc = NodeDoc.addAssignee assignee data.nodeDoc }


removeAssignee : User -> Model -> Model
removeAssignee assignee data =
    { data | nodeDoc = NodeDoc.removeAssignee assignee data.nodeDoc }


addSelectedProject : TensionProject -> Model -> Model
addSelectedProject tp data =
    { data | selectedProjects = data.selectedProjects ++ [ tp ] }


removeSelectedProject : String -> Model -> Model
removeSelectedProject projectId data =
    { data | selectedProjects = List.filter (\tp -> tp.project.id /= projectId) data.selectedProjects }


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
    initModel data.session



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
    not (isPostEmpty [ "title", "message", "invitation" ] data.nodeDoc.form.post)
        && not (isTemplateUnmodified data)


{-| Check if the current form content matches the selected template exactly (no user edits).
-}
isTemplateUnmodified : Model -> Bool
isTemplateUnmodified data =
    case data.selectedTemplate of
        Just tpl ->
            let
                currentTitle =
                    Dict.get "title" data.nodeDoc.form.post |> withDefault "" |> String.trim

                currentMessage =
                    Dict.get "message" data.nodeDoc.form.post |> withDefault "" |> String.trim
            in
            currentTitle == String.trim tpl.title && currentMessage == String.trim tpl.comment

        Nothing ->
            False



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = -- Data control
      PushTension (GqlData Tension -> Msg)
    | OnSubmit Bool (Time.Posix -> Msg)
    | GotPath Bool (GqlData LocalGraph) -- GraphQL
    | OnOutsideTreeClickClose
    | UnfreezeOutsideClick
      -- Modal control
    | SetIsActive2 Bool
    | OnOpen NewTensionInput (Maybe ProjectDraft)
    | OnOpenCircle NewTensionInput
    | OnOpenRole NewTensionInput
    | OnOpenRoleUser NewTensionInput String
    | OnReset
    | OnClose ModalData
    | OnCloseSafe String String
    | OnSwitchTab TensionTab
    | OnGotRoles (GqlData (List RoleExtFull))
    | OnChangeNodeStep NodeStep
    | OnTensionStep TensionStep
    | OnTargetClick String
    | OnTypeClick String
    | DoInvite
    | OnInvite Time.Posix
    | PushAck (GqlData IdPayload)
    | OnToggleDropdownRoles String
      -- Doc change
    | OnChangeTensionType TensionType.TensionType
    | OnChangeTensionSource EmitterOrReceiver
    | OnChangeTensionTarget (GqlData NodesDict) Node
    | OnChangePost String String
    | OnSelectRoleExt RoleExtFull
    | OnSelectVisibility NodeVisibility.NodeVisibility
    | OnAddDomains
    | OnAddPolicies
    | OnAddResponsabilities
    | OnSubmitTension Bool Time.Posix
    | OnTensionAck (GqlData Tension)
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- 3-choice Confirm Modal
    | DoModalConfirm3Open String
    | DoModalConfirm3Discard
    | DoModalConfirm3SaveDraft
    | DoModalConfirm3KeepEditing
      -- Common
    | NoMsg
    | LogErr String
    | UpdateUctx UserCtx
      -- Templates
    | GotTemplatesForPicker (RestData (List TensionTemplateLite))
    | TemplatesLoadingSlow
    | OnSelectTemplate TensionTemplateLite
    | GotTemplateContent (GqlData TensionTemplateFull)
    | OnSelectBlankTension
      -- Draft persistence
    | SaveDraftDelayed Int
      -- Components
    | LabelSearchPanelMsg LabelSearchPanel.Msg
    | UserSearchPanelMsg UserSearchPanel.Msg
    | ProjectSearchPanelMsg ProjectSearchPanel.Msg
    | OnAddProjectCardAck (GqlData (List ProjectCard))
    | InviteInputMsg UserInput.Msg
    | CommentsMsg Comments.Msg


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe ( Tension, Maybe ProjectDraft )
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

        OnSubmit isSendable next ->
            if isSendable then
                ( model, out0 [ sendNow next ] )

            else
                ( model, noOut )

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
                            ( model, out0 [ send (OnOpen (FromPath newPath) Nothing) ] )

                        Nothing ->
                            let
                                newPath =
                                    { prevPath | path = path.path ++ (List.tail prevPath.path |> withDefault []) }

                                nameid =
                                    List.head path.path |> Maybe.map .nameid |> withDefault ""
                            in
                            ( { model | path_data = Success newPath }, out0 [ queryLocalGraph apis nameid False (GotPath False) ] )

                Failure err ->
                    ( { model | path_data = result }, out0 [ Ports.logErr (String.join " | " err) ] )

                _ ->
                    ( { model | path_data = result }, noOut )

        OnOutsideTreeClickClose ->
            if model.freezeOutsideClick then
                ( model, noOut )

            else
                ( { model | freezeOutsideClick = True }, out0 [ Ports.outsideClickClose "closeTreeSelFromJs" "tree-selector", sendSleep UnfreezeOutsideClick 250 ] )

        UnfreezeOutsideClick ->
            ( { model | freezeOutsideClick = False }, noOut )

        -- Modal control
        SetIsActive2 v ->
            if v then
                ( { model | isActive = model.isActive2 }, out0 [ Ports.open_modal "tensionModal" ] )

            else
                ( { model | isActive2 = model.isActive }, noOut )

        OnOpen t d ->
            case model.session.user of
                LoggedIn uctx ->
                    let
                        -- Load draft from session or from ProjectDraft
                        newModel =
                            case d of
                                Just draft ->
                                    let
                                        nodeDocWithDraft =
                                            model.nodeDoc
                                                |> NodeDoc.updatePost "title" draft.title
                                                |> NodeDoc.updatePost "message" (withDefault "" draft.message)

                                        formWithDraft =
                                            nodeDocWithDraft.form

                                        nodeDocWithRefs =
                                            NodeDoc.setForm
                                                { formWithDraft
                                                    | labels = withDefault [] draft.labels
                                                    , assignees = withDefault [] draft.assignees
                                                }
                                                nodeDocWithDraft
                                    in
                                    { model
                                        | draft = d
                                        , nodeDoc = nodeDocWithRefs
                                    }

                                Nothing ->
                                    -- Check for saved tension draft
                                    case model.currentDraft of
                                        Just tensionDraft ->
                                            { model
                                                | nodeDoc =
                                                    model.nodeDoc
                                                        |> NodeDoc.updatePost "title" tensionDraft.title
                                                        |> NodeDoc.updatePost "message" tensionDraft.message
                                            }

                                        Nothing ->
                                            model

                        restoreDraftCmd =
                            case Dict.get "message" newModel.nodeDoc.form.post of
                                Just m ->
                                    send (Comments.OnChangeComment "message" m) |> Cmd.map CommentsMsg

                                Nothing ->
                                    Cmd.none
                    in
                    case t of
                        FromNameid nameid ->
                            ( newModel, out0 [ queryLocalGraph apis nameid True (GotPath True), restoreDraftCmd ] )

                        FromPath p ->
                            let
                                data =
                                    setPath p newModel
                            in
                            if data.sources == [] && data.refresh_trial == 0 then
                                ( { data | refresh_trial = 1 }
                                , Out [ sendSleep (OnOpen (FromPath p) d) 500 ] [ DoUpdateToken ] Nothing
                                )

                            else if data.sources == [] then
                                ( { data | isActive2 = True } |> setStep TensionNotAuthorized
                                , out0 [ sendSleep (SetIsActive2 True) 10 ]
                                )

                            else
                                let
                                    switchCmd =
                                        case model.activeTab of
                                            NewTensionTab ->
                                                Cmd.none

                                            NewRoleTab ->
                                                send (OnSwitchTab NewRoleTab)

                                            NewCircleTab ->
                                                send (OnSwitchTab NewCircleTab)

                                    -- Use pre-loaded templates from session when available, fallback to fetch
                                    hasDraft =
                                        newModel.draft /= Nothing || newModel.currentDraft /= Nothing

                                    -- Pre-loaded templates are only valid when the target matches the page focus
                                    targetMatchesFocus =
                                        model.session.node_focus
                                            |> Maybe.map (\nf -> nf.nameid == p.focus.nameid)
                                            |> withDefault False

                                    ( templateCmd, showPicker ) =
                                        if model.activeTab /= NewTensionTab || hasDraft then
                                            ( Cmd.none, False )

                                        else
                                            case ( model.templates, targetMatchesFocus ) of
                                                ( RemoteData.Success templates, True ) ->
                                                    -- Pre-loaded from session for same node: decide instantly
                                                    ( Cmd.none, not (List.isEmpty templates) )

                                                _ ->
                                                    -- Target differs from page focus or not loaded: fetch for actual target
                                                    ( Cmd.batch
                                                        [ fetchTensionTemplatesTop apis p.focus.nameid True GotTemplatesForPicker
                                                        , sendSleep TemplatesLoadingSlow 500
                                                        ]
                                                    , False
                                                    )

                                    isTemplateTensionOnly_ =
                                        p.root |> Maybe.andThen .isTemplateTensionOnly |> withDefault False
                                in
                                ( { data | isActive2 = True, isTemplateTensionOnly = isTemplateTensionOnly_, showTemplatePicker = showPicker } |> setUctx uctx
                                , out0
                                    [ sendSleep (SetIsActive2 True) 10
                                    , Cmd.map CommentsMsg (send <| Comments.OnSetTarget (List.map .nameid p.path))
                                    , switchCmd
                                    , restoreDraftCmd
                                    , templateCmd
                                    ]
                                )

                LoggedOut ->
                    ( { model | isActive2 = True } |> setStep AuthNeeded, out0 [ send (SetIsActive2 True) ] )

        OnOpenCircle t ->
            ( { model | activeTab = NewCircleTab, force_init = True }, out0 [ send (OnOpen t Nothing) ] )

        OnOpenRole t ->
            let
                cmd =
                    if isSuccess model.result then
                        send OnReset

                    else
                        Cmd.none
            in
            ( { model | activeTab = NewRoleTab, force_init = True }, out0 [ send (OnOpen t Nothing), cmd ] )

        OnOpenRoleUser t u ->
            ( { model
                | activeTab = NewRoleTab
                , force_init = True
                , withUsers = [ u ]
                , doInvite = True
                , activeButton = Just 0
                , simplifiedView = True
              }
            , out0
                [ send (OnOpen t Nothing)
                , UserInput.OnClickUser { username = u, name = Nothing } |> send |> Cmd.map InviteInputMsg
                , if isSuccess model.result then
                    send OnReset

                  else
                    Cmd.none
                ]
            )

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

        OnCloseSafe link _ ->
            if canExitSafe model then
                ( model, out0 [ send (OnClose { reset = True, link = link }) ] )

            else
                ( model
                , out0 [ send (DoModalConfirm3Open link) ]
                )

        OnSwitchTab tab ->
            let
                hasDraft =
                    model.draft /= Nothing || model.currentDraft /= Nothing

                targetMatchesFocus =
                    let
                        targetNameid =
                            withMaybeData model.path_data |> Maybe.map .focus |> Maybe.map .nameid
                    in
                    Maybe.map2 (\t nf -> t == nf.nameid) targetNameid model.session.node_focus
                        |> withDefault False

                ( cmds, showPicker ) =
                    case tab of
                        NewTensionTab ->
                            if hasDraft then
                                ( [], False )

                            else
                                case ( model.templates, targetMatchesFocus ) of
                                    ( RemoteData.Success templates, True ) ->
                                        ( [], not (List.isEmpty templates) )

                                    _ ->
                                        let
                                            focusNameid =
                                                withMaybeData model.path_data |> Maybe.map .focus |> Maybe.map .nameid |> withDefault ""
                                        in
                                        ( [ fetchTensionTemplatesTop apis focusNameid True GotTemplatesForPicker
                                          , sendSleep TemplatesLoadingSlow 500
                                          ]
                                        , False
                                        )

                        NewRoleTab ->
                            if withMaybeData model.roles_result == Nothing then
                                let
                                    nameids =
                                        getPath model.path_data |> List.map .nameid
                                in
                                ( [ queryRolesFull apis nameids OnGotRoles ], False )

                            else
                                ( [], False )

                        _ ->
                            ( [], False )
            in
            ( switchTab tab { model | showTemplatePicker = showPicker }, out0 (Ports.bulma_driver "tensionModal" :: cmds) )

        OnGotRoles result ->
            ( { model | roles_result = result }, noOut )

        OnChangeNodeStep step ->
            ( changeNodeStep step model, out0 [ Ports.bulma_driver "tensionModal" ] )

        OnTensionStep step ->
            ( setStep step model, out0 [ Ports.bulma_driver "tensionModal" ] )

        OnTargetClick id_ ->
            case model.isTargetOpen of
                "" ->
                    if id_ /= "" then
                        ( { model | isTargetOpen = id_ }
                        , out0 [ Ports.requireTreeData, sendSleep OnOutsideTreeClickClose 250 ]
                        )

                    else
                        ( model, noOut )

                _ ->
                    ( { model | isTargetOpen = "" }, out0 [ Ports.click "" ] )

        OnTypeClick id_ ->
            ( { model | isTypeOpen = ternary (model.isTypeOpen == "") id_ "" }, noOut )

        DoInvite ->
            ( { model | doInvite = True }, out0 [ Ports.focusOn "userInput" ] )

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
                    ( { model | refresh_trial = i }, out2 [ sendSleep (OnSubmit True OnInvite) 500 ] [ DoUpdateToken ] )

                OkAuth c ->
                    let
                        form =
                            model.nodeDoc.form

                        tensionid =
                            withMapData .id model.result |> withDefaultData ""

                        isSelfContract_ =
                            isSelfContract model.nodeDoc.form.uctx model.nodeDoc.form.users

                        link =
                            if isSelfContract_ then
                                Route.Tension_Dynamic_Dynamic_Action { param1 = nid2rootid model.nodeDoc.form.target.nameid, param2 = tensionid } |> toHref

                            else
                                Route.Tension_Dynamic_Dynamic_Contract_Dynamic { param1 = nid2rootid model.nodeDoc.form.target.nameid, param2 = tensionid, param3 = c.id } |> toHref
                    in
                    ( { model | action_result = result }
                    , out2
                        [ send (OnClose { reset = True, link = "" }) ]
                        ([ DoPushSystemNotif
                            { cls = "is-success"
                            , content =
                                div [ class "is-flex is-align-items-center mr-5" ]
                                    [ A.icon1 "icon-check icon-2x has-text-success" ""
                                    , text model.nodeDoc.form.txt.added
                                    , text space_
                                    , a [ href link ]
                                        [ ternary (model.activeTab == NewTensionTab)
                                            (text T.checkItOut_fem)
                                            (text T.checkItOut_masc)
                                        ]
                                    ]
                            }
                         ]
                            ++ ternary (isSelfContract form.uctx form.users) [ DoUpdateToken ] []
                        )
                    )

                DuplicateErr ->
                    ( { model | action_result = Failure [ T.duplicateContractError ] }, noOut )

                _ ->
                    ( { model | action_result = result }, noOut )

        OnToggleDropdownRoles nid ->
            let
                newModel =
                    if Dict.member nid model.expanded_lines then
                        { model | expanded_lines = Dict.remove nid model.expanded_lines }

                    else
                        { model | expanded_lines = Dict.insert nid False model.expanded_lines }
            in
            ( newModel, noOut )

        -- Doc change
        OnChangeTensionType type_ ->
            ( setTensionType type_ model, noOut )

        OnChangeTensionSource source ->
            ( setSource source model, noOut )

        OnChangeTensionTarget odata target ->
            let
                resetTemplates m =
                    { m | templates = RemoteData.NotAsked, selectedTemplate = Nothing, templatesLoadingSlow = False, showTemplatePicker = False }

                -- If the form is empty, fetch templates for the new target and show picker
                templateCmd =
                    if model.activeTab == NewTensionTab && not (hasData model) then
                        fetchTensionTemplatesTop apis target.nameid True GotTemplatesForPicker

                    else
                        Cmd.none
            in
            case localGraphFromOrga target.nameid odata of
                Just path ->
                    ( setPath path model |> resetTemplates, out0 [ send (OnTargetClick ""), templateCmd ] )

                Nothing ->
                    ( setTarget (shrinkNode target) model |> resetTemplates, out0 [ send (OnTargetClick ""), templateCmd ] )

        OnChangePost field value ->
            let
                newModel =
                    { model | nodeDoc = NodeDoc.updatePost field value model.nodeDoc }

                -- Schedule debounced draft save when title changes (message changes go through Comments)
                ( finalModel, saveCmd ) =
                    if field == "title" && model.activeTab == NewTensionTab then
                        let
                            newTimer =
                                model.draftSaveTimer + 1
                        in
                        ( { newModel | draftSaveTimer = newTimer }
                        , sendSleep (SaveDraftDelayed newTimer) 3500
                        )

                    else
                        ( newModel, Cmd.none )
            in
            ( finalModel, out0 [ saveCmd ] )

        OnSelectRoleExt role ->
            ( { model | nodeDoc = NodeDoc.updateFromRoleExt role model.nodeDoc }, out0 [ send (OnChangeNodeStep NodeValidateStep) ] )

        OnSelectVisibility visibility ->
            ( { model | nodeDoc = NodeDoc.updatePost "visibility" (NodeVisibility.toString visibility) model.nodeDoc }, out0 [ send (OnChangeNodeStep NodeValidateStep) ] )

        OnAddResponsabilities ->
            ( { model | nodeDoc = NodeDoc.addResponsabilities model.nodeDoc }, out0 [ Ports.bulma_driver "tensionModal" ] )

        OnAddDomains ->
            ( { model | nodeDoc = NodeDoc.addDomains model.nodeDoc }, out0 [ Ports.bulma_driver "tensionModal" ] )

        OnAddPolicies ->
            ( { model | nodeDoc = NodeDoc.addPolicies model.nodeDoc }, out0 [ Ports.bulma_driver "tensionModal" ] )

        OnSubmitTension doClose time ->
            let
                newModel =
                    if model.activeTab == NewTensionTab then
                        { model | nodeDoc = NodeDoc.resetNode model.nodeDoc }

                    else
                        model

                events =
                    case model.activeTab of
                        NewTensionTab ->
                            [ Ev TensionEvent.Created "" "" ]

                        NewRoleTab ->
                            if doClose then
                                [ Ev TensionEvent.Created "" "", Ev TensionEvent.BlobCreated "" "", Ev TensionEvent.BlobPushed "" "" ]

                            else
                                [ Ev TensionEvent.Created "" "", Ev TensionEvent.BlobCreated "" "" ]

                        NewCircleTab ->
                            if doClose then
                                [ Ev TensionEvent.Created "" "", Ev TensionEvent.BlobCreated "" "", Ev TensionEvent.BlobPushed "" "" ]

                            else
                                [ Ev TensionEvent.Created "" "", Ev TensionEvent.BlobCreated "" "" ]
            in
            ( newModel
                |> post "createdAt" (fromTime time)
                |> setEvents events
                |> setStatus (ternary doClose TensionStatus.Closed TensionStatus.Open)
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
                            { model
                                | nodeDoc =
                                    model.nodeDoc
                                        |> NodeDoc.setId tension.id
                                        |> NodeDoc.setUsers (List.map (\u -> { username = u, name = Nothing, email = "", pattern = "" }) model.withUsers)
                            }

                        -- Fire-and-forget: link selected projects in parallel with the
                        -- close cmd so the UI is not blocked by the project cards creation.
                        projectCmds =
                            model.selectedProjects
                                |> List.map
                                    (\tp ->
                                        addProjectCard apis
                                            { uctx = model.nodeDoc.form.uctx
                                            , tids = [ Just tension.id ]
                                            , colid = tp.column.id
                                            , pos = 0
                                            , post = Dict.empty
                                            , title = ""
                                            }
                                            OnAddProjectCardAck
                                    )

                        ( cmds, gcmds_ ) =
                            if model.doInvite && not (List.isEmpty data.nodeDoc.form.users) then
                                ( send (OnSubmit True OnInvite) :: projectCmds
                                , []
                                )

                            else
                                let
                                    link =
                                        Route.Tension_Dynamic_Dynamic { param1 = nid2rootid model.nodeDoc.form.target.nameid, param2 = tension.id } |> toHref
                                in
                                ( send (OnClose { reset = True, link = "" }) :: projectCmds
                                , [ DoPushSystemNotif
                                        { cls = "is-success"
                                        , content =
                                            div [ class "is-flex is-align-items-center mr-5" ]
                                                [ A.icon1 "icon-check icon-2x has-text-success" ""
                                                , text model.nodeDoc.form.txt.added
                                                , text space_
                                                , a [ href link ]
                                                    [ ternary (model.activeTab == NewTensionTab)
                                                        (text T.checkItOut_fem)
                                                        (text T.checkItOut_masc)
                                                    ]
                                                ]
                                        }
                                  ]
                                )

                        gcmds =
                            -- Clear draft on successful tension creation
                            DoUpdateDraft ClearNewTension
                                :: (if tension.status == TensionStatus.Open then
                                        DoPushTension tension :: gcmds_

                                    else
                                        gcmds_
                                   )

                        output =
                            Just ( tension, model.draft )
                    in
                    case model.activeTab of
                        NewTensionTab ->
                            ( setResult result data, Out cmds gcmds output )

                        NewRoleTab ->
                            let
                                newNameid =
                                    getNewNameid NodeType.Role model.nodeDoc
                            in
                            ( setResult result data, Out cmds (DoFetchNode newNameid :: gcmds) output )

                        NewCircleTab ->
                            let
                                newNameid =
                                    getNewNameid NodeType.Circle model.nodeDoc
                            in
                            ( setResult result data, Out cmds (DoFetchNode newNameid :: gcmds) output )

                DuplicateErr ->
                    ( setResult (Failure [ T.duplicateNameError ]) model, noOut )

                _ ->
                    ( setResult result model, noOut )

        LabelSearchPanelMsg msg ->
            let
                ( panel, out ) =
                    LabelSearchPanel.update apis msg model.labelsPanel

                newModel =
                    Maybe.map
                        (\r ->
                            if Tuple.first r then
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

        UserSearchPanelMsg msg ->
            let
                ( panel, out ) =
                    UserSearchPanel.update apis msg model.assigneesPanel

                newModel =
                    Maybe.map
                        (\r ->
                            if Tuple.first r then
                                addAssignee (Tuple.second r) model

                            else
                                removeAssignee (Tuple.second r) model
                        )
                        out.result
                        |> withDefault model

                ( cmds, _ ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { newModel | assigneesPanel = panel }
            , out2 (out.cmds |> List.map (\m -> Cmd.map UserSearchPanelMsg m) |> List.append cmds) out.gcmds
            )

        OnAddProjectCardAck result ->
            -- Fire-and-forget post-creation card linkage. Log failures only.
            case result of
                Failure err ->
                    ( model, out0 [ Ports.logErr (String.join " | " err) ] )

                _ ->
                    ( model, noOut )

        ProjectSearchPanelMsg msg ->
            let
                ( panel, out ) =
                    ProjectSearchPanel.update apis msg model.projectsPanel

                newModel =
                    case out.result of
                        Just (ProjectAdded tp) ->
                            addSelectedProject tp model

                        Just (ProjectRemoved projectId) ->
                            removeSelectedProject projectId model

                        Nothing ->
                            model

                ( cmds, _ ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { newModel | projectsPanel = panel }
            , out2 (out.cmds |> List.map (\m -> Cmd.map ProjectSearchPanelMsg m) |> List.append cmds) out.gcmds
            )

        InviteInputMsg msg ->
            let
                ( data, out ) =
                    UserInput.update apis msg model.inviteInput

                users =
                    out.result
                        |> Maybe.map (\( selected, u ) -> ternary selected u [])
                        |> withDefault model.nodeDoc.form.users

                ( cmds, _ ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model
                | inviteInput = data

                -- setUsers make user in the tension blob which lead to an a request error
                --, nodeDoc = NodeDoc.setUsers users model.nodeDoc
                , withUsers = List.map (\u -> u.username) users
              }
            , out2 (out.cmds |> List.map (\m -> Cmd.map InviteInputMsg m) |> List.append cmds) out.gcmds
            )

        CommentsMsg msg ->
            let
                ( data, out ) =
                    Comments.update apis msg model.comments

                ( nodeDoc, draftTimer, draftSaveCmd ) =
                    case out.result of
                        Just (PostChanged ( k, v )) ->
                            let
                                newNodeDoc =
                                    NodeDoc.updatePost k v model.nodeDoc
                            in
                            -- Schedule debounced draft save when message changes
                            if k == "message" then
                                let
                                    newTimer =
                                        model.draftSaveTimer + 1

                                    time_delay =
                                        ternary (v == "") 0 3500
                                in
                                ( newNodeDoc, newTimer, sendSleep (SaveDraftDelayed newTimer) time_delay )

                            else
                                ( newNodeDoc, model.draftSaveTimer, Cmd.none )

                        _ ->
                            ( model.nodeDoc, model.draftSaveTimer, Cmd.none )

                ( cmds, _ ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | comments = data, nodeDoc = nodeDoc, draftSaveTimer = draftTimer }
            , out2 (draftSaveCmd :: (out.cmds |> List.map (\m -> Cmd.map CommentsMsg m) |> List.append cmds)) out.gcmds
            )

        -- Templates
        GotTemplatesForPicker result ->
            case result of
                RemoteData.Success templates ->
                    if List.isEmpty templates then
                        ( { model | templates = result, templatesLoadingSlow = False, showTemplatePicker = False }, noOut )

                    else
                        ( { model | templates = result, templatesLoadingSlow = False, showTemplatePicker = True }, noOut )

                _ ->
                    ( { model | templates = result, showTemplatePicker = False }, noOut )

        TemplatesLoadingSlow ->
            case model.templates of
                RemoteData.Loading ->
                    ( { model | templatesLoadingSlow = True }, noOut )

                RemoteData.NotAsked ->
                    ( { model | templatesLoadingSlow = True }, noOut )

                _ ->
                    ( model, noOut )

        OnSelectTemplate tpl ->
            -- Two-step: select lite template, then fetch full content via GQL
            ( { model | templateLoading = True }
            , out0 [ getTensionTemplateById apis tpl.id GotTemplateContent ]
            )

        GotTemplateContent result ->
            case result of
                Success tpl ->
                    let
                        newModel =
                            { model
                                | selectedTemplate = Just tpl
                                , showTemplatePicker = False
                                , templateLoading = False
                                , nodeDoc =
                                    model.nodeDoc
                                        |> NodeDoc.updatePost "title" tpl.title
                                        |> NodeDoc.updatePost "message" tpl.comment
                            }

                        form =
                            newModel.nodeDoc.form

                        newForm =
                            { form
                                | labels = withDefault [] tpl.labels
                                , assignees = withDefault [] tpl.assignees
                            }

                        commentCmd =
                            send (Comments.OnChangeComment "message" tpl.comment) |> Cmd.map CommentsMsg
                    in
                    ( { newModel | nodeDoc = NodeDoc.setForm newForm newModel.nodeDoc }
                    , out0 [ send (OnChangeTensionType tpl.type_), commentCmd, Ports.bulma_driver "tensionModal" ]
                    )

                Failure err ->
                    ( { model | templateLoading = False }, out0 [ Ports.logErr (String.join " " err) ] )

                _ ->
                    ( { model | templateLoading = False }, noOut )

        OnSelectBlankTension ->
            ( { model | showTemplatePicker = False, selectedTemplate = Nothing }
            , out0 [ Ports.bulma_driver "tensionModal" ]
            )

        -- Confirm Modal
        DoModalConfirmOpen msg mess ->
            ( { model | modal_confirm = ModalConfirm.open msg mess model.modal_confirm }, noOut )

        DoModalConfirmClose _ ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, noOut )

        DoModalConfirmSend ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, out0 [ send model.modal_confirm.msg ] )

        -- 3-choice Confirm Modal
        DoModalConfirm3Open link ->
            ( { model
                | modal_confirm = ModalConfirm.open NoMsg { message = Nothing, txts = [ ( T.confirmUnsavedDraft, "" ) ], confirmClass = "is-success", confirmLabel = T.confirm } model.modal_confirm
                , modal_confirm3_link = link
              }
            , noOut
            )

        DoModalConfirm3Discard ->
            -- Close modal, reset form, do NOT save draft
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }
            , Out [ send (OnClose { reset = True, link = model.modal_confirm3_link }) ] [ DoUpdateDraft ClearNewTension ] Nothing
            )

        DoModalConfirm3SaveDraft ->
            -- Save draft via DoUpdateDraft, then close
            let
                draftTitle =
                    Dict.get "title" model.nodeDoc.form.post |> withDefault ""

                draftMessage =
                    Dict.get "message" model.nodeDoc.form.post |> withDefault ""

                draft =
                    TensionDraft draftTitle draftMessage ""
            in
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }
            , Out [ send (OnClose { reset = True, link = model.modal_confirm3_link }) ] [ DoUpdateDraft (SaveNewTension draft) ] Nothing
            )

        DoModalConfirm3KeepEditing ->
            -- Close confirm modal only, stay in form
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, noOut )

        -- Common
        NoMsg ->
            ( model, noOut )

        LogErr err ->
            ( model, out0 [ Ports.logErr err ] )

        UpdateUctx uctx ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | user = LoggedIn uctx }, nodeDoc = NodeDoc.setUctx uctx model.nodeDoc }, noOut )

        SaveDraftDelayed timerValue ->
            -- Only save if this is the most recent scheduled save (debounce)
            if timerValue == model.draftSaveTimer then
                -- Read current message content (may have been modified by rich text ports)
                let
                    draftTitle =
                        Dict.get "title" model.nodeDoc.form.post |> withDefault "" |> String.trim

                    draftMessage =
                        Dict.get "message" model.nodeDoc.form.post |> withDefault "" |> String.trim

                    draft =
                        TensionDraft draftTitle draftMessage ""
                in
                if draftMessage == "" || isTemplateUnmodified model then
                    ( model, Out [] [ DoUpdateDraft ClearNewTension ] Nothing )

                else
                    ( model, Out [] [ DoUpdateDraft (SaveNewTension draft) ] Nothing )

            else
                ( model, noOut )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    [ Ports.mcPD Ports.closeModalTensionFromJs LogErr OnClose
    , Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    , Ports.uctxPD Ports.loadUserCtxFromJs LogErr UpdateUctx
    ]
        ++ (if model.isActive then
                (LabelSearchPanel.subscriptions model.labelsPanel |> List.map (\s -> Sub.map LabelSearchPanelMsg s))
                    ++ (UserSearchPanel.subscriptions model.assigneesPanel |> List.map (\s -> Sub.map UserSearchPanelMsg s))
                    ++ (ProjectSearchPanel.subscriptions model.projectsPanel |> List.map (\s -> Sub.map ProjectSearchPanelMsg s))
                    ++ (UserInput.subscriptions model.inviteInput |> List.map (\s -> Sub.map InviteInputMsg s))
                    ++ (Comments.subscriptions model.comments |> List.map (\s -> Sub.map CommentsMsg s))

            else
                []
           )
        ++ (if model.isTargetOpen /= "" then
                [ Ports.closeTreeSelFromJs (always (OnTargetClick ""))
                , Events.onKeyUp (Dom.key "Escape" (OnTargetClick ""))
                ]

            else
                []
           )
        ++ (if model.isTypeOpen /= "" then
                [ Events.onMouseUp (JD.succeed (OnTypeClick ""))
                , Events.onKeyUp (Dom.key "Escape" (OnTypeClick ""))
                ]

            else
                []
           )



-- ------------------------------
-- V I E W
-- ------------------------------


view : GqlData NodesDict -> GqlData LocalGraph -> State -> Html Msg
view tree_data path_data (State model) =
    if model.isActive2 then
        div []
            [ viewModal tree_data (State model)
            , ModalConfirm.view3
                { data = model.modal_confirm
                , onDiscard = DoModalConfirm3Discard
                , onSaveDraft = DoModalConfirm3SaveDraft
                , onKeepEditing = DoModalConfirm3KeepEditing
                }
            ]

    else
        viewButton path_data model


viewButton : GqlData LocalGraph -> Model -> Html Msg
viewButton path_data model =
    div [ class "tensionButton is-hidden-tablet", classList [ ( "is-invisible", not (isSuccess path_data) || model.isActive ) ] ]
        [ button ([ class "button is-success" ] ++ (withMaybeData path_data |> unwrap [] (\p -> [ onClick (OnOpen (FromPath p) Nothing) ])))
            [ A.icon "icon-plus icon-2x" ]
        ]


viewModal : GqlData NodesDict -> State -> Html Msg
viewModal tree_data (State model) =
    div
        [ id "tensionModal"
        , class "modal modal-fx-slideTop"
        , classList [ ( "is-active", model.isActive ), ( "fixed-top", model.step == TensionFinal && withMaybeData model.result == Nothing ) ]
        , attribute "data-modal-close" "closeModalTensionFromJs"
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "tensionModal"
            , onClick (OnCloseSafe "" "")
            ]
            []
        , div [ class "modal-content" ]
            [ viewStep tree_data (State model) ]

        --, button [ class "modal-close is-large", onClick (OnCloseSafe "" "") ] []
        ]


viewStep : GqlData NodesDict -> State -> Html Msg
viewStep tree_data (State model) =
    case model.step of
        TensionSource ->
            text "not implemented; remove in commit 7169bad"

        TensionFinal ->
            case model.activeTab of
                NewTensionTab ->
                    if model.draft /= Nothing || model.currentDraft /= Nothing then
                        viewTension tree_data model

                    else if model.showTemplatePicker then
                        viewTemplatePicker model

                    else
                        viewTension tree_data model

                NewRoleTab ->
                    viewCircle tree_data model

                NewCircleTab ->
                    viewCircle tree_data model

        TensionNotAuthorized ->
            let
                userCanJoin =
                    withMaybeData model.path_data
                        |> Maybe.map
                            (\p ->
                                unwrap2 False .userCanJoin p.root
                            )
                        |> withDefault False
            in
            viewJoinForTensionNeeded model.session userCanJoin OnClose

        AuthNeeded ->
            viewAuthNeeded OnClose


viewHeader : GqlData NodesDict -> Model -> Html Msg
viewHeader tree_data model =
    div [ class "panel-heading pt-2 pb-2" ]
        [ div [ class "level modal-card-title is-size-6" ]
            [ -- div [ class "level-left is-hidden" ]
              --   [ div [ class "has-text-weight-semibold", style "margin-left" "-8px" ] [ textT model.txt.title ] ]
              div [ class "level-left" ]
                [ viewTensionType model ]
            , div [ class "level-right" ]
                [ viewRecipients tree_data model ]
            ]
        ]


viewTensionTabs : SessionCommon -> Bool -> TensionTab -> PNode -> Html Msg
viewTensionTabs session isAdmin tab targ =
    let
        -- This is the type of the receiver node.
        type_ =
            nid2type targ.nameid
    in
    div [ id "tensionTabTop", class "tabs bulma-issue-33" ]
        [ ul []
            [ li [ classList [ ( "is-active", tab == NewTensionTab ) ] ]
                [ a [ class "tootltip", title (T.newTensionHelp session.lexicon), onClickPD (OnSwitchTab NewTensionTab), target "_blank" ]
                    [ A.icon1 "icon-exchange" (T.tension session.lexicon) ]
                ]
            , if isAdmin && type_ == NodeType.Circle then
                li [ classList [ ( "is-active", tab == NewCircleTab ) ] ]
                    [ a [ class "tootltip is-left", title T.newCircleHelp, onClickPD (OnSwitchTab NewCircleTab), target "_blank" ]
                        [ A.icon1 "icon-git-branch" T.circle ]
                    ]

              else
                text ""
            , if isAdmin && type_ == NodeType.Circle then
                li [ classList [ ( "is-active", tab == NewRoleTab ) ] ]
                    [ a [ class "tootltip is-left", title T.newRoleHelp, onClickPD (OnSwitchTab NewRoleTab), target "_blank" ]
                        [ A.icon1 "icon-leaf" T.role ]
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

        isOpen =
            model.isTypeOpen /= ""
    in
    div []
        [ span [ class "is-discrete" ] [ text ("Type" ++ ":" ++ space_) ]
        , if model.activeTab == NewTensionTab then
            B.dropdownLight
                { dropdown_id = "type-menu"
                , isOpen = isOpen
                , dropdown_cls = "tension-modal-dropdown"
                , button_cls = ""
                , button_html =
                    span [ class "button-light" ]
                        [ span [ class <| "has-text-weight-medium " ++ tensionTypeColor "text" tension_type ] [ tensionIcon2 tension_type ]
                        , i [ class "ml-2 icon-chevron-down1 icon-tiny" ] []
                        ]
                , menu_cls = ""
                , content_cls = "has-border-light-small"
                , msg = OnTypeClick (ternary isOpen "" "something")
                , content_html =
                    div [ class "dropdown-item" ]
                        [ [ TensionType.Operational, TensionType.Governance, TensionType.Help ]
                            |> List.map
                                (\x ->
                                    let
                                        isActive =
                                            x == tension_type
                                    in
                                    div
                                        [ class "card has-border column p-0 m-3 is-h is-clickable"
                                        , classList [ ( "is-selected", isActive ) ]
                                        , onClick (OnChangeTensionType x)
                                        ]
                                        [ div [ class "card-content p-3" ]
                                            [ h2 [ class "is-strong is-size-6" ] [ tensionIcon2 x ]
                                            , div [ class "content" ]
                                                [ text (tensionType2descr model.session.lexicon x), br [] [], br [] [], span [ class "help-label" ] [ text (tensionType2notif x) ] ]
                                            ]
                                        ]
                                )
                            |> div [ class "columns" ]
                        , [ TensionType.Alert, TensionType.Announcement ]
                            |> List.map
                                (\x ->
                                    let
                                        isActive =
                                            x == tension_type
                                    in
                                    div
                                        [ class "card has-border column p-0 m-3 is-h is-clickable"
                                        , classList [ ( "is-selected", isActive ) ]
                                        , onClick (OnChangeTensionType x)
                                        ]
                                        [ div [ class "card-content p-3" ]
                                            [ h2 [ class "is-strong is-size-6" ] [ tensionIcon2 x ]
                                            , div [ class "content" ]
                                                [ text (tensionType2descr model.session.lexicon x), br [] [], br [] [], span [ class "help-label" ] [ A.icon1 "icon-alert-triangle" "", text (tensionType2notif x) ] ]
                                            ]
                                        ]
                                )
                            |> div [ class "columns mt-mobile" ]
                        ]
                }

          else
            span [ class <| "has-text-weight-medium " ++ tensionTypeColor "text" tension_type ]
                [ tensionIcon2 tension_type ]
        ]


viewRecipients : GqlData NodesDict -> Model -> Html Msg
viewRecipients tree_data model =
    let
        form =
            model.nodeDoc.form

        isOpen =
            model.isTargetOpen /= ""
    in
    div [ class "recipients-wrapper" ]
        [ -- @DEBUG: emitter is ignored now...
          span [ class "is-discrete recipients-label" ] [ textH (T.to_in ++ ":" ++ space_) ]
        , B.dropdownLight
            { dropdown_id = "target-menu"
            , isOpen = isOpen
            , dropdown_cls = ""
            , button_cls = ""
            , button_html =
                span [ class "is-wrapped-33" ]
                    [ span [ class "button is-small is-rounded has-border is-wrapped is-inline-block" ]
                        [ text form.target.name, i [ class "ml-2 icon-chevron-down1 icon-tiny" ] [] ]
                    ]
            , menu_cls = "is-right is-left-mobile"
            , content_cls = "has-border p-0"
            , content_html = viewSelectorTree (OnChangeTensionTarget tree_data) OnToggleDropdownRoles [ model.nodeDoc.form.target.nameid ] model.expanded_lines tree_data
            , msg = ternary isOpen (OnTargetClick "") (OnTargetClick "something")
            }
        ]



---
--- Final Views
---


viewTemplatePicker : Model -> Html Msg
viewTemplatePicker model =
    let
        form =
            model.nodeDoc.form

        isAdmin =
            hasLazyAdminRole form.uctx Nothing form.target.nameid
    in
    div [ class "panel modal-card submitFocus" ]
        [ if not model.simplifiedView && isAdmin then
            Lazy.lazy4 viewTensionTabs model.session isAdmin model.activeTab form.target

          else
            text ""
        , div [ class "modal-card-head" ]
            [ span [ class "modal-card-title has-text-weight-semibold" ] [ text T.selectTemplate ] ]
        , div [ class "modal-card-body" ]
            [ if model.templateLoading then
                div [ class "spinner" ] []

              else
                case model.templates of
                    RemoteData.Success templates ->
                        div [] <|
                            (templates
                                |> List.map
                                    (\t ->
                                        div
                                            [ class "box is-clickable mb-3"
                                            , tabindex 0
                                            , onClick (OnSelectTemplate t)
                                            , onEnter (OnSelectTemplate t)
                                            ]
                                            [ p [ class "has-text-weight-semibold" ] [ text t.name ]
                                            , t.description
                                                |> unwrap (text "") (\desc -> p [ class "help" ] [ text desc ])
                                            ]
                                    )
                            )
                                ++ (if not model.isTemplateTensionOnly then
                                        [ hr [ style "width" "50%", style "margin" "1rem auto" ] []
                                        , div
                                            [ class "box is-clickable mb-3"
                                            , tabindex 0
                                            , onClick OnSelectBlankTension
                                            , onEnter OnSelectBlankTension
                                            ]
                                            [ p [ class "has-text-weight-semibold" ] [ text T.blankTension ]
                                            , p [ class "help" ] [ text T.blankTensionHelp ]
                                            ]
                                        ]

                                    else
                                        []
                                   )

                    _ ->
                        -- showTemplatePicker is only True when templates are Success with items
                        text ""
            ]
        , div [ class "modal-card-foot" ]
            [ div [ class "field level is-mobile" ]
                [ div [ class "level-left" ]
                    [ button [ class "button", onClick (OnCloseSafe "" "") ] [ text T.cancel ] ]
                ]
            ]
        ]


viewTension : GqlData NodesDict -> Model -> Html Msg
viewTension tree_data model =
    let
        form =
            model.nodeDoc.form

        isAdmin =
            hasLazyAdminRole form.uctx Nothing form.target.nameid

        title =
            Dict.get "title" form.post |> withDefault ""

        commentOpts =
            { hasTips = True
            , isModal = True
            , placeholderText = T.leaveCommentOpt
            , messageHelper = model.nodeDoc.form.txt.message_help
            }

        isLoading =
            model.result == LoadingSlowly

        isSendable =
            isPostSendable [ "title" ] form.post
    in
    div [ class "panel modal-card submitFocus" ]
        [ if model.simplifiedView then
            text ""

          else
            Lazy.lazy4 viewTensionTabs model.session isAdmin model.activeTab form.target
        , Lazy.lazy2 viewHeader tree_data model
        , div [ class "modal-card-body" ]
            [ div [ class "field" ]
                [ div [ class "control" ]
                    [ input
                        [ class "input autofocus followFocus"
                        , attribute "data-nextfocus" "textAreaModal"
                        , type_ "text"
                        , placeholder T.subject
                        , spellcheck True
                        , required True
                        , value title
                        , onInput (OnChangePost "title")
                        ]
                        []
                    ]
                , p [ class "help-label" ] [ text form.txt.name_help ]
                , br [] []
                ]
            , Comments.viewNewTensionCommentInput model.session commentOpts model.comments |> Html.map CommentsMsg
            , br [] [] -- allows selectors panel to display without overlap
            , let
                pathTargets =
                    getPathWithChildren model.path_data

                rootTargets =
                    -- Assignees are org-wide (queryMembers filters by rootnameid).
                    pathTargets |> List.head |> Maybe.map List.singleton |> withDefault []

                labelsOp =
                    { selectedLabels = form.labels
                    , targets = pathTargets
                    , isRight = False
                    }

                assigneesOp =
                    { selectedAssignees = form.assignees
                    , targets = rootTargets
                    , isRight = False
                    }

                projectsOp =
                    { selectedProjects = model.selectedProjects
                    , targets = pathTargets
                    , isRight = False
                    }

                hasLabels =
                    not (List.isEmpty form.labels)

                hasAssignees =
                    not (List.isEmpty form.assignees)

                hasProjects =
                    not (List.isEmpty model.selectedProjects)
              in
              div [ class "field" ]
                [ div [ class "control" ]
                    [ -- Inline container for buttons without selections: assignees, labels, projects
                      div [ class "is-flex is-align-items-center mb-2" ]
                        [ showIf (not hasAssignees) <|
                            (UserSearchPanel.viewNew assigneesOp model.assigneesPanel
                                |> Html.map UserSearchPanelMsg
                            )
                        , showIf (not hasLabels) <|
                            (LabelSearchPanel.viewNew labelsOp model.labelsPanel
                                |> Html.map LabelSearchPanelMsg
                            )
                        , showIf (not hasProjects) <|
                            (ProjectSearchPanel.viewNew projectsOp model.projectsPanel
                                |> Html.map ProjectSearchPanelMsg
                            )
                        ]

                    -- Assignees on own line if has selections
                    , showIf hasAssignees <|
                        div [ class "mb-2" ]
                            [ UserSearchPanel.viewNew assigneesOp model.assigneesPanel
                                |> Html.map UserSearchPanelMsg
                            ]

                    -- Labels on own line if has selections
                    , showIf hasLabels <|
                        div [ class "mb-2" ]
                            [ LabelSearchPanel.viewNew labelsOp model.labelsPanel
                                |> Html.map LabelSearchPanelMsg
                            ]

                    -- Projects on own line if has selections
                    , showIf hasProjects <|
                        div [ class "mb-2" ]
                            [ ProjectSearchPanel.viewNew projectsOp model.projectsPanel
                                |> Html.map ProjectSearchPanelMsg
                            ]
                    ]
                ]
            ]
        , div [ class "modal-card-foot" ]
            [ case model.result of
                Failure err ->
                    viewGqlErrors err

                _ ->
                    text ""
            , div [ class "field level is-mobile" ]
                [ div [ class "level-left" ]
                    [ button
                        [ class "button"
                        , onClick (OnCloseSafe "" "")
                        ]
                        [ text T.cancel ]
                    ]
                , div [ class "level-right" ]
                    [ div [ class "buttons" ]
                        [ button
                            [ class "button is-success defaultSubmit"
                            , classList [ ( "is-loading", isLoading ) ]
                            , disabled (not isSendable)
                            , onClick (OnSubmit (isSendable && not isLoading) (OnSubmitTension False))
                            ]
                            [ text form.txt.submit ]
                        ]
                    ]
                ]
            ]
        ]


viewCircle : GqlData NodesDict -> Model -> Html Msg
viewCircle tree_data model =
    let
        form =
            model.nodeDoc.form

        isAdmin =
            hasLazyAdminRole form.uctx Nothing form.target.nameid

        isLoading =
            model.result == LoadingSlowly || model.action_result == LoadingSlowly

        isSendable =
            isPostSendable [ "title" ] form.post && form.node.name /= Nothing && (form.node.mandate |> Maybe.map .purpose) /= Nothing
    in
    div [ class "panel modal-card submitFocus" ] <|
        [ if model.simplifiedView then
            text ""

          else
            Lazy.lazy4 viewTensionTabs model.session isAdmin model.activeTab form.target
        , Lazy.lazy2 viewHeader tree_data model
        ]
            ++ (case model.nodeStep of
                    RoleAuthorityStep ->
                        [ viewRolesExt model
                        , div [ class "modal-card-foot" ]
                            [ div [ class "field" ]
                                [ div [ class "is-pulled-left" ]
                                    [ button [ class "button", onClick (OnCloseSafe "" "") ] [ text T.cancel ] ]
                                ]
                            ]
                        ]

                    CircleVisibilityStep ->
                        [ viewCircleVisibility model
                        , div [ class "modal-card-foot" ]
                            [ div [ class "field" ]
                                [ div [ class "is-pulled-left" ]
                                    [ button [ class "button", onClick (OnCloseSafe "" "") ] [ text T.cancel ] ]
                                ]
                            ]
                        ]

                    NodeValidateStep ->
                        let
                            inviteText =
                                if model.doInvite && not (List.isEmpty model.withUsers) then
                                    let
                                        users =
                                            List.map (\u -> { username = u, name = Nothing, email = "", pattern = "" }) model.withUsers
                                    in
                                    " + "
                                        ++ ternary (isSelfContract form.uctx users) T.link T.invite

                                else
                                    ""
                        in
                        [ viewNodeValidate model
                        , div [ class "modal-card-foot" ]
                            [ case model.result of
                                Failure err ->
                                    viewGqlErrors err

                                _ ->
                                    text ""
                            , case model.action_result of
                                Failure err ->
                                    viewGqlErrors err

                                _ ->
                                    text ""
                            , div [ class "field level is-mobile" ]
                                [ div [ class "level-left" ]
                                    [ button [ class "button", onClick <| OnChangeNodeStep (ternary (model.activeTab == NewRoleTab) RoleAuthorityStep CircleVisibilityStep) ]
                                        [ A.icon0 "icon-chevron-left", text T.back ]
                                    ]
                                , div [ class "level-right" ]
                                    [ div [ class "buttons" ]
                                        -- Make this as simply at possible / to complex for user from now...
                                        [ if model.simplifiedView || True then
                                            text ""

                                          else
                                            button
                                                [ class "button is-warning"
                                                , classList [ ( "is-loading", isLoading && model.activeButton == Just 1 ) ]
                                                , disabled (not isSendable || isLoading)
                                                , onClickSafe (OnSubmit (isSendable && not isLoading) <| OnSubmitTension False)
                                                ]
                                                [ text form.txt.submit ]
                                        , button
                                            [ class "button is-success defaultSubmit"
                                            , classList [ ( "is-loading", isLoading && model.activeButton == Just 0 ) ]
                                            , disabled (not isSendable || isLoading)
                                            , onClickSafe (OnSubmit (isSendable && not isLoading) <| OnSubmitTension True)
                                            ]
                                            [ text (form.txt.close_submit ++ inviteText) ]
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
    nav [ class "breadcrumb has-succeeds-separator lifeline is-small", attribute "aria-labels" "breadcrumbs" ]
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
            { session = model.session
            , data = model.nodeDoc
            , result = model.result
            , onChangePost = OnChangePost
            , onAddDomains = OnAddDomains
            , onAddPolicies = OnAddPolicies
            , onAddResponsabilities = OnAddResponsabilities
            , mdOps = Nothing
            }
    in
    div [ class "modal-card-body" ]
        [ viewNodeBreadcrumb form model.nodeStep
        , viewAboutInput2 form.txt form.node op
        , viewMandateInput form.txt form.node.mandate op
        , br [] []
        , showIf (model.activeTab == NewRoleTab) (viewUserInvite model)

        --, showIf (not (List.member (Dict.get "message" form.post) [ Nothing, Just "" ])) <|
        --    div [ class "mt-2" ]
        --        [ Comments.viewNewTensionCommentInput model.session commentOpts model.comments |> Html.map CommentsMsg ]
        ]


viewUserInvite : Model -> Html Msg
viewUserInvite model =
    if model.doInvite then
        viewInviteRole model

    else
        div [ class "field is-grouped" ]
            [ div [ class "button is-primary is-small", onClick DoInvite ]
                [ text ("+ " ++ T.inviteSomeone ++ " " ++ T.toThisRole)
                ]
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
        --showMsg "roleAuthority-0" "is-info" "icon-info" T.roleAuthorityHeader ""
        , div [ class "subtitle" ] [ text T.selectRoleTemplate ]
        , case model.roles_result of
            Success roles ->
                List.map
                    (\role ->
                        div
                            [ class "card has-border column p-0 m-3 is-h is-clickable role-ext-card"
                            , classList [ ( "is-selected", Just role.id == form.node.role_ext ) ]
                            ]
                            [ div [ class "card-content p-4", onClick (OnSelectRoleExt role) ]
                                [ h2 [ class "mb-3" ] [ viewRoleExt model.commonOp "" Nothing role ]
                                , div [ class "content is-small" ] [ text (withDefault "" role.about) ]
                                ]
                            ]
                    )
                    roles
                    |> (\l ->
                            l
                                ++ [ br [ class "clearfix" ] []
                                   , div [ class "card-content", attribute "style" (ternary (List.length l == 0) "margin-top: -1rem;" "") ]
                                        [ if List.length l == 0 then
                                            span [ class "content is-small" ]
                                                [ text T.noTemplateRole
                                                , br [ class "my-3" ] []
                                                , text T.youCanMake
                                                ]

                                          else
                                            span [ class "content is-small" ]
                                                [ text T.needNewRole
                                                , br [ class "my-3" ] []
                                                , text T.makeA
                                                ]
                                        , span
                                            [ class "button is-small has-text-link mx-2"
                                            , title T.adhocRoleHint
                                            , onClick (OnChangeNodeStep NodeValidateStep)
                                            ]
                                            [ text T.adhocRole ]
                                        , span [ class "content is-small" ] [ text T.orAdd ]
                                        , span
                                            [ class "button is-small has-text-link mx-2"
                                            , title T.templateRoleHint
                                            , onClick (OnCloseSafe (toLink SettingsBaseUri form.target.nameid [] ++ "?m=roles&a=new") "")
                                            ]
                                            [ text T.templateRole ]
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
        , div [ class "subtitle" ] [ text T.selectCircleVisibility ]

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
                                    ( "icon-globe", visibility2descr x )

                                NodeVisibility.Private ->
                                    ( "icon-users", visibility2descr x )

                                NodeVisibility.Secret ->
                                    ( "icon-lock", visibility2descr x )
                    in
                    div
                        [ class "card has-border column p-0 m-3 is-h is-clickable"
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
    div [ class "has-border-hint-primary" ]
        [ UserInput.view { label_text = text (T.inviteOrLink ++ ":"), showEmail = True, placeholder_text = Nothing } model.inviteInput |> Html.map InviteInputMsg
        , viewInvitationInput model
        ]


viewInvitationInput : Model -> Html Msg
viewInvitationInput model =
    -- Be carefull to not overwrite tension_form.post is reusing Comments inputs.
    let
        message =
            Dict.get "invitation" model.nodeDoc.form.post |> withDefault ""
    in
    div [ class "field" ]
        [ div [ class "control" ]
            [ textarea
                [ class "textarea"
                , rows 3
                , placeholder T.leaveCommentOpt
                , value message
                , onInput <| OnChangePost "invitation"
                ]
                []
            ]
        , p [ class "help-label" ] [ text T.invitationMessageHelp ]
        ]



--
-- Utils
--


getNewNameid : NodeType.NodeType -> NodeDoc -> String
getNewNameid type_ nodeDoc =
    nodeDoc.form.node.nameid
        |> Maybe.map (\nid -> nodeIdCodec nodeDoc.form.target.nameid nid (withDefault type_ nodeDoc.form.node.type_))
        |> withDefault ""
