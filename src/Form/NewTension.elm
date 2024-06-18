{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2024 Fractale Co

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
import Bulk exposing (Ev, FormText, InputViewMode(..), TensionForm, UserState(..), getPath, initFormText, isSelfContract, localGraphFromOrga, makeCandidateContractForm, tensionToActionForm)
import Bulk.Bulma as B
import Bulk.Codecs exposing (DocType(..), FractalBaseRoute(..), getOrgaRoles, nearestCircleid, nid2rootid, nid2type, nodeIdCodec, toLink, ur2eor)
import Bulk.Error exposing (viewAuthNeeded, viewGqlErrors, viewJoinForTensionNeeded)
import Bulk.View exposing (tensionIcon2, tensionType2descr, tensionType2notif, tensionTypeColor, viewRoleExt, visibility2descr)
import Components.Comments as Comments exposing (OutType(..))
import Components.LabelSearchPanel as LabelSearchPanel
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.NodeDoc as NodeDoc exposing (NodeDoc, NodeView(..), viewAboutInput2, viewMandateInput)
import Components.TreeMenu exposing (viewSelectorTree)
import Components.UserInput as UserInput
import Dict
import Dom
import Extra exposing (space_, ternary, textH, unwrap, unwrap2)
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
import Html exposing (Html, a, br, button, div, h2, i, input, label, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, autofocus, class, classList, disabled, href, id, placeholder, required, rows, spellcheck, style, tabindex, target, title, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import Json.Decode as JD
import Loading exposing (GqlData, ModalData, RequestResult(..), isSuccess, withDefaultData, withMaybeData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Ports
import Query.AddContract exposing (addOneContract)
import Query.AddTension exposing (addOneTension)
import Query.PatchTension exposing (actionRequest)
import Query.QueryNode exposing (queryLocalGraph, queryRolesFull)
import Schemas.TreeMenu exposing (ExpandedLines)
import Session exposing (Apis, CommonMsg, Conf, GlobalCmd(..), LabelSearchPanelOnClickAction(..))
import Text as T
import Time



-- ------------------------------
-- M O D E L
-- ------------------------------


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
    , path_data : GqlData LocalGraph
    , action_result : GqlData IdPayload
    , draft : Maybe ProjectDraft
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
    , withUsers : Maybe (List String)
    , simplifiedView : Bool

    -- Common
    , conf : Conf
    , refresh_trial : Int
    , modal_confirm : ModalConfirm Msg
    , commonOp : CommonMsg Msg

    -- Components
    , labelsPanel : LabelSearchPanel.State
    , inviteInput : UserInput.State
    , userInput : UserInput.State
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


init : UserState -> Conf -> State
init user conf =
    initModel user conf |> State


initModel : UserState -> Conf -> Model
initModel user conf =
    { user = user
    , result = NotAsked
    , sources = []
    , step = TensionFinal
    , isActive = False
    , isActive2 = False
    , activeTab = NewTensionTab
    , activeButton = Nothing
    , nodeDoc = NodeDoc.init "" Nothing NodeEdit user
    , path_data = NotAsked -- may be different than the current path_data (op.path_data)
    , action_result = NotAsked
    , doInvite = False
    , withUsers = Nothing
    , simplifiedView = False
    , draft = Nothing
    , isTargetOpen = ""
    , isTypeOpen = ""
    , expanded_lines = Dict.empty
    , freezeOutsideClick = False

    -- Role/Circle
    , nodeStep = RoleAuthorityStep -- will change
    , roles_result = Loading
    , force_init = False

    -- Common
    , conf = conf
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    , commonOp = CommonMsg NoMsg LogErr

    -- Components
    , labelsPanel = LabelSearchPanel.init "" SelectLabel user
    , inviteInput = UserInput.init [] True False user
    , userInput = UserInput.init [] False False user
    , comments = Comments.init "" "" user
    }



-- Global methods
--  nothing here
-- State Controls


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
                            , txt = initFormText Nothing
                        }

                    NewRoleTab ->
                        { form
                            | type_ = Just TensionType.Governance
                            , blob_type = Just BlobType.OnNode
                            , node = { node | type_ = Just NodeType.Role }
                            , action = Just TensionAction.NewRole
                            , users = []
                            , txt = initFormText (Just NodeType.Role)
                        }
                            |> NodeDoc.updateNodeForm "name" (Dict.get "title" form.post |> withDefault "")

                    NewCircleTab ->
                        { form
                            | type_ = Just TensionType.Governance
                            , blob_type = Just BlobType.OnNode
                            , node = { node | type_ = Just NodeType.Circle }
                            , action = Just TensionAction.NewCircle
                            , users = []
                            , txt = initFormText (Just NodeType.Circle)
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
    initModel data.user data.conf



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
    not (isPostEmpty [ "title", "message" ] data.nodeDoc.form.post)



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
    | InviteInputMsg UserInput.Msg
    | UserInputMsg UserInput.Msg
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
            case model.user of
                LoggedIn uctx ->
                    let
                        newModel =
                            case d of
                                Just draft ->
                                    { model
                                        | draft = d
                                        , nodeDoc =
                                            model.nodeDoc
                                                |> NodeDoc.updatePost "title" draft.title
                                                |> NodeDoc.updatePost "message" (withDefault "" draft.message)
                                    }

                                Nothing ->
                                    model

                        cmd =
                            case Dict.get "message" newModel.nodeDoc.form.post of
                                Just m ->
                                    send (Comments.OnChangeComment "message" m) |> Cmd.map CommentsMsg

                                Nothing ->
                                    Cmd.none
                    in
                    case t of
                        FromNameid nameid ->
                            ( newModel, out0 [ queryLocalGraph apis nameid True (GotPath True), cmd ] )

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
                                    switch_cmd =
                                        case model.activeTab of
                                            NewTensionTab ->
                                                Cmd.none

                                            NewRoleTab ->
                                                send (OnSwitchTab NewRoleTab)

                                            NewCircleTab ->
                                                send (OnSwitchTab NewCircleTab)
                                in
                                ( { data | isActive2 = True } |> setUctx uctx
                                , out0
                                    [ sendSleep (SetIsActive2 True) 10
                                    , switch_cmd
                                    , Cmd.map UserInputMsg (send <| UserInput.ChangePath (List.map .nameid p.path))
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
            let
                cmd =
                    if isSuccess model.result then
                        send OnReset

                    else
                        Cmd.none
            in
            ( { model | activeTab = NewRoleTab, force_init = True, withUsers = Just [ u ], activeButton = Just 0, simplifiedView = True }
            , out0 [ send (OnOpen t Nothing), cmd ]
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

        OnCloseSafe link onCloseTxt ->
            if canExitSafe model then
                ( model, out0 [ send (OnClose { reset = True, link = link }) ] )

            else
                ( model
                , out0
                    [ send
                        (DoModalConfirmOpen (OnClose { reset = True, link = link })
                            { message = Nothing, txts = [ ( T.confirmUnsaved, onCloseTxt ) ] }
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
                                        getPath model.path_data |> List.map .nameid
                                in
                                [ queryRolesFull apis nameids OnGotRoles ]

                            else
                                []

                        _ ->
                            []
            in
            ( switchTab tab model, out0 (Ports.bulma_driver "tensionModal" :: cmds) )

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
                    ( { model | refresh_trial = i }, out2 [ sendSleep (OnSubmit True OnInvite) 500 ] [ DoUpdateToken ] )

                OkAuth _ ->
                    let
                        form =
                            model.nodeDoc.form
                    in
                    ( { model | action_result = result }
                    , ternary (isSelfContract form.uctx form.users) (out1 [ DoUpdateToken ]) noOut
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
            case localGraphFromOrga target.nameid odata of
                Just path ->
                    ( setPath path model, out0 [ send (OnTargetClick "") ] )

                Nothing ->
                    ( setTarget (shrinkNode target) model, out0 [ send (OnTargetClick "") ] )

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
                                    NodeDoc.setId tension.id model.nodeDoc
                                        |> (\nd ->
                                                case model.withUsers of
                                                    Just us ->
                                                        NodeDoc.setUsers
                                                            (List.map (\u -> { username = u, name = Nothing, email = "", pattern = "" }) us)
                                                            nd

                                                    Nothing ->
                                                        nd
                                           )
                            }

                        gcmds =
                            if tension.status == TensionStatus.Open then
                                [ DoPushTension tension ]

                            else
                                []

                        cmds =
                            if not (List.isEmpty data.nodeDoc.form.users) then
                                [ send (OnSubmit True OnInvite) ]

                            else
                                []

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
        --    , if not model.isLookupOpen then
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
            ( { model | inviteInput = data, nodeDoc = NodeDoc.setUsers users model.nodeDoc }
            , out2 (out.cmds |> List.map (\m -> Cmd.map InviteInputMsg m) |> List.append cmds) out.gcmds
            )

        UserInputMsg msg ->
            let
                ( data, out ) =
                    UserInput.update apis msg model.userInput

                cmd =
                    case out.result of
                        Just ( selected, us ) ->
                            if selected then
                                case us of
                                    [ u ] ->
                                        Ports.pushInputSelection u.username

                                    _ ->
                                        Cmd.none

                            else
                                Cmd.none

                        Nothing ->
                            Cmd.none

                ( cmds, _ ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | userInput = data }
            , out2 (out.cmds |> List.map (\m -> Cmd.map UserInputMsg m) |> List.append (cmd :: cmds)) out.gcmds
            )

        CommentsMsg msg ->
            let
                ( data, out ) =
                    Comments.update apis msg model.comments

                nodeDoc =
                    case out.result of
                        Just (PostChanged ( k, v )) ->
                            --send (OnChangePost k v)
                            NodeDoc.updatePost k v model.nodeDoc

                        _ ->
                            model.nodeDoc

                ( cmds, _ ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | comments = data, nodeDoc = nodeDoc }
            , out2 (out.cmds |> List.map (\m -> Cmd.map CommentsMsg m) |> List.append cmds) out.gcmds
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
        ++ (if model.isActive then
                (LabelSearchPanel.subscriptions model.labelsPanel |> List.map (\s -> Sub.map LabelSearchPanelMsg s))
                    ++ (UserInput.subscriptions model.inviteInput |> List.map (\s -> Sub.map InviteInputMsg s))
                    ++ (UserInput.subscriptions model.userInput |> List.map (\s -> Sub.map UserInputMsg s))
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
            , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
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
            viewJoinForTensionNeeded userCanJoin OnClose

        AuthNeeded ->
            viewAuthNeeded OnClose


viewSuccess : Tension -> Model -> Html Msg
viewSuccess res model =
    let
        isSelfContract_ =
            isSelfContract model.nodeDoc.form.uctx model.nodeDoc.form.users

        link =
            case model.action_result of
                Success c ->
                    if isSelfContract_ then
                        Route.Tension_Dynamic_Dynamic_Action { param1 = nid2rootid model.nodeDoc.form.target.nameid, param2 = res.id } |> toHref

                    else
                        Route.Tension_Dynamic_Dynamic_Contract_Dynamic { param1 = nid2rootid model.nodeDoc.form.target.nameid, param2 = res.id, param3 = c.id } |> toHref

                _ ->
                    Route.Tension_Dynamic_Dynamic { param1 = nid2rootid model.nodeDoc.form.target.nameid, param2 = res.id } |> toHref
    in
    div [ class "notification is-success-light", autofocus True, tabindex 0, onEnter (OnClose { reset = True, link = "" }) ]
        [ button [ class "delete", onClick (OnCloseSafe "" "") ] []
        , div [ class "is-flex is-align-items-center" ]
            [ A.icon1 "icon-check icon-2x has-text-success" ""
            , text model.nodeDoc.form.txt.added
            , text space_
            , a
                [ href link
                , onClickPD (OnClose { reset = True, link = link })
                , target "_blank"
                ]
                [ case model.activeTab of
                    NewTensionTab ->
                        text T.checkItOut_fem

                    _ ->
                        text T.checkItOut_masc
                ]
            ]
        , if model.activeTab == NewRoleTab && model.activeButton == Just 0 then
            case model.action_result of
                Success _ ->
                    div [ class "is-flex is-align-items-center" ]
                        [ A.icon1 "icon-check icon-2x has-text-success" ""
                        , if isSelfContract_ then
                            text T.self_link_action_success

                          else
                            text T.link_action_success
                        ]

                _ ->
                    if model.doInvite then
                        viewInviteRole model

                    else
                        span [ class "m-2 is-inline-flex is-align-items-baseline" ]
                            [ textH T.or_, a [ class "button is-small mx-2 is-primary", onClick DoInvite, target "_blank" ] [ text T.inviteSomeone ], text T.toThisRole ]

          else if model.activeTab == NewCircleTab && model.activeButton == Just 0 then
            span [ class "m-2 is-inline-flex is-align-items-baseline" ]
                [ textH T.or_
                , a [ class "button is-small mx-2 is-primary", onClick (OnOpenRole (FromNameid (getNewNameid NodeType.Role model.nodeDoc))), target "_blank" ] [ text T.addRole ]
                , text T.inThisCircle
                ]

          else
            text ""
        ]


viewHeader : GqlData NodesDict -> Model -> Html Msg
viewHeader tree_data model =
    div [ class "panel-heading pt-2 pb-2", style "border-radius" "0" ]
        [ div [ class "level modal-card-title is-size-6" ]
            [ -- div [ class "level-left is-hidden" ]
              --   [ div [ class "has-text-weight-semibold", style "margin-left" "-8px" ] [ textT model.txt.title ] ]
              div [ class "level-left" ]
                [ viewTensionType model ]
            , div [ class "level-right" ]
                [ viewRecipients tree_data model ]
            ]
        ]


viewTensionTabs : Bool -> TensionTab -> PNode -> Html Msg
viewTensionTabs isAdmin tab targ =
    let
        -- This is the type of the receiver node.
        type_ =
            nid2type targ.nameid
    in
    div [ id "tensionTabTop", class "tabs bulma-issue-33" ]
        [ ul []
            [ li [ classList [ ( "is-active", tab == NewTensionTab ) ] ]
                [ a [ class "tootltip has-tooltip-bottom is-left has-tooltip-arrow", attribute "data-tooltip" T.newTensionHelp, onClickPD (OnSwitchTab NewTensionTab), target "_blank" ]
                    [ A.icon1 "icon-exchange" T.tension ]
                ]
            , if isAdmin && type_ == NodeType.Circle then
                li [ classList [ ( "is-active", tab == NewCircleTab ) ] ]
                    [ a [ class "tootltip has-tooltip-bottom is-left has-tooltip-arrow", attribute "data-tooltip" T.newCircleHelp, onClickPD (OnSwitchTab NewCircleTab), target "_blank" ]
                        [ A.icon1 "icon-git-branch" T.circle ]
                    ]

              else
                text ""
            , if isAdmin && type_ == NodeType.Circle then
                li [ classList [ ( "is-active", tab == NewRoleTab ) ] ]
                    [ a [ class "tootltip has-tooltip-bottom is-left has-tooltip-arrow", attribute "data-tooltip" T.newRoleHelp, onClickPD (OnSwitchTab NewRoleTab), target "_blank" ]
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
        [ span [ class "has-text-grey-light" ] [ text ("Type" ++ ":" ++ space_) ]
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
                , content_cls = "has-border-light"
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
                                        [ class "card has-border column is-paddingless m-3 is-h"
                                        , classList [ ( "is-selected", isActive ) ]
                                        , onClick (OnChangeTensionType x)
                                        ]
                                        [ div [ class "card-content p-3" ]
                                            [ h2 [ class "is-strong is-size-6" ] [ tensionIcon2 x ]
                                            , div [ class "content is-small" ]
                                                [ text (tensionType2descr x), span [ class "help" ] [ text (tensionType2notif x) ] ]
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
                                        [ class "card has-border column is-paddingless m-3 is-h"
                                        , classList [ ( "is-selected", isActive ) ]
                                        , onClick (OnChangeTensionType x)
                                        ]
                                        [ div [ class "card-content p-3" ]
                                            [ h2 [ class "is-strong is-size-6" ] [ tensionIcon2 x ]
                                            , div [ class "content is-small" ]
                                                [ text (tensionType2descr x), span [ class "help" ] [ text (tensionType2notif x) ] ]
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
    div [ attribute "style" "max-width: 285px;" ]
        [ -- @DEBUG: emitter is ignored now...
          span [ class "has-text-grey-light", attribute "style" "position:relative;top:7px;" ] [ textH (T.to_in ++ ":" ++ space_) ]
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


viewTension : GqlData NodesDict -> Model -> Html Msg
viewTension tree_data model =
    let
        form =
            model.nodeDoc.form

        isAdmin =
            hasLazyAdminRole form.uctx Nothing form.target.nameid

        title =
            Dict.get "title" form.post |> withDefault ""

        isLoading =
            model.result == LoadingSlowly

        isSendable =
            isPostSendable [ "title" ] form.post
    in
    case model.result of
        Success res ->
            viewSuccess res model

        other ->
            div [ class "panel modal-card submitFocus" ]
                [ if model.simplifiedView then
                    text ""

                  else
                    Lazy.lazy3 viewTensionTabs isAdmin model.activeTab form.target
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
                    , Comments.viewNewTensionCommentInput model.conf model.comments |> Html.map CommentsMsg
                    , div [ class "field" ]
                        [ div [ class "control" ]
                            [ LabelSearchPanel.viewNew
                                { selectedLabels = form.labels
                                , targets = getPath model.path_data |> List.map .nameid
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
            model.result == LoadingSlowly

        isSendable =
            isPostSendable [ "title" ] form.post && form.node.name /= Nothing && (form.node.mandate |> Maybe.map .purpose) /= Nothing
    in
    case model.result of
        Success res ->
            viewSuccess res model

        other ->
            div [ class "panel modal-card submitFocus" ] <|
                [ if model.simplifiedView then
                    text ""

                  else
                    Lazy.lazy3 viewTensionTabs isAdmin model.activeTab form.target
                , Lazy.lazy2 viewHeader tree_data model
                ]
                    ++ (case model.nodeStep of
                            RoleAuthorityStep ->
                                [ viewRolesExt model
                                , div [ class "modal-card-foot", attribute "style" "display: block;" ]
                                    [ div [ class "field" ]
                                        [ div [ class "is-pulled-left" ]
                                            [ button [ class "button is-light", onClick (OnCloseSafe "" "") ] [ text T.cancel ] ]
                                        ]
                                    ]
                                ]

                            CircleVisibilityStep ->
                                [ viewCircleVisibility model
                                , div [ class "modal-card-foot", attribute "style" "display: block;" ]
                                    [ div [ class "field" ]
                                        [ div [ class "is-pulled-left" ]
                                            [ button [ class "button is-light", onClick (OnCloseSafe "" "") ] [ text T.cancel ] ]
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
                                                        , classList
                                                            [ ( "is-loading", isLoading && model.activeButton == Just 1 ) ]
                                                        , disabled (not isSendable || isLoading)
                                                        , onClickSafe (OnSubmit (isSendable && not isLoading) <| OnSubmitTension False)
                                                        ]
                                                        [ text form.txt.submit ]
                                                , button
                                                    [ class "button is-success defaultSubmit"
                                                    , classList
                                                        [ ( "is-loading", isLoading && model.activeButton == Just 0 ) ]
                                                    , disabled (not isSendable || isLoading)
                                                    , onClickSafe (OnSubmit (isSendable && not isLoading) <| OnSubmitTension True)
                                                    ]
                                                    [ text form.txt.close_submit ]
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
    in
    div [ class "modal-card-body" ]
        [ viewNodeBreadcrumb form model.nodeStep
        , viewAboutInput2 form.txt form.node op
        , viewMandateInput form.txt form.node.mandate op
        , br [] []
        , if not (List.member (Dict.get "message" form.post) [ Nothing, Just "" ]) then
            div [ class "mt-2" ]
                [ Comments.viewNewTensionCommentInput model.conf model.comments |> Html.map CommentsMsg
                ]

          else
            text ""
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
        , div [ class "subtitle" ] [ text T.selectRoleTemplate ]
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
                                            span [ class "content is-small" ] [ text T.noTemplateRole, br [ class "mb-5" ] [], text T.youCanMake ]

                                          else
                                            span [ class "content is-small" ] [ text T.needNewRole, br [] [], text T.makeA ]
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
            [ UserInput.view { label_text = text (T.inviteOrLink ++ ":") } model.inviteInput |> Html.map InviteInputMsg
            , viewCommentInput model
            , case model.action_result of
                Failure err ->
                    div [ class "field" ] [ viewGqlErrors err ]

                _ ->
                    text ""
            , div [ class "field" ]
                [ div [ class "is-pulled-right" ]
                    [ button
                        [ class "button is-link"
                        , classList [ ( "is-loading", isLoading ) ]
                        , disabled (not (isUsersSendable form.users) || isLoading)
                        , onClick (OnSubmit (not isLoading) OnInvite)
                        ]
                        [ ternary (isSelfContract form.uctx form.users) T.link T.invite |> text ]
                    ]
                ]
            ]
        ]


viewCommentInput : Model -> Html Msg
viewCommentInput model =
    let
        message =
            Dict.get "message" model.nodeDoc.form.post |> withDefault ""
    in
    div [ class "field" ]
        [ div [ class "control" ]
            [ textarea
                [ class "textarea"
                , rows 3
                , placeholder T.leaveCommentOpt
                , value message
                , onInput <| OnChangePost "message"
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
