{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2023 Fractale Co

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


module Components.ActionPanel exposing (Msg(..), State, init, isOpen_, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Browser.Events as Events
import Bulk
    exposing
        ( ActionForm
        , Ev
        , UserForm
        , UserState(..)
        , blobFromTensionHead
        , getNode
        , getNodeRights
        , getOwners
        , initActionForm
        , isSelfContract
        , makeCandidateContractForm
        , uctxFromUser
        )
import Bulk.Codecs exposing (ActionType(..), DocType(..), FractalBaseRoute(..), TensionCharac, getOrgaRoles, isBaseMember, isOwner, nearestCircleid, nid2rootid, playsRole, uriFromNameid, userFromBaseMember)
import Bulk.Error exposing (viewGqlErrors)
import Bulk.View exposing (auth2icon, auth2str, roleColor, viewUserFull, visibility2descr, visibility2icon)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Components.MoveTension as MoveTension
import Components.UserInput as UserInput
import Dict exposing (Dict)
import Dom
import Extra exposing (mor, ternary, textH, upH)
import Extra.Events exposing (onClickPD)
import Extra.Views exposing (showMsg)
import Form exposing (isPostEmpty, isUsersSendable)
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.NodeVisibility as NodeVisibility
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), isFailure, isSuccess)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Ports
import Query.AddContract exposing (addOneContract)
import Query.PatchTension exposing (actionRequest)
import Query.QueryNode exposing (fetchNode2)
import Query.QueryTension exposing (getTensionHead)
import Session exposing (Apis, GlobalCmd(..), Screen, isMobile)
import String.Format as Format
import Text as T
import Time


type State
    = State Model


type alias Model =
    { user : UserState
    , action_result : GqlData IdPayload
    , isOpen : Bool
    , isModalActive : Bool
    , form : ActionForm
    , state : PanelState
    , step : ActionStep
    , domid : String -- allow multiple panel to coexists
    , targetid : String -- real nameid of the target (case of node whithout tension ->  membership roles)
    , role_type : Maybe RoleType.RoleType -- real role_type
    , pos : Maybe ( Int, Int )

    -- Common
    , screen : Screen
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg

    -- Components
    , moveTension : MoveTension.State
    , userInput : UserInput.State
    }


type PanelState
    = MoveAction
    | VisibilityAction
    | AuthorityAction
    | LinkAction
    | UnLinkAction User
    | ArchiveAction
    | UnarchiveAction
    | LeaveAction


type ActionStep
    = StepOne
    | StepAck IdPayload


initModel : UserState -> Screen -> Model
initModel user screen =
    { user = user
    , action_result = NotAsked
    , isOpen = False
    , isModalActive = False
    , form = initActionForm "" user
    , state = LinkAction -- random
    , step = StepOne
    , domid = ""
    , targetid = ""
    , role_type = Nothing
    , pos = Nothing

    -- Common
    , screen = screen
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    , moveTension = MoveTension.init user
    , userInput = UserInput.init False user
    }


init : UserState -> Screen -> State
init user screen =
    initModel user screen |> State


panelAction2str : PanelState -> String
panelAction2str action =
    case action of
        MoveAction ->
            T.move

        VisibilityAction ->
            T.visibility

        AuthorityAction ->
            T.authority

        LinkAction ->
            T.firstLink

        UnLinkAction _ ->
            T.unlink

        ArchiveAction ->
            T.archive

        UnarchiveAction ->
            T.unarchive

        LeaveAction ->
            T.leaveRole


action2submitstr : PanelState -> Bool -> String
action2submitstr action selfContract =
    case action of
        MoveAction ->
            -- handled by MoveTension components
            ""

        VisibilityAction ->
            T.submit

        AuthorityAction ->
            T.submit

        LinkAction ->
            if selfContract then
                T.link

            else
                T.invite

        UnLinkAction _ ->
            T.unlink

        ArchiveAction ->
            T.archive

        UnarchiveAction ->
            T.unarchive

        LeaveAction ->
            T.leaveRole


action2header : PanelState -> NodeType.NodeType -> String
action2header action type_ =
    case action of
        MoveAction ->
            -- handled by MoveTension components
            ""

        VisibilityAction ->
            T.visibility_header

        AuthorityAction ->
            case type_ of
                NodeType.Circle ->
                    T.authority_circle_header

                NodeType.Role ->
                    T.authority_role_header

        LinkAction ->
            T.link_header

        UnLinkAction _ ->
            T.unlink_header

        ArchiveAction ->
            T.archive_header

        UnarchiveAction ->
            T.unarchive_header

        LeaveAction ->
            T.leave_header


action2post : PanelState -> Bool -> String
action2post action selfContract =
    case action of
        MoveAction ->
            -- handled by MoveTension components
            ""

        VisibilityAction ->
            T.visibility_action_success

        AuthorityAction ->
            T.authority_action_success

        LinkAction ->
            if selfContract then
                T.self_link_action_success

            else
                T.link_action_success

        UnLinkAction _ ->
            T.unlink_action_success

        ArchiveAction ->
            T.archive_action_success

        UnarchiveAction ->
            T.unarchive_action_success

        LeaveAction ->
            T.left_action_success


action2color : PanelState -> String
action2color action =
    case action of
        VisibilityAction ->
            "warning"

        AuthorityAction ->
            "warning"

        ArchiveAction ->
            "warning"

        LinkAction ->
            "link"

        UnLinkAction _ ->
            "danger"

        LeaveAction ->
            "danger"

        _ ->
            -- default
            ""



-- Global methods


isOpen_ : String -> State -> Bool
isOpen_ domid (State model) =
    domid == model.domid && (model.isOpen || model.isModalActive)



--- State Controls


open : String -> String -> String -> Node -> Model -> Model
open domid tid bid node data =
    let
        f =
            data.form
    in
    { data | isOpen = True, domid = domid }
        |> setTid tid
        |> setBid bid
        |> setNode node
        |> setFragment (\frag -> { frag | type_ = Just node.type_ })


close : Model -> Model
close data =
    { data | isOpen = False, pos = Nothing }


setActionResult : GqlData IdPayload -> Model -> Model
setActionResult result data =
    let
        ( isModalActive, step ) =
            case result of
                Success res ->
                    ( data.isModalActive, StepAck res )

                Failure _ ->
                    ( data.isModalActive, data.step )

                _ ->
                    ( data.isModalActive, data.step )
    in
    { data | action_result = result, isModalActive = isModalActive, step = step }


openModal : Model -> Model
openModal data =
    { data | isModalActive = True }


closeModal : Model -> Model
closeModal data =
    { data | isModalActive = False }


reset : Model -> Model
reset data =
    initModel data.user data.screen


setStep : ActionStep -> Model -> Model
setStep step data =
    { data | step = step }


setAction : PanelState -> Model -> Model
setAction action data =
    { data | state = action }


setActionForm : Model -> Model
setActionForm data =
    let
        node =
            data.form.node

        frag =
            data.form.fragment

        events =
            case data.state of
                MoveAction ->
                    -- @see MoveTension.elm
                    []

                VisibilityAction ->
                    [ Ev TensionEvent.Visibility
                        (node.visibility |> NodeVisibility.toString)
                        (frag.visibility |> withDefault node.visibility |> NodeVisibility.toString)
                    ]

                AuthorityAction ->
                    case node.type_ of
                        NodeType.Circle ->
                            [ Ev TensionEvent.Authority
                                (node.mode |> NodeMode.toString)
                                (frag.mode |> withDefault node.mode |> NodeMode.toString)
                            ]

                        NodeType.Role ->
                            [ Ev TensionEvent.Authority
                                (node.role_type |> Maybe.map (\rt -> RoleType.toString rt) |> withDefault "")
                                (mor frag.role_type node.role_type |> Maybe.map (\rt -> RoleType.toString rt) |> withDefault "")
                            ]

                LinkAction ->
                    [ Ev TensionEvent.MemberLinked
                        (node.first_link |> Maybe.map .username |> withDefault "")
                        (mor
                            (List.head data.form.users |> Maybe.map (\x -> ternary (x.email == "") x.username x.email))
                            (node.first_link |> Maybe.map .username)
                            |> withDefault ""
                        )
                    ]

                UnLinkAction user ->
                    -- see (1)
                    [ Ev TensionEvent.MemberUnlinked user.username "" ]

                ArchiveAction ->
                    [ Ev TensionEvent.BlobArchived "" "" ]

                UnarchiveAction ->
                    [ Ev TensionEvent.BlobUnarchived "" "" ]

                LeaveAction ->
                    -- see (1)
                    [ Ev TensionEvent.UserLeft
                        data.form.uctx.username
                        (data.role_type |> Maybe.map (\rt -> RoleType.toString rt) |> withDefault "")
                    ]

        -- (1) for membership node, as their is no blob, role_type need to be passed to the backend to avoid extra database request
        -- Note: This only work if the membersip node is not present in the tree (ie. not for Owner node...)
        -- (see QueryNode.nodeOrgaFilter)
    in
    data |> setEvents events


updatePost : String -> String -> Model -> Model
updatePost field value data =
    let
        f =
            data.form
    in
    { data | form = { f | post = Dict.insert field value f.post } }


setTid : String -> Model -> Model
setTid tid data =
    let
        f =
            data.form
    in
    { data | form = { f | tid = tid } }


setBid : String -> Model -> Model
setBid bid data =
    let
        f =
            data.form
    in
    { data | form = { f | bid = bid } }


setNode : Node -> Model -> Model
setNode n data =
    let
        f =
            data.form
    in
    { data | form = { f | node = n } }


setFragment : (NodeFragment -> NodeFragment) -> Model -> Model
setFragment fun data =
    let
        f =
            data.form
    in
    { data | form = { f | fragment = fun f.fragment } }


setEvents : List Ev -> Model -> Model
setEvents events data =
    let
        f =
            data.form
    in
    { data | form = { f | events = events } }



-- utils


canExitSafe : Model -> Bool
canExitSafe model =
    -- Condition to close safely (e.g. empty form data)
    (hasData model
        == False
        && (case model.state of
                VisibilityAction ->
                    List.member model.form.fragment.visibility [ Just model.form.node.visibility, Nothing ]

                AuthorityAction ->
                    case model.form.node.type_ of
                        NodeType.Circle ->
                            List.member model.form.fragment.mode [ Just model.form.node.mode, Nothing ]

                        NodeType.Role ->
                            List.member model.form.fragment.role_type [ model.form.node.role_type, Nothing ]

                LinkAction ->
                    not (isUsersSendable model.form.users)

                _ ->
                    True
           )
    )
        || isSuccess model.action_result


hasData : Model -> Bool
hasData model =
    -- When you can commit (e.g. empty form data)
    isPostEmpty [ "message" ] model.form.post == False


isSendable : Model -> Bool
isSendable model =
    case model.state of
        VisibilityAction ->
            model.form.node.visibility /= withDefault model.form.node.visibility model.form.fragment.visibility

        AuthorityAction ->
            case model.form.node.type_ of
                NodeType.Circle ->
                    model.form.node.mode /= withDefault model.form.node.mode model.form.fragment.mode

                NodeType.Role ->
                    model.form.node.role_type /= mor model.form.fragment.role_type model.form.node.role_type

        LinkAction ->
            isUsersSendable model.form.users

        _ ->
            True



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = -- Data
      OnOpen_ String String String Node
    | OnOpen String String (GqlData NodesDict) (Maybe ( Int, Int ))
    | OnGetNode (GqlData Node)
    | OnClose
    | OnReset
    | PushAction ActionForm PanelState
    | OnSubmit (Time.Posix -> Msg)
    | OnOpenModal PanelState
    | OnCloseModal ModalData
    | OnCloseModalSafe String String
    | OnChangePost String String
    | OnChangeVisibility NodeVisibility.NodeVisibility
    | OnChangeMode NodeMode.NodeMode
    | OnChangeRoleType RoleType.RoleType
    | OnActionSubmit Time.Posix
    | OnActionMove
      --
      --| ActionStep1 xxx
    | GotTensionToMove (GqlData TensionHead)
    | PushAck (GqlData IdPayload)
      -- Autonomous action (components)
    | DoMove TensionHead
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Common
    | NoMsg
    | LogErr String
    | UpdateUctx UserCtx
    | Navigate String
    | Do (List GlobalCmd)
      -- Components
    | MoveTensionMsg MoveTension.Msg
    | UserInputMsg UserInput.Msg


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe NodeFragment
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


update : Apis -> Msg -> State -> ( State, Out )
update apis message (State model) =
    update_ apis message model
        |> Tuple.mapFirst State


update_ apis message model =
    case message of
        -- Data
        OnOpen_ domid tid bid node ->
            if not model.isOpen then
                ( open domid tid bid node model, noOut )

            else
                ( close model, noOut )

        OnOpen domid nameid tree pos ->
            -- Open panel in HelperBar
            if not model.isOpen then
                let
                    cmds =
                        ternary (isSuccess tree) [] [ Ports.requireTreeData ]
                in
                case getNode nameid tree of
                    Just node ->
                        let
                            ( tid, bid ) =
                                node.source
                                    |> Maybe.map (\b -> ( b.tension.id, b.id ))
                                    |> withDefault
                                        -- Members Roles have no tension attached. They are attached to the root node tension.
                                        (getNode (nid2rootid node.nameid) tree
                                            |> Maybe.map (\n -> n.source)
                                            |> withDefault Nothing
                                            |> Maybe.map (\b -> ( b.tension.id, b.id ))
                                            |> withDefault ( "", "" )
                                        )
                        in
                        ( { model | pos = pos, targetid = nameid, role_type = node.role_type }, out0 (send (OnOpen_ domid tid bid node) :: cmds) )

                    Nothing ->
                        ( { model | pos = pos, targetid = nameid, domid = domid }, out0 (fetchNode2 apis nameid OnGetNode :: cmds) )

            else
                ( close model, noOut )

        OnGetNode result ->
            case result of
                Success node ->
                    let
                        ( tid, bid ) =
                            node.source
                                |> Maybe.map (\b -> ( b.tension.id, b.id ))
                                |> withDefault
                                    (node.parent
                                        |> Maybe.map (\n -> n.source)
                                        |> withDefault Nothing
                                        |> Maybe.map (\b -> ( b.tension.id, b.id ))
                                        |> withDefault ( "", "" )
                                    )
                    in
                    ( { model | role_type = node.role_type }, out0 [ send (OnOpen_ model.domid tid bid node) ] )

                _ ->
                    ( model, noOut )

        OnClose ->
            ( close model, noOut )

        OnReset ->
            ( reset model, noOut )

        PushAction form state ->
            case state of
                LinkAction ->
                    if isSelfContract model.form.uctx model.form.users then
                        ( model, out0 [ actionRequest apis form PushAck ] )

                    else
                        let
                            contractForms =
                                makeCandidateContractForm form
                        in
                        ( model, out0 (List.map (\c -> addOneContract apis c PushAck) contractForms) )

                _ ->
                    ( model, out0 [ actionRequest apis form PushAck ] )

        OnSubmit next ->
            ( model, out0 [ sendNow next ] )

        OnOpenModal action ->
            let
                newModel =
                    model
                        |> openModal
                        |> setAction action
                        |> setStep StepOne
                        |> close

                cmds =
                    case action of
                        LinkAction ->
                            [ Cmd.map UserInputMsg (send (UserInput.OnLoad False)) ]

                        _ ->
                            []
            in
            ( newModel, out0 (Ports.open_modal ("actionPanelModal" ++ model.domid) :: cmds) )

        OnCloseModal data ->
            let
                cmds =
                    ternary data.reset
                        [ sendSleep OnReset 333
                        , case model.state of
                            LinkAction ->
                                Cmd.map UserInputMsg (send UserInput.OnReset)

                            UnLinkAction _ ->
                                Cmd.map UserInputMsg (send UserInput.OnReset)

                            _ ->
                                send NoMsg
                        ]
                        []

                gcmds =
                    if data.link /= "" then
                        [ DoNavigate data.link ]

                    else if isSuccess model.action_result then
                        case model.state of
                            MoveAction ->
                                []

                            VisibilityAction ->
                                let
                                    visibility =
                                        withDefault model.form.node.visibility model.form.fragment.visibility
                                in
                                [ DoUpdateNode model.form.node.nameid (\n -> { n | visibility = visibility }) ]

                            AuthorityAction ->
                                case model.form.node.type_ of
                                    NodeType.Circle ->
                                        let
                                            mode =
                                                withDefault model.form.node.mode model.form.fragment.mode
                                        in
                                        [ DoUpdateNode model.form.node.nameid (\n -> { n | mode = mode }) ]

                                    NodeType.Role ->
                                        let
                                            role_type =
                                                mor model.form.fragment.role_type model.form.node.role_type
                                        in
                                        [ DoUpdateNode model.form.node.nameid (\n -> { n | role_type = role_type }) ]

                            LinkAction ->
                                if isSelfContract model.form.uctx model.form.users then
                                    let
                                        fs =
                                            model.form.users |> List.head |> Maybe.map (\u -> { username = u.username, name = u.name })
                                    in
                                    [ DoUpdateNode model.form.node.nameid (\n -> { n | first_link = fs }) ]

                                else
                                    --Contract based event...
                                    []

                            UnLinkAction _ ->
                                [ DoUpdateNode model.form.node.nameid (\n -> { n | first_link = Nothing }) ]

                            ArchiveAction ->
                                [ DoDelNodes [ model.form.node.nameid ] ]

                            UnarchiveAction ->
                                []

                            LeaveAction ->
                                -- Ignore Guest deletion (either non visible or very small)
                                [ DoUpdateNode model.form.node.nameid (\n -> { n | first_link = Nothing })

                                --, DoFocus (nearestCircleid model.form.node.nameid)
                                ]

                    else
                        []
            in
            ( closeModal model
              -- Ports.click: unock the tooltip if click from the tooltip, else avoid id as the click will move to the parent node
            , Out
                (cmds ++ [ Ports.close_modal, ternary (model.domid == "actionPanelContentTooltip") (Ports.click "canvasOrga") Cmd.none ])
                gcmds
                (ternary (isSuccess model.action_result)
                    (Just model.form.fragment)
                    Nothing
                )
            )

        OnChangePost field value ->
            ( updatePost field value model, noOut )

        OnChangeVisibility visibility ->
            ( setFragment (\frag -> { frag | visibility = Just visibility }) model, noOut )

        OnChangeMode mode ->
            ( setFragment (\frag -> { frag | mode = Just mode }) model, noOut )

        OnChangeRoleType role_type ->
            ( setFragment (\frag -> { frag | role_type = Just role_type }) model, noOut )

        OnActionSubmit time ->
            let
                data =
                    model
                        -- This is where the tension event and old/new data is set
                        -- which will actually propagate mutation to the node if
                        -- user pass the @auth process.
                        |> setActionForm
                        |> setActionResult LoadingSlowly
                        |> updatePost "createdAt" (fromTime time)
            in
            ( data, out0 [ send (PushAction data.form data.state) ] )

        PushAck result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( setActionResult NotAsked model
                    , out0 [ Ports.raiseAuthModal model.form.uctx ]
                    )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, out2 [ sendSleep (PushAction model.form model.state) 500 ] [ DoUpdateToken ] )

                OkAuth _ ->
                    ( model |> setActionResult result, noOut )

                DuplicateErr ->
                    ( setActionResult (Failure [ T.duplicateContractError ]) model, noOut )

                _ ->
                    -- Update the token on failure (e.g. secret circle leave will raise an error while operation processed.
                    ( setActionResult result model, ternary (isFailure result) (out1 [ DoUpdateToken ]) noOut )

        OnActionMove ->
            ( model, out0 [ getTensionHead apis model.form.uctx model.form.tid GotTensionToMove ] )

        GotTensionToMove result ->
            case result of
                Success th ->
                    ( model, out0 [ send (DoMove th) ] )

                _ ->
                    ( model, noOut )

        DoMove t ->
            ( close model, out0 [ Cmd.map MoveTensionMsg (send (MoveTension.OnOpen t.id t.receiver.nameid (blobFromTensionHead t))) ] )

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
            let
                form =
                    model.form
            in
            ( { model | user = LoggedIn uctx, form = { form | uctx = uctx } }, noOut )

        Navigate link ->
            ( model, out1 [ DoNavigate link ] )

        Do gcmds ->
            ( close model, out1 gcmds )

        OnCloseModalSafe link onCloseTxt ->
            if canExitSafe model then
                ( model, out0 [ send (OnCloseModal { reset = True, link = link }) ] )

            else
                ( model
                , out0 [ send (DoModalConfirmOpen (OnCloseModal { reset = True, link = link }) { message = Nothing, txts = [ ( T.confirmUnsaved, onCloseTxt ) ] }) ]
                )

        -- Components
        MoveTensionMsg msg ->
            let
                ( data, out ) =
                    MoveTension.update apis msg model.moveTension

                ( cmds, gcmds ) =
                    out.result
                        |> Maybe.map
                            (\( closing, d ) ->
                                if closing && d /= Nothing then
                                    let
                                        ( nameid, parentid_new, nameid_new ) =
                                            Maybe.map Tuple.second d |> withDefault ( "", "", "" )
                                    in
                                    ( [], [ DoMoveNode nameid parentid_new nameid_new ] )

                                else if closing && (model.domid == "actionPanelContentTooltip") then
                                    ( [ Ports.click "canvasOrga" ], [] )

                                else
                                    ( [], [] )
                            )
                        |> withDefault ( [], [] )
            in
            ( { model | moveTension = data }, out2 (List.map (\m -> Cmd.map MoveTensionMsg m) out.cmds |> List.append cmds) (out.gcmds ++ gcmds) )

        UserInputMsg msg ->
            let
                ( data, out ) =
                    UserInput.update apis msg model.userInput

                users =
                    out.result
                        |> Maybe.map (\( selected, u ) -> ternary selected u [])
                        |> withDefault model.form.users

                ( cmds, gcmds ) =
                    ( [], [] )

                form =
                    model.form
            in
            ( { model | userInput = data, form = { form | users = users } }, out2 (List.map (\m -> Cmd.map UserInputMsg m) out.cmds |> List.append cmds) (out.gcmds ++ gcmds) )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    [ --Ports.mcPD Ports.closeActionPanelModalFromJs LogErr OnCloseModal
      Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    , Ports.uctxPD Ports.loadUserCtxFromJs LogErr UpdateUctx
    ]
        ++ (MoveTension.subscriptions |> List.map (\s -> Sub.map MoveTensionMsg s))
        ++ (UserInput.subscriptions |> List.map (\s -> Sub.map UserInputMsg s))
        ++ (if model.isOpen then
                [ Events.onMouseUp (Dom.outsideClickClose model.domid OnClose)
                , Events.onKeyUp (Dom.key "Escape" OnClose)
                ]

            else if model.isModalActive then
                [ Events.onKeyUp (Dom.key "Escape" (OnCloseModal { reset = False, link = "" }))
                ]

            else
                []
           )



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    { tc : TensionCharac
    , isRight : Bool -- view option
    , domid : String
    , tree_data : GqlData NodesDict
    }


view : Op -> State -> Html Msg
view op (State model) =
    if model.domid == op.domid then
        div []
            [ if model.isOpen then
                viewPanel op model

              else
                text ""
            , viewModal op model
            , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
            , MoveTension.view { tree_data = op.tree_data } model.moveTension |> Html.map MoveTensionMsg
            ]

    else
        text ""


viewPanel : Op -> Model -> Html Msg
viewPanel op model =
    let
        pos =
            case model.pos of
                Just ( a, b ) ->
                    attribute "style"
                        ("position:absolute; left:"
                            ++ String.fromInt (a + 50)
                            ++ "px;"
                            ++ "top:"
                            ++ String.fromInt (b + 25)
                            ++ "px;"
                        )

                Nothing ->
                    attribute "style" ""

        hasRole =
            playsRole model.form.uctx model.targetid

        isOwner_ =
            isOwner model.form.uctx model.targetid

        ownerRole =
            model.role_type == Just RoleType.Owner

        isAdmin =
            isOwner_
                || (List.length (getNodeRights model.form.uctx model.form.node op.tree_data) > 0 && not ownerRole)

        isBaseMember_ =
            isBaseMember model.targetid

        isCircle =
            model.role_type == Nothing
    in
    div [ class "actionPanelStyle", pos ]
        [ div
            [ class "dropdown-content"
            , classList [ ( "is-right", op.isRight ) ]
            ]
          <|
            (-- SHORTCUT ACTION
             if not (List.member model.form.node.role_type [ Just RoleType.Guest, Just RoleType.Owner ]) then
                [ -- View Action
                  div
                    [ class "dropdown-item button-light"
                    , onClick (Navigate (uriFromNameid OverviewBaseUri model.form.node.nameid []))
                    ]
                    [ A.icon1 "" T.view ]
                , -- Edit Action
                  div
                    [ class "dropdown-item button-light"
                    , onClick
                        (Navigate
                            (Route.Tension_Dynamic_Dynamic_Action { param1 = nid2rootid model.form.node.nameid, param2 = model.form.tid } |> toHref)
                        )
                    ]
                    [ A.icon1 "icon-edit-2" T.edit ]
                ]

             else
                []
            )
                ++ (-- ADD ACTION
                    if model.form.node.role_type /= Just RoleType.Guest then
                        [ div
                            [ class "dropdown-item button-light", onClick (Do [ DoCreateTension model.form.node.nameid ]) ]
                            [ A.icon1 "icon-plus" (T.add ++ "...") ]
                        ]

                    else
                        []
                   )
                -- ACTION
                ++ (if isAdmin && not isBaseMember_ then
                        let
                            isRoot =
                                nid2rootid model.form.node.nameid == model.form.node.nameid
                        in
                        [ hr [ class "dropdown-divider" ] []

                        -- Move Action
                        , if not isRoot then
                            div [ class "dropdown-item button-light", onClick OnActionMove ]
                                [ span [ class "arrow-right2 pl-0 pr-3" ] [], text (panelAction2str MoveAction) ]

                          else
                            text ""

                        -- Authority Action
                        , div [ class "dropdown-item button-light", onClick (OnOpenModal AuthorityAction) ]
                            [ A.icon1 (auth2icon op.tc) (auth2str op.tc) ]
                        , case model.form.node.type_ of
                            -- Visibility Action
                            NodeType.Circle ->
                                div [ class "dropdown-item button-light", onClick (OnOpenModal VisibilityAction) ]
                                    [ A.icon1 "icon-lock" (panelAction2str VisibilityAction) ]

                            NodeType.Role ->
                                text ""
                        ]
                            ++ -- ARCHIVE ACTION
                               (if not isRoot then
                                    [ case op.tc.action_type of
                                        EDIT ->
                                            div [ class "dropdown-item button-light is-warning", onClick (OnOpenModal ArchiveAction) ]
                                                [ A.icon1 "icon-archive" (panelAction2str ArchiveAction) ]

                                        ARCHIVE ->
                                            div [ class "dropdown-item button-light", onClick (OnOpenModal UnarchiveAction) ]
                                                [ A.icon1 "icon-archive" (panelAction2str UnarchiveAction) ]

                                        NEW ->
                                            div [] [ text T.notImplemented ]
                                    ]

                                else
                                    []
                               )
                            |> (\l ->
                                    if isCircle then
                                        l

                                    else
                                        l ++ [ hr [ class "dropdown-divider" ] [] ]
                               )

                    else
                        []
                   )
                -- LINK/LEAVE ACTION
                ++ (if hasRole && not isCircle then
                        [ div [ class "dropdown-item button-light is-danger", onClick (OnOpenModal LeaveAction) ]
                            [ if isBaseMember_ then
                                A.icon1 "icon-log-out" T.leaveOrga

                              else
                                A.icon1 "icon-log-out" (panelAction2str LeaveAction)
                            ]
                        ]

                    else if isAdmin && not isBaseMember_ && not isCircle then
                        -- Link/Unlink Action
                        [ case model.form.node.first_link of
                            Just user ->
                                div [ class "dropdown-item button-light is-danger", onClick (OnOpenModal (UnLinkAction user)) ]
                                    [ A.icon1 "icon-user-x" (panelAction2str (UnLinkAction user)) ]

                            Nothing ->
                                div [ class "dropdown-item button-light is-success", onClick (OnOpenModal LinkAction) ]
                                    [ A.icon1 "icon-user-plus" (panelAction2str LinkAction) ]
                        ]

                    else if isAdmin && (isBaseMember_ && not ownerRole) && not isCircle then
                        --  Remove User (Assume Guest)
                        let
                            user =
                                userFromBaseMember model.targetid |> withDefault "" |> (\u -> { username = u, name = Nothing })
                        in
                        [ div [ class "dropdown-item button-light is-danger", onClick (OnOpenModal (UnLinkAction user)) ]
                            [ A.icon1 "icon-user-plus" T.removeUser ]
                        ]

                    else
                        []
                   )
        ]


viewModal : Op -> Model -> Html Msg
viewModal op model =
    div
        [ id ("actionPanelModal" ++ model.domid)
        , class "modal is-light modal-fx-fadeIn"
        , classList [ ( "is-active", model.isModalActive ) ]

        --, attribute "data-modal-close" "closeActionPanelModalFromJs"
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" ("actionPanelModal" ++ model.domid)
            , onClick (OnCloseModalSafe "" "")
            ]
            []
        , div [ class "modal-content" ] [ viewModalContent op model ]

        --, button [ class "modal-close is-large", onClick (OnCloseModalSafe "" "") ] []
        ]


viewModalContent : Op -> Model -> Html Msg
viewModalContent op model =
    case model.step of
        StepOne ->
            viewStep1 op model

        StepAck data ->
            let
                selfContract =
                    isSelfContract model.form.uctx model.form.users
            in
            div
                [ class "notification is-success-light" ]
                [ button [ class "delete", onClick (OnCloseModalSafe "" "") ] []
                , A.icon1 "icon-check icon-2x has-text-success" " "
                , text (action2post model.state selfContract ++ ". ")
                , if model.state == LinkAction && not selfContract then
                    let
                        link =
                            Route.Tension_Dynamic_Dynamic_Contract_Dynamic { param1 = nid2rootid model.form.node.nameid, param2 = model.form.tid, param3 = data.id } |> toHref
                    in
                    a
                        [ href link
                        , onClickPD (OnCloseModal { reset = True, link = link })
                        , target "_blank"
                        ]
                        [ text T.consult ]

                  else
                    text ""
                ]



--- Viewer


viewStep1 : Op -> Model -> Html Msg
viewStep1 op model =
    let
        color =
            action2color model.state

        isLoading =
            model.action_result == LoadingSlowly
    in
    div [ class "modal-card" ]
        [ div [ class ("modal-card-head has-background-" ++ color) ]
            [ div [ class "modal-card-title is-wrapped is-size-6 has-text-grey-dark has-text-weight-semibold" ]
                [ action2header model.state model.form.node.type_
                    |> Format.namedValue "type" (NodeType.toString model.form.node.type_)
                    |> text
                    |> List.singleton
                    |> span []
                , text ":"
                , span [ class "has-text-primary ml-2" ] [ text model.form.node.name ]

                --, button [ class "delete is-pulled-right", onClick (OnCloseModalSafe "" "") ] []
                ]
            ]
        , div [ class "modal-card-body" ] <|
            case model.state of
                MoveAction ->
                    []

                VisibilityAction ->
                    [ viewVisibility op model ]

                AuthorityAction ->
                    case model.form.node.type_ of
                        NodeType.Circle ->
                            [ viewCircleAuthority op model ]

                        NodeType.Role ->
                            [ viewRoleAuthority op model ]

                LinkAction ->
                    [ UserInput.view { label_text = text (T.inviteOrLink ++ ":") } model.userInput |> Html.map UserInputMsg
                    , viewComment model
                    ]

                UnLinkAction user ->
                    [ div [ class "mb-5" ] [ text T.confirmToUnlinkUser, text ": ", viewUserFull 2 True True user ]
                    , viewComment model
                    ]

                LeaveAction ->
                    [ if List.length (getOrgaRoles [ model.form.node.nameid ] (uctxFromUser model.user).roles) == 1 then
                        showMsg "leaveMe" "is-warning is-light" "icon-alert-triangle" T.confirmLeaveOrga ""

                      else
                        text ""
                    , viewComment model
                    ]

                ArchiveAction ->
                    [ viewComment model ]

                UnarchiveAction ->
                    [ viewComment model ]
        , div [ class "modal-card-foot", attribute "style" "display: block;" ]
            [ case model.action_result of
                Failure err ->
                    div [ class "field" ] [ viewGqlErrors err ]

                _ ->
                    text ""
            , div [ class "field level is-mobile" ]
                [ div [ class "level-left" ]
                    [ button
                        [ class "button is-light"
                        , onClick (OnCloseModalSafe "" "")
                        ]
                        [ text T.cancel ]
                    ]
                , div [ class "level-right" ]
                    [ button
                        [ class ("button defaultSubmit is-light is-" ++ color)
                        , classList [ ( "is-loading", isLoading ) ]
                        , disabled (not (isSendable model) || isLoading)
                        , onClick (OnSubmit OnActionSubmit)
                        ]
                        [ action2submitstr model.state (isSelfContract model.form.uctx model.form.users) |> text ]
                    ]
                ]
            ]
        ]


viewComment : Model -> Html Msg
viewComment model =
    let
        message =
            Dict.get "message" model.form.post |> withDefault ""

        line_len =
            List.length <| String.lines message

        ( max_len, min_len ) =
            if isMobile model.screen then
                ( 5, 2 )

            else
                ( 10, 3 )
    in
    div [ class "field" ]
        [ div [ class "control submitFocus" ]
            [ textarea
                [ class "textarea"
                , rows (min max_len (max line_len min_len))
                , placeholder T.leaveCommentOpt
                , value message
                , onInput <| OnChangePost "message"
                ]
                []
            ]
        , if model.state == LinkAction then
            p [ class "help-label" ] [ text T.invitationMessageHelp ]

          else
            p [ class "help-label" ] [ text T.tensionMessageHelp ]
        ]



-- Specific Viewer


viewVisibility : Op -> Model -> Html Msg
viewVisibility op model =
    let
        isRoot =
            nid2rootid model.form.node.nameid == model.form.node.nameid
    in
    div []
        [ -- Show the help information
          showMsg "visibility-0" "is-info is-light" "icon-info" T.visibilityInfoHeader ""

        -- Show the choices as card.
        , NodeVisibility.list
            |> List.filter (\v -> not (v == NodeVisibility.Secret && isRoot))
            |> List.map
                (\x ->
                    let
                        isActive =
                            x == withDefault model.form.node.visibility model.form.fragment.visibility

                        extra_descr =
                            if isRoot && x == NodeVisibility.Private then
                                -- Tell user that the organisation setting may be modified
                                " " ++ T.visibilityRestriction

                            else
                                ""
                    in
                    div
                        [ class "card has-border column is-paddingless m-3 is-h"
                        , classList [ ( "is-selected", isActive ) ]
                        , onClick (OnChangeVisibility x)
                        ]
                        [ div [ class "card-content p-4" ]
                            [ h2 [ class "is-strong is-size-5" ]
                                [ A.icon1 (visibility2icon x ++ " icon-bg") (NodeVisibility.toString x) ]
                            , div [ class "content is-small" ]
                                [ text (visibility2descr x), span [ class "help" ] [ text extra_descr ] ]
                            ]
                        ]
                )
            |> div [ class "columns" ]
        ]


viewCircleAuthority : Op -> Model -> Html Msg
viewCircleAuthority op model =
    div []
        [ -- Show the help information
          showMsg "circleAuthority-0" "is-info is-light" "icon-info" T.circleAuthorityHeader T.circleAuthorityDoc

        -- Show the choices as card.
        , NodeMode.list
            |> List.map
                (\x ->
                    let
                        isActive =
                            x == withDefault model.form.node.mode model.form.fragment.mode

                        ( icon, description ) =
                            case x of
                                NodeMode.Coordinated ->
                                    ( "icon-", T.authCoordinated )

                                NodeMode.Agile ->
                                    ( "icon-", T.authAgile )
                    in
                    div
                        [ class "card has-border column is-paddingless m-3 is-h"
                        , classList [ ( "is-selected", isActive ) ]
                        , onClick (OnChangeMode x)
                        ]
                        [ div [ class "card-content p-4" ]
                            [ h2 [ class "is-strong is-size-5" ]
                                [ A.icon1 (icon ++ " icon-bg") (NodeMode.toString x) ]
                            , div [ class "content is-small" ]
                                [ text description ]
                            ]
                        ]
                )
            |> div [ class "columns is-multiline" ]
        ]


viewRoleAuthority : Op -> Model -> Html Msg
viewRoleAuthority op model =
    div []
        [ -- Show the help information
          --showMsg "roleAuthority-0" "is-info is-light" "icon-info" T.roleAuthorityHeader ""
          -- Show the choices as card.
          --RoleType.list
          [ ( RoleType.Peer, T.peerRoleInfo ), ( RoleType.Coordinator, T.coordinatorRoleInfo ) ]
            |> List.map
                (\( x, description ) ->
                    let
                        isActive =
                            Just x == mor model.form.fragment.role_type model.form.node.role_type

                        icon =
                            "icon-user has-text-" ++ roleColor x
                    in
                    div
                        [ class "card has-border column is-paddingless m-3 is-h"
                        , attribute "style" "min-width: 150px;"
                        , classList [ ( "is-selected", isActive ) ]
                        , onClick (OnChangeRoleType x)
                        ]
                        [ div [ class "card-content p-4" ]
                            [ h2 [ class "is-strong is-size-5" ]
                                [ A.icon1 (icon ++ " icon-bg") (RoleType.toString x) ]
                            , div [ class "content is-small" ]
                                [ text description ]
                            ]
                        ]
                )
            |> div [ class "columns is-multiline" ]
        ]
