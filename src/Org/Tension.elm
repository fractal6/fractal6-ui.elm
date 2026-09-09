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


module Org.Tension exposing (Flags, Model, Msg, TensionTab(..), init, page, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), getTensionRights, parseErr)
import Browser.Navigation as Nav
import Codecs exposing (CommentDraft, DraftUpdate(..))
import Components.ActionPanel as ActionPanel
import Components.AuthModal as AuthModal
import Components.Comments as Comments exposing (OutType(..))
import Components.ContractsPage as ContractsPage
import Components.HelperBar as HelperBar
import Components.JoinOrga as JoinOrga
import Components.LabelSearchPanel as LabelSearchPanel exposing (viewLabels)
import Components.MoveTension as MoveTension
import Components.NodeDoc as NodeDoc exposing (NodeDoc, NodeEdit(..), NodeView(..))
import Components.OrgaMenu as OrgaMenu
import Components.ProjectSearchPanel as ProjectSearchPanel
import Components.SelectType as SelectType
import Components.TreeMenu as TreeMenu
import Components.UserSearchPanel as UserSearchPanel exposing (viewUsers)
import Dict
import Form.Help as Help
import Form.NewTension as NTF
import Fractale.Codecs exposing (FocusState, FractalBaseRoute(..), NodeFocus, eor2ur, focusFromNameid, focusFromPath, focusState, getOrgaRoles, getTensionNode, id3Changed, nid2rootid, nodeFromTensionHead, toLink)
import Fractale.Error exposing (viewGqlErrors, viewJoinForCommentNeeded, viewMaybeErrors)
import Fractale.Form exposing (..)
import Fractale.Graph exposing (..)
import Fractale.HotUpdate exposing (..)
import Fractale.User exposing (..)
import Fractale.View exposing (statusColor, tensionIcon2, tensionStatus2str, viewCircleTarget, viewNodeDescr, viewNodeRefShort, viewRole, viewRoleExt, viewTensionDateAndUser, viewUserFull, viewUsernameLink)
import Generated.Route as Route exposing (Route(..), toHref)
import Global exposing (Msg(..))
import Html exposing (Html, a, button, div, h1, h2, hr, i, input, li, nav, p, span, strong, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, spellcheck, style, title, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, RequestResult(..), errorIsNoDataFound, fromMaybeData, isSuccess, loadingSpin, withDefaultData, withMapData, withMaybeData, withMaybeMapData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.PatchTension exposing (patchComment, patchLiteral, publishBlob, pushTensionPatch)
import Query.PatchUser exposing (markAsRead, toggleOrgaWatch, toggleTensionSubscription)
import Query.QueryNode exposing (queryLocalGraph)
import Query.QueryTension exposing (getTensionBlobs, getTensionComments, getTensionHead)
import Query.Reaction exposing (addReaction, deleteReaction)
import Schema.Enum.Lang as Lang
import Schema.Enum.NodeType as NodeType
import Schema.Enum.RoleType as RoleType
import Schema.Enum.TensionEvent as TensionEvent
import Schema.Enum.TensionStatus as TensionStatus
import Schema.Enum.TensionType as TensionType
import Scroll
import Session exposing (CommonMsg, GlobalCmd(..), LabelSearchPanelOnClickAction(..), ProjectSearchPanelOnClickAction(..), SessionCommon, UserSearchPanelOnClickAction(..), ViewMode(..), isMobile)
import String.Extra as SE
import String.Format as Format
import Text as T
import Time
import Url exposing (Url)
import Utils.Bool exposing (ternary)
import Utils.Cmd exposing (send, sendNow, sendSleep)
import Utils.Date exposing (formatDate)
import Utils.DomEvents exposing (onClickSP)
import Utils.Html exposing (showIf)
import Utils.Maybe exposing (unwrap)
import Utils.String exposing (decap)
import Utils.Url exposing (queryParser)



---- PROGRAM ----


type alias Flags =
    { param1 : String
    , param2 : String
    , param3 : TensionTab
    , param4 : Maybe String
    }


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
                    -- Global
                    DoFocus nameid ->
                        ( Cmd.none, send (NavigateNode nameid) )

                    DoNavigate link ->
                        ( Cmd.none, send (NavigateRaw link) )

                    DoReplaceUrl url ->
                        ( Cmd.none, send (ReplaceUrl url) )

                    DoUpdateToken ->
                        ( Cmd.none, send UpdateUserToken )

                    DoUpdateUserSession uctx ->
                        ( Cmd.none, send (UpdateUserSession uctx) )

                    DoUpdatePath path ->
                        ( Cmd.none, send (UpdateSessionPath path) )

                    DoUpdateTree tree ->
                        ( Cmd.none, send (UpdateSessionTree tree) )

                    DoUpdateOrgs orgs ->
                        ( Cmd.none, send (UpdateSessionOrgs orgs) )

                    DoToggleWatchOrga a ->
                        ( Cmd.none, send (ToggleWatchOrga a) )

                    DoPushSystemNotif a ->
                        ( Cmd.none, send (OnPushSystemNotif a) )

                    -- Component
                    DoCreateTension a ntm d ->
                        case ntm of
                            Nothing ->
                                ( Cmd.map NewTensionMsg <| send (NTF.OnOpen (FromNameid a) d), Cmd.none )

                            Just NodeType.Circle ->
                                ( Cmd.map NewTensionMsg <| send (NTF.OnOpenCircle (FromNameid a)), Cmd.none )

                            Just NodeType.Role ->
                                ( Cmd.map NewTensionMsg <| send (NTF.OnOpenRole (FromNameid a)), Cmd.none )

                    DoJoinOrga a ->
                        ( Cmd.map JoinOrgaMsg <| send (JoinOrga.OnOpen a JoinOrga.JoinOne), Cmd.none )

                    DoOpenActionPanel a b c ->
                        ( send <| OpenActionPanel a b c, Cmd.none )

                    DoToggleTreeMenu ->
                        ( Cmd.map TreeMenuMsg <| send TreeMenu.OnToggle, Cmd.none )

                    DoFetchNode nameid ->
                        ( Cmd.map TreeMenuMsg <| sendSleep (TreeMenu.FetchNewNode nameid False) 333, Cmd.none )

                    DoAddNodes nodes ->
                        ( Cmd.map TreeMenuMsg <| send (TreeMenu.AddNodes nodes), Cmd.none )

                    DoUpdateNode nameid fun ->
                        ( Cmd.map TreeMenuMsg <| send (TreeMenu.UpdateNode nameid fun), Cmd.none )

                    DoDelNodes nameids ->
                        ( Cmd.map TreeMenuMsg <| send (TreeMenu.DelNodes nameids), Cmd.none )

                    DoMoveNode a b c ->
                        ( Cmd.map TreeMenuMsg <| send (TreeMenu.MoveNode a b c), Cmd.none )

                    DoUpdateDraft draftUpdate ->
                        ( Cmd.none, send (Global.UpdateDraft draftUpdate) )

                    _ ->
                        ( Cmd.none, Cmd.none )
            )
        |> List.unzip



---- MODEL ----


type alias Model =
    { -- Focus
      node_focus : NodeFocus
    , path_data : GqlData LocalGraph
    , lookup_users : List User

    -- Page
    , focusState : FocusState
    , tensionid : String
    , baseUri : FractalBaseRoute
    , force_reload_path : Bool
    , contractid : Maybe String
    , activeTab : TensionTab
    , nodeView : NodeView
    , jumpTo : Maybe String
    , tension_head : GqlData TensionHead
    , tension_comments : GqlData TensionComments
    , tension_blobs : GqlData TensionBlobs
    , expandedEvents : List Int
    , eid : String
    , subscribe_result : GqlData Bool -- init a GotTensionHead
    , unsubscribe : String
    , unsubscribe_result : GqlData Bool
    , unwatch : String
    , unwatch_result : GqlData Bool

    -- Form
    , tension_form : TensionForm

    -- Title Result
    , isTitleEdit : Bool
    , title_result : GqlData IdPayload

    -- Blob Edit
    , nodeDoc : NodeDoc
    , publish_result : GqlData TensionBlobFlag
    , expandedDiff : String -- blob id expanded in the revisions view ("" = none)

    -- Side Pane
    , isTensionAdmin : Bool
    , isAssigneeOpen : Bool
    , isLabelOpen : Bool

    -- Common
    , refresh_trial : Int
    , session : SessionCommon
    , draftSaveTimer : Int
    , comments : Comments.State
    , empty : {}
    , commonOp : CommonMsg Msg

    -- Components
    , helperBar : HelperBar.State
    , help : Help.State
    , tensionForm : NTF.State
    , assigneesPanel : UserSearchPanel.State
    , labelsPanel : LabelSearchPanel.State
    , projectsPanel : ProjectSearchPanel.State
    , actionPanel : ActionPanel.State
    , moveTension : MoveTension.State
    , contractsPage : ContractsPage.State
    , selectType : SelectType.State
    , joinOrga : JoinOrga.State
    , authModal : AuthModal.State
    , orgaMenu : OrgaMenu.State
    , treeMenu : TreeMenu.State
    }



-- Query parameters


type TensionTab
    = Conversation
    | Document
    | Contracts



---- INIT ----


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        session =
            global.session

        -- Focus
        rootnameid =
            flags.param1 |> Url.percentDecode |> withDefault ""

        tid =
            flags.param2

        tab =
            flags.param3

        cid_m =
            flags.param4

        newFocus_ =
            NodeFocus rootnameid rootnameid NodeType.Circle

        -- What has changed
        baseUri =
            case tab of
                Conversation ->
                    --TensionBaseUri
                    -- better UX to redirect to tensions list when navigating tensions.
                    TensionsBaseUri

                Document ->
                    MandateBaseUri

                Contracts ->
                    ContractsBaseUri

        fs =
            focusState TensionBaseUri session.referer global.url session.common.node_focus newFocus_

        -- Session snapshot shared by the page model and every component init.
        -- Lexicon is dropped on a real org switch so views fall back to defaults
        -- until GotOrgaInfo lands the new org's lexicon.
        sessionCommon =
            freshSessionOnOrgaSwitch fs session.common

        newFocus =
            if fs.orgChange then
                -- This allow TreeMenu to reload in case of orgChange
                newFocus_

            else
                session.common.path_data
                    |> Maybe.map focusFromPath
                    |> withDefault newFocus_

        nodeView =
            Dict.get "v" session.common.query |> withDefault [] |> List.head |> withDefault "" |> NodeDoc.nodeViewDecoder

        path_data =
            ternary fs.orgChange Loading (fromMaybeData session.common.path_data Loading)

        focusid =
            withMaybeData path_data
                |> Maybe.map (\p -> p.focus.nameid)
                |> withDefault newFocus.nameid

        model =
            { node_focus = newFocus
            , lookup_users = []
            , tensionid = tid
            , baseUri = baseUri
            , force_reload_path = global.url.fragment == Just ""
            , contractid = cid_m
            , activeTab = tab
            , nodeView = nodeView
            , jumpTo = Dict.get "goto" session.common.query |> Maybe.map List.head |> withDefault Nothing
            , path_data = path_data
            , tension_head = ternary fs.orgChange Loading (fromMaybeData session.data.tension_head Loading)
            , focusState = fs
            , tension_comments = Loading
            , tension_blobs = Loading
            , expandedEvents = []
            , eid = ""
            , subscribe_result = NotAsked
            , unsubscribe = ""
            , unsubscribe_result = NotAsked
            , unwatch = ""
            , unwatch_result = NotAsked

            -- Form
            , tension_form = initTensionForm sessionCommon.lexicon tid Nothing sessionCommon.user

            -- Title Result
            , isTitleEdit = False
            , title_result = NotAsked

            -- Blob Edit
            , nodeDoc =
                NodeDoc.init sessionCommon.lexicon tid Nothing nodeView sessionCommon.user
                    |> (\x ->
                            case session.data.tension_head of
                                Just th ->
                                    NodeDoc.initBlob sessionCommon.lexicon (th.latest_blob |> Maybe.andThen .node |> withDefault (initNodeFragment Nothing)) x

                                Nothing ->
                                    x
                       )
            , publish_result = NotAsked
            , expandedDiff = ""

            -- Side Pane
            , isTensionAdmin = withDefault False session.isAdmin
            , isAssigneeOpen = False
            , isLabelOpen = False
            , assigneesPanel = UserSearchPanel.init tid AssignUser sessionCommon.user
            , labelsPanel = LabelSearchPanel.init tid AssignLabel sessionCommon.user
            , projectsPanel = ProjectSearchPanel.init tid AssignProject sessionCommon.user

            -- Common
            , session = sessionCommon
            , helperBar = HelperBar.init baseUri global.url.query newFocus sessionCommon
            , help = Help.init sessionCommon
            , tensionForm = NTF.init sessionCommon
            , refresh_trial = 0
            , moveTension = MoveTension.init sessionCommon
            , contractsPage = ContractsPage.init focusid sessionCommon
            , selectType = SelectType.init tid sessionCommon
            , actionPanel = ActionPanel.init sessionCommon
            , empty = {}
            , commonOp = CommonMsg NoMsg LogErr
            , joinOrga = JoinOrga.init newFocus.nameid sessionCommon

            -- Open a signin dialog if contracts are requested
            , authModal = AuthModal.init (Dict.get "puid" sessionCommon.query |> Maybe.map List.head |> withDefault (ternary (baseUri == ContractsBaseUri) (Just "") Nothing)) sessionCommon
            , orgaMenu = OrgaMenu.init newFocus session.data.orga_menu session.data.orgs_data sessionCommon
            , treeMenu = TreeMenu.init baseUri global.url.query newFocus session.data.tree_menu session.data.tree_data sessionCommon
            , draftSaveTimer = 0
            , comments =
                let
                    maybeDraft =
                        Dict.get tid session.data.drafts.comments
                in
                Comments.initWithDraft focusid tid sessionCommon maybeDraft
            }

        refresh =
            Maybe.map (\x -> id3Changed x.id global.url) session.data.tension_head |> withDefault True

        -- Memory optimization
        ( tension_head, hist_cmd ) =
            case model.tension_head of
                Success th ->
                    ( Success { th | history = Nothing }
                    , Cmd.map CommentsMsg (send (Comments.SetHistory (withDefault [] th.history) model.jumpTo))
                    )

                _ ->
                    ( model.tension_head, Cmd.none )
    in
    ( { model
        | subscribe_result = withMapData .isSubscribed model.tension_head
        , tension_head = tension_head
      }
    , Cmd.batch (hist_cmd :: refresh_cmds refresh global model)
    , if fs.menuChange || refresh then
        -- No refresh here because all the focus is not encoded in the tension URL.
        send (UpdateSessionFocus (Just newFocus))

      else
        Cmd.none
    )


refresh_cmds : Bool -> Global.Model -> Model -> List (Cmd Msg)
refresh_cmds refresh global model =
    let
        apis =
            global.session.apis

        query =
            queryParser global.url
    in
    [ if refresh then
        send LoadTensionHead

      else
        Cmd.none
    , if refresh then
        Cmd.map ProjectSearchPanelMsg (send ProjectSearchPanel.OnLoadCards)

      else
        Cmd.none
    , case model.activeTab of
        Conversation ->
            getTensionComments apis model.tensionid GotTensionComments

        Document ->
            if NodeDoc.getNodeView model.nodeDoc == NodeVersions then
                getTensionBlobs apis model.tensionid GotTensionBlobs

            else
                Cmd.none

        Contracts ->
            Cmd.map ContractsPageMsg (send (ContractsPage.OnLoad model.tensionid model.contractid))
    , if model.node_focus.rootnameid == "" && not refresh then
        -- Fix contract with empty rootnameid redirection
        case model.tension_head of
            Success th ->
                Cmd.map ContractsPageMsg (send (ContractsPage.SetRootnameid (nid2rootid th.receiver.nameid)))

            _ ->
                Cmd.none

      else
        Cmd.none
    , sendSleep PassedSlowLoadTreshold 500
    , case Dict.get "eid" query |> Maybe.map List.head |> withDefault Nothing of
        Just eid ->
            send (MarkAsRead eid)

        Nothing ->
            Cmd.none
    , case Dict.get "unsubscribe" query |> Maybe.map List.head |> withDefault Nothing of
        Just "email" ->
            send (DoUnsubscribe "email")

        _ ->
            Cmd.none
    , case Dict.get "unwatch" query |> Maybe.map List.head |> withDefault Nothing of
        Just "email" ->
            send (DoUnwatch "email")

        _ ->
            Cmd.none
    , Cmd.map AuthModalMsg (send AuthModal.OnStart)
    , Cmd.map OrgaMenuMsg (send OrgaMenu.OnLoad)
    , Cmd.map TreeMenuMsg (send TreeMenu.OnLoad)
    ]



---- MSG ----


type Msg
    = -- Loading
      PassedSlowLoadTreshold -- timer
    | LoadTensionHead
    | PushTitle
    | PushBlob_ TensionForm
    | PublishBlob
    | Submit Bool (Time.Posix -> Msg) -- Get Current Time
      -- Data Queries
    | GotPath Bool (GqlData LocalGraph)
    | GotTensionHead (GqlData TensionHead)
    | GotIsSubscribe (GqlData Bool)
    | GotTensionComments (GqlData TensionComments)
    | GotTensionBlobs (GqlData TensionBlobs)
    | MarkAsRead String
    | GotMarkAsRead (GqlData IdPayload)
    | ToggleSubscription String
      -- Unsubscribe from url
    | DoUnsubscribe String
    | GotUnsubscribe (GqlData Bool)
    | OnCloseUnsubscribe
      -- @TODO: move this to global (e.g ToggleWatchOrga)
    | DoUnwatch String
    | GotUnwatch (GqlData Bool)
    | OnCloseUnwatch
      -- Pin/Unpin
    | PinTension Time.Posix
    | UnpinTension Time.Posix
    | PinAck (GqlData IdPayload)
      --
      -- Page Action
      --
    | ExpandEvent Int
      -- Edit title
    | DoChangeTitle
    | ChangePost String String
    | CancelTitle
    | SubmitTitle Time.Posix
    | TitleAck (GqlData IdPayload)
      -- Blob edit
    | OnToggleDiff String
    | ChangeBlobEdit NodeEdit
    | ChangeBlobPost String String
    | AddDomains
    | AddPolicies
    | AddResponsabilities
      -- Blob Submit
    | CommitBlob NodeDoc Time.Posix
    | BlobAck (GqlData PatchTensionPayloadID)
    | PushBlob String Time.Posix
    | PushBlobAck (GqlData TensionBlobFlag)
    | CancelBlob
      -- Assignees
    | DoAssigneeEdit
      -- Labels
    | DoLabelEdit
      -- Projects
    | DoProjectEdit
      -- move tension
    | DoMove TensionHead
      -- Node Action
    | OpenActionPanel String String (Maybe ( Int, Int ))
      -- Common
    | NoMsg
    | LogErr String
    | ScrollToElement String
    | UpdateUctx UserCtx
      -- Draft persistence
    | SaveDraftDelayed Int
      -- Components
    | HelperBarMsg HelperBar.Msg
    | HelpMsg Help.Msg
    | NewTensionMsg NTF.Msg
    | UserSearchPanelMsg UserSearchPanel.Msg
    | LabelSearchPanelMsg LabelSearchPanel.Msg
    | ProjectSearchPanelMsg ProjectSearchPanel.Msg
    | MoveTensionMsg MoveTension.Msg
    | ContractsPageMsg ContractsPage.Msg
    | SelectTypeMsg SelectType.Msg
    | ActionPanelMsg ActionPanel.Msg
    | JoinOrgaMsg JoinOrga.Msg
    | AuthModalMsg AuthModal.Msg
    | OrgaMenuMsg OrgaMenu.Msg
    | TreeMenuMsg TreeMenu.Msg
    | CommentsMsg Comments.Msg



--- UPDATE ----


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global message model =
    let
        apis =
            global.session.apis
    in
    case message of
        LoadTensionHead ->
            let
                ( uctx, cmd ) =
                    case global.session.common.user of
                        LoggedIn uctx_ ->
                            ( uctx_
                              -- Now directly loaded in TensionLoad query
                              --, getIsSubscribe apis uctx_.username model.tensionid GotIsSubscribe
                            , Cmd.none
                            )

                        LoggedOut ->
                            ( initUserctx, Cmd.none )
            in
            ( model, Cmd.batch [ getTensionHead apis uctx model.tensionid GotTensionHead, cmd ], Cmd.none )

        PushTitle ->
            ( model, patchLiteral apis model.tension_form TitleAck, Cmd.none )

        PushBlob_ form ->
            ( model, pushTensionPatch apis form BlobAck, Cmd.none )

        PublishBlob ->
            let
                form =
                    model.tension_form

                bid =
                    form.events
                        |> List.filter (\x -> x.event_type == TensionEvent.BlobPushed)
                        |> List.head
                        |> Maybe.map .new
                        |> withDefault ""
            in
            ( model, publishBlob apis bid form PushBlobAck, Cmd.none )

        PassedSlowLoadTreshold ->
            let
                tension_h =
                    ternary (model.tension_head == Loading) LoadingSlowly model.tension_head

                tension_c =
                    ternary (model.tension_comments == Loading) LoadingSlowly model.tension_comments

                tension_b =
                    ternary (model.tension_blobs == Loading) LoadingSlowly model.tension_blobs
            in
            ( { model | tension_head = tension_h, tension_comments = tension_c, tension_blobs = tension_b }, Cmd.none, Cmd.none )

        Submit isSendable nextMsg ->
            if isSendable then
                ( model, sendNow nextMsg, Cmd.none )

            else
                ( model, Cmd.none, Cmd.none )

        -- Data queries
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

                                isAdmin =
                                    getTensionRights (uctxFromUser global.session.common.user) model.tension_head result

                                newFocus =
                                    focusFromNameid newPath.focus.nameid
                            in
                            ( { model | path_data = Success newPath, isTensionAdmin = isAdmin, node_focus = newFocus }
                            , Cmd.batch
                                [ Maybe.map (\did -> send (ScrollToElement did)) model.jumpTo |> withDefault Cmd.none
                                , Cmd.map TreeMenuMsg (send (TreeMenu.OnUpdateFocus newFocus))
                                , Cmd.map OrgaMenuMsg (send (OrgaMenu.OnUpdateFocus newFocus))
                                ]
                            , Cmd.batch
                                [ send (UpdateSessionPath (Just newPath))
                                , send (UpdateSessionAdmin (Just isAdmin))
                                , send (UpdateSessionFocusOnly (Just newFocus))
                                ]
                            )

                        Nothing ->
                            let
                                newPath =
                                    { prevPath | path = path.path ++ (List.tail prevPath.path |> withDefault []) }

                                nameid =
                                    List.head path.path |> Maybe.map .nameid |> withDefault ""
                            in
                            ( { model | path_data = Success newPath }
                            , queryLocalGraph apis nameid False (GotPath False)
                            , Cmd.none
                            )

                _ ->
                    ( { model | path_data = result }, Cmd.none, Cmd.none )

        -- Page
        GotTensionHead result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( model, Ports.raiseAuthModal (uctxFromUser global.session.common.user), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep LoadTensionHead 500, send UpdateUserToken )

                OkAuth th ->
                    let
                        ( targetid, nodeDoc ) =
                            case getTensionNode th of
                                Just _ ->
                                    let
                                        node =
                                            th.latest_blob |> Maybe.andThen .node |> withDefault (initNodeFragment Nothing)
                                    in
                                    ( th.governed_node |> Maybe.map .nameid |> withDefault th.receiver.nameid
                                    , NodeDoc.initBlob model.session.lexicon node model.nodeDoc
                                    )

                                Nothing ->
                                    ( th.receiver.nameid, model.nodeDoc )

                        hasLocalGraph =
                            isSuccess model.path_data && not model.focusState.orgChange && not model.force_reload_path

                        focusid =
                            if model.force_reload_path then
                                targetid

                            else
                                nid2rootid targetid

                        isAdmin =
                            getTensionRights (uctxFromUser global.session.common.user) result model.path_data
                    in
                    ( { model
                        -- Memory Optimization: Do no store history twice.
                        | tension_head = Success { th | history = Nothing }
                        , subscribe_result = fromMaybeData (Just th.isSubscribed) model.subscribe_result
                        , nodeDoc = nodeDoc
                        , isTensionAdmin = ternary hasLocalGraph isAdmin model.isTensionAdmin
                      }
                    , Cmd.batch
                        [ ternary hasLocalGraph
                            -- Do not change the context of the user anonymously, its confusing
                            (Maybe.map (\did -> send (ScrollToElement did)) model.jumpTo |> withDefault Cmd.none)
                            (queryLocalGraph apis focusid True (GotPath True))
                        , Ports.bulma_driver ""
                        , Cmd.map ContractsPageMsg (send (ContractsPage.SetRootnameid (nid2rootid targetid)))
                        , Cmd.map CommentsMsg (send (Comments.SetHistory (withDefault [] th.history) model.jumpTo))
                        ]
                    , Cmd.batch
                        [ send (UpdateSessionTensionHead (withMaybeData result))
                        , ternary hasLocalGraph (send (UpdateSessionAdmin (Just isAdmin))) Cmd.none
                        ]
                    )

                _ ->
                    ( { model | tension_head = result }, Cmd.none, send (UpdateSessionTensionHead (withMaybeData result)) )

        GotIsSubscribe result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( model, Ports.raiseAuthModal (uctxFromUser global.session.common.user), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep LoadTensionHead 500, send UpdateUserToken )

                OkAuth d ->
                    let
                        th =
                            withMapData (\x -> { x | isSubscribed = d }) model.tension_head
                    in
                    ( { model | subscribe_result = result, tension_head = th }, Cmd.none, send (UpdateSessionTensionHead (withMaybeData th)) )

                _ ->
                    ( { model | subscribe_result = result }, Cmd.none, Cmd.none )

        GotTensionComments result ->
            case result of
                Success d ->
                    -- Memory Optimization: Do no store comments twice.
                    ( { model | tension_comments = Success { d | comments = Nothing } }
                    , Cmd.map CommentsMsg (send <| Comments.SetComments (withDefault [] d.comments))
                    , Cmd.none
                    )

                _ ->
                    ( { model | tension_comments = result }, Cmd.none, Cmd.none )

        GotTensionBlobs result ->
            ( { model | tension_blobs = result }, Cmd.none, Ports.bulma_driver "" )

        ExpandEvent i ->
            -- @fix/bulma: dropdown clidk handler lost during the operation
            ( { model | expandedEvents = model.expandedEvents ++ [ i ] }, Cmd.none, Ports.bulma_driver "" )

        MarkAsRead eid ->
            ( { model | eid = eid }, markAsRead apis eid True GotMarkAsRead, Cmd.none )

        GotMarkAsRead result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( model, Ports.raiseAuthModal (uctxFromUser global.session.common.user), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep (MarkAsRead model.eid) 500, send UpdateUserToken )

                OkAuth _ ->
                    ( model, Cmd.none, send RefreshNotifCount )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        ToggleSubscription username ->
            case model.tension_head of
                Success th ->
                    ( { model | subscribe_result = LoadingSlowly, unsubscribe = "" }
                    , toggleTensionSubscription apis username model.tensionid (not th.isSubscribed) GotIsSubscribe
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        DoUnsubscribe name ->
            case global.session.common.user of
                LoggedIn uctx ->
                    ( { model | unsubscribe = name }
                    , toggleTensionSubscription apis uctx.username model.tensionid False GotUnsubscribe
                    , Cmd.none
                    )

                LoggedOut ->
                    ( model, Ports.raiseAuthModal (uctxFromUser global.session.common.user), Cmd.none )

        OnCloseUnsubscribe ->
            ( { model | unsubscribe = "" }, Cmd.none, Cmd.none )

        GotUnsubscribe result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( model, Ports.raiseAuthModal (uctxFromUser global.session.common.user), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep (DoUnsubscribe model.unwatch) 500, send UpdateUserToken )

                OkAuth d ->
                    ( { model | unsubscribe_result = result }, sendSleep OnCloseUnsubscribe 5000, Cmd.none )

                _ ->
                    ( { model | unsubscribe_result = result }, Cmd.none, Cmd.none )

        DoUnwatch name ->
            case global.session.common.user of
                LoggedIn uctx ->
                    ( { model | unwatch = name }
                    , toggleOrgaWatch apis uctx.username model.node_focus.rootnameid False GotUnwatch
                    , Cmd.none
                    )

                LoggedOut ->
                    ( model, Ports.raiseAuthModal (uctxFromUser global.session.common.user), Cmd.none )

        OnCloseUnwatch ->
            ( { model | unwatch = "" }, Cmd.none, Cmd.none )

        GotUnwatch result ->
            -- @DEBUG/FIX: remove this, and use the global message ToggleWatchOrga instead:
            -- * NEED: push user notifications to inform the success of the operation (do this for tension unsubscribe also).
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( model, Ports.raiseAuthModal (uctxFromUser global.session.common.user), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep (DoUnwatch model.unwatch) 500, send UpdateUserToken )

                OkAuth d ->
                    ( { model | unwatch_result = result }, sendSleep OnCloseUnwatch 5000, send (GotIsWatching result) )

                _ ->
                    ( { model | unwatch_result = result }, Cmd.none, Cmd.none )

        -- Pin/Unpin
        PinTension time ->
            let
                form =
                    model.tension_form

                newForm =
                    { form
                        | post = Dict.insert "createdAt" (fromTime time) form.post
                        , events = [ Ev TensionEvent.Pinned "" "" ]
                    }
            in
            ( { model | tension_form = newForm }
            , patchLiteral apis newForm PinAck
            , Cmd.none
            )

        UnpinTension time ->
            let
                form =
                    model.tension_form

                newForm =
                    { form
                        | post = Dict.insert "createdAt" (fromTime time) form.post
                        , events = [ Ev TensionEvent.Unpinned "" "" ]
                    }
            in
            ( { model | tension_form = newForm }
            , patchLiteral apis newForm PinAck
            , Cmd.none
            )

        PinAck result ->
            case parseErr result 2 of
                Authenticate ->
                    ( model, Ports.raiseAuthModal (uctxFromUser global.session.common.user), Cmd.none )

                OkAuth d ->
                    let
                        v =
                            model.tension_form.events |> List.any (\x -> x.event_type == TensionEvent.Pinned)

                        th =
                            withMapData (\x -> { x | isPinned = v }) model.tension_head
                    in
                    ( { model | tension_head = th, tension_form = initTensionForm global.session.common.lexicon model.tensionid Nothing global.session.common.user }
                    , Cmd.none
                    , send (UpdateSessionTensionHead (withMaybeData th))
                    )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        -- Page Action
        ChangePost field value ->
            let
                form =
                    model.tension_form

                newForm =
                    { form | post = Dict.insert field value form.post }
            in
            ( { model | tension_form = newForm }, Cmd.none, Cmd.none )

        DoChangeTitle ->
            ( { model | isTitleEdit = True }, Ports.focusOn "titleInput", Cmd.none )

        CancelTitle ->
            ( { model | isTitleEdit = False, tension_form = initTensionForm global.session.common.lexicon model.tensionid Nothing global.session.common.user, title_result = NotAsked }, Cmd.none, Cmd.none )

        SubmitTitle time ->
            let
                form =
                    model.tension_form

                newForm =
                    { form
                        | post =
                            form.post
                                |> Dict.insert "createdAt" (fromTime time)
                        , events =
                            [ Ev TensionEvent.TitleUpdated
                                (model.tension_head |> withMaybeMapData .title |> withDefault "")
                                (Dict.get "title" form.post |> withDefault "")
                            ]
                    }
            in
            ( { model | tension_form = newForm, title_result = LoadingSlowly }, send PushTitle, Cmd.none )

        TitleAck result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | title_result = NotAsked }, Ports.raiseAuthModal (uctxFromUser global.session.common.user), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep PushTitle 500, send UpdateUserToken )

                OkAuth _ ->
                    let
                        tension_h =
                            case model.tension_head of
                                Success t ->
                                    Success { t | title = Dict.get "title" model.tension_form.post |> withDefault "" }

                                other ->
                                    other

                        resetForm =
                            initTensionForm global.session.common.lexicon model.tensionid Nothing global.session.common.user
                    in
                    ( { model | tension_head = tension_h, tension_form = resetForm, title_result = result, isTitleEdit = False }
                    , Cmd.none
                    , send (UpdateSessionTensionHead (withMaybeData tension_h))
                    )

                _ ->
                    ( { model | title_result = result }, Cmd.none, Cmd.none )

        DoMove t ->
            ( model
            , Cmd.batch [ Cmd.map MoveTensionMsg (send (MoveTension.OnOpen t.id t.receiver.nameid t.latest_blob)) ]
            , Cmd.none
            )

        OnToggleDiff bid ->
            ( { model | expandedDiff = ternary (model.expandedDiff == bid) "" bid }, Cmd.none, Cmd.none )

        ChangeBlobEdit value ->
            ( { model | nodeDoc = NodeDoc.setNodeEdit (Just value) model.nodeDoc }, Cmd.none, Ports.bulma_driver "blobDocument" )

        ChangeBlobPost field value ->
            ( { model | nodeDoc = NodeDoc.updatePost field value model.nodeDoc }, Cmd.none, Cmd.none )

        AddResponsabilities ->
            ( { model | nodeDoc = NodeDoc.addResponsabilities model.nodeDoc }, Cmd.none, Ports.bulma_driver "blobDocument" )

        AddDomains ->
            ( { model | nodeDoc = NodeDoc.addDomains model.nodeDoc }, Cmd.none, Ports.bulma_driver "blobDocument" )

        AddPolicies ->
            ( { model | nodeDoc = NodeDoc.addPolicies model.nodeDoc }, Cmd.none, Ports.bulma_driver "blobDocument" )

        CommitBlob data time ->
            let
                newDoc =
                    data
                        |> NodeDoc.updatePost "createdAt" (fromTime time)
                        |> NodeDoc.setEvents [ Ev TensionEvent.BlobCommitted "" "" ]
                        |> NodeDoc.setResult LoadingSlowly
            in
            ( { model | nodeDoc = newDoc }, send (PushBlob_ newDoc.form), Cmd.none )

        BlobAck result ->
            let
                newDoc =
                    NodeDoc.setResult result model.nodeDoc
            in
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | nodeDoc = NodeDoc.setResult NotAsked model.nodeDoc }, Ports.raiseAuthModal (uctxFromUser global.session.common.user), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep (PushBlob_ newDoc.form) 500, send UpdateUserToken )

                OkAuth tp ->
                    let
                        th =
                            case model.tension_head of
                                Success t ->
                                    let
                                        latest_blob =
                                            case tp.blobs of
                                                Just blobs ->
                                                    List.head blobs

                                                Nothing ->
                                                    t.latest_blob
                                    in
                                    Success
                                        { t
                                            | latest_blob = latest_blob
                                            , draft_node_type = latest_blob |> Maybe.andThen .node |> Maybe.andThen .type_
                                        }

                                other ->
                                    other

                        nd =
                            case th of
                                Success t ->
                                    NodeDoc.initBlob model.session.lexicon (t.latest_blob |> Maybe.andThen .node |> withDefault (initNodeFragment Nothing)) newDoc

                                _ ->
                                    newDoc
                    in
                    ( { model | tension_head = th, nodeDoc = nd |> NodeDoc.reset }
                    , Cmd.batch [ Cmd.map CommentsMsg (send <| Comments.PushEvents (List.map (\e -> eventFromForm e model.tension_form) model.tension_form.events)) ]
                    , send (UpdateSessionTensionHead (withMaybeData th))
                    )

                _ ->
                    ( { model | nodeDoc = newDoc }, Cmd.none, Cmd.none )

        CancelBlob ->
            ( { model | nodeDoc = NodeDoc.reset model.nodeDoc }, Cmd.none, Cmd.none )

        PushBlob bid time ->
            let
                form =
                    model.tension_form

                newForm =
                    { form
                        | events = [ Ev TensionEvent.BlobPushed "" bid ]
                        , post = Dict.fromList [ ( "createdAt", fromTime time ) ]
                    }
            in
            ( { model | tension_form = newForm, publish_result = LoadingSlowly }
            , send PublishBlob
            , Cmd.none
            )

        PushBlobAck result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | publish_result = NotAsked }, Ports.raiseAuthModal (uctxFromUser global.session.common.user), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep PublishBlob 500, send UpdateUserToken )

                DuplicateErr ->
                    ( { model | publish_result = Failure [ T.duplicateNameError ] }, Cmd.none, Cmd.none )

                OkAuth r ->
                    case model.tension_head of
                        Success th ->
                            let
                                latest_blob =
                                    th.latest_blob
                                        |> Maybe.map
                                            (\b -> { b | pushedFlag = withDefault [] r.blobs |> List.head |> Maybe.map .pushedFlag |> withDefault b.pushedFlag })

                                newTh =
                                    { th | latest_blob = latest_blob, title = r.title, governed_node = r.governed_node }

                                resetForm =
                                    initTensionForm global.session.common.lexicon model.tensionid Nothing global.session.common.user
                            in
                            ( { model
                                | tension_head = Success newTh
                                , tension_form = resetForm
                                , publish_result = result
                              }
                            , Cmd.batch [ Cmd.map CommentsMsg (send <| Comments.PushEvents (List.map (\e -> eventFromForm e model.tension_form) model.tension_form.events)) ]
                            , Cmd.batch [ send UpdateUserToken, send (UpdateSessionTensionHead (Just newTh)) ]
                            )

                        _ ->
                            ( { model | publish_result = result }, Cmd.none, Cmd.none )

                _ ->
                    ( { model | publish_result = result }, Cmd.none, Cmd.none )

        -- Assignees
        DoAssigneeEdit ->
            -- Assignee selection is org-wide (queryMembers filters by rootnameid).
            ( model, Cmd.map UserSearchPanelMsg (send (UserSearchPanel.OnOpen [ model.node_focus.rootnameid ])), Cmd.none )

        UserSearchPanelMsg msg ->
            let
                ( panel, out ) =
                    UserSearchPanel.update apis msg model.assigneesPanel

                ( th, upth ) =
                    Maybe.map
                        (\r ->
                            withMapData
                                (\x ->
                                    let
                                        assignees =
                                            if Tuple.first r then
                                                withDefault [] x.assignees ++ [ Tuple.second r ]

                                            else
                                                LE.remove (Tuple.second r) (withDefault [] x.assignees)
                                    in
                                    { x | assignees = Just assignees }
                                )
                                model.tension_head
                        )
                        out.result
                        |> Maybe.map (\th_ -> ( th_, send <| UpdateSessionTensionHead (withMaybeData th_) ))
                        |> withDefault ( model.tension_head, Cmd.none )

                isAssigneeOpen =
                    UserSearchPanel.isOpen_ panel

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | assigneesPanel = panel, tension_head = th, isAssigneeOpen = isAssigneeOpen }
            , out.cmds |> List.map (\m -> Cmd.map UserSearchPanelMsg m) |> List.append cmds |> Cmd.batch
            , Cmd.batch (gcmds ++ [ upth ])
            )

        -- Labels
        DoLabelEdit ->
            -- Search labels declared on the path (root → focus) and direct children of focus.
            ( model, Cmd.map LabelSearchPanelMsg (send (LabelSearchPanel.OnOpen (getPathWithChildren model.path_data) False)), Cmd.none )

        LabelSearchPanelMsg msg ->
            let
                ( panel, out ) =
                    LabelSearchPanel.update apis msg model.labelsPanel

                ( th, upth ) =
                    Maybe.map
                        (\r ->
                            withMapData
                                (\x ->
                                    let
                                        labels =
                                            if Tuple.first r then
                                                withDefault [] x.labels ++ [ Tuple.second r ]

                                            else
                                                LE.remove (Tuple.second r) (withDefault [] x.labels)
                                    in
                                    { x | labels = Just labels }
                                )
                                model.tension_head
                        )
                        out.result
                        |> Maybe.map (\th_ -> ( th_, send <| UpdateSessionTensionHead (withMaybeData th_) ))
                        |> withDefault ( model.tension_head, Cmd.none )

                isLabelOpen =
                    LabelSearchPanel.isOpen_ panel

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | labelsPanel = panel, tension_head = th, isLabelOpen = isLabelOpen }
            , out.cmds |> List.map (\m -> Cmd.map LabelSearchPanelMsg m) |> List.append cmds |> Cmd.batch
            , Cmd.batch (gcmds ++ [ upth ])
            )

        -- Projects
        DoProjectEdit ->
            -- Search projects declared on the path (root → focus) and direct children of focus.
            ( model, Cmd.map ProjectSearchPanelMsg (send (ProjectSearchPanel.OnOpen (getPathWithChildren model.path_data))), Cmd.none )

        ProjectSearchPanelMsg msg ->
            let
                ( panel, out ) =
                    ProjectSearchPanel.update apis msg model.projectsPanel

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | projectsPanel = panel }
            , out.cmds |> List.map (Cmd.map ProjectSearchPanelMsg) |> List.append cmds |> Cmd.batch
            , Cmd.batch gcmds
            )

        -- Node Action
        OpenActionPanel domid nameid pos ->
            ( model, Cmd.map ActionPanelMsg (send <| ActionPanel.OnOpen domid nameid (TreeMenu.getOrgaData_ model.treeMenu) pos), Cmd.none )

        -- Common
        NoMsg ->
            ( model, Cmd.none, Cmd.none )

        LogErr err ->
            ( model, Ports.logErr err, Cmd.none )

        ScrollToElement did ->
            ( model, Scroll.scrollToElement did NoMsg, Cmd.none )

        UpdateUctx uctx ->
            ( { model
                | isTensionAdmin = getTensionRights uctx model.tension_head model.path_data
                , nodeDoc = NodeDoc.setUctx uctx model.nodeDoc
              }
            , Cmd.none
            , Cmd.none
            )

        SaveDraftDelayed timerValue ->
            -- Only save if this is the most recent scheduled save (debounce)
            if timerValue == model.draftSaveTimer then
                -- Read current message content (may have been modified by rich text ports)
                let
                    draftMessage =
                        Comments.getCurrentMessage model.comments |> withDefault ""

                    draft =
                        CommentDraft draftMessage ""
                in
                if draftMessage == "" then
                    ( model, Cmd.none, send (Global.UpdateDraft (ClearComment model.tensionid)) )

                else
                    ( model, Cmd.none, send (Global.UpdateDraft (SaveComment model.tensionid draft)) )

            else
                ( model, Cmd.none, Cmd.none )

        -- Components
        HelperBarMsg msg ->
            let
                ( data, out ) =
                    HelperBar.update apis msg model.helperBar

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | helperBar = data }, out.cmds |> List.map (\m -> Cmd.map HelperBarMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        NewTensionMsg msg ->
            let
                state =
                    case msg of
                        NTF.OnOpen _ _ ->
                            model.tensionForm
                                |> NTF.setCurrentDraft global.session.data.drafts.newTension
                                |> NTF.setSessionTemplates global.session.data.tension_templates

                        _ ->
                            model.tensionForm

                ( tf, out ) =
                    NTF.update apis msg state

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | tensionForm = tf }, out.cmds |> List.map (\m -> Cmd.map NewTensionMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        HelpMsg msg ->
            let
                ( help, out ) =
                    Help.update apis msg model.help

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | help = help }, out.cmds |> List.map (\m -> Cmd.map HelpMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        MoveTensionMsg msg ->
            let
                ( data, out ) =
                    MoveTension.update apis msg model.moveTension

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | moveTension = data }, out.cmds |> List.map (\m -> Cmd.map MoveTensionMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        ContractsPageMsg msg ->
            let
                ( data, out ) =
                    ContractsPage.update apis msg model.contractsPage

                ( th, upth ) =
                    Maybe.map
                        (\r ->
                            --withMapData (\x -> { x | contracts = Just (Tuple.second r) }) model.tension_head
                            withMapData (\x -> { x | contracts = Just [ { id = x.id } ] }) model.tension_head
                        )
                        out.result
                        |> Maybe.map (\th_ -> ( th_, send <| UpdateSessionTensionHead (withMaybeData th_) ))
                        |> withDefault ( model.tension_head, Cmd.none )

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | contractsPage = data, tension_head = th }
            , out.cmds |> List.map (\m -> Cmd.map ContractsPageMsg m) |> List.append cmds |> Cmd.batch
            , Cmd.batch (gcmds ++ [ upth ])
            )

        SelectTypeMsg msg ->
            let
                ( data, out ) =
                    SelectType.update apis msg model.selectType

                ( th, upth ) =
                    out.result
                        |> Maybe.map (\( _, r ) -> withMapData (\x -> { x | type_ = r }) model.tension_head)
                        |> Maybe.map (\th_ -> ( th_, send <| UpdateSessionTensionHead (withMaybeData th_) ))
                        |> withDefault ( model.tension_head, Cmd.none )

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | selectType = data, tension_head = th }
            , out.cmds |> List.map (\m -> Cmd.map SelectTypeMsg m) |> List.append cmds |> Cmd.batch
            , Cmd.batch (gcmds ++ [ upth ])
            )

        ActionPanelMsg msg ->
            let
                ( data, out ) =
                    ActionPanel.update apis msg model.actionPanel

                -- Update NodeFragment and governed archive state locally.
                th =
                    Maybe.map
                        (\r ->
                            withMapData
                                (\x ->
                                    let
                                        latest_blob =
                                            Maybe.map (\b -> { b | node = Just <| nodeFragmentUpdate b.node r }) x.latest_blob

                                        -- Archive transitions: flip the client-requested state, which the backend just applied.
                                        governed_node =
                                            case ActionPanel.getState_ model.actionPanel of
                                                ActionPanel.ArchiveAction ->
                                                    Maybe.map (setGovernedArchived True) x.governed_node

                                                ActionPanel.UnarchiveAction ->
                                                    Maybe.map (setGovernedArchived False) x.governed_node

                                                _ ->
                                                    x.governed_node
                                    in
                                    { x
                                        | latest_blob = latest_blob
                                        , governed_node = governed_node
                                        , draft_node_type = latest_blob |> Maybe.andThen .node |> Maybe.andThen .type_
                                    }
                                )
                                model.tension_head
                        )
                        out.result
                        |> withDefault model.tension_head

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | actionPanel = data, tension_head = th }
            , out.cmds |> List.map (\m -> Cmd.map ActionPanelMsg m) |> List.append cmds |> Cmd.batch
            , Cmd.batch gcmds
            )

        JoinOrgaMsg msg ->
            let
                state =
                    case msg of
                        JoinOrga.OnOpen _ _ ->
                            JoinOrga.setCurrentDraft global.session.data.drafts.newInvite model.joinOrga

                        _ ->
                            model.joinOrga

                ( data, out ) =
                    JoinOrga.update apis msg state

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | joinOrga = data }, out.cmds |> List.map (\m -> Cmd.map JoinOrgaMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        AuthModalMsg msg ->
            let
                ( data, out ) =
                    AuthModal.update apis msg model.authModal

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds

                cmds_extra =
                    out.result
                        |> Maybe.map
                            (\o ->
                                if Tuple.first o then
                                    -- reload silently the page if needed
                                    [ Nav.replaceUrl global.key (Url.toString global.url) ]

                                else
                                    []
                            )
                        |> withDefault []
            in
            ( { model | authModal = data }, out.cmds |> List.map (\m -> Cmd.map AuthModalMsg m) |> List.append (cmds ++ cmds_extra) |> Cmd.batch, Cmd.batch gcmds )

        OrgaMenuMsg msg ->
            let
                ( data, out ) =
                    OrgaMenu.update apis msg model.orgaMenu

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | orgaMenu = data }, out.cmds |> List.map (\m -> Cmd.map OrgaMenuMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        TreeMenuMsg msg ->
            let
                ( data, out ) =
                    TreeMenu.update apis msg model.treeMenu

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | treeMenu = data }, out.cmds |> List.map (\m -> Cmd.map TreeMenuMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        CommentsMsg msg ->
            let
                ( data, out ) =
                    Comments.update apis msg model.comments

                ( tension_head, gcmd ) =
                    case out.result of
                        Just (TensionCommentAdded status) ->
                            let
                                th =
                                    withMapData (\t -> { t | status = withDefault t.status status }) model.tension_head
                            in
                            ( th
                            , send (UpdateSessionTensionHead (withMaybeData th))
                            )

                        _ ->
                            ( model.tension_head, Cmd.none )

                -- Schedule debounced draft save when comment content changes
                ( draftTimer, draftSaveCmd ) =
                    case out.result of
                        Just (PostChanged ( "message", v )) ->
                            let
                                newTimer =
                                    model.draftSaveTimer + 1

                                time_delay =
                                    ternary (v == "") 0 3500
                            in
                            ( newTimer, sendSleep (SaveDraftDelayed newTimer) time_delay )

                        _ ->
                            ( model.draftSaveTimer, Cmd.none )

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | comments = data, tension_head = tension_head, draftSaveTimer = draftTimer }
            , (out.cmds |> List.map (\m -> Cmd.map CommentsMsg m) |> List.append cmds |> (::) draftSaveCmd) |> Cmd.batch
            , Cmd.batch (gcmd :: gcmds)
            )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ model =
    [ Ports.uctxPD Ports.loadUserCtxFromJs LogErr UpdateUctx
    ]
        ++ (HelperBar.subscriptions |> List.map (\s -> Sub.map HelperBarMsg s))
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        ++ (NTF.subscriptions model.tensionForm |> List.map (\s -> Sub.map NewTensionMsg s))
        ++ (ActionPanel.subscriptions model.actionPanel |> List.map (\s -> Sub.map ActionPanelMsg s))
        ++ (JoinOrga.subscriptions model.joinOrga |> List.map (\s -> Sub.map JoinOrgaMsg s))
        ++ (AuthModal.subscriptions |> List.map (\s -> Sub.map AuthModalMsg s))
        ++ (OrgaMenu.subscriptions |> List.map (\s -> Sub.map OrgaMenuMsg s))
        ++ (UserSearchPanel.subscriptions model.assigneesPanel |> List.map (\s -> Sub.map UserSearchPanelMsg s))
        ++ (LabelSearchPanel.subscriptions model.labelsPanel |> List.map (\s -> Sub.map LabelSearchPanelMsg s))
        ++ (ProjectSearchPanel.subscriptions model.projectsPanel |> List.map (\s -> Sub.map ProjectSearchPanelMsg s))
        ++ (MoveTension.subscriptions model.moveTension |> List.map (\s -> Sub.map MoveTensionMsg s))
        ++ (SelectType.subscriptions |> List.map (\s -> Sub.map SelectTypeMsg s))
        ++ (TreeMenu.subscriptions |> List.map (\s -> Sub.map TreeMenuMsg s))
        ++ (case model.activeTab of
                Conversation ->
                    Comments.subscriptions model.comments |> List.map (\s -> Sub.map CommentsMsg s)

                Document ->
                    []

                Contracts ->
                    ContractsPage.subscriptions model.contractsPage |> List.map (\s -> Sub.map ContractsPageMsg s)
           )
        |> Sub.batch


{-| Roots carry the isRootArchived flag; regular nodes the isArchived one.
-}
setGovernedArchived : Bool -> GovernedNode -> GovernedNode
setGovernedArchived archived g =
    if nid2rootid g.nameid == g.nameid then
        { g | isRootArchived = Just archived }

    else
        { g | isArchived = archived }



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    let
        helperData =
            { path_data = withMaybeData model.path_data
            , isPanelOpen = ActionPanel.isOpen_ "actionPanelHelper" model.actionPanel
            , orgaInfo = global.session.data.orgaInfo
            , isRootArchived = isRootArchivedOn model.node_focus.rootnameid model.path_data (TreeMenu.getOrgaData_ model.treeMenu)
            }

        panelData =
            { lifecycle = Active
            , isRight = True
            , domid = "actionPanelHelper"
            , tree_data = TreeMenu.getOrgaData_ model.treeMenu
            }
    in
    { title =
        case model.tension_head of
            Success t ->
                case model.activeTab of
                    Conversation ->
                        t.title

                    Document ->
                        t.title ++ " · " ++ ternary (model.nodeView == NodeVersions) T.revisions T.document

                    Contracts ->
                        t.title ++ " · " ++ T.contracts

            _ ->
                "Loading..."
    , body =
        [ div [ class "orgPane" ]
            [ HelperBar.view helperData model.helperBar |> Html.map HelperBarMsg
            , div [ id "mainPane" ] [ view_ global model ]
            ]
        , Lazy.lazy2 Help.view model.empty model.help |> Html.map HelpMsg
        , Lazy.lazy3 NTF.view (TreeMenu.getOrgaData_ model.treeMenu) model.path_data model.tensionForm |> Html.map NewTensionMsg
        , Lazy.lazy2 MoveTension.view (TreeMenu.getOrgaData_ model.treeMenu) model.moveTension |> Html.map MoveTensionMsg
        , Lazy.lazy2 SelectType.view model.empty model.selectType |> Html.map SelectTypeMsg
        , Lazy.lazy2 JoinOrga.view model.empty model.joinOrga |> Html.map JoinOrgaMsg
        , Lazy.lazy2 AuthModal.view model.empty model.authModal |> Html.map AuthModalMsg
        , Lazy.lazy2 OrgaMenu.view model.empty model.orgaMenu |> Html.map OrgaMenuMsg
        , Lazy.lazy3 TreeMenu.view model.empty helperData.isPanelOpen model.treeMenu |> Html.map TreeMenuMsg
        , ActionPanel.view panelData model.actionPanel |> Html.map ActionPanelMsg
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ class "columns is-centered" ]
        [ div [ class "column is-12 is-11-desktop is-9-fullhd" ]
            [ case model.tension_head of
                Success t ->
                    viewTension global.session.common.user t model

                Failure err ->
                    -- If user has only contract visibility right...
                    --if ContractsPage.hasCid model.contractsPage then
                    --    ContractsPage.view { emitterid = "", receiverid = "", isAdmin = model.isTensionAdmin, now = model.now } model.contractsPage
                    --        |> Html.map ContractsPageMsg
                    --else
                    div []
                        [ viewGqlErrors err
                        , showIf (errorIsNoDataFound err) <|
                            a [ class "button is-rounded is-primary is-center", href (toHref Login) ] [ text T.signin ]
                        ]

                LoadingSlowly ->
                    div [ class "spinner" ] []

                other ->
                    text ""

            -- User notification
            , if isSuccess model.unsubscribe_result && model.unsubscribe /= "" then
                div [ class "f6-notification notification has-timer is-success" ]
                    [ button [ class "delete", onClick OnCloseUnsubscribe ] []
                    , text (T.beenUnsubscribe model.session.lexicon)
                    ]

              else
                text ""
            , if isSuccess model.unwatch_result && model.unwatch /= "" then
                div [ class "f6-notification notification has-timer is-success" ]
                    [ button [ class "delete", onClick OnCloseUnwatch ] []
                    , text T.beenUnwatch
                    ]

              else
                text ""
            ]
        ]


viewTension : UserState -> TensionHead -> Model -> Html Msg
viewTension u t model =
    let
        uctx_m =
            case u of
                LoggedIn uctx ->
                    Just uctx

                LoggedOut ->
                    Nothing

        isAuthor =
            Maybe.map (\uctx -> t.createdBy.username == uctx.username) uctx_m |> withDefault False

        tensionNode =
            getTensionNode t

        blob_m =
            t.latest_blob
    in
    div []
        [ div [ class "columns m-0" ]
            -- @DEBUG: width corresponding to is-9 is hard-coded in modal-content (below) to
            -- avoid overflow with no scroll caude by <pre> tag
            [ div [ class "column is-9 px-0 pt-0" ]
                [ h1 [ class "title tensionTitle" ] <|
                    if model.isTitleEdit then
                        let
                            title =
                                Dict.get "title" model.tension_form.post |> withDefault t.title

                            isLoading =
                                model.title_result == LoadingSlowly

                            isSendable =
                                title /= t.title
                        in
                        [ div [ class "field is-grouped" ]
                            [ p [ class "control is-expanded" ]
                                [ input
                                    [ id "titleInput"
                                    , class "input is-human"
                                    , type_ "text"
                                    , placeholder "Title*"
                                    , spellcheck True
                                    , value title
                                    , onInput (ChangePost "title")
                                    ]
                                    []
                                ]
                            , p [ class "control buttons" ]
                                [ button
                                    [ class "button is-success is-small"
                                    , classList [ ( "is-loading", isLoading ) ]
                                    , disabled (not isSendable)
                                    , onClick (Submit (isSendable && not isLoading) SubmitTitle)
                                    ]
                                    [ text T.update ]
                                , button [ class "button is-small", onClick CancelTitle ] [ text T.cancel ]
                                ]
                            ]
                        , viewMaybeErrors model.title_result
                        ]

                    else
                        [ span [ class "is-human" ] [ text t.title ]
                        , if (model.isTensionAdmin || isAuthor) && blob_m == Nothing then
                            div
                                [ class "button has-text-weight-normal is-pulled-right is-small is-hidden-embed"
                                , title T.editTitle
                                , style "vertical-align" "middle" -- @needHelp do not work with pulled right.
                                , onClick DoChangeTitle
                                ]
                                [ A.icon "icon-edit-2" ]

                          else
                            text ""
                        ]
                , div [ class "tensionSubtitle level" ]
                    [ div [ class "level-left" ] <|
                        List.map (div [ class "level-item" ] << List.singleton) <|
                            [ span
                                [ class "tag is-rounded has-background-tag"
                                , classList [ ( "is-w", model.isTensionAdmin || isAuthor ) ]
                                , ternary (model.isTensionAdmin || isAuthor) (onClick <| SelectTypeMsg (SelectType.OnOpen t.type_)) (onClick NoMsg)
                                ]
                                [ tensionIcon2 t.type_ ]
                            , -- Governance tensions with a node doc are auto-closed on creation; a "Closed" tag would be misleading since the object exists.
                              if t.type_ == TensionType.Governance && tensionNode /= Nothing then
                                text ""

                              else
                                span [ class ("tag is-rounded is-w  is-" ++ statusColor t.status), onClick (ScrollToElement "tensionCommentInput") ]
                                    [ t.status |> tensionStatus2str |> text ]
                            , viewTensionDateAndUser model.session "is-discrete" t.createdAt t.createdBy
                            ]
                    , div [ class "level-right" ] <|
                        List.map (div [ class "level-item" ] << List.singleton) <|
                            [ viewCircleTarget OverviewBaseUri model.commonOp "" t.receiver ]
                    ]
                ]
            ]
        , div [ class "columns is-centered", classList [ ( "is-variable is-4", not (isMobile model.session.screen) ) ] ]
            [ div [ class "column is-9" ]
                [ div [ class "tabs is-md" ]
                    [ ul []
                        [ li [ classList [ ( "is-active", model.activeTab == Conversation ) ] ]
                            [ a [ href (Route.Tension_Dynamic_Dynamic { param1 = model.node_focus.rootnameid, param2 = t.id } |> toHref) ]
                                [ A.icon1 "icon-message-square" T.conversation ]
                            ]
                        , if t.latest_blob /= Nothing then
                            li [ classList [ ( "is-active", model.activeTab == Document && model.nodeView /= NodeVersions ) ] ]
                                [ a [ href (Route.Tension_Dynamic_Dynamic_Action { param1 = model.node_focus.rootnameid, param2 = t.id } |> toHref) ]
                                    [ A.icon1 "icon-copy" T.document ]
                                ]

                          else
                            text ""
                        , if t.latest_blob /= Nothing then
                            li [ classList [ ( "is-active", model.activeTab == Document && model.nodeView == NodeVersions ) ] ]
                                [ a [ href ((Route.Tension_Dynamic_Dynamic_Action { param1 = model.node_focus.rootnameid, param2 = t.id } |> toHref) ++ "?v=history") ]
                                    [ A.icon1 "icon-history" T.revisions ]
                                ]

                          else
                            text ""
                        , if t.contracts /= Nothing && t.contracts /= Just [] || model.baseUri == ContractsBaseUri then
                            li [ classList [ ( "is-active", model.activeTab == Contracts ) ] ]
                                [ a [ href (Route.Tension_Dynamic_Dynamic_Contract { param1 = model.node_focus.rootnameid, param2 = t.id } |> toHref) ]
                                    [ A.icon1 "icon-link-2" T.contracts ]
                                ]

                          else
                            text ""
                        ]
                    ]
                , case model.activeTab of
                    Conversation ->
                        viewConversation u t model

                    Document ->
                        case t.latest_blob of
                            Just b ->
                                viewDocument u t b model

                            Nothing ->
                                div [] [ text "No document yet..." ]

                    Contracts ->
                        ContractsPage.view
                            { receiverid = t.receiver.nameid, isAdmin = model.isTensionAdmin }
                            model.contractsPage
                            |> Html.map ContractsPageMsg
                ]
            , div [ class "column is-3 is-hidden-embed" ]
                [ viewSidePane u t model ]
            ]
        ]


viewConversation : UserState -> TensionHead -> Model -> Html Msg
viewConversation u t model =
    let
        userCanJoin =
            withMaybeData model.path_data
                |> Maybe.map
                    (\path ->
                        path.root |> Maybe.map (\r -> r.userCanJoin == Just True) |> withDefault False
                    )
                |> withDefault False

        userCanComment =
            -- Author or member can comment tension.
            -- is Author
            case u of
                LoggedIn uctx ->
                    (uctx.username == t.createdBy.username)
                        || -- is Member
                           (getOrgaRoles [ t.receiver.nameid ] uctx.roles /= [])

                LoggedOut ->
                    False

        userInput =
            case u of
                LoggedIn _ ->
                    if userCanComment then
                        Comments.viewTensionCommentInput model.session t model.comments |> Html.map CommentsMsg

                    else
                        viewJoinForCommentNeeded userCanJoin

                LoggedOut ->
                    if userCanJoin then
                        viewJoinForCommentNeeded userCanJoin

                    else
                        text ""
    in
    case model.tension_comments of
        Success t_comments ->
            div [ class "comments" ]
                [ Lazy.lazy3 Comments.viewCommentsTension model.session t model.comments |> Html.map CommentsMsg
                , hr [ class "is-2" ] []
                , userInput
                ]

        Failure err ->
            viewGqlErrors err

        LoadingSlowly ->
            div [ class "spinner" ] []

        _ ->
            text ""


viewDocument : UserState -> TensionHead -> Blob -> Model -> Html Msg
viewDocument u t b model =
    -- Node Document
    let
        nodeData =
            { focus = model.node_focus
            , tid_r = Success t.id
            , node = b.node |> Maybe.map (nodeFromTensionHead t)
            , node_data = b.node |> Maybe.map (\d -> NodeData d.about d.mandate) |> withDefault initNodeData
            , leads = []
            , session = model.session
            , isLazy = False
            , source = model.baseUri
            , hasBeenPushed = t.governed_node /= Nothing
            , hasInnerToolbar = False
            , isAdmin = model.isTensionAdmin
            }

        op =
            { session = model.session
            , data = model.nodeDoc
            , result = NotAsked
            , publish_result = model.publish_result
            , blob = b
            , tension_blobs = model.tension_blobs
            , expandedDiff = model.expandedDiff
            , onToggleDiff = OnToggleDiff
            , onSubmit = Submit
            , onSubmitBlob = CommitBlob
            , onCancelBlob = CancelBlob
            , onPushBlob = PushBlob
            , onChangeEdit = ChangeBlobEdit
            , onChangePost = ChangeBlobPost
            , onAddDomains = AddDomains
            , onAddPolicies = AddPolicies
            , onAddResponsabilities = AddResponsabilities
            , mdOps = Nothing
            }
    in
    NodeDoc.view nodeData (Just op)



--
-- Side Pane
--


viewSidePane : UserState -> TensionHead -> Model -> Html Msg
viewSidePane u t model =
    let
        tensionNode =
            getTensionNode t

        assignees =
            t.assignees |> withDefault []

        labels =
            t.labels |> withDefault []

        blob_m =
            t.latest_blob

        --
        uctx_m =
            case u of
                LoggedIn uctx ->
                    Just uctx

                LoggedOut ->
                    Nothing

        hasRole =
            Maybe.map
                (\b ->
                    let
                        fs =
                            b.node
                                |> Maybe.map .first_link
                                |> withDefault Nothing
                    in
                    Maybe.map (\uctx -> Just uctx.username == fs) uctx_m |> withDefault False
                )
                blob_m
                |> withDefault False

        --
        isAdmin =
            model.isTensionAdmin

        isAuthor =
            Maybe.map (\uctx -> t.createdBy.username == uctx.username) uctx_m |> withDefault False

        hasAssigneeRight =
            isAdmin

        hasLabelRight =
            isAdmin || isAuthor

        hasProjectRight =
            isAdmin || isAuthor

        hasBlobRight =
            isAdmin && t.governed_node /= Nothing && blob_m /= Nothing

        rid =
            nid2rootid t.receiver.nameid

        isRoot =
            t.receiver.nameid == rid && (unwrap Nothing .node blob_m |> unwrap Nothing .nameid) == Just ""
    in
    div [ class "tensionSidePane mt-5 pt-3" ] <|
        [ -- Assignees/User select
          div
            [ class "media"
            , classList [ ( "is-w2", hasAssigneeRight ) ]
            , ternary hasAssigneeRight (onClick DoAssigneeEdit) (onClick NoMsg)
            ]
            [ div [ class "media-content" ] <|
                (case u of
                    LoggedIn _ ->
                        [ h2
                            [ class "subtitle" ]
                            [ text T.assignees
                            , if model.isAssigneeOpen then
                                A.icon "icon-x is-pulled-right"

                              else if hasAssigneeRight then
                                A.icon "icon-settings is-pulled-right"

                              else
                                text ""
                            ]
                        , UserSearchPanel.view
                            { selectedAssignees = assignees
                            , targets = model.path_data |> withMaybeMapData (\x -> List.map .nameid x.path) |> withDefault []
                            , isRight = False
                            }
                            model.assigneesPanel
                            |> Html.map UserSearchPanelMsg
                        ]

                    LoggedOut ->
                        [ h2 [ class "subtitle" ] [ text T.assignees ] ]
                )
                    ++ [ div []
                            [ if List.length assignees > 0 then
                                viewUsers True assignees

                              else
                                div [ class "help-label is-italic" ] [ text T.noneYet ]
                            ]
                       ]
            ]

        -- Label select
        , div
            [ class "media"
            , classList [ ( "is-w2", hasLabelRight ) ]
            , ternary hasLabelRight (onClick DoLabelEdit) (onClick NoMsg)
            ]
            [ div [ class "media-content" ] <|
                (case u of
                    LoggedIn _ ->
                        [ h2 [ class "subtitle" ]
                            [ text T.labels
                            , if model.isLabelOpen then
                                A.icon "icon-x is-pulled-right"

                              else if hasLabelRight then
                                A.icon "icon-settings is-pulled-right"

                              else
                                text ""
                            ]
                        , LabelSearchPanel.view
                            { selectedLabels = labels
                            , targets = model.path_data |> withMaybeMapData (.focus >> .nameid >> List.singleton) |> withDefault []
                            , isRight = False
                            }
                            model.labelsPanel
                            |> Html.map LabelSearchPanelMsg
                        ]

                    LoggedOut ->
                        [ h2 [ class "subtitle" ] [ text T.labels ] ]
                )
                    ++ [ div [ class "tension-labelsList" ]
                            [ if List.length labels > 0 then
                                viewLabels Nothing labels

                              else
                                div [ class "help-label is-italic" ] [ text T.noneYet ]
                            ]
                       ]
            ]

        -- Projects
        , div
            [ class "media"
            , classList [ ( "is-w2", hasProjectRight ) ]
            , ternary hasProjectRight (onClick DoProjectEdit) (onClick NoMsg)
            ]
            [ div [ class "media-content" ] <|
                (case u of
                    LoggedIn _ ->
                        [ h2 [ class "subtitle" ]
                            [ text T.projects
                            , if ProjectSearchPanel.isOpen_ model.projectsPanel then
                                A.icon "icon-x is-pulled-right"

                              else if hasProjectRight then
                                A.icon "icon-settings is-pulled-right"

                              else
                                text ""
                            ]
                        , ProjectSearchPanel.view
                            { selectedProjects = ProjectSearchPanel.getSelectedProjects model.projectsPanel
                            , targets = model.path_data |> withMaybeMapData (.focus >> .nameid >> List.singleton) |> withDefault []
                            , isRight = False
                            }
                            model.projectsPanel
                            |> Html.map ProjectSearchPanelMsg
                        ]

                    LoggedOut ->
                        [ h2 [ class "subtitle" ] [ text T.projects ] ]
                )
                    ++ [ ProjectSearchPanel.viewCards hasProjectRight model.projectsPanel
                            |> Html.map ProjectSearchPanelMsg
                       ]
            ]

        -- Document
        , Maybe.map2
            (\blob nodeState ->
                -- Hide if there is no document
                let
                    domid =
                        "actionPanelContent"

                    isOpen =
                        ActionPanel.isOpen_ domid model.actionPanel && (hasBlobRight || hasRole)

                    node =
                        blob.node |> withDefault (initNodeFragment Nothing) |> nodeFromTensionHead t
                in
                div
                    [ class "media"
                    , classList [ ( "is-w2", hasBlobRight || hasRole ) ]
                    ]
                    [ div [ class "media-content wrapped-container" ]
                        [ div
                            [ class "media-content"
                            , if not isOpen then
                                onClick (OpenActionPanel domid node.nameid Nothing)

                              else
                                onClick (ActionPanelMsg ActionPanel.OnClose)
                            ]
                          <|
                            (case u of
                                LoggedIn _ ->
                                    [ div [ id domid ]
                                        [ h2
                                            [ class "subtitle" ]
                                            [ text T.document
                                            , if isOpen then
                                                A.icon "icon-x is-pulled-right"

                                              else if hasBlobRight || hasRole then
                                                A.icon "icon-settings is-pulled-right"

                                              else
                                                text ""
                                            ]
                                        , if hasBlobRight || hasRole then
                                            let
                                                panelData =
                                                    { lifecycle = nodeState.lifecycle
                                                    , isRight = False
                                                    , domid = domid
                                                    , tree_data = TreeMenu.getOrgaData_ model.treeMenu
                                                    }
                                            in
                                            ActionPanel.view panelData model.actionPanel |> Html.map ActionPanelMsg

                                          else
                                            text ""
                                        ]
                                    ]

                                LoggedOut ->
                                    [ h2 [ class "subtitle" ] [ text T.document ] ]
                            )
                                ++ [ viewNodeDescr True node
                                   , -- Node Artefact
                                     case node.type_ of
                                        NodeType.Circle ->
                                            -- @debug: can't center item :/
                                            div [ class "is-flex" ]
                                                [ viewCircleTarget OverviewBaseUri model.commonOp "mb-3 is-medium is-align-self-center" { name = node.name, nameid = node.nameid, role_type = node.role_type, color = node.color }
                                                ]

                                        NodeType.Role ->
                                            case node.role_type of
                                                Just rt ->
                                                    if t.governed_node /= Nothing then
                                                        viewRole "" False False Nothing (Just <| toLink OverviewBaseUri node.nameid []) (\_ _ _ -> NoMsg) (eor2ur node)

                                                    else
                                                        viewRoleExt model.commonOp "is-small" Nothing { name = node.name, color = node.color, role_type = rt }

                                                Nothing ->
                                                    text ""
                                   , Maybe.map
                                        (\fs ->
                                            div [ class "mt-2" ] [ span [ class "is-inline-flex mr-2" ] [ A.icon1 "icon-user" (T.firstLink ++ " :") ], viewUserFull 0 True False fs ]
                                        )
                                        node.first_link
                                        |> withDefault (text "")
                                   , if nodeState.lifecycle == Archived then
                                        div [ class "mt-2 has-text-warning" ] [ A.icon1 "icon-archive" T.archived ]

                                     else
                                        text ""
                                   ]
                        , if model.activeTab == Document then
                            text ""

                          else
                            let
                                op =
                                    { session = model.session
                                    , data = model.nodeDoc
                                    , result = NotAsked
                                    , publish_result = model.publish_result
                                    , blob = blob
                                    , tension_blobs = model.tension_blobs
                                    , expandedDiff = model.expandedDiff
                                    , onToggleDiff = OnToggleDiff
                                    , onSubmit = Submit
                                    , onSubmitBlob = CommitBlob
                                    , onCancelBlob = CancelBlob
                                    , onPushBlob = PushBlob
                                    , onChangeEdit = ChangeBlobEdit
                                    , onChangePost = ChangeBlobPost
                                    , onAddDomains = AddDomains
                                    , onAddPolicies = AddPolicies
                                    , onAddResponsabilities = AddResponsabilities
                                    , mdOps = Nothing
                                    }
                            in
                            div [ class "is-flex mt-3" ]
                                [ NodeDoc.viewNodeStatus model.isTensionAdmin op
                                ]
                        ]
                    ]
            )
            blob_m
            tensionNode
            |> withDefault (text "")
        , -- Subscriptions
          case u of
            LoggedIn uctx ->
                let
                    ( iconElt, subscribe_txt ) =
                        case model.tension_head |> withMaybeData |> Maybe.map .isSubscribed of
                            Just True ->
                                ( A.icon1 "icon-bell-off icon-1x" T.unsubscribe, T.tensionSubscribeText )

                            Just False ->
                                ( A.icon1 "icon-bell icon-1x" T.subscribe, T.tensionUnsubscribeText )

                            Nothing ->
                                ( text "", "" )
                in
                div [ class "media pb-0" ]
                    [ div [ class "media-content" ]
                        [ h2 [ class "subtitle" ]
                            [ text T.notifications ]
                        , p
                            [ class "button is-fullwidth has-background-evidence is-small "
                            , style "border-radius" "5px"
                            , onClick (ToggleSubscription uctx.username)
                            ]
                            [ iconElt, loadingSpin (model.subscribe_result == LoadingSlowly) ]
                        , p [ class "help" ] [ text subscribe_txt ]
                        , case model.subscribe_result of
                            Failure err ->
                                viewGqlErrors err

                            _ ->
                                text ""
                        ]
                    ]

            LoggedOut ->
                text ""
        ]
            -- Extra action (Move, Lock, ...)
            ++ (if not isRoot && (isAdmin || isAuthor) then
                    let
                        hasNode =
                            tensionNode /= Nothing
                    in
                    [ hr [ class "has-background-border-light" ] [] ]
                        ++ (if isAdmin then
                                [ div
                                    [ class "is-smaller2 has-text-weight-semibold button-light discrete-link mb-4"
                                    , title <|
                                        ternary t.isPinned
                                            (T.unpinTensionHelp model.session.lexicon)
                                            (T.pinTensionHelp model.session.lexicon)
                                    , onClick (Submit True <| ternary t.isPinned UnpinTension PinTension)
                                    ]
                                    [ A.icon1_noflex "icon-pin" <|
                                        ternary t.isPinned T.unpinTension T.pinTension
                                    , showIf t.isPinned <| A.icon "icon-disc has-text-success ml-2"
                                    ]
                                ]

                            else
                                []
                           )
                        ++ (if not hasNode then
                                [ div
                                    [ class "is-smaller2 has-text-weight-semibold button-light discrete-link mb-4"
                                    , title (T.moveTensionHelp model.session.lexicon)
                                    , onClick (DoMove t)
                                    ]
                                    [ span [ class "arrow-right2 pl-0 pr-2" ] [], text T.move ]
                                ]

                            else
                                []
                           )
                        ++ (if isAdmin && not hasNode then
                                [ div
                                    [ class "is-smaller2 has-text-weight-semibold button-light discrete-link mb-4"
                                    , title T.updateTypeHelp
                                    , onClick <| SelectTypeMsg (SelectType.OnOpen t.type_)
                                    ]
                                    [ A.icon1 "icon-diamond" T.updateType ]
                                ]

                            else
                                []
                           )
                        ++ (if isAdmin then
                                [--, div [ class "is-smaller2 has-text-weight-semibold button-light discrete-link mb-4" ] [ A.icon1 "icon-lock icon-sm" "Lock conversation" ]
                                ]

                            else
                                []
                           )

                else
                    []
               )
