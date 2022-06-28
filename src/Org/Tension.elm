module Org.Tension exposing (Flags, Model, Msg, TensionTab(..), init, page, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Browser.Navigation as Nav
import Codecs exposing (LookupResult, QuickDoc)
import Components.ActionPanel as ActionPanel
import Components.AuthModal as AuthModal
import Components.Comments exposing (viewComment, viewCommentInput)
import Components.ContractsPage as ContractsPage
import Components.DocToolBar as DocToolBar exposing (ActionView(..))
import Components.HelperBar as HelperBar exposing (HelperBar)
import Components.JoinOrga as JoinOrga
import Components.LabelSearchPanel as LabelSearchPanel
import Components.Loading as Loading
    exposing
        ( GqlData
        , ModalData
        , RequestResult(..)
        , WebData
        , fromMaybeData
        , isFailure
        , loadingSpin
        , viewAuthNeeded
        , viewGqlErrors
        , viewHttpErrors
        , viewMaybeErrors
        , withDefaultData
        , withMapData
        , withMaybeData
        , withMaybeDataMap
        )
import Components.MoveTension as MoveTension
import Components.NodeDoc as NodeDoc exposing (NodeDoc)
import Components.OrgaMenu as OrgaMenu
import Components.SelectType as SelectType
import Components.TreeMenu as TreeMenu
import Components.UserSearchPanel as UserSearchPanel
import Dict exposing (Dict)
import Extra exposing (ternary)
import Extra.Date exposing (formatDate)
import Extra.Url exposing (queryBuilder, queryParser)
import Form exposing (isPostSendable)
import Form.Help as Help
import Form.NewTension as NTF exposing (NewTensionInput(..), TensionTab(..))
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.NodeVisibility as NodeVisibility
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (Msg(..), send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, strong, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, readonly, rows, spellcheck, style, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs
    exposing
        ( ActionType(..)
        , DocType(..)
        , FractalBaseRoute(..)
        , NodeFocus
        , focusFromNameid
        , focusFromPath
        , focusState
        , getOrgaRoles
        , getTensionCharac
        , isOwner
        , nid2rootid
        , nodeFromFragment
        , tensionAction2NodeType
        , uriFromUsername
        )
import ModelCommon.Requests exposing (getQuickDoc, login, signupValidate)
import ModelCommon.View
    exposing
        ( action2icon
        , action2str
        , archiveActionToggle
        , auth2icon
        , auth2val
        , statusColor
        , tensionIcon2
        , tensionTypeColor
        , viewJoinNeeded
        , viewLabel
        , viewLabels
        , viewNodeRefShort
        , viewTensionArrow
        , viewTensionDateAndUser
        , viewTensionDateAndUserC
        , viewUpdated
        , viewUser0
        , viewUser2
        , viewUserFull
        , viewUsernameLink
        , viewUsers
        )
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.PatchTension exposing (actionRequest, patchComment, patchLiteral, publishBlob, pushTensionPatch)
import Query.PatchUser exposing (markAsRead, toggleTensionSubscription)
import Query.QueryNode exposing (fetchNode, queryFocusNode, queryLocalGraph)
import Query.QueryTension exposing (getTensionBlobs, getTensionComments, getTensionHead)
import Query.QueryUser exposing (getIsSubscribe)
import RemoteData exposing (RemoteData)
import Scroll
import Session exposing (GlobalCmd(..), LabelSearchPanelOnClickAction(..), UserSearchPanelOnClickAction(..))
import String.Extra as SE
import Task
import Text as T exposing (textH, textT, toText, upH)
import Time
import Url exposing (Url)



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
                    DoNavigate link ->
                        ( send (Navigate link), Cmd.none )

                    DoReplaceUrl url ->
                        ( Cmd.none, send (ReplaceUrl url) )

                    DoUpdateToken ->
                        ( Cmd.none, send UpdateUserToken )

                    DoUpdateUserSession uctx ->
                        ( Cmd.none, send (UpdateUserSession uctx) )

                    DoUpdateOrgs orgs ->
                        ( Cmd.none, send (UpdateSessionOrgs orgs) )

                    DoUpdateTree tree ->
                        ( Cmd.none, send (UpdateSessionTree tree) )

                    DoCreateTension nameid ->
                        ( Cmd.map NewTensionMsg <| send (NTF.OnOpen (FromNameid nameid)), Cmd.none )

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
    , tensionid : String
    , baseUri : FractalBaseRoute
    , contractid : Maybe String
    , activeTab : TensionTab
    , actionView : ActionView
    , jumpTo : Maybe String
    , tension_head : GqlData TensionHead
    , tension_comments : GqlData TensionComments
    , tension_blobs : GqlData TensionBlobs
    , expandedEvents : List Int
    , subscribe_result : GqlData Bool
    , eid : String

    -- Form (Title, Status, Comment)
    , tension_form : TensionForm
    , tension_patch : GqlData PatchTensionPayloadID

    -- Title Result
    , isTitleEdit : Bool
    , title_result : GqlData IdPayload

    -- Comment Edit
    , comment_form : CommentPatchForm
    , comment_result : GqlData Comment

    -- Blob Edit
    , nodeDoc : NodeDoc
    , publish_result : GqlData BlobFlag

    -- Side Pane
    , isTensionAdmin : Bool
    , isAssigneeOpen : Bool
    , isLabelOpen : Bool

    -- Common
    , inputViewMode : InputViewMode
    , helperBar : HelperBar
    , refresh_trial : Int
    , now : Time.Posix
    , empty : {}

    -- Components
    , help : Help.State
    , tensionForm : NTF.State
    , assigneesPanel : UserSearchPanel.State
    , labelsPanel : LabelSearchPanel.State
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


actionViewEncoder : ActionView -> String
actionViewEncoder x =
    case x of
        DocView ->
            ""

        DocEdit ->
            "edit"

        DocVersion ->
            "history"

        NoView ->
            "noview"


actionViewDecoder : String -> ActionView
actionViewDecoder x =
    case x of
        "edit" ->
            DocEdit

        "history" ->
            DocVersion

        "noview" ->
            NoView

        _ ->
            DocView



---- MSG ----


type Msg
    = PassedSlowLoadTreshold -- timer
    | LoadTensionHead
    | LoadTensionComments
    | PushCommentPatch
    | PushTitle
    | PushBlob_ TensionForm
    | PublishBlob
    | Submit (Time.Posix -> Msg) -- Get Current Time
      -- Data Queries
    | GotPath Bool (GqlData LocalGraph)
      -- Page
    | GotTensionHead (GqlData TensionHead)
    | GotIsSucribe (GqlData Bool)
    | GotTensionComments (GqlData TensionComments)
    | GotTensionBlobs (GqlData TensionBlobs)
    | ExpandEvent Int
    | ToggleSubscription String
    | MarkAsRead String
    | GotMarkAsRead (GqlData IdPayload)
      --
      -- Page Action
      --
      -- new comment
    | ChangeTensionPost String String -- {field value}
    | SubmitComment (Maybe TensionStatus.TensionStatus) Time.Posix
    | CommentAck (GqlData PatchTensionPayloadID)
      -- edit comment
    | DoUpdateComment String
    | CancelCommentPatch
    | ChangeCommentPost String String
    | SubmitCommentPatch Time.Posix
    | CommentPatchAck (GqlData Comment)
      -- edit title
    | DoChangeTitle
    | CancelTitle
    | SubmitTitle Time.Posix
    | TitleAck (GqlData IdPayload)
      -- Blob control
    | DoBlobEdit BlobType.BlobType
    | CancelBlob
      -- Blob doc edit
    | ChangeBlobPost String String
    | AddDomains
    | AddPolicies
    | AddResponsabilities
    | ChangeBlobMD String
      -- Blob Submit
    | SubmitBlob NodeDoc Time.Posix --@debug new type to handle MdDoc
    | BlobAck (GqlData PatchTensionPayloadID)
    | PushBlob String Time.Posix
    | PushBlobAck (GqlData BlobFlag)
      -- Assignees
    | DoAssigneeEdit
      -- Labels
    | DoLabelEdit
      -- move tension
    | DoMove TensionHead
      -- Node Action
    | OpenActionPanel String Blob
      -- Common
    | NoMsg
    | LogErr String
    | Navigate String
    | ChangeInputViewMode InputViewMode
    | ChangeUpdateViewMode InputViewMode
    | ExpandRoles
    | CollapseRoles
    | ScrollToElement String
    | UpdateUctx UserCtx
      -- Components
    | HelpMsg Help.Msg
    | NewTensionMsg NTF.Msg
    | UserSearchPanelMsg UserSearchPanel.Msg
    | LabelSearchPanelMsg LabelSearchPanel.Msg
    | MoveTensionMsg MoveTension.Msg
    | ContractsPageMsg ContractsPage.Msg
    | SelectTypeMsg SelectType.Msg
    | ActionPanelMsg ActionPanel.Msg
    | JoinOrgaMsg JoinOrga.Msg
    | AuthModalMsg AuthModal.Msg
    | OrgaMenuMsg OrgaMenu.Msg
    | TreeMenuMsg TreeMenu.Msg



---- INIT ----


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        apis =
            global.session.apis

        -- Query parameters
        query =
            queryParser global.url

        -- Focus
        rootnameid =
            flags.param1 |> Url.percentDecode |> withDefault ""

        tid =
            flags.param2

        tab =
            flags.param3

        cid_m =
            flags.param4

        -- Focus
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
            focusState baseUri global.session.referer global.url global.session.node_focus newFocus_

        newFocus =
            if fs.orgChange then
                -- This allow TreeMenu to reload in case of orgChange
                newFocus_

            else
                global.session.path_data
                    |> Maybe.map focusFromPath
                    |> withDefault newFocus_

        model =
            { node_focus = newFocus
            , lookup_users = []
            , tensionid = tid
            , baseUri = baseUri
            , contractid = cid_m
            , activeTab = tab
            , actionView = Dict.get "v" query |> withDefault [] |> List.head |> withDefault "" |> actionViewDecoder
            , jumpTo = Dict.get "goto" query |> Maybe.map List.head |> withDefault Nothing
            , path_data = fromMaybeData global.session.path_data Loading
            , tension_head = fromMaybeData global.session.tension_head Loading
            , tension_comments = Loading
            , tension_blobs = Loading
            , expandedEvents = []
            , subscribe_result = NotAsked
            , eid = ""

            -- Form (Title, Status, Comment)
            , tension_form = initTensionForm tid global.session.user

            -- Push Comment / Change status
            , tension_patch = NotAsked

            -- Title Result
            , isTitleEdit = False
            , title_result = NotAsked

            -- Comment Edit
            , comment_form = initCommentPatchForm global.session.user
            , comment_result = NotAsked

            -- Blob Edit
            , nodeDoc = NodeDoc.create tid global.session.user
            , publish_result = NotAsked

            -- Side Pane
            , isTensionAdmin = withDefault False global.session.isAdmin
            , isAssigneeOpen = False
            , assigneesPanel = UserSearchPanel.init tid AssignUser global.session.user
            , isLabelOpen = False
            , labelsPanel = LabelSearchPanel.init tid AssignLabel global.session.user

            -- Common
            , inputViewMode = Write
            , helperBar = HelperBar.create
            , help = Help.init global.session.user
            , tensionForm = NTF.init global.session.user
            , refresh_trial = 0
            , moveTension = MoveTension.init global.session.user
            , contractsPage = ContractsPage.init rootnameid global.session.user
            , selectType = SelectType.init tid global.session.user
            , actionPanel = ActionPanel.init global.session.user
            , now = global.now
            , empty = {}
            , joinOrga = JoinOrga.init newFocus.nameid global.session.user
            , authModal = AuthModal.init global.session.user (Dict.get "puid" query |> Maybe.map List.head |> withDefault Nothing)
            , orgaMenu = OrgaMenu.init newFocus global.session.orga_menu global.session.orgs_data global.session.user
            , treeMenu = TreeMenu.init baseUri global.url.query newFocus global.session.tree_menu global.session.tree_data global.session.user
            }

        refresh =
            tensionChanged2 model.tension_head global.url || model.tension_head == Loading
    in
    ( { model | subscribe_result = withMapData (\x -> withDefault False x.isSubscribed) model.tension_head }
    , Cmd.batch (refresh_cmds refresh global model)
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
    , case model.activeTab of
        Conversation ->
            getTensionComments apis model.tensionid GotTensionComments

        Document ->
            case model.actionView of
                DocView ->
                    Cmd.none

                DocEdit ->
                    Cmd.none

                DocVersion ->
                    getTensionBlobs apis model.tensionid GotTensionBlobs

                NoView ->
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
    , Cmd.map AuthModalMsg (send AuthModal.OnStart)
    , Cmd.map OrgaMenuMsg (send OrgaMenu.OnLoad)
    , Cmd.map TreeMenuMsg (send TreeMenu.OnLoad)
    ]



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
                    case global.session.user of
                        LoggedIn uctx_ ->
                            ( uctx_
                              -- Now directly loaded in TensionLoad query
                              --, getIsSubscribe apis uctx_.username model.tensionid GotIsSucribe
                            , Cmd.none
                            )

                        LoggedOut ->
                            ( initUserctx, Cmd.none )
            in
            ( model, Cmd.batch [ getTensionHead apis uctx model.tensionid GotTensionHead, cmd ], Cmd.none )

        LoadTensionComments ->
            ( model, pushTensionPatch apis model.tension_form CommentAck, Cmd.none )

        PushCommentPatch ->
            ( model, patchComment apis model.comment_form CommentPatchAck, Cmd.none )

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

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

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
                                    getTensionRights (uctxFromUser global.session.user) model.tension_head result

                                newFocus =
                                    focusFromNameid newPath.focus.nameid
                            in
                            ( { model | path_data = Success newPath, isTensionAdmin = isAdmin, node_focus = newFocus }
                            , Cmd.batch
                                [ Maybe.map (\did -> send (ScrollToElement did)) model.jumpTo |> withDefault Cmd.none
                                , Cmd.map TreeMenuMsg (send (TreeMenu.OnUpdateFocus newFocus))
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
                            , queryLocalGraph apis nameid (GotPath False)
                            , Cmd.none
                            )

                _ ->
                    ( { model | path_data = result }, Cmd.none, Cmd.none )

        -- Page
        GotTensionHead result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( model, Ports.raiseAuthModal (uctxFromUser global.session.user), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep LoadTensionHead 500, send UpdateUserToken )

                OkAuth th ->
                    ( { model | tension_head = result, subscribe_result = fromMaybeData th.isSubscribed NotAsked }
                    , Cmd.batch
                        [ queryLocalGraph apis th.receiver.nameid (GotPath True)
                        , Ports.bulma_driver ""
                        , Cmd.map ContractsPageMsg (send (ContractsPage.SetRootnameid (nid2rootid th.receiver.nameid)))
                        ]
                    , send (UpdateSessionTensionHead (withMaybeData result))
                    )

                _ ->
                    ( { model | tension_head = result }
                    , Ports.bulma_driver ""
                    , send (UpdateSessionTensionHead (withMaybeData result))
                    )

        GotIsSucribe result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( model, Ports.raiseAuthModal (uctxFromUser global.session.user), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep LoadTensionHead 500, send UpdateUserToken )

                OkAuth d ->
                    let
                        th =
                            withMapData (\x -> { x | isSubscribed = Just d }) model.tension_head
                    in
                    ( { model | subscribe_result = result, tension_head = th }, Cmd.none, send (UpdateSessionTensionHead (withMaybeData th)) )

                _ ->
                    ( { model | subscribe_result = result }, Cmd.none, Cmd.none )

        GotTensionComments result ->
            ( { model | tension_comments = result }, Cmd.none, Ports.bulma_driver "" )

        GotTensionBlobs result ->
            ( { model | tension_blobs = result }, Cmd.none, Ports.bulma_driver "" )

        ExpandEvent i ->
            -- @fix/bulma: dropdown clidk handler lost during the operation
            ( { model | expandedEvents = model.expandedEvents ++ [ i ] }, Cmd.none, Ports.bulma_driver "" )

        ToggleSubscription username ->
            case model.tension_head of
                Success th ->
                    ( { model | subscribe_result = LoadingSlowly }
                    , toggleTensionSubscription apis username model.tensionid (not (withDefault False th.isSubscribed)) GotIsSucribe
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        MarkAsRead eid ->
            ( { model | eid = eid }, markAsRead apis eid True GotMarkAsRead, Cmd.none )

        GotMarkAsRead result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( model, Ports.raiseAuthModal (uctxFromUser global.session.user), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep (MarkAsRead model.eid) 500, send UpdateUserToken )

                OkAuth _ ->
                    ( model, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        -- Page Action
        ChangeTensionPost field value ->
            let
                form =
                    model.tension_form

                newForm =
                    if field == "message" && value == "" then
                        { form | post = Dict.remove field form.post }

                    else
                        { form | post = Dict.insert field value form.post }
            in
            ( { model | tension_form = newForm }, Cmd.none, Cmd.none )

        SubmitComment status_m time ->
            let
                form =
                    model.tension_form

                eventComment =
                    case Dict.get "message" form.post of
                        Just _ ->
                            [ Ev TensionEvent.CommentPushed "" "" ]

                        Nothing ->
                            []

                eventStatus =
                    case status_m of
                        Just TensionStatus.Open ->
                            [ Ev TensionEvent.Reopened "Closed" "Open" ]

                        Just TensionStatus.Closed ->
                            [ Ev TensionEvent.Closed "Open" "Closed" ]

                        Nothing ->
                            []

                newForm =
                    { form
                        | post = Dict.insert "createdAt" (fromTime time) form.post
                        , status = status_m
                        , events = eventComment ++ eventStatus
                    }
            in
            ( { model | tension_form = newForm, tension_patch = LoadingSlowly }
            , send LoadTensionComments
            , Cmd.none
            )

        CommentAck result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | tension_patch = NotAsked }, Ports.raiseAuthModal (uctxFromUser global.session.user), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep LoadTensionComments 500, send UpdateUserToken )

                OkAuth tp ->
                    let
                        events =
                            model.tension_form.events

                        tension_h =
                            case model.tension_head of
                                Success t ->
                                    Success
                                        { t
                                            | status = withDefault t.status model.tension_form.status
                                            , history =
                                                withDefault [] t.history
                                                    ++ (events
                                                            |> List.filter (\e -> e.event_type /= TensionEvent.CommentPushed)
                                                            |> List.map (\e -> eventFromForm e model.tension_form)
                                                       )
                                                    |> Just
                                        }

                                other ->
                                    other

                        tension_c =
                            if List.member TensionEvent.CommentPushed (List.map .event_type events) then
                                case model.tension_comments of
                                    Success t ->
                                        Success { t | comments = Just (withDefault [] t.comments ++ withDefault [] tp.comments) }

                                    other ->
                                        other

                            else
                                model.tension_comments

                        resetForm =
                            initTensionForm model.tensionid global.session.user
                    in
                    ( { model
                        | tension_head = tension_h
                        , tension_comments = tension_c
                        , tension_form = resetForm
                        , tension_patch = result
                      }
                    , Ports.bulma_driver ""
                    , send (UpdateSessionTensionHead (withMaybeData tension_h))
                    )

                _ ->
                    case result of
                        Failure _ ->
                            let
                                form =
                                    model.tension_form

                                resetForm =
                                    { form | status = Nothing }
                            in
                            ( { model | tension_patch = result, tension_form = resetForm }, Cmd.none, Cmd.none )

                        _ ->
                            ( { model | tension_patch = result }, Cmd.none, Cmd.none )

        DoUpdateComment id ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | id = id } }, Ports.focusOn "updateCommentInput", Cmd.none )

        CancelCommentPatch ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | id = "" }, comment_result = NotAsked }, Cmd.none, Ports.bulma_driver "" )

        ChangeCommentPost field value ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | post = Dict.insert field value form.post } }, Cmd.none, Cmd.none )

        SubmitCommentPatch time ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | post = Dict.insert "updatedAt" (fromTime time) form.post }, comment_result = LoadingSlowly }, send PushCommentPatch, Cmd.none )

        CommentPatchAck result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | comment_result = NotAsked }, Ports.raiseAuthModal (uctxFromUser global.session.user), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep PushCommentPatch 500, send UpdateUserToken )

                OkAuth comment ->
                    let
                        tension_c =
                            case model.tension_comments of
                                Success t ->
                                    let
                                        comments =
                                            withDefault [] t.comments

                                        n =
                                            comments
                                                |> LE.findIndex (\c -> c.id == comment.id)
                                                |> withDefault 0
                                    in
                                    Success { t | comments = Just (LE.setAt n comment comments) }

                                other ->
                                    other

                        resetForm =
                            initCommentPatchForm global.session.user
                    in
                    ( { model | tension_comments = tension_c, comment_form = resetForm, comment_result = result }, Cmd.none, Ports.bulma_driver "" )

                _ ->
                    ( { model | comment_result = result }, Cmd.none, Cmd.none )

        DoChangeTitle ->
            ( { model | isTitleEdit = True }, Ports.focusOn "titleInput", Cmd.none )

        CancelTitle ->
            ( { model | isTitleEdit = False, tension_form = initTensionForm model.tensionid global.session.user, title_result = NotAsked }, Cmd.none, Ports.bulma_driver "" )

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
                                (model.tension_head |> withMaybeDataMap (\x -> x.title) |> withDefault "")
                                (Dict.get "title" form.post |> withDefault "")
                            ]
                    }
            in
            ( { model | tension_form = newForm, title_result = LoadingSlowly }, send PushTitle, Cmd.none )

        TitleAck result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | title_result = NotAsked }, Ports.raiseAuthModal (uctxFromUser global.session.user), Cmd.none )

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
                            initTensionForm model.tensionid global.session.user
                    in
                    ( { model | tension_head = tension_h, tension_form = resetForm, title_result = result, isTitleEdit = False }
                    , Ports.bulma_driver ""
                    , send (UpdateSessionTensionHead (withMaybeData tension_h))
                    )

                _ ->
                    ( { model | title_result = result }, Cmd.none, Cmd.none )

        DoMove t ->
            ( model
            , Cmd.batch [ Cmd.map MoveTensionMsg (send (MoveTension.OnOpen t.id t.receiver.nameid (blobFromTensionHead t))) ]
            , Cmd.none
            )

        DoBlobEdit blobType ->
            case model.tension_head of
                Success th ->
                    case th.action of
                        Just action ->
                            case (getTensionCharac action).doc_type of
                                NODE _ ->
                                    let
                                        doc =
                                            NodeDoc.create model.tensionid global.session.user
                                                |> NodeDoc.initBlob blobType (nodeFromTension th)
                                                |> NodeDoc.edit
                                    in
                                    ( { model | nodeDoc = doc }, Cmd.none, Cmd.none )

                                MD ->
                                    -- @Debug: Not implemented
                                    --let
                                    --    doc =
                                    --        MdDoc.create model.tensionid global.session.user
                                    --            |> MdDoc.initBlob blobType (mdFromTension th)
                                    --            |> MdDoc.edit
                                    --in
                                    --( { model | mdDoc = doc }, Cmd.none, Cmd.none )
                                    ( model, Cmd.none, Cmd.none )

                        Nothing ->
                            -- No Document attached or Unknown format
                            ( model, Cmd.none, Cmd.none )

                _ ->
                    -- Loading or Error
                    ( model, Cmd.none, Cmd.none )

        ChangeBlobPost field value ->
            ( { model | nodeDoc = NodeDoc.updatePost field value model.nodeDoc }, Cmd.none, Cmd.none )

        AddResponsabilities ->
            ( { model | nodeDoc = NodeDoc.addResponsabilities model.nodeDoc }, Cmd.none, Cmd.none )

        AddDomains ->
            ( { model | nodeDoc = NodeDoc.addDomains model.nodeDoc }, Cmd.none, Cmd.none )

        AddPolicies ->
            ( { model | nodeDoc = NodeDoc.addPolicies model.nodeDoc }, Cmd.none, Cmd.none )

        ChangeBlobMD value ->
            let
                form =
                    model.tension_form

                newForm =
                    { form | md = Just value }
            in
            ( { model | tension_form = newForm }, Cmd.none, Cmd.none )

        SubmitBlob data time ->
            let
                form =
                    model.tension_form

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
                    ( { model | nodeDoc = NodeDoc.setResult NotAsked model.nodeDoc }, Ports.raiseAuthModal (uctxFromUser global.session.user), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep (PushBlob_ newDoc.form) 500, send UpdateUserToken )

                OkAuth tp ->
                    let
                        th =
                            case model.tension_head of
                                Success t ->
                                    Success
                                        { t
                                            | blobs = ternary (tp.blobs == Nothing) t.blobs tp.blobs
                                            , history =
                                                withDefault [] t.history
                                                    ++ (model.tension_form.events
                                                            |> List.filter (\e -> e.event_type /= TensionEvent.CommentPushed)
                                                            |> List.map (\e -> eventFromForm e model.tension_form)
                                                       )
                                                    |> Just
                                        }

                                other ->
                                    other
                    in
                    ( { model | tension_head = th, nodeDoc = NodeDoc.cancelEdit newDoc }
                    , Ports.bulma_driver ""
                    , send (UpdateSessionTensionHead (withMaybeData th))
                    )

                _ ->
                    ( { model | nodeDoc = newDoc }, Cmd.none, Cmd.none )

        CancelBlob ->
            ( { model | nodeDoc = NodeDoc.cancelEdit model.nodeDoc }
            , Cmd.none
            , Ports.bulma_driver ""
            )

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
                    ( { model | publish_result = NotAsked }, Ports.raiseAuthModal (uctxFromUser global.session.user), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep PublishBlob 500, send UpdateUserToken )

                OkAuth r ->
                    case model.tension_head of
                        Success th ->
                            let
                                blobs =
                                    th.blobs
                                        |> Maybe.map
                                            (\bs ->
                                                let
                                                    -- @debug r.pushedFlag not sync
                                                    pushedFlag =
                                                        Dict.get "createdAt" model.tension_form.post
                                                in
                                                bs
                                                    |> List.head
                                                    |> Maybe.map
                                                        (\b -> [ { b | pushedFlag = pushedFlag } ])
                                            )
                                        |> withDefault Nothing

                                newTh =
                                    { th | blobs = blobs }

                                resetForm =
                                    initTensionForm model.tensionid global.session.user
                            in
                            ( { model
                                | publish_result = result
                                , tension_head = Success newTh
                                , tension_form = resetForm
                              }
                            , Cmd.none
                            , Cmd.batch [ send UpdateUserToken, send (UpdateSessionTensionHead (Just newTh)) ]
                            )

                        _ ->
                            ( { model | publish_result = result }, Cmd.none, Cmd.none )

                _ ->
                    ( { model | publish_result = result }, Cmd.none, Cmd.none )

        -- Assignees
        DoAssigneeEdit ->
            let
                targets =
                    model.path_data |> withMaybeDataMap (\x -> List.map (\y -> y.nameid) x.path) |> withDefault []
            in
            ( model, Cmd.map UserSearchPanelMsg (send (UserSearchPanel.OnOpen targets)), Cmd.none )

        UserSearchPanelMsg msg ->
            let
                ( panel, out ) =
                    UserSearchPanel.update apis msg model.assigneesPanel

                th =
                    Maybe.map
                        (\r ->
                            withMapData
                                (\x ->
                                    let
                                        assignees =
                                            if Tuple.first r == True then
                                                withDefault [] x.assignees ++ [ Tuple.second r ]

                                            else
                                                LE.remove (Tuple.second r) (withDefault [] x.assignees)
                                    in
                                    { x | assignees = Just assignees }
                                )
                                model.tension_head
                        )
                        out.result
                        |> withDefault model.tension_head

                isAssigneeOpen =
                    UserSearchPanel.isOpen_ panel

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | assigneesPanel = panel, tension_head = th, isAssigneeOpen = isAssigneeOpen }
            , out.cmds |> List.map (\m -> Cmd.map UserSearchPanelMsg m) |> List.append cmds |> Cmd.batch
            , Cmd.batch (gcmds ++ [ send (UpdateSessionTensionHead (withMaybeData th)) ])
            )

        -- Labels
        DoLabelEdit ->
            let
                targets =
                    getCircles model.path_data
            in
            ( model, Cmd.map LabelSearchPanelMsg (send (LabelSearchPanel.OnOpen targets False)), Cmd.none )

        LabelSearchPanelMsg msg ->
            let
                ( panel, out ) =
                    LabelSearchPanel.update apis msg model.labelsPanel

                th =
                    Maybe.map
                        (\r ->
                            withMapData
                                (\x ->
                                    let
                                        labels =
                                            if Tuple.first r == True then
                                                withDefault [] x.labels ++ [ Tuple.second r ]

                                            else
                                                LE.remove (Tuple.second r) (withDefault [] x.labels)
                                    in
                                    { x | labels = Just labels }
                                )
                                model.tension_head
                        )
                        out.result
                        |> withDefault model.tension_head

                isLabelOpen =
                    LabelSearchPanel.isOpen_ panel

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | labelsPanel = panel, tension_head = th, isLabelOpen = isLabelOpen }
            , out.cmds |> List.map (\m -> Cmd.map LabelSearchPanelMsg m) |> List.append cmds |> Cmd.batch
            , Cmd.batch (gcmds ++ [ send (UpdateSessionTensionHead (withMaybeData th)) ])
            )

        -- Node Action
        OpenActionPanel domid blob ->
            let
                parentid =
                    model.tension_head |> withMaybeDataMap (\th -> th.receiver.nameid) |> withDefault ""

                tid =
                    model.tensionid

                bid =
                    blob.id

                node =
                    blob.node |> withDefault (initNodeFragment Nothing) |> nodeFromFragment parentid
            in
            ( model, Cmd.map ActionPanelMsg (send <| ActionPanel.OnOpen domid tid bid node), Cmd.none )

        NewTensionMsg msg ->
            let
                ( tf, out ) =
                    NTF.update apis msg model.tensionForm

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | tensionForm = tf }, out.cmds |> List.map (\m -> Cmd.map NewTensionMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        -- Common
        NoMsg ->
            ( model, Cmd.none, Cmd.none )

        LogErr err ->
            ( model, Ports.logErr err, Cmd.none )

        Navigate url ->
            ( model, Cmd.none, Nav.pushUrl global.key url )

        ChangeInputViewMode viewMode ->
            ( { model | inputViewMode = viewMode }, Cmd.none, Cmd.none )

        ChangeUpdateViewMode viewMode ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | viewMode = viewMode } }, Cmd.none, Cmd.none )

        ExpandRoles ->
            ( { model | helperBar = HelperBar.expand model.helperBar }, Cmd.none, Cmd.none )

        CollapseRoles ->
            ( { model | helperBar = HelperBar.collapse model.helperBar }, Cmd.none, Cmd.none )

        ScrollToElement did ->
            ( model, Scroll.scrollToElement did NoMsg, Cmd.none )

        UpdateUctx uctx ->
            ( { model | isTensionAdmin = getTensionRights uctx model.tension_head model.path_data }
            , Cmd.none
            , Cmd.none
            )

        -- Components
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

                th =
                    Maybe.map
                        (\r ->
                            --withMapData (\x -> { x | contracts = Just (Tuple.second r) }) model.tension_head
                            withMapData (\x -> { x | contracts = Just [ { id = x.id } ] }) model.tension_head
                        )
                        out.result
                        |> withDefault model.tension_head

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | contractsPage = data, tension_head = th }
            , out.cmds |> List.map (\m -> Cmd.map ContractsPageMsg m) |> List.append cmds |> Cmd.batch
            , Cmd.batch (gcmds ++ [ send (UpdateSessionTensionHead (withMaybeData th)) ])
            )

        SelectTypeMsg msg ->
            let
                ( data, out ) =
                    SelectType.update apis msg model.selectType

                th =
                    out.result
                        |> Maybe.map
                            (\( _, r ) ->
                                withMapData (\x -> { x | type_ = r }) model.tension_head
                            )
                        |> withDefault model.tension_head

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | selectType = data, tension_head = th }
            , out.cmds |> List.map (\m -> Cmd.map SelectTypeMsg m) |> List.append cmds |> Cmd.batch
            , Cmd.batch (gcmds ++ [ send (UpdateSessionTensionHead (withMaybeData th)) ])
            )

        ActionPanelMsg msg ->
            let
                ( data, out ) =
                    ActionPanel.update apis msg model.actionPanel

                -- Update NodeFragment locally
                th =
                    Maybe.map
                        (\r ->
                            withMapData
                                (\x ->
                                    let
                                        blobs =
                                            Maybe.map (\y -> List.map (\b -> { b | node = Just <| nodeFragmentUpdate b.node r }) y) x.blobs
                                    in
                                    { x | blobs = blobs }
                                )
                                model.tension_head
                        )
                        out.result
                        |> withDefault model.tension_head

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | actionPanel = data, tension_head = th }, out.cmds |> List.map (\m -> Cmd.map ActionPanelMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        JoinOrgaMsg msg ->
            let
                ( data, out ) =
                    JoinOrga.update apis msg model.joinOrga

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
                                if Tuple.first o == True then
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


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ model =
    [ Ports.uctxPD Ports.loadUserCtxFromJs LogErr UpdateUctx
    ]
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        ++ (NTF.subscriptions model.tensionForm |> List.map (\s -> Sub.map NewTensionMsg s))
        ++ (ActionPanel.subscriptions model.actionPanel |> List.map (\s -> Sub.map ActionPanelMsg s))
        ++ (JoinOrga.subscriptions model.joinOrga |> List.map (\s -> Sub.map JoinOrgaMsg s))
        ++ (AuthModal.subscriptions |> List.map (\s -> Sub.map AuthModalMsg s))
        ++ (OrgaMenu.subscriptions |> List.map (\s -> Sub.map OrgaMenuMsg s))
        ++ (UserSearchPanel.subscriptions model.assigneesPanel |> List.map (\s -> Sub.map UserSearchPanelMsg s))
        ++ (LabelSearchPanel.subscriptions model.labelsPanel |> List.map (\s -> Sub.map LabelSearchPanelMsg s))
        ++ (MoveTension.subscriptions |> List.map (\s -> Sub.map MoveTensionMsg s))
        ++ (ContractsPage.subscriptions |> List.map (\s -> Sub.map ContractsPageMsg s))
        ++ (SelectType.subscriptions |> List.map (\s -> Sub.map SelectTypeMsg s))
        ++ (TreeMenu.subscriptions |> List.map (\s -> Sub.map TreeMenuMsg s))
        |> Sub.batch



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    let
        helperData =
            { user = global.session.user
            , uriQuery = global.url.query
            , path_data = withMaybeData model.path_data
            , focus = model.node_focus
            , baseUri = model.baseUri
            , data = model.helperBar
            , onExpand = ExpandRoles
            , onCollapse = CollapseRoles
            , onToggleTreeMenu = TreeMenuMsg TreeMenu.OnToggle
            }
    in
    { title =
        case model.tension_head of
            Success t ->
                t.title

            _ ->
                "Loading..."
    , body =
        [ HelperBar.view helperData
        , div [ id "mainPane" ] [ view_ global model ]
        , Help.view model.empty model.help |> Html.map HelpMsg
        , NTF.view { tree_data = TreeMenu.getOrgaData_ model.treeMenu, path_data = model.path_data } model.tensionForm |> Html.map NewTensionMsg
        , MoveTension.view { tree_data = TreeMenu.getOrgaData_ model.treeMenu } model.moveTension |> Html.map MoveTensionMsg
        , SelectType.view model.empty model.selectType |> Html.map SelectTypeMsg
        , JoinOrga.view model.empty model.joinOrga |> Html.map JoinOrgaMsg
        , AuthModal.view model.empty model.authModal |> Html.map AuthModalMsg
        , OrgaMenu.view model.empty model.orgaMenu |> Html.map OrgaMenuMsg
        , TreeMenu.view model.empty model.treeMenu |> Html.map TreeMenuMsg
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ class "columns is-centered" ]
        [ div [ class "column is-12 is-11-desktop is-9-fullhd" ]
            [ case model.tension_head of
                Success t ->
                    viewTension global.session.user t model

                Failure err ->
                    -- If user has only contract visibility right...
                    --if ContractsPage.hasCid model.contractsPage then
                    --    ContractsPage.view { emitterid = "", receiverid = "", isAdmin = model.isTensionAdmin, now = model.now } model.contractsPage
                    --        |> Html.map ContractsPageMsg
                    --else
                    viewGqlErrors err

                LoadingSlowly ->
                    div [ class "spinner" ] []

                other ->
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
    in
    div [ id "tensionPage" ]
        [ div [ class "columns is-marginless" ]
            -- @DEBUG: width corresponding to is-9 is hard-coded in modal-content (below) to
            -- avoid overflow with no scroll caude by <pre> tag
            [ div [ class "column is-9" ]
                [ h1 [ class "title tensionTitle" ] <|
                    if model.isTitleEdit then
                        let
                            title =
                                Dict.get "title" model.tension_form.post |> withDefault t.title

                            isLoading =
                                model.title_result == LoadingSlowly

                            isSendable =
                                title /= t.title

                            doSubmit =
                                ternary isSendable [ onClick (Submit <| SubmitTitle) ] []
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
                                    , onInput (ChangeTensionPost "title")
                                    ]
                                    []
                                ]
                            , p [ class "control buttons" ]
                                [ button [ class "button is-small", onClick CancelTitle ] [ textH T.cancel ]
                                , button
                                    ([ class "button is-success is-small"
                                     , classList [ ( "is-loading", isLoading ) ]
                                     , disabled (not isSendable)
                                     ]
                                        ++ doSubmit
                                    )
                                    [ textH T.updateTitle ]
                                ]
                            ]
                        , viewMaybeErrors model.title_result
                        ]

                    else
                        [ span [ class "is-human" ] [ text t.title ]
                        , if model.isTensionAdmin || isAuthor then
                            div
                                [ class "button has-text-weight-normal is-pulled-right is-small tooltip has-tooltip-arrow"
                                , attribute "data-tooltip" (upH T.editTitle)
                                , style "vertical-align" "middle" -- @needHelp do not work with pulled right.
                                , onClick DoChangeTitle
                                ]
                                [ A.icon "icon-edit-2" ]

                          else
                            text ""
                        ]
                , div [ class "tensionSubtitle" ]
                    [ span
                        [ class "tag is-rounded is-light"
                        , classList [ ( "is-w", model.isTensionAdmin || isAuthor ) ]
                        , ternary (model.isTensionAdmin || isAuthor) (onClick <| SelectTypeMsg (SelectType.OnOpen t.type_)) (onClick NoMsg)
                        ]
                        [ tensionIcon2 t.type_ ]
                    , if t.type_ /= TensionType.Governance || t.status == TensionStatus.Open then
                        -- As Governance tension get automatically closed when there are created,
                        -- there status is not relevant, I can cause confusion to user as the object exists.
                        span [ class ("is-w tag is-rounded is-" ++ statusColor t.status), onClick (ScrollToElement "tensionCommentInput") ]
                            [ t.status |> TensionStatus.toString |> text ]

                      else
                        text ""
                    , viewTensionDateAndUser model.now "is-discrete" t.createdAt t.createdBy

                    -- @DEBUG: emitter and receiver not used anymore ?
                    --, viewTensionArrow "is-pulled-right" t.emitter t.receiver
                    ]
                ]
            ]
        , div [ class "columns is-centered is-variable is-4" ]
            [ div [ class "column is-9" ]
                [ div [ class "tabs is-md" ]
                    [ ul []
                        [ li [ classList [ ( "is-active", model.activeTab == Conversation ) ] ]
                            [ a [ href (Route.Tension_Dynamic_Dynamic { param1 = model.node_focus.rootnameid, param2 = t.id } |> toHref) ]
                                [ A.icon1 "icon-message-square" (upH T.conversation) ]
                            ]
                        , if t.blobs /= Nothing && t.blobs /= Just [] then
                            li [ classList [ ( "is-active", model.activeTab == Document ) ] ]
                                [ a [ href (Route.Tension_Dynamic_Dynamic_Action { param1 = model.node_focus.rootnameid, param2 = t.id } |> toHref) ]
                                    [ A.icon1 "icon-copy" (upH T.document) ]
                                ]

                          else
                            text ""
                        , if t.contracts /= Nothing && t.contracts /= Just [] then
                            li [ classList [ ( "is-active", model.activeTab == Contracts ) ] ]
                                [ a [ href (Route.Tension_Dynamic_Dynamic_Contract { param1 = model.node_focus.rootnameid, param2 = t.id } |> toHref) ]
                                    [ A.icon1 "icon-link-2" (upH T.contracts) ]
                                ]

                          else
                            text ""
                        ]
                    ]
                , case model.activeTab of
                    Conversation ->
                        viewConversation u t model

                    Document ->
                        case t.blobs |> withDefault [] of
                            [ b ] ->
                                viewDocument u t b model

                            _ ->
                                div [] [ text "No document yet..." ]

                    Contracts ->
                        case t.contracts |> withDefault [] of
                            [ c ] ->
                                ContractsPage.view { emitterid = t.emitter.nameid, receiverid = t.receiver.nameid, isAdmin = model.isTensionAdmin, now = model.now } model.contractsPage
                                    |> Html.map ContractsPageMsg

                            _ ->
                                div [] [ text "No contracts yet..." ]
                ]
            , div [ class "column is-3" ]
                [ viewSidePane u t model ]
            ]
        ]


viewConversation : UserState -> TensionHead -> Model -> Html Msg
viewConversation u t model =
    let
        userInput =
            case u of
                LoggedIn uctx ->
                    -- Author or member can comment tension.
                    if
                        -- is Author
                        uctx.username
                            == t.createdBy.username
                            || -- is Member
                               getOrgaRoles [ t.emitter.nameid, t.receiver.nameid ] uctx.roles
                            /= []
                    then
                        let
                            opNew =
                                { doChangeViewMode = ChangeInputViewMode
                                , doChangePost = ChangeTensionPost
                                , doSubmit = Submit
                                , doSubmitComment = SubmitComment
                                , rows = 7
                                }
                        in
                        viewCommentInput opNew uctx t model.tension_form model.tension_patch model.inputViewMode

                    else
                        viewJoinNeeded model.node_focus

                LoggedOut ->
                    viewJoinNeeded model.node_focus
    in
    case model.tension_comments of
        Success comments ->
            div [ class "comments" ]
                [ Lazy.lazy7 viewComments model.now t.action t.history comments model.comment_form model.comment_result model.expandedEvents
                , hr [ class "has-background-border-light is-2" ] []
                , userInput
                ]

        Failure err ->
            viewGqlErrors err

        LoadingSlowly ->
            div [ class "spinner" ] []

        other ->
            text ""


viewComments :
    Time.Posix
    -> Maybe TensionAction.TensionAction
    -> Maybe (List Event)
    -> TensionComments
    -> CommentPatchForm
    -> GqlData Comment
    -> List Int
    -> Html Msg
viewComments now action history_m comments_m comment_form comment_result expandedEvents =
    let
        comments =
            withDefault [] comments_m.comments

        history =
            withDefault [] history_m

        allEvts =
            -- When event and comment are created at the same time, show the comment first.
            List.indexedMap (\i c -> { type_ = Nothing, createdAt = c.createdAt, i = i, n = 0 }) comments
                ++ List.indexedMap (\i e -> { type_ = Just e.event_type, createdAt = e.createdAt, i = i, n = 0 }) history
                |> List.sortBy .createdAt

        viewCommentOrEvent : { type_ : Maybe TensionEvent.TensionEvent, createdAt : String, i : Int, n : Int } -> Html Msg
        viewCommentOrEvent e =
            case e.type_ of
                Just _ ->
                    case LE.getAt e.i history of
                        Just event ->
                            viewEvent now action event

                        Nothing ->
                            text ""

                Nothing ->
                    case LE.getAt e.i comments of
                        Just c ->
                            let
                                op =
                                    { doUpdate = DoUpdateComment
                                    , doCancelComment = CancelCommentPatch
                                    , doChangeViewMode = ChangeUpdateViewMode
                                    , doChangePost = ChangeCommentPost
                                    , doSubmit = Submit
                                    , doEditComment = SubmitCommentPatch
                                    , now = now
                                    }
                            in
                            viewComment op c comment_form comment_result

                        Nothing ->
                            text ""
    in
    allEvts
        -- Filter events if there a above a given number.
        -- If above, we keep track of the extra number of event
        -- until a non-event (i.e a comment) is met.
        |> LE.indexedFoldr
            (\i e d ->
                let
                    evts =
                        Tuple.first d

                    state =
                        Tuple.second d

                    isAbove =
                        (List.length evts > 6)
                            && (e.type_ /= Nothing)
                            && (evts
                                    |> List.take 6
                                    |> List.filter (\x -> x.type_ == Nothing)
                                    |> List.length
                               )
                            == 0

                    isClicked =
                        state.isClicked || List.member i expandedEvents
                in
                if e.type_ == Just TensionEvent.Created then
                    -- Ignore these type
                    ( evts, state )

                else if isAbove && state.nskip == 0 && isClicked == False then
                    ( evts, { state | nskip = 1, i = i } )

                else if isAbove && state.nskip > 0 && state.isClicked == False then
                    ( evts, { state | nskip = state.nskip + 1 } )

                else if state.nskip > 0 && e.type_ == Nothing && state.isClicked == False then
                    let
                        btn =
                            { type_ = Nothing, n = state.nskip, createdAt = "", i = state.i }
                    in
                    ( [ e ] ++ [ btn ] ++ evts, { state | nskip = 0, isClicked = False } )

                else if e.type_ == Nothing then
                    ( [ e ] ++ evts, { state | nskip = 0, isClicked = False } )

                else
                    ( [ e ] ++ evts, { state | isClicked = isClicked } )
            )
            -- The tuple.first: filterered list of events
            -- The tuple.second: state of the fold loop. We stored the skips when a new comment is
            -- encoutered in order to insert a button later at the current position.
            ( [], { nskip = 0, isCollapsed = True, isClicked = False, i = 0 } )
        |> Tuple.first
        |> List.map
            (\x ->
                if x.n > 0 then
                    div
                        [ class "button is-small actionComment m-4"
                        , attribute "style" "left:10%;"
                        , onClick (ExpandEvent x.i)
                        ]
                        [ toText [ "Show", String.fromInt x.n, "older events" ] ]

                else
                    Lazy.lazy viewCommentOrEvent x
            )
        |> div []


viewEvent : Time.Posix -> Maybe TensionAction.TensionAction -> Event -> Html Msg
viewEvent now action event =
    let
        eventView =
            case event.event_type of
                TensionEvent.Reopened ->
                    viewEventStatus now event TensionStatus.Open

                TensionEvent.Closed ->
                    viewEventStatus now event TensionStatus.Closed

                TensionEvent.TitleUpdated ->
                    viewEventTitle now event

                TensionEvent.TypeUpdated ->
                    viewEventType now event

                TensionEvent.Visibility ->
                    viewEventVisibility now event

                TensionEvent.Authority ->
                    viewEventAuthority now event action

                TensionEvent.AssigneeAdded ->
                    viewEventAssignee now event True

                TensionEvent.AssigneeRemoved ->
                    viewEventAssignee now event False

                TensionEvent.LabelAdded ->
                    viewEventLabel now event True

                TensionEvent.LabelRemoved ->
                    viewEventLabel now event False

                TensionEvent.BlobPushed ->
                    viewEventPushed now event action

                TensionEvent.BlobArchived ->
                    viewEventArchived now event action True

                TensionEvent.BlobUnarchived ->
                    viewEventArchived now event action False

                TensionEvent.MemberLinked ->
                    viewEventMemberLinked now event action

                TensionEvent.MemberUnlinked ->
                    viewEventMemberUnlinked now event action

                TensionEvent.UserJoined ->
                    viewEventUserJoined now event action

                TensionEvent.UserLeft ->
                    viewEventUserLeft now event action

                TensionEvent.Moved ->
                    viewEventMoved now event

                _ ->
                    []
    in
    if eventView == [] then
        text ""

    else
        div [ id event.createdAt, class "media is-paddingless actionComment" ] eventView


viewEventStatus : Time.Posix -> Event -> TensionStatus.TensionStatus -> List (Html Msg)
viewEventStatus now event status =
    let
        ( actionIcon, actionText ) =
            case status of
                TensionStatus.Open ->
                    ( "icon-alert-circle", T.reopened )

                TensionStatus.Closed ->
                    ( "icon-alert-circle", T.closed )
    in
    [ span [ class "media-left", style "margin-left" "-4px" ] [ A.icon (actionIcon ++ " icon-1half has-text-" ++ statusColor status) ]
    , span [ class "media-content", attribute "style" "padding-top: 4px;margin-left: -4px" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, strong [] [ text actionText ], text (formatDate now event.createdAt) ]
        ]
    ]


viewEventTitle : Time.Posix -> Event -> List (Html Msg)
viewEventTitle now event =
    let
        icon =
            A.icon "icon-edit-2"
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, text T.updated, span [ class "is-strong" ] [ text T.title ], text (formatDate now event.createdAt) ]
        , span [ class "ml-3" ]
            [ span [ class "is-strong is-crossed" ] [ event.old |> withDefault "" |> text ]
            , span [ class "arrow-right" ] []
            , span [ class "is-strong" ] [ event.new |> withDefault "" |> text ]
            ]
        ]
    ]


viewEventType : Time.Posix -> Event -> List (Html Msg)
viewEventType now event =
    let
        icon =
            A.icon "icon-edit-2"
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, text T.updated, span [ class "is-strong" ] [ text T.type_ ], text (formatDate now event.createdAt) ]
        , span [ class "ml-3" ]
            [ span [ class "is-strong" ] [ event.old |> withDefault "" |> TensionType.fromString |> withDefault TensionType.Operational |> tensionIcon2 ]
            , span [ class "arrow-right" ] []
            , span [ class "is-strong" ] [ event.new |> withDefault "" |> TensionType.fromString |> withDefault TensionType.Operational |> tensionIcon2 ]
            ]
        ]
    ]


viewEventVisibility : Time.Posix -> Event -> List (Html Msg)
viewEventVisibility now event =
    let
        icon =
            A.icon "icon-lock"
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, text T.updated, span [ class "is-strong" ] [ text T.visibility ], text (formatDate now event.createdAt) ]
        , span [ class "ml-3" ]
            [ span [ class "is-strong" ] [ event.old |> withDefault "" |> text ]
            , span [ class "arrow-right" ] []
            , span [ class "is-strong" ] [ event.new |> withDefault "" |> text ]
            ]
        ]
    ]


viewEventAuthority : Time.Posix -> Event -> Maybe TensionAction.TensionAction -> List (Html Msg)
viewEventAuthority now event action =
    let
        ( icon, eventText ) =
            case tensionAction2NodeType action of
                Just NodeType.Circle ->
                    ( A.icon "icon-key", T.governance )

                Just NodeType.Role ->
                    ( A.icon "icon-key", T.authority )

                _ ->
                    ( A.icon "icon-key", "unknown action" )
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, text T.updated, span [ class "is-strong" ] [ text eventText ], text (formatDate now event.createdAt) ]
        , span [ class "ml-3" ]
            [ span [ class "is-strong" ] [ event.old |> withDefault "" |> text ]
            , span [ class "arrow-right" ] []
            , span [ class "is-strong" ] [ event.new |> withDefault "" |> text ]
            ]
        ]
    ]


viewEventAssignee : Time.Posix -> Event -> Bool -> List (Html Msg)
viewEventAssignee now event isNew =
    let
        icon =
            A.icon "icon-user"

        ( actionText, value ) =
            if isNew then
                ( T.assigned, withDefault "" event.new )

            else
                ( T.unassigned, withDefault "" event.old )
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <|
            List.intersperse (text " ")
                [ viewUsernameLink event.createdBy.username, strong [] [ text actionText ], viewUsernameLink value, text (formatDate now event.createdAt) ]
        ]
    ]


viewEventLabel : Time.Posix -> Event -> Bool -> List (Html Msg)
viewEventLabel now event isNew =
    let
        icon =
            A.icon "icon-tag"

        ( actionText, value ) =
            if isNew then
                ( T.addedThe, withDefault "unknown" event.new )

            else
                ( T.removedThe, withDefault "unknown" event.old )

        label =
            Label "" (SE.leftOfBack "§" value) (SE.rightOfBack "§" value |> Just)
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <|
            List.intersperse (text " ")
                [ viewUsernameLink event.createdBy.username, strong [] [ text actionText ], viewLabel "" label, text "label", text (formatDate now event.createdAt) ]
        ]
    ]


viewEventPushed : Time.Posix -> Event -> Maybe TensionAction.TensionAction -> List (Html Msg)
viewEventPushed now event action_m =
    let
        action =
            withDefault TensionAction.NewRole action_m
    in
    [ div [ class "media-left" ] [ A.icon "icon-share" ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, strong [] [ text T.published ], text T.this, text (action2str action), text (formatDate now event.createdAt) ]
        ]
    ]


viewEventArchived : Time.Posix -> Event -> Maybe TensionAction.TensionAction -> Bool -> List (Html Msg)
viewEventArchived now event action_m isArchived =
    let
        action =
            withDefault TensionAction.NewRole action_m

        ( icon, txt ) =
            if isArchived then
                ( A.icon "icon-archive", T.archived )

            else
                ( i [ class "icon-archive icon-is-slashed" ] [], T.unarchived )
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, strong [] [ text txt ], text T.this, text (action2str action), text (formatDate now event.createdAt) ]
        ]
    ]


viewEventMemberLinked : Time.Posix -> Event -> Maybe TensionAction.TensionAction -> List (Html Msg)
viewEventMemberLinked now event action_m =
    [ div [ class "media-left" ] [ A.icon "icon-user-check has-text-success" ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink (withDefault "" event.new), text T.hasBeen, strong [] [ text T.linked ], text T.toThisRole, text (formatDate now event.createdAt) ]
        ]
    ]


viewEventMemberUnlinked : Time.Posix -> Event -> Maybe TensionAction.TensionAction -> List (Html Msg)
viewEventMemberUnlinked now event action_m =
    [ div [ class "media-left" ] [ A.icon "icon-user has-text-danger" ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink (withDefault "" event.old), text T.hasBeen, strong [] [ text T.unlinked ], text T.toThisRole, text (formatDate now event.createdAt) ]
        ]
    ]


viewEventUserJoined : Time.Posix -> Event -> Maybe TensionAction.TensionAction -> List (Html Msg)
viewEventUserJoined now event action_m =
    let
        action_txt =
            "the organisation"
    in
    [ div [ class "media-left" ] [ A.icon "icon-log-in" ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, strong [] [ text T.joined ], text action_txt, text (formatDate now event.createdAt) ]
        ]
    ]


viewEventUserLeft : Time.Posix -> Event -> Maybe TensionAction.TensionAction -> List (Html Msg)
viewEventUserLeft now event action_m =
    let
        action =
            withDefault TensionAction.NewRole action_m

        action_txt =
            case event.new of
                Just type_ ->
                    case RoleType.fromString type_ of
                        Just RoleType.Guest ->
                            T.theOrganisation

                        _ ->
                            T.role

                Nothing ->
                    action2str action
    in
    [ div [ class "media-left" ] [ A.icon "icon-log-out" ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, strong [] [ text T.left ], text action_txt, text (formatDate now event.createdAt) ]
        ]
    ]


viewEventMoved : Time.Posix -> Event -> List (Html Msg)
viewEventMoved now event =
    let
        action_txt =
            "tension"
    in
    [ div [ class "media-left" ] [ span [ class "arrow-right2 pl-0 pr-0 mr-0" ] [] ]
    , div [ class "media-content" ]
        [ span [] <|
            List.intersperse (text " ")
                [ viewUsernameLink event.createdBy.username
                , strong [] [ text T.moved ]
                , text action_txt
                , text T.from
                , event.old |> Maybe.map (\nid -> viewNodeRefShort OverviewBaseUri nid) |> withDefault (text "unknown")
                , text T.to
                , event.new |> Maybe.map (\nid -> viewNodeRefShort OverviewBaseUri nid) |> withDefault (text "unknown")
                , text (formatDate now event.createdAt)
                ]
        ]
    ]



--
-- </ View Event>
--


viewBlobToolBar : UserState -> TensionHead -> Blob -> Model -> Html Msg
viewBlobToolBar u t b model =
    let
        op =
            { focus = model.node_focus
            , tid = t.id
            , actionView = Just model.actionView
            , blob = b
            , isAdmin = model.isTensionAdmin
            , now = model.now
            , publish_result = model.publish_result
            , onSubmit = Submit
            , onPushBlob = PushBlob
            }
    in
    div [ class "blobToolBar" ]
        [ div [ class "level" ]
            [ div [ class "level-left" ]
                [ DocToolBar.viewToolbar { focus = model.node_focus, tid = t.id, actionView = Just model.actionView } ]
            , div [ class "level-right" ]
                [ DocToolBar.viewStatus op ]
            ]
        , case model.publish_result of
            Failure err ->
                viewGqlErrors err

            _ ->
                text ""
        ]


viewDocument : UserState -> TensionHead -> Blob -> Model -> Html Msg
viewDocument u t b model =
    div []
        [ div [ class "mb-4" ] [ viewBlobToolBar u t b model ]
        , if b.md /= Nothing then
            -- Markdown Document
            case model.actionView of
                _ ->
                    div [] [ text "@todo show markdown" ]

          else
            -- Node Document
            let
                nodeData =
                    { data = Success t.id
                    , node = b.node |> withDefault (initNodeFragment Nothing)
                    , isLazy = False
                    , source = model.baseUri

                    --, focus = model.node_focus
                    , hasBeenPushed = t.history |> withDefault [] |> List.map (\e -> e.event_type) |> List.member TensionEvent.BlobPushed
                    , toolbar = Nothing
                    , receiver = t.receiver.nameid
                    }
            in
            case model.actionView of
                DocView ->
                    NodeDoc.view nodeData Nothing

                DocEdit ->
                    let
                        msgs =
                            { data = model.nodeDoc
                            , result = NotAsked
                            , onBlobEdit = DoBlobEdit
                            , onCancelBlob = CancelBlob
                            , onSubmitBlob = SubmitBlob
                            , onSubmit = Submit
                            , onChangePost = ChangeBlobPost
                            , onAddDomains = AddDomains
                            , onAddPolicies = AddPolicies
                            , onAddResponsabilities = AddResponsabilities
                            }
                    in
                    NodeDoc.view nodeData (Just msgs)

                DocVersion ->
                    NodeDoc.viewVersions model.now model.tension_blobs

                NoView ->
                    text ""
        ]



--
-- Side Pane
--


viewSidePane : UserState -> TensionHead -> Model -> Html Msg
viewSidePane u t model =
    let
        tc_m =
            Maybe.map (\a -> getTensionCharac a) t.action

        assignees =
            t.assignees |> withDefault []

        labels =
            t.labels |> withDefault []

        blob_m =
            t.blobs |> withDefault [] |> List.head

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
                                |> Maybe.map (\n -> n.first_link)
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

        hasBlobRight =
            isAdmin && Maybe.map .action_type tc_m /= Just NEW && blob_m /= Nothing

        rid =
            nid2rootid t.receiver.nameid

        isRoot =
            t.emitter.nameid == rid && t.receiver.nameid == rid
    in
    div [ class "tensionSidePane" ] <|
        [ -- Assignees/User select
          div
            [ class "media"
            , classList [ ( "is-w", hasAssigneeRight ) ]
            , ternary hasAssigneeRight (onClick DoAssigneeEdit) (onClick NoMsg)
            ]
            [ div [ class "media-content" ] <|
                (case u of
                    LoggedIn _ ->
                        [ h2
                            [ class "subtitle is-h" ]
                            [ textH T.assignees
                            , if model.isAssigneeOpen then
                                A.icon "icon-x is-pulled-right"

                              else if hasAssigneeRight then
                                A.icon "icon-settings is-pulled-right"

                              else
                                text ""
                            ]
                        , UserSearchPanel.view
                            { selectedAssignees = t.assignees |> withDefault []
                            , targets = model.path_data |> withMaybeDataMap (\x -> List.map (\y -> y.nameid) x.path) |> withDefault []
                            }
                            model.assigneesPanel
                            |> Html.map UserSearchPanelMsg
                        ]

                    LoggedOut ->
                        [ h2 [ class "subtitle" ] [ textH T.assignees ] ]
                )
                    ++ [ div []
                            [ if List.length assignees > 0 then
                                viewUsers assignees

                              else
                                div [ class "help is-italic" ] [ textH T.noneYet ]
                            ]
                       ]
            ]

        -- Label select
        , div
            [ class "media"
            , classList [ ( "is-w", hasLabelRight ) ]
            , ternary hasLabelRight (onClick DoLabelEdit) (onClick NoMsg)
            ]
            [ div [ class "media-content" ] <|
                (case u of
                    LoggedIn _ ->
                        [ h2 [ class "subtitle is-h" ]
                            [ textH T.labels
                            , if model.isLabelOpen then
                                A.icon "icon-x is-pulled-right"

                              else if hasLabelRight then
                                A.icon "icon-settings is-pulled-right"

                              else
                                text ""
                            ]
                        , LabelSearchPanel.view
                            { selectedLabels = t.labels |> withDefault []
                            , targets = model.path_data |> withMaybeDataMap (\x -> [ shrinkNode x.focus ]) |> withDefault []
                            }
                            model.labelsPanel
                            |> Html.map LabelSearchPanelMsg
                        ]

                    LoggedOut ->
                        [ h2 [ class "subtitle" ] [ textH T.labels ] ]
                )
                    ++ [ div [ class "tension-labelsList" ]
                            [ if List.length labels > 0 then
                                viewLabels labels

                              else
                                div [ class "help is-italic" ] [ textH T.noneYet ]
                            ]
                       ]
            ]

        -- Document
        , Maybe.map2
            (\blob tc ->
                -- Hide if there is no document
                let
                    domid =
                        "actionPanelContent"

                    node =
                        blob_m |> Maybe.map .node |> withDefault Nothing |> withDefault (initNodeFragment Nothing) |> nodeFromFragment t.receiver.nameid
                in
                div [ class "media" ]
                    [ div [ class "media-content" ]
                        [ div
                            [ class "media-content"
                            , classList [ ( "is-w", hasBlobRight || hasRole ) ]
                            , onClick (OpenActionPanel domid blob)
                            ]
                          <|
                            (case u of
                                LoggedIn _ ->
                                    [ div [ id domid ]
                                        [ h2
                                            [ class "subtitle is-h" ]
                                            [ textH T.document
                                            , if ActionPanel.isOpen_ model.actionPanel then
                                                A.icon "icon-x is-pulled-right"

                                              else if hasBlobRight || hasRole then
                                                A.icon "icon-settings is-pulled-right"

                                              else
                                                text ""
                                            ]
                                        , if hasBlobRight || hasRole then
                                            let
                                                panelData =
                                                    { tc = tc
                                                    , isAdmin = hasBlobRight
                                                    , hasRole = hasRole
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
                                    [ h2 [ class "subtitle" ] [ textH T.document ] ]
                            )
                                ++ [ div [ class "level is-mobile" ] <|
                                        case tc.doc_type of
                                            NODE NodeType.Circle ->
                                                [ span [ class "level-item" ] [ A.icon1 (action2icon tc) (SE.humanize (action2str tc.action)) ]
                                                , span [ class "level-item" ] [ A.icon1 (auth2icon tc) (auth2val node tc) ]
                                                , span [ class "level-item" ] [ A.icon1 "icon-eye" (NodeVisibility.toString node.visibility) ]
                                                ]

                                            NODE NodeType.Role ->
                                                [ span [ class "level-item" ] [ A.icon1 (action2icon tc) (SE.humanize (action2str tc.action)) ]
                                                , span [ class "level-item" ] [ A.icon1 (auth2icon tc) (auth2val node tc) ]
                                                ]

                                            MD ->
                                                [ div [ class "help is-italic" ] [ text T.notImplemented ] ]
                                   ]
                                ++ [ Maybe.map
                                        (\fs ->
                                            span [] [ span [ class "is-highlight mr-2" ] [ text "First-link:" ], viewUserFull 0 True False fs ]
                                        )
                                        node.first_link
                                        |> withDefault (text "")
                                   ]
                                ++ [ if tc.action_type == ARCHIVE then
                                        span [ class "has-text-warning" ] [ A.icon1 "icon-archive" T.archived ]

                                     else
                                        text ""
                                   ]
                        , if model.activeTab == Document then
                            text ""

                          else
                            let
                                op =
                                    { focus = model.node_focus
                                    , tid = t.id
                                    , actionView = Just model.actionView
                                    , blob = blob
                                    , isAdmin = model.isTensionAdmin
                                    , now = model.now
                                    , publish_result = model.publish_result
                                    , onSubmit = Submit
                                    , onPushBlob = PushBlob
                                    }
                            in
                            div [ class "mt-3" ] [ DocToolBar.viewStatus op ]
                        ]
                    ]
            )
            blob_m
            tc_m
            |> withDefault (text "")
        , -- Subscriptions
          case u of
            LoggedIn uctx ->
                let
                    ( iconElt, txt ) =
                        case model.tension_head |> withMaybeData |> Maybe.map (\x -> x.isSubscribed) |> withDefault Nothing of
                            Just True ->
                                ( A.icon1 "icon-bell-off icon-1x" (upH T.unsubscribe), T.tensionSubscribeText )

                            Just False ->
                                ( A.icon1 "icon-bell icon-1x" (upH T.subscribe), T.tensionUnsubscribeText )

                            Nothing ->
                                ( text "", "" )
                in
                div [ class "media pb-0" ]
                    [ div [ class "media-content" ]
                        [ h2 [ class "subtitle" ]
                            [ textH T.notifications ]
                        , p
                            [ class "button is-fullwidth has-background-evidence is-small "
                            , style "border-radius" "5px"
                            , onClick (ToggleSubscription uctx.username)
                            ]
                            [ iconElt, loadingSpin (model.subscribe_result == LoadingSlowly) ]
                        , p [ class "help" ] [ textH txt ]
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
                            Maybe.map .doc_type tc_m
                                |> (\x ->
                                        case x of
                                            Just (NODE _) ->
                                                True

                                            _ ->
                                                False
                                   )
                    in
                    [ hr [ class "has-background-border-light" ] [] ]
                        ++ (if not hasNode then
                                [ div
                                    [ class "is-smaller2 has-text-weight-semibold button-light is-link mb-4"
                                    , onClick (DoMove t)
                                    ]
                                    [ span [ class "arrow-right2 pl-0 pr-2" ] [], textH T.moveTension ]
                                ]

                            else
                                []
                           )
                        ++ (if isAdmin && not hasNode then
                                [ div
                                    [ class "is-smaller2 has-text-weight-semibold button-light is-link mb-4"
                                    , onClick <| SelectTypeMsg (SelectType.OnOpen t.type_)
                                    ]
                                    [ A.icon "icon-disc mr-1", text "Change type" ]
                                ]

                            else
                                []
                           )
                        ++ (if isAdmin then
                                [--, div [ class "is-smaller2 has-text-weight-semibold button-light is-link mb-4" ] [ A.icon "icon-lock icon-sm mr-1", text "Lock tension" ]
                                ]

                            else
                                []
                           )

                else
                    []
               )



---- Utils


url2tid : Url -> String
url2tid url =
    url.path |> String.split "/" |> LE.getAt 3 |> withDefault url.path


tensionChanged : Maybe Url -> Url -> Bool
tensionChanged from_m to =
    let
        tid1 =
            from_m
                |> Maybe.map url2tid
                |> withDefault ""

        tid2 =
            url2tid to
    in
    tid1 /= tid2


tensionChanged2 : GqlData TensionHead -> Url -> Bool
tensionChanged2 t_m to =
    let
        tid1 =
            t_m
                |> withMaybeDataMap (\x -> x.id)
                |> withDefault ""

        tid2 =
            url2tid to
    in
    tid1 /= tid2


eventFromForm : Ev -> TensionForm -> Event
eventFromForm event form =
    { id = ""
    , createdAt = Dict.get "createdAt" form.post |> withDefault ""
    , createdBy = Username form.uctx.username
    , event_type = event.event_type
    , old = Just event.old
    , new = Just event.new
    }
