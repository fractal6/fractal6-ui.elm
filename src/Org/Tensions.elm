module Org.Tensions exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Array
import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Browser.Dom as Dom
import Browser.Events as Events
import Browser.Navigation as Nav
import Codecs exposing (QuickDoc)
import Components.ActionPanel as ActionPanel
import Components.AuthModal as AuthModal
import Components.HelperBar as HelperBar exposing (HelperBar)
import Components.JoinOrga as JoinOrga
import Components.LabelSearchPanel as LabelSearchPanel
import Components.MoveTension as MoveTension
import Components.OrgaMenu as OrgaMenu
import Components.TreeMenu as TreeMenu
import Components.UserSearchPanel as UserSearchPanel
import Dict exposing (Dict)
import Extra exposing (space_, ternary, textH, upH)
import Extra.Events exposing (onClickPD, onDragEnd, onDragEnter, onDragLeave, onDragStart, onDrop, onEnter, onKeydown, onTab)
import Extra.Url exposing (queryBuilder, queryParser)
import Fifo exposing (Fifo)
import Form exposing (isPostSendable)
import Form.Help as Help
import Form.NewTension as NTF exposing (NewTensionInput(..), TensionTab(..))
import Fractal.Enum.Lang as Lang
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, li, nav, option, p, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, autocomplete, autofocus, class, classList, disabled, href, id, list, placeholder, rows, selected, style, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading
    exposing
        ( GqlData
        , ModalData
        , RequestResult(..)
        , WebData
        , fromMaybeData
        , fromMaybeWebData
        , isSuccess
        , viewAuthNeeded
        , viewGqlErrors
        , viewHttpErrors
        , withDefaultData
        , withDefaultWebData
        , withMapData
        , withMaybeData
        , withMaybeDataMap
        )
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (ActionType(..), DocType(..), Flags_, FractalBaseRoute(..), NodeFocus, basePathChanged, focusFromNameid, focusState, nameidFromFlags, uriFromNameid)
import ModelCommon.View exposing (mediaTension, tensionIcon2, tensionStatus2String, tensionType2String, tensionTypeColor)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Process
import Query.PatchTension exposing (actionRequest)
import Query.QueryNode exposing (queryLocalGraph)
import Query.QueryTension exposing (queryExtTension, queryIntTension)
import RemoteData exposing (RemoteData)
import Requests exposing (fetchChildren, fetchTensionAll, fetchTensionCount, fetchTensionExt, fetchTensionInt)
import Session exposing (GlobalCmd(..), LabelSearchPanelOnClickAction(..), Screen, UserSearchPanelOnClickAction(..))
import Task
import Text as T
import Time
import Url exposing (Url)



---- PROGRAM ----


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

                    DoPushTension tension ->
                        ( send (PushTension tension), Cmd.none )

                    DoUpdateToken ->
                        ( Cmd.none, send UpdateUserToken )

                    DoUpdateUserSession uctx ->
                        ( Cmd.none, send (UpdateUserSession uctx) )

                    DoUpdateOrgs orgs ->
                        ( Cmd.none, send (UpdateSessionOrgs orgs) )

                    DoCreateTension nameid ->
                        ( Cmd.map NewTensionMsg <| send (NTF.OnOpen (FromNameid nameid)), Cmd.none )

                    -- Tree Data
                    DoUpdateTree tree ->
                        ( Cmd.none, send (UpdateSessionTree tree) )

                    DoUpdatePath path ->
                        ( Cmd.none, send (UpdateSessionPath path) )

                    DoFocus nameid ->
                        ( Cmd.none, send (NavigateNode nameid) )

                    DoFetchNode nameid ->
                        ( Cmd.map TreeMenuMsg <| send (TreeMenu.FetchNewNode nameid False), Cmd.none )

                    DoAddNodes nodes ->
                        ( Cmd.map TreeMenuMsg <| send (TreeMenu.AddNodes nodes), Cmd.none )

                    DoUpdateNode nameid fun ->
                        ( Cmd.map TreeMenuMsg <| send (TreeMenu.UpdateNode nameid fun), Cmd.none )

                    DoDelNodes nameids ->
                        ( Cmd.map TreeMenuMsg <| send (TreeMenu.DelNodes nameids), Cmd.none )

                    DoMoveNode a b c ->
                        ( Cmd.map TreeMenuMsg <| send (TreeMenu.MoveNode a b c), Cmd.none )

                    _ ->
                        ( Cmd.none, Cmd.none )
            )
        |> List.unzip



---- MODEL --


type alias Model =
    { -- Focus
      node_focus : NodeFocus
    , path_data : GqlData LocalGraph
    , children : WebData (List NodeId)

    -- Pages
    , tensions_int : GqlData (List Tension)
    , tensions_ext : GqlData (List Tension)
    , tensions_all : GqlData TensionsDict
    , query : Dict String (List String)
    , offset : Int
    , pattern : Maybe String
    , initPattern : Maybe String
    , viewMode : TensionsView
    , statusFilter : StatusFilter
    , typeFilter : TypeFilter
    , depthFilter : DepthFilter
    , sortFilter : SortFilter
    , authors : List User
    , labels : List Label
    , tensions_count : GqlData TensionsCount

    -- Board
    , boardHeight : Maybe Float
    , hover_column : Maybe String
    , movingTension : Maybe Tension
    , moveFifo : Fifo ( Int, Tension )
    , movingHoverC : Maybe { pos : Int, to_receiverid : String }
    , movingHoverT : Maybe { pos : Int, tid : String, to_receiverid : String }
    , dragCount : Int

    -- Common
    , screen : Screen
    , helperBar : HelperBar
    , refresh_trial : Int
    , url : Url
    , now : Time.Posix
    , lang : Lang.Lang
    , empty : {}

    -- Components
    , help : Help.State
    , tensionForm : NTF.State
    , authorsPanel : UserSearchPanel.State
    , labelsPanel : LabelSearchPanel.State
    , actionPanel : ActionPanel.State
    , moveTension : MoveTension.State
    , joinOrga : JoinOrga.State
    , authModal : AuthModal.State
    , orgaMenu : OrgaMenu.State
    , treeMenu : TreeMenu.State
    }


type TensionDirection
    = InternalTension
    | ExternalTension
    | ListTension



-- Query parameters


queryIsEmpty : Model -> Bool
queryIsEmpty model =
    model.statusFilter
        == defaultStatusFilter
        && model.typeFilter
        == defaultTypeFilter
        && model.depthFilter
        == defaultDepthFilter
        && model.authors
        == defaultAuthorsFilter
        && model.labels
        == defaultLabelsFilter
        && not (Dict.member "q" model.query)


type TensionsView
    = ListView
    | IntExtView
    | CircleView


viewModeEncoder : TensionsView -> String
viewModeEncoder x =
    case x of
        ListView ->
            "list"

        IntExtView ->
            "intext"

        CircleView ->
            "circle"


viewModeDecoder : String -> TensionsView
viewModeDecoder x =
    case x of
        "intext" ->
            IntExtView

        "circle" ->
            CircleView

        _ ->
            ListView


defaultView : String
defaultView =
    "list"


type DepthFilter
    = SelectedNode
    | AllSubChildren


depthFilterList =
    [ AllSubChildren, SelectedNode ]


depthFilterEncoder : DepthFilter -> String
depthFilterEncoder x =
    case x of
        AllSubChildren ->
            "all"

        SelectedNode ->
            "current"


depthFilterDecoder : String -> DepthFilter
depthFilterDecoder x =
    case x of
        "current" ->
            SelectedNode

        _ ->
            AllSubChildren


defaultDepth : String
defaultDepth =
    "all"


defaultDepthFilter =
    AllSubChildren


depthFilter2Text : DepthFilter -> String
depthFilter2Text x =
    case x of
        AllSubChildren ->
            T.depthAll

        SelectedNode ->
            T.depthSelected


type StatusFilter
    = OpenStatus
    | ClosedStatus
    | AllStatus


statusFilterEncoder : StatusFilter -> String
statusFilterEncoder x =
    case x of
        AllStatus ->
            "all"

        ClosedStatus ->
            "closed"

        OpenStatus ->
            "open"


statusFilterDecoder : String -> StatusFilter
statusFilterDecoder x =
    case x of
        "all" ->
            AllStatus

        "closed" ->
            ClosedStatus

        _ ->
            OpenStatus


defaultStatus : String
defaultStatus =
    "open"


defaultStatusFilter =
    OpenStatus


statusFilter2Text : StatusFilter -> String
statusFilter2Text x =
    case x of
        AllStatus ->
            T.all

        OpenStatus ->
            tensionStatus2String TensionStatus.Open

        ClosedStatus ->
            tensionStatus2String TensionStatus.Closed


type TypeFilter
    = AllTypes
    | OneType TensionType.TensionType


typeFilterEncoder : TypeFilter -> String
typeFilterEncoder x =
    case x of
        AllTypes ->
            "all"

        OneType t ->
            TensionType.toString t |> String.toLower


typeFilterDecoder : String -> TypeFilter
typeFilterDecoder x =
    case TensionType.fromString (upH x) of
        Just t ->
            OneType t

        Nothing ->
            AllTypes


defaultType : String
defaultType =
    "all"


defaultTypeFilter =
    AllTypes


typeFilter2Text : TypeFilter -> String
typeFilter2Text x =
    case x of
        AllTypes ->
            T.all

        OneType t ->
            tensionType2String t


type SortFilter
    = NewestSort
    | OldestSort


sortFilterList =
    [ NewestSort, OldestSort ]


sortFilterEncoder : SortFilter -> String
sortFilterEncoder x =
    case x of
        NewestSort ->
            "newest"

        OldestSort ->
            "oldest"


sortFilterDecoder : String -> SortFilter
sortFilterDecoder x =
    case x of
        "oldest" ->
            OldestSort

        _ ->
            NewestSort


defaultSort : String
defaultSort =
    "newest"


defaultSortFilter =
    NewestSort


sortFilter2Text : SortFilter -> String
sortFilter2Text x =
    case x of
        NewestSort ->
            T.newest

        OldestSort ->
            T.oldest



{- Authors parameters -}


authorsEncoder : List User -> List ( String, String )
authorsEncoder authors =
    authors |> List.map (\x -> ( "u", x.username ))


defaultAuthorsFilter =
    []



{- labels parameters -}


labelsEncoder : List Label -> List ( String, String )
labelsEncoder labels =
    labels |> List.map (\x -> ( "l", x.name ))


defaultLabelsFilter =
    []



{- limit -}


nfirstL : Int
nfirstL =
    15


nfirstC : Int
nfirstC =
    -- @debug message/warning if thera are more than nfirstC tensions
    1000


loadEncoder : Int -> String
loadEncoder x =
    String.fromInt x


loadDecoder : String -> Int
loadDecoder x =
    String.toInt x |> withDefault 0


hasLoadMore : GqlData (List Tension) -> Int -> Bool
hasLoadMore tensions offset =
    case tensions of
        Success ts ->
            List.length ts == nfirstL * offset

        _ ->
            False



--
-- DoLoad utils
--


getTargetsHere : Model -> List String
getTargetsHere model =
    case model.depthFilter of
        AllSubChildren ->
            case model.children of
                RemoteData.Success children ->
                    children |> List.map (\x -> x.nameid) |> List.append [ model.node_focus.nameid ]

                _ ->
                    []

        SelectedNode ->
            case model.path_data of
                Success path ->
                    path.focus.children |> List.map (\x -> x.nameid) |> List.append [ path.focus.nameid ]

                _ ->
                    []


statusDecoder : StatusFilter -> Maybe TensionStatus.TensionStatus
statusDecoder statusF =
    case statusF of
        AllStatus ->
            Nothing

        OpenStatus ->
            Just TensionStatus.Open

        ClosedStatus ->
            Just TensionStatus.Closed


typeDecoder : TypeFilter -> Maybe TensionType.TensionType
typeDecoder typeF =
    case typeF of
        AllTypes ->
            Nothing

        OneType t ->
            Just t



---- MSG ----


type Msg
    = PassedSlowLoadTreshold -- timer
    | OnResize Int Int
    | FitBoard (Result Dom.Error Dom.Element)
    | PushTension Tension
    | Submit (Time.Posix -> Msg) -- Get Current Time
      -- Data Queries
    | GotPath Bool (GqlData LocalGraph) -- GraphQL
    | GotChildren (WebData (List NodeId)) -- HTTP/Json
    | GotChildren2 (List String) -- use TreeMenu to get children
    | GotTensionsInt Int (GqlData (List Tension)) -- GraphQL
    | GotTensionsExt (GqlData (List Tension)) -- GraphQL
    | GotTensionsAll (GqlData (List Tension)) -- GraphQL
    | GotTensionsCount (GqlData TensionsCount)
      -- Page Action
    | DoLoadInit
    | DoLoad Bool -- query tensions
    | ChangePattern String
    | ChangeViewFilter TensionsView
    | ChangeStatusFilter StatusFilter
    | ChangeTypeFilter TypeFilter
    | ChangeDepthFilter DepthFilter
    | ChangeSortFilter SortFilter
    | ChangeAuthor
    | ChangeLabel
    | SearchKeyDown Int
    | ResetData
    | OnClearFilter
    | SubmitTextSearch
    | SubmitSearchReset
    | SubmitSearch
    | GoView TensionsView
    | SetOffset Int
      -- Board
    | OnColumnHover (Maybe String)
    | OnMove { pos : Int, to_receiverid : String } Tension
    | OnCancelHov
    | OnEndMove
    | OnMoveEnterC { pos : Int, to_receiverid : String } Bool
    | OnMoveLeaveC
    | OnMoveLeaveC_
    | OnMoveEnterT { pos : Int, tid : String, to_receiverid : String }
    | OnMoveDrop String
      -- Common
    | NoMsg
    | LogErr String
    | Navigate String
    | ExpandRoles
    | CollapseRoles
    | OnGoRoot
    | OpenActionPanel String String (Maybe ( Int, Int ))
      -- Components
    | HelpMsg Help.Msg
    | NewTensionMsg NTF.Msg
    | UserSearchPanelMsg UserSearchPanel.Msg
    | LabelSearchPanelMsg LabelSearchPanel.Msg
    | JoinOrgaMsg JoinOrga.Msg
    | AuthModalMsg AuthModal.Msg
    | OrgaMenuMsg OrgaMenu.Msg
    | TreeMenuMsg TreeMenu.Msg
    | ActionPanelMsg ActionPanel.Msg
    | MoveTensionMsg MoveTension.Msg



---- INIT ----


type alias Flags =
    Flags_


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        apis =
            global.session.apis

        -- Query parameters
        query =
            queryParser global.url

        -- Focus
        newFocus =
            flags
                |> nameidFromFlags
                |> focusFromNameid

        -- What has changed
        fs =
            focusState TensionsBaseUri global.session.referer global.url global.session.node_focus newFocus

        -- Model init
        model =
            { node_focus = newFocus
            , screen = global.session.screen
            , path_data = fromMaybeData global.session.path_data Loading
            , children = fromMaybeWebData global.session.children RemoteData.Loading
            , tensions_int = fromMaybeData global.session.tensions_int Loading
            , tensions_ext = fromMaybeData global.session.tensions_ext Loading
            , tensions_all = fromMaybeData global.session.tensions_all Loading
            , query = query
            , offset = ternary fs.refresh 0 (Dict.get "load" query |> withDefault [] |> List.head |> withDefault "" |> loadDecoder)
            , authorsPanel =
                UserSearchPanel.load global.session.authorsPanel global.session.user
            , labelsPanel =
                LabelSearchPanel.load global.session.labelsPanel global.session.user
            , pattern = Dict.get "q" query |> withDefault [] |> List.head
            , initPattern = Dict.get "q" query |> withDefault [] |> List.head
            , viewMode = Dict.get "v" query |> withDefault [] |> List.head |> withDefault "" |> viewModeDecoder
            , statusFilter = Dict.get "s" query |> withDefault [] |> List.head |> withDefault "" |> statusFilterDecoder
            , typeFilter = Dict.get "t" query |> withDefault [] |> List.head |> withDefault "" |> typeFilterDecoder
            , depthFilter = Dict.get "d" query |> withDefault [] |> List.head |> withDefault "" |> depthFilterDecoder
            , sortFilter = Dict.get "sort" query |> withDefault [] |> List.head |> withDefault "" |> sortFilterDecoder
            , authors = Dict.get "u" query |> withDefault [] |> List.map (\x -> User x Nothing)
            , labels = Dict.get "l" query |> withDefault [] |> List.map (\x -> Label "" x Nothing [])
            , tensions_count = fromMaybeData global.session.tensions_count Loading

            -- Board
            , boardHeight = Nothing
            , hover_column = Nothing
            , movingTension = Nothing
            , moveFifo = Fifo.empty
            , movingHoverC = Nothing
            , movingHoverT = Nothing
            , dragCount = 0

            -- Common
            , helperBar = HelperBar.create
            , help = Help.init global.session.user global.session.screen
            , tensionForm = NTF.init global.session.user global.session.screen
            , refresh_trial = 0
            , url = global.url
            , now = global.now
            , lang = global.session.lang
            , moveTension = MoveTension.init global.session.user
            , empty = {}
            , joinOrga = JoinOrga.init newFocus.nameid global.session.user global.session.screen
            , authModal = AuthModal.init global.session.user (Dict.get "puid" query |> Maybe.map List.head |> withDefault Nothing)
            , orgaMenu = OrgaMenu.init newFocus global.session.orga_menu global.session.orgs_data global.session.user
            , treeMenu = TreeMenu.init TensionsBaseUri global.url.query newFocus global.session.tree_menu global.session.tree_data global.session.user
            , actionPanel = ActionPanel.init global.session.user
            }
                |> (\m ->
                        case TreeMenu.getList_ m.node_focus.nameid m.treeMenu of
                            [] ->
                                m

                            nameids ->
                                { m | children = RemoteData.Success (List.map (\x -> NodeId x Nothing) nameids) }
                   )

        --
        -- Refresh tensions when data are in a Loading state or if
        -- the query just changed (ie referer), regardless the "v" a "load" parameters.
        --
        dataNeedLoad =
            case model.viewMode of
                ListView ->
                    model.tensions_int == Loading

                IntExtView ->
                    model.tensions_int == Loading

                CircleView ->
                    model.tensions_all == Loading

        refresh =
            (fs.refresh || dataNeedLoad)
                || (case global.session.referer of
                        Just referer ->
                            let
                                oldQuery =
                                    queryParser referer
                            in
                            (Dict.remove "v" oldQuery |> Dict.remove "load")
                                /= (Dict.remove "v" query |> Dict.remove "load")

                        --|| Dict.get "v" oldQuery
                        --== Dict.get "v" query
                        Nothing ->
                            True
                   )

        cmds =
            [ if fs.focusChange || model.path_data == Loading then
                [ queryLocalGraph apis newFocus.nameid (GotPath True), send ResetData ]

              else if getTargetsHere model == [] then
                -- path of children has not been loaded
                [ send DoLoadInit ]

              else if refresh then
                [ send (DoLoad False) ]

              else if model.viewMode == CircleView then
                [ Ports.hide "footBar", Task.attempt FitBoard (Dom.getElement "tensionsCircle") ]

              else if not dataNeedLoad && model.offset == 0 then
                -- Assume data are already loaded, actualise offset.
                [ send (SetOffset 1) ]

              else
                []
            , [ sendSleep PassedSlowLoadTreshold 500 ]
            , [ Cmd.map OrgaMenuMsg (send OrgaMenu.OnLoad) ]
            , [ Cmd.map TreeMenuMsg (send TreeMenu.OnLoad) ]
            ]
                |> List.concat
    in
    ( model
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
        PushTension tension ->
            let
                tensions_int =
                    hotTensionPush tension model.tensions_int

                tensions_all =
                    hotTensionPush2 tension model.tensions_all
            in
            ( { model | tensions_int = Success tensions_int, tensions_all = Success tensions_all }
            , Cmd.none
            , Cmd.batch [ send (UpdateSessionTensionsInt (Just tensions_int)), send (UpdateSessionTensionsAll (Just tensions_all)) ]
            )

        PassedSlowLoadTreshold ->
            let
                tensions_int =
                    ternary (model.tensions_int == Loading) LoadingSlowly model.tensions_int

                tensions_ext =
                    ternary (model.tensions_ext == Loading) LoadingSlowly model.tensions_ext

                tensions_all =
                    ternary (model.tensions_all == Loading) LoadingSlowly model.tensions_all

                tensions_count =
                    ternary (model.tensions_count == Loading) LoadingSlowly model.tensions_count
            in
            ( { model | tensions_int = tensions_int, tensions_ext = tensions_ext, tensions_all = tensions_all, tensions_count = tensions_count }, Cmd.none, Cmd.none )

        OnResize w h ->
            let
                screen =
                    global.session.screen

                newScreen =
                    { screen | w = w, h = h }
            in
            ( { model | screen = newScreen }, Task.attempt FitBoard (Dom.getElement "tensionsCircle"), send (UpdateSessionScreen newScreen) )

        FitBoard elt ->
            case elt of
                Ok e ->
                    let
                        h =
                            e.viewport.height - e.element.y
                    in
                    ( { model | boardHeight = Just h }, Cmd.none, Cmd.none )

                Err _ ->
                    ( model, Cmd.none, Cmd.none )

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
                            in
                            ( { model | path_data = Success newPath }
                            , send DoLoadInit
                            , send (UpdateSessionPath (Just newPath))
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

        GotChildren result ->
            case result of
                RemoteData.Success children ->
                    ( { model | children = result }, send (DoLoad False), send (UpdateSessionChildren (Just children)) )

                _ ->
                    ( { model | children = result }, Cmd.none, Cmd.none )

        GotChildren2 nameids ->
            ( { model | children = RemoteData.Success (List.map (\x -> NodeId x Nothing) nameids) }, send (DoLoad False), Cmd.none )

        GotTensionsInt inc result ->
            let
                newTensions =
                    case model.tensions_int of
                        Success tsOld ->
                            case result of
                                Success ts ->
                                    tsOld ++ ts |> Success

                                _ ->
                                    tsOld |> Success

                        _ ->
                            result
            in
            ( { model | tensions_int = newTensions, offset = model.offset + inc }, Cmd.none, send (UpdateSessionTensionsInt (withMaybeData newTensions)) )

        GotTensionsExt result ->
            let
                newTensions =
                    case model.tensions_ext of
                        Success tsOld ->
                            case result of
                                Success ts ->
                                    tsOld ++ ts |> Success

                                _ ->
                                    tsOld |> Success

                        _ ->
                            result
            in
            ( { model | tensions_ext = newTensions }, Cmd.none, send (UpdateSessionTensionsExt (withMaybeData newTensions)) )

        GotTensionsAll result ->
            let
                newResult =
                    withMapData
                        (\data ->
                            --
                            -- Convert a list of tension into a Dict of tension by Receiverid
                            --
                            let
                                addParam : Tension -> Maybe (List Tension) -> Maybe (List Tension)
                                addParam value maybeValues =
                                    case maybeValues of
                                        Just values ->
                                            Just (values ++ [ value ])

                                        Nothing ->
                                            Just [ value ]

                                toDict2 : List ( String, Tension ) -> Dict String (List Tension)
                                toDict2 parameters =
                                    List.foldl
                                        (\( k, v ) dict -> Dict.update k (addParam v) dict)
                                        Dict.empty
                                        parameters
                            in
                            List.map (\x -> ( x.receiver.nameid, x )) data |> toDict2
                        )
                        result
            in
            ( { model | tensions_all = newResult }, Task.attempt FitBoard (Dom.getElement "tensionsCircle"), send (UpdateSessionTensionsAll (withMaybeData newResult)) )

        GotTensionsCount result ->
            ( { model | tensions_count = result }, Cmd.none, send (UpdateSessionTensionsCount (withMaybeData result)) )

        DoLoadInit ->
            ( model
            , case model.depthFilter of
                AllSubChildren ->
                    --fetchChildren apis model.node_focus.nameid GotChildren
                    Cmd.map TreeMenuMsg (send TreeMenu.OnRequireData)

                SelectedNode ->
                    send (DoLoad False)
            , Cmd.none
            )

        DoLoad reset ->
            -- if reset, reset the offset
            -- else increments the results span
            let
                nameids =
                    getTargetsHere model

                status =
                    statusDecoder model.statusFilter

                type_ =
                    typeDecoder model.typeFilter

                sort_ =
                    sortFilterEncoder model.sortFilter |> (\s -> ternary (s == defaultSort) Nothing (Just s))

                offset =
                    -- In other words reset=True, it resets the offset (used by  panel filter (User, Label, etc)
                    ternary reset 0 model.offset

                ( inc, first, skip ) =
                    if offset > 1 && model.tensions_int == Loading && reset == False then
                        -- load a bunch of data (do not increase offset here)
                        ( 0, nfirstL * offset, 0 )

                    else
                        ( 1, nfirstL, offset * nfirstL )
            in
            if nameids == [] then
                ( model, Cmd.none, Cmd.none )

            else if model.viewMode == CircleView then
                ( { model | tensions_all = LoadingSlowly }
                , fetchTensionAll apis nameids nfirstC 0 model.pattern status model.authors model.labels type_ sort_ GotTensionsAll
                , Ports.hide "footBar"
                )

            else if List.member model.viewMode [ ListView, IntExtView ] then
                ( if reset then
                    { model | offset = offset, tensions_int = LoadingSlowly, tensions_ext = LoadingSlowly }

                  else
                    model
                , Cmd.batch
                    [ fetchTensionInt apis nameids first skip model.pattern status model.authors model.labels type_ sort_ (GotTensionsInt inc)

                    -- Note: make tension query only based on tensions_int (receiver). see fractal6.go commit e9cfd8a.
                    --, fetchTensionExt apis nameids first skip model.pattern status model.authors model.labels type_ GotTensionsExt
                    --
                    , fetchTensionCount apis nameids model.pattern model.authors model.labels type_ Nothing GotTensionsCount
                    ]
                , Ports.show "footBar"
                )

            else
                ( model, Cmd.none, Cmd.none )

        ChangePattern value ->
            ( { model | pattern = Just value }, Cmd.none, Cmd.none )

        ChangeViewFilter value ->
            ( { model | viewMode = value }, send SubmitSearch, Cmd.none )

        ChangeStatusFilter value ->
            ( { model | statusFilter = value }, send SubmitSearchReset, Cmd.none )

        ChangeTypeFilter value ->
            ( { model | typeFilter = value }, send SubmitSearchReset, Cmd.none )

        ChangeDepthFilter value ->
            ( { model | depthFilter = value }, send SubmitSearchReset, Cmd.none )

        ChangeSortFilter value ->
            ( { model | sortFilter = value }, send SubmitSearchReset, Cmd.none )

        ChangeAuthor ->
            let
                targets =
                    model.path_data |> withMaybeDataMap (\x -> List.map (\y -> y.nameid) x.path) |> withDefault []
            in
            ( model, Cmd.map UserSearchPanelMsg (send (UserSearchPanel.OnOpen targets)), Cmd.none )

        ChangeLabel ->
            let
                targets =
                    getCircles model.path_data
            in
            ( model, Cmd.map LabelSearchPanelMsg (send (LabelSearchPanel.OnOpen targets True)), Cmd.none )

        SearchKeyDown key ->
            case key of
                13 ->
                    --ENTER
                    ( model, send SubmitTextSearch, Cmd.none )

                27 ->
                    --ESC
                    ( model, send (ChangePattern ""), Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        ResetData ->
            ( { model | offset = 0, tensions_int = Loading, tensions_ext = Loading, tensions_all = Loading, tensions_count = Loading, path_data = Loading }
            , Cmd.none
            , Cmd.batch
                [ send (UpdateSessionTensionsInt Nothing)
                , send (UpdateSessionTensionsExt Nothing)
                , send (UpdateSessionTensionsAll Nothing)
                ]
            )

        OnClearFilter ->
            let
                query =
                    queryBuilder
                        [ ( "v", viewModeEncoder model.viewMode |> (\x -> ternary (x == defaultView) "" x) )
                        , ( "sort", sortFilterEncoder model.sortFilter |> (\x -> ternary (x == defaultSort) "" x) )
                        ]
                        |> (\q -> ternary (q == "") "" ("?" ++ q))
            in
            ( model
            , Cmd.batch [ Nav.pushUrl global.key (uriFromNameid TensionsBaseUri model.node_focus.nameid [] ++ query), send ResetData ]
            , Cmd.none
            )

        SubmitTextSearch ->
            if
                (model.pattern |> withDefault "" |> String.trim)
                    == (Dict.get "q" model.query |> withDefault [] |> List.head |> withDefault "")
            then
                ( model, Cmd.none, Cmd.none )

            else
                ( model, send SubmitSearchReset, Cmd.none )

        SubmitSearchReset ->
            -- Send search and reset the other results
            ( model
            , Cmd.batch [ send SubmitSearch, send ResetData ]
            , Cmd.none
            )

        SubmitSearch ->
            let
                query =
                    queryBuilder
                        ([ ( "q", model.pattern |> withDefault "" |> String.trim )
                         , ( "v", viewModeEncoder model.viewMode |> (\x -> ternary (x == defaultView) "" x) )
                         , ( "s", statusFilterEncoder model.statusFilter |> (\x -> ternary (x == defaultStatus) "" x) )
                         , ( "t", typeFilterEncoder model.typeFilter |> (\x -> ternary (x == defaultType) "" x) )
                         , ( "d", depthFilterEncoder model.depthFilter |> (\x -> ternary (x == defaultDepth) "" x) )
                         , ( "sort", sortFilterEncoder model.sortFilter |> (\x -> ternary (x == defaultSort) "" x) )
                         , ( "load", loadEncoder model.offset |> (\x -> ternary (x == "0" || x == "1") "" x) )
                         ]
                            ++ authorsEncoder model.authors
                            ++ labelsEncoder model.labels
                        )
                        |> (\q -> ternary (q == "") "" ("?" ++ q))
            in
            ( model, Nav.pushUrl global.key (uriFromNameid TensionsBaseUri model.node_focus.nameid [] ++ query), Cmd.none )

        GoView viewMode ->
            let
                ( needLoad, cmds ) =
                    case viewMode of
                        ListView ->
                            ( not (isSuccess model.tensions_int), [ Ports.show "footBar" ] )

                        IntExtView ->
                            ( not (isSuccess model.tensions_int), [ Ports.show "footBar" ] )

                        CircleView ->
                            ( not (isSuccess model.tensions_all), [ Ports.hide "footBar", Task.attempt FitBoard (Dom.getElement "tensionsCircle") ] )
            in
            ( { model | viewMode = viewMode }
            , Cmd.batch (ternary needLoad [ send (DoLoad False) ] [] ++ cmds)
            , Cmd.none
            )

        SetOffset v ->
            ( { model | offset = v }, Cmd.none, Cmd.none )

        OnColumnHover v ->
            ( { model | hover_column = v }, Cmd.none, Cmd.none )

        OnMove c t ->
            ( { model | dragCount = 0, movingHoverC = Just c, movingTension = Just t }, Cmd.none, Cmd.none )

        OnEndMove ->
            let
                cmds =
                    [ sendSleep OnCancelHov 300 ]
            in
            Maybe.map2
                (\t { pos, to_receiverid } ->
                    --if t.id == tid then
                    if t.receiver.nameid == to_receiverid then
                        ( model, Cmd.batch cmds, Cmd.none )

                    else
                        let
                            cmd =
                                Cmd.map MoveTensionMsg (send (MoveTension.OnMoveRaw t.id t.receiver.nameid to_receiverid))

                            j =
                                Maybe.map (\x -> x.pos) model.movingHoverT |> withDefault -1
                        in
                        ( { model | moveFifo = Fifo.insert ( j, t ) model.moveFifo }, Cmd.batch (cmd :: cmds), Cmd.none )
                )
                model.movingTension
                model.movingHoverC
                |> withDefault
                    ( model, Cmd.batch cmds, Cmd.none )

        OnCancelHov ->
            ( { model | movingHoverC = Nothing, movingHoverT = Nothing }, Cmd.none, Cmd.none )

        OnMoveEnterC hover reset ->
            if Just hover == model.movingHoverC then
                if reset then
                    ( { model | movingHoverT = Nothing }, Cmd.none, Cmd.none )

                else
                    ( { model | dragCount = 1 }, Cmd.none, Cmd.none )

            else
                ( { model | dragCount = 1, movingHoverC = Just hover, movingHoverT = Nothing }, Cmd.none, Cmd.none )

        OnMoveLeaveC ->
            ( { model | dragCount = model.dragCount - 1 }, sendSleep OnMoveLeaveC_ 50, Cmd.none )

        OnMoveLeaveC_ ->
            if model.dragCount < 0 then
                ( model, send OnCancelHov, Cmd.none )

            else
                ( model, Cmd.none, Cmd.none )

        OnMoveEnterT hover ->
            ( { model | movingHoverT = Just hover }, Cmd.none, Cmd.none )

        OnMoveDrop nameid ->
            let
                f =
                    Debug.log "move drop" nameid
            in
            ( { model | movingTension = Nothing, movingHoverC = Nothing, movingHoverT = Nothing }
            , Cmd.none
            , Cmd.none
            )

        -- Authors
        UserSearchPanelMsg msg ->
            let
                ( panel, out ) =
                    UserSearchPanel.update apis msg model.authorsPanel

                authors =
                    Maybe.map
                        (\r ->
                            if Tuple.first r == True then
                                model.authors ++ [ Tuple.second r ]

                            else
                                List.filter (\x -> x.username /= (Tuple.second r).username) model.authors
                        )
                        out.result
                        |> withDefault model.authors

                ( cmds_, gcmds ) =
                    mapGlobalOutcmds out.gcmds

                cmds =
                    if model.authors /= authors then
                        -- Without sendSleep, the loadingSpin doesnt got activated (race condition with SessionUpdate)
                        cmds_ ++ [ sendSleep SubmitSearchReset 100 ]

                    else
                        cmds_
            in
            ( { model | authorsPanel = panel, authors = authors }
            , out.cmds |> List.map (\m -> Cmd.map UserSearchPanelMsg m) |> List.append cmds |> Cmd.batch
            , Cmd.batch (gcmds ++ [ panel |> UserSearchPanel.getModel |> Just |> UpdateSessionAuthorsPanel |> send ])
            )

        -- Labels
        LabelSearchPanelMsg msg ->
            let
                ( panel, out ) =
                    LabelSearchPanel.update apis msg model.labelsPanel

                labels =
                    Maybe.map
                        (\r ->
                            if Tuple.first r == True then
                                model.labels ++ [ Tuple.second r ]

                            else
                                List.filter (\x -> x.name /= (Tuple.second r).name) model.labels
                        )
                        out.result
                        |> withDefault model.labels

                ( cmds_, gcmds ) =
                    mapGlobalOutcmds out.gcmds

                cmds =
                    if model.labels /= labels then
                        -- Without sendSleep, the loadingSpin doesnt got activated (race condition with SessionUpdate)
                        cmds_ ++ [ sendSleep SubmitSearchReset 100 ]

                    else
                        cmds_
            in
            ( { model | labelsPanel = panel, labels = labels }
            , out.cmds |> List.map (\m -> Cmd.map LabelSearchPanelMsg m) |> List.append cmds |> Cmd.batch
            , Cmd.batch (gcmds ++ [ panel |> LabelSearchPanel.getModel |> Just |> UpdateSessionLabelsPanel |> send ])
            )

        -- Common
        NoMsg ->
            ( model, Cmd.none, Cmd.none )

        LogErr err ->
            ( model, Ports.logErr err, Cmd.none )

        Navigate url ->
            ( model, Cmd.none, Nav.pushUrl global.key url )

        ExpandRoles ->
            ( { model | helperBar = HelperBar.expand model.helperBar }, Cmd.none, Cmd.none )

        CollapseRoles ->
            ( { model | helperBar = HelperBar.collapse model.helperBar }, Cmd.none, Cmd.none )

        OnGoRoot ->
            let
                node_focus =
                    model.node_focus
            in
            ( { model | node_focus = { node_focus | nameid = node_focus.rootnameid } }, send SubmitSearchReset, Cmd.none )

        OpenActionPanel domid nameid pos ->
            ( model, Cmd.map ActionPanelMsg (send <| ActionPanel.OnOpen domid nameid (TreeMenu.getOrgaData_ model.treeMenu) pos), Cmd.none )

        -- Components
        NewTensionMsg msg ->
            let
                ( tf, out ) =
                    NTF.update apis msg model.tensionForm

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

                -- reload silently the page if needed
                cmds_extra =
                    out.result
                        |> Maybe.map
                            (\o ->
                                if Tuple.first o == True then
                                    [ Nav.replaceUrl global.key (Url.toString model.url) ]

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

                extra_cmd =
                    if out.result == Just ( True, True ) then
                        send (GotChildren2 (TreeMenu.getList_ model.node_focus.nameid data))

                    else
                        Cmd.none
            in
            ( { model | treeMenu = data }, out.cmds |> List.map (\m -> Cmd.map TreeMenuMsg m) |> List.append (extra_cmd :: cmds) |> Cmd.batch, Cmd.batch gcmds )

        ActionPanelMsg msg ->
            let
                ( data, out ) =
                    ActionPanel.update apis msg model.actionPanel

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | actionPanel = data }, out.cmds |> List.map (\m -> Cmd.map ActionPanelMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        MoveTensionMsg msg ->
            let
                ( data, out ) =
                    MoveTension.update apis msg model.moveTension

                ( tensions_all, moveFifo ) =
                    Maybe.map
                        (\( tid, ( old_nid, new_nid, _ ) ) ->
                            let
                                ( move, moveFifo_ ) =
                                    Fifo.remove model.moveFifo
                            in
                            ( withMapData
                                -- Add the moved tension to list
                                (Dict.update new_nid
                                    (\ts_m ->
                                        Maybe.map2
                                            (\ts ( pos, tension ) ->
                                                let
                                                    r =
                                                        tension.receiver

                                                    t =
                                                        { tension | receiver = { r | nameid = new_nid } }
                                                in
                                                if pos < 0 then
                                                    ts ++ [ t ]

                                                else
                                                    LE.splitAt pos ts |> (\( a, b ) -> a ++ [ t ] ++ b)
                                            )
                                            ts_m
                                            move
                                    )
                                    -- Remove the moved tension from list
                                    >> Dict.update old_nid (Maybe.map (List.filter (\t -> t.id /= tid)))
                                )
                                model.tensions_all
                            , moveFifo_
                            )
                        )
                        (Maybe.map Tuple.second out.result |> withDefault Nothing)
                        |> withDefault ( model.tensions_all, model.moveFifo )

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | moveFifo = moveFifo, tensions_all = tensions_all, moveTension = data }, out.cmds |> List.map (\m -> Cmd.map MoveTensionMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ model =
    [ Events.onResize (\w h -> OnResize w h)
    ]
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        ++ (NTF.subscriptions model.tensionForm |> List.map (\s -> Sub.map NewTensionMsg s))
        ++ (UserSearchPanel.subscriptions model.authorsPanel |> List.map (\s -> Sub.map UserSearchPanelMsg s))
        ++ (LabelSearchPanel.subscriptions model.labelsPanel |> List.map (\s -> Sub.map LabelSearchPanelMsg s))
        ++ (JoinOrga.subscriptions model.joinOrga |> List.map (\s -> Sub.map JoinOrgaMsg s))
        ++ (AuthModal.subscriptions |> List.map (\s -> Sub.map AuthModalMsg s))
        ++ (OrgaMenu.subscriptions |> List.map (\s -> Sub.map OrgaMenuMsg s))
        ++ (TreeMenu.subscriptions |> List.map (\s -> Sub.map TreeMenuMsg s))
        ++ (ActionPanel.subscriptions model.actionPanel |> List.map (\s -> Sub.map ActionPanelMsg s))
        ++ (MoveTension.subscriptions |> List.map (\s -> Sub.map MoveTensionMsg s))
        |> Sub.batch



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    let
        helperData =
            { user = global.session.user
            , uriQuery = model.url.query
            , path_data = withMaybeData model.path_data
            , focus = model.node_focus
            , baseUri = TensionsBaseUri
            , data = model.helperBar
            , onExpand = ExpandRoles
            , onCollapse = CollapseRoles
            , onToggleTreeMenu = TreeMenuMsg TreeMenu.OnToggle
            , onJoin = JoinOrgaMsg (JoinOrga.OnOpen model.node_focus.rootnameid JoinOrga.JoinOne)
            , onOpenPanel = ternary (ActionPanel.isOpen_ "actionPanelHelper" model.actionPanel) (\_ _ _ -> NoMsg) OpenActionPanel
            }

        panelData =
            { tc = { action = TensionAction.EditRole, action_type = EDIT, doc_type = NODE NodeType.Role }
            , isRight = True
            , domid = "actionPanelHelper"
            , tree_data = TreeMenu.getOrgaData_ model.treeMenu
            }
    in
    { title = "Tensions · " ++ (String.join "/" <| LE.unique [ model.node_focus.rootnameid, model.node_focus.nameid |> String.split "#" |> LE.last |> withDefault "" ])
    , body =
        [ div [ class "orgPane" ]
            [ HelperBar.view helperData
            , div [ id "mainPane" ]
                [ view_ global model
                , if model.viewMode == CircleView then
                    viewCircleTensions model

                  else
                    text ""
                ]
            ]
        , Help.view model.empty model.help |> Html.map HelpMsg
        , NTF.view { tree_data = TreeMenu.getOrgaData_ model.treeMenu, path_data = model.path_data } model.tensionForm |> Html.map NewTensionMsg
        , JoinOrga.view model.empty model.joinOrga |> Html.map JoinOrgaMsg
        , AuthModal.view model.empty model.authModal |> Html.map AuthModalMsg
        , OrgaMenu.view model.empty model.orgaMenu |> Html.map OrgaMenuMsg
        , TreeMenu.view model.empty model.treeMenu |> Html.map TreeMenuMsg
        , ActionPanel.view panelData model.actionPanel |> Html.map ActionPanelMsg
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    let
        isFullwidth =
            model.viewMode == CircleView
    in
    div [ id "tensions", class "columns is-centered is-marginless" ]
        [ div [ class "column is-12 is-11-desktop is-9-fullhd", classList [ ( "pb-0", isFullwidth ) ] ]
            [ div [ class "columns is-centered", classList [ ( "mb-1", isFullwidth == False ), ( "mb-0", isFullwidth ) ] ]
                [ div [ class "column is-12", classList [ ( "pb-0", isFullwidth ) ] ] [ viewSearchBar model ] ]
            , case model.children of
                RemoteData.Failure err ->
                    viewHttpErrors err

                _ ->
                    text ""
            , case model.viewMode of
                ListView ->
                    viewListTensions model

                IntExtView ->
                    viewIntExtTensions model

                CircleView ->
                    text ""
            , if model.viewMode == CircleView && (withMaybeData model.tensions_all |> Maybe.map (\x -> (Dict.values x |> List.concat |> List.length) == 1000) |> withDefault False) then
                div [ class "column is-12  is-aligned-center", attribute "style" "margin-left: 0.5rem;" ]
                    [ button [ class "button is-small" ]
                        -- @TODO: load more for CircleView
                        [ text "Viewing only the 1000 more recent tensions" ]
                    ]

              else if model.viewMode /= CircleView && (hasLoadMore model.tensions_int model.offset || hasLoadMore model.tensions_ext model.offset) then
                div [ class "column is-12 is-aligned-center", attribute "style" "margin-left: 0.5rem;" ]
                    [ button [ class "button is-small", onClick (DoLoad False) ]
                        [ text T.showMore ]
                    ]

              else
                text ""
            ]
        ]


viewSearchBar : Model -> Html Msg
viewSearchBar model =
    let
        checked =
            A.icon1 "icon-check has-text-success" ""

        unchecked =
            A.icon1 "icon-check has-text-success is-invisible" ""

        clearFilter =
            if queryIsEmpty model then
                text ""

            else
                span
                    [ class "tag is-rounded is-small is-danger is-light button-light"
                    , attribute "style" "margin: 0.35rem;"
                    , onClick OnClearFilter
                    ]
                    [ text T.clearFilters ]
    in
    div [ id "searchBarTensions", class "searchBar" ]
        [ div [ class "columns mt-0 mb-0" ]
            [ div [ class "column is-5" ]
                [ div [ class "field has-addons" ]
                    [ div [ class "control  is-expanded" ]
                        [ input
                            [ class "is-rounded input is-small pr-6"
                            , type_ "search"
                            , autocomplete False
                            , autofocus False
                            , placeholder T.searchTensions
                            , value (withDefault "" model.pattern)
                            , onInput ChangePattern
                            , onKeydown SearchKeyDown
                            ]
                            []
                        , span [ class "icon-input-flex-right" ]
                            [ span [ class "vbar has-border-color" ] []
                            , span [ class "button-light is-w px-1", onClick (SearchKeyDown 13) ]
                                [ A.icon "icon-search" ]
                            ]
                        ]
                    ]
                ]
            , div [ class "column is-7 flex-gap" ]
                [ div [ class "field has-addons filterBar mb-0" ]
                    [ div [ class "control dropdown" ]
                        [ div [ class "is-small button dropdown-trigger", attribute "aria-controls" "status-filter" ]
                            [ ternary (model.statusFilter /= defaultStatusFilter) (span [ class "badge is-link2" ] []) (text "")
                            , text T.status
                            , A.icon "ml-2 icon-chevron-down1 icon-tiny"
                            ]
                        , div [ id "status-filter", class "dropdown-menu", attribute "role" "menu" ]
                            [ div
                                [ class "dropdown-content" ]
                                [ div [ class "dropdown-item button-light", onClick <| ChangeStatusFilter AllStatus ]
                                    [ ternary (model.statusFilter == AllStatus) checked unchecked, text (statusFilter2Text AllStatus) ]
                                , div [ class "dropdown-item button-light", onClick <| ChangeStatusFilter OpenStatus ]
                                    [ ternary (model.statusFilter == OpenStatus) checked unchecked, text (statusFilter2Text OpenStatus) ]
                                , div [ class "dropdown-item button-light", onClick <| ChangeStatusFilter ClosedStatus ]
                                    [ ternary (model.statusFilter == ClosedStatus) checked unchecked, text (statusFilter2Text ClosedStatus) ]
                                ]
                            ]
                        ]
                    , div [ class "control dropdown" ]
                        [ div [ class "is-small button dropdown-trigger", attribute "aria-controls" "type-filter" ]
                            [ ternary (model.typeFilter /= defaultTypeFilter) (span [ class "badge is-link2" ] []) (text "")
                            , text T.type_
                            , A.icon "ml-2 icon-chevron-down1 icon-tiny"
                            ]
                        , div [ id "type-filter", class "dropdown-menu", attribute "role" "menu" ]
                            [ div
                                [ class "dropdown-content" ]
                                ([ div [ class "dropdown-item button-light", onClick <| ChangeTypeFilter AllTypes ]
                                    [ ternary (model.typeFilter == AllTypes) checked unchecked, text (typeFilter2Text AllTypes) ]
                                 ]
                                    ++ List.map
                                        (\t ->
                                            div [ class "dropdown-item button-light", onClick <| ChangeTypeFilter (OneType t) ]
                                                [ ternary (model.typeFilter == OneType t) checked unchecked, span [ class (tensionTypeColor "text" t) ] [ text (typeFilter2Text (OneType t)) ] ]
                                        )
                                        TensionType.list
                                )
                            ]
                        ]
                    , div [ class "control", onClick ChangeLabel ]
                        [ div [ class "is-small button" ]
                            [ ternary (model.labels /= defaultLabelsFilter) (span [ class "badge is-link2" ] []) (text "")
                            , text T.label
                            , A.icon "ml-2 icon-chevron-down1 icon-tiny"
                            ]
                        , LabelSearchPanel.view
                            { selectedLabels = model.labels
                            , targets = model.path_data |> withMaybeDataMap (\x -> [ shrinkNode x.focus ]) |> withDefault []
                            , isRight = False
                            }
                            model.labelsPanel
                            |> Html.map LabelSearchPanelMsg
                        ]
                    , div [ class "control", onClick ChangeAuthor ]
                        [ div [ class "is-small button" ]
                            [ ternary (model.authors /= defaultAuthorsFilter) (span [ class "badge is-link2" ] []) (text "")
                            , text T.author
                            , A.icon "ml-2 icon-chevron-down1 icon-tiny"
                            ]
                        , UserSearchPanel.view
                            { selectedAssignees = model.authors
                            , targets = model.path_data |> withMaybeDataMap (\x -> List.map (\y -> y.nameid) x.path) |> withDefault []
                            , isRight = True
                            }
                            model.authorsPanel
                            |> Html.map UserSearchPanelMsg
                        ]
                    , div [ class "control dropdown" ]
                        [ div [ class "is-small button dropdown-trigger", attribute "aria-controls" "depth-filter" ]
                            [ ternary (model.depthFilter /= defaultDepthFilter) (span [ class "badge is-link2" ] []) (text "")
                            , text T.depth
                            , A.icon "ml-2 icon-chevron-down1 icon-tiny"
                            ]
                        , div [ id "depth-filter", class "dropdown-menu is-right", attribute "role" "menu" ]
                            [ div [ class "dropdown-content" ] <|
                                List.map
                                    (\t ->
                                        div [ class "dropdown-item button-light", onClick <| ChangeDepthFilter t ]
                                            [ ternary (model.depthFilter == t) checked unchecked, text (depthFilter2Text t) ]
                                    )
                                    depthFilterList
                            ]
                        ]
                    ]
                , clearFilter
                ]
            ]
        , div [ class "tabs no-overflow is-md" ]
            [ ul []
                [ li [ classList [ ( "is-active", model.viewMode == ListView ) ] ]
                    [ a [ onClickPD (ChangeViewFilter ListView), target "_blank" ]
                        --[ a [ onClickPD (GoView ListView), target "_blank" ]
                        [ div [ class "tooltip has-tooltip-right has-tooltip-arrow", attribute "data-tooltip" T.tensionsListTooltip ] [ text T.list ] ]
                    ]

                --, li [ classList [ ( "is-active", model.viewMode == IntExtView ) ] ]
                --    [ a [ onClickPD (ChangeViewFilter IntExtView), target "_blank" ]
                --        --[ a [ onClickPD (GoView IntExtView), target "_blank" ]
                --        [ div [ class "tooltip has-tooltip-right has-tooltip-arrow", attribute "data-tooltip" T.tensionsIntExtTooltip ] [ text "Internal/External" ] ]
                --    ]
                , li [ classList [ ( "is-active", model.viewMode == CircleView ) ] ]
                    [ a [ onClickPD (ChangeViewFilter CircleView), target "_blank" ]
                        --[ a [ onClickPD (GoView CircleView), target "_blank" ]
                        [ div [ class "tooltip has-tooltip-right has-tooltip-arrow", attribute "data-tooltip" T.tensionsCircleTooltip ] [ text T.byCircle ] ]
                    ]
                ]
            ]
        ]


viewTensionsListHeader : Model -> Html Msg
viewTensionsListHeader model =
    let
        checked =
            A.icon1 "icon-check has-text-success" ""

        unchecked =
            A.icon1 "icon-check has-text-success is-invisible" ""
    in
    div
        [ class "pt-3 pb-3 has-border-light has-background-header"
        , attribute "style" "border-top-left-radius: 6px; border-top-right-radius: 6px;"
        ]
        [ div
            [ class "level is-marginless is-mobile"
            ]
            [ div [ class "level-left px-3" ]
                [ viewTensionsCount model
                , if model.node_focus.nameid /= model.node_focus.rootnameid then
                    span [ class "is-hidden-mobile help-label button-light is-h is-discrete px-5 pb-2", onClick OnGoRoot ] [ A.icon "arrow-up", text T.goRoot ]

                  else
                    text ""
                ]
            , div [ class "level-right px-3" ]
                [ div [ class "control dropdown" ]
                    [ div [ class "dropdown-trigger button-light is-h is-size-7 has-text-weight-semibold", attribute "aria-controls" "sort-filter" ]
                        [ text T.sort
                        , A.icon "ml-1 icon-chevron-down1 icon-tiny"
                        ]
                    , div [ id "sort-filter", class "dropdown-menu is-right", attribute "role" "menu" ]
                        [ div [ class "dropdown-content" ] <|
                            List.map
                                (\t ->
                                    div [ class "dropdown-item button-light", onClick <| ChangeSortFilter t ]
                                        [ ternary (model.sortFilter == t) checked unchecked, t |> sortFilter2Text |> text ]
                                )
                                sortFilterList
                        ]
                    ]
                ]
            ]
        , if model.node_focus.nameid /= model.node_focus.rootnameid then
            div [ class "is-hidden-tablet help-label button-light is-h is-discrete px-5 pb-2", onClick OnGoRoot ] [ A.icon "arrow-up", text T.goRoot ]

          else
            text ""
        ]


viewTensionsCount : Model -> Html Msg
viewTensionsCount model =
    case model.tensions_count of
        Success c ->
            let
                activeCls =
                    "is-hovered has-text-weight-semibold"

                inactiveCls =
                    "has-background-header"
            in
            div [ class "buttons has-addons m-0" ]
                [ div
                    [ class "button is-rounded is-small"
                    , classList [ ( activeCls, model.statusFilter == OpenStatus ), ( inactiveCls, model.statusFilter /= OpenStatus ) ]
                    , onClick <| ChangeStatusFilter OpenStatus
                    ]
                    [ span [] [ c.open |> String.fromInt |> text ], text (space_ ++ T.openTension) ]
                , div
                    [ class "button is-rounded is-small"
                    , classList [ ( activeCls, model.statusFilter == ClosedStatus ), ( inactiveCls, model.statusFilter /= ClosedStatus ) ]
                    , onClick <| ChangeStatusFilter ClosedStatus
                    ]
                    [ c.closed |> String.fromInt |> text, text (space_ ++ T.closedTension) ]
                ]

        LoadingSlowly ->
            div [ class "buttons has-addons m-0" ]
                [ button [ class "button is-rounded is-small" ] [ text T.openTension ]
                , button [ class "button is-rounded is-small" ] [ text T.closedTension ]
                ]

        _ ->
            div [] []


viewListTensions : Model -> Html Msg
viewListTensions model =
    let
        t1 =
            model.tensions_int |> withDefaultData []

        t2 =
            model.tensions_ext |> withDefaultData []

        tensions_d =
            case t1 ++ t2 of
                [] ->
                    model.tensions_int

                other ->
                    --other |> List.sortBy .createdAt |> (\l -> ternary (model.sortFilter == defaultSortFilter) l (List.reverse l) ) |> Success
                    other |> (\l -> ternary (model.sortFilter == defaultSortFilter) l (List.reverse l)) |> Success
    in
    div [ class "columns is-centered" ]
        [ div [ class "column is-12" ]
            [ viewTensionsListHeader model
            , viewTensions model.lang model.now model.node_focus model.initPattern tensions_d ListTension
            ]
        ]


viewIntExtTensions : Model -> Html Msg
viewIntExtTensions model =
    div [ class "columns is-centered" ]
        [ div [ class "column is-6-desktop is-5-fullhd" ]
            [ h2 [ class "subtitle has-text-weight-semibold has-text-centered" ] [ text "Internal tensions" ]
            , viewTensions model.lang model.now model.node_focus model.initPattern model.tensions_int InternalTension
            ]
        , div [ class "vline" ] []
        , div [ class "column is-6-desktop is-5-fullhd" ]
            [ h2 [ class "subtitle has-text-weight-semibold has-text-centered" ] [ text "External tensions" ]
            , viewTensions model.lang model.now model.node_focus model.initPattern model.tensions_ext ExternalTension
            ]
        ]


viewCircleTensions : Model -> Html Msg
viewCircleTensions model =
    let
        -- Should alway be loaded here !
        children =
            withDefaultWebData [] model.children
    in
    case model.tensions_all of
        Success data ->
            children
                |> List.filter
                    (\n ->
                        -- Ignore circle with no tensions
                        case Dict.get n.nameid data of
                            Nothing ->
                                False

                            Just [] ->
                                False

                            _ ->
                                True
                    )
                |> List.indexedMap
                    (\i n ->
                        let
                            tensions =
                                Dict.get n.nameid data |> withDefault []

                            j_last =
                                List.length tensions - 1

                            t_m =
                                List.head tensions

                            rcv_name_m =
                                Maybe.map (.receiver >> .name) t_m
                        in
                        [ div
                            [ class "column is-3"
                            , onDragEnter (OnMoveEnterC { pos = i, to_receiverid = n.nameid } False)
                            , onDragLeave OnMoveLeaveC

                            -- @DEBUG not working
                            --, onDrop (OnMoveDrop n.nameid)
                            , attribute "ondragover" "return false"

                            --, onMouseEnter (OnColumnHover (Just n.nameid)
                            ]
                            [ div [ class "subtitle is-aligned-center mb-0 pb-3" ]
                                [ rcv_name_m |> withDefault "Loading..." |> text
                                , span
                                    [ class "tag is-rounded button-light is-w has-border is-pulled-right ml-1"

                                    --, classList [ ( "is-invisible", model.hover_column /= Just n.nameid ) ]
                                    , onClick (NewTensionMsg (NTF.OnOpen (FromNameid n.nameid)))
                                    ]
                                    [ A.icon "icon-plus" ]
                                ]
                            , tensions
                                --|> List.sortBy .createdAt
                                |> (\l -> ternary (model.sortFilter == defaultSortFilter) l (List.reverse l))
                                |> List.indexedMap
                                    (\j t ->
                                        let
                                            draggedTid =
                                                Maybe.map .id model.movingTension

                                            itemDragged =
                                                draggedTid == Just t.id

                                            upperTid =
                                                LE.getAt (j - 1) tensions |> Maybe.map .id

                                            isHoveredUp =
                                                -- exclude the dragged item
                                                not itemDragged
                                                    -- hovered tension
                                                    && (Maybe.map .tid model.movingHoverT == Just t.id)
                                                    -- exclude if the dragged item is next
                                                    && (draggedTid /= upperTid)

                                            hasLastColumn =
                                                -- exclude the dragged item
                                                not itemDragged
                                                    -- nothing to drag
                                                    && (model.movingHoverT == Nothing)
                                                    -- last item
                                                    && (j_last == j && Maybe.map .pos model.movingHoverC == Just i)

                                            draggingDiv =
                                                div
                                                    [ class "box is-shrinked2 mb-2 mx-2 is-dragging"
                                                    , style "opacity" "0.6"
                                                    , style "height" "4rem"
                                                    ]
                                                    []
                                        in
                                        ternary isHoveredUp
                                            [ draggingDiv ]
                                            []
                                            ++ [ div
                                                    ([ class "box is-shrinked2 mb-2 mx-2"
                                                     , classList [ ( "is-dragging", model.movingHoverT /= Nothing ) ]
                                                     , attribute "draggable" "true"
                                                     , attribute "ondragstart" "event.dataTransfer.setData(\"text/plain\", \"dummy\")"
                                                     , onDragStart <| OnMove { pos = i, to_receiverid = t.receiver.nameid } t
                                                     , onDragEnd OnEndMove
                                                     , onDragEnter (OnMoveEnterT { pos = i, tid = t.id, to_receiverid = t.receiver.nameid })
                                                     ]
                                                        ++ (if j_last == j then
                                                                -- reset hoverT to draw below
                                                                [ onDragLeave (OnMoveEnterC { pos = i, to_receiverid = t.receiver.nameid } True) ]

                                                            else
                                                                []
                                                           )
                                                    )
                                                    [ mediaTension model.lang model.now model.node_focus t True False "is-size-6" Navigate ]
                                               ]
                                            ++ ternary hasLastColumn
                                                [ draggingDiv ]
                                                []
                                    )
                                |> List.concat
                                |> div [ class "content scrollbar-thin" ]
                            ]
                        , div [ class "divider is-vertical2 is-small is-hidden-mobile" ] []
                        ]
                    )
                |> List.concat
                |> (\x ->
                        if List.length x == 0 then
                            [ div [ class "ml-6 p-6" ]
                                [ text T.noTensionsYet
                                , ternary (model.node_focus.nameid /= model.node_focus.rootnameid)
                                    (span [ class "help-label button-light is-h is-discrete", onClick OnGoRoot ] [ A.icon "arrow-up", text T.goRoot ])
                                    (text "")
                                ]
                            ]

                        else
                            x
                   )
                |> div
                    [ id "tensionsCircle"
                    , class "columns is-fullwidth is-marginless is-mobile kb-board"

                    --, onMouseLeave (OnColumnHover Nothing)
                    , attribute "style" <|
                        case model.boardHeight of
                            Just h ->
                                "overflow-y: hidden; overflow-x: auto; height:" ++ String.fromFloat h ++ "px;"

                            Nothing ->
                                "overflow-y: hidden; overflow-x: auto;"
                    ]

        Failure err ->
            viewGqlErrors err

        _ ->
            div [ class "spinner" ] []


viewTensions : Lang.Lang -> Time.Posix -> NodeFocus -> Maybe String -> GqlData (List Tension) -> TensionDirection -> Html Msg
viewTensions lang now focus pattern tensionsData tensionDir =
    div
        [ class "box is-shrinked"
        , attribute "style" "border-top-left-radius: 0px; border-top-right-radius: 0px;"
        , classList [ ( "spinner", tensionsData == LoadingSlowly ) ]
        ]
        [ case tensionsData of
            Success tensions ->
                if List.length tensions > 0 then
                    tensions
                        |> List.map (\t -> mediaTension lang now focus t True True "is-size-6 t-o" Navigate)
                        |> div [ id "tensionsTab" ]

                else if pattern /= Nothing then
                    div [ class "m-4" ] [ text T.noResultsFor, text ": ", text (pattern |> withDefault "") ]

                else
                    case focus.type_ of
                        NodeType.Role ->
                            case tensionDir of
                                InternalTension ->
                                    div [ class "m-4" ] [ text T.noTensionRole ]

                                ExternalTension ->
                                    div [ class "m-4" ] [ text T.noTensionRole ]

                                ListTension ->
                                    div [ class "m-4" ] [ text T.noTensionRole ]

                        NodeType.Circle ->
                            case tensionDir of
                                InternalTension ->
                                    div [ class "m-4" ] [ text T.noTensionCircle ]

                                ExternalTension ->
                                    div [ class "m-4" ] [ text T.noTensionCircle ]

                                ListTension ->
                                    div [ class "m-4" ] [ text T.noTensionCircle ]

            Failure err ->
                viewGqlErrors err

            _ ->
                div [] []
        ]
