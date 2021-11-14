module Org.Tensions exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Array
import Auth exposing (ErrState(..), parseErr, refreshAuthModal)
import Browser.Dom as Dom
import Browser.Events as Events
import Browser.Navigation as Nav
import Codecs exposing (QuickDoc)
import Components.HelperBar as HelperBar exposing (HelperBar)
import Components.LabelSearchPanel as LabelSearchPanel
import Components.Loading as Loading
    exposing
        ( GqlData
        , ModalData
        , RequestResult(..)
        , WebData
        , fromMaybeData
        , fromMaybeWebData
        , viewAuthNeeded
        , viewGqlErrors
        , viewHttpErrors
        , withDefaultData
        , withMaybeData
        , withMaybeDataMap
        )
import Components.UserSearchPanel as UserSearchPanel
import Date exposing (formatTime)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Extra.Events exposing (onClickPD, onEnter, onKeydown, onTab)
import Extra.Url exposing (queryBuilder, queryParser)
import Form exposing (isPostSendable)
import Form.Help as Help
import Form.NewTension as NTF exposing (TensionTab(..))
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, li, nav, option, p, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, autocomplete, autofocus, class, classList, disabled, href, id, list, placeholder, rows, selected, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Icon as I
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (Flags_, FractalBaseRoute(..), NodeFocus, basePathChanged, focusFromNameid, focusState, nameidFromFlags, uriFromNameid)
import ModelCommon.Requests exposing (fetchChildren, fetchTensionAll, fetchTensionCount, fetchTensionExt, fetchTensionInt, getQuickDoc, login)
import ModelCommon.View exposing (mediaTension, tensionTypeColor)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Process
import Query.PatchTension exposing (actionRequest)
import Query.QueryNode exposing (fetchNode, queryLocalGraph)
import Query.QueryTension exposing (queryExtTension, queryIntTension)
import RemoteData exposing (RemoteData)
import Session exposing (GlobalCmd(..), LabelSearchPanelOnClickAction(..), Screen, UserSearchPanelOnClickAction(..))
import Task
import Text as T exposing (textH, textT)
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

                    DoAuth uctx ->
                        ( send (DoOpenAuthModal uctx), Cmd.none )

                    DoUpdateToken ->
                        ( Cmd.none, send UpdateUserToken )

                    DoPushTension tension ->
                        ( send (PushTension tension), Cmd.none )

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
    , tensions_int : GqlData TensionsList
    , tensions_ext : GqlData TensionsList
    , tensions_all : GqlData TensionsList
    , boardHeight : Maybe Float
    , offset : Int
    , pattern : Maybe String
    , initPattern : Maybe String
    , viewMode : TensionsView
    , statusFilter : StatusFilter
    , typeFilter : TypeFilter
    , depthFilter : DepthFilter
    , authors : List User
    , labels : List Label
    , tensions_count : GqlData TensionsCount

    -- Common
    , node_action : ActionState
    , screen : Screen
    , isModalActive : Bool -- Only use by JoinOrga for now. (other actions rely on Bulma drivers)
    , modalAuth : ModalAuth
    , helperBar : HelperBar
    , refresh_trial : Int
    , url : Url

    -- Components
    , help : Help.State
    , tensionForm : NTF.State
    , authorsPanel : UserSearchPanel.State
    , labelsPanel : LabelSearchPanel.State
    }


type TensionDirection
    = InternalTension
    | ExternalTension
    | ListTension



-- Query parameters


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


type TypeFilter
    = AllTypes
    | GovernanceType
    | OperationalType
    | HelpType


typeFilterEncoder : TypeFilter -> String
typeFilterEncoder x =
    case x of
        GovernanceType ->
            "governance"

        OperationalType ->
            "operational"

        HelpType ->
            "help"

        AllTypes ->
            "all"


typeFilterDecoder : String -> TypeFilter
typeFilterDecoder x =
    case x of
        "governance" ->
            GovernanceType

        "operational" ->
            OperationalType

        "help" ->
            HelpType

        default ->
            AllTypes


defaultType : String
defaultType =
    "all"


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


defaultStatusFilter =
    OpenStatus


defaultTypeFilter =
    AllTypes


defaultDepthFilter =
    AllSubChildren


defaultAuthorsFilter =
    []


defaultLabelsFilter =
    []


loadEncoder : Int -> String
loadEncoder x =
    String.fromInt x


loadDecoder : String -> Int
loadDecoder x =
    String.toInt x |> withDefault 0



{- Authors parameters -}


authorsEncoder : List User -> List ( String, String )
authorsEncoder authors =
    authors |> List.map (\x -> ( "u", x.username ))



{- labels parameters -}


labelsEncoder : List Label -> List ( String, String )
labelsEncoder labels =
    labels |> List.map (\x -> ( "l", x.name ))


nfirstL : Int
nfirstL =
    15


nfirstC : Int
nfirstC =
    -- @debug message/warning if thera are more than nfirstC tensions
    1000


hasLoadMore : GqlData TensionsList -> Int -> Bool
hasLoadMore tensions offset =
    case tensions of
        Success ts ->
            List.length ts == nfirstL * offset

        other ->
            False



--
-- DoLoad utils
--


getTargets : Model -> List String
getTargets model =
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

        GovernanceType ->
            Just TensionType.Governance

        OperationalType ->
            Just TensionType.Operational

        HelpType ->
            Just TensionType.Help



---- MSG ----


type Msg
    = PassedSlowLoadTreshold -- timer
    | OnResize Int Int
    | FitBoard (Result Dom.Error Dom.Element)
    | PushTension Tension
    | PushGuest ActionForm
    | Submit (Time.Posix -> Msg) -- Get Current Time
      -- Data Queries
    | GotPath Bool (GqlData LocalGraph) -- GraphQL
    | GotChildren (WebData (List NodeId)) -- HTTP/Json
    | GotTensionsInt Int (GqlData TensionsList) -- GraphQL
    | GotTensionsExt (GqlData TensionsList) -- GraphQL
    | GotTensionsAll (GqlData TensionsList) -- GraphQL
    | GotTensionsCount (GqlData TensionsCount)
      -- Page Action
    | DoLoadInit
    | DoLoad Bool -- query tensions
    | ChangePattern String
    | ChangeViewFilter TensionsView
    | ChangeStatusFilter StatusFilter
    | ChangeTypeFilter TypeFilter
    | ChangeDepthFilter DepthFilter
    | ChangeAuthor
    | ChangeLabel
    | SearchKeyDown Int
    | ResetData
    | OnClearFilter
    | SubmitSearchReset
    | SubmitSearch
    | GoView TensionsView
    | SetOffset Int
      -- New Tension
    | DoCreateTension LocalGraph
      -- JoinOrga Action
    | DoJoinOrga String
    | DoJoinOrga2 (GqlData Node)
    | DoJoinOrga3 Node Time.Posix
    | JoinAck (GqlData ActionResult)
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
    | DoOpenModal -- ports receive / Open  modal
    | DoCloseModal ModalData -- ports receive / Close modal
    | ExpandRoles
    | CollapseRoles
      -- Components
    | HelpMsg Help.Msg
    | NewTensionMsg NTF.Msg
    | UserSearchPanelMsg UserSearchPanel.Msg
    | LabelSearchPanelMsg LabelSearchPanel.Msg



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
            , boardHeight = Nothing
            , offset = ternary fs.refresh 0 (Dict.get "load" query |> withDefault [] |> List.head |> withDefault "" |> loadDecoder)
            , tensions_int = fromMaybeData global.session.tensions_int Loading
            , tensions_ext = fromMaybeData global.session.tensions_ext Loading
            , tensions_all = fromMaybeData global.session.tensions_all Loading
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
            , authors = Dict.get "u" query |> withDefault [] |> List.map (\x -> User x Nothing)
            , labels = Dict.get "l" query |> withDefault [] |> List.map (\x -> Label "" x Nothing)
            , tensions_count = fromMaybeData global.session.tensions_count Loading

            -- Common
            , node_action = NoOp
            , isModalActive = False
            , modalAuth = Inactive
            , helperBar = HelperBar.create
            , help = Help.init global.session.user
            , tensionForm = NTF.init global.session.user
            , refresh_trial = 0
            , url = global.url
            }

        --
        -- Refresh tensions when data are in a Loading state or if
        -- the query just changed (ie referer), regardless the "v" a "load" parameters.
        --
        dataToLoad =
            case model.viewMode of
                ListView ->
                    model.tensions_int

                IntExtView ->
                    model.tensions_int

                CircleView ->
                    model.tensions_all

        refresh =
            (fs.refresh || dataToLoad == Loading)
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
            [ if fs.focusChange then
                [ queryLocalGraph apis.gql newFocus.nameid (GotPath True), send ResetData ]

              else if getTargets model == [] then
                -- path of children has not been loaded
                [ send DoLoadInit ]

              else if refresh then
                [ send (DoLoad False) ]

              else if model.viewMode == CircleView then
                [ Ports.hide "footBar", Task.attempt FitBoard (Dom.getElement "tensionsCircle") ]

              else if dataToLoad /= Loading && model.offset == 0 then
                -- Assume data are already loaded, actualise offset.
                [ send (SetOffset 1) ]

              else
                []
            , [ sendSleep PassedSlowLoadTreshold 500 ]
            , [ sendSleep InitModals 400 ]
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
                tensions =
                    hotTensionPush tension model.tensions_all
            in
            ( { model | tensions_all = Success tensions }, Cmd.none, send (UpdateSessionTensions (Just tensions)) )

        PushGuest form ->
            ( model, actionRequest apis.gql form JoinAck, Cmd.none )

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
                                    List.head path.path |> Maybe.map (\p -> p.nameid) |> withDefault ""
                            in
                            ( { model | path_data = Success newPath }
                            , queryLocalGraph apis.gql nameid (GotPath False)
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

        GotTensionsInt inc result ->
            let
                newTensions =
                    case model.tensions_int of
                        Success tsOld ->
                            case result of
                                Success ts ->
                                    tsOld ++ ts |> Success

                                other ->
                                    tsOld |> Success

                        other ->
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

                                other ->
                                    tsOld |> Success

                        other ->
                            result
            in
            ( { model | tensions_ext = newTensions }, Cmd.none, send (UpdateSessionTensionsExt (withMaybeData newTensions)) )

        GotTensionsAll result ->
            ( { model | tensions_all = result }, Task.attempt FitBoard (Dom.getElement "tensionsCircle"), send (UpdateSessionTensionsAll (withMaybeData result)) )

        GotTensionsCount result ->
            ( { model | tensions_count = result }, Cmd.none, send (UpdateSessionTensionsCount (withMaybeData result)) )

        DoLoadInit ->
            ( model
            , case model.depthFilter of
                AllSubChildren ->
                    fetchChildren apis.rest model.node_focus.nameid GotChildren

                SelectedNode ->
                    send (DoLoad False)
            , Cmd.none
            )

        DoLoad reset ->
            -- if reset, reset the offset
            -- else increments the results span
            let
                nameids =
                    getTargets model

                status =
                    statusDecoder model.statusFilter

                type_ =
                    typeDecoder model.typeFilter

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
                , fetchTensionAll apis.rest nameids nfirstC 0 model.pattern status model.authors model.labels type_ GotTensionsAll
                , Ports.hide "footBar"
                )

            else if List.member model.viewMode [ ListView, IntExtView ] then
                ( if reset then
                    { model | offset = offset, tensions_int = LoadingSlowly, tensions_ext = LoadingSlowly }

                  else
                    model
                , Cmd.batch
                    [ fetchTensionInt apis.rest nameids first skip model.pattern status model.authors model.labels type_ (GotTensionsInt inc)
                    , fetchTensionExt apis.rest nameids first skip model.pattern status model.authors model.labels type_ GotTensionsExt
                    , fetchTensionCount apis.rest nameids model.pattern model.authors model.labels type_ GotTensionsCount
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

        ChangeAuthor ->
            let
                targets =
                    model.path_data |> withMaybeDataMap (\x -> List.map (\y -> y.nameid) x.path) |> withDefault []
            in
            ( model, Cmd.map UserSearchPanelMsg (send (UserSearchPanel.OnOpen targets)), Cmd.none )

        ChangeLabel ->
            let
                targets =
                    model.path_data |> withMaybeDataMap (\x -> List.map (\y -> y.nameid) x.path) |> withDefault []
            in
            ( model, Cmd.map LabelSearchPanelMsg (send (LabelSearchPanel.OnOpen targets)), Cmd.none )

        SearchKeyDown key ->
            case key of
                13 ->
                    --ENTER
                    ( model, send SubmitSearchReset, Cmd.none )

                27 ->
                    --ESC
                    ( model, send (ChangePattern ""), Cmd.none )

                other ->
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
                        [ ( "v", viewModeEncoder model.viewMode |> (\x -> ternary (x == defaultView) "" x) ) ]
                        |> (\q -> ternary (q == "") "" ("?" ++ q))
            in
            ( model
            , Cmd.batch [ Nav.pushUrl global.key (uriFromNameid TensionsBaseUri model.node_focus.nameid ++ query), send ResetData ]
            , Cmd.none
            )

        SubmitSearchReset ->
            -- Reset the other results
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
                         , ( "load", loadEncoder model.offset |> (\x -> ternary (x == "0" || x == "1") "" x) )
                         ]
                            ++ authorsEncoder model.authors
                            ++ labelsEncoder model.labels
                        )
                        |> (\q -> ternary (q == "") "" ("?" ++ q))
            in
            ( model, Nav.pushUrl global.key (uriFromNameid TensionsBaseUri model.node_focus.nameid ++ query), Cmd.none )

        GoView viewMode ->
            let
                ( data_m, cmds ) =
                    Tuple.mapFirst (\x -> withMaybeData x) <|
                        case viewMode of
                            ListView ->
                                ( model.tensions_int, [ Ports.show "footBar" ] )

                            IntExtView ->
                                ( model.tensions_int, [ Ports.show "footBar" ] )

                            CircleView ->
                                ( model.tensions_all, [ Ports.hide "footBar", Task.attempt FitBoard (Dom.getElement "tensionsCircle") ] )
            in
            ( { model | viewMode = viewMode }
            , Cmd.batch (ternary (data_m == Nothing) [ send (DoLoad False) ] [] ++ cmds)
            , Cmd.none
            )

        SetOffset v ->
            ( { model | offset = v }, Cmd.none, Cmd.none )

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
                                LE.remove (Tuple.second r) model.authors
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
                                LE.remove (Tuple.second r) model.labels
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

        -- New tension
        DoCreateTension lg ->
            let
                tf =
                    model.tensionForm
                        |> NTF.setUser_ global.session.user
                        |> NTF.setPath_ lg
            in
            ( { model | tensionForm = tf }, Cmd.map NewTensionMsg (send NTF.OnOpen), Cmd.none )

        NewTensionMsg msg ->
            let
                ( tf, out ) =
                    NTF.update apis msg model.tensionForm

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | tensionForm = tf }, out.cmds |> List.map (\m -> Cmd.map NewTensionMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        -- Join
        DoJoinOrga rootnameid ->
            case global.session.user of
                LoggedOut ->
                    ( { model | node_action = ActionAuthNeeded }
                    , send DoOpenModal
                    , Cmd.none
                    )

                LoggedIn uctx ->
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

                other ->
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
                                , ( "old", f.uctx.username )
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

                        OkAuth n ->
                            ( { model | node_action = JoinOrga (JoinValidation form result) }
                            , Cmd.none
                            , send UpdateUserToken
                            )

                        _ ->
                            ( { model | node_action = JoinOrga (JoinValidation form result) }, Cmd.none, Cmd.none )

                default ->
                    ( model, Cmd.none, Cmd.none )

        -- Token Refresh
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

                other ->
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
                gcmd =
                    if data.link /= "" then
                        send (Navigate data.link)

                    else
                        Cmd.none
            in
            ( { model | isModalActive = False }, gcmd, Ports.close_modal )

        ExpandRoles ->
            ( { model | helperBar = HelperBar.expand model.helperBar }, Cmd.none, Cmd.none )

        CollapseRoles ->
            ( { model | helperBar = HelperBar.collapse model.helperBar }, Cmd.none, Cmd.none )

        -- Help
        HelpMsg msg ->
            let
                ( help, out ) =
                    Help.update apis msg model.help

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | help = help }, out.cmds |> List.map (\m -> Cmd.map HelpMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ model =
    [ Ports.mcPD Ports.closeModalFromJs LogErr DoCloseModal
    , Events.onResize (\w h -> OnResize w h)
    ]
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        ++ (NTF.subscriptions model.tensionForm |> List.map (\s -> Sub.map NewTensionMsg s))
        ++ (UserSearchPanel.subscriptions model.authorsPanel |> List.map (\s -> Sub.map UserSearchPanelMsg s))
        ++ (LabelSearchPanel.subscriptions model.labelsPanel |> List.map (\s -> Sub.map LabelSearchPanelMsg s))
        |> Sub.batch



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Tensions · " ++ (String.join "/" <| LE.unique [ model.node_focus.rootnameid, model.node_focus.nameid |> String.split "#" |> List.reverse |> List.head |> withDefault "" ])
    , body =
        [ view_ global model
        , refreshAuthModal model.modalAuth { closeModal = DoCloseAuthModal, changePost = ChangeAuthPost, submit = SubmitUser, submitEnter = SubmitKeyDown }
        , Help.view {} model.help |> Html.map HelpMsg
        , NTF.view { users_data = fromMaybeData global.session.users_data NotAsked } model.tensionForm |> Html.map NewTensionMsg
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    let
        helperData =
            { user = global.session.user
            , uriQuery = model.url.query
            , path_data = global.session.path_data
            , baseUri = TensionsBaseUri
            , data = model.helperBar
            , onJoin = DoJoinOrga model.node_focus.rootnameid
            , onExpand = ExpandRoles
            , onCollapse = CollapseRoles
            , onCreateTension = DoCreateTension
            }

        isFullwidth =
            model.viewMode == CircleView
    in
    div [ id "mainPane" ]
        [ HelperBar.view helperData
        , div [ class "columns is-centered", classList [ ( "mb-0", isFullwidth ) ] ]
            [ div [ class "column is-10-desktop is-10-widescreen is-11-fullhd", classList [ ( "pb-0", isFullwidth ) ] ]
                [ div [ class "columns is-centered", classList [ ( "mb-0", isFullwidth ) ] ]
                    [ div [ class "column is-10-desktop is-9-fullhd", classList [ ( "pb-0", isFullwidth ) ] ] [ viewSearchBar model ] ]
                , div [] <|
                    case model.children of
                        RemoteData.Failure err ->
                            [ viewHttpErrors err ]

                        other ->
                            []
                , case model.viewMode of
                    ListView ->
                        viewListTensions model

                    IntExtView ->
                        viewIntExtTensions model

                    CircleView ->
                        text ""
                , if model.viewMode == CircleView && (withMaybeData model.tensions_all |> Maybe.map (\ts -> List.length ts == 1000) |> withDefault False) then
                    div [ class "column is-12  is-aligned-center", attribute "style" "margin-left: 0.5rem;" ]
                        [ button [ class "button is-small" ]
                            -- @TODO: load more for CircleView
                            [ text "Viewing only the 1000 more recent tensions" ]
                        ]

                  else if model.viewMode /= CircleView && (hasLoadMore model.tensions_int model.offset || hasLoadMore model.tensions_ext model.offset) then
                    div [ class "column is-12  is-aligned-center", attribute "style" "margin-left: 0.5rem;" ]
                        [ button [ class "button is-small", onClick (DoLoad False) ]
                            [ text "Load more" ]
                        ]

                  else
                    text ""
                ]
            ]
        , if model.viewMode == CircleView then
            viewCircleTensions model

          else
            text ""
        , setupActionModal model.isModalActive model.node_action
        ]


viewSearchBar : Model -> Html Msg
viewSearchBar model =
    let
        checked =
            I.icon1 "icon-check has-text-success" ""

        unchecked =
            I.icon1 "icon-check has-text-success is-invisible" ""

        clearFilter =
            if queryIsEmpty model then
                text ""

            else
                span
                    [ class "tag is-rounded is-small is-danger is-light button-light"
                    , attribute "style" "margin: 0.35rem;"
                    , onClick OnClearFilter
                    ]
                    [ text "Clear filters" ]
    in
    div [ id "searchBarTensions", class "searchBar" ]
        [ div [ class "columns mt-0 mb-0" ]
            [ div [ class "column is-6" ]
                [ div [ class "field has-addons" ]
                    [ div [ class "control has-icons-left is-expanded" ]
                        [ input
                            [ class "is-rounded input is-small"
                            , type_ "search"
                            , autocomplete False
                            , autofocus False
                            , placeholder "Search tensions"
                            , value (withDefault "" model.pattern)
                            , onInput ChangePattern
                            , onKeydown SearchKeyDown
                            ]
                            []
                        , span [ class "icon is-left" ] [ i [ class "icon-search" ] [] ]
                        ]
                    ]
                ]
            , div [ class "column is-6 flex-gap" ]
                [ div [ class "field has-addons filterBar mb-0" ]
                    [ div [ class "control dropdown" ]
                        [ div [ class "is-small button dropdown-trigger", attribute "aria-controls" "status-filter" ]
                            [ ternary (model.statusFilter /= defaultStatusFilter) (span [ class "badge" ] []) (text "")
                            , textH T.status
                            , i [ class "ml-3 icon-chevron-down icon-tiny" ] []
                            ]
                        , div [ id "status-filter", class "dropdown-menu", attribute "role" "menu" ]
                            [ div
                                [ class "dropdown-content" ]
                                [ div [ class "dropdown-item button-light", onClick <| ChangeStatusFilter AllStatus ]
                                    [ ternary (model.statusFilter == AllStatus) checked unchecked, textH (statusFilterEncoder AllStatus) ]
                                , div [ class "dropdown-item button-light", onClick <| ChangeStatusFilter OpenStatus ]
                                    [ ternary (model.statusFilter == OpenStatus) checked unchecked, textH (statusFilterEncoder OpenStatus) ]
                                , div [ class "dropdown-item button-light", onClick <| ChangeStatusFilter ClosedStatus ]
                                    [ ternary (model.statusFilter == ClosedStatus) checked unchecked, textH (statusFilterEncoder ClosedStatus) ]
                                ]
                            ]
                        ]
                    , div [ class "control dropdown" ]
                        [ div [ class "is-small button dropdown-trigger", attribute "aria-controls" "type-filter" ]
                            [ ternary (model.typeFilter /= defaultTypeFilter) (span [ class "badge" ] []) (text "")
                            , textH T.type_
                            , i [ class "ml-2 icon-chevron-down icon-tiny" ] []
                            ]
                        , div [ id "type-filter", class "dropdown-menu is-right", attribute "role" "menu" ]
                            [ div
                                [ class "dropdown-content" ]
                                [ div [ class "dropdown-item button-light", onClick <| ChangeTypeFilter AllTypes ]
                                    [ ternary (model.typeFilter == AllTypes) checked unchecked, textH (typeFilterEncoder AllTypes) ]
                                , div [ class "dropdown-item button-light", onClick <| ChangeTypeFilter OperationalType ]
                                    [ ternary (model.typeFilter == OperationalType) checked unchecked, span [ class (tensionTypeColor "text" TensionType.Operational) ] [ textH (typeFilterEncoder OperationalType) ] ]
                                , div [ class "dropdown-item button-light", onClick <| ChangeTypeFilter GovernanceType ]
                                    [ ternary (model.typeFilter == GovernanceType) checked unchecked, span [ class (tensionTypeColor "text" TensionType.Governance) ] [ textH (typeFilterEncoder GovernanceType) ] ]
                                , div [ class "dropdown-item button-light", onClick <| ChangeTypeFilter HelpType ]
                                    [ ternary (model.typeFilter == HelpType) checked unchecked, span [ class (tensionTypeColor "text" TensionType.Help) ] [ textH (typeFilterEncoder HelpType) ] ]
                                ]
                            ]
                        ]
                    , div [ class "control", onClick ChangeAuthor ]
                        [ div [ class "is-small button" ]
                            [ ternary (model.authors /= defaultAuthorsFilter) (span [ class "badge" ] []) (text "")
                            , text "Author"
                            , i [ class "ml-2 icon-chevron-down icon-tiny" ] []
                            ]
                        , UserSearchPanel.view
                            { selectedAssignees = model.authors
                            , targets = model.path_data |> withMaybeDataMap (\x -> List.map (\y -> y.nameid) x.path) |> withDefault []
                            }
                            model.authorsPanel
                            |> Html.map UserSearchPanelMsg
                        ]
                    , div [ class "control", onClick ChangeLabel ]
                        [ div [ class "is-small button" ]
                            [ ternary (model.labels /= defaultLabelsFilter) (span [ class "badge" ] []) (text "")
                            , text "Label"
                            , i [ class "ml-2 icon-chevron-down icon-tiny" ] []
                            ]
                        , LabelSearchPanel.view
                            { selectedLabels = model.labels
                            , targets = model.path_data |> withMaybeDataMap (\x -> List.map (\y -> y.nameid) x.path) |> withDefault []
                            }
                            model.labelsPanel
                            |> Html.map LabelSearchPanelMsg
                        ]
                    , div [ class "control dropdown" ]
                        [ div [ class "is-small button dropdown-trigger", attribute "aria-controls" "depth-filter" ]
                            [ ternary (model.depthFilter /= defaultDepthFilter) (span [ class "badge" ] []) (text "")
                            , textH T.depth
                            , i [ class "ml-2 icon-chevron-down icon-tiny" ] []
                            ]
                        , div [ id "depth-filter", class "dropdown-menu is-right", attribute "role" "menu" ]
                            [ div
                                [ class "dropdown-content" ]
                                [ div [ class "dropdown-item button-light", onClick <| ChangeDepthFilter AllSubChildren ]
                                    [ ternary (model.depthFilter == AllSubChildren) checked unchecked, textH (depthFilterEncoder AllSubChildren) ]
                                , div [ class "dropdown-item button-light", onClick <| ChangeDepthFilter SelectedNode ]
                                    [ ternary (model.depthFilter == SelectedNode) checked unchecked, textH (depthFilterEncoder SelectedNode) ]
                                ]
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
                        [ div [ class "tooltip has-tooltip-right has-tooltip-arrow", attribute "data-tooltip" "Display tensions as List." ] [ text "List" ] ]
                    ]
                , li [ classList [ ( "is-active", model.viewMode == IntExtView ) ] ]
                    [ a [ onClickPD (ChangeViewFilter IntExtView), target "_blank" ]
                        --[ a [ onClickPD (GoView IntExtView), target "_blank" ]
                        [ div [ class "tooltip has-tooltip-right has-tooltip-arrow", attribute "data-tooltip" "Display indegenous and exogenous tensions in respect to the current circle. " ] [ text "Internal/External" ] ]
                    ]
                , li [ classList [ ( "is-active", model.viewMode == CircleView ) ] ]
                    [ a [ onClickPD (ChangeViewFilter CircleView), target "_blank" ]
                        --[ a [ onClickPD (GoView CircleView), target "_blank" ]
                        [ div [ class "tooltip has-tooltip-right has-tooltip-arrow", attribute "data-tooltip" "Display tensions by (targeted) circles." ] [ text "Circles" ] ]
                    ]
                ]
            ]
        ]


viewTensionsCount : Model -> Html Msg
viewTensionsCount model =
    case model.tensions_count of
        Success c ->
            let
                activeCls =
                    "has-background-grey-darker is-selected is-hovered"

                inactiveCls =
                    "has-background-grey-dark"
            in
            div [ class "buttons has-addons mb-1" ]
                [ div
                    [ class "button is-rounded is-small"
                    , classList [ ( activeCls, model.statusFilter == OpenStatus ), ( inactiveCls, model.statusFilter /= OpenStatus ) ]
                    , onClick <| ChangeStatusFilter OpenStatus
                    ]
                    [ span [] [ c.open |> String.fromInt |> text ], text "\u{00A0}Open" ]
                , div
                    [ class "button is-rounded is-small"
                    , classList [ ( activeCls, model.statusFilter == ClosedStatus ), ( inactiveCls, model.statusFilter /= ClosedStatus ) ]
                    , onClick <| ChangeStatusFilter ClosedStatus
                    ]
                    [ c.closed |> String.fromInt |> text, text "\u{00A0}Closed" ]
                ]

        LoadingSlowly ->
            div [ class "buttons has-addons mb-1" ]
                [ button [ class "button is-rounded is-small" ] [ text "Open" ]
                , button [ class "button is-rounded is-small" ] [ text "Closed" ]
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
                    other |> List.sortBy .createdAt |> List.reverse |> Success
    in
    div [ class "columns is-centered" ]
        [ div [ class "column is-10-desktop is-10-fullhd" ]
            [ viewTensionsCount model
            , viewTensions model.node_focus model.initPattern tensions_d ListTension
            ]
        ]


viewIntExtTensions : Model -> Html Msg
viewIntExtTensions model =
    div [ class "columns is-centered" ]
        [ div [ class "column is-6-desktop is-5-fullhd" ]
            [ h2 [ class "subtitle has-text-weight-semibold has-text-centered" ] [ textH T.internalTensions ]
            , viewTensions model.node_focus model.initPattern model.tensions_int InternalTension
            ]
        , div [ class "vline" ] []
        , div [ class "column is-6-desktop is-5-fullhd" ]
            [ h2 [ class "subtitle has-text-weight-semibold has-text-centered" ] [ textH T.externalTensions ]
            , viewTensions model.node_focus model.initPattern model.tensions_ext ExternalTension
            ]
        ]


viewCircleTensions : Model -> Html Msg
viewCircleTensions model =
    case model.tensions_all of
        Success tensions ->
            let
                --
                -- Convert a list of tension into a Dict of tension by Receiverid
                --
                addParam : Tension -> Maybe (List Tension) -> Maybe (List Tension)
                addParam value maybeValues =
                    case maybeValues of
                        Just values ->
                            Just (value :: values)

                        Nothing ->
                            Just [ value ]

                toDict2 : List ( String, Tension ) -> Dict String (List Tension)
                toDict2 parameters =
                    List.foldl
                        (\( k, v ) dict -> Dict.update k (addParam v) dict)
                        Dict.empty
                        parameters

                tensions_d =
                    tensions
                        |> List.map (\x -> ( x.receiver.nameid, x ))
                        |> toDict2
            in
            Dict.toList tensions_d
                |> List.map
                    (\( k, ts ) ->
                        [ div [ class "column is-3" ]
                            [ div [ class "subtitle is-aligned-center mb-3" ]
                                [ ts
                                    |> List.head
                                    |> Maybe.map (\h -> h.receiver.name)
                                    |> withDefault "Loading..."
                                    |> text
                                ]
                            , ts
                                |> List.sortBy .createdAt
                                |> List.reverse
                                |> List.map
                                    (\t ->
                                        div [ class "box is-shrinked2 mb-2 mx-2" ]
                                            [ mediaTension model.node_focus t True False "is-size-6" Navigate ]
                                    )
                                |> List.append []
                                |> div [ class "content" ]
                            ]
                        , div [ class "divider is-vertical2 is-small is-hidden-mobile" ] []
                        ]
                    )
                |> List.concat
                |> (\x -> ternary (List.length x == 0) [ div [] [ text "No tensions here." ] ] x)
                |> div
                    [ id "tensionsCircle"
                    , class "columns is-fullwidth is-marginless is-mobile kb-board"
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


viewTensions : NodeFocus -> Maybe String -> GqlData TensionsList -> TensionDirection -> Html Msg
viewTensions focus pattern tensionsData tensionDir =
    div [ class "box is-shrinked", classList [ ( "spinner", tensionsData == LoadingSlowly ) ] ]
        [ case tensionsData of
            Success tensions ->
                if List.length tensions > 0 then
                    tensions
                        |> List.map (\t -> mediaTension focus t True True "is-size-6" Navigate)
                        |> div [ id "tensionsTab" ]

                else if pattern /= Nothing then
                    div [ class "m-4" ] [ textH T.noResultsFor, text ": ", text (pattern |> withDefault "") ]

                else
                    case focus.type_ of
                        NodeType.Role ->
                            case tensionDir of
                                InternalTension ->
                                    div [ class "m-4" ] [ textH T.noIntTensionRole ]

                                ExternalTension ->
                                    div [ class "m-4" ] [ textH T.noExtTensionRole ]

                                ListTension ->
                                    div [ class "m-4" ] [ textH T.noTensionRole ]

                        NodeType.Circle ->
                            case tensionDir of
                                InternalTension ->
                                    div [ class "m-4" ] [ textH T.noIntTensionCircle ]

                                ExternalTension ->
                                    div [ class "m-4" ] [ textH T.noExtTensionCircle ]

                                ListTension ->
                                    div [ class "m-4" ] [ textH T.noTensionCircle ]

            Failure err ->
                viewGqlErrors err

            _ ->
                div [] []
        ]



-- Actions


setupActionModal : Bool -> ActionState -> Html Msg
setupActionModal isModalActive action =
    div
        [ id "actionModal"
        , class "modal modal-fx-fadeIn"
        , classList [ ( "is-active", isModalActive ) ]
        , attribute "data-modal-close" "closeModalFromJs"
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "actionModal"
            , onClick (DoCloseModal { reset = True, link = "" })
            ]
            []
        , div [ class "modal-content" ]
            [ case action of
                JoinOrga step ->
                    viewJoinOrgaStep step

                NoOp ->
                    text ""

                AskErr err ->
                    viewGqlErrors [ err ]

                ActionAuthNeeded ->
                    viewAuthNeeded DoCloseModal
            ]
        , button [ class "modal-close is-large", onClick (DoCloseModal { reset = True, link = "" }) ] []
        ]


viewJoinOrgaStep : JoinStep ActionForm -> Html Msg
viewJoinOrgaStep step =
    case step of
        JoinInit _ ->
            div [ class "box spinner" ] [ text "" ]

        JoinNotAuthorized errMsg ->
            viewGqlErrors errMsg

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

                default ->
                    div [ class "box spinner" ] [ text "" ]
