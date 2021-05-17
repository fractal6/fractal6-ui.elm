module ModelCommon exposing (..)

import Array exposing (Array)
import Codecs exposing (WindowPos, userCtxDecoder, windowDecoder)
import Components.Loading as Loading exposing (ErrorData, GqlData, RequestResult(..), WebData, withMaybeDataMap)
import Dict exposing (Dict)
import Dict.Extra as DE
import Extra exposing (toMapOfList)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Json.Decode as JD
import Maybe exposing (withDefault)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), NodeFocus, nearestCircleid)
import ModelSchema exposing (..)
import Ports
import RemoteData
import Set
import Task
import Time
import Url exposing (Url)



--
-- Session / Global
-- @debug: rename this file Session ?
--


type alias Apis =
    { auth : String
    , gql : String
    , rest : String
    , data : String
    }


type alias Screen =
    { w : Int, h : Int }


type alias SessionFlags =
    { uctx : Maybe JD.Value
    , window_pos : Maybe JD.Value
    , apis : Apis
    , screen : Screen
    }


type alias Session =
    { user : UserState
    , referer : Maybe Url
    , token_data : WebData UserCtx
    , node_focus : Maybe NodeFocus
    , path_data : Maybe LocalGraph
    , orga_data : Maybe NodesData
    , users_data : Maybe UsersData
    , node_data : Maybe NodeData
    , tensions_data : Maybe TensionsData
    , tension_head : Maybe TensionHead
    , isAdmin : Maybe Bool
    , node_action : Maybe ActionState
    , node_quickSearch : Maybe NodesQuickSearch
    , apis : Apis
    , window_pos : Maybe WindowPos
    , screen : Screen
    }


type GlobalCmd
    = --| Delay msg1 Float
      --| SubmitDelay Int
      DoAuth UserCtx
    | DoUpdateToken
    | DoNavigate String
    | DoReplaceUrl String
    | DoModalAsk String String -- SafeClose
      --
    | DoFetchNode String
    | DoPushTension Tension


type alias NodesQuickSearch =
    { pattern : String
    , lookup : Array Node
    , idx : Int
    , visible : Bool
    }


resetSession : SessionFlags -> Session
resetSession flags =
    { referer = Nothing
    , user = LoggedOut
    , token_data = RemoteData.NotAsked
    , node_focus = Nothing
    , path_data = Nothing
    , orga_data = Nothing
    , users_data = Nothing
    , node_data = Nothing
    , tensions_data = Nothing
    , tension_head = Nothing
    , isAdmin = Nothing
    , node_action = Nothing
    , node_quickSearch = Nothing
    , window_pos = Nothing
    , apis = flags.apis
    , screen = flags.screen
    }


fromLocalSession : SessionFlags -> ( Session, List (Cmd msg) )
fromLocalSession flags =
    let
        ( user, cmd1 ) =
            case flags.uctx of
                Just raw ->
                    case JD.decodeValue userCtxDecoder raw of
                        Ok uctx ->
                            ( LoggedIn uctx, Cmd.none )

                        Err err ->
                            ( LoggedOut, Ports.logErr (JD.errorToString err) )

                Nothing ->
                    ( LoggedOut, Cmd.none )

        ( window_pos, cmd2 ) =
            case flags.window_pos of
                Just raw ->
                    case JD.decodeValue windowDecoder raw of
                        Ok v ->
                            ( Just v, Cmd.none )

                        Err err ->
                            ( Nothing, Ports.logErr (JD.errorToString err) )

                Nothing ->
                    ( Nothing, Cmd.none )
    in
    ( { referer = Nothing
      , user = user
      , token_data = RemoteData.NotAsked
      , node_focus = Nothing
      , path_data = Nothing
      , orga_data = Nothing
      , users_data = Nothing
      , node_data = Nothing
      , tensions_data = Nothing
      , tension_head = Nothing
      , isAdmin = Nothing
      , node_action = Nothing
      , node_quickSearch = Nothing
      , window_pos = window_pos
      , apis = flags.apis
      , screen = flags.screen
      }
    , [ cmd1, cmd2 ]
    )


orgaToUsersData : NodesData -> UsersData
orgaToUsersData nd =
    nd
        |> Dict.toList
        |> List.map (\( k, n ) -> Maybe.map (\fs -> ( nearestCircleid k, { username = fs.username, name = fs.name } )) n.first_link)
        |> List.filterMap identity
        |> toMapOfList



--
-- User State
--


type UserState
    = LoggedOut
    | LoggedIn UserCtx



--
-- Modal
--


type alias UserAuthForm =
    { post : Dict String String
    , result : WebData UserCtx
    }


type ModalAuth
    = Inactive
    | Active UserAuthForm



--
-- Action Step and Form Data
--


type ActionState
    = ActionAuthNeeded
    | AskErr String
    | NoOp
      --  @debug: move this to Components
    | JoinOrga (JoinStep ActionForm)



-- Tension Form


type alias TensionForm =
    { uctx : UserCtx
    , source : UserRole
    , target : PNode
    , status : TensionStatus.TensionStatus
    , tension_type : TensionType.TensionType
    , labels : List Label
    , action : Maybe TensionAction.TensionAction
    , post : Post -- For String type,  createdBy, createdAt, title, message, etc

    --
    , users : List UserForm

    -- data
    , events_type : Maybe (List TensionEvent.TensionEvent)
    , blob_type : Maybe BlobType.BlobType
    , node : NodeFragment
    }


type alias TensionPatchForm =
    { id : String
    , uctx : UserCtx
    , status : Maybe TensionStatus.TensionStatus
    , tension_type : Maybe TensionType.TensionType
    , action : Maybe TensionAction.TensionAction
    , emitter : Maybe EmitterOrReceiver
    , receiver : Maybe EmitterOrReceiver
    , post : Post -- createdBy, createdAt, title, message...

    --
    , users : List UserForm

    -- data
    , events_type : Maybe (List TensionEvent.TensionEvent)
    , blob_type : Maybe BlobType.BlobType
    , node : NodeFragment
    , md : Maybe String
    }


type alias UserForm =
    { username : String, role_type : RoleType.RoleType, pattern : String }


type alias CommentPatchForm =
    { id : String
    , uctx : UserCtx
    , post : Post
    , viewMode : InputViewMode
    }


type alias AssigneeForm =
    { uctx : UserCtx
    , tid : String
    , targets : List String -- Where the labels come from
    , assignee : User -- selected/unselected item
    , isNew : Bool -- to add or remove item
    , events_type : Maybe (List TensionEvent.TensionEvent)
    , post : Post
    }


initAssigneeForm : String -> UserState -> AssigneeForm
initAssigneeForm tid user =
    { uctx =
        case user of
            LoggedIn uctx ->
                uctx

            LoggedOut ->
                UserCtx "" Nothing (UserRights False False) []
    , tid = tid
    , targets = []
    , assignee = User "" Nothing
    , isNew = False
    , events_type = Nothing
    , post = Dict.empty
    }


type alias LabelForm =
    { uctx : UserCtx
    , tid : String
    , targets : List String -- Where the items come from
    , label : Label -- selected/unselected item
    , isNew : Bool -- to add or remove item
    , events_type : Maybe (List TensionEvent.TensionEvent)
    , post : Post
    }


initLabelForm : String -> UserState -> LabelForm
initLabelForm tid user =
    { uctx =
        case user of
            LoggedIn uctx ->
                uctx

            LoggedOut ->
                UserCtx "" Nothing (UserRights False False) []
    , tid = tid
    , targets = []
    , label = Label "" "" Nothing
    , isNew = False
    , events_type = Nothing
    , post = Dict.empty
    }


{-|

    Create tension a the current focus

-}
initTensionForm : UserState -> TensionForm
initTensionForm user =
    { uctx =
        case user of
            LoggedIn uctx ->
                uctx

            LoggedOut ->
                UserCtx "" Nothing (UserRights False False) []
    , source = UserRole "" "" "" RoleType.Guest
    , target = initPNode
    , status = TensionStatus.Open
    , tension_type = TensionType.Operational
    , labels = []
    , action = Nothing
    , post = Dict.empty
    , users = []
    , events_type = Nothing
    , blob_type = Nothing
    , node = initNodeFragment Nothing
    }


initTensionPatchForm : String -> UserState -> TensionPatchForm
initTensionPatchForm tid user =
    { uctx =
        case user of
            LoggedIn uctx ->
                uctx

            LoggedOut ->
                UserCtx "" Nothing (UserRights False False) []
    , id = tid
    , status = Nothing
    , tension_type = Nothing
    , action = Nothing
    , emitter = Nothing
    , receiver = Nothing
    , post = Dict.empty
    , users = []
    , events_type = Nothing
    , blob_type = Nothing
    , node = initNodeFragment Nothing
    , md = Nothing
    }



--Settings Form


type alias LabelNodeForm =
    { uctx : UserCtx
    , id : String
    , nameid : String
    , post : Post
    }


initLabelNodeForm : UserState -> String -> LabelNodeForm
initLabelNodeForm user nameid =
    { uctx =
        case user of
            LoggedIn uctx ->
                uctx

            LoggedOut ->
                UserCtx "" Nothing (UserRights False False) []
    , id = ""
    , nameid = nameid
    , post = Dict.empty
    }



-- Join Form
-- @debug: ActionForm is defined twice here and in ActionPanel


type alias ActionForm =
    { uctx : UserCtx
    , tid : String
    , bid : String
    , node : Node
    , events_type : Maybe (List TensionEvent.TensionEvent)
    , post : Post
    }


initActionForm : String -> UserState -> ActionForm
initActionForm tid user =
    { uctx =
        case user of
            LoggedIn uctx ->
                uctx

            LoggedOut ->
                UserCtx "" Nothing (UserRights False False) []
    , tid = tid
    , bid = ""
    , node = initNode
    , events_type = Nothing
    , post = Dict.empty
    }



-- Steps


{-| Join Step
-}
type JoinStep form
    = JoinInit (GqlData Node)
    | JoinValidation form (GqlData ActionResult)
    | JoinNotAuthorized ErrorData



-- View


type InputViewMode
    = Write
    | Preview



--
-- Getters
--


getNode : String -> GqlData NodesData -> Maybe Node
getNode nameid orga =
    case orga of
        Success nodes ->
            Dict.get nameid nodes

        _ ->
            Nothing


getNodeName : String -> GqlData NodesData -> String
getNodeName nameid orga =
    let
        errMsg =
            "Error: Node unknown"
    in
    case orga of
        Success nodes ->
            Dict.get nameid nodes
                |> Maybe.map (\n -> n.name)
                |> withDefault errMsg

        _ ->
            errMsg


getParentId : String -> GqlData NodesData -> Maybe String
getParentId nameid odata =
    case odata of
        Success data ->
            data
                |> Dict.get nameid
                |> Maybe.map (\n -> n.parent)
                |> withDefault Nothing
                |> Maybe.map (\p -> p.nameid)

        _ ->
            Nothing


getParents : String -> GqlData NodesData -> List Node
getParents nameid odata =
    case odata of
        Success data ->
            case Maybe.map (\n -> n.parent) (Dict.get nameid data) |> withDefault Nothing of
                Just p ->
                    case getNode p.nameid odata of
                        Just n ->
                            [ n ] ++ getParents p.nameid odata

                        Nothing ->
                            getParents p.nameid odata

                Nothing ->
                    []

        _ ->
            []


getChildren : String -> GqlData NodesData -> List Node
getChildren nid odata =
    odata
        |> withMaybeDataMap
            (\x ->
                x |> Dict.values |> List.filter (\n -> Just (nearestCircleid nid) == Maybe.map (\m -> m.nameid) n.parent)
            )
        |> withDefault []


getChildrenLeaf : String -> GqlData NodesData -> List Node
getChildrenLeaf nid odata =
    odata
        |> withMaybeDataMap
            (\x ->
                x |> Dict.values |> List.filter (\n -> n.type_ == NodeType.Role && Just (nearestCircleid nid) == Maybe.map (\m -> m.nameid) n.parent)
            )
        |> withDefault []


getParentFragmentFromRole role =
    let
        l =
            String.split "#" role.nameid
                |> List.filter (\x -> x /= "")
                |> Array.fromList
    in
    Array.get (Array.length l - 2) l |> withDefault ""


hotNodeInsert : Node -> GqlData NodesData -> NodesData
hotNodeInsert node odata =
    -- Push a new node in the model if data is success
    case odata of
        Success data ->
            Dict.insert node.nameid node data

        _ ->
            Dict.empty


hotNodePush : List Node -> GqlData NodesData -> NodesData
hotNodePush nodes odata =
    -- Push a new node in the model if data is success
    case odata of
        Success data ->
            Dict.union (List.map (\n -> ( n.nameid, n )) nodes |> Dict.fromList) data

        _ ->
            Dict.empty


hotNodePull : List String -> GqlData NodesData -> NodesData
hotNodePull nameids odata =
    -- Push a new node in the model if data is success
    case odata of
        Success data ->
            data |> DE.removeMany (Set.fromList nameids)

        other ->
            Dict.empty


hotTensionPush : Tension -> GqlData TensionsData -> TensionsData
hotTensionPush tension tsData =
    -- Push a new tension in the model if data is success
    case tsData of
        Success tensions ->
            [ tension ] ++ tensions

        _ ->
            []


hotNodeUpdateName : TensionForm -> GqlData NodesData -> NodesData
hotNodeUpdateName form odata =
    case odata of
        Success data ->
            form.node.name
                |> Maybe.map
                    (\name ->
                        case odata of
                            Success ndata ->
                                Dict.update form.target.nameid (\nm -> nm |> Maybe.map (\n -> { n | name = name })) ndata

                            other ->
                                Dict.empty
                    )
                |> withDefault Dict.empty

        other ->
            Dict.empty
