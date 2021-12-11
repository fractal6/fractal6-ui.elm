module ModelCommon exposing (..)

import Array exposing (Array)
import Components.Loading as Loading
    exposing
        ( ErrorData
        , GqlData
        , RequestResult(..)
        , WebData
        , withMaybeDataMap
        )
import Dict exposing (Dict)
import Dict.Extra as DE
import Extra exposing (toMapOfList)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Maybe exposing (withDefault)
import ModelCommon.Codecs
    exposing
        ( FractalBaseRoute(..)
        , getCircleRoles
        , getCoordoRoles
        , getOrgaRoles
        , isCircle
        , isOwner
        , nearestCircleid
        , nid2rootid
        )
import ModelSchema exposing (..)
import Set



--
-- User State
--


type UserState
    = LoggedOut
    | LoggedIn UserCtx


uctxFromUser : UserState -> UserCtx
uctxFromUser user =
    case user of
        LoggedIn uctx ->
            uctx

        LoggedOut ->
            initUserctx



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


type alias Ev =
    { event_type : TensionEvent.TensionEvent
    , old : String
    , new : String
    }


type alias TensionForm =
    { uctx : UserCtx
    , source : UserRole
    , target : PNode
    , status : TensionStatus.TensionStatus
    , type_ : TensionType.TensionType
    , labels : List Label
    , action : Maybe TensionAction.TensionAction
    , post : Post -- For String type,  createdBy, createdAt, title, message, etc

    --
    , users : List UserForm

    -- data
    , events : List Ev
    , blob_type : Maybe BlobType.BlobType
    , node : NodeFragment
    }


type alias TensionPatchForm =
    { id : String
    , uctx : UserCtx
    , status : Maybe TensionStatus.TensionStatus
    , type_ : Maybe TensionType.TensionType
    , action : Maybe TensionAction.TensionAction
    , emitter : Maybe EmitterOrReceiver
    , receiver : Maybe EmitterOrReceiver
    , post : Post -- createdBy, createdAt, title, message...

    --
    , users : List UserForm

    -- data
    , events : List Ev
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
    , events : List Ev
    , post : Post
    }


initAssigneeForm : String -> UserState -> AssigneeForm
initAssigneeForm tid user =
    { uctx =
        case user of
            LoggedIn uctx ->
                uctx

            LoggedOut ->
                initUserctx
    , tid = tid
    , targets = []
    , assignee = User "" Nothing
    , isNew = False
    , events = []
    , post = Dict.empty
    }


type alias LabelForm =
    { uctx : UserCtx
    , tid : String
    , targets : List String -- Where the items come from
    , label : Label -- selected/unselected item
    , isNew : Bool -- to add or remove item
    , events : List Ev
    , post : Post
    }


initLabelForm : String -> UserState -> LabelForm
initLabelForm tid user =
    { uctx =
        case user of
            LoggedIn uctx ->
                uctx

            LoggedOut ->
                initUserctx
    , tid = tid
    , targets = []
    , label = Label "" "" Nothing
    , isNew = False
    , events = []
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
                initUserctx
    , source = UserRole "" "" RoleType.Guest
    , target = initPNode
    , status = TensionStatus.Open
    , type_ = TensionType.Operational
    , labels = []
    , action = Nothing
    , post = Dict.empty
    , users = []
    , events = []
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
                initUserctx
    , id = tid
    , status = Nothing
    , type_ = Nothing
    , action = Nothing
    , emitter = Nothing
    , receiver = Nothing
    , post = Dict.empty
    , users = []
    , events = []
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
                initUserctx
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
    , fragment : NodeFragment
    , events : List Ev
    , post : Post
    }


initActionForm : String -> UserState -> ActionForm
initActionForm tid user =
    { uctx =
        case user of
            LoggedIn uctx ->
                uctx

            LoggedOut ->
                initUserctx
    , tid = tid
    , bid = ""
    , node = initNode
    , fragment = initNodeFragment Nothing
    , events = []
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


getTargets : GqlData LocalGraph -> Maybe (List RoleType.RoleType) -> List PNode
getTargets lg exclude_role_type_m =
    case lg of
        Success path ->
            case exclude_role_type_m of
                Just exclude_role_types_ ->
                    -- Exclude the roles givens in the list
                    let
                        exclude_role_types =
                            List.map (\x -> Just x) exclude_role_types_
                    in
                    path.path ++ (path.focus.children |> List.filter (\x -> not (List.member x.role_type exclude_role_types)) |> List.map shrinkNode)

                Nothing ->
                    -- Keep only circle
                    path.path ++ (path.focus.children |> List.filter (\x -> isCircle x.nameid) |> List.map shrinkNode)

        _ ->
            []


getNode : String -> GqlData NodesDict -> Maybe Node
getNode nameid orga =
    case orga of
        Success nodes ->
            Dict.get nameid nodes

        _ ->
            Nothing


getNodeName : String -> GqlData NodesDict -> String
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


getParentId : String -> GqlData NodesDict -> Maybe String
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


getParents : String -> GqlData NodesDict -> List Node
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


getChildren : String -> GqlData NodesDict -> List Node
getChildren nid odata =
    odata
        |> withMaybeDataMap
            (\x ->
                x |> Dict.values |> List.filter (\n -> Just (nearestCircleid nid) == Maybe.map (\m -> m.nameid) n.parent)
            )
        |> withDefault []


getIdsFromPath : GqlData LocalGraph -> Maybe ( String, String )
getIdsFromPath data =
    case data of
        Success d ->
            case d.focus.source of
                Just blob ->
                    Just ( d.focus.nameid, blob.tension.id )

                Nothing ->
                    Nothing

        _ ->
            Nothing


getParentFragmentFromRole role =
    let
        l =
            String.split "#" role.nameid
                |> List.filter (\x -> x /= "")
                |> Array.fromList
    in
    Array.get (Array.length l - 2) l |> withDefault ""



--
-- Setters
--


hotNodeInsert : Node -> GqlData NodesDict -> NodesDict
hotNodeInsert node odata =
    -- Push a new node in the model if data is success
    case odata of
        Success data ->
            Dict.insert node.nameid node data

        _ ->
            Dict.empty


hotNodePush : List Node -> GqlData NodesDict -> NodesDict
hotNodePush nodes odata =
    -- Push a new node in the model if data is success
    case odata of
        Success data ->
            Dict.union (List.map (\n -> ( n.nameid, n )) nodes |> Dict.fromList) data

        _ ->
            Dict.empty


hotNodePull : List String -> GqlData NodesDict -> ( NodesDict, Maybe Node )
hotNodePull nameids odata =
    -- Push a new node in the model if data is success
    -- return the first node removed
    case odata of
        Success data ->
            ( data |> DE.removeMany (Set.fromList nameids)
            , List.head nameids |> Maybe.map (\nid -> Dict.get nid data) |> withDefault Nothing
            )

        _ ->
            ( Dict.empty, Nothing )


hotTensionPush : Tension -> GqlData TensionsList -> TensionsList
hotTensionPush tension tsData =
    -- Push a new tension in the model if data is success
    case tsData of
        Success tensions ->
            [ tension ] ++ tensions

        _ ->
            []


hotNodeUpdateName : TensionForm -> GqlData NodesDict -> NodesDict
hotNodeUpdateName form odata =
    case odata of
        Success data ->
            form.node.name
                |> Maybe.map
                    (\name ->
                        Dict.update form.target.nameid (\nm -> nm |> Maybe.map (\n -> { n | name = name })) data
                    )
                |> withDefault Dict.empty

        _ ->
            Dict.empty



-- Data Utils


blobFromTensionHead : TensionHead -> Maybe Blob
blobFromTensionHead th =
    case th.blobs of
        Just [ b ] ->
            Just b

        _ ->
            Nothing


orgaToUsersData : NodesDict -> UsersDict
orgaToUsersData nd =
    nd
        |> Dict.toList
        |> List.map (\( k, n ) -> Maybe.map (\fs -> ( nearestCircleid k, { username = fs.username, name = fs.name } )) n.first_link)
        |> List.filterMap identity
        |> toMapOfList



{-
   Auth
-}


getNodeRights : UserCtx -> Node -> GqlData NodesDict -> List UserRole
getNodeRights uctx target odata =
    let
        orgaRoles =
            getOrgaRoles [ target.nameid ] uctx.roles
    in
    if List.length orgaRoles == 0 then
        []

    else if isOwner orgaRoles then
        List.filter (\r -> r.role_type == RoleType.Owner) orgaRoles

    else
        let
            childrenRoles =
                getChildren target.nameid odata |> List.filter (\n -> n.type_ == NodeType.Role)

            childrenCoordos =
                List.filter (\n -> n.role_type == Just RoleType.Coordinator) childrenRoles

            circleRoles =
                getCircleRoles [ target.nameid ] orgaRoles

            allCoordoRoles =
                getCoordoRoles orgaRoles

            coordoRoles =
                getCoordoRoles circleRoles
        in
        case target.mode of
            NodeMode.Agile ->
                case circleRoles of
                    [] ->
                        -- No member in this circle
                        orgaRoles

                    circleRoles_ ->
                        circleRoles_

            NodeMode.Coordinated ->
                case coordoRoles of
                    [] ->
                        -- No coordo in this circle
                        if List.length childrenCoordos == 0 && List.length allCoordoRoles > 0 then
                            allCoordoRoles

                        else
                            []

                    coordoRoles_ ->
                        coordoRoles_


getTensionRights : UserCtx -> GqlData TensionHead -> GqlData LocalGraph -> Bool
getTensionRights uctx th_d path_d =
    case th_d of
        Success th ->
            case path_d of
                Success p ->
                    let
                        orgaRoles =
                            getOrgaRoles [ p.focus.nameid ] uctx.roles

                        childrenRoles =
                            p.focus.children |> List.filter (\n -> n.role_type /= Nothing)

                        childrenCoordos =
                            List.filter (\n -> n.role_type == Just RoleType.Coordinator) childrenRoles

                        circleRoles =
                            getCircleRoles [ th.receiver.nameid, th.emitter.nameid ] orgaRoles

                        coordoRoles =
                            getCoordoRoles circleRoles
                    in
                    if List.member uctx.username (th.assignees |> withDefault [] |> List.map (\u -> u.username)) then
                        -- assignee
                        True
                        --else if uctx.username == th.createdBy.username then
                        --    -- Author
                        --    True
                        --

                    else if isOwner orgaRoles then
                        -- is Owner
                        True

                    else
                        -- has role base autorization
                        case p.focus.mode of
                            NodeMode.Agile ->
                                -- Is a  Circle member
                                (List.length circleRoles > 0)
                                    || -- Or No member in this circle
                                       (List.length orgaRoles > 0)

                            NodeMode.Coordinated ->
                                -- Is a circle coordo
                                (List.length coordoRoles > 0)
                                    || -- Or No coordo in this circle
                                       (List.length childrenCoordos == 0 && List.length (getCoordoRoles orgaRoles) > 0)

                _ ->
                    False

        _ ->
            False
