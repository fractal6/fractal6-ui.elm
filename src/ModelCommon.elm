module ModelCommon exposing (..)

import Array exposing (Array)
import Components.Loading as Loading
    exposing
        ( GqlData
        , RequestResult(..)
        , WebData
        , withMaybeData
        , withMaybeDataMap
        )
import Dict exposing (Dict)
import Dict.Extra as DE
import Extra exposing (toMapOfList)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.ContractStatus as ContractStatus
import Fractal.Enum.ContractType as ContractType
import Fractal.Enum.Lang as Lang
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon.Codecs
    exposing
        ( FractalBaseRoute(..)
        , contractIdCodec
        , getCircleRoles
        , getCoordoRoles
        , getOrgaRoles
        , isCircle
        , isOwner
        , memberIdCodec
        , nearestCircleid
        , nid2rootid
        , nodeFromFragment
        , voteIdCodec
        )
import ModelSchema exposing (..)
import Set



{-

   ModelCommon are a collections of modesl forms share accros components
   and utiliy function for data manipulations.

-}
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
-- Forms
--
{- -}
-- Tension Form


type alias Ev =
    { event_type : TensionEvent.TensionEvent
    , old : String
    , new : String
    }


ev2eventFragment : Ev -> EventFragment
ev2eventFragment ev =
    { event_type = ev.event_type, old = Just ev.old, new = Just ev.new }


type alias TensionForm =
    { id : String
    , uctx : UserCtx
    , source : UserRole
    , target : PNode
    , status : Maybe TensionStatus.TensionStatus
    , type_ : Maybe TensionType.TensionType
    , action : Maybe TensionAction.TensionAction
    , emitter : Maybe EmitterOrReceiver
    , receiver : Maybe EmitterOrReceiver
    , post : Post -- createdBy, createdAt, title, message and Node attr...

    -- data
    , events : List Ev
    , blob_type : Maybe BlobType.BlobType
    , node : NodeFragment
    , md : Maybe String
    , users : List UserForm
    , labels : List Label
    }


initTensionForm : String -> UserState -> TensionForm
initTensionForm tid user =
    { uctx =
        case user of
            LoggedIn uctx ->
                uctx

            LoggedOut ->
                initUserctx
    , id = tid
    , source = UserRole "" "" RoleType.Guest
    , target = initPNode
    , status = Nothing
    , type_ = Nothing
    , action = Nothing
    , emitter = Nothing
    , receiver = Nothing
    , post = Dict.empty
    , users = []
    , events = []
    , labels = []
    , blob_type = Nothing
    , node = initNodeFragment Nothing
    , md = Nothing
    }


type alias UserForm =
    -- Name is optional but when get user from lookup it
    -- allow to manage two records, a User and UserForm but only one.
    { username : String, name : Maybe String, email : String, pattern : String }


type alias CommentPatchForm =
    { id : String -- comment id (for edit/patch)
    , pid : String -- parent id (e.g. tid of cid) (for creating)
    , uctx : UserCtx
    , post : Post
    , viewMode : InputViewMode
    }


initCommentPatchForm : UserState -> CommentPatchForm
initCommentPatchForm user =
    { uctx = uctxFromUser user
    , id = ""
    , pid = ""
    , post = Dict.empty
    , viewMode = Write
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


encodeLabel : Label -> String
encodeLabel label =
    label.name ++ "§" ++ withDefault "" label.color


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



--Settings Form


type alias ArtefactNodeForm =
    { uctx : UserCtx
    , id : String
    , nameid : String
    , post : Post
    , mandate : Mandate
    }


initArtefactNodeForm : UserState -> String -> ArtefactNodeForm
initArtefactNodeForm user nameid =
    { uctx =
        case user of
            LoggedIn uctx ->
                uctx

            LoggedOut ->
                initUserctx
    , id = ""
    , nameid = nameid
    , post = Dict.fromList [ ( "color", "#dddddd" ) ]
    , mandate = initMandate
    }



-- Join Form


type alias ActionForm =
    { uctx : UserCtx
    , tid : String
    , bid : String
    , node : Node
    , fragment : NodeFragment
    , events : List Ev
    , users : List UserForm -- Note: For contract, One event <-> One user (candidate)
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
    , users = []
    , events = []
    , post = Dict.empty
    }


initUserForm : UserForm
initUserForm =
    { username = "", name = Nothing, email = "", pattern = "" }


tensionToActionForm : TensionForm -> ActionForm
tensionToActionForm form =
    initActionForm form.id (LoggedIn form.uctx)
        |> (\f ->
                { f
                    | node = nodeFromFragment form.target.nameid form.node
                    , users = form.users
                    , events = form.events
                    , post = form.post
                }
           )



-- User Profile Form


type alias UserProfileForm =
    { username : String
    , notifyByEmail : Maybe Bool
    , lang : Maybe Lang.Lang
    , post : Post
    }


initUserProfileForm : String -> UserProfileForm
initUserProfileForm username =
    UserProfileForm username Nothing Nothing Dict.empty


type alias OrgaForm =
    { uctx : UserCtx
    , post : Post
    }



{-
   Contract Form
-}


type alias ContractForm =
    { uctx : UserCtx
    , tid : String
    , status : ContractStatus.ContractStatus
    , contract_type : ContractType.ContractType
    , event : EventFragment
    , contractid : String
    , participants : List Vote
    , candidates : List Username
    , pending_candidates : List Email
    , post : Post
    }


initContractForm : UserState -> ContractForm
initContractForm user =
    { uctx =
        case user of
            LoggedIn uctx ->
                uctx

            LoggedOut ->
                initUserctx
    , tid = "" -- example
    , status = ContractStatus.Open
    , contract_type = ContractType.AnyCoordoDual
    , event = initEventFragment
    , contractid = ""
    , participants = []
    , candidates = []
    , pending_candidates = []
    , post = Dict.empty
    }


buildVote : String -> String -> String -> Int -> Vote
buildVote contractid rootnameid username value =
    { voteid = voteIdCodec contractid rootnameid username
    , node = { nameid = memberIdCodec rootnameid username }
    , data = [ value ]
    }


makeCandidateContractForm : ActionForm -> List ContractForm
makeCandidateContractForm form =
    List.map2 Tuple.pair form.users form.events
        |> List.map
            (\( u, e ) ->
                let
                    -- @codefactor: put it in Codec.contractIdCodec.
                    -- (pobleme with circular import due to TensionEvent defined in ModelCommon)
                    ( et, old, new ) =
                        ( TensionEvent.toString e.event_type, e.old, e.new )

                    contractid =
                        contractIdCodec form.tid et old new

                    rootnameid =
                        nid2rootid form.node.nameid

                    -- Feed candidate and pendingcandidate (only one candidate/pendingCandidate supported)
                    ( candidates, pending_candidates ) =
                        List.foldl
                            (\uf ( cand, pend ) ->
                                if uf.email == "" then
                                    ( [ { username = uf.username } ], [] )

                                else
                                    ( [], [ { email = uf.email } ] )
                            )
                            ( [], [] )
                            [ u ]
                in
                { uctx = form.uctx
                , tid = form.tid
                , event = ev2eventFragment e
                , post = form.post
                , status = ContractStatus.Open
                , contract_type = ContractType.AnyCandidates
                , contractid = contractid
                , participants = [ buildVote contractid rootnameid form.uctx.username 1 ]
                , candidates = candidates
                , pending_candidates = pending_candidates
                }
            )


form2cid : ActionForm -> String
form2cid form =
    let
        -- @codefactor: put it in Codec.contractIdCodec.
        -- (pobleme with circular import due to TensionEvent defined in ModelCommon)
        ( et, old, new ) =
            List.head form.events
                |> Maybe.map (\x -> ( TensionEvent.toString x.event_type, x.old, x.new ))
                |> withDefault ( "", "", "" )
    in
    contractIdCodec form.tid et old new


isSelfContract : UserCtx -> List UserForm -> Bool
isSelfContract uctx users =
    List.length users
        == 1
        && List.member uctx.username (List.map (\x -> x.username) users)



-- View


type InputViewMode
    = Write
    | Preview



--
-- Getters
--


isFreshOrga : NodesDict -> Bool
isFreshOrga data =
    (Dict.filter
        (\k v ->
            v.role_type /= Just RoleType.Owner && v.role_type /= Just RoleType.Pending
        )
        data
        |> Dict.size
    )
        == 1


getCircles : GqlData LocalGraph -> List PNode
getCircles lg =
    case lg of
        Success path ->
            path.path

        -- ++ (path.focus.children |> List.filter (\x -> isCircle x.nameid) |> List.map shrinkNode)
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


getParentFragmentFromRole role =
    let
        l =
            String.split "#" role.nameid
                |> List.filter (\x -> x /= "")
                |> Array.fromList
    in
    Array.get (Array.length l - 2) l |> withDefault ""


nodeFromTension t =
    t.blobs
        |> withDefault []
        |> List.head
        |> Maybe.map (\h -> h.node)
        |> withDefault Nothing
        |> withDefault (initNodeFragment Nothing)


mdFromTension t =
    t.blobs
        |> withDefault []
        |> List.head
        |> Maybe.map (\h -> h.md)
        |> withDefault Nothing


tidFromPath : GqlData LocalGraph -> Maybe String
tidFromPath path =
    case path of
        Success p ->
            p.focus.source
                |> Maybe.map .tension
                |> Maybe.map .id

        _ ->
            Nothing


localGraphFromOrga : String -> GqlData NodesDict -> Maybe LocalGraph
localGraphFromOrga nameid orga_d =
    case orga_d of
        Success orga ->
            let
                root_m =
                    Dict.get (nid2rootid nameid) orga
                        |> Maybe.map
                            (\n ->
                                { name = n.name
                                , nameid = n.nameid
                                , userCanJoin = n.userCanJoin
                                }
                            )

                focus_m =
                    Dict.get nameid orga
                        |> Maybe.map
                            (\n ->
                                { name = n.name
                                , nameid = n.nameid
                                , type_ = n.type_
                                , visibility = n.visibility
                                , mode = n.mode
                                , source = n.source
                                , children =
                                    DE.filterMap
                                        (\_ c ->
                                            Maybe.map
                                                (\p ->
                                                    if p.nameid == nameid then
                                                        Just { name = c.name, nameid = c.nameid, role_type = c.role_type }

                                                    else
                                                        Nothing
                                                )
                                                c.parent
                                                |> withDefault Nothing
                                        )
                                        orga
                                        |> Dict.values
                                }
                            )

                getPath : String -> List PNode
                getPath nameid_ =
                    Dict.get nameid_ orga
                        |> Maybe.map
                            (\n ->
                                { name = n.name
                                , nameid = n.nameid
                                , source = n.source
                                }
                                    :: (case n.parent of
                                            Just p ->
                                                getPath p.nameid

                                            Nothing ->
                                                []
                                       )
                            )
                        |> withDefault []
            in
            Maybe.map
                (\f ->
                    { root = root_m
                    , path = getPath nameid |> List.reverse
                    , focus = f
                    }
                )
                focus_m

        _ ->
            Nothing



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


orgaToUsers : NodesDict -> List User
orgaToUsers nd =
    nd
        |> Dict.toList
        |> List.map (\( k, n ) -> n.first_link)
        |> List.filterMap identity
        |> LE.uniqueBy (\u -> u.username)



{-
   Auth
-}


{-| Returns Admin roles, which covers the

  - Admin User of the orga
  - User with coordo role below that node
  - User with the first corrdo role on parent, if no cordo below.

-}
getNodeRights : UserCtx -> Node -> GqlData NodesDict -> List UserRole
getNodeRights uctx target_ odata =
    let
        -- The authority to edit a Node is determined by the tension receiver Node,
        -- which is the direct parent of the givent Node.
        target =
            withMaybeData odata
                |> Maybe.map
                    (\data ->
                        Dict.get (target_.parent |> Maybe.map .nameid |> withDefault target_.nameid) data
                    )
                |> withDefault Nothing
                |> withDefault initNode

        orgaRoles =
            getOrgaRoles [ target.nameid ] uctx.roles
    in
    if List.length orgaRoles == 0 then
        []

    else if isOwner uctx target.nameid then
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


{-| Return True if user has tension edition rights, which covers the:

  - Orga Admin of the tension receiver node.
  - Coordo of the tension receiver node.
  - Assignee of the tension.

-}
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

                    else if isOwner uctx p.focus.nameid then
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


{-| First Circle then role, each group
sorted alphabetically
-}
sortNode a b =
    let
        len_a =
            List.length (String.split "#" a.nameid)

        len_b =
            List.length (String.split "#" b.nameid)
    in
    if len_a < len_b then
        LT

    else if len_a == len_b then
        if a.nameid < b.nameid then
            LT

        else
            GT

    else
        GT
