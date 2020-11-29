module ModelCommon exposing (..)

import Array exposing (Array)
import Components.Loading as Loading exposing (ErrorData, GqlData, RequestResult(..), WebData)
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
import Json.Decode as JD
import Json.Decode.Extra as JDE
import Json.Encode as JE
import Json.Encode.Extra as JEE
import Maybe exposing (withDefault)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), NodeFocus, nearestCircleid, nodeFromFocus)
import ModelSchema exposing (..)
import RemoteData
import Set
import Url exposing (Url)



--
-- Session / Global
--


type alias SessionFlags =
    { uctx : Maybe JD.Value
    , apis : Apis
    }


type alias Session =
    { user : UserState
    , referer : Maybe Url
    , token_data : WebData UserCtx
    , node_focus : Maybe NodeFocus
    , focus : Maybe FocusNode
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
    }


type alias Apis =
    { gql : String
    , rest : String
    , auth : String
    }


type alias NodesQuickSearch =
    { pattern : String
    , lookup : Array Node
    , idx : Int
    , visible : Bool
    }


initSession : SessionFlags -> Session
initSession flags =
    { referer = Nothing
    , user = LoggedOut
    , token_data = RemoteData.NotAsked
    , node_focus = Nothing
    , focus = Nothing
    , path_data = Nothing
    , orga_data = Nothing
    , users_data = Nothing
    , node_data = Nothing
    , tensions_data = Nothing
    , tension_head = Nothing
    , isAdmin = Nothing
    , node_action = Nothing
    , node_quickSearch = Nothing
    , apis = flags.apis
    }


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
    = ActionChoice Node
    | AddTension TensionStep
    | AddCircle NodeStep
    | JoinOrga (JoinStep JoinOrgaForm)
    | ActionAuthNeeded
    | AskErr String
    | NoOp



-- Tension Form


type alias TensionForm =
    { uctx : UserCtx
    , source : UserRole
    , target : Node
    , targetData : NodeData
    , status : TensionStatus.TensionStatus
    , tension_type : TensionType.TensionType
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
    , pattern : String
    , assignee : User -- last one clicked/selected
    , isNew : Bool -- toggle select
    , events_type : Maybe (List TensionEvent.TensionEvent)
    , post : Post
    }


initAssigneeForm : UserState -> String -> AssigneeForm
initAssigneeForm user tid =
    { uctx =
        case user of
            LoggedIn uctx ->
                uctx

            LoggedOut ->
                UserCtx "" Nothing (UserRights False False) []
    , tid = tid
    , pattern = ""
    , assignee = User "" Nothing
    , isNew = False
    , events_type = Nothing
    , post = Dict.empty
    }


{-|

    Create tension a the current focus

-}
initTensionForm : NodeFocus -> TensionForm
initTensionForm focus =
    { uctx = UserCtx "" Nothing (UserRights False False) []
    , source = UserRole "" "" "" RoleType.Guest
    , target = nodeFromFocus focus
    , targetData = initNodeData
    , status = TensionStatus.Open
    , tension_type = TensionType.Operational
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



-- Join Form


type alias JoinOrgaForm =
    { uctx : UserCtx
    , rootnameid : String
    , post : Post
    }



-- Steps


{-| Tension Step
-}
type TensionStep
    = TensionInit
    | TensionSource (List UserRole)
    | TensionFinal
    | TensionNotAuthorized ErrorData


{-| Node Step (Role Or Circle, add and edit)
-}
type NodeStep
    = NodeInit
    | NodeSource (List UserRole)
    | NodeFinal
    | NodeNotAuthorized ErrorData


{-| Join Step
-}
type JoinStep form
    = JoinInit form
    | JoinValidation form (GqlData Node)
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


getParentidFromRole : UserRole -> String
getParentidFromRole role =
    let
        l =
            String.split "#" role.nameid
                |> List.filter (\x -> x /= "")
    in
    List.take (List.length l - 1) l
        |> String.join "#"


getParentFragmentFromRole role =
    let
        l =
            String.split "#" role.nameid
                |> List.filter (\x -> x /= "")
                |> Array.fromList
    in
    Array.get (Array.length l - 2) l |> withDefault ""


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



--
-- Json Decoders/Encoders
--
{-
   User decoder/encoder
-}


userCtxDecoder : JD.Decoder UserCtx
userCtxDecoder =
    JD.map4 UserCtx
        (JD.field "username" JD.string)
        (JD.maybe <| JD.field "name" JD.string)
        (JD.field "rights" <|
            JD.map2 UserRights
                (JD.field "canLogin" JD.bool)
                (JD.field "canCreateRoot" JD.bool)
        )
        --(JD.maybe <|
        (JD.field "roles"
            (JD.list <|
                JD.map4 UserRole
                    (JD.field "name" JD.string)
                    (JD.field "nameid" JD.string)
                    (JD.field "rootnameid" JD.string)
                    (JD.field "role_type" <| RoleType.decoder)
             --@DEBUG/BUG: fail siletnly if role_type is not present in Role !
            )
            |> JDE.withDefault []
        )


userCtxEncoder : UserCtx -> JE.Value
userCtxEncoder userCtx =
    JE.object
        [ ( "username", JE.string userCtx.username )
        , ( "name", JEE.maybe JE.string userCtx.name )
        , ( "rights"
          , JE.object
                [ ( "canLogin", JE.bool userCtx.rights.canLogin )
                , ( "canCreateRoot", JE.bool userCtx.rights.canCreateRoot )
                ]
          )
        , ( "roles"
          , JE.list JE.object <|
                List.map
                    (\r ->
                        [ ( "rootnameid", JE.string r.rootnameid )
                        , ( "nameid", JE.string r.nameid )
                        , ( "name", JE.string r.name )
                        , ( "role_type", JE.string <| RoleType.toString r.role_type )
                        ]
                    )
                    userCtx.roles
            --(userCtx.roles |> withDefault [])
          )
        ]


userDecoder : JD.Decoder User
userDecoder =
    JD.map2 User
        (JD.field "username" JD.string)
        (JD.maybe <| JD.field "name" JD.string)


usersEncoder : List User -> JE.Value
usersEncoder users =
    JE.list JE.object <| List.map userEncoder users


userEncoder : User -> List ( String, JE.Value )
userEncoder u =
    [ ( "username", JE.string u.username )
    , ( "name", JEE.maybe JE.string u.name )
    ]



{-
   Nodes Encoder/Decoder
-}


nodesEncoder : NodesData -> JE.Value
nodesEncoder nodes =
    JE.list JE.object <| List.map nodeEncoder <| Dict.values nodes


nodeEncoder : Node -> List ( String, JE.Value )
nodeEncoder node =
    [ ( "createdAt", JE.string node.createdAt )
    , ( "name", JE.string node.name )
    , ( "nameid", JE.string node.nameid )
    , ( "rootnameid", JE.string node.rootnameid )
    , ( "parent"
      , JEE.maybe JE.object <|
            Maybe.map
                (\p ->
                    [ ( "nameid", JE.string p.nameid )
                    , ( "isPrivate", JE.bool p.isPrivate )
                    ]
                )
                node.parent
      )
    , ( "type_", JE.string <| NodeType.toString node.type_ )
    , ( "role_type", JEE.maybe JE.string <| Maybe.map (\t -> RoleType.toString t) node.role_type )
    , ( "first_link", JEE.maybe JE.object <| Maybe.map (\fs -> userEncoder fs) node.first_link )
    , ( "charac"
      , JE.object
            [ ( "userCanJoin", JE.bool node.charac.userCanJoin )
            , ( "mode", JE.string <| NodeMode.toString node.charac.mode )
            ]
      )
    , ( "isPrivate", JE.bool node.isPrivate )
    , ( "source"
      , JEE.maybe JE.object <|
            Maybe.map
                (\p ->
                    [ ( "id", JE.string p.id ) ]
                )
                node.source
      )
    ]



-- Node decoder --When receiving data from Javascript


nodeDecoder : JD.Decoder Node
nodeDecoder =
    --JD.map9 Node
    JD.succeed Node
        |> JDE.andMap (JD.field "createdAt" JD.string)
        |> JDE.andMap (JD.field "name" JD.string)
        |> JDE.andMap (JD.field "nameid" JD.string)
        |> JDE.andMap (JD.field "rootnameid" JD.string)
        |> JDE.andMap (JD.maybe (JD.field "parent" nodeIdDecoder))
        |> JDE.andMap (JD.field "type_" NodeType.decoder)
        |> JDE.andMap (JD.maybe (JD.field "role_type" RoleType.decoder))
        |> JDE.andMap (JD.maybe (JD.field "first_link" userDecoder))
        |> JDE.andMap (JD.field "charac" characDecoder)
        |> JDE.andMap (JD.field "isPrivate" JD.bool)
        |> JDE.andMap (JD.maybe (JD.field "source" blobIdDecoder))


idDecoder : JD.Decoder IdPayload
idDecoder =
    JD.map IdPayload (JD.field "id" JD.string)


blobIdDecoder : JD.Decoder BlobId
blobIdDecoder =
    JD.map2 BlobId
        (JD.field "id" JD.string)
        (JD.field "tension" idDecoder)


nodeIdDecoder : JD.Decoder NodeId
nodeIdDecoder =
    JD.map2 NodeId
        (JD.field "nameid" JD.string)
        (JD.field "isPrivate" JD.bool)


characDecoder : JD.Decoder NodeCharac
characDecoder =
    JD.map2 NodeCharac
        (JD.field "userCanJoin" JD.bool)
        (JD.field "mode" NodeMode.decoder)


localGraphDecoder : JD.Decoder LocalGraph
localGraphDecoder =
    -- @Debug: manually update the localGraph type in  nodeFocusedFromJs port
    JD.map3 LocalGraph
        (JD.maybe <|
            JD.field "root" <|
                JD.map4 RootNode
                    (JD.field "name" JD.string)
                    (JD.field "nameid" JD.string)
                    (JD.field "charac" characDecoder)
                    (JD.field "isPrivate" JD.bool)
        )
        (JD.field "path"
            (JD.list <|
                JD.map3 PNode
                    (JD.field "name" JD.string)
                    (JD.field "nameid" JD.string)
                    (JD.field "isPrivate" JD.bool)
            )
        )
        (JD.field "focus" <|
            JD.map6 FocusNode
                (JD.field "name" JD.string)
                (JD.field "nameid" JD.string)
                (JD.field "type_" NodeType.decoder)
                (JD.field "charac" characDecoder)
                (JD.field "children"
                    (JD.list <|
                        JD.map4 EmitterOrReceiver
                            (JD.field "name" JD.string)
                            (JD.field "nameid" JD.string)
                            (JD.maybe (JD.field "role_type" RoleType.decoder))
                            (JD.field "isPrivate" JD.bool)
                    )
                )
                (JD.field "isPrivate" JD.bool)
        )



-- Utils


type alias LocalGraph_ =
    -- Helper for encoding ActionState / Receiving Node from JS.
    --Result JD.Error Node
    Result String LocalGraph


type alias Node_ =
    -- Helper for encoding ActionState / Receiving Node from JS.
    --Result JD.Error Node
    Result String Node


type alias LookupResult a =
    -- Helper for encoding ActionState / Receiving Node from JS.
    --Result JD.Error Node
    Result String (List a)
