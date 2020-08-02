module ModelCommon exposing (..)

import Array exposing (Array)
import Components.Loading as Loading exposing (ErrorData, WebData)
import Dict exposing (Dict)
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
import ModelCommon.Uri as Uri exposing (FractalBaseRoute(..), NodeFocus)
import ModelSchema exposing (..)
import QuickSearch as Qsearch
import Url exposing (Url)



--
-- Session / Global
--


type alias Session =
    { user : UserState
    , referer : Url
    , token_data : WebData UserCtx
    , node_focus : Maybe NodeFocus
    , path_data : Maybe LocalGraph
    , orga_data : Maybe NodesData
    , tensions_circle : Maybe TensionsData
    , data : Maybe NodeData
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

    --, lut : Qsearch.Table Node
    --, lookup : List Node
    }



--
-- User State
--


type UserState
    = LoggedOut
    | LoggedIn UserCtx



--
-- Modal
--


type alias UserForm =
    { post : Dict String String
    , result : WebData UserCtx
    }


type ModalAuth
    = Inactive
    | Active UserForm



--
-- Action Step and Form Data
--


type ActionState
    = ActionChoice Node
    | AddTension (TensionStep TensionForm)
    | AddCircle (NodeStep TensionForm (List Node))
    | EditAbout (NodeStep TensionForm IdPayload)
    | EditMandate (NodeStep TensionForm IdPayload)
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
    , tension_type : TensionType.TensionType
    , action : Maybe TensionAction.TensionAction
    , post : Post -- For String type,  createdBy, createdAt, title, message, etc

    -- data
    , event_type : Maybe TensionEvent.TensionEvent
    , blob_type : Maybe BlobType.BlobType
    , node : NodeFragment
    }


type alias TensionPatchForm =
    { id : String
    , uctx : UserCtx
    , status : Maybe TensionStatus.TensionStatus
    , emitter : Maybe EmitterOrReceiver
    , receiver : Maybe EmitterOrReceiver
    , post : Post -- createdBy, createdAt, title, message...

    -- data
    , event_type : Maybe TensionEvent.TensionEvent
    , blob_type : Maybe BlobType.BlobType
    , node : NodeFragment
    }


type alias CommentPatchForm =
    { id : String
    , uctx : UserCtx
    , post : Post
    , viewMode : InputViewMode
    }



-- Join Form


type alias JoinOrgaForm =
    { uctx : UserCtx
    , rootnameid : String
    , id : Maybe String
    , post : Post
    }



-- Steps


{-| Tension Step
-}
type TensionStep form
    = TensionInit form
    | TensionSource form (List UserRole)
    | TensionFinal form (GqlData Tension)
    | TensionNotAuthorized ErrorData


{-| Node Step (Role Or Circle, add and edit)
-}
type NodeStep form data
    = NodeInit form
    | NodeSource form (List UserRole)
    | NodeFinal form (GqlData data)
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


{-|

    Push a new tension in the model if data is success

-}
hotTensionPush : Tension -> GqlData TensionsData -> TensionsData
hotTensionPush tension tsData =
    case tsData of
        Success tensions ->
            [ tension ] ++ tensions

        other ->
            []


{-|

    Push a new node in the model if data is success

-}
hotNodePush : List Node -> GqlData NodesData -> NodesData
hotNodePush nodes odata =
    case odata of
        Success data ->
            Dict.union (List.map (\n -> ( n.nameid, n )) nodes |> Dict.fromList) data

        other ->
            Dict.empty



-- Json Decoders/Encoders
{-
   User decoder/encoder
-}


userDecoder : JD.Decoder UserCtx
userDecoder =
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


userEncoder : UserCtx -> JE.Value
userEncoder userCtx =
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



{-
   Nodes Encoder/Decoder
-}


graphPackEncoder : NodesData -> String -> JE.Value
graphPackEncoder data focus =
    JE.object
        [ ( "data", nodesEncoder data )
        , ( "focusid", JE.string focus )
        ]


nodesEncoder : NodesData -> JE.Value
nodesEncoder nodes =
    JE.list JE.object <| List.map nodeEncoder <| Dict.values nodes


nodeEncoder : Node -> List ( String, JE.Value )
nodeEncoder node =
    [ ( "id", JE.string node.id )
    , ( "createdAt", JE.string node.createdAt )
    , ( "name", JE.string node.name )
    , ( "nameid", JE.string node.nameid )
    , ( "rootnameid", JE.string node.rootnameid )
    , ( "parentid", JEE.maybe JE.string <| Maybe.map (\x -> x.nameid) node.parent )
    , ( "type_", JE.string <| NodeType.toString node.type_ )
    , ( "role_type", JEE.maybe JE.string <| Maybe.map (\t -> RoleType.toString t) node.role_type )
    , ( "first_link", JEE.maybe JE.string <| Maybe.map (\x -> x.username) node.first_link )
    , ( "charac"
      , JE.object
            [ ( "userCanJoin", JE.bool node.charac.userCanJoin )
            , ( "mode", JE.string <| NodeMode.toString node.charac.mode )
            ]
      )
    , ( "isPrivate", JE.bool node.isPrivate )
    ]



-- Node decoder --When receiving data from Javascript


nodeDecoder : JD.Decoder Node
nodeDecoder =
    --JD.map9 Node
    JD.succeed Node
        |> JDE.andMap (JD.field "id" JD.string)
        |> JDE.andMap (JD.field "createdAt" JD.string)
        |> JDE.andMap (JD.field "name" JD.string)
        |> JDE.andMap (JD.field "nameid" JD.string)
        |> JDE.andMap (JD.field "rootnameid" JD.string)
        |> JDE.andMap (JD.maybe (JD.map NodeId <| JD.field "parentid" JD.string))
        |> JDE.andMap (JD.field "type_" NodeType.decoder)
        |> JDE.andMap (JD.maybe (JD.field "role_type" RoleType.decoder))
        |> JDE.andMap (JD.maybe (JD.map Username <| JD.field "first_link" JD.string))
        |> JDE.andMap (JD.field "charac" characDecoder)
        |> JDE.andMap (JD.field "isPrivate" JD.bool)


characDecoder : JD.Decoder NodeCharac
characDecoder =
    JD.map2 NodeCharac
        (JD.field "userCanJoin" JD.bool)
        (JD.field "mode" NodeMode.decoder)


localGraphDecoder : JD.Decoder LocalGraph
localGraphDecoder =
    JD.map3 LocalGraph
        (JD.maybe <|
            JD.field "root" <|
                JD.map4 RootNode
                    (JD.field "name" JD.string)
                    (JD.field "nameid" JD.string)
                    (JD.field "charac" characDecoder)
                    (JD.field "id" JD.string)
        )
        (JD.field "path"
            (JD.list <|
                JD.map2 PNode
                    (JD.field "name" JD.string)
                    (JD.field "nameid" JD.string)
            )
        )
        (JD.field "focus" <|
            JD.map4 FocusNode
                (JD.field "name" JD.string)
                (JD.field "nameid" JD.string)
                (JD.field "type_" NodeType.decoder)
                (JD.field "children" (JD.list <| JD.map NodeId (JD.field "nameid" JD.string)))
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


type alias Nodes_ =
    -- Helper for encoding ActionState / Receiving Node from JS.
    --Result JD.Error Node
    Result String (List Node)
