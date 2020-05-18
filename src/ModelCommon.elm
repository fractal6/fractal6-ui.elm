module ModelCommon exposing (..)

import Array
import Dict exposing (Dict)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionType as TensionType
import Json.Decode as JD
import Json.Decode.Extra as JDE
import Json.Encode as JE
import Json.Encode.Extra as JEE
import Maybe exposing (withDefault)
import ModelCommon.Uri as Uri exposing (FractalBaseRoute(..))
import ModelOrg exposing (..)



--
-- Session / Global
--


type alias Session =
    { user : UserState
    , node_focus : Maybe NodeFocus
    , node_path : Maybe NodePath
    , orga_data : Maybe NodesData
    , circle_tensions : Maybe TensionsData
    , node_action : Maybe ActionState
    }



--
-- User
--


type UserState
    = LoggedOut
    | LoggedIn UserCtx


type alias UserCtx =
    { username : String
    , name : Maybe String
    , rights : UserRights
    , roles : List UserRole
    }


type alias UserRights =
    { canLogin : Bool
    , canCreateRoot : Bool
    }


type alias UserRole =
    { rootnameid : String
    , nameid : String
    , name : String
    , role_type : RoleType.RoleType
    }



--
-- Focus
--


type alias NodeFocus =
    { rootnameid : String
    , isRoot : Bool
    , nameid : String
    , type_ : NodeType.NodeType

    --, name : Maybe String // get the name when JS/D3 finished the rendering Job
    }


type alias NodePath =
    Array.Array { nidjs : String, nameid : String, name : String }



--
-- Data Structures
--


type alias OrgaData =
    GqlData NodesData


type alias NodesData =
    Dict String Node


type alias CircleTensionsData =
    GqlData TensionsData



--
-- Action Step and Form Data
--


type ActionState
    = Ask (ActionStep Node)
    | AskErr String
    | NotAsk


type ActionStep target
    = FirstStep target -- AskActions
    | AddTensionStep TensionForm -- AskNewTension
    | AuthNeeded


type alias TensionForm =
    { step : TensionStep
    , post : Post
    , result : GqlData (Maybe AddTensionPayload)
    , source : Maybe Node
    , target : Node
    , user : UserCtx
    , rootnameid : String
    }


type TensionStep
    = TensionTypeForm
    | TensionSourceForm (List UserRole)
    | TensionFinalForm (Maybe UserRole)
    | TensionValidation
    | TensionNotAuthorized ErrorData


type alias NodeTarget =
    -- Helper for encoding ActionState / Receiving Node from JS.
    Result JD.Error Node



--
-- Getters
--


uriFromNameid : FractalBaseRoute -> String -> String
uriFromNameid loc nameid =
    let
        path =
            String.split "#" nameid

        b =
            Uri.toString loc
    in
    [ b ]
        ++ path
        |> String.join "/"
        |> String.append "/"



--|> String.append "/"


focusFromNameid : String -> NodeFocus
focusFromNameid nameid_ =
    let
        path =
            String.split "#" nameid_ |> Array.fromList

        -- get key nav path node
        rootid =
            Array.get 0 path |> withDefault ""

        lastNode =
            Array.get 1 path |> withDefault ""

        role =
            Array.get 2 path |> withDefault ""

        -- extra attribute
        isRoot =
            lastNode == "" && role == ""

        nodeType =
            if role == "" then
                NodeType.Circle

            else
                NodeType.Role

        -- build the node name ID
        nameid =
            if isRoot then
                rootid

            else if nodeType == NodeType.Circle then
                String.join "#" [ rootid, lastNode ]

            else
                String.join "#" [ rootid, lastNode, role ]
    in
    NodeFocus rootid isRoot nameid nodeType



--
-- Json Decoders/Encoders
--
-- User decoder/encoder


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
                    (JD.field "rootnameid" JD.string)
                    (JD.field "nameid" JD.string)
                    (JD.field "name" JD.string)
                    (JD.field "role_type" <| RoleType.decoder)
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



-- GraphPack and Nodes Encoder


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
    , ( "name", JE.string node.name )
    , ( "nameid", JE.string node.nameid )
    , ( "parentid", JEE.maybe JE.string <| Maybe.map (\x -> x.nameid) node.parent )
    , ( "type_", JE.string <| NodeType.toString node.type_ )
    ]
