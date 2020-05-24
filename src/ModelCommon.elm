module ModelCommon exposing (..)

import Components.Loading as Loading exposing (WebData)
import Dict exposing (Dict)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionType as TensionType
import Json.Decode as JD
import Json.Decode.Extra as JDE
import Json.Encode as JE
import Json.Encode.Extra as JEE
import Maybe exposing (withDefault)
import ModelCommon.Uri as Uri exposing (FractalBaseRoute(..), NodeFocus, NodePath)
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
    , token_data : WebData UserCtx
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
-- Action Step and Form Data
--


type ActionState
    = Ask (ActionStep Node) -- Node actions
    | JoinOrga (JoinStep JoinOrgaForm)
    | AskErr String
    | NotAsk


type
    ActionStep target
    -- Actions shows up in a Modal
    = FirstStep target -- Pick an actions
    | AddTensionStep TensionForm -- AskNewTension
    | ActionAuthNeeded



-- Tension Form


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



-- Join Form


type JoinStep form
    = JoinInit form
    | JoinValidation form (GqlData (Maybe AddNodePayload))
    | JoinNotAuthorized ErrorData
    | JoinAuthNeeded


type alias JoinOrgaForm =
    { user : UserCtx
    , rootnameid : String
    }



--
-- Getters
--


getNodeName : OrgaData -> String -> String
getNodeName oData nameid =
    let
        errMsg =
            "Error: Node unknown"
    in
    case oData of
        Success nodes ->
            Dict.get nameid nodes
                |> Maybe.map (\n -> n.name)
                |> withDefault errMsg

        _ ->
            errMsg



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



-- Utils


type alias NodeTarget =
    -- Helper for encoding ActionState / Receiving Node from JS.
    Result JD.Error Node
