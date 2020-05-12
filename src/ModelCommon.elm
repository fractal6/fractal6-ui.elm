module ModelCommon exposing (..)

import Array
import Dict exposing (Dict)
import Fractal.Enum.NodeType as NodeType
import Json.Decode as JD exposing (Value, decodeValue)
import Json.Encode as JE
import Json.Encode.Extra as JEE
import Maybe exposing (withDefault)
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
    , roles : Maybe (List UserRole)
    }


type alias UserRole =
    { rootnameid : String
    , nameid : String
    , name : String
    , role_type : String
    }



--
-- Focus
--


type alias NodeFocus =
    { rootid : String
    , nameid : String
    , isRoot : Bool

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


type TensionStep
    = TensionTypeForm
    | TensionFinalForm
    | TensionValidation


type alias TensionForm =
    { step : TensionStep
    , post : Post
    , result : GqlData (Maybe AddTensionPayload)
    , target : Node
    , source : Node
    }


type alias NodeTarget =
    -- Helper for encoding ActionState / Receiving Node from JS.
    Result JD.Error Node



--
-- Getters
--


uriFromFocus : NodeFocus -> String
uriFromFocus focus =
    if focus.isRoot then
        String.join "/" [ "/org", focus.rootid ]

    else
        String.join "/" [ "/org", focus.rootid, focus.nameid ]



--
-- Json Decoders/Encoders
--


userDecoder : JD.Decoder UserCtx
userDecoder =
    JD.map3 UserCtx
        (JD.field "username" JD.string)
        (JD.maybe <| JD.field "name" JD.string)
        (JD.maybe <|
            JD.field "roles" <|
                JD.list <|
                    JD.map4 UserRole
                        (JD.field "rootnameid" JD.string)
                        (JD.field "nameid" JD.string)
                        (JD.field "name" JD.string)
                        (JD.field "role_type" JD.string)
        )


userEncoder : UserCtx -> JE.Value
userEncoder userCtx =
    JE.object
        [ ( "username", JE.string userCtx.username )
        , ( "name", JEE.maybe JE.string userCtx.name )
        , ( "roles"
          , JE.list JE.object <|
                List.map
                    (\r ->
                        [ ( "rootnameid", JE.string r.rootnameid )
                        , ( "nameid", JE.string r.nameid )
                        , ( "name", JE.string r.name )
                        , ( "role_type", JE.string r.role_type )
                        ]
                    )
                    (userCtx.roles |> withDefault [])
          )
        ]



--
-- GraphPack and Nodes Encoder
--


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
