module ModelCommon exposing (..)

import Array exposing (Array)
import Components.Loading as Loading exposing (ErrorData, WebData)
import Dict exposing (Dict)
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionType as TensionType
import Json.Decode as JD
import Json.Decode.Extra as JDE
import Json.Encode as JE
import Json.Encode.Extra as JEE
import Maybe exposing (withDefault)
import ModelCommon.Uri as Uri exposing (FractalBaseRoute(..), NodeFocus, NodePath)
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
    , node_path : Maybe NodePath
    , orga_data : Maybe NodesData
    , circle_tensions : Maybe TensionsData
    , node_action : Maybe ActionState
    , node_quickSearch : Maybe NodesQuickSearch
    }


type alias NodesQuickSearch =
    { pattern : String
    , lookup : Array Node
    , idx : Int

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
-- Action Step and Form Data
--


type ActionState
    = ActionChoice Node
    | AddTension (TensionStep TensionForm)
    | AddCircle (CircleStep CircleForm)
    | JoinOrga (JoinStep JoinOrgaForm)
    | ActionAuthNeeded
    | AskErr String
    | NotAsk



-- Tension Form


type TensionStep form
    = TensionInit form
    | TensionSource form (List UserRole)
    | TensionFinal form (GqlData (Maybe Tension))
    | TensionNotAuthorized ErrorData


type alias TensionForm =
    { user : UserCtx
    , source : Maybe UserRole
    , target : Node
    , post : Post
    }



-- Circle Form


type CircleStep form
    = CircleInit form
    | CircleSource form (List UserRole)
    | CircleFinal form (GqlData (Maybe Node))
    | CircleNotAuthorized ErrorData


type alias CircleForm =
    { user : UserCtx
    , source : Maybe UserRole
    , target : Node
    , post : Post
    }



-- Join Form


type JoinStep form
    = JoinInit form
    | JoinValidation form (GqlData (Maybe Node))
    | JoinNotAuthorized ErrorData
    | JoinAuthNeeded


type alias JoinOrgaForm =
    { user : UserCtx
    , rootnameid : String
    }



--
-- Getters
--


getNodeMode : String -> OrgaData -> Maybe NodeMode.NodeMode
getNodeMode nameid orga =
    case orga of
        Success nodes ->
            Dict.get nameid nodes
                |> Maybe.map (\n -> n.charac.mode)

        _ ->
            Nothing


getNodeName : String -> OrgaData -> String
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


getParentFragmentFromRole : UserRole -> String
getParentFragmentFromRole role =
    let
        l =
            String.split "#" role.nameid
                |> List.filter (\x -> x /= "")
                |> Array.fromList
    in
    Array.get (Array.length l - 2) l |> withDefault ""



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
        |> JDE.andMap (JD.maybe (JD.map ParentNode <| JD.field "parentid" JD.string))
        |> JDE.andMap (JD.field "type_" NodeType.decoder)
        |> JDE.andMap (JD.maybe (JD.field "role_type" RoleType.decoder))
        |> JDE.andMap (JD.maybe (JD.map Username <| JD.field "first_link" JD.string))
        |> JDE.andMap
            (JD.field "charac" <|
                JD.map2 NodeCharac
                    (JD.field "userCanJoin" JD.bool)
                    (JD.field "mode" NodeMode.decoder)
            )



-- Utils


type alias Node_ =
    -- Helper for encoding ActionState / Receiving Node from JS.
    --Result JD.Error Node
    Result String Node


type alias Nodes_ =
    -- Helper for encoding ActionState / Receiving Node from JS.
    --Result JD.Error Node
    Result String (List Node)
