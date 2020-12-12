module Codecs exposing (..)

import Dict
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Json.Decode as JD
import Json.Decode.Extra as JDE
import Json.Encode as JE
import Json.Encode.Extra as JEE
import ModelSchema
    exposing
        ( BlobId
        , EmitterOrReceiver
        , FocusNode
        , IdPayload
        , LocalGraph
        , Node
        , NodeCharac
        , NodeId
        , NodesData
        , PNode
        , RootNode
        , User
        , UserCtx
        , UserRights
        , UserRole
        )



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



{-
   Window Decoder
-}


type alias WindowPos =
    { two : String
    , three : String
    }


windowDecoder : JD.Decoder WindowPos
windowDecoder =
    JD.map2 WindowPos
        (JD.field "two" JD.string)
        (JD.field "three" JD.string)



{-
   QuickDoc Decoder
-}


type alias QuickDoc =
    List QDoc


type alias QDoc =
    { name : String
    , tasks : List TDoc
    }


type alias TDoc =
    { header : String, content : String }


quickDocDecoder : JD.Decoder QuickDoc
quickDocDecoder =
    JD.list <|
        JD.map2 QDoc
            (JD.field "name" JD.string)
            (JD.field "tasks"
                (JD.list <|
                    JD.map2 TDoc
                        (JD.field "header" JD.string)
                        (JD.field "content" JD.string)
                )
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
