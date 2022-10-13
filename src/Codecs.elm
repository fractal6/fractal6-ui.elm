module Codecs exposing (..)

import Dict
import Fractal.Enum.Lang as Lang
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.NodeVisibility as NodeVisibility
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.UserType as UserType
import Json.Decode as JD
import Json.Decode.Extra as JDE
import Json.Encode as JE
import Json.Encode.Extra as JEE
import Loading exposing (ModalData)
import ModelSchema
    exposing
        ( BlobId
        , EmitterOrReceiver
        , FocusNode
        , IdPayload
        , Label
        , NameidPayload
        , Node
        , NodeId
        , NodesDict
        , PNode
        , RNode
        , User
        , UserCtx
        , UserRights
        , UserRole
        )



--
-- Json Decoders/Encoders
--
{-
   Label decoder/encoder
-}


labelDecoder : JD.Decoder Label
labelDecoder =
    JD.map4 Label
        (JD.field "id" JD.string)
        (JD.field "name" JD.string)
        (JD.maybe <| JD.field "color" JD.string)
        (JD.field "nodes" (JD.list <| JD.map NameidPayload (JD.field "nameid" JD.string)))


labelEncoder : Label -> List ( String, JE.Value )
labelEncoder u =
    [ ( "id", JE.string u.id )
    , ( "name", JE.string u.name )
    , ( "color", JEE.maybe JE.string u.color )
    ]


labelsEncoder : List Label -> JE.Value
labelsEncoder users =
    JE.list JE.object <| List.map labelEncoder users



{-
   User decoder/encoder
-}


userCtxDecoder : JD.Decoder UserCtx
userCtxDecoder =
    JD.map7 UserCtx
        (JD.maybe <| JD.field "name" JD.string)
        (JD.field "username" JD.string)
        (JD.field "lang" <| Lang.decoder)
        (JD.field "rights" <|
            JD.map3 UserRights
                (JD.field "canLogin" JD.bool)
                (JD.field "canCreateRoot" JD.bool)
                (JD.field "type_" <| UserType.decoder)
        )
        (JD.field "roles"
            (JD.list <|
                JD.map4 UserRole
                    (JD.field "name" JD.string)
                    (JD.field "nameid" JD.string)
                    (JD.field "role_type" <| RoleType.decoder)
                    (JD.maybe <| JD.field "color" JD.string)
             --@DEBUG/BUG: fail silently if role_type is not present in Role !
            )
            |> JDE.withDefault []
        )
        (JD.field "client_version" JD.string)
        (JD.field "expiresAt" JD.string)


userCtxEncoder : UserCtx -> JE.Value
userCtxEncoder userCtx =
    JE.object
        [ ( "name", JEE.maybe JE.string userCtx.name )
        , ( "username", JE.string userCtx.username )
        , ( "lang", JE.string <| Lang.toString userCtx.lang )
        , ( "rights"
          , JE.object
                [ ( "canLogin", JE.bool userCtx.rights.canLogin )
                , ( "canCreateRoot", JE.bool userCtx.rights.canCreateRoot )
                , ( "type_", JE.string <| UserType.toString userCtx.rights.type_ )
                ]
          )
        , ( "roles"
          , JE.list JE.object <|
                List.map
                    (\r ->
                        [ ( "nameid", JE.string r.nameid )
                        , ( "name", JE.string r.name )
                        , ( "role_type", JE.string <| RoleType.toString r.role_type )
                        , ( "color", JEE.maybe JE.string r.color )
                        ]
                    )
                    userCtx.roles
            --(userCtx.roles |> withDefault [])
          )
        , ( "client_version", JE.string userCtx.client_version )
        , ( "expiresAt", JE.string userCtx.expiresAt )
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


nodesEncoder : NodesDict -> JE.Value
nodesEncoder nodes =
    JE.list JE.object <| List.map nodeEncoder <| Dict.values nodes


nodeEncoder : Node -> List ( String, JE.Value )
nodeEncoder node =
    [ ( "name", JE.string node.name )
    , ( "nameid", JE.string node.nameid )
    , ( "parent"
      , JEE.maybe JE.object <|
            Maybe.map
                (\p -> [ ( "nameid", JE.string p.nameid ) ])
                node.parent
      )
    , ( "type_", JE.string <| NodeType.toString node.type_ )
    , ( "role_type", JEE.maybe JE.string <| Maybe.map (\t -> RoleType.toString t) node.role_type )
    , ( "color", JEE.maybe JE.string node.color )
    , ( "first_link", JEE.maybe JE.object <| Maybe.map (\fs -> userEncoder fs) node.first_link )
    , ( "visibility", JE.string <| NodeVisibility.toString node.visibility )
    , ( "mode", JE.string <| NodeMode.toString node.mode )
    , ( "source"
      , JEE.maybe JE.object <|
            Maybe.map
                (\p ->
                    [ ( "id", JE.string p.id )
                    , ( "tension", JE.object [ ( "id", JE.string p.tension.id ) ] )
                    ]
                )
                node.source
      )
    , ( "userCanJoin", JEE.maybe JE.bool node.userCanJoin )
    ]



-- Node decoder --When receiving data from Javascript


nodeDecoder : JD.Decoder Node
nodeDecoder =
    JD.succeed Node
        |> JDE.andMap (JD.field "name" JD.string)
        |> JDE.andMap (JD.field "nameid" JD.string)
        |> JDE.andMap (JD.maybe (JD.field "parent" nodeIdDecoder))
        |> JDE.andMap (JD.field "type_" NodeType.decoder)
        |> JDE.andMap (JD.maybe (JD.field "role_type" RoleType.decoder))
        |> JDE.andMap (JD.maybe (JD.field "color" JD.string))
        |> JDE.andMap (JD.maybe (JD.field "first_link" userDecoder))
        |> JDE.andMap (JD.field "visibility" NodeVisibility.decoder)
        |> JDE.andMap (JD.field "mode" NodeMode.decoder)
        |> JDE.andMap (JD.maybe (JD.field "source" blobIdDecoder))
        |> JDE.andMap (JD.maybe (JD.field "userCanJoin" JD.bool))


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
        (JD.maybe (JD.field "source" blobIdDecoder))


emitterOrReceiverDecoder : JD.Decoder EmitterOrReceiver
emitterOrReceiverDecoder =
    JD.map4 EmitterOrReceiver
        (JD.field "name" JD.string)
        (JD.field "nameid" JD.string)
        (JD.maybe (JD.field "role_type" RoleType.decoder))
        (JD.maybe (JD.field "color" JD.string))



{-
   Window Decoder
-}


type alias WindowPos =
    { bottomLeft : String
    , topRight : String
    }


windowDecoder : JD.Decoder WindowPos
windowDecoder =
    JD.map2 WindowPos
        (JD.field "bottomLeft" JD.string)
        (JD.field "topRight" JD.string)


windowEncoder : WindowPos -> JE.Value
windowEncoder w =
    JE.object
        [ ( "bottomLeft", JE.string w.bottomLeft )
        , ( "topRight", JE.string w.topRight )
        ]



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


{-| Modal Decoder
-}
modalDataDecoder : JD.Decoder ModalData
modalDataDecoder =
    JD.map2 ModalData
        (JD.field "reset" JD.bool)
        (JD.field "link" JD.string)



-- Utils


type alias LookupResult a =
    Result String (List a)
