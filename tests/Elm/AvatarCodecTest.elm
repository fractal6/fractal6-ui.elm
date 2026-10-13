module Elm.AvatarCodecTest exposing (..)

import Codecs exposing (nodeDecoder, nodeEncoder, userCtxDecoder, userCtxEncoder, userDecoder, userEncoder)
import Expect
import Json.Decode as JD
import Json.Encode as JE
import ModelSchema exposing (initNode, initUserctx)
import Test exposing (Test, describe, test)


avatarCodecTests : Test
avatarCodecTests =
    describe "avatar codecs"
        [ test "user without avatar" <|
            \_ ->
                JD.decodeString userDecoder """{"username":"bob","name":"Bob"}"""
                    |> Expect.equal (Ok { username = "bob", name = Just "Bob", avatar = Nothing })
        , test "user with nested avatar (backend shape)" <|
            \_ ->
                JD.decodeString userDecoder """{"username":"bob","avatar":{"id":"0x2a"}}"""
                    |> Result.map .avatar
                    |> Expect.equal (Ok (Just "0x2a"))
        , test "user round trip" <|
            \_ ->
                let
                    u =
                        { username = "bob", name = Nothing, avatar = Just "0x2a" }
                in
                JE.object (userEncoder u)
                    |> JD.decodeValue userDecoder
                    |> Expect.equal (Ok u)
        , test "uctx without avatar" <|
            \_ ->
                JD.decodeString userCtxDecoder """{"username":"bob","lang":"EN","rights":{"canLogin":true,"canCreateRoot":false,"type_":"Regular"},"roles":[],"expiresAt":"x"}"""
                    |> Result.map .avatar
                    |> Expect.equal (Ok Nothing)
        , test "uctx round trip" <|
            \_ ->
                let
                    uctx =
                        { initUserctx | username = "bob", avatar = Just "0x2a" }
                in
                userCtxEncoder uctx
                    |> JD.decodeValue userCtxDecoder
                    |> Expect.equal (Ok uctx)
        , test "node without avatar encodes null" <|
            \_ ->
                JE.object (nodeEncoder initNode)
                    |> JD.decodeValue (JD.field "avatar" (JD.null True))
                    |> Expect.equal (Ok True)
        , test "node with absent or null avatar" <|
            \_ ->
                [ nodeJson "", nodeJson ",\"avatar\":null" ]
                    |> List.map (JD.decodeString nodeDecoder >> Result.map .avatar)
                    |> Expect.equal [ Ok Nothing, Ok Nothing ]
        , test "node with nested avatar (backend shape)" <|
            \_ ->
                JD.decodeString nodeDecoder (nodeJson ",\"avatar\":{\"id\":\"0x2a\"}")
                    |> Result.map .avatar
                    |> Expect.equal (Ok (Just "0x2a"))
        , test "node round trip" <|
            \_ ->
                let
                    n =
                        { initNode | name = "Org", nameid = "org", avatar = Just "0x2a" }
                in
                JE.object (nodeEncoder n)
                    |> JD.decodeValue nodeDecoder
                    |> Expect.equal (Ok n)
        ]


nodeJson : String -> String
nodeJson extra =
    """{"name":"Org","nameid":"org","type_":"Circle","visibility":"Public","mode":"Coordinated","n_open_tensions":0,"n_open_contracts":0""" ++ extra ++ "}"
