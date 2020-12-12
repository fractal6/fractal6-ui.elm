module ModelCommon.Requests exposing (..)

import Codecs exposing (nodeIdDecoder, quickDocDecoder, userCtxDecoder, userDecoder)
import Components.Loading as Loading exposing (GqlData, RequestResult(..), WebData, expectJson, toErrorData)
import Dict exposing (Dict)
import Fractal.Enum.RoleType as RoleType
import Http
import Json.Decode as JD
import Json.Encode as JE
import Maybe exposing (withDefault)
import ModelSchema exposing (Member, NodeId, Post, UserCtx, UserRoleExtended)
import Query.QueryNode exposing (MemberNode, User)
import RemoteData exposing (RemoteData)



{-
   riskyRequest are needed to set cookies on the client through CORS.
-}


{-|

    Get all children recursively

-}
fetchChildren url targetid msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = url ++ "/sub_children"
        , body = Http.jsonBody <| JE.string targetid
        , expect = expectJson (RemoteData.fromResult >> msg) childrenDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


childrenDecoder : JD.Decoder (List NodeId)
childrenDecoder =
    JD.list nodeIdDecoder


{-|

    Get all member node (role with first link) recursively

-}
fetchMembers url targetid msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = url ++ "/sub_members"
        , body = Http.jsonBody <| JE.string targetid
        , expect = expectJson (RemoteData.fromResult >> membersDecoder2 >> msg) membersDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


membersDecoder : JD.Decoder (List MemberNode)
membersDecoder =
    JD.list <|
        JD.map8 MemberNode
            (JD.field "createdAt" JD.string)
            (JD.field "name" JD.string)
            (JD.field "nameid" JD.string)
            (JD.field "rootnameid" JD.string)
            (JD.field "role_type" RoleType.decoder |> JD.maybe)
            (JD.field "first_link" userDecoder |> JD.maybe)
            (JD.field "parent" nodeIdDecoder |> JD.maybe)
            (JD.field "isPrivate" JD.bool)


membersDecoder2 : WebData (List MemberNode) -> GqlData (List Member)
membersDecoder2 input =
    let
        n2r n =
            UserRoleExtended n.name n.nameid n.rootnameid (n.role_type |> withDefault RoleType.Guest) n.createdAt n.parent n.isPrivate
    in
    case input of
        RemoteData.Success children ->
            let
                toTuples : MemberNode -> List ( String, Member )
                toTuples m =
                    case m.first_link of
                        Just fs ->
                            [ ( fs.username, Member fs.username fs.name [ n2r m ] ) ]

                        Nothing ->
                            []

                toDict : List ( String, Member ) -> Dict String Member
                toDict inputs =
                    List.foldl
                        (\( k, v ) dict -> Dict.update k (addParam v) dict)
                        Dict.empty
                        inputs

                addParam : Member -> Maybe Member -> Maybe Member
                addParam m maybeMember =
                    case maybeMember of
                        Just member ->
                            Just { member | roles = member.roles ++ m.roles }

                        Nothing ->
                            Just m
            in
            List.concatMap toTuples children
                |> toDict
                |> Dict.values
                |> Success

        RemoteData.Loading ->
            Loading

        RemoteData.NotAsked ->
            NotAsked

        RemoteData.Failure err ->
            Failure (toErrorData err)



--
-- User management
--


login url post msg =
    --, Http.post
    --    { url = "http://localhost:8888/login"
    --    , body = Http.jsonBody <| JE.dict identity JE.string form.post
    --    , expect = expectJson (RemoteData.fromResult >> GotSignin) userCtxDecoder
    --    }
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = url ++ "/login"
        , body = Http.jsonBody <| JE.dict identity JE.string post
        , expect = expectJson (RemoteData.fromResult >> msg) userCtxDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


signup url post msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = url ++ "/signup"
        , body = Http.jsonBody <| JE.dict identity JE.string post
        , expect = expectJson (RemoteData.fromResult >> msg) userCtxDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


tokenack url msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = url ++ "/tokenack"
        , body = Http.emptyBody
        , expect = expectJson (RemoteData.fromResult >> msg) userCtxDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



--
-- Organisation management
--


createOrga url post msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = url ++ "/createorga"
        , body = Http.jsonBody <| JE.dict identity JE.string post
        , expect = expectJson (RemoteData.fromResult >> msg) nodeIdDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



--
-- Get Doc
--


getQuickDoc url lang msg =
    Http.riskyRequest
        { method = "GET"
        , headers = []
        , url = url ++ ("/quickdoc." ++ lang ++ ".json")
        , body = Http.emptyBody
        , expect = expectJson (RemoteData.fromResult >> msg) quickDocDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
