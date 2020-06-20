module ModelCommon.Requests exposing (fetchChildren, fetchMembers, login, signup)

import Components.Loading as Loading exposing (WebData, expectJson, toErrorData)
import Dict exposing (Dict)
import Fractal.Enum.RoleType as RoleType
import Http
import Json.Decode as JD
import Json.Encode as JE
import Maybe exposing (withDefault)
import ModelCommon exposing (userDecoder)
import ModelSchema exposing (GqlData, Member, NodeId, Post, RequestResult(..), UserCtx, UserRoleExtended)
import Query.QueryNodes exposing (MemberNode, User)
import RemoteData exposing (RemoteData)



{-
   riskyRequest are needed to set cookies on the client through CORS.
-}


{-|

    Get all children recursively

-}
fetchChildren targetid msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = "http://localhost:8888/q/sub_children"
        , body = Http.jsonBody <| JE.string targetid
        , expect = expectJson (RemoteData.fromResult >> msg) childrenDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


childrenDecoder : JD.Decoder (List NodeId)
childrenDecoder =
    JD.list <|
        JD.map NodeId (JD.field "nameid" JD.string)


{-|

    Get all member node (role with first link) recursively

-}
fetchMembers targetid msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = "http://localhost:8888/q/sub_members"
        , body = Http.jsonBody <| JE.string targetid
        , expect = expectJson (RemoteData.fromResult >> membersDecoder2 >> msg) membersDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


membersDecoder : JD.Decoder (List MemberNode)
membersDecoder =
    JD.list <|
        JD.map7 MemberNode
            (JD.field "createdAt" JD.string)
            (JD.field "name" JD.string)
            (JD.field "nameid" JD.string)
            (JD.field "rootnameid" JD.string)
            (JD.field "role_type" RoleType.decoder |> JD.maybe)
            (JD.field "first_link"
                (JD.map2 User
                    (JD.field "username" JD.string)
                    (JD.field "name" JD.string |> JD.maybe)
                )
                |> JD.maybe
            )
            (JD.field "parent"
                (JD.map NodeId (JD.field "nameid" JD.string))
                |> JD.maybe
            )


membersDecoder2 : WebData (List MemberNode) -> GqlData (List Member)
membersDecoder2 input =
    let
        n2r n =
            UserRoleExtended n.name n.nameid n.rootnameid (n.role_type |> withDefault RoleType.Guest) n.createdAt n.parent
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


login post msg =
    --, Http.post
    --    { url = "http://localhost:8888/login"
    --    , body = Http.jsonBody <| JE.dict identity JE.string form.post
    --    , expect = expectJson (RemoteData.fromResult >> GotSignin) userDecoder
    --    }
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = "http://localhost:8888/login"
        , body = Http.jsonBody <| JE.dict identity JE.string post
        , expect = expectJson (RemoteData.fromResult >> msg) userDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


signup post msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = "http://localhost:8888/signup"
        , body = Http.jsonBody <| JE.dict identity JE.string post
        , expect = expectJson (RemoteData.fromResult >> msg) userDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
