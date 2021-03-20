module ModelCommon.Requests exposing (..)

import Codecs exposing (emitterOrReceiverDecoder, nodeIdDecoder, quickDocDecoder, userCtxDecoder, userDecoder)
import Components.Loading as Loading exposing (GqlData, RequestResult(..), WebData, expectJson, fromResult, toErrorData)
import Dict exposing (Dict)
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Http
import Json.Decode as JD
import Json.Decode.Extra as JDE
import Json.Encode as JE
import Json.Encode.Extra as JEE
import Maybe exposing (withDefault)
import ModelSchema exposing (Label, LabelFull, Member, NodeId, Post, Tension, User, UserCtx, UserRoleExtended, Username)
import Query.QueryNode exposing (MemberNode)
import RemoteData exposing (RemoteData)



{-
   riskyRequest are needed to set cookies on the client through CORS.
-}


{-|

    Get all children ** Nodes ** below the given node recursively

-}
fetchChildren url targetid msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = url ++ "/sub_children"
        , body = Http.jsonBody <| JE.string targetid
        , expect = expectJson (RemoteData.fromResult >> msg) <| JD.list nodeIdDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-|

    Get all member ** Nodes ** below the given node (role with first link) recursively

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


{-|

    Get all ** Labels ** below the given node recursively

-}
fetchLabels url targetid msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = url ++ "/sub_labels"
        , body = Http.jsonBody <| JE.string targetid
        , expect = expectJson (RemoteData.fromResult >> msg) <| JD.list labelFullDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


labelFullDecoder : JD.Decoder LabelFull
labelFullDecoder =
    JD.map5 LabelFull
        (JD.field "id" JD.string)
        (JD.field "name" JD.string)
        (JD.field "color" JD.string |> JD.maybe)
        (JD.field "description" JD.string |> JD.maybe)
        (JD.field "n_nodes" JD.int |> JD.maybe)


labelDecoder : JD.Decoder Label
labelDecoder =
    JD.map3 Label
        (JD.field "id" JD.string)
        (JD.field "name" JD.string)
        (JD.field "color" JD.string |> JD.maybe)



{-
   Query Int/Ext **Tensions**
-}


type alias TensionQuery =
    { targetids : List String
    , first : Int
    , offset : Int
    , query_ : Maybe String
    , status_ : Maybe TensionStatus.TensionStatus
    , authors : List User
    , labels : List Label
    , type_ : Maybe TensionType.TensionType
    }


fetchTensionInt url targetids first offset query_ status_ authors labels type_ msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = url ++ "/tensions_int"
        , body = Http.jsonBody <| fetchTensionEncoder targetids first offset query_ status_ authors labels type_
        , expect = expectJson (fromResult >> msg) <| JD.list tensionDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchTensionExt url targetids first offset query_ status_ authors labels type_ msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = url ++ "/tensions_ext"
        , body = Http.jsonBody <| fetchTensionEncoder targetids first offset query_ status_ authors labels type_
        , expect = expectJson (fromResult >> msg) <| JD.list tensionDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchTensionEncoder : List String -> Int -> Int -> Maybe String -> Maybe TensionStatus.TensionStatus -> List User -> List Label -> Maybe TensionType.TensionType -> JD.Value
fetchTensionEncoder nameids first offset query_ status_ authors labels type_ =
    JE.object
        [ ( "nameids", JE.list JE.string nameids )
        , ( "first", JE.int first )
        , ( "offset", JE.int offset )
        , ( "query", JEE.maybe JE.string query_ )
        , ( "status", JEE.maybe JE.string <| Maybe.map (\t -> TensionStatus.toString t) status_ )
        , ( "authors", JE.list JE.string <| List.map (\x -> x.username) authors )
        , ( "labels", JE.list JE.string <| List.map (\x -> x.name) labels )
        , ( "type_", JEE.maybe JE.string <| Maybe.map (\t -> TensionType.toString t) type_ )
        ]


tensionDecoder : JD.Decoder Tension
tensionDecoder =
    JD.succeed Tension
        |> JDE.andMap (JD.field "id" JD.string)
        |> JDE.andMap (JD.field "createdAt" JD.string)
        |> JDE.andMap (JD.field "createdBy" (JD.map Username (JD.field "username" JD.string)))
        |> JDE.andMap (JD.field "title" JD.string)
        |> JDE.andMap (JD.field "type_" TensionType.decoder)
        |> JDE.andMap (JD.maybe <| JD.field "labels" (JD.list <| labelDecoder))
        |> JDE.andMap (JD.field "emitter" emitterOrReceiverDecoder)
        |> JDE.andMap (JD.field "receiver" emitterOrReceiverDecoder)
        |> JDE.andMap (JD.maybe <| JD.field "action" TensionAction.decoder)
        |> JDE.andMap (JD.field "status" TensionStatus.decoder)
        |> JDE.andMap (JD.maybe <| JD.field "n_comments" JD.int)



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
