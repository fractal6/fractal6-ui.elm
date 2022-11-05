{-
  Fractale - Self-organisation for humans.
  Copyright (C) 2022 Fractale Co

  This file is part of Fractale.

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
-}

module Requests exposing (..)

import Bytes exposing (Bytes)
import Codecs exposing (emitterOrReceiverDecoder, labelDecoder, nodeIdDecoder, quickDocDecoder, userCtxDecoder, userDecoder)
import Dict exposing (Dict)
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Http exposing (expectWhatever)
import Image exposing (Image)
import Json.Decode as JD
import Json.Decode.Extra as JDE
import Json.Encode as JE
import Json.Encode.Extra as JEE
import Loading exposing (expectJson, fromResult, mapWeb2Data)
import Maybe exposing (withDefault)
import ModelSchema
    exposing
        ( Count
        , Label
        , Member
        , NameidPayload
        , NodeId
        , Post
        , RoleExt
        , Tension
        , TensionsCount
        , User
        , UserRoleExtended
        , Username
        )
import Query.QueryNode exposing (MemberNode, membersNodeDecoder)
import RemoteData exposing (RemoteData)
import Session exposing (Apis)



{- HTTP / Rest requests modules

   riskyRequest are needed to set cookies on the client through CORS.

   Why the data should be requested here instead of different Graphql Request ?
   Because one of the following raison
   - Recursive queries are handle trough DQL requests.
   - either data are not directlty related to the Graph database such as
      * security request (get or reset a password
      * query some docs
-}


setHeaders : Apis -> List Http.Header
setHeaders api =
    -- CORS error in dev mode !?
    --[ Http.header "X-Client-Version" api.version ]
    []


{-|

    Get all children ** Nodes ** below the given node recursively

-}
fetchChildren api targetid msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = api.rest ++ "/sub_nodes"
        , body = Http.jsonBody <| JE.string targetid
        , expect = expectJson (RemoteData.fromResult >> msg) <| JD.list nodeIdDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-|

    Get all member ** Nodes ** below the given node (role with lead link) recursively

-}
fetchMembersSub api targetid msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = api.rest ++ "/sub_members"
        , body = Http.jsonBody <| JE.string targetid
        , expect = expectJson (RemoteData.fromResult >> mapWeb2Data membersNodeDecoder >> msg) membersDecoder
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
            (JD.field "role_type" RoleType.decoder |> JD.maybe)
            (JD.field "color" JD.string |> JD.maybe)
            (JD.field "first_link" userDecoder |> JD.maybe)
            (JD.field "parent" nodeIdDecoder |> JD.maybe)


{-|

    Get all ** Labels ** from the parent, unril the root node

-}
fetchLabelsTop api targetid msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = api.rest ++ "/top_labels"
        , body = Http.jsonBody <| JE.string targetid
        , expect = expectJson (RemoteData.fromResult >> msg) <| JD.list labelDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-|

    Get all ** Labels ** below the given node recursively

-}
fetchLabelsSub api targetid msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = api.rest ++ "/sub_labels"
        , body = Http.jsonBody <| JE.string targetid
        , expect = expectJson (RemoteData.fromResult >> msg) <| JD.list labelDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-|

    Get all ** Roles ** from the parent, unril the root node

-}
fetchRolesTop api targetid msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = api.rest ++ "/top_roles"
        , body = Http.jsonBody <| JE.string targetid
        , expect = expectJson (RemoteData.fromResult >> msg) <| JD.list roleDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-|

    Get all ** Roles ** below the given node recursively

-}
fetchRolesSub api targetid msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = api.rest ++ "/sub_roles"
        , body = Http.jsonBody <| JE.string targetid
        , expect = expectJson (RemoteData.fromResult >> msg) <| JD.list roleDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


roleDecoder : JD.Decoder RoleExt
roleDecoder =
    JD.map5 RoleExt
        (JD.field "id" JD.string)
        (JD.field "name" JD.string)
        (JD.field "color" JD.string |> JD.maybe)
        (JD.field "role_type" RoleType.decoder)
        (JD.field "nodes" (JD.list <| JD.map NameidPayload (JD.field "nameid" JD.string)))



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


fetchTensionInt api targetids first offset query_ status_ authors labels type_ sort_ msg =
    Http.riskyRequest
        { method = "POST"
        , headers = setHeaders api
        , url = api.rest ++ "/tensions_int"
        , body = Http.jsonBody <| fetchTensionEncoder targetids first offset query_ status_ authors labels type_ sort_
        , expect = expectJson (fromResult >> msg) <| JD.list tensionDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchTensionExt api targetids first offset query_ status_ authors labels type_ sort_ msg =
    Http.riskyRequest
        { method = "POST"
        , headers = setHeaders api
        , url = api.rest ++ "/tensions_ext"
        , body = Http.jsonBody <| fetchTensionEncoder targetids first offset query_ status_ authors labels type_ sort_
        , expect = expectJson (fromResult >> msg) <| JD.list tensionDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchTensionAll api targetids first offset query_ status_ authors labels type_ sort_ msg =
    Http.riskyRequest
        { method = "POST"
        , headers = setHeaders api
        , url = api.rest ++ "/tensions_all"
        , body = Http.jsonBody <| fetchTensionEncoder targetids first offset query_ status_ authors labels type_ sort_
        , expect = expectJson (fromResult >> msg) <| JD.list tensionDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchTensionCount api targetids query_ authors labels type_ sort_ msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = api.rest ++ "/tensions_count"
        , body = Http.jsonBody <| fetchTensionEncoder targetids 0 0 query_ Nothing authors labels type_ sort_
        , expect = expectJson (fromResult >> msg) <| JD.map2 TensionsCount (JD.field "open" JD.int) (JD.field "closed" JD.int)
        , timeout = Nothing
        , tracker = Nothing
        }


fetchTensionEncoder : List String -> Int -> Int -> Maybe String -> Maybe TensionStatus.TensionStatus -> List User -> List Label -> Maybe TensionType.TensionType -> Maybe String -> JD.Value
fetchTensionEncoder nameids first offset query_ status_ authors labels type_ sort_ =
    JE.object
        [ ( "nameids", JE.list JE.string nameids )
        , ( "first", JE.int first )
        , ( "offset", JE.int offset )
        , ( "query", JEE.maybe JE.string query_ )
        , ( "sort", JEE.maybe JE.string sort_ )
        , ( "status", JEE.maybe JE.string <| Maybe.map TensionStatus.toString status_ )
        , ( "authors", JE.list JE.string <| List.map .username authors )
        , ( "labels", JE.list JE.string <| List.map .name labels )
        , ( "type_", JEE.maybe JE.string <| Maybe.map TensionType.toString type_ )
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
-- Organisation management
--


createOrga api post msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = api.auth ++ "/createorga"
        , body = Http.jsonBody <| JE.dict identity JE.string post
        , expect = expectJson (RemoteData.fromResult >> msg) nodeIdDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


setUserCanJoin api nameid val msg =
    Http.riskyRequest
        { method = "POST"
        , headers = setHeaders api
        , url = api.auth ++ "/setusercanjoin"
        , body = Http.jsonBody <| JE.object [ ( "nameid", JE.string nameid ), ( "val", JE.bool val ) ]
        , expect = expectJson (RemoteData.fromResult >> msg) JD.bool
        , timeout = Nothing
        , tracker = Nothing
        }


setGuestCanCreateTension api nameid val msg =
    Http.riskyRequest
        { method = "POST"
        , headers = setHeaders api
        , url = api.auth ++ "/setguestcancreatetension"
        , body = Http.jsonBody <| JE.object [ ( "nameid", JE.string nameid ), ( "val", JE.bool val ) ]
        , expect = expectJson (RemoteData.fromResult >> msg) JD.bool
        , timeout = Nothing
        , tracker = Nothing
        }



--
-- User management
--


login api post msg =
    --, Http.post
    --    { url = "http://localhost:8888/login"
    --    , body = Http.jsonBody <| JE.dict identity JE.string form.post
    --    , expect = expectJson (RemoteData.fromResult >> GotSignin) userCtxDecoder
    --    }
    Http.riskyRequest
        { method = "POST"
        , headers = setHeaders api
        , url = api.auth ++ "/login"
        , body = Http.jsonBody <| JE.dict identity JE.string post
        , expect = expectJson (RemoteData.fromResult >> msg) userCtxDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


logout api msg =
    Http.riskyRequest
        { method = "GET"
        , headers = []
        , url = api.auth ++ "/logout"
        , expect = expectWhatever (\_ -> msg)
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


signup api post msg =
    Http.riskyRequest
        { method = "POST"
        , headers = setHeaders api
        , url = api.auth ++ "/signup"
        , body = Http.jsonBody <| JE.dict identity JE.string post
        , expect = expectJson (RemoteData.fromResult >> msg) JD.bool
        , timeout = Nothing
        , tracker = Nothing
        }


signupValidate api post msg =
    Http.riskyRequest
        { method = "POST"
        , headers = setHeaders api
        , url = api.auth ++ "/validate"
        , body = Http.jsonBody <| JE.dict identity JE.string post
        , expect = expectJson (RemoteData.fromResult >> msg) userCtxDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


tokenack api msg =
    Http.riskyRequest
        { method = "POST"
        , headers = setHeaders api
        , url = api.auth ++ "/tokenack"
        , body = Http.emptyBody
        , expect = expectJson (RemoteData.fromResult >> msg) userCtxDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


uuidCheck api post msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = api.auth ++ "/uuidcheck"
        , body = Http.jsonBody <| JE.dict identity JE.string post
        , expect = expectJson (RemoteData.fromResult >> msg) JD.bool
        , timeout = Nothing
        , tracker = Nothing
        }


resetPassword api post msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = api.auth ++ "/resetpassword"
        , body = Http.jsonBody <| JE.dict identity JE.string post
        , expect = expectJson (RemoteData.fromResult >> msg) JD.bool
        , timeout = Nothing
        , tracker = Nothing
        }


resetPassword2 api post msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = api.auth ++ "/resetpassword2"
        , body = Http.jsonBody <| JE.dict identity JE.string post
        , expect = expectJson (RemoteData.fromResult >> msg) userCtxDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


resetPasswordChallenge api msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = api.auth ++ "/resetpasswordchallenge"
        , body = Http.emptyBody

        -- see https://github.com/justgook/elm-image/issues/9
        , expect = Http.expectBytesResponse (RemoteData.fromResult >> msg) httpReponseToImage

        --, expect = Http.expectJson (RemoteData.fromResult >> msg) File.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


httpReponseToImage : Http.Response Bytes -> Result Http.Error (Maybe Image)
httpReponseToImage response =
    case response of
        Http.GoodStatus_ _ body ->
            Ok <| Image.decode body

        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)



--
-- Get Doc
--


getQuickDoc api lang msg =
    Http.request
        { method = "GET"
        , headers = []
        , url = api.doc ++ ("/quickdoc." ++ lang ++ ".json")
        , body = Http.emptyBody
        , expect = expectJson (RemoteData.fromResult >> msg) quickDocDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
