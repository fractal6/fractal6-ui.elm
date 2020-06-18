module ModelCommon.Requests exposing (fetchChildren, login, signup)

import Components.Loading as Loading exposing (WebData, expectJson)
import Http
import Json.Decode as JD
import Json.Encode as JE
import ModelCommon exposing (userDecoder)
import ModelSchema exposing (NodeId, Post, UserCtx)
import RemoteData exposing (RemoteData)



{-
   riskyRequest are needed to set cookies on the client through CORS.
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
