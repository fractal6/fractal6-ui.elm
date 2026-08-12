{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2026 Fractale Co

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


module Api.File exposing
    ( Anchor(..)
    , ApiError(..)
    , UploadResult
    , delete
    , errorToString
    , upload
    )

{-| REST transport for /file/\* endpoints.

The handler accepts multipart form fields for the anchor (`tid`/`cid`/`userid`/
`orgaid`) alongside the file part. We always pass the anchor as form fields.

-}

import File exposing (File)
import Http
import Json.Decode as JD
import Json.Decode.Extra as JDE
import Session exposing (Apis)



--
-- Types
--


type Anchor
    = CommentAnchor { tid : String, cid : String }
    | UserAvatar String
    | OrgAvatar String


type alias UploadResult =
    { id : String
    , url : String
    , filename : String
    , contentType : String
    , size : Int
    , embedded : Bool
    }


type ApiError
    = StorageDisabled -- 503
    | TooLarge -- 413
    | Unauthorized -- 401
    | Forbidden -- 403
    | NotFound -- 404
    | BadRequest String -- 400 + body
    | Network -- timeout / connection
    | Other Int String -- status + body


errorToString : ApiError -> String
errorToString err =
    case err of
        StorageDisabled ->
            "File uploads are disabled on this instance."

        TooLarge ->
            "File too large."

        Unauthorized ->
            "You need to sign in to upload files."

        Forbidden ->
            "You don't have permission to upload here."

        NotFound ->
            "Not found."

        BadRequest s ->
            "Bad request: " ++ s

        Network ->
            "Network error during upload."

        Other code body ->
            "Unexpected error (" ++ String.fromInt code ++ "): " ++ body



--
-- Encoding
--


anchorParts : Anchor -> List Http.Part
anchorParts a =
    case a of
        CommentAnchor { tid, cid } ->
            [ Http.stringPart "tid" tid, Http.stringPart "cid" cid ]

        UserAvatar username ->
            [ Http.stringPart "userid" username ]

        OrgAvatar rootnameid ->
            [ Http.stringPart "orgaid" rootnameid ]


uploadResultDecoder : JD.Decoder UploadResult
uploadResultDecoder =
    JD.succeed UploadResult
        |> JDE.andMap (JD.field "id" JD.string)
        |> JDE.andMap (JD.field "url" JD.string)
        |> JDE.andMap (JD.field "filename" JD.string)
        |> JDE.andMap (JD.field "contentType" JD.string)
        |> JDE.andMap (JD.field "size" JD.int)
        |> JDE.andMap
            (JD.maybe (JD.field "embedded" JD.bool)
                |> JD.map (Maybe.withDefault False)
            )



--
-- Public API
--


{-| Upload a file under `anchor`. Backend rewrites `![…](filename)` references
in the carrier message when present.
-}
upload : Apis -> Anchor -> File -> (Result ApiError UploadResult -> msg) -> Cmd msg
upload api anchor file msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = api.file ++ "/upload"
        , body = Http.multipartBody (Http.filePart "file" file :: anchorParts anchor)
        , expect = expectJsonResult msg uploadResultDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


delete : Apis -> String -> (Result ApiError () -> msg) -> Cmd msg
delete api fileId msg =
    Http.riskyRequest
        { method = "DELETE"
        , headers = []
        , url = api.file ++ "/" ++ fileId
        , body = Http.emptyBody
        , expect = expectWhateverResult msg
        , timeout = Nothing
        , tracker = Nothing
        }



--
-- Http response → ApiError mapping
--


expectJsonResult : (Result ApiError a -> msg) -> JD.Decoder a -> Http.Expect msg
expectJsonResult toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Other 0 ("bad url: " ++ url))

                Http.Timeout_ ->
                    Err Network

                Http.NetworkError_ ->
                    Err Network

                Http.GoodStatus_ _ body ->
                    case JD.decodeString decoder body of
                        Ok v ->
                            Ok v

                        Err err ->
                            Err (Other 200 (JD.errorToString err))

                Http.BadStatus_ meta body ->
                    Err (statusToApiError meta.statusCode body)


expectWhateverResult : (Result ApiError () -> msg) -> Http.Expect msg
expectWhateverResult toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Other 0 ("bad url: " ++ url))

                Http.Timeout_ ->
                    Err Network

                Http.NetworkError_ ->
                    Err Network

                Http.GoodStatus_ _ _ ->
                    Ok ()

                Http.BadStatus_ meta body ->
                    -- 404 on DELETE = already gone, treat as success
                    if meta.statusCode == 404 then
                        Ok ()

                    else
                        Err (statusToApiError meta.statusCode body)


statusToApiError : Int -> String -> ApiError
statusToApiError code body =
    case code of
        400 ->
            BadRequest body

        401 ->
            Unauthorized

        403 ->
            Forbidden

        404 ->
            NotFound

        413 ->
            TooLarge

        503 ->
            StorageDisabled

        _ ->
            Other code body
