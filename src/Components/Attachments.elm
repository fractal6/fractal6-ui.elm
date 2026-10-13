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


module Components.Attachments exposing
    ( Ack
    , Msg
    , State
    , UploadProgress
    , handoff
    , hasActiveUploads
    , init
    , retarget
    , setPasteTargets
    , stagedCount
    , subscriptions
    , update
    , uploadProgress
    , uploadedIds
    , viewCommentChips
    , viewPendingsRow
    , withBlobPreviews
    )

import Api.File as ApiFile
import Assets as A
import Dict
import File
import File.Select
import Html exposing (Html, button, div, li, progress, span, text, ul)
import Html.Attributes exposing (attribute, class, disabled, title, type_, value)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import List.Extra as LE
import Maybe exposing (withDefault)
import Ports
import Session exposing (Apis, GlobalCmd(..))
import Text as T
import Utils.Bool exposing (ternary)
import Utils.Html exposing (showIf)



-- ------------------------------
-- M O D E L
-- ------------------------------


type State
    = State Model


type alias Model =
    -- pendingByEditor: files queued in an editor before its carrier (cid) exists.
    -- activeByCid:     files being uploaded to a saved comment (FIFO drain, one in flight).
    -- Paste filenames (`paste-<Date.now()>-<i><ext>`) are generated in
    -- ports.js so the File object's own .name matches the multipart upload
    -- and the markdown placeholder.
    { pendingByEditor : Dict.Dict String (List PendingFile)
    , activeByCid : Dict.Dict String UploadBatch

    -- failedByCid: failed uploads of a drained batch, kept under the comment until dismissed.
    , failedByCid : Dict.Dict String (List PendingFile)

    -- pasteTargets: editor ids this instance renders. The paste port is
    -- broadcast to every mounted Comments, so foreign pastes must be ignored.
    , pasteTargets : List String
    }


type alias PendingFile =
    { filename : String
    , file : File.File
    , isPaste : Bool -- true if inserted as inline `![](filename)` placeholder
    , status : PendingStatus
    , objectUrl : String -- blob: URL for in-place preview; "" for non-paste picks
    }


type PendingStatus
    = Queued
    | Uploading
    | Uploaded String -- saved file id
    | UploadFailed ApiFile.ApiError


type alias UploadBatch =
    { tid : String
    , editorId : String
    , files : List PendingFile -- retained until the entire batch finishes
    , done : Int -- acknowledged files; also the index of the file in flight
    , fraction : Float -- upload progress of the file in flight
    }


type alias UploadProgress =
    { done : Int, total : Int, fraction : Float }


type alias PastedFiles =
    { targetId : String
    , files : List File.File
    , objectUrls : List String
    , dims : List String -- natural "WxH" of pasted images, "" when unknown
    , isPaste : Bool -- False for dropped files, staged as plain attachments
    }


init : List String -> State
init pasteTargets =
    State
        { pendingByEditor = Dict.empty
        , activeByCid = Dict.empty
        , failedByCid = Dict.empty
        , pasteTargets = pasteTargets
        }



-- Global methods


{-| True while any batch is still draining. Owners that reset this state on
close (e.g. the card panel) must hold their reset until it is False.
-}
hasActiveUploads : State -> Bool
hasActiveUploads (State model) =
    not (Dict.isEmpty model.activeByCid)


{-| Live upload state of the batch handed off by `editorId`, if any. Callers
owning the carrier mutation (e.g. the new tension modal) use it to hold their
close until the files have landed.
-}
uploadProgress : String -> State -> Maybe UploadProgress
uploadProgress editorId (State model) =
    model.activeByCid
        |> Dict.values
        |> LE.find (\b -> b.editorId == editorId)
        |> Maybe.map (\b -> { done = b.done, total = List.length b.files, fraction = b.fraction })


{-| Declare the editors this instance owns (see `pasteTargets`).
-}
setPasteTargets : List String -> State -> State
setPasteTargets targets (State model) =
    State { model | pasteTargets = targets }


{-| How many files staged in `editorId` will really be uploaded (pastes whose
placeholder was deleted from `message` before submit don't count).
-}
stagedCount : String -> String -> State -> Int
stagedCount editorId message (State model) =
    Dict.get editorId model.pendingByEditor
        |> withDefault []
        |> prunePastesByMessage message
        |> Tuple.first
        |> List.length


{-| Prune the pastes deleted from the carrier `message`, then move the files staged in
`editorId` to the saved comment `cid` and start the upload drain. Without a cid
(no comment was created) the files only get pruned.
-}
handoff : Apis -> { editorId : String, message : String, tid : String, cid : Maybe String } -> State -> ( State, Cmd Msg )
handoff apis { editorId, message, tid, cid } (State model) =
    let
        ( pendingByEditor0, revokeCmds ) =
            prunePastesForEditor message editorId model.pendingByEditor

        ( pendingByEditor1, activeByCid1, cmd ) =
            case cid of
                Just cid_ ->
                    handoffPendingToCid apis editorId tid cid_ pendingByEditor0 model.activeByCid

                Nothing ->
                    ( pendingByEditor0, model.activeByCid, Cmd.none )
    in
    ( State { model | pendingByEditor = pendingByEditor1, activeByCid = activeByCid1 }
    , Cmd.batch (cmd :: revokeCmds)
    )


{-| Ids of the files of the active batch of `cid` already uploaded (rendered as chips
until the batch completes).
-}
uploadedIds : String -> State -> List String
uploadedIds cid (State model) =
    Dict.get cid model.activeByCid
        |> Maybe.map .files
        |> withDefault []
        |> List.filterMap
            (\p ->
                case p.status of
                    Uploaded fileId ->
                        Just fileId

                    _ ->
                        Nothing
            )



-- Placeholders


{-| `![|WxH](name)`; the `|WxH` alt suffix is parsed back by `Markdown.splitImageDims`.
-}
imagePlaceholder : String -> String -> String
imagePlaceholder dims name =
    "![" ++ ternary (dims == "") "" ("|" ++ dims) ++ "](" ++ name ++ ")"


linksTo : String -> String -> Bool
linksTo target message =
    String.contains ("](" ++ target ++ ")") message


{-| Point the markdown links targeting `from` to `to`.
-}
retarget : String -> String -> String -> String
retarget from to message =
    String.replace ("](" ++ from ++ ")") ("](" ++ to ++ ")") message


{-| Resolve `![](paste-…)` placeholders to local blob URLs so the
preview can show pasted images that haven't been uploaded yet.
Already-saved `![](/file/<id>)` is left untouched — the renderer
prefixes those with the file server URL itself.
-}
withBlobPreviews : String -> State -> String -> String
withBlobPreviews targetId (State model) message =
    Dict.get targetId model.pendingByEditor
        |> withDefault []
        |> List.filter (\p -> p.isPaste && p.objectUrl /= "")
        |> List.foldl (\p acc -> retarget p.filename p.objectUrl acc) message



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = OnPickFiles String -- targetId
    | OnFilesSelected String File.File (List File.File)
    | OnPastedFiles PastedFiles
    | OnRemovePending String String -- targetId, filename
    | OnUploadAck String String (Result ApiFile.ApiError ApiFile.UploadResult) -- cid, filename, result
    | OnUploadProgress String Http.Progress -- cid, progress
    | OnDismissFailed String String -- cid, filename
    | LogErr String


{-| An upload of `cid` acknowledged: `saved` on success, `batchDone` once the batch drained.
-}
type alias Ack =
    { cid : String
    , filename : String
    , saved : Maybe ApiFile.UploadResult
    , batchDone : Bool
    }


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe Ack
    }


noOut : Out
noOut =
    Out [] [] Nothing


out0 : List (Cmd Msg) -> Out
out0 cmds =
    Out cmds [] Nothing


out1 : List GlobalCmd -> Out
out1 cmds =
    Out [] cmds Nothing


out2 : List (Cmd Msg) -> List GlobalCmd -> Out
out2 cmds gcmds =
    Out cmds gcmds Nothing


update : Apis -> Msg -> State -> ( State, Out )
update apis message (State model) =
    update_ apis message model
        |> Tuple.mapFirst State


update_ : Apis -> Msg -> Model -> ( Model, Out )
update_ apis message model =
    case message of
        OnPickFiles targetId ->
            ( model
            , out0 [ File.Select.files [] (OnFilesSelected targetId) ]
            )

        OnFilesSelected targetId f rest ->
            let
                ( files, tooLarge ) =
                    List.partition (\fi -> File.size fi <= maxUploadBytes) (f :: rest)

                pendings =
                    List.map
                        (\fi ->
                            { filename = File.name fi
                            , file = fi
                            , isPaste = False
                            , status = Queued
                            , objectUrl = ""
                            }
                        )
                        files

                pendingByEditor1 =
                    Dict.update targetId (Maybe.withDefault [] >> (\xs -> xs ++ pendings) >> Just) model.pendingByEditor
            in
            ( { model | pendingByEditor = pendingByEditor1 }, out1 (tooLargeNotif tooLarge) )

        OnPastedFiles { targetId, files, objectUrls, dims, isPaste } ->
            if not (List.member targetId model.pasteTargets) then
                -- Paste meant for another mounted Comments instance.
                ( model, noOut )

            else if List.isEmpty files then
                -- The clipboard advertised files the browser could not read.
                ( model, out1 [ errorNotif T.clipboardNotAttachable [] ] )

            else
                let
                    -- ports.js sets each File's name to `paste-<Date.now()>-<i><ext>`
                    -- and ships a parallel blob URL and dim. We just read them off.
                    ( triples, tooLarge ) =
                        List.map3 (\fi url d -> ( fi, url, d ))
                            files
                            (objectUrls ++ List.repeat (List.length files) "")
                            (dims ++ List.repeat (List.length files) "")
                            |> List.partition (\( fi, _, _ ) -> File.size fi <= maxUploadBytes)

                    revokeCmds =
                        tooLarge
                            |> List.filter (\( _, url, _ ) -> url /= "")
                            |> List.map (\( _, url, _ ) -> Ports.revokeObjectUrl url)

                    newPendings =
                        triples
                            |> List.map
                                (\( fi, url, _ ) ->
                                    { filename = File.name fi
                                    , file = fi
                                    , isPaste = isPaste
                                    , status = Queued
                                    , objectUrl = url
                                    }
                                )

                    pendingByEditor1 =
                        Dict.update targetId (Maybe.withDefault [] >> (\xs -> xs ++ newPendings) >> Just) model.pendingByEditor

                    -- `![|WxH](name)` lets the renderer reserve the image box before
                    -- download; the dim is empty for non-images and failed decodes.
                    insertCmds =
                        if isPaste then
                            List.map2
                                (\p d -> Ports.insertAtCaret targetId (imagePlaceholder d p.filename ++ " "))
                                newPendings
                                (List.map (\( _, _, d ) -> d) triples)

                        else
                            []
                in
                ( { model | pendingByEditor = pendingByEditor1 }
                , out2 (insertCmds ++ revokeCmds) (tooLargeNotif (List.map (\( fi, _, _ ) -> fi) tooLarge))
                )

        OnRemovePending targetId filename ->
            let
                revokeCmd =
                    Dict.get targetId model.pendingByEditor
                        |> Maybe.withDefault []
                        |> List.filter (\p -> p.filename == filename && p.objectUrl /= "")
                        |> List.map (\p -> Ports.revokeObjectUrl p.objectUrl)

                pendingByEditor1 =
                    Dict.update targetId
                        (Maybe.map (List.filter (\p -> p.filename /= filename)))
                        model.pendingByEditor
            in
            ( { model | pendingByEditor = pendingByEditor1 }, out0 revokeCmd )

        OnUploadAck cid filename result ->
            case Dict.get cid model.activeByCid of
                Nothing ->
                    ( model, noOut )

                Just batch ->
                    let
                        revokeCmd =
                            LE.getAt batch.done batch.files
                                |> Maybe.map .objectUrl
                                |> Maybe.withDefault ""
                                |> (\u ->
                                        if u /= "" then
                                            [ Ports.revokeObjectUrl u ]

                                        else
                                            []
                                   )

                        status =
                            case result of
                                Ok up ->
                                    Uploaded up.id

                                Err err ->
                                    UploadFailed err

                        batch1 =
                            { batch
                                | files = LE.updateAt batch.done (\p -> { p | status = status }) batch.files
                                , done = batch.done + 1
                                , fraction = 0
                            }

                        batchDone =
                            batch1.done == List.length batch1.files

                        -- Once the batch drained, its failures outlive it: kept under the
                        -- comment and notified, as carriers (new tension, card panel) close.
                        failed =
                            ternary batchDone (List.filter isUploadFailed batch1.files) []

                        failedByCid1 =
                            ternary (List.isEmpty failed)
                                model.failedByCid
                                (Dict.update cid (Maybe.withDefault [] >> (\xs -> xs ++ failed) >> Just) model.failedByCid)

                        failedNotif =
                            ternary (List.isEmpty failed)
                                []
                                [ errorNotif T.attachmentsFailed
                                    (List.filterMap
                                        (\p ->
                                            case p.status of
                                                UploadFailed err ->
                                                    Just (p.filename ++ ": " ++ ApiFile.errorToString err)

                                                _ ->
                                                    Nothing
                                        )
                                        failed
                                    )
                                ]

                        ( nextCmd, activeByCid1 ) =
                            drainNext apis cid batch1 model.activeByCid

                        ack =
                            Just { cid = cid, filename = filename, saved = Result.toMaybe result, batchDone = batchDone }
                    in
                    case result of
                        Ok _ ->
                            ( { model | activeByCid = activeByCid1, failedByCid = failedByCid1 }
                            , Out (nextCmd :: revokeCmd) failedNotif ack
                            )

                        Err err ->
                            -- Keep the failed chip visible while continuing the batch.
                            ( { model | activeByCid = activeByCid1, failedByCid = failedByCid1 }
                            , Out (nextCmd :: Ports.logErr ("file upload failed for " ++ filename ++ ": " ++ ApiFile.errorToString err) :: revokeCmd) failedNotif ack
                            )

        OnUploadProgress cid progress ->
            case progress of
                Http.Sending p ->
                    ( { model | activeByCid = Dict.update cid (Maybe.map (\b -> { b | fraction = Http.fractionSent p })) model.activeByCid }, noOut )

                Http.Receiving _ ->
                    ( model, noOut )

        OnDismissFailed cid filename ->
            ( { model | failedByCid = Dict.update cid (Maybe.map (List.filter (\p -> p.filename /= filename))) model.failedByCid }, noOut )

        LogErr err ->
            ( model, out0 [ Ports.logErr err ] )


pastedFilesDecoder : JD.Decoder PastedFiles
pastedFilesDecoder =
    JD.map5 PastedFiles
        (JD.field "targetId" JD.string)
        (JD.field "files" (JD.list File.decoder))
        (JD.field "objectUrls" (JD.list JD.string))
        (JD.field "dims" (JD.list JD.string))
        (JD.field "isPaste" JD.bool)



-- Drop paste pendings whose `![](filename)` is no longer in the carrier
-- message — that's how the user cancels an inline-pasted image (delete
-- the markdown line). Returns the kept pendings and the blob URLs to revoke.


prunePastesByMessage : String -> List PendingFile -> ( List PendingFile, List String )
prunePastesByMessage message pendings =
    let
        ( keep, drop ) =
            List.partition (\p -> not p.isPaste || linksTo p.filename message) pendings

        urls =
            drop
                |> List.filterMap
                    (\p ->
                        if p.objectUrl /= "" then
                            Just p.objectUrl

                        else
                            Nothing
                    )
    in
    ( keep, urls )


{-| Apply prunePastesByMessage to one editor against its carrier message.
Returns the updated dict and a list of revoke commands.
-}
prunePastesForEditor : String -> String -> Dict.Dict String (List PendingFile) -> ( Dict.Dict String (List PendingFile), List (Cmd msg) )
prunePastesForEditor message editorId pendingByEditor =
    case Dict.get editorId pendingByEditor of
        Just xs ->
            let
                ( keep, urls ) =
                    prunePastesByMessage message xs
            in
            ( Dict.insert editorId keep pendingByEditor
            , List.map Ports.revokeObjectUrl urls
            )

        Nothing ->
            ( pendingByEditor, [] )



-- Upload-queue helpers


{-| Move pending files from an editor id to a comment cid and start uploading.
-}
handoffPendingToCid : Apis -> String -> String -> String -> Dict.Dict String (List PendingFile) -> Dict.Dict String UploadBatch -> ( Dict.Dict String (List PendingFile), Dict.Dict String UploadBatch, Cmd Msg )
handoffPendingToCid apis editorId tid cid pendingByEditor activeByCid =
    case Dict.get editorId pendingByEditor of
        Nothing ->
            ( pendingByEditor, activeByCid, Cmd.none )

        Just [] ->
            ( pendingByEditor, activeByCid, Cmd.none )

        Just collected ->
            let
                pendingByEditor1 =
                    Dict.remove editorId pendingByEditor

                batch =
                    { tid = tid, editorId = editorId, files = collected, done = 0, fraction = 0 }

                ( cmd, activeByCid1 ) =
                    drainNext apis cid batch activeByCid
            in
            ( pendingByEditor1, activeByCid1, cmd )


{-| Upload the next file serially, retaining every chip until the batch completes.
-}
drainNext : Apis -> String -> UploadBatch -> Dict.Dict String UploadBatch -> ( Cmd Msg, Dict.Dict String UploadBatch )
drainNext apis cid batch activeByCid =
    case LE.getAt batch.done batch.files of
        Nothing ->
            ( Cmd.none, Dict.remove cid activeByCid )

        Just file ->
            ( ApiFile.upload apis (uploadTracker cid) (ApiFile.CommentAnchor { tid = batch.tid, cid = cid }) file.file (OnUploadAck cid file.filename)
            , Dict.insert cid { batch | files = LE.updateAt batch.done (\p -> { p | status = Uploading }) batch.files } activeByCid
            )


uploadTracker : String -> String
uploadTracker cid =
    "upload-" ++ cid


{-| ponytail: hardcoded client cap, can drift from the backend `storage.max_upload_bytes`;
expose that setting to the client if they must match.
-}
maxUploadBytes : Int
maxUploadBytes =
    100 * 1024 * 1024


isUploadFailed : PendingFile -> Bool
isUploadFailed p =
    case p.status of
        UploadFailed _ ->
            True

        _ ->
            False


errorNotif : String -> List String -> GlobalCmd
errorNotif header lines =
    DoPushSystemNotif
        { cls = "is-danger"
        , content = div [] [ text header, showIf (lines /= []) (ul [] (List.map (\l -> li [] [ text l ]) lines)) ]
        }


tooLargeNotif : List File.File -> List GlobalCmd
tooLargeNotif files =
    ternary (List.isEmpty files) [] [ errorNotif T.filesTooLarge (List.map File.name files) ]


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    Ports.pd Ports.pastedFilesFromJs pastedFilesDecoder LogErr OnPastedFiles
        :: (Dict.keys model.activeByCid |> List.map (\cid -> Http.track (uploadTracker cid) (OnUploadProgress cid)))



-- ------------------------------
-- V I E W
-- ------------------------------


{-| Staged files stay in the editor; with `showActive` (modal editors), the uploads
handed off by `targetId` remain here too.
-}
viewPendingsRow : Bool -> String -> State -> Html Msg
viewPendingsRow showActive targetId (State model) =
    let
        batches =
            Dict.values model.activeByCid |> List.filter (\batch -> batch.editorId == targetId)

        pendings =
            Dict.get targetId model.pendingByEditor |> withDefault []
    in
    div [ class "pending-files is-flex is-flex-wrap-wrap is-align-items-center" ]
        ([ button
            [ class "button is-small is-tiny py-1"
            , type_ "button"
            , disabled (not (List.isEmpty batches))
            , title (ternary (List.isEmpty batches) "" T.uploadInProgress)
            , onClick (OnPickFiles targetId)
            ]
            [ A.icon1 "icon-paperclip icon-xs" T.attach ]
         ]
            ++ (if showActive then
                    List.concatMap (\batch -> List.map (viewPendingChip Nothing batch.fraction) batch.files) batches

                else
                    []
               )
            -- Staged pastes are removed through their markdown placeholder, not a chip.
            ++ List.map (\p -> viewPendingChip (Just (OnRemovePending targetId p.filename)) 0 p) (List.filter (not << .isPaste) pendings)
        )


{-| Active batch and failed uploads of comment `cid`, [] when none.
-}
viewCommentChips : String -> State -> List (Html Msg)
viewCommentChips cid (State model) =
    let
        uploadBatch =
            Dict.get cid model.activeByCid

        failed =
            Dict.get cid model.failedByCid |> withDefault []
    in
    if uploadBatch == Nothing && List.isEmpty failed then
        []

    else
        [ case uploadBatch of
            Just batch ->
                div [ class "pending-files is-flex is-flex-wrap-wrap is-align-items-center" ]
                    (List.map (viewPendingChip Nothing batch.fraction) batch.files)

            Nothing ->
                text ""
        , showIf (failed /= []) <|
            div [ class "pending-files is-flex is-flex-wrap-wrap is-align-items-center" ]
                (List.map (\p -> viewPendingChip (Just (OnDismissFailed cid p.filename)) 0 p) failed)
        ]


viewPendingChip : Maybe Msg -> Float -> PendingFile -> Html Msg
viewPendingChip onRemove fraction p =
    let
        ( statusEl, progressFraction ) =
            case p.status of
                Queued ->
                    ( A.icon "icon-file", Nothing )

                Uploading ->
                    ( span [ class "loader is-inline-block ml-1" ] [], Just (clamp 0 1 fraction) )

                Uploaded _ ->
                    ( A.icon "icon-check has-text-success", Just 1 )

                UploadFailed _ ->
                    ( A.icon "icon-alert-triangle has-text-danger", Nothing )
    in
    span
        [ class "tag attachment-chip"
        , title <|
            case p.status of
                UploadFailed err ->
                    p.filename ++ ": " ++ ApiFile.errorToString err

                _ ->
                    p.filename
        ]
        [ case progressFraction of
            Just amount ->
                progress
                    [ class "progress is-success mb-0"
                    , value (String.fromFloat amount)
                    , attribute "max" "1"
                    , attribute "aria-label" p.filename
                    ]
                    []

            Nothing ->
                text ""
        , statusEl
        , span [ class "attachment-name ml-1" ] [ text p.filename ]
        , case onRemove of
            Just msg ->
                button
                    [ class "delete is-small ml-2"
                    , type_ "button"
                    , title T.remove
                    , attribute "aria-label" (T.remove ++ " " ++ p.filename)
                    , onClick msg
                    ]
                    []

            Nothing ->
                text ""
        ]
