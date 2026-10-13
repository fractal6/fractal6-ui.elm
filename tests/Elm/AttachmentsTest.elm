module Elm.AttachmentsTest exposing (..)

import Api.File as ApiFile
import Components.Attachments exposing (PendingStatus(..), prunePastesByMessage, retarget, storeBatchFailures)
import Dict
import Expect
import Test exposing (..)


{-| Plain stand-in for `PendingFile` (elm-test can't build a `File.File`).
-}
pending : String -> Bool -> String -> PendingStatus -> { filename : String, isPaste : Bool, objectUrl : String, status : PendingStatus }
pending filename isPaste objectUrl status =
    { filename = filename, isPaste = isPaste, objectUrl = objectUrl, status = status }


pruneTests : Test
pruneTests =
    let
        pick =
            pending "doc.pdf" False "" Queued

        paste =
            pending "paste-1-0.png" True "blob:a" Queued
    in
    describe "prunePastesByMessage"
        [ test "paste still linked in the message is kept" <|
            \_ ->
                prunePastesByMessage "hi ![|10x20](paste-1-0.png) " [ pick, paste ]
                    |> Expect.equal ( [ pick, paste ], [] )
        , test "paste whose placeholder was deleted is dropped and its blob revoked" <|
            \_ ->
                prunePastesByMessage "hi" [ pick, paste ]
                    |> Expect.equal ( [ pick ], [ "blob:a" ] )
        , test "dropped paste without blob URL revokes nothing" <|
            \_ ->
                prunePastesByMessage "" [ { paste | objectUrl = "" } ]
                    |> Expect.equal ( [], [] )
        , test "picked files are never pruned (staged count)" <|
            \_ ->
                prunePastesByMessage "" [ pick, { pick | filename = "b.txt" }, paste ]
                    |> Tuple.first
                    |> List.length
                    |> Expect.equal 2
        , test "plain filename mention is not a link" <|
            \_ ->
                prunePastesByMessage "see paste-1-0.png" [ paste ]
                    |> Tuple.first
                    |> Expect.equal []
        ]


batchFailuresTests : Test
batchFailuresTests =
    let
        ok =
            pending "a.png" False "" (Uploaded "id-a")

        ko =
            pending "b.png" False "" (UploadFailed ApiFile.TooLarge)

        queued =
            pending "c.png" False "" Queued
    in
    describe "storeBatchFailures"
        [ test "no failure collected while the batch is still draining" <|
            \_ ->
                storeBatchFailures "c1" { done = 2, files = [ ok, ko, queued ] } Dict.empty
                    |> Expect.equal ( [], Dict.empty )
        , test "failures of a drained batch move to the failed store of its cid" <|
            \_ ->
                storeBatchFailures "c1" { done = 3, files = [ ok, ko, { queued | status = UploadFailed ApiFile.Network } ] } Dict.empty
                    |> Expect.equal
                        ( [ ko, { queued | status = UploadFailed ApiFile.Network } ]
                        , Dict.fromList [ ( "c1", [ ko, { queued | status = UploadFailed ApiFile.Network } ] ) ]
                        )
        , test "failures append to earlier ones of the same cid" <|
            \_ ->
                storeBatchFailures "c1" { done = 1, files = [ ko ] } (Dict.fromList [ ( "c1", [ { ko | filename = "old.png" } ] ) ])
                    |> Tuple.second
                    |> Expect.equal (Dict.fromList [ ( "c1", [ { ko | filename = "old.png" }, ko ] ) ])
        , test "a drained batch without failure leaves the store untouched" <|
            \_ ->
                storeBatchFailures "c1" { done = 1, files = [ ok ] } (Dict.fromList [ ( "c2", [ ko ] ) ])
                    |> Expect.equal ( [], Dict.fromList [ ( "c2", [ ko ] ) ] )
        ]


retargetTests : Test
retargetTests =
    describe "retarget"
        [ test "rewrites every link to the placeholder" <|
            \_ ->
                retarget "paste-1-0.png" "/file/xyz" "![|10x20](paste-1-0.png) and [f](paste-1-0.png)"
                    |> Expect.equal "![|10x20](/file/xyz) and [f](/file/xyz)"
        , test "leaves bare mentions and other links alone" <|
            \_ ->
                retarget "a.png" "/file/x" "a.png ![](ba.png)"
                    |> Expect.equal "a.png ![](ba.png)"
        ]
