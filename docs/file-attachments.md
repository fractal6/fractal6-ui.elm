# File Attachments

Attach files to comments and paste screenshots inline. Frontend logic lives in the
`src/Components/Attachments.elm` sub-component, embedded by `src/Components/Comments.elm`;
transport in `src/Api/File.elm`, backend on the `/file/*` REST routes (`Apis.file`).

## Attachments sub-component

`Attachments.State` holds the files staged per editor, the active upload batch per cid, the
failed uploads per cid and the `pasteTargets`. It owns the attachment Msgs (pick, paste/drop,
remove, upload ack/progress, dismiss), the paste port and `Http.track` subscriptions, the chip
views, the blob preview and the placeholder helpers (`retarget`).

`Comments` embeds it like its other sub-components (`AttachmentsMsg`) and keeps:

- thin public wrappers (`kickoffUploads`, `stagedCount`, `uploadProgress`, `hasActiveUploads`,
  `setPasteTargets`), so pages never import `Attachments`;
- the editor ids (`threadEditors`, `modalEditors`, `"commentInput"`, …);
- the local comment patch on upload ack (`Out.result : Maybe Ack`): on `saved` it rewrites the
  message with `Attachments.retarget` and appends to `comment.files`; on `batchDone` it emits
  its own `OutType.UploadsDone`;
- saved-file deletion (`OnDeleteAttachment`).

## URL resolution

Files are stored as relative `/file/<id>` paths, in both the comment markdown and the
`Comment.files` edge. The origin (`FILE_SERVER_URL`, injected at build time) is prepended at
render time by `frac6Renderer`'s `image` override. Pass `""` as `renderMarkdown`'s first
argument for content that can't hold attachments.

## Transport — `src/Api/File.elm`

Stand-alone REST module: `/file/*` is not in the GraphQL schema (only the read-only
`Comment.files`, `User.avatar`, `Node.avatar` edges are).

- `upload` — multipart `POST {api.file}/upload`, anchor fields alongside `file`.
- `delete` — `DELETE {api.file}/<id>`; 404 counts as success.
- No GET: render `/file/<id>` and let the browser follow the redirect to the presigned URL.

`Anchor` is `CommentAnchor { tid, cid }` / `UserAvatar` / `OrgAvatar`; errors map to an
`ApiError` union (`StorageDisabled`, `TooLarge`, `Unauthorized`, …) with `errorToString`.

## Upload ordering

An upload anchors on a saved `cid`, so the carrier mutation must land first:

| Flow | Order |
|------|-------|
| New comment on existing tension | `pushTensionPatch` → returned cid → upload |
| Edit comment | `patchComment` → reuse cid → upload |
| New tension | page runs `addOneTension`, then calls `Comments.kickoffUploads` with `{tid, cid}` |

The first two carriers live in `Comments.elm` so the handoff is automatic; a page owning its
own carrier calls `kickoffUploads` itself. All of them go through `Attachments.handoff`: prune
the pastes deleted from the message, move the staged files to the cid, start the drain.

The carrier also declares **how many** files are coming (never which): `stagedCount` posts
`nfiles`, mapped to `Comment.expected_attachments` by `Query.AddTension.expectedAttachments`.
The backend holds the notification email until that many files land.

Uploads drain **serially per cid** (`Attachments.drainNext`): the backend rewrites the `![…](filename)`
placeholders in `Comment.message` on every upload, a read-modify-write that concurrent
uploads would lose.

## Progress and close gate

Each upload is tracked as `upload-<cid>` (`Http.track`); `uploadProgress editorId` exposes
`{done, total, fraction}`. Chips render in selection order, progress filling behind the
filename, and are replaced by saved links on the final ack.

Closing a carrier resets the `Comments.State` and would drop the queue, so the close is held
until `OutType.UploadsDone`: `NewTension` on its success branch, `CardPanel` by hiding the
panel and deferring `resetModel` (`Comments.hasActiveUploads`). A manual close still drops what
is left in the queue, on purpose.

## Inline paste and drop

Textareas carry `data-paste-capture="true"`; `initFileCapture` (`assets/js/fileCapture.js`) attaches
document-level `paste` / `dragover` / `dragleave` / `drop` listeners that forward files to Elm
with the textarea id, through the same `pastedFilesFromJs` port. `dragover` is prevented (so
`drop` fires), sets `dropEffect = 'copy'` and toggles `.is-dragover` on the textarea
(`assets/sass/components/_comments.scss`) — only for drags carrying files, so text/link drags
keep their native behaviour.

The payload's `isPaste` splits the two flows:

- **image paste** (`isPaste: true`) — files renamed, blob preview, `![|WxH](name)` inserted at
  the caret.
- **drop, or non-image paste** (`isPaste: false`) — original filename (renamed only if the
  clipboard gave none), no placeholder: staged as a plain attachment chip, exactly like the
  Attach button (`OnFilesSelected`). A pasted video or PDF must not become an `<img>`.

Clipboard files are read from `items`, falling back to `files`. When the clipboard advertises
`Files` but none is readable (e.g. a file copied from the Windows explorer), an empty `files`
list is sent so Elm can report it.

Two constraints on the paste step:

- **It owns the filename.** It rebuilds each clipboard `File` as `paste-<timestamp>-<i><ext>`.
  The name must match between the markdown placeholder and the multipart part, because the
  backend rewrite (`web/handlers/files.go::rewriteMessageForFile`) looks the placeholder up by
  multipart name, and only rewrites the first occurrence of each name.
- **The port is a broadcast.** Every mounted `Comments` instance receives it, so each only
  accepts the editors it renders — `pasteTargets` in `Attachments.State`, defaulting to `threadEditors`,
  overridden with `setPasteTargets Comments.modalEditors` by modal components. Without it the
  same paste is inserted twice.

Then, in Elm:

1. `OnPastedFiles` stores `{file, objectUrl, …}` in `pendingByEditor[targetId]` and inserts
   `![|WxH](<filename>)` at the caret through `Ports.insertAtCaret` (undo-preserving).

   `|WxH` (natural size, decoded from the blob by `imageDims`, shipped as the port's `dims`)
   rides in the alt text so it survives the backend rewrite, which only touches the URL;
   `Markdown.splitImageDims` turns it back into `width`/`height` so the browser reserves the box before
   the image downloads. Empty for non-images and failed decodes: the `.markdown-body img`
   background placeholder (`assets/sass/markdown.scss`) covers those.
2. On carrier success the queue drains.
3. Each `OnUploadAck` yields an `Ack`; `Comments` rewrites the local message to `/file/<id>`
   and appends the file to `comment.files`, mirroring what the server stored.

Blob URLs are revoked through `REVOKE_OBJECT_URL` on ack, manual removal, and pruning.

## Errors

Every failure surfaces as a danger system notification (`DoPushSystemNotif`, via `gcmds`), so
it is seen even when the carrier (new tension modal, card panel) has already closed:

- **Too large** — files over `ApiFile.maxUploadBytes` (hardcoded 100 MB, may drift from the
  backend `storage.max_upload_bytes`) are rejected at staging, before any request, and notified
  as `ApiFile.TooLarge`.
- **Unreadable clipboard** — empty paste payload → `T.clipboardNotAttachable`.
- **Upload failed** — when a batch drains, its failed files are notified with
  `ApiFile.errorToString` and kept per cid (`storeBatchFailures`): shown under the comment as
  warning chips (error in the tooltip) until dismissed (`OnDismissFailed`).

Both file errors share one notification format (`T.attachmentsFailed` + `name: error` lines).

A failed paste leaves its `![](paste-…)` placeholder in the saved message (not handled yet).

## Views

- `Attachments.viewPendingsRow` — editor row: Attach button, one chip per pending file; with
  `showActive` (modal editors) also the in-flight uploads and their progress.
- `Attachments.viewCommentChips` — active batch and failed chips under a comment.
- `Comments.viewSavedAttachments` — `comment.files` with `embedded == False` under a rendered
  comment, plus delete for the owner.
- Staged pastes live as markdown in the textarea; deleting the line cancels them
  (`prunePastesByMessage`). `Preview` mode swaps placeholders for their `blob:` URL
  (`Attachments.withBlobPreviews`).

## Check

- `npm run test:js -- --runInBand fileCapture` drives the paste/drop listeners in jsdom.
- `elm-test tests/Elm/AttachmentsTest.elm` covers paste pruning, batch-end failures and `retarget`.

## Not wired

- Org avatars: the transport has the anchor, no UI uses it. User avatars: see [avatars.md](avatars.md).
- Contract comments: no tension anchor the backend accepts, so no paste capture, no Attach.

## Files

| File | Role |
|------|------|
| `src/Api/File.elm` | REST upload/delete, `Anchor`, `ApiError`, `maxUploadBytes`, decoders |
| `src/Components/Attachments.elm` | Staging, per-cid queue, paste handling, chip views, `handoff`, `retarget` |
| `src/Components/Comments.elm` | Carriers, public wrappers (`kickoffUploads`, …), ack patch, saved-file view/delete |
| `src/Ports.elm` | `pastedFilesFromJs`, `insertAtCaret` |
| `assets/js/fileCapture.js` | `initFileCapture` (paste/drop capture) |
| `assets/js/ports.js` | `INSERT_AT_CARET` |
| `assets/sass/components/_comments.scss` | `.is-dragover` drop-target highlight, chip styles |
| `tests/Js/fileCapture.test.js` | jsdom check of the paste/drop listeners |
| `tests/Elm/AttachmentsTest.elm` | Pure transitions of `Attachments` |
| `assets/js/textutils.js` | `replaceRange`, shared with rich-text |
| `src/ModelSchema.elm`, `src/Query/QueryTension.elm` | `CommentFile`, `Comment.files` |
| `src/Query/AddTension.elm` | `buildComment`, `expectedAttachments` (`nfiles` post key) |
