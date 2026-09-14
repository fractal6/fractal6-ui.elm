# File Attachments

Attach files to comments and paste screenshots inline. Frontend logic lives in
`src/Components/Comments.elm`, transport in `src/Api/File.elm`, backend on the `/file/*` REST
routes (`Apis.file`).

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
own carrier calls `kickoffUploads` itself.

The carrier also declares **how many** files are coming (never which): `stagedCount` posts
`nfiles`, mapped to `Comment.expected_attachments` by `Query.AddTension.expectedAttachments`.
The backend holds the notification email until that many files land.

Uploads drain **serially per cid** (`drainNext`): the backend rewrites the `![…](filename)`
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

## Inline paste

Textareas carry `data-paste-capture="true"`; a document-level `paste` listener in
`assets/js/ports.js` forwards clipboard files to Elm with the textarea id. Two constraints on
that JS step:

- **It owns the filename.** It rebuilds each clipboard `File` as `paste-<timestamp>-<i><ext>`.
  The name must match between the markdown placeholder and the multipart part, because the
  backend rewrite (`web/handlers/files.go::rewriteMessageForFile`) looks the placeholder up by
  multipart name, and only rewrites the first occurrence of each name.
- **The port is a broadcast.** Every mounted `Comments` instance receives it, so each only
  accepts the editors it renders — `Model.pasteTargets`, defaulting to `threadEditors`,
  overridden with `setPasteTargets Comments.modalEditors` by modal components. Without it the
  same paste is inserted twice.

Then, in Elm:

1. `OnPastedFiles` stores `{file, objectUrl, …}` in `pendingByEditor[targetId]` and inserts
   `![](<filename>)` at the caret through `Ports.insertAtCaret` (undo-preserving).
2. On carrier success the queue drains.
3. Each `OnUploadAck` rewrites the local message to `/file/<id>` and appends the file to
   `comment.files`, mirroring what the server stored.

Blob URLs are revoked through `REVOKE_OBJECT_URL` on ack, manual removal, and pruning.

## Views

- `viewPendingsRow` — editor row: Attach button, one chip per pending file, upload progress.
- `viewSavedAttachments` — `comment.files` with `embedded == False` under a rendered comment,
  plus delete for the owner.
- Staged pastes live as markdown in the textarea; deleting the line cancels them
  (`prunePastesByMessage`). `Preview` mode swaps placeholders for their `blob:` URL.

## Check

`npm run test:js -- --runInBand uploadChips` drives the real Elm state and views in jsdom (no
HTTP, no browser engine).

## Not wired

- Avatars: the transport has the anchors, no UI uses them.
- Contract comments: no tension anchor the backend accepts, so no paste capture, no Attach.

## Files

| File | Role |
|------|------|
| `src/Api/File.elm` | REST upload/delete, `Anchor`, `ApiError`, decoders |
| `src/Components/Comments.elm` | Per-cid queue, paste handling, chip views, `kickoffUploads` |
| `src/Ports.elm` | `pastedFilesFromJs`, `insertAtCaret` |
| `assets/js/ports.js` | Paste capture, `INSERT_AT_CARET` |
| `assets/js/textutils.js` | `replaceRange`, shared with rich-text |
| `src/ModelSchema.elm`, `src/Query/QueryTension.elm` | `CommentFile`, `Comment.files` |
| `src/Query/AddTension.elm` | `buildComment`, `expectedAttachments` (`nfiles` post key) |
