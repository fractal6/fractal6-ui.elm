# File Attachments

Attach files to comments and paste screenshots inline. All the frontend logic lives in
`src/Components/Comments.elm`; the backend serves and stores through the `/file/*` REST routes
(`Apis.file` in `Session.elm`).

## URL resolution

Files are stored as relative `/file/<id>` paths, both in the comment markdown and in the
`Comment.files` edge. The file server origin is prepended at render time: `frac6Renderer`
overrides the `image` handler for any `src` starting with `/file/`, and `viewSavedAttachments`
builds its `href` from `session.file_server_url`. The origin is injected at build time
(`FILE_SERVER_URL` in `webpack.config.js`) and mirrored on `Apis` and `SessionCommon`. Pass
`""` as `renderMarkdown`'s first argument for content that can't hold attachments — the image
override then does nothing.

## Transport — `src/Api/File.elm`

Stand-alone REST module: the `/file/*` routes are not in the GraphQL schema (only the
read-only `Comment.files`, `User.avatar`, `Node.avatar` edges are).

- `upload` — multipart `POST {api.file}/upload` with the anchor fields alongside `file`.
- `delete` — `DELETE {api.file}/<id>`; 404 counts as success.
- No GET: render `/file/<id>` directly and let the browser follow the redirect to the
  presigned URL.

`Anchor` is `CommentAnchor { tid, cid }` / `UserAvatar` / `OrgAvatar`; only comments are wired
to a UI. Errors map to an `ApiError` union (`StorageDisabled`, `TooLarge`, `Unauthorized`, …)
with `errorToString` for display.

## Upload ordering

An upload anchors on a saved `cid`, so the carrier mutation must land first:

| Flow | Order |
|------|-------|
| New comment on existing tension | `pushTensionPatch` → returned cid → upload each file |
| Edit comment | `patchComment` → reuse cid → upload each file |
| New tension with attachments | the page runs `addOneTension`, then calls `Comments.kickoffUploads` with `{tid, cid}` |

The first two carriers live inside `Comments.elm`, so the handoff is automatic
(`handoffPendingForNewComment` / `handoffPendingToCid`). A page owning its own carrier calls
`kickoffUploads` itself once its mutation succeeds.

Uploads then drain **serially per cid** (`drainNext`, `Model.activeByCid`): the backend
rewrites the `![…](filename)` placeholders inside `Comment.message` on every upload, a
read-modify-write that concurrent uploads would lose.

## Inline paste

Textareas carry `data-paste-capture="true"`; a single document-level `paste` listener in
`assets/js/ports.js` forwards clipboard files to Elm with the textarea id.

That JS step is **the source of truth for the filename**: it rebuilds each clipboard `File` as
`paste-<timestamp>-<i><ext>` and creates the `blob:` object URL. The name must match between
the markdown placeholder and the multipart part, because the backend rewrite
(`web/handlers/files.go::rewriteMessageForFile`) looks the placeholder up by multipart name —
clipboard items would otherwise all arrive as `image.png`. Uniqueness is on us: the server
only rewrites the first occurrence of each name.

Then, in Elm:

1. `OnPastedFiles` stores `{file, objectUrl, …}` in `pendingByEditor[targetId]` and inserts
   `![](<filename>)` at the caret through `Ports.insertAtCaret` (undo-preserving).
2. On carrier success the queue drains.
3. Each `OnUploadAck` rewrites the local message to `/file/<id>` and appends the file to
   `comment.files`, mirroring what the server stored — a refresh renders the same `<img>`.

## Chips and preview

The editor row (`viewPendingsRow`) shows the Attach button and one tag per non-paste pending
file. Pastes are deliberately absent: they live as markdown in the textarea, and deleting that
line is how you cancel them (`prunePastesByMessage` drops them and revokes the blob URL at
handoff). The saved row (`viewSavedAttachments`) lists `comment.files` with `embedded == False`
under each rendered comment, with a delete button for the file owner.

In `Preview` mode, pending `![](paste-…)` placeholders are swapped for their `blob:` URL
before rendering. Blob URLs are revoked through the `REVOKE_OBJECT_URL` port on upload ack,
manual removal, and placeholder pruning.

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
