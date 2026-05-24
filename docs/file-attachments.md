# File Attachments

Lets users attach files to comments and paste images (screenshots) inline.
Wired into `Components/Comments.elm`. Backend serves and stores via the
`/file/*` REST routes (see `Apis.file` in `Session.elm`).

## Transport — `Api/File.elm`

Stand-alone REST module, separate from the GraphQL client (the `/file/*`
routes are not in the GQL schema beyond the read-only `Comment.files`,
`User.avatar` and `Node.avatar` edges).

- `upload : Apis -> Anchor -> File -> Cmd msg` — `POST {api.file}/upload`,
  multipart, anchor fields (`tid`/`cid`/`userid`/`orgaid`) sent as form
  parts alongside `file`.
- `delete : Apis -> String -> Cmd msg` — `DELETE {api.file}/<id>`. A 404 is
  treated as success (file already gone).
- No GET helper — render `<a href="/file/<id>">` / `<img src="/file/<id>">`
  and let the browser follow the 302 to the presigned URL.

`Anchor` covers `CommentAnchor { tid, cid }`, `UserAvatar username`,
`OrgAvatar rootnameid`. Only `CommentAnchor` is wired in v1.

Errors are mapped to `ApiError` so callers can react: `StorageDisabled` (503),
`TooLarge` (413), `Unauthorized` (401), `Forbidden` (403), `NotFound` (404),
`BadRequest`, `Network`, `Other`. `errorToString` gives a user-facing string.

## Carrier-first ordering

Uploads anchor on a saved `cid`, so the carrier mutation has to land first.
Per submit flow:

| Flow | Order |
|------|------|
| New comment on existing tension | `pushTensionPatch` → use returned cid → upload each file |
| Edit comment                    | `patchComment` → reuse cid → upload each file |
| New tension with attachments    | parent page runs `addOneTension` → calls `Comments.kickoffUploads` with the resulting `{tid, cid}` |

For flows whose carrier lives inside `Comments.elm` the handoff is
automatic (see `handoffPendingForNewComment` / `handoffPendingToCid`).
Pages that own the carrier themselves (currently only the tension-creation
page using `viewNewTensionCommentInput`) call `Comments.kickoffUploads`
once their mutation succeeds.

## Per-cid serial queue

Backend rewrites `![…](filename)` placeholders in the comment message on
each upload — a read-modify-write on `Comment.message`. Concurrent uploads
for the same comment would race (last-writer-wins), so the queue drains
one at a time per cid: see `drainNext` and `Model.activeByCid` (a
`Dict cid UploadBatch`). On each completion the head is dequeued and the
next upload is started.

## Inline screenshot paste

1. The textarea carries `data-paste-capture="true"`. A single
   `document`-level `paste` listener in `assets/js/ports.js` reads
   `clipboardData.items` of `kind === 'file'` and ships them to Elm via
   `pastedFilesFromJs` together with the textarea id.
2. `OnPastedFiles` in `Comments.elm` generates a unique filename per item
   (`paste-<epochms>-<n><ext>`, counter persisted in `Model.pasteCounter`),
   stores the blob in `pendingByEditor[targetId]`, and issues
   `Ports.insertAtCaret targetId ("![](<filename>) ")` to place the
   placeholder at the caret without breaking native undo (the underlying
   `replaceRange` helper is the same one used by the rich-text shortcuts;
   see `docs/markup-richtext.md`).
3. On carrier success the queue drains. As each `OnUploadAck` arrives the
   comment's message is rewritten locally
   (`String.replace ("](" ++ filename ++ ")") ("](/file/" ++ id ++ ")")`)
   and the new file is appended to `comment.files`. No refetch is required.

The placeholder filename uniqueness is the frontend's responsibility — the
server-side rewrite (`rewriteMessageForFile`) only catches the first
occurrence of each name and skips inside fenced/inline code blocks.

## Chips

- Editor row (`viewPendingsRow`): the "Attach" button (`File.Select.files`)
  + a tag per `pendingByEditor` entry. Status icon is queued / spinner
  while `Uploading` / warning on `UploadFailed`. Removal before submit is
  client-only.
- Saved row (`viewSavedAttachments`): under each rendered comment, lists
  `comment.files` filtered by `embedded == False`. The delete button is
  shown only when the file's `createdBy.username` matches the viewer.
  Deletes go through `Api.File.delete` and prune the file from the local
  comment on success.

## Not wired

- Avatars (user + org). The transport supports `UserAvatar` / `OrgAvatar`
  anchors but no UI consumes them yet.
- Contract comments. Contracts don't have a tension anchor that the
  backend would accept; `data-paste-capture` is intentionally omitted on
  `commentContractInput` and no Attach button is rendered.
- A loading placeholder for inline pasted images while their upload is
  in flight — the broken-image icon shows briefly until the local message
  rewrite swaps the URL.

## Files

| File | Role |
|------|------|
| `src/Api/File.elm` | REST upload/delete, `Anchor`, `ApiError`, decoders |
| `src/Components/Comments.elm` | Per-editor queue, paste handler, chip views, `kickoffUploads` |
| `src/Ports.elm` | `pastedFilesFromJs` port + `insertAtCaret` helper |
| `assets/js/ports.js` | Document-level `paste` capture + `INSERT_AT_CARET` action |
| `assets/js/textutils.js` | `replaceRange` (undo-preserving textarea edit, shared with rich-text) |
| `src/ModelSchema.elm` | `CommentFile` alias and `Comment.files` field |
| `src/Query/QueryTension.elm` | `commentPayload` now selects `Comment.files` |
