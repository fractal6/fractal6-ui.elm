# Draft Persistence

Save user drafts (tension comments and new tension forms) to localStorage, restoring them across page navigation and browser close. Also includes a fix for checkbox interactions that previously could cause data loss.

## Features

### 1. OnCheckbox Backup Fix

When a user clicks a checkbox in a comment's markdown while composing a new comment, the checkbox update no longer causes loss of the in-progress text.

**How it works:**
- Before submitting a checkbox change, the current `tension_form.post["message"]` is saved to `post_backup`
- After `CommentPatchAck` succeeds in stealth mode, the backup is restored to the form

### 2. Draft Persistence

Drafts are automatically saved to localStorage and restored when returning to the same context.

**Supported contexts:**
- **New tension modal**: Stores title + message
- **Tension comment**: Stores message only (keyed by tensionId)

**Limits:**
- Maximum 20 comment drafts (rotation removes oldest when exceeded)
- Empty drafts are not saved (removes existing draft if content is cleared)

## Data Types

Located in `src/Codecs.elm`:

```elm
-- Draft for new tension modal (has title + message)
type alias TensionDraft =
    { title : String
    , message : String
    , updatedAt : String
    }

-- Draft for tension comments (message only)
type alias CommentDraft =
    { message : String
    , updatedAt : String
    }

-- Store with explicit separation between new tension and comment drafts
type alias DraftStore =
    { newTension : Maybe TensionDraft
    , comments : Dict String CommentDraft  -- keyed by tensionId
    }

-- Type-safe draft update commands
type DraftUpdate
    = SaveNewTension TensionDraft
    | ClearNewTension
    | SaveComment String CommentDraft  -- tensionId, draft
    | ClearComment String              -- tensionId
```

## Architecture

### Message Flow

1. **Components** schedule debounced saves via `SaveDraftDelayed timer` (timer only, no content)
2. **Handlers** read current content from model when timer fires, then emit `DoUpdateDraft` GlobalCmd
3. **Page-level `mapGlobalOutcmds`** forwards to `Global.UpdateDraft`
4. **Global.elm** processes the update and persists to localStorage

Note: Content is read at fire-time (not schedule-time) to capture any modifications made by rich text ports.

### GlobalCmd (`src/Session.elm`)

```elm
| DoUpdateDraft DraftUpdate
```

### Global Msg (`src/Global.elm`)

```elm
| UpdateDraft DraftUpdate
```

## Usage

### Loading drafts on init

**New tension modal** (`src/Form/NewTension.elm`):
```elm
case model.session.drafts.newTension of
    Just tensionDraft ->
        { model | nodeDoc = model.nodeDoc
            |> NodeDoc.updatePost "title" tensionDraft.title
            |> NodeDoc.updatePost "message" tensionDraft.message
        }
    Nothing ->
        model
```

**Tension comments** (`src/Org/Tension.elm`):
```elm
let
    maybeDraft =
        Dict.get tid session.common.drafts.comments
            |> Maybe.map .message
in
Comments.initWithDraft focusid tid session.common maybeDraft
```

### Saving drafts (debounced)

Draft content is read from current model state when `SaveDraftDelayed` fires (not when scheduled).
This ensures rich text port modifications are captured correctly.

**New tension** (`src/Form/NewTension.elm`):
```elm
let
    newTimer = model.draftSaveTimer + 1
in
( { model | draftSaveTimer = newTimer }
, sendSleep (SaveDraftDelayed newTimer) TIME_DELAY
)

-- Handler reads current content from model
SaveDraftDelayed timerValue ->
    if timerValue == model.draftSaveTimer then
        let
            draftTitle = Dict.get "title" model.nodeDoc.form.post |> withDefault ""
            draftMessage = Dict.get "message" model.nodeDoc.form.post |> withDefault ""
            draft = TensionDraft draftTitle draftMessage ""
        in
        if draftMessage == "" then
            ( model, Out [] [ DoUpdateDraft ClearNewTension ] Nothing )
        else
            ( model, Out [] [ DoUpdateDraft (SaveNewTension draft) ] Nothing )
    else
        ( model, noOut )
```

**Tension comment** (`src/Org/Tension.elm`):
```elm
let
    newTimer = model.draftSaveTimer + 1
in
( newTimer, sendSleep (SaveDraftDelayed newTimer) TIME_DELAY )

-- Handler reads current content from Comments component
SaveDraftDelayed timerValue ->
    if timerValue == model.draftSaveTimer then
        let
            draftMessage = Comments.getCurrentMessage model.comments |> withDefault ""
            draft = CommentDraft draftMessage ""
        in
        if draftMessage == "" then
            ( model, Cmd.none, send (Global.UpdateDraft (ClearComment model.tensionid)) )
        else
            ( model, Cmd.none, send (Global.UpdateDraft (SaveComment model.tensionid draft)) )
    else
        ( model, Cmd.none, Cmd.none )
```

### Clearing drafts on submission

**New tension**:
```elm
DoUpdateDraft ClearNewTension :: gcmds
```

**Tension comment** (`src/Components/Comments.elm`):
```elm
Out cmds [ DoUpdateDraft (ClearComment model.tension_form.id) ] result
```

### Forwarding in mapGlobalOutcmds

Each page-level `mapGlobalOutcmds` must handle:
```elm
DoUpdateDraft draftUpdate ->
    ( Cmd.none, send (Global.UpdateDraft draftUpdate) )
```

## Files Involved

| File | Purpose |
|------|---------|
| `src/Codecs.elm` | TensionDraft, CommentDraft, DraftStore, DraftUpdate types, encoders/decoders |
| `src/Session.elm` | GlobalCmd (DoUpdateDraft) |
| `src/Ports.elm` | saveDrafts function |
| `src/Global.elm` | UpdateDraft handler, draft rotation logic |
| `src/Components/Comments.elm` | post_backup, initWithDraft, getCurrentMessage, ClearComment emit |
| `src/Form/NewTension.elm` | Draft load/save for new tensions |
| `src/Org/Tension.elm` | Draft load/save for tension comments |
| `src/Org/*.elm` | mapGlobalOutcmds forwards DoUpdateDraft |
| `public/index.js` | Load drafts from localStorage on init |
| `assets/js/ports.js` | SAVE_DRAFTS action handler |

## localStorage Format

```json
{
  "newTension": {
    "title": "My tension title",
    "message": "My tension message",
    "updatedAt": "2024-01-15T10:30:00Z"
  },
  "comments": {
    "tensionId123": {
      "message": "My comment draft",
      "updatedAt": "2024-01-15T10:30:00Z"
    }
  }
}
```

## Performance Considerations

- **Debounced saves**: TIME_DELAYms delay prevents writes on every keystroke
- **Lazy rotation**: Only checks comment draft count when adding a new one
- **Minimal port traffic**: Single JSON object per save operation
- **Session sync**: Both SessionData and SessionCommon are updated to avoid stale reads
- **Type-safe**: DraftUpdate union type ensures correct usage at compile time
