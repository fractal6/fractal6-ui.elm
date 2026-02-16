# Draft Persistence

Save user drafts (tension comments and new tension forms) to localStorage, restoring them across page navigation and browser close.

Drafts are automatically saved to localStorage and restored when returning to the same context.

**Supported contexts:**
- **New tension modal**: Stores title + message
- **Invite modal**: Stores message only
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
    , newInvite : Maybe CommentDraft
    , comments : Dict String CommentDraft  -- keyed by tensionId
    }

-- Type-safe draft update commands
type DraftUpdate
    = SaveNewTension TensionDraft
    | ClearNewTension
    | SaveNewInvite CommentDraft
    | ClearNewInvite
    | SaveComment String CommentDraft  -- tensionId, draft
    | ClearComment String              -- tensionId
```

## Architecture

### Message Flow

1. **Components** schedule debounced saves via `SaveDraftDelayed timer` (timer only, no content)
2. **Handlers** read current content from model when timer fires, then emit `DoUpdateDraft` GlobalCmd
3. **Page-level `mapGlobalOutcmds`** forwards to `Global.UpdateDraft`
4. **Global.elm** processes the update and persists to localStorage via `Ports.saveDrafts`

Note: Content is read at fire-time (not schedule-time) to capture any modifications made by rich text ports.

### Draft Restoration

Drafts are restored using an **interceptor pattern** at the Org page level. Instead of components reading drafts from a stale `SessionCommon` copy, Org pages inject the current draft from `global.session.data.drafts` into the component state before calling its update function on `OnOpen`:

```elm
NewTensionMsg msg ->
    let
        state =
            case msg of
                NTF.OnOpen _ _ ->
                    NTF.setCurrentDraft global.session.data.drafts.newTension model.tensionForm
                _ ->
                    model.tensionForm
        ( data, out ) =
            NTF.update apis msg state
    ...
```

This ensures the component always has fresh draft data regardless of when the page was initialized.

Additionally:
- **Tension page**: Passes draft directly to `Comments.initWithDraft` on init
- **CardPanel**: Looks up draft from session when opening, passes to `Comments.initWithDraft`

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
Draft is injected via `setCurrentDraft` at the Org page level before `OnOpen` is processed.
```elm
-- In component's OnOpen handler:
case model.currentDraft of
    Just tensionDraft ->
        { model | nodeDoc = model.nodeDoc
            |> NodeDoc.updatePost "title" tensionDraft.title
            |> NodeDoc.updatePost "message" tensionDraft.message
        }
    Nothing ->
        model
```

**Invite modal** (`src/Components/JoinOrga.elm`):
Draft is injected via `setCurrentDraft` at the Org page level before `OnOpen` is processed.
```elm
-- In component's OnOpen handler (InviteOne branch):
let savedDraft = model.currentDraft
```

**Tension comments** (`src/Org/Tension.elm`):
```elm
let
    maybeDraft =
        Dict.get tid session.data.drafts.comments
in
Comments.initWithDraft focusid tid session.common maybeDraft
```

**CardPanel** (`src/Org/Project.elm`):
```elm
OnOpenCardPanel card ->
    let
        maybeDraft =
            case card.card of
                CardTension t ->
                    Dict.get t.id global.session.data.drafts.comments

                _ ->
                    Nothing
    in
    ( model
    , Cmd.map CardPanelMsg (send (CardPanel.OnOpen card maybeDraft))
    , Cmd.none
    )
```

### Comments.initWithDraft

The `Comments.initWithDraft` function accepts the full `CommentDraft` record:

```elm
initWithDraft : String -> String -> SessionCommon -> Maybe CommentDraft -> State
initWithDraft nameid tensionid session maybeDraft =
    let
        model = initModel nameid tensionid session
        tension_form =
            case maybeDraft of
                Just draft ->
                    { model.tension_form | post = Dict.insert "message" draft.message model.tension_form.post }
                Nothing ->
                    model.tension_form
    in
    State { model | tension_form = tension_form }
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

**CardPanel comment** (`src/Components/CardPanel.elm`):
```elm
SaveDraftDelayed timerValue ->
    if timerValue == model.draftSaveTimer then
        let
            draftMessage = Comments.getCurrentMessage model.comments |> withDefault ""
            draft = CommentDraft draftMessage ""
        in
        if draftMessage == "" then
            ( model, out1 [ DoUpdateDraft (ClearComment model.tension_form.id) ] )
        else
            ( model, out1 [ DoUpdateDraft (SaveComment model.tension_form.id draft) ] )
    else
        ( model, noOut )
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
| `src/Components/Comments.elm` | initWithDraft, getCurrentMessage, ClearComment emit |
| `src/Components/CardPanel.elm` | Draft load from session on OnOpen, SaveDraftDelayed |
| `src/Form/NewTension.elm` | Draft load/save for new tensions, `setCurrentDraft` |
| `src/Components/JoinOrga.elm` | Draft load/save for invite messages, `setCurrentDraft` |
| `src/Org/Tension.elm` | Draft load/save for tension comments |
| `src/Org/Project.elm` | OnOpenCardPanel passes draft from session to CardPanel |
| `src/Org/*.elm` | mapGlobalOutcmds forwards DoUpdateDraft, inject drafts on OnOpen |
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
  "newInvite": {
    "message": "My invite message",
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

- **Debounced saves**: 3.5 second delay prevents writes on every keystroke
- **Lazy rotation**: Only checks comment draft count when adding a new one
- **Minimal port traffic**: Single JSON object per save operation
- **Single source of truth**: DraftStore lives only in SessionData, avoiding stale reads from SessionCommon
- **Type-safe**: DraftUpdate union type ensures correct usage at compile time
- **Direct restoration**: Drafts are passed directly from session on component init (no port propagation needed)
