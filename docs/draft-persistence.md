# Draft Persistence

Unsent user text is saved to `localStorage.drafts` and restored when the user comes back to
the same context — across navigation and browser restart.

Three contexts: the new tension modal (title + message), the invite modal (message), and
tension comments (message, keyed by tensionId). Empty content clears the draft instead of
saving it, and comment drafts rotate out the oldest past 20 entries.

## Types

`src/Codecs.elm` holds `TensionDraft`, `CommentDraft`, the `DraftStore` (newTension /
newInvite / comments dict) and the `DraftUpdate` union (`SaveNewTension`, `ClearComment`, …)
used as the single vocabulary for mutations, plus its codecs.

## Saving

1. A component debounces with `SaveDraftDelayed timer`, carrying only a timer token.
2. When the timer fires, the handler reads the content from the model — not from the message —
   so rich-text port edits are captured.
3. It emits `DoUpdateDraft` (a `GlobalCmd`), which page-level `mapGlobalOutcmds` forwards to
   `Global.UpdateDraft`.
4. `Global.elm` owns rotation and persists through `Ports.saveDrafts`.

Submitting a form emits the matching `Clear*` update.

## Restoring

The `DraftStore` lives in `SessionData` only — never in the `SessionCommon` copies components
hold, which would go stale. So drafts are pushed in, not pulled:

- `Org/Tension.elm` and `Org/Project.elm` (CardPanel) pass the draft to `Comments.initWithDraft`.
- Modals that outlive their page (`Form/NewTension.elm`, `Components/JoinOrga.elm`) get the
  draft injected with `setCurrentDraft` at the Org page level, intercepting `OnOpen` before
  the component update runs (see the interceptor pattern in `docs/architecture.md`).

## Files

| File | Purpose |
|------|---------|
| `src/Codecs.elm` | Draft types and codecs |
| `src/Global.elm` | `UpdateDraft` handler, rotation, persistence |
| `src/Session.elm` | `DoUpdateDraft` GlobalCmd |
| `src/Components/Comments.elm` | `initWithDraft`, `getCurrentMessage` |
| `src/Form/NewTension.elm`, `src/Components/JoinOrga.elm`, `src/Components/CardPanel.elm` | Draft-aware components |
| `src/Org/*.elm` | Forward `DoUpdateDraft`, inject drafts on `OnOpen` |
| `public/index.js`, `assets/js/ports.js` | Read/write `localStorage.drafts` |
