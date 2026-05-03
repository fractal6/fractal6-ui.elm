# Markup Rich Text

Keyboard interactions wired into comment/tension textareas. Implemented as a `keydown` handler in `assets/js/bulma_drivers.js` (`markupRichText` function).

## Triggers

Two inline pickers attach to any textarea registered via `setupHandler("keydown", markupRichText, ...)`:

- `@` at start-of-line or after whitespace → opens the user search panel (`searchInput`).
- `:` at start-of-line or after whitespace → opens the emoji picker (`emojiInput`).

Both are routed through the shared `handlePickerKey()` helper, which exchanges keystrokes with Elm via ports (`changePatternFromJs` / `changeEmojiPatternFromJs`, `arrowFromJs`, `selectActiveItemFromJs`).

## Picker Key Handling

`handlePickerKey()` operates in two states, tracked via `tooltip.dataset.arrowMode`:

**Pattern mode** (panel open, no item highlighted):
- `ArrowDown` or `Tab` → enter arrow mode and highlight the first item.
- `Space`, `Enter`, `Escape`, `ArrowUp/Left/Right`, or backspacing past the trigger char → close the panel.
- Any pattern char (or `Backspace` within the pattern) → forward the updated query to Elm.

**Arrow mode** (an item is highlighted):
- `ArrowUp/Down/Left/Right` → move selection (sent to Elm via `arrowPort`).
- `Enter` / `Return` / `Tab` → select the highlighted item (`selectPort`).
- Anything else → close the panel.

`Tab` is treated as `ArrowDown` when entering arrow mode so users can quickly grab the first match without reaching for the arrow keys.

## Other Behaviors in `markupRichText`

Beyond the pickers, the same handler also implements:

- List continuation on newline (`-`, `*`, `+`, `1.`, `> `, `- [ ]`, `* [ ]`, `+ [ ]`).
- Tab / Shift+Tab indentation inside list items.
- Auto-removal of an empty list marker on a second `Enter`.
- Backspace on an empty list line dedents one level, then removes the marker.

## Programmatic edits and undo (`replaceRange`)

All textarea mutations in `markupRichText` go through the `replaceRange(el, start, end, text, caretStart, caretEnd)` helper exported from `assets/js/textutils.js`. It tries `document.execCommand('insertText', ...)` first so the browser's native Ctrl+Z stack survives the edit. `execCommand` is deprecated but remains the only cross-browser way to mutate a `<textarea>` without wiping undo, and every shipping browser still implements it (no spec'd replacement). If the call returns `false` or throws, the helper falls back to a direct `el.value = ...` assignment plus a manual `input` event — the edit still applies but is not undoable.

The same helper is used by `ports.js` for `@`-mention and emoji picker insertions, so all programmatic textarea edits in the app share one undo-preserving path.

## Files

| File | Role |
|------|------|
| `assets/js/bulma_drivers.js` | `markupRichText`, `handlePickerKey`, `show/hideSearchInput`, `show/hideEmojiInput` |
| `assets/js/textutils.js` | `replaceRange` (undo-preserving textarea edit), `getCaretCoordinates` |
| `src/Components/UserInput.elm` | Elm side of the `@` user picker |
| `src/Components/EmojiPicker.elm` | Elm side of the `:` emoji picker |
| `src/Ports.elm` | Port declarations used by both pickers |
