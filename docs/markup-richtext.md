# Markup Rich Text

Keyboard interactions wired into comment/tension textareas. Implemented as a `keydown` handler in `assets/js/bulma_drivers.js` (`markupRichText` function).

## Triggers

Three inline pickers attach to any textarea registered via `setupHandler("keydown", markupRichText, ...)`:

- `@` at start-of-line or after whitespace → opens the user search panel (`searchInput`).
- `:` at start-of-line or after whitespace → opens the emoji picker (`emojiInput`).
- `0x` at start-of-line or after whitespace → opens the tension picker (`tensionInput`). Picking a tension replaces `0x<pattern>` with its id, rendered as a link by `Markdown.elm`.

All are routed through the shared `handlePickerKey()` helper, which exchanges keystrokes with Elm via ports (`changePatternFromJs` / `changeEmojiPatternFromJs` / `changeTensionPatternFromJs`, `arrowFromJs`, `selectActiveItemFromJs`).

### Tension picker

- The pattern can hold spaces: a single Space does not close this picker; two consecutive spaces, or a pattern over 48 chars (the `handlePickerKey` lookback), do.
- Scope is always the whole organisation (root nameid of the `UserInput` targets).
- The list is a pool of up to 100 tensions filtered client-side: a tension matches if any word of the pattern is in its title or prefixes its id, best matches first, 15 shown.
- The pool is the latest open tensions until a word is complete (followed by a space); then it is a server `anyoftext` title search, refetched each time a complete word is added or removed.

## Picker Key Handling

`handlePickerKey()` operates in two states, tracked via `tooltip.dataset.arrowMode`:

**Pattern mode** (panel open, no item highlighted):
- `ArrowDown` or `Tab` → enter arrow mode and highlight the first item.
- `Space` (the tension picker needs two in a row), `Enter`, `Escape`, `ArrowUp/Left/Right`, or backspacing past the trigger → close the panel.
- Any pattern char (or `Backspace` within the pattern) → forward the updated query to Elm.

**Arrow mode** (an item is highlighted):
- `ArrowUp/Down/Left/Right` → move selection (sent to Elm via `arrowPort`).
- `Tab` → cycle to the next item, wrapping around (`arrowPort` sends `"next"`).
- `Enter` / `Return` → select the highlighted item (`selectPort`).
- Anything else → close the panel.

`Tab` enters arrow mode on the first item, then cycles through items, so users can browse matches without reaching for the arrow keys.

## Other Behaviors in `markupRichText`

Beyond the pickers, the same handler also implements:

- List continuation on newline (`-`, `*`, `+`, `1.`, `> `, `- [ ]`, `* [ ]`, `+ [ ]`).
- Tab / Shift+Tab indentation inside list items.
- Auto-removal of an empty list marker on a second `Enter`.
- Backspace on an empty list line dedents one level, then removes the marker.

## Tab fallthrough

When `markupRichText` doesn't consume the Tab (no selection, not in a list), it bubbles to the
`.submitFocus` container, which focuses `.defaultSubmit` — unless that button is disabled, since a
disabled element can't take focus and the Tab would be swallowed.

## Programmatic edits and undo (`replaceRange`)

All textarea mutations in `markupRichText` go through the `replaceRange(el, start, end, text, caretStart, caretEnd)` helper exported from `assets/js/textutils.js`. It tries `document.execCommand('insertText', ...)` first so the browser's native Ctrl+Z stack survives the edit. `execCommand` is deprecated but remains the only cross-browser way to mutate a `<textarea>` without wiping undo, and every shipping browser still implements it (no spec'd replacement). If the call returns `false` or throws, the helper falls back to a direct `el.value = ...` assignment plus a manual `input` event — the edit still applies but is not undoable.

The same helper is used by `ports.js` for `@`-mention, emoji and tension picker insertions, so all programmatic textarea edits in the app share one undo-preserving path.

## Files

| File | Role |
|------|------|
| `assets/js/bulma_drivers.js` | `markupRichText`, `handlePickerKey`, `show/hideSearchInput`, `show/hideEmojiInput`, `show/hideTensionInput` |
| `assets/js/textutils.js` | `replaceRange` (undo-preserving textarea edit), `getCaretCoordinates`, `caretAtAnchor` |
| `src/Components/UserInput.elm` | Elm side of the `@` user picker |
| `src/Components/EmojiPicker.elm` | Elm side of the `:` emoji picker |
| `src/Components/TensionPicker.elm` | Elm side of the `0x` tension picker (fetch, filter, view) |
| `src/Ports.elm` | Port declarations used by the pickers |
