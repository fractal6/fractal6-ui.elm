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

- List continuation on newline (`-`, `*`, `+`, `1.`, `> `, `- [ ]`).
- Tab / Shift+Tab indentation inside list items.
- Auto-removal of an empty list marker on a second `Enter`.

## Files

| File | Role |
|------|------|
| `assets/js/bulma_drivers.js` | `markupRichText`, `handlePickerKey`, `show/hideSearchInput`, `show/hideEmojiInput` |
| `src/Components/UserInput.elm` | Elm side of the `@` user picker |
| `src/Components/EmojiPicker.elm` | Elm side of the `:` emoji picker |
| `src/Ports.elm` | Port declarations used by both pickers |
