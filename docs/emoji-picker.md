# Emoji Picker

An inline emoji picker triggered by typing `:` in comment textareas.

## Usage

1. In any comment textarea (tension comments, contract comments, update comments, new tension input), type `:` preceded by a space or at the start of the line.
2. An emoji grid popup appears near the cursor.
3. Continue typing after `:` to filter emojis (e.g., `:smi` shows smile-related emojis).
4. Click an emoji to insert its unicode character, replacing the `:pattern` text.
5. Press Space, Tab, Enter, or Escape to dismiss the picker without inserting an emoji.
6. Backspacing to delete the `:` also dismisses the picker.

## Architecture

The emoji picker follows the same architecture as the `@` mention tooltip (UserInput component):

- **JS detects the trigger** (`:` character) in `bulma_drivers.js` via `markupRichText()`
- **JS positions the popup** at the caret using `showEmojiInput()` / `hideEmojiInput()`
- **JS communicates with Elm** via ports: `openEmojiPickerFromJs`, `closeEmojiPickerFromJs`, `changeEmojiPatternFromJs`
- **Elm manages state** in `Components.EmojiPicker` (port module)
- **Elm renders the grid** in `viewEmojiSeeker`
- **Selection flows back to JS** via `Ports.pushEmojiSelection` which triggers the `PUSH_EMOJI_SELECTION` action in `ports.js`

## Files

| File | Role |
|------|------|
| `src/Components/EmojiData.elm` | Pure data module: ~400 curated emojis with search function |
| `src/Components/EmojiPicker.elm` | Port module: state management, ports, view |
| `src/Ports.elm` | `pushEmojiSelection` outgoing port |
| `src/Components/Comments.elm` | Wires EmojiPicker into all comment textareas |
| `assets/js/bulma_drivers.js` | `:` trigger detection, `showEmojiInput`/`hideEmojiInput` helpers |
| `assets/js/ports.js` | `PUSH_EMOJI_SELECTION` action handler |
| `assets/sass/components/_emoji.scss` | `.emojiPicker`, `.emojiGrid`, `.emojiItem` styles |

## Adding Emojis

To add more emojis, edit the `allEmojis` list in `src/Components/EmojiData.elm`. Each entry has:

```elm
{ unicode = "😀", name = "grinning", keywords = "smile happy face" }
```

The `name` and `keywords` fields are used for search filtering.
