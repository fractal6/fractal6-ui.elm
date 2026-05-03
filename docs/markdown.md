
# Markdown Rich-Text Editing System

The markdown editing system provides a toolbar for formatting text in comment inputs. It spans three layers: the Elm toolbar UI, the Elm port interface, and the JavaScript text manipulation handlers.

## Architecture Overview

```
 Elm Toolbar (Comments.elm)        Elm Ports (Ports.elm)         JavaScript (ports.js + bulma_drivers.js)
┌─────────────────────────┐    ┌───────────────────────┐    ┌──────────────────────────────────┐
│ viewCommentInputHeader  │───>│ Ports.richText        │───>│ RICH_TEXT handler                 │
│  - Heading, Bold, ...   │    │  outgoing port        │    │  - toggleMarkup() / pushLine()   │
│  - onClick dispatches   │    │  {action, data}       │    │  - dispatches 'input' event back │
│    OnRichText msg       │    │                       │    │    to Elm                         │
└─────────────────────────┘    └───────────────────────┘    └──────────────────────────────────┘
```

## 1. Toolbar (Comments.elm)

The toolbar is rendered by `viewCommentInputHeader` (line ~1408). It takes an `OpCommentHeader` record:

```elm
type alias OpCommentHeader msg =
    { onChangeViewMode : InputViewMode -> msg
    , onRichText : String -> String -> msg
    , onToggleMdHelp : String -> msg
    }
```

Each toolbar button triggers `op.onRichText targetid "Command"` where `targetid` is the DOM id of the target `<textarea>` and `"Command"` is a string like `"Bold"`, `"Heading"`, etc.

### Toolbar buttons

| Button | Command | JS function | Inserts/toggles |
|--------|---------|-------------|-----------------|
| H | `Heading` | `pushLine` | `### ` prefix |
| **B** | `Bold` | `toggleMarkup` | `**` wrap |
| *I* | `Italic` | `toggleMarkup` | `_` wrap |
| ~~S~~ | `Strikethrough` | `toggleMarkup` | `~~` wrap |
| Quote | `Quote` | `pushLine` | `> ` prefix |
| Link | `Link` | `toggleMarkup` | `[text](url)` wrap |
| List | `List-ul` | `pushLine` | `- ` prefix |
| Ordered | `List-ol` | `pushLine` | `1. ` prefix |
| Check | `List-check` | `pushLine` | `- [ ] ` prefix |
| @user | `MentionUser` | `pushLine` + tooltip | `@` prefix + user search |
| Tension | `MentionTension` | `pushLine` | `0x` prefix |
| Details | `Details` | `insertBlock` | `<details><summary>` block |

### Toolbar usage locations

The toolbar is used in multiple input contexts, each with a unique `targetid`:

- `"textAreaModal"` — new tension comment input
- `"commentInput"` — tension comment input
- `"commentContractInput"` — contract comment input
- `"updateCommentInput"` — update comment input
- `"draftInput"` — card panel draft input (in `CardPanel.elm`)

## 2. Elm Port Interface (Ports.elm)

The `richText` function sends a `RICH_TEXT` action through the generic `outgoing` port:

```elm
richText : String -> String -> Cmd msg
richText targetid command =
    outgoing
        { action = "RICH_TEXT"
        , data =
            JE.object
                [ ( "target", JE.string targetid )
                , ( "command", JE.string command )
                ]
        }
```

The `outgoing` port is a generic port that dispatches actions to JavaScript by `action` name.

## 3. JavaScript Handlers (ports.js)

The `RICH_TEXT` handler in `ports.js` (line ~676) dispatches to the appropriate JS function based on the command string:

```javascript
'RICH_TEXT': (app, session, msg) => {
    var $input = document.getElementById(msg.target);
    $input.focus();
    if (c == "Bold") toggleMarkup($input, "**")
    else if (c == "Heading") pushLine($input, "### ")
    // ... etc
    $input.dispatchEvent(new Event('input', { bubbles: true, cancelable: true }));
}
```

After text manipulation, an `input` event is dispatched so Elm picks up the changed textarea value through its normal `onInput` handler.

### `toggleMarkup(obj, mark, prefix, suffix)`

Used for **inline** markup (Bold, Italic, Strikethrough, Link). Behavior:

- If selected text is already wrapped in the mark → removes the mark (toggle off)
- If no selection → inserts the mark pair and positions cursor inside
- Handles leading/trailing whitespace in selections correctly
- For links: uses `prefix="["`, `suffix="()"` pattern

### `pushLine(obj, mark, isInline)`

Used for **line-prefix** markup (Heading, Quote, Lists, Mentions). Behavior:

- **Multiline selection**: prefixes each non-empty line with the mark
- **Single line, surrounded by whitespace**: prepends mark to current line
- **Single line, mid-paragraph**: creates a new section with `\n\n` padding
- `isInline=true` (Mentions): stays on the current line if it already starts with the same mark

### `insertBlock(obj, template, cursorOffset)`

Used for **block-level** templates (Details/Summary). Behavior:

- If text is selected → wraps the selection inside the template
- If no selection → inserts the full template block
- Positions cursor at the specified offset for immediate editing

## 4. Keyboard-Driven Rich Text (bulma_drivers.js)

The `markupRichText` function in `bulma_drivers.js` is attached as a `keydown` handler on all `.textarea` elements. It provides:

### Auto-continuation on Enter

- Unordered lists (`-`, `*`, `+`): continues with the same marker on the next line
- Ordered lists (`1. `): continues with incremented number
- Checkboxes (`- [ ] `, `* [ ] `, `+ [ ] `): continues with the same checkbox marker
- Blockquotes (`> `): continues with `> `
- Empty list marker: pressing Enter removes it (single newline, exits the list)

### Tab indentation

- Tab inserts 2-space indentation for list items
- Only activates when cursor is in a list context
- With a multi-line selection, Tab/Shift+Tab indents/dedents every selected line

### Backspace handling

- On an empty indented list item (≥2 spaces of indent): removes one indent level
- On an empty unindented list item: removes the marker entirely

### @ Mention tooltip

- When `@` is typed (with space, newline, or tab before — or at start-of-input), shows a user search dropdown
- Arrow keys, Tab, Enter navigate/select; Escape dismisses
- Sends patterns to Elm via `changePatternFromJs` port

### Programmatic edits and undo

All keyboard-driven mutations (Tab indent, list continuation, marker removal, nbsp normalisation) go through `replaceRange()` exported from `assets/js/textutils.js`. It preserves the browser's native Ctrl+Z stack via `document.execCommand('insertText', …)`, with a direct `el.value = …` fallback if that ever fails. The same helper backs `PUSH_INPUT_SELECTION` and `PUSH_EMOJI_SELECTION` in `ports.js`, so picker insertions are also undoable.

## 5. Checkbox Interaction (Markdown.elm ↔ bulma_drivers.js)

Rendered markdown checkboxes are interactive:

1. `Markdown.elm` renders checkboxes with class `checkbox_readonly`
2. `bulma_drivers.js` attaches click handlers on these elements
3. On click → sends `{ isChecked, position, cid }` to Elm via `checkboxFromJs` port
4. `Comments.elm` `OnCheckbox` handler calls `Markdown.setMdCheckbox` to toggle the checkbox in the source text
5. The modified comment is silently submitted via `SubmitCommentPatch`

## 6. Markdown Rendering (Markdown.elm)

`renderMarkdown` uses `elm-markdown` with a custom renderer (`frac6Renderer`) that supports:

- Internal vs. external link differentiation
- Interactive checkboxes (`checkbox_readonly` class)
- Custom HTML tags: `<i>`, `<u>`, `<span>`, `<div>`, `<details>`, `<summary>`
- Auto-linking: bare URLs, `@username` mentions, `0x...` tension references

## File Locations

| File | Role |
|------|------|
| `src/Components/Comments.elm` | Toolbar UI, message handling |
| `src/Ports.elm` | `richText` outgoing port |
| `src/Markdown.elm` | Markdown rendering, checkbox utility |
| `assets/js/ports.js` | `RICH_TEXT` handler, `toggleMarkup`, `pushLine`, `insertBlock`, picker insertions |
| `assets/js/bulma_drivers.js` | Keyboard shortcuts, auto-continuation, @ mentions, checkbox clicks |
| `assets/js/textutils.js` | `replaceRange` (undo-preserving textarea edit), `getCaretCoordinates` |
