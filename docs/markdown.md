# Markdown Rich-Text Editing

The formatting toolbar above comment inputs. Three layers: the Elm toolbar, the `RICH_TEXT`
port, and the JS text manipulation handlers.

1. `viewCommentInputHeader` in `src/Components/Comments.elm` renders the buttons; each one
   dispatches `op.onRichText targetid command`, where `targetid` is the DOM id of the target
   `<textarea>` (`commentInput`, `textAreaModal`, `draftInput`, …).
2. `Ports.richText` sends `{ action = "RICH_TEXT", target, command }` out.
3. The `RICH_TEXT` handler in `assets/js/ports.js` mutates the textarea, then dispatches an
   `input` event so Elm picks the new value up through its normal `onInput`.

## Commands

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

The three JS primitives (`assets/js/ports.js`):

- `toggleMarkup(obj, mark, prefix, suffix)` — inline markup. Wraps the selection, unwraps it
  if already wrapped, or inserts the pair with the cursor inside when there is no selection.
- `pushLine(obj, mark, isInline)` — line-prefix markup. Prefixes every non-empty line of a
  multiline selection; otherwise prepends to the current line, opening a new `\n\n` section if
  the cursor sits mid-paragraph. `isInline` (mentions) keeps the cursor on the current line.
- `insertBlock(obj, template, cursorOffset)` — block template, wrapping the selection if any.

## Keyboard editing

Auto-continuation of lists and quotes, Tab indentation, `@`/`:` pickers and the
undo-preserving `replaceRange` helper live in the `markupRichText` keydown handler.
See `docs/markup-richtext.md`.

## Rendering and checkboxes

`renderMarkdown` in `src/Markdown.elm` uses `elm-markdown` with the custom `frac6Renderer`:
internal vs external links, a limited set of raw HTML tags (`<i> <u> <span> <div> <details>
<summary> <a>`), auto-linking of bare URLs, `@username` and `0x…` tension references, and
checkboxes rendered with the `checkbox_readonly` class.

### Link attributes

`[Link](/path){target="_blank" title="More details"}` is supported. `expandLinkAttributes`
(preprocessing, in `frac6Parser`) rewrites such links into an inline `<a>` tag handled by the
renderer — elm-markdown has no way to carry extra attributes through its link AST node. Only
`target`, `title`, `class` and `rel` are kept; anything else is dropped and a block with no
valid attribute leaves the link untouched.

Those checkboxes are clickable: `bulma_drivers.js` catches the click and sends
`{ isChecked, position, cid }` over the `checkboxFromJs` port; `Comments.elm` toggles the
source text with `Markdown.setMdCheckbox` and silently re-submits the comment.

### Triple click to edit

Ctrl (or Cmd) + double clicking a comment body (author only) opens the editor with the caret
at the clicked line: `Dom.onCtrlDblClick` reads the rendered text, `findAnchorPos`
(`assets/js/textutils.js`) matches it back to a source line. Being also a text selection
gesture, it is ignored when the shared comment form is busy (checkbox submit, other comment
being edited).

## Files

| File | Role |
|------|------|
| `src/Components/Comments.elm` | Toolbar UI, message handling |
| `src/Ports.elm` | `richText` outgoing port |
| `src/Markdown.elm` | Rendering, checkbox utility |
| `assets/js/ports.js` | `RICH_TEXT` handler, `toggleMarkup`, `pushLine`, `insertBlock` |
| `assets/js/bulma_drivers.js` | Keyboard handling, checkbox clicks |
| `assets/js/textutils.js` | `replaceRange`, `getCaretCoordinates`, `caretAtAnchor` |
