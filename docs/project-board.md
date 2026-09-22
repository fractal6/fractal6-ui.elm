# Project Board

The kanban board of a project: `src/Components/Board.elm`, mounted by `src/Org/Project.elm`.
Columns and cards live in `model.project : ProjectData`, updated optimistically; the mutation
result is tracked silently in `model.board_result`.

Related: `project-permissions.md` (who can edit), `project-collaborators.md` (settings panel),
`project-templates.md` (predefined column layouts).

## Features

- Columns and cards are HTML5-draggable (`moveProjectColumn` / `moveProjectCard`). The backend
  shifts the siblings positions, so the front only sends the target position.
- Each column ends with a `kb-drop-zone`, sticky at the bottom while dragging so the last position
  stays reachable when cards overflow; it doubles as the placeholder when hovered.
- Column menu (admin) and card menu (ellipsis) for the column/card actions; the bulk ones act on
  the current selection.
- Inline draft creation in a column (first line is the title, the rest the message), convertible
  to a tension.

## Card selection

`model.activeCards : List String`, rendered with `is-focusing`. Click selects one card,
Ctrl/Cmd+click toggles, "select all" in the column menu adds the column's visible cards, a click
outside clears. The clearing mouseup subscription is guarded by `Dom.withoutModifier` (ctrl+click
extends) and `Dom.outsideClickCloseBy keepsSelection` (menus acting on the selection don't clear
it).

Dragging a card of a 2+ selection moves the whole selection to the drop target in board order
(left-to-right, top-to-bottom), skipping cards already in the target column. Moves are sent one
at a time through `model.cardMoveQueue` (drained by `GotCardMoved`, concurrent drops are
appended) since each move shifts positions server-side.
