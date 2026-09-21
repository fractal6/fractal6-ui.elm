# Graphpack canvas

The interactive circle-packing chart of an organisation, shown on the Overview
page (`src/Org/Overview.elm`). It is a plain-canvas app living outside Elm:

- `assets/js/graphpack_d3.js` — the whole app (`GraphPack` object): layout, rendering, motion, hit-testing.
- `assets/js/ports.js` — the Elm bridge (`*_GRAPHPACK` actions, one `GraphPack` instance per session).
- `src/Ports.elm` — Elm-side senders/subscriptions.

## Data flow

Elm sends a flat node list (`INIT_GRAPHPACK`). `formatGraph` nests it into a tree
(dropping Owner/Member special roles), `computeDepth` adds stats, then
`d3.hierarchy` + `packGraph` compute the layout (`resetGraphPack`). Other actions:
`FOCUS_GRAPHPACK` (zoom to a node), `DRAW_GRAPHPACK` / `REMOVEDRAW_GRAPHPACK`
(re-layout after node changes), `FLUSH_GRAPHPACK` (recolor only, e.g. theme change).

### Sizing

`packGraph`/`packLayout` pack the tree bottom-up with `d3.packSiblings`, then scale it
to fit the canvas. Circles have a minimum size (`minCircleRayon`) so small ones stay
comparable, and sibling order is deterministic — independent of snapshot order and of
renames.

### Update payload and identity

`DRAW_GRAPHPACK` receives `{ data, focusid, nodeRenames }`: a complete snapshot plus
an **old nameid → new nameid** map (empty for ordinary updates). Moving a role changes
its ID; circle IDs are flat and unchanged, descendants follow parent links. `TreeMenu`
reparents the confirmed move with `hotNodeMove` and carries the map through
`Overview.OnUpdateTree`, which is also the path for additions, archives, restores and
edits. Moves are applied locally only: `hotNodeMove` also shifts `n_open_tensions`
(−1 old parent, +1 new parent) and the token is refreshed only when the move renames
the node (a role), which is the sole rights-impacting change.

## Rendering

Single visible canvas (`#canvasOrga`). The backing store is scaled by
`devicePixelRatio` (`sizeDom`); drawing and hit-testing stay in CSS pixels through the
context transform. Each frame `drawCanvas` clears and draws two zones split at the
focused node:

- `drawOutside` — up to 2 ancestors and the focused node's siblings, dimmed.
- `drawInside` — the focused subtree, at most 3 levels deep.

`drawNode` projects pack coordinates through `zoomCtx`; names, hover borders and the
tooltip are drawn on top. Colors come from CSS variables (`computeCircleColorRange`).

### Motion and lifecycle

One cancelable D3 timer eases both layout and viewport; interruptions resume from the
displayed frame and reduced motion applies endpoints immediately. Interaction
(hit-testing, dragging, keyboard, tooltip actions) is paused during motion. Canvas
removal cancels listeners and timers; empty, unusable or different-org snapshots clear
the old data.

The canvas is resizable by dragging the grips around it (`bindResizer`), width being
split between the two Bulma columns. Sizes are not persisted. The `#overview` row is
`Html.Keyed` so Elm drops those JS inline styles when leaving the page.

## Hit-testing

`getNodeUnderPointer` is purely geometric: it inverts the `zoomCtx` transform and walks
the hierarchy downward (`nodeContains`), mirroring the render rules.

Do not replace this with hidden-canvas pixel picking (`getImageData`): browsers with
fingerprinting protection (Brave, Firefox `resistFingerprinting`) add noise to canvas
readbacks, which silently breaks color-keyed lookups.

## Interaction

Pointer handlers registered in `init` (`pointer*` events so mouse, touch and pen share
one path) resolve the node under the pointer and notify Elm through the
`node*FromJs` ports: navigate, hover tooltip, focus, tooltip actions, context menu.

The tooltip (`#nodeTooltip`) is an Elm-rendered element positioned by JS; switching
node keeps it visible and re-places it after Elm renders the options. `isFrozen` locks
the target while the tooltip's `ActionPanel` menu is open — the menu renders inside the
trigger's `#domid` (`Overview.viewActionPanel`), so picking an item is not an outside
click, and the lock is released only by `ActionPanel.OnClose` (`CLEAR_CONTEXT_MENU`).

Onboarding action cards (new tension, project, circle, role) advertise what can be done
in the focused node, in two mutually exclusive places:

- `#canvasCards` — on a fresh orga (root circle with nothing packed inside,
  `Graph.isFreshOrga`), the cards occupy its quarters. `placeCanvasCards` sizes the 2x2
  box to the circle and hides it during zoom or when too small.
- `#welcomeCards` — above the canvas otherwise, toggled by the grid icon in
  `#canvasButtons`.

Both target the focused node; the project card goes to `/p/{nameid}?new=1`, which
`Org.Projects` reads to open the create form. See the shared
[ActionCard component](action-cards.md).

The canvas is focusable (`tabindex=0`) and `canvasKeyDownEvent` navigates: ←/→ cycle
siblings, ↓/Enter dive into the first child, ↑/Esc/Backspace go to the parent, Home goes
to the root. Modifier combos are left to the browser.

## Drag-and-drop move

Dragging a node onto another circle moves it there. Navigation fires on `mouseup` so a
press can become a drag; the drop sends `nodeDraggedFromJs [source, target]`.
`getDropTarget` rejects roles, the current parent and the dragged node's own subtree.

Elm side (`Org/Overview.elm`, `NodeDragged`): gated by `getNodeRights`, then
`ActionPanel.OnMoveTo domid tid target` opens `MoveTension` with the target
pre-selected. `domid` must name a rendered panel, as `ActionPanel.view` only renders the
move modal for its own `domid`.

## Tests

- `tests/Js/graphpackHitTest.test.js` — geometric hit-testing (depth caps, sibling stop level, role factors, transform inversion).
- `tests/Js/graphpackPorts.test.js` — the real canvas renderer with controlled D3 timers: packing, focus/move mapping, motion, tooltip, resize, cleanup, cards placement.
- `tests/Elm/GraphTest.elm` — subtree discovery, atomic moves, rename payloads, counter shift, drag-modal activation, tooltip menu nesting.
- `tests/Elm/ShapesTest.elm` — quarter-disc path geometry.
