# Graphpack canvas

The interactive circle-packing chart of an organisation, shown on the Overview
page (`src/Org/Overview.elm`). It is a plain-canvas app living outside Elm:

- `assets/js/graphpack_d3.js` — the whole app (`GraphPack` object): layout, rendering, zooming, hit-testing.
- `assets/js/ports.js` — the Elm bridge (`*_GRAPHPACK` actions, one `GraphPack` instance per session).
- `src/Ports.elm` — Elm-side senders/subscriptions.

## Data flow

Elm sends the flat node list (`INIT_GRAPHPACK`). `formatGraph` nests it into a
tree (dropping Owner/Member special roles), `computeDepth` adds stats, then
`d3.hierarchy` + `packGraph` compute the layout (`resetGraphPack`). Other actions:
`FOCUS_GRAPHPACK` (zoom to a node), `DRAW_GRAPHPACK` / `REMOVEDRAW_GRAPHPACK`
(re-layout after node changes), `FLUSH_GRAPHPACK` (recolor, e.g. theme or user change).

### Sizing

`packGraph`/`packLayout` pack the tree bottom-up with `d3.packSiblings`, then scale
it to fit the canvas. A role's radius is `sqrt(nodeSize)` (weight decreasing with
depth); a circle encloses its packed children but never goes below
`minCircleRayon`, the footprint of two of its own roles. Empty, one-role and
two-role circles therefore share one baseline size, and a circle only grows when
its content needs the room. Siblings are packed circles first, then by descending
radius, with names and stable `nameid` as tie-breakers only: sibling order and
sizes are independent of the snapshot order and of renames.

### Update payload and identity

`DRAW_GRAPHPACK` receives `{ data, focusid, nodeRenames }`: a complete snapshot
and its **old nameid → new nameid** dictionary (empty for ordinary updates).
`TreeMenu` atomically reparents the confirmed move with `hotNodeMove`, carrying
that mapping through `Overview.OnUpdateTree`. Descendants follow parent links:
circle IDs are flat and unchanged; moving a role changes its ID. No API or blob
ID is used. Session data is installed before renamed-focus URL navigation.
Route focus IDs are normalized before layout cache checks and rename mapping.

Moves also refresh the authoritative tree: parent-dependent authorization and
aggregates cannot be inferred from the cache. Stale responses requery instead of
overwriting newer edits; refreshes carry no rename map. Unchanged layout inputs
update metadata without restarting motion. Token storage only flushes colors.
All additions, archives, restores and node edits redraw through `OnUpdateTree`.

## Rendering

Single visible canvas (`#canvasOrga`). The backing store is scaled by
`devicePixelRatio` (`sizeDom`), all drawing and hit-testing stay in CSS pixels
through the context transform. Each frame: `drawCanvas` →
`clearCanvas` + `drawCurrent`, which draws two zones split at the focused node:

- `drawOutside` — up to 2 ancestors and the focused node's siblings, dimmed.
- `drawInside` — the focused subtree, at most 3 levels deep.

`drawNode` projects pack coordinates to the canvas through `zoomCtx`
(`addNodeCtx`, cached on `node.ctx`); role circles are shrunk by type
(`nodeRayon`). Names, hover borders and the tooltip are drawn on top. Colors
come from CSS variables (`computeCircleColorRange`).

### Motion and lifecycle

One cancelable D3 timer eases layout and viewport over 400ms. Visible nodes and
ancestors interpolate `x/y/r`; entries grow, exits shrink/fade outside the live
hierarchy. The renderer retains old/new visible nodes during reparenting and
paints larger circles first. Interruptions resume from the displayed frame;
focus zooms use `interpolateZoom`. Reduced motion applies endpoints immediately.
Motion caches the old/new visible draw list; invisible nodes settle immediately.
Static zoom skips unchanged geometry and reuses radius order; changing radii
still sort each frame, including reflows and exits interrupted by zoom.

Hit-testing, dragging, keyboard navigation and tooltip actions pause during
motion; the focused tooltip returns on completion. External focus commands may
interrupt. Resize preserves the viewport; reverse uses the same transition path.
Same-canvas initialization preserves active motion, and redraw cancels stale
initialization. Canvas removal cancels listeners/timers. Empty or unusable
snapshots and different-org loading clear old data rather than leaving it active.

The canvas is resizable by dragging the grips around it (`#canvasResizer`
below, `#canvasResizerV` on the right, `#canvasResizerC` on the bottom-right
corner for both axes, wired by `bindResizer`): height goes to `userHeight`,
width to `userColWidth`, split between the two Bulma columns by `setColWidth`.
Both reset on page reload. The `#overview` row is `Html.Keyed` so Elm drops
those JS inline styles when leaving the page.

## Hit-testing

`getNodeUnderPointer` is purely geometric: it inverts the `zoomCtx` transform
and walks the hierarchy downward (`nodeContains`), mirroring the render rules —
at most 3 levels below the focus inside its subtree, down to the siblings
outside.

Do not replace this with hidden-canvas pixel picking (`getImageData`): browsers
with fingerprinting protection (Brave, Firefox `resistFingerprinting`) add
noise to canvas readbacks, which silently breaks color-keyed lookups.

## Interaction

Pointer handlers on the canvas (registered in `init`; `pointer*` events so mouse,
touch and pen share one code path, with `touch-action: pinch-zoom` so
single-finger moves are not eaten by page scrolling) resolve the node under the
pointer and notify Elm through ports: `nodeClickedFromJs` (navigate),
`nodeHoveredFromJs` (tooltip), `nodeFocusedFromJs`, `nodeLeftClickedFromJs` /
`nodeRightClickedFromJs` (tooltip actions / context menu). The tooltip
(`#nodeTooltip`) is an Elm-rendered element positioned by JS. Node switches keep
it visible and replace its title/position after Elm renders the options, using a
cancelable animation frame; pending actions stay inert. Visible replacements glide
between nodes with explicit position transitions, unchanged on hover. Hidden
placement and reduced-motion preferences skip the animation. Hover handoff follows
the tooltip's current rectangle and its connecting gap, including outside the canvas. `isFrozen`
locks the target while the tooltip's `ActionPanel` menu is open. The
menu renders inside the trigger's `#domid` (`Overview.viewActionPanel`), so picking
an item is not an outside click; the lock is only released by `ActionPanel.OnClose`
(`CLEAR_CONTEXT_MENU`): click outside, click on the tooltip, or the item's
modal / move closing.

A freshly created organisation shows `#welcomeCards` (action cards for new
tension, new project, circle, role) above the canvas. Once the root has other
nodes, the cards hide; a grid icon in `#canvasButtons` toggles them. The project
card goes to `/p/{nameid}?new=1`, which `Org.Projects` reads to open the create form.

The canvas is focusable (`tabindex=0`) and `canvasKeyDownEvent` navigates from
the keyboard: ←/→ cycle siblings, ↓/Enter dive into the first child, ↑/Esc/
Backspace go to the parent, Home goes to the root. Modifier combos are left to
the browser.

## Drag-and-drop move

Dragging a node onto another circle moves it there. Navigation fires on
`mouseup` so a press can become a drag: past `dragThreshold` px the drag arms
(`drawDragFeedback`), and the drop sends `nodeDraggedFromJs [source, target]`.
`getDropTarget` rejects roles, the current parent and the dragged node's own
subtree.

Elm side (`Org/Overview.elm`, `NodeDragged`): gated by `getNodeRights`, then
`ActionPanel.OnMoveTo domid tid target` opens `MoveTension` with the target
pre-selected. `domid` must name a rendered panel, as `ActionPanel.view` only
renders the move modal for its own `domid`.

## Tests

`tests/Js/graphpackHitTest.test.js` covers the geometric hit-testing (depth
caps, sibling stop level, role factors, transform inversion).
`tests/Js/graphpackPorts.test.js` exercises the real canvas renderer with controlled
D3 timers: deterministic packing, encoded focus and move mapping, tooltip placement,
endpoints, interruptions, exits, reduced motion, interaction pauses, resize/reverse,
initialization races, cleanup, and recolor-only refreshes. Hover checks cover
frame-coalesced replacement, action gating, node entry, tooltip handoff and menu freezing.
`tests/Elm/GraphTest.elm` covers subtree discovery, atomic moves, snapshot-scoped
rename payloads, authoritative reconciliation, drag-modal activation and the tooltip menu nesting.
