# Graphpack canvas

The interactive circle-packing chart of an organisation, shown on the Overview
page (`src/Org/Overview.elm`). It is a plain-canvas app living outside Elm:

- `assets/js/graphpack_d3.js` — the whole app (`GraphPack` object): layout, rendering, zooming, hit-testing.
- `assets/js/ports.js` — the Elm bridge (`*_GRAPHPACK` actions, one `GraphPack` instance per session).
- `src/Ports.elm` — Elm-side senders/subscriptions.

## Data flow

Elm sends the flat node list (`INIT_GRAPHPACK`). `formatGraph` nests it into a
tree (dropping Owner/Member special roles), `computeDepth` adds stats and
invisible "Hidden" filler children so lone circles pack nicely, then
`d3.hierarchy` + `d3.pack` compute the layout (`resetGraphPack`). Other actions:
`FOCUS_GRAPHPACK` (zoom to a node), `DRAW_GRAPHPACK` / `REMOVEDRAW_GRAPHPACK`
(re-layout after node changes), `FLUSH_GRAPHPACK` (recolor, e.g. theme change).

## Rendering

Single visible canvas (`#canvasOrga`). Each frame: `drawCanvas` →
`clearCanvas` + `drawCurrent`, which draws two zones split at the focused node:

- `drawOutside` — up to 2 ancestors and the focused node's siblings, dimmed.
- `drawInside` — the focused subtree, at most 3 levels deep.

`drawNode` projects pack coordinates to the canvas through `zoomCtx`
(`addNodeCtx`, cached on `node.ctx`); role circles are shrunk by type
(`nodeRayon`). Names, hover borders and the tooltip are drawn on top. Colors
come from CSS variables (`computeCircleColorRange`).

Zooming (`zoomToNode`) runs `d3.interpolateZoom` in a `d3.timer` that mutates
`zoomCtx` and redraws every frame.

## Hit-testing

`getNodeUnderPointer` is purely geometric: it inverts the `zoomCtx` transform
and walks the hierarchy downward (`nodeContains`), mirroring the render rules —
at most 3 levels below the focus inside its subtree, down to the siblings
outside, skipping Hidden fillers.

Do not replace this with hidden-canvas pixel picking (`getImageData`): browsers
with fingerprinting protection (Brave, Firefox `resistFingerprinting`) add
noise to canvas readbacks, which silently breaks color-keyed lookups.

## Interaction

Mouse handlers on the canvas (registered in `init`) resolve the node under the
pointer and notify Elm through ports: `nodeClickedFromJs` (navigate),
`nodeHoveredFromJs` (tooltip), `nodeFocusedFromJs`, `nodeLeftClickedFromJs` /
`nodeRightClickedFromJs` (tooltip actions / context menu). The tooltip
(`#nodeTooltip`) is an Elm-rendered element positioned by JS; `isFrozen` /
`isFrozenMenu` pause hover updates while it is open.

## Tests

`tests/Js/graphpackHitTest.test.js` covers the geometric hit-testing (depth
caps, sibling stop level, role factors, transform inversion).
