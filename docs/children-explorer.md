# Resources Explorer

## Overview

The Resources Explorer is a recursive expandable accordion displayed below the NodeDoc box in the Overview page (`src/Org/Overview.elm`). It allows users to browse the hierarchy of children nodes inline — previewing about text, mandate, and sub-children — without navigating away from the current focus.

## Usage

- When a circle has children, a "Resources" section appears below the main document box.
- Click on any child row to expand it and see its about text and mandate.
- If the expanded child is a circle with its own children, those appear nested below with a left border for visual hierarchy.
- Click again to collapse.
- The children list automatically resets when the focus changes to a different node.

## Implementation Details

### Model fields
- `children_expanded : Set String` — tracks which nodes are expanded (by nameid), works at any depth
- `children_data : Dict String (GqlData NodeData)` — lazy-loaded about/mandate per node

### Messages
- `ToggleChildExpand String` — toggle expand/collapse for any node at any depth
- `GotChildData String (GqlData NodeData)` — receive lazy-loaded node data

### View functions
- `viewChildrenExplorer` — entry point, renders the "Resources" box for the focused node
- `viewChildrenList` — renders a list of children (circles first, then roles), parameterized by depth
- `viewChildRow` — renders a single row with chevron, icon, name, color dot, first link; indented by depth with left border
- `viewChildExpandedContent` — renders about/mandate content, then recursively calls `viewChildrenList` for sub-children

### Helper
- `getChildrenFromTree` — extracts direct children of a nameid from `tree_data` (NodesDict) as `List EmitterOrReceiver`

### Data flow
1. Top-level children come from `model.path_data.focus.children`
2. Sub-children are computed from `model.tree_data` via `getChildrenFromTree`
3. On expand, if data not cached, fires `fetchNodeData` for that node's nameid
4. Result stored in `children_data` dict for caching (works for any depth)
5. On focus change (`NodeFocused`), both `children_expanded` and `children_data` are reset

### Visual hierarchy
- Each depth level adds 20px left margin and a `2px solid var(--border-light)` left border
- Circles show chevron icons (expandable); roles show a dash icon (leaf nodes)
