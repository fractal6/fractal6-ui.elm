# Resources Explorer

A recursive accordion under the NodeDoc box on the Overview page (`src/Org/Overview.elm`).
It lets users browse a circle's children inline — about text, mandate, sub-children — without
leaving the current focus. It only shows up when the focused node has children.

Circles get a chevron and expand; roles are leaves. Each depth level indents by 20px with a
left border.

## Implementation

Two model fields: `children_expanded : Set String` (nameids, any depth) and
`children_data : Dict String (GqlData NodeData)` (lazily fetched about/mandate). Both reset on
`NodeFocused`.

The views mirror the recursion: `viewChildrenExplorer` (entry) → `viewChildrenList` (circles
first, then roles, parameterized by depth) → `viewChildRow` → `viewChildExpandedContent`,
which calls back into `viewChildrenList` for sub-children.

Top-level children come from `model.path_data.focus.children`, deeper ones from
`model.tree_data` through `getChildrenFromTree`. Expanding a node with no cached data fires
`fetchNodeData`; the result is cached in `children_data` (`ToggleChildExpand` /
`GotChildData`).
