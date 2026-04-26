# Search Panels

Search panels are dropdown components for looking up and (de)selecting entities
attached to a node or a tension: labels, projects, users. The panels share a
common shape — `OnOpen targets ...` opens the panel and triggers a fetch — but
the **scope** of that fetch depends on the call site.

Components:

- `src/Components/LabelSearchPanel.elm`
- `src/Components/ProjectSearchPanel.elm`
- `src/Components/UserSearchPanel.elm`

## Two scope rules

There are two — and only two — scope rules in use:

### 1. Path, non-recursive (focused-node search)

Used when the panel is attached to a focused node: tension side panel, card
panel, link-tension panel.

```
targets    = path nameids (root → focus, inclusive)
recursive  = false  -- only labels/projects declared on these exact nodes
```

The panel queries each `nameid` in `targets` directly (`nidsFilter`). Children
of those nodes are **not** included. Rationale: a label or project attachable
to a tension/card should be one declared on the focus or one of its ancestors
(which the focus inherits from).

Helper: `Bulk.getPath model.path_data |> List.map .nameid`.

### 2. Root, recursive (org-wide list filter)

Used when the panel filters a list at org level (tensions list, project page).

```
targets    = [ root nameid ]            -- e.g. model.node_focus.rootnameid
recursive  = true                       -- all descendants
```

The panel queries the root and walks down (`nidsDownFilter`). In practice this
covers every label declared anywhere in the organisation — which is the right
scope for filtering a global list.

## Call sites

| File | Caller | Targets | Recursive |
|------|--------|---------|-----------|
| `Org/Tension.elm` | `DoLabelEdit` | path | false |
| `Org/Tension.elm` | `DoProjectEdit` | path | (project panel is non-recursive only) |
| `Components/CardPanel.elm` | `DoLabelEdit` | path | false |
| `Components/LinkTensionPanel.elm` | label filter | path | false |
| `Org/Tensions.elm` | `ChangeLabel` | `[root]` | true |
| `Org/Project.elm` | label filter | `[root]` | true |

## API

`LabelSearchPanel.OnOpen : List String -> Bool -> Msg`

- `targets` — nameids to query.
- `Bool` — `True` recurse into children (`queryLabelsDown`); `False` query only the listed nodes (`queryLabels`).

`ProjectSearchPanel.OnOpen : List String -> Msg`

- One mode only: query the listed nodes, no recursion (`getOpenProjectsForPanel` → `nidsFilter`). Add a `Bool` argument here too if a recursive-from-root project filter is ever needed.

## Underlying queries

- `Query.QueryNode.queryLabels` — non-recursive label fetch (GQL `nidsFilter`).
- `Query.QueryNode.queryLabelsDown` — recursive-down label fetch (GQL `nidsDownFilter`).
- `Query.QueryNode.getOpenProjectsForPanel` — non-recursive open-project fetch (GQL `nidsFilter`).

`nidsFilter` matches `nameid in [...]`; `nidsDownFilter` builds a regexp that
matches each nameid and its descendants.

The REST `fetchLabelsTop` (recursive *up* to root) is no longer used by this
panel — it is still used by `Org/Settings.elm`.
