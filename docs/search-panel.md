# Search Panels

Search panels are dropdown components for looking up and (de)selecting entities
attached to a node or a tension: labels, projects, users. The panels share a
common shape — `OnOpen targets ...` opens the panel and triggers a fetch — but
the **scope** of that fetch depends on the call site.

Components:

- `src/Components/LabelSearchPanel.elm` — also owns `viewLabels` / `viewLabel`
- `src/Components/ProjectSearchPanel.elm` — also owns `viewCards`, `viewProjectColumnTag`, the tension card list and column-move dropdown
- `src/Components/UserSearchPanel.elm` — also owns `viewUsers` / `viewUser`

## Scope rules

Three scope rules are in use, depending on the panel and call site:

### 1. Path + focus children, non-recursive (label / project — focused-node search)

Used when the label or project panel is attached to a focused node: tension
side panel, card panel, link-tension panel, new-tension modal.

```
targets    = path nameids (root → focus, inclusive) ++ direct children of focus
recursive  = false  -- only labels/projects declared on these exact nodes
```

The panel queries each `nameid` in `targets` directly (`nidsFilter`, indexed
`in_` lookup). Deeper descendants are **not** included. Rationale: a label or
project attachable to a tension/card should be one declared on the focus, one
of its ancestors (which the focus inherits from), or a direct subcircle.

Helper: `Bulk.getPathWithChildren model.path_data`.

### 2. Root, members-only (assignee selection)

Used by the user/assignee panel anywhere it appears. Members of an
organisation are special role nodes attached directly under the root circle,
so assignee lookup is always **org-wide**.

```
targets    = [ root nameid ]    -- e.g. model.node_focus.rootnameid
```

`UserSearchPanel.OnOpen` calls `queryMembers`, which extracts the rootid from
the last nameid (`nid2rootid`) and filters `rootnameid == rootid` plus an
`activeMembershipRoleTypes` match. Path / focus / children are irrelevant —
the panel proposes every active member of the organisation. We pass
`[ rootnameid ]` rather than the path to make the scope explicit at call
sites.

### 3. Root, recursive (org-wide label list filter)

Used when the label panel filters a list at org level (tensions list, project
page).

```
targets    = [ root nameid ]
recursive  = true                       -- all descendants
```

The panel queries the root and walks down (`nidsDownFilter`). In practice this
covers every label declared anywhere in the organisation — which is the right
scope for filtering a global list.

## Call sites

| File | Caller | Targets | Recursive |
|------|--------|---------|-----------|
| `Org/Tension.elm` | `DoLabelEdit` | path + focus children | false |
| `Org/Tension.elm` | `DoAssigneeEdit` | `[rootnameid]` | n/a (rule 2) |
| `Org/Tension.elm` | `DoProjectEdit` | path + focus children | (project panel is non-recursive only) |
| `Components/CardPanel.elm` | `DoLabelEdit` | path + focus children | false |
| `Components/CardPanel.elm` | `DoAssigneeEdit` | `[rootnameid]` | n/a (rule 2) |
| `Components/LinkTensionPanel.elm` | label filter | path + focus children | false |
| `Form/NewTension.elm` | label / project pickers | path + focus children | false |
| `Form/NewTension.elm` | assignee picker | `[rootnameid]` | n/a (rule 2) |
| `Org/Tensions.elm` | `ChangeLabel` | `[root]` | true |
| `Org/Project.elm` | label filter | `[root]` | true |

## New tension modal (`Form/NewTension.elm`)

The new tension modal hosts three pickers in this order: **assignees**,
**labels**, **projects**. Labels and projects follow rule 1 (path + focus
children, non-recursive); assignees follow rule 2 (org-wide,
`[rootnameid]`).

### Project linkage during tension creation

The tension does not exist yet when the project panel opens, so the panel runs
in `SelectProject` mode (see `Components/ProjectSearchPanel.elm` —
`OnProjectAdd` branches on `model.action`). Selections are tracked in
`model.selectedProjects : List TensionProject` without firing any mutation.

Once the tension is created (`OnTensionAck`'s `OkAuth` branch), one
`addProjectCard` cmd is fired per selected project, **in parallel** with the
modal's close cmd. The user perceives instant tension creation; project cards
are created in the background. Failures are logged via `Ports.logErr` and do
not block the UX. The default column for each project is its
`NoStatusColumn` (or the lowest-`pos` column if absent) — the column choice is
captured at selection time and reused at submit time.

## API

`LabelSearchPanel.OnOpen : List String -> Bool -> Msg`

- `targets` — nameids to query.
- `Bool` — `True` recurse into children (`queryLabelsDown`); `False` query only the listed nodes (`queryLabels`).

`ProjectSearchPanel.OnOpen : List String -> Msg`

- One mode only: query the listed nodes, no recursion (`getOpenProjectsForPanel` → `nidsFilter`). Add a `Bool` argument here too if a recursive-from-root project filter is ever needed.

`ProjectSearchPanelOnClickAction` (in `src/Session.elm`):

- `AssignProject` — fire `addProjectCard` immediately on selection (e.g. tension side panel where the tension already exists).
- `SelectProject` — track the selection only and emit it via `Out`; no mutation. Used by the new tension modal where the tension does not exist yet.

## Underlying queries

- `Query.QueryNode.queryLabels` — non-recursive label fetch (GQL `nidsFilter`).
- `Query.QueryNode.queryLabelsDown` — recursive-down label fetch (GQL `nidsDownFilter`).
- `Query.QueryNode.getOpenProjectsForPanel` — non-recursive open-project fetch (GQL `nidsFilter`).
- `Query.QueryNode.queryMembers` — org-wide member fetch. Takes a list of nameids but only the last one is used: `nid2rootid` extracts the rootid, and the GQL filter is `rootnameid == rootid` + active membership role types.

`nidsFilter` matches `nameid in [...]`; `nidsDownFilter` builds a regexp that
matches each nameid and its descendants.

The REST `fetchLabelsTop` (recursive *up* to root) is no longer used by this
panel — it is still used by `Org/Settings.elm`.
