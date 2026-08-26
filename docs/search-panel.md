# Search Panels

Dropdowns for looking up and (de)selecting entities attached to a node or tension: labels,
projects, users. They share the same shape — `OnOpen targets …` opens the panel and triggers
a fetch — but the **scope** of that fetch depends on the call site.

- `src/Components/LabelSearchPanel.elm` — also owns `viewLabels` / `viewLabel`
- `src/Components/ProjectSearchPanel.elm` — also owns `viewCards`, `viewProjectColumnTag`, the tension card list and column-move dropdown
- `src/Components/UserSearchPanel.elm` — also owns `viewUsers` / `viewUser`

## Scope rules

**1. Path + focus children, non-recursive** — label and project panels attached to a focused
node (tension side panel, card panel, link-tension panel, new-tension modal). Targets are the
path nameids (root → focus) plus the direct children of the focus, from
`Fractale.Graph.getPathWithChildren`, queried directly with no recursion. An attachable label
or project must be declared on the focus, an ancestor it inherits from, or a direct subcircle.

**2. Root, members-only** — the user/assignee panel, everywhere. Members are role nodes under
the root circle, so lookup is always org-wide: pass `[ rootnameid ]` (explicit at the call
site) and `queryMembers` filters on the rootid plus active membership role types.

**3. Root, recursive** — the label panel used as a filter over an org-level list (tensions
list, project page). Targets `[ rootnameid ]` and walks all descendants, i.e. every label in
the organisation.

| Call site | Panel | Rule |
|-----------|-------|------|
| `Org/Tension.elm`, `Components/CardPanel.elm`, `Components/LinkTensionPanel.elm`, `Form/NewTension.elm` | label, project | 1 |
| same files | assignee | 2 |
| `Org/Tensions.elm`, `Org/Project.elm` | label filter | 3 |

## API

- `LabelSearchPanel.OnOpen : List String -> Bool -> Msg` — the `Bool` recurses into children
  (`queryLabelsDown`) instead of querying only the listed nodes (`queryLabels`).
- `ProjectSearchPanel.OnOpen : List String -> Msg` — non-recursive only
  (`getOpenProjectsForPanel`). Add a `Bool` if a recursive project filter is ever needed.
- `ProjectSearchPanelOnClickAction` (`src/Session.elm`) — `AssignProject` fires
  `addProjectCard` on selection (the tension already exists); `SelectProject` only records the
  selection and emits it through `Out`.

Queries live in `Query.QueryNode`. `nidsFilter` matches `nameid in [...]`; `nidsDownFilter`
builds a regexp matching each nameid and its descendants.

## Project linkage during tension creation

The new tension modal hosts assignees, labels and projects. The tension doesn't exist yet when
the project panel opens, so it runs in `SelectProject` mode and selections accumulate in
`model.selectedProjects` without any mutation. On `OnTensionAck`/`OkAuth` one `addProjectCard`
is fired per selection, in parallel with closing the modal — creation feels instant, cards are
created in the background, failures only reach `Ports.logErr`. The target column is captured
at selection time (the project's `NoStatusColumn`, or its lowest-`pos` column).
