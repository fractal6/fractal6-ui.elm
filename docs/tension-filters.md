# Tension list filters

The tension list (`src/Org/Tensions.elm`) drives every filter from the URL query
string: each filter has an encoder/decoder pair, `SubmitSearch` writes them back
into the URL, and `init` reads them from `session.common.query`. `OnClearFilter`
rebuilds the URL keeping only `v` and `sort`.

| Param  | Filter        | Default  |
|--------|---------------|----------|
| `q`    | text pattern  | –        |
| `v`    | view mode     | `list`   |
| `s`    | status        | `open`   |
| `t`    | tension type  | `all`    |
| `org`  | org filter    | –        |
| `d`    | depth         | `all`    |
| `sort` | sort          | `newest` |
| `u`    | authors       | –        |
| `l`    | labels        | –        |

## Text pattern (`q=`)

The pattern is parsed by `Utils.String.parseSearchPattern`: `"quoted"` (or
`'quoted'`) spans require **all** their words to match (`alloftext`), the
unquoted remainder matches **any** word (`anyoftext`); clauses are ANDed, each
matching title OR message. The split happens client-side everywhere:

- GraphQL `queryTension` builds the filter directly (`searchFilters` in
  `Query/QueryTension.elm`);
- REST `/q/tensions/*` receives `pattern` + `pattern_exact` (split in
  `tensionQueryEncoder`), assembled server-side by `db.SearchTextFilter`.

Project search (`Org/Projects.elm`, link-tension panel) applies the same rule
on `Project.name` via `searchNameFilter` in `Query/QueryNode.elm`. The journal
search does not support quoted spans.

## Org filters (`org=`)

Values: `open_roles`, `roles`, `circles`, `archived_roles`, `archived_circles`.

They list the **governance tension behind the org nodes** of the focused
subtree rather than filtering tensions by type. `open_roles` means a role with no
member linked (`not has: first_link`).

A `TensionFilter` cannot reach node properties, so these run their own GraphQL
query — `Query.QueryTension.queryOrgTensions`. It queries `Node` (scoped by a
`nameid` prefix regexp, filtered on `type_` / `isArchived` / `first_link`)
and walks back to the tension through `Node.source` (the published blob) →
`Blob.tension`. Nodes in `membershipRoleTypes` are excluded: the `@username`
membership nodes and the Owner role are created without a governance tension,
so they have no `source` blob and would silently shorten a page. Ordering and paging
therefore happen on the node: `orgNodesOrder` maps `newest`/`oldest` onto
`Node.createdAt` and `activity` onto `Node.updatedAt`.

An org filter and the type filter are mutually exclusive — picking one resets
the other. The remaining filters (labels, authors, text pattern) do not apply.
Governance tensions are auto-closed on creation (see `node-governance.md`), so
the status control is hidden and the header shows `N <filter name>` (loaded list
length) instead of the open/closed counters.

The selector is behind a discreet **More filters** control in two places:

- `viewCatMenu` (left type list) — accordion expand
- type dropdown of `viewSearchBar` — horizontal slide to a second pane
