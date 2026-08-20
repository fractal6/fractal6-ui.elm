# Node Governance (derived state)

A governance tension governs at most one Node. The backend owns the relation
(`Tension.governed_node`); the frontend derives everything from it — there is
no persisted action state.

## How it works

A tension's Node state is derived, never stored: a governed Node with
`isArchived` (or `isRootArchived` for a root) gives `Active`/`Archived`; before publication the latest blob
fragment gives `Draft`; ordinary tensions have none. `receiver` is the parent
circle, never the governed Node. Publish/archive/unarchive are tension events,
and status tags are hidden for governance tensions since they are auto-closed
on creation. The receiver codec is still used to build identity for drafts and
the NewTension preview, where no Node exists yet.

## Root archiving

Archiving an **organisation** (root node) is a flat flag, `Node.isRootArchived`, deliberately not
the `isArchived` used for circles and roles: it skips the hierarchy rules that come with the latter
(a circle cannot be archived while it has children, a node cannot be unarchived under an archived
parent) and the first-link unlink. Nothing is deleted, children and tensions are untouched, and the
action is reversible. It is written through the same `BlobArchived` /
`BlobUnarchived` tension events; the backend branches on the root and flips `isRootArchived`.

`getTensionNode` reports `Archived` when either flag is set. In the ActionPanel the root's menu state
comes from the node's own flag (`Op.lifecycle` only ever tracks `isArchived`) and success patches the
tree via `DoUpdateNode` instead of `DoDelNodes`. The breadcrumb badge (`Components/HelperBar.elm`)
and the profile "active | archived" tabs (`src/User/Profile.elm`, see `docs/orga-search-profile.md`)
read the flag through `Fractale.Graph.isRootArchivedOn`, which prefers the tree (patched in place, so
the badge flips without a refetch) and falls back to the path. The Explore listing
(`queryPublicOrga`) excludes archived orgs in the GraphQL filter.

## Recursive archive

Archiving a **circle** archives its whole subtree, and optionally closes every open tension attached
to it (checkbox in the ActionPanel archive modal, sent as the `BlobArchived` event `new` field:
`"true"` or `""`). The checkbox is shown for roles too (a role has its own tensions), not for a root
(flat flag, children untouched).

The backend requires authority on **every descendant circle** before archiving anything. The modal
probes it with `hasSubtreeAuthority` (`POST /q/nodes/subauth` → bare bool, `src/Requests.elm`): only
`Success True` renders the hint + checkbox and enables the confirm button (`isSendable`), anything
else (pending, failed, denied) shows an authorization message and keeps the action disabled —
fail-closed. The probe is advisory only; the backend stays the gate, so its error (which names the
blocking circle) is still rendered in the modal footer. Roles and roots skip the round trip.

## Revisions

The document history is shown under the Tension page **Revisions** tab
(`…/action?v=history`, `NodeView = NodeVersions`). Each revision row
(`Components/NodeDoc.elm` `viewVersions`) has an inline eye button toggling a
split diff against the previous revision. Revision deletion (via a
`TensionEvent.BlobDeleted` event proxy) is planned but not implemented yet.

## Where things live

- `src/ModelSchema.elm` — `GovernedNode`, `NodeLifecycle`, `TensionNode` types.
- `src/Fractale/Codecs.elm` — derivation helpers (`getTensionNode`, `nodeFromTensionHead`).
- `src/Query/QueryTension.elm` — GraphQL selections; `src/Requests.elm` — REST decoders.
- `src/Org/Tension.elm` — patches local state from mutation responses.
- Consumers: `Fractale/View.elm` and `Components/Board.elm` (icons, archived badge),
  `Components/ActionPanel.elm` (archive menu), `Fractale/Event.elm` (timeline wording).

Backend counterpart: `fractal6.go/docs/node-governance.md`.
