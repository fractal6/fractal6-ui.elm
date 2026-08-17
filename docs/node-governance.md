# Node Governance (derived state)

A governance tension governs at most one Node. The backend owns the relation
(`Tension.governed_node`); the frontend derives everything from it — there is
no persisted action state.

## How it works

A tension's Node state is derived, never stored: a governed Node with
`isArchived` gives `Active`/`Archived`; before publication the latest blob
fragment gives `Draft`; ordinary tensions have none. `receiver` is the parent
circle, never the governed Node. Publish/archive/unarchive are tension events,
and status tags are hidden for governance tensions since they are auto-closed
on creation. The receiver codec is still used to build identity for drafts and
the NewTension preview, where no Node exists yet.

## Where things live

- `src/ModelSchema.elm` — `GovernedNode`, `NodeLifecycle`, `TensionNode` types.
- `src/Fractale/Codecs.elm` — derivation helpers (`getTensionNode`, `nodeFromTensionHead`).
- `src/Query/QueryTension.elm` — GraphQL selections; `src/Requests.elm` — REST decoders.
- `src/Org/Tension.elm` — patches local state from mutation responses.
- Consumers: `Fractale/View.elm` and `Components/Board.elm` (icons, archived badge),
  `Components/ActionPanel.elm` (archive menu), `Fractale/Event.elm` (timeline wording).

Backend counterpart: `fractal6.go/docs/node-governance.md`.
