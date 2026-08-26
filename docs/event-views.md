# Event Views

The timeline entries on tension pages — who did what, when. Everything lives in
`src/Fractale/Event.elm`: event metadata (`eventTypeToText`, `eventToIcon`, `eventToLink`),
the `viewEvent` dispatcher with its `viewEvent*` functions, and the contract wording helpers.

```elm
viewEvent : SessionCommon -> Maybe String -> NodeType.NodeType -> Event -> Html msg
```

The `Maybe String` is the focus node id (label links) and the `NodeType` gives the wording for
governance events, derived from the tension's governed/draft Node (see
`docs/node-governance.md`); it defaults to Role when absent. Unhandled event types render
`text ""`.

Views are polymorphic in `msg`, so any module can use them. `Components/Comments.elm` calls
them through `Lazy.lazy4 viewEvent session focusid nodeType event`.

The authoritative list of event types and their semantics is the backend EMAP
(`fractal6.go/graph/tension_op.go`, `TensionEventHook`); the frontend mirrors it by
pattern-matching on `Fractal.Enum.TensionEvent`.

i18n has two groups in `i18n/i18n.toml`: event descriptions (`pinned_event`, `closed_event`)
for notification panels, and action verbs (`pinned2`, `closed2`) for the timeline.
