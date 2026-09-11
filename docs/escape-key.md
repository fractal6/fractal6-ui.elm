# Escape key

Three mechanisms can close something on ESC. A container must own ESC in only one.

| Mechanism | Priority handling | Survives `Ports.bulma_driver` |
|---|---|---|
| `.modal-escape` + `data-modal` -> `closeModal` (`assets/js/bulma_drivers.js`) | skips when a `.panel.dropList` is visible inside | yes, re-registered from the DOM |
| `Ports.outsideClickClose msg target hasEsc` (`assets/js/ports.js`) | none | no, imperative listeners are purged |
| `Browser.Events.onKeyDown (Dom.key "Escape" ...)` in a component | none, every subscription fires | yes |

`BulmaDriver` removes all `document` handlers and re-registers only the DOM-declared
ones, so an ESC listener registered by `outsideClickClose` dies on any later
`bulma_driver` call (it did for the tension card panel, not for drafts).

Components with nested ESC-closable widgets keep ESC in Elm: pass `hasEsc = False`
to `outsideClickClose` (click-outside only) and gate the subscription on a
`hasEscConsumer` predicate, since Elm subscriptions cannot stop propagation between
each other. Use `onKeyDown`: JS-driven widgets (emoji picker, `@`-mention, confirm
modal) close on keydown, so a keyup guard reads stale state.

| File | Role |
|---|---|
| `src/Components/CardPanel.elm` | reference implementation: `hasEscConsumer` + guarded `onKeyDown` |
| `src/Components/Comments.elm` | `hasEscConsumer` aggregating emoji picker, `@`-mention, confirm modal, highlighted comment |
| `src/Components/UserSearchPanel.elm`, `LabelSearchPanel.elm` | `isOpen_` getters used by the guard |
