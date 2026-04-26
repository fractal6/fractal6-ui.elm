# Event Views Architecture

## Overview

Event views render the timeline/history entries in tension pages. Each `TensionEvent` type has a corresponding view function that produces an HTML representation showing who did what and when.

## Location

All event view functions live in `src/Bulk/Event.elm`. This module serves as the single source of truth for:

- **Event metadata** — `eventTypeToText`, `eventToIcon`, `eventToLink`
- **Event rendering** — `viewEvent` dispatcher and all `viewEvent*` sub-functions
- **Contract utilities** — `contractTypeToText`, `contractEventToText`, etc.

## Key Functions

### `viewEvent`

The main dispatcher. Signature:

```elm
viewEvent : SessionCommon -> Maybe String -> Maybe TensionAction -> Event -> Html msg
```

- `SessionCommon` — session context (language, current time, lexicon)
- `Maybe String` — optional focus node ID (used for label links)
- `Maybe TensionAction` — the tension's action type (NewRole, EditCircle, etc.)
- `Event` — the event record to render

Returns `text ""` for unhandled event types.

### Supported Events

The list of event types and their authoritative semantics live in the backend
EMAP at `fractal6.go/graph/tension_op.go` (`TensionEventHook`). The frontend
mirrors each one in a `viewEvent*` function, dispatched from `viewEvent` by
pattern-matching on `Fractal.Enum.TensionEvent`.

## Usage

`Components/Comments.elm` imports `viewEvent` from `Bulk.Event` and uses it via `Html.Lazy.lazy4`:

```elm
Lazy.lazy4 viewEvent session focusid action event
```

The view functions use polymorphic `Html msg` signatures (not `Html Msg`), making them reusable from any module without message type coupling.

## i18n

Event-related translations are in `i18n/i18n.toml` under two groups:
- **Event descriptions** (e.g. `pinned_event`, `closed_event`) — used in notification panels
- **Action verbs** (e.g. `pinned2`, `closed2`) — used in timeline event views
