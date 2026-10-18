# Scroll Position Detection

Detect when the main window is scrolled to the top, bottom, or somewhere in the middle.

## How it works

1. **JavaScript listener**: A scroll event listener in `assets/js/ports.js` uses `requestAnimationFrame` for throttling and only sends updates when the position changes.

2. **Port communication**: The position ("top", "bottom", "middle") is sent to Elm via the `scrollPositionFromJs` port.

3. **Elm type**: The `Ports.ScrollPosition` type represents the three states:
   - `ScrollTop` - Window is at the top (scrollY <= 5px)
   - `ScrollMiddle` - Window is scrolled, not at top or bottom
   - `ScrollBottom` - Window is at the bottom (within 5px of max scroll)

## Usage in components

Access the scroll position from the session:

```elm
view : Global.Model -> Model -> Document Msg
view global model =
    let
        scrollPos = global.session.common.scrollPosition
        isAtTop = scrollPos == Ports.ScrollTop
    in
    -- Use isAtTop to conditionally show/hide elements
```

## Performance considerations

- Uses `requestAnimationFrame` to throttle updates (~60fps max)
- Uses `{ passive: true }` listener for scroll performance
- Only sends to Elm when position actually changes
- 5px threshold prevents flickering at boundaries

## Scroll restoration on back/forward

`assets/js/scrollRestore.js` (`initScrollRestore`, called from `ports.js`) restores the board
scroll (project board and tensions board views: circle, label, assignee) when navigating with the browser back/forward buttons.

- **Save**: a capture-phase `scroll` listener on `document` records, per URL, the scroll of
  `.kb-board` elements and their children that have an `id` (board horizontal scroll, columns
  `.content` vertical scroll). Only the last 10 URLs are kept.
- **Restore**: on `popstate`, the saved positions are re-applied every frame for 3s, so they win
  over the async board render and auto-scrolls. Any user input (wheel, touch, key, mouse) stops it.
- The window scroll is not handled (left to the browser, `history.scrollRestoration = auto`).

Limitations: elements without an `id` are not restored; positions
are in memory only (lost on reload); the same URL visited twice shares one entry.

Tests: `tests/Js/scrollRestore.test.js`.
