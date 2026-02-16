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
