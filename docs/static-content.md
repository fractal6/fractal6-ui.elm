
# Static HTML Content

Static HTML content (e.g., welcome page descriptions, blog posts) can be fetched from the assets server and injected into Elm views.

## How it works

1. **Fetching**: Use `Requests.fetchStaticPage` to fetch HTML content from `api.assets`:
   ```elm
   fetchStaticPage apis "welcome" (GotStaticContent "f6-static-welcome")
   -- Fetches: {api.assets}/welcome.html
   ```

2. **Rendering**: Since Elm 0.19 removed direct `innerHTML` support (see https://github.com/elm/html/issues/172), we use a port-based approach:
   - Create an empty **keyed** div with a specific ID in the view (see [Virtual DOM cleanup](#virtual-dom-cleanup-with-keyednode) for why):
     ```elm
     Keyed.node "div" [ id "f6-static-welcome" ] []
     ```
   - When content is received, use the `Ports.setInnerHtml` port to inject HTML:
     ```elm
     Ports.setInnerHtml { id = "f6-static-welcome", html = content }
     ```

Always prepend the html id of the target element by "f6-static".


3. **Assets Server**: Static HTML files are stored on the assets server (configured via `api.assets` in webpack). The backend serves these files as-is.

## Message pattern with partial application

Use partial application to pass the element ID without adding model state:

```elm
-- Msg type includes element ID as first parameter
| GotStaticContent String (Result Http.Error String)

-- Fetch with element ID via partial application
fetchStaticPage apis "welcome" (GotStaticContent "f6-static-welcome")

-- Handler extracts the ID from the message
GotStaticContent elementId result ->
    case result of
        Ok content ->
            ( model, Ports.setInnerHtml { id = elementId, html = content }, Cmd.none )
        Err _ ->
            ( { model | staticContent = StaticFailure }, Cmd.none, Cmd.none )
```

## Virtual DOM cleanup with Keyed.node

When using `setInnerHtml` to inject HTML into the DOM, the injected content lives **outside Elm's virtual DOM**. Elm doesn't know about these children, which causes a problem: when navigating to a different page, Elm's virtual DOM diffing algorithm **morphs** (patches) the existing DOM node rather than replacing it. This means the innerHTML-injected children survive navigation and orphan into the new page's DOM.

The fix is to use `Html.Keyed.node` instead of a regular `div` for the container element:

```elm
import Html.Keyed as Keyed

-- In the view:
StaticSuccess ->
    Keyed.node "div" [ id "f6-static-welcome" ] []
```

This works because Elm's virtual DOM treats keyed nodes (type 2) differently from regular nodes (type 1). When a keyed node is diffed against a non-keyed node (which happens when Elm navigates to a different page whose view returns regular `div` elements), the entire DOM element is **replaced** rather than morphed. This destroys all innerHTML-injected content automatically.

**Do not** attempt to clean up innerHTML via ports (e.g., a `clearInnerHtml` command) -- the target element is typically already removed from the DOM by the time the port command executes.

## SPA navigation from static HTML

Static HTML content may contain internal links (e.g., `<a href="/signup">`). By default, clicking these triggers a full page reload, bypassing Elm's SPA routing.

To enable seamless SPA navigation, the `setInnerHtml` JavaScript handler intercepts clicks on internal links:

```javascript
// In assets/js/ports.js - setInnerHtml handler
el.addEventListener('click', function(e) {
    var target = e.target.closest('a');
    if (target && target.hasAttribute('href')) {
        var href = target.getAttribute('href');
        // Internal links: starts with / but not //
        if (href.startsWith('/') && !href.startsWith('//')) {
            e.preventDefault();
            app.ports.navigateFromJs.send(href);
        }
    }
});
```

The `navigateFromJs` incoming port (defined in `src/Ports.elm`) is subscribed to in `Global.elm` and mapped to `NavigateRaw`, which calls `Nav.pushUrl` for standard SPA navigation.

External links (absolute URLs, protocol-relative URLs) are not intercepted and proceed with normal browser navigation.

## Example implementation

See `src/Pages/About.elm` for a complete example of:
- Fetching static content on page init
- Storing content state in the model (`StaticContentState` type)
- Rendering via `Keyed.node` and the `setInnerHtml` port

## File locations

- **Development templates**: Static HTML templates are stored in `assets/html/` (e.g., `assets/html/welcome.html`)
- **Deployment**: These files must be deployed to the assets server at `{api.assets}/` (e.g., `https://api.fractale.co/assets/welcome.html`)

## Guidelines

- Static HTML should contain only body content (no `<html>`, `<head>`, `<body>` tags)
- Use Bulma CSS classes for styling consistency
- You can include `<style>` for custom CSS but ban the use of`<script>` interactions, only do it with modern CSS and animation.
- The port-based innerHTML approach is safe for server-controlled content from trusted sources
- For images/assets in static HTML, use absolute URLs pointing to the assets server
- Always use `Keyed.node` for static content containers to ensure proper cleanup on navigation
- Internal links in static HTML will automatically use SPA navigation (no full page reload)

## CSS Animation Limitations in Injected Content

When HTML is injected via `innerHTML` (through `Ports.setInnerHtml`), certain CSS features don't work reliably:

### Problematic Features

1. **CSS `@property` rules**: Custom property animations with `@property` don't resolve in dynamically injected stylesheets. Counter animations using this feature will show initial values (often 0).

2. **Fade-in animations**: Browsers don't recognize injected elements as "new" for animation triggers. Elements with `animation: fadeInUp` will either show at their final state immediately or not animate at all.

3. **Transform animations in Chromium**: GPU compositing can have issues with transform animations in dynamically injected content. Rolling/scrolling text animations may stutter or fail.

### Workarounds

- **Static values**: Use plain text instead of CSS counter animations (e.g., "500+" instead of animated counting)
- **No entry animations**: Content appears immediately - this is acceptable for dynamically loaded pages
- **Chromium-friendly animations**: For transform-based animations (like rolling text), use:
  - `-webkit-` prefixes for all properties
  - `will-change: transform` to hint GPU acceleration
  - `transform-style: preserve-3d` and `backface-visibility: hidden`
  - Duplicate keyframes with `-webkit-` prefix

### Animations That Work Well

- `@keyframes` with basic transforms (when properly prefixed)
- CSS transitions on hover/focus states
- Infinite background animations (gradients, patterns)
- Scroll-triggered animations via intersection observer (would require JS port)
