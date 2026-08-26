# Static HTML Content

Server-hosted HTML fragments (welcome page, about, blog posts) fetched from the assets server
and injected into an Elm view. See `src/Pages/About.elm` for a full example.

## Flow

1. `Requests.fetchStaticPage apis "welcome" (GotStaticContent "f6-static-welcome")` fetches
   `{api.assets}/welcome.html`. The element id travels in the message by partial application,
   so no extra model state is needed. Always prefix the target id with `f6-static`.
2. The view renders an **empty keyed node**: `Keyed.node "div" [ id "f6-static-welcome" ] []`.
3. The handler injects with `Ports.setInnerHtml { id = elementId, html = content }` — Elm 0.19
   has no `innerHTML` attribute ([elm/html#172](https://github.com/elm/html/issues/172)).

Sources live in `assets/html/` and must be deployed to the assets server at `{api.assets}/`.

## Why `Keyed.node`

Injected children live outside Elm's virtual DOM. On navigation Elm *morphs* a plain `div`
instead of replacing it, so the injected content would orphan into the next page. A keyed node
diffed against a non-keyed one is replaced wholesale, which drops the injected children.

Do **not** clean up through a port instead: by the time the command runs the element is
usually already gone.

## SPA navigation

The `setInnerHtml` handler in `assets/js/ports.js` intercepts clicks on links whose `href`
starts with a single `/` and sends them to the `navigateFromJs` port, which `Global.elm` maps
to `NavigateRaw` (`Nav.pushUrl`). Absolute and protocol-relative URLs are left alone.

## Guidelines

- Body content only — no `<html>`/`<head>`/`<body>`.
- Bulma classes for consistency; absolute URLs for images.
- `<style>` is allowed, `<script>` is not. Interactions must be pure CSS.
- The approach is only safe for trusted, server-controlled content.

## CSS caveats in injected content

Injected elements aren't "new" to the browser's animation engine, so entry animations
(`fadeInUp`) and `@property`-based counter animations don't fire — use static values and let
content appear immediately. Transform animations need `-webkit-` prefixed properties and
keyframes plus `will-change: transform` / `backface-visibility: hidden` to composite reliably
in Chromium. Hover transitions and infinite background animations work fine.
