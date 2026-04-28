# Service Worker

The service worker (`public/service-worker.js`) is registered from `public/index.html` and acts as the local cache layer for static assets.

## Why we have one

- **Offline shell**: serve the app shell when the network is unavailable.
- **Performance**: avoid re-downloading content-hashed bundles (`/static/*`) that never change.

## What it does

- **Cache name**: `CACHE_NAME` is set at build time by webpack (`CopyPlugin` in `webpack.config.js` replaces `VERSION_PLACEHOLDER` with the commit hash). Each deploy gets a distinct cache.
- **On `activate`**: deletes any cache whose name doesn't match the current `CACHE_NAME` (prevents old caches from accumulating across deploys), then claims existing clients.
- **On `fetch`**: routes per request type, see below.

## Routing rules

| Request | Strategy | Why |
|---|---|---|
| Cross-origin | Pass-through (no SW logic) | Not our business. |
| Non-GET (POST/PUT/DELETE) | Pass-through | Cache API rejects POST; mutations must hit network. |
| `/api`, `/auth`, `/q`, `/notifications` | Pass-through | Live backend endpoints; never cache. |
| Navigation (`request.mode === 'navigate'`) | **Network-first**, cache fallback | The SPA shell (`index.html`) must be fresh after deploys. Falls back to cache only when offline. |
| Everything else (`/static/*`, fonts, images) | **Cache-first** | Content-hashed URLs are immutable; serving from cache is always correct. |

## Interaction with the version banner

The "New Version Released" banner (`Components/Navbar.elm`) and the `forceReload` port (`assets/js/ports.js`) provide a separate mechanism for **already-open tabs** to detect a new deploy. On click, `forceReload` unregisters the SW and wipes its caches before navigating with a `?version=<ts>` cache-buster. The SW strategy above and the banner are complementary:

- SW network-first navigations cover **cold loads** (new tab / refresh).
- The banner covers **long-lived tabs** that haven't navigated since the deploy.

## Backend collaboration

`fractal6.go/web/fileserver.go` sets:

- `Cache-Control: no-cache` on `index.html` and `service-worker.js` (must revalidate).
- `Cache-Control: public, max-age=31536000, immutable` on `/static/*`.

`no-cache` on the SW file itself ensures the browser always sees a new `service-worker.js` after a deploy, which triggers the install/activate cycle and the cache cleanup above.
