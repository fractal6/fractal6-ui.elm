# Service Worker

The service worker (`public/service-worker.js`) is registered from `public/index.js` (production builds only — dev bundles are unhashed, cache-first would serve stale code) and acts as the local cache layer for static assets.

## Why we have one

- **Offline shell**: serve the app shell when the network is unavailable.
- **Performance**: avoid re-downloading content-hashed bundles (`/static/*`) that never change.

## What it does

- **Cache name**: `CACHE_NAME` is set at build time by webpack (`CopyPlugin` in `webpack.config.js` replaces `VERSION_PLACEHOLDER` with the commit hash). Each deploy gets a distinct cache.
- **On `install`**: precaches the SPA shell (`/`) so navigations to never-visited routes work offline.
- **On `activate`**: deletes any cache whose name doesn't match the current `CACHE_NAME` (prevents old caches from accumulating across deploys), then claims existing clients.
- **On `fetch`**: routes per request type, see below.

## Routing rules

| Request | Strategy | Why |
|---|---|---|
| Cross-origin | Pass-through (no SW logic) | Not our business. |
| Non-GET (POST/PUT/DELETE) | Pass-through | Cache API rejects POST; mutations must hit network. |
| Navigation (`request.mode === 'navigate'`) | **Network-first**, precached `/` shell offline | Every SPA route returns the same shell, which must be fresh after deploys. |
| `/static/*` | **Cache-first** | Content-hashed URLs are immutable; serving from cache is always correct. |
| Everything else | Pass-through | Allowlist, not denylist: backend routes (`/api`, `/auth`, `/q`, `/file`, `/notifications`) can share the SPA origin in single-domain deployments and must always hit the network; `/assets` static content (see `docs/static-content.md`) updates independently of deploys. |

Only `res.ok` responses are cached (never persist a 404/500 as an asset).

## Interaction with the version banner

The "New Version Released" banner (`Components/Navbar.elm`) and the `forceReload` port (`assets/js/ports.js`) provide a separate mechanism for **already-open tabs** to detect a new deploy. On click, `forceReload` unregisters the SW and wipes its caches before navigating with a `?version=<ts>` cache-buster. The SW strategy above and the banner are complementary:

- SW network-first navigations cover **cold loads** (new tab / refresh).
- The banner covers **long-lived tabs** that haven't navigated since the deploy.

## Backend collaboration

`fractal6.go/web/fileserver.go` sets:

- `Cache-Control: no-cache` on `index.html` and `service-worker.js` (must revalidate).
- `Cache-Control: public, max-age=31536000, immutable` on `/static/*`.

`no-cache` on the SW file itself ensures the browser always sees a new `service-worker.js` after a deploy, which triggers the install/activate cycle and the cache cleanup above.
