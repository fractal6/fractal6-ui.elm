const CACHE_NAME = 'VERSION_PLACEHOLDER';

// Precache the SPA shell so navigations to never-visited routes work offline.
self.addEventListener('install', (e) => {
  e.waitUntil(
    caches.open(CACHE_NAME)
      .then((cache) => cache.add('/'))
      .then(() => self.skipWaiting())
  );
});

self.addEventListener('activate', (e) => {
  e.waitUntil(
    caches.keys()
      .then((keys) => Promise.all(
        keys.filter((k) => k !== CACHE_NAME).map((k) => caches.delete(k))
      ))
      .then(() => clients.claim())
  );
});

self.addEventListener('fetch', (e) => {
  const url = new URL(e.request.url);

  // Skip cross-origin and non-GET
  if (url.origin !== location.origin) return;
  if (e.request.method !== 'GET') return;

  // Network-first for navigations: every SPA route returns the same shell,
  // so a fresh fetch after deploys, the precached '/' shell when offline.
  if (e.request.mode === 'navigate') {
    e.respondWith(fetch(e.request).catch(() => caches.match('/')));
    return;
  }

  // Cache-first for /static/ only (content-hashed URLs are immutable).
  // Everything else passes through: backend routes (/api, /q, /file, ...) can
  // share the SPA origin, and /assets content updates independently of deploys.
  if (!url.pathname.startsWith('/static/')) return;
  e.respondWith(
    caches.open(CACHE_NAME).then((cache) =>
      cache.match(e.request).then((response) =>
        response || fetch(e.request).then((fetchResponse) => {
          if (fetchResponse.ok) cache.put(e.request, fetchResponse.clone());
          return fetchResponse;
        })
      )
    )
  );
});
