const CACHE_NAME = 'VERSION_PLACEHOLDER';

self.addEventListener('install', (e) => {
  self.skipWaiting();
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

  // Skip cross-origin, non-GET, and same-origin API calls
  if (url.origin !== location.origin) return;
  if (e.request.method !== 'GET') return;
  if (url.pathname.startsWith('/api') || url.pathname.startsWith('/auth') ||
      url.pathname.startsWith('/q') || url.pathname.startsWith('/notifications')) return;

  // Network-first for navigations: ensures a fresh SPA shell after deploys,
  // falls back to cache when offline.
  if (e.request.mode === 'navigate') {
    e.respondWith(
      fetch(e.request)
        .then((res) => {
          caches.open(CACHE_NAME).then((cache) => cache.put(e.request, res.clone()));
          return res;
        })
        .catch(() => caches.match(e.request))
    );
    return;
  }

  // Cache-first for static assets (content-hashed URLs are immutable).
  e.respondWith(
    caches.open(CACHE_NAME).then((cache) =>
      cache.match(e.request).then((response) =>
        response || fetch(e.request).then((fetchResponse) => {
          cache.put(e.request, fetchResponse.clone());
          return fetchResponse;
        })
      )
    )
  );
});
