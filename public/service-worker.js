const CACHE_NAME = 'VERSION_PLACEHOLDER';

self.addEventListener('install', (e) => {
  // Skip pre-caching - cache files as they're requested instead
  self.skipWaiting();
});

self.addEventListener('activate', (e) => {
  e.waitUntil(clients.claim());
});

self.addEventListener('fetch', (e) => {
  const url = new URL(e.request.url);

  // Skip external API calls
  if (url.origin !== location.origin) return;

  e.respondWith(
    caches.open(CACHE_NAME).then((cache) => {
      return cache.match(e.request).then((response) => {
        // Return cached or fetch and cache it
        return response || fetch(e.request).then((fetchResponse) => {
          cache.put(e.request, fetchResponse.clone());
          return fetchResponse;
        });
      });
    })
  );
});
