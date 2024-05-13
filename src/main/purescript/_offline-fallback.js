var CACHE = 'offline-fallback'

self.addEventListener('install', function(event) {
	event.waitUntil(precache());
});

self.addEventListener('message', function(event) {
	if (event.data === 'update') {
		console.log("updated index.html");
		event.waitUntil(precache());
	}
})
  
function precache() {
	return caches.open(CACHE).then(function (cache) {
		return cache.add('./index.html');
	});
}

self.addEventListener('fetch', function(event) {
	// Only fall back for HTML documents.
	var request = event.request;
	if (request.method === 'GET' && request.headers.get('accept').includes('text/html') && ["index.html", "app/"].includes(request.url.split('/').at(-1))) {
	  // `fetch()` will use the cache when possible, to this examples
	  // depends on cache-busting URL parameter to avoid the cache.
	  event.respondWith(
		fetch(request).catch(function(error) {
		  // `fetch()` throws an exception when the server is unreachable but not
		  // for valid HTTP responses, even `4xx` or `5xx` range.
		  console.error(
			'[onfetch] Failed. Serving cached offline fallback ' +
			error
		  );
		  return caches.open(CACHE).then(function(cache) {
			return cache.match('index.html');
		  });
		})
	  );
	}
	// Any other handlers come here. Without calls to `event.respondWith()` the
	// request will be handled without the ServiceWorker.
});