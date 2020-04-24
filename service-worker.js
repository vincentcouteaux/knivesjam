var CACHE_NAME = 'static-cache';
var urlsToCache = [
  '.',
  'index.html',
  'style.css',
  'w3.css',
  'immutable.min.js',
  'knivesjam_logo.svg',
  'elm.js',
  'dexie.js',
  'librarydb.js',
  'tune.js',
  'MaterialIcons-Regular.woff',
  'texture_papier2.jpg',
  'wavs/jbass1.wav',
  'wavs/jbass2.wav',
  'wavs/jbass3.wav',
  'wavs/jbass4.wav',
  'wavs/jbass5.wav',
  'wavs/jbass6.wav',
  'wavs/jbass7.wav',
  'wavs/jbass8.wav',
  'wavs/jbass9.wav',
  'wavs/jbass10.wav',
  'wavs/jbass11.wav',
  'wavs/jbass12.wav',
  'wavs/jbass13.wav',
  'wavs/jbass14.wav',
  'wavs/jbass15.wav',
  'wavs/jbass16.wav',
  'wavs/jbass17.wav',
  'wavs/jbass18.wav',
  'wavs/jbass19.wav',
  'wavs/jbass20.wav',
  'wavs/jbass21.wav',
  'wavs/jbass22.wav',
  'wavs/jbass23.wav',
  'wavs/jbass24.wav',
  'wavs/jbass25.wav',
  'wavs/piano/102.wav',
  'wavs/piano/105.wav',
  'wavs/piano/107.wav',
  'wavs/piano/24.wav',
  'wavs/piano/30.wav',
  'wavs/piano/35.wav',
  'wavs/piano/39.wav',
  'wavs/piano/42.wav',
  'wavs/piano/47.wav',
  'wavs/piano/51.wav',
  'wavs/piano/54.wav',
  'wavs/piano/57.wav',
  'wavs/piano/60.wav',
  'wavs/piano/63.wav',
  'wavs/piano/66.wav',
  'wavs/piano/69.wav',
  'wavs/piano/72.wav',
  'wavs/piano/75.wav',
  'wavs/piano/78.wav',
  'wavs/piano/81.wav',
  'wavs/piano/84.wav',
  'wavs/piano/87.wav',
  'wavs/piano/90.wav',
  'wavs/piano/93.wav',
  'wavs/piano/96.wav',
  'wavs/piano/99.wav',
  'wavs/drums/crash.wav',
  'wavs/drums/hh.wav',
  'wavs/drums/kick.wav',
  'wavs/drums/snare.wav',
  'wavs/drums/ride.wav',
  'wavs/drums/rim.wav'
];
self.addEventListener('install', function(event) {
  event.waitUntil(
    caches.open(CACHE_NAME)
    .then(function(cache) {
      return cache.addAll(urlsToCache);
    })
  );
});

self.addEventListener('fetch', function(event) {
  event.respondWith(
    caches.match(event.request)
    .then(function(response) {
      return response || fetchAndCache(event.request);
    })
  );
});

function fetchAndCache(url) {
  return fetch(url)
  .then(function(response) {
    // Check if we received a valid response
    if (!response.ok) {
      throw Error(response.statusText);
    }
    return caches.open(CACHE_NAME)
    .then(function(cache) {
      cache.put(url, response.clone());
      return response;
    });
  })
  .catch(function(error) {
    console.log('Request failed:', error);
    // You could return a custom offline 404 page here
  });
}
