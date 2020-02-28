let request = window.indexedDB.open("LibraryDatabase", 0)
var db;
request.onerror = evt => { console.log("Accès à la base de données locale refusée") };
request.onsuccess = evt => {
    db = evt.target.result;
};
request.onupgradeneeded = evt => {
    let db = evt.target.result;
    let objectStore = db.createObjectStore("songs", {keyPath: "key"});
    objectStore.createIndex("title", "title", {unique: false});
    objectStore.createIndex("composer", "composer", {unique: false});
    objectStore.createIndex("beatsPerBar", "beatsPerBar", {unique: false});
    objectStore.createIndex("defaultTempo", "defaultTempo", {unique: false});
    objectStore.createIndex("style", "style", {unique: false});
    objectStore.createIndex("chordProg", "chordProg", {unique: false});
};

db.onerror = evt => { console.log("Database error: " + evt.target.errorCode) };
