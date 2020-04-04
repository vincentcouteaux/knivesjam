
var db = new Dexie("library_db");
db.version(1).stores({
    songs: "key,title,composer,beatsPerBar,defaultTempo,style,chordProg"
});


const initDixie = app => {
    console.log(app);
    //*
    app.ports.addSong2db.subscribe(newsong => {
        console.log(newsong);
        db.songs.put(newsong).then(s => {
                console.log(s + " has been added")
            }).catch( e => {
                console.log(e);
            });
    });
    //*/

    app.ports.queryAllSongs.subscribe(() => {
        db.songs.each(song => { 
                app.ports.gotASong.send(JSON.stringify(song));
            }
        );
    });

    app.ports.deleteSong.subscribe(s => {
        console.log("deleting ", s);
        db.songs.delete(s).then(k => console.log(k + " has been deleted")
                ).catch(e => console.log(e));
    });
};
/*let request = window.indexedDB.open("LibraryDatabase", 0)
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
*/
