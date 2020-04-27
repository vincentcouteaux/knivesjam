
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
        db.songs.count(c => {
            if (c == 0) {
                var xmlhttp = new XMLHttpRequest();
                xmlhttp.onreadystatechange = function() {
                    if (this.readyState == 4 && this.status == 200) {
                      var myObj = JSON.parse(this.responseText);
                      myObj.forEach(song => {
                        db.songs.put(song);
                        app.ports.gotASong.send(JSON.stringify(song));
                      });
                    }
                };
                xmlhttp.open("GET", "default_library.json", true);
                xmlhttp.send();

            };
        });
    });

    app.ports.deleteSong.subscribe(s => {
        console.log("deleting ", s);
        db.songs.delete(s).then(k => console.log(k + " has been deleted")
                ).catch(e => console.log(e));
    });
};
