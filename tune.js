
const initTune = (ctx, instruments, app) => {
    var BPM = 120;
    var inplay = false;
    let cursor = 0;
    let timeoutid = null;
    let sequence = [ [0, true, 48, "piano", .5]
                   , [.5, false, 48, "piano"]
                   , [1, true, 48, "piano", .8]
                   , [1.5, false, 48, "piano"]
                   , [2, true, 48, "piano", 1]
                   , [2.5, false, 48, "piano"]
                   , [3, true, 50, "piano", 2]
                   , [3.5, false, 50, "piano"]
                   , [4, true, 52, "piano", .1]
                   , [5.5, false, 52, "piano"]
                   , [6, true, 50, "piano", .3]
                   , [7.5, false, 50, "piano"]];
    let nextsequence = []

    const play_recur = (seq, time) => {
        if (seq.length <= 0)
            return seq;
        if (time < seq[0][0]) //or <= ?
            return seq;
        if (seq[0][1])
            instruments[seq[0][3]].startNote(seq[0][2], seq[0][4]);
        else instruments[seq[0][3]].stopNote(seq[0][2]);
        return play_recur(seq.slice(1), time);
    };
    const sched2 = curseq => prevtime => {
        if (!inplay)
            return;
        const curtime = ctx.currentTime;
        cursor = cursor + (curtime - prevtime)*BPM/60;
        if (app.ports.cursorChanged)
            app.ports.cursorChanged.send(cursor);
        const newseq = play_recur(curseq, cursor);
        //console.log(cursor, curtime, newseq.length);
        if (newseq.length > 0) {
            timeoutid = window.setTimeout(() => sched2(newseq)(curtime), 4);
        }
        else {
            app.ports.sequenceFinished.send(null);
            //console.log("finished!", ctx.currentTime);
            if (nextsequence.length > 0) {
                sequence = nextsequence;
                nextsequence = [];
                cursor = cursor - Math.floor(cursor);
                const newseq2 = play_recur(sequence, cursor);
                timeoutid = window.setTimeout(() => sched2(newseq2)(curtime), 20);
            }
            //TODO populate nextsequence
        }
    };

    const playFrom = () => {
        stopAllInst();
        if (timeoutid !== null)
            window.clearTimeout(timeoutid);
        let cut = 0;
        while (cut < sequence.length && sequence[cut][0] < cursor) {
            cut = cut + 1;
        }
        sched2(sequence.slice(cut))(ctx.currentTime);
    };
    const stopAllInst = () => {
        for (let inst in instruments) {
            instruments[inst].stopAllNotes();
        }
    };
    //const setCursor = c => {

    app.ports.play.subscribe(() => {
        if (ctx.state == "suspended") {
            ctx.resume();
        }
        inplay = true;
        playFrom();
        
    });
    app.ports.pause.subscribe(() => {
        stopAllInst();
        inplay = false;
    });
    if (app.ports.setCursor) {
        app.ports.setCursor.subscribe(c => {
            //console.log("set cursor : ", c, ctx.currentTime);
            cursor = c;
            if (app.ports.cursorChanged)
                app.ports.cursorChanged.send(cursor);
            if (inplay)
                playFrom();
        });
    }
    app.ports.setBpm.subscribe(b => {
        BPM = b;
    });
    const receiveSeq = s => {
        let newseq = s.map(el => [el.time , el.onset, el.pitch
                                   , el.instrument, el.gain]);
        newseq.sort((x, y) => x[0] > y[0] ? 1:-1);
        return newseq;
    };
    app.ports.setSequence.subscribe(s => {
        sequence = receiveSeq(s);
        //nextSequence = newseq.slice();
        playFrom();
    });
    if (app.ports.setNextSequence) {
        app.ports.setNextSequence.subscribe(s => {
            nextsequence = receiveSeq(s);
        });
    }
    if (app.ports.setInstVolume) {
        app.ports.setInstVolume.subscribe(req => {
            inst = req[0];
            vol = req[1];
            instruments[inst].volume = vol/100;
        });
    }
};
