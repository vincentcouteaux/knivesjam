
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
    let nextEvent2Sched = 0;

    let schedAhead = 0.1;
    let lookAhead = 20;

    const scheduleEvent = evt => {
        const evtTime = (evt[0] - cursor)*60/BPM;
        if (evt[1])
            instruments[evt[3]].startNote(evt[2], evt[4], ctx.currentTime + evtTime);
        else instruments[evt[3]].stopNote(evt[2], ctx.currentTime + evtTime);

    };

    const scheduler = (prevtime) => {
        if (!inplay)
            return;
        const curTime = ctx.currentTime;
        cursor = cursor + (curTime - prevtime)*BPM/60;
        if (app.ports.cursorChanged)
            app.ports.cursorChanged.send(cursor);
        const nextCursor = cursor + schedAhead*BPM/60;
        //console.log(cursor, nextCursor, nextCursor-cursor);
        let i = 0;
        while (nextEvent2Sched < sequence.length && sequence[nextEvent2Sched][0] < nextCursor) {
            scheduleEvent(sequence[nextEvent2Sched]);
            nextEvent2Sched += 1;
            i++;
        }
        //console.log(i, " events scheduled this time, last = ", sequence[nextEvent2Sched-1], cursor, nextCursor);
        if (nextEvent2Sched < sequence.length) {
            timeoutid = window.setTimeout(() => scheduler(curTime), lookAhead);
        }
        else {
            app.ports.sequenceFinished.send(null);
            if (nextsequence.length > 0) {
                const lasttime = sequence[nextEvent2Sched-1][0];
                sequence = nextsequence;
                nextsequence = [];
                cursor -= lasttime;
                nextEvent2Sched = 0;
                scheduler(ctx.currentTime);    
            }
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
        nextEvent2Sched = cut;
        scheduler(ctx.currentTime);
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
        newseq.sort((x, y) => 
                        x[0] > y[0] ? 1:
                        x[0] < y[0] ? -1:
                        x[1] && !y[1] ? 1:-1 );
        return newseq;
    };
    if (app.ports.setSequence) {
        app.ports.setSequence.subscribe(s => {
            stopAllInst();
            sequence = receiveSeq(s);
            //nextSequence = newseq.slice();
            //playFrom();
        });
    }
    if (app.ports.setSequenceAndPlay) {
        app.ports.setSequenceAndPlay.subscribe(s => {
            stopAllInst();
            sequence = receiveSeq(s);
            //nextSequence = newseq.slice();
            inplay = true;
            cursor = 0;
            playFrom();
        });
    }
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
