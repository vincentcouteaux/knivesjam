const midinotes = Immutable.List([...Array(71).keys()]).map(x => x+24);
const midi2freq = n => 440*(2**((n-69)/12));


const bufInstrument = (bufMap, ctx) => ({
    buffers: bufMap,
    sources: [],
    stopNote: function(pitch, time) {
        if (pitch in this.sources && this.sources[pitch] !== null) {
            this.sources[pitch].gain.setValueAtTime(0, time); 
        }
    },
    startNote: function(pitch, gain, time) {
        //console.log(pitch);
        //console.log(this.buffers.get(pitch));
        this.stopNote(pitch, time);
        let bufferSource = ctx.createBufferSource()
        const gainNode = ctx.createGain();
        gainNode.gain.setValueAtTime(gain, ctx.currentTime);
        bufferSource.buffer = this.buffers.get(pitch);
        bufferSource.connect(gainNode);
        gainNode.connect(ctx.destination);
        bufferSource.start(time);
        this.sources[pitch] = gainNode;
    },
    stopAllNotes: function() {
        this.sources.forEach((_, i) => this.stopNote(i, ctx.currentTime));
    }
});

const sineBufInstrument = ctx => {
    const getBuffer = freq => {
        var buf = ctx.createBuffer(1, ctx.sampleRate/3, ctx.sampleRate);
        for (var i=0; i<(ctx.sampleRate/3); i++) {
            buf.getChannelData(0)[i] = Math.cos(2*freq*Math.PI*i/ctx.sampleRate)/3;
        }
        return buf;
    };
    const buffers = midinotes.map(midi2freq).map(getBuffer);
    const mapBuffers = midinotes.zip(buffers);
    //console.log(mapBuffers);
    //console.log(mapBuffers.get(30));
    return bufInstrument(new Map(mapBuffers), ctx);
};

const bassBufInstrument = ctx => {
    let buffersMap = [];
    //let decodeCallbacks = [];
    for (var i=1; i<=25; i++) {
        let request = new XMLHttpRequest();
        const i_ = i;
        request.open("GET", "wavs/jbass"+i+".wav", true);
        request.responseType = "arraybuffer";
        request.onload = () => {
            ctx.decodeAudioData(request.response,
                                buf => {
                                    buffersMap[i_+15] = buf;
                                },
                                e => console.log("error: ", e));
        };
        request.send();
    }
    let sources = [];
    return {
        stopNote: function(pitch, time) {
            if (pitch in sources && sources[pitch] !== null) {
                //sources[pitch].gain.exponentialRampToValueAtTime(.01, time +.2); 
                const gain = sources[pitch].gain;
                gain.setValueAtTime(gain.value, time); 
                gain.exponentialRampToValueAtTime(.01, time +.2); 
            }
        },
        startNote: function(pitch, gain, time) {
            this.stopNote(pitch, time);
            if (pitch in buffersMap) {
                // TODO investigate when the same note is played twice consecutively
                let bufferSource = ctx.createBufferSource()
                const gainNode = ctx.createGain();
                gainNode.gain.setValueAtTime(gain*this.volume, ctx.currentTime);
                if (pitch == 25) {
                    bufferSource.buffer = buffersMap[pitch+1];
                    bufferSource.playbackRate.value = Math.pow(2, -1/12);
                }
                else {
                    bufferSource.buffer = buffersMap[pitch];
                }
                bufferSource.connect(gainNode);
                gainNode.connect(ctx.destination);
                bufferSource.start(time);
                sources[pitch] = gainNode;
            }
            else console.log(pitch, " not playable");
        },
        stopAllNotes: function() {
            sources.forEach((_, i) => this.stopNote(i, ctx.currentTime));
        },
        volume: 1
    };
};

const pianoBufInstrument = ctx => {
    const wavfiles_available = [51, 54, 57, 60, 63, 66, 69, 72, 75];
    let buffersMap = [];
    wavfiles_available.forEach(i => {
        let request = new XMLHttpRequest();
        request.open("GET", "wavs/piano/"+i+".wav", true);
        request.responseType = "arraybuffer";
        request.onload = () => {
            ctx.decodeAudioData(request.response,
                                buf => {
                                    buffersMap[i] = buf;
                                },
                                e => console.log("error: ", e));
        };
        request.send();
    });
    const argMax = array => array.map((x, i) => [x, i]).reduce((r, a) => (a[0] > r[0] ? a : r))[1];

    const closest = i => {
        const dist = wavfiles_available.map(x => -Math.abs(x-i)); 
        return wavfiles_available[argMax(dist)];
    };

    const halfstep = Math.pow(2, 1/12);
    //console.log("halfstep", halfstep);
    let sources = [];

    return {
        stopNote: function(pitch, time) {
            if (pitch in sources && sources[pitch] !== null) {
                const gain = sources[pitch].gain;
                gain.setValueAtTime(gain.value, time); 
                gain.exponentialRampToValueAtTime(.001, time +1); 
            }
        },
        startNote: function(pitch, gain, time) {
            //this.stopNote(pitch, time);
            let bufferSource = ctx.createBufferSource()
            const gainNode = ctx.createGain();
            const closestnote = closest(pitch);
            gainNode.gain.setValueAtTime(gain*this.volume, ctx.currentTime);
            bufferSource.buffer = buffersMap[closestnote];
            bufferSource.playbackRate.value = Math.pow(halfstep, pitch-closestnote);
            bufferSource.connect(gainNode);
            gainNode.connect(ctx.destination);
            bufferSource.start(time);
            sources[pitch] = gainNode;
        },
        stopAllNotes: function() {
            sources.forEach((_, i) => this.stopNote(i, ctx.currentTime));
        },
        volume: 1
    };
};

const drumsBufInstrument = ctx => {
    let ride_buffer;
    let snare_buffer;
    let kick_buffer;
    let hh_buffer;
    let rim_buffer;
    let crash_buffer;
    var request1 = new XMLHttpRequest();
    request1.open("GET", "wavs/drums/kick.wav", true);
    request1.responseType = "arraybuffer";
    request1.onload = () => {
        ctx.decodeAudioData(request1.response,
                            buf => {
                                kick_buffer = buf;
                            },
                            e => console.log("error: ", e));
    };
    request1.send();
    var request2 = new XMLHttpRequest();
    request2.open("GET", "wavs/drums/snare.wav", true);
    request2.responseType = "arraybuffer";
    request2.onload = () => {
        ctx.decodeAudioData(request2.response,
                            buf => {
                                snare_buffer = buf;
                            },
                            e => console.log("error: ", e));
    };
    request2.send();
    var request3 = new XMLHttpRequest();
    request3.open("GET", "wavs/drums/ride.wav", true);
    request3.responseType = "arraybuffer";
    request3.onload = () => {
        ctx.decodeAudioData(request3.response,
                            buf => {
                                ride_buffer = buf;
                            },
                            e => console.log("error: ", e));
    };
    request3.send();
    var request4 = new XMLHttpRequest();
    request4.open("GET", "wavs/drums/hh.wav", true);
    request4.responseType = "arraybuffer";
    request4.onload = () => {
        ctx.decodeAudioData(request4.response,
                            buf => {
                                hh_buffer = buf;
                            },
                            e => console.log("error: ", e));
    };
    request4.send();
    var request5 = new XMLHttpRequest();
    request5.open("GET", "wavs/drums/rim.wav", true);
    request5.responseType = "arraybuffer";
    request5.onload = () => {
        ctx.decodeAudioData(request5.response,
                            buf => {
                                rim_buffer = buf;
                            },
                            e => console.log("error: ", e));
    };
    request5.send();
    var request6 = new XMLHttpRequest();
    request6.open("GET", "wavs/drums/crash.wav", true);
    request6.responseType = "arraybuffer";
    request6.onload = () => {
        ctx.decodeAudioData(request6.response,
                            buf => {
                                crash_buffer = buf;
                            },
                            e => console.log("error: ", e));
    };
    request6.send();
    return {
        stopNote: function(pitch, time) {
            },
        startNote: function(pitch, gain, time) {
            let bufferSource = ctx.createBufferSource()
            const gainNode = ctx.createGain();
            gainNode.gain.setValueAtTime(gain*this.volume, ctx.currentTime);
            bufferSource.buffer = pitch==0?kick_buffer:
                                  pitch==1?snare_buffer:
                                  pitch==2?ride_buffer:
                                  pitch==3?hh_buffer:
                                  pitch==4?rim_buffer:crash_buffer;
            bufferSource.connect(gainNode);
            gainNode.connect(ctx.destination);
            bufferSource.start(time);
        },
        stopAllNotes: function() {
        },
        volume: 1
    };
};
