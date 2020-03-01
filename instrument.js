const midinotes = Immutable.List([...Array(71).keys()]).map(x => x+24);
const midi2freq = n => 440*(2**((n-69)/12));


const bufInstrument = (bufMap, ctx) => ({
    buffers: bufMap,
    sources: [],
    stopNote: function(pitch) {
        if (pitch in this.sources && this.sources[pitch] !== null) {
            this.sources[pitch].gain.setValueAtTime(0, ctx.currentTime); 
        }
    },
    startNote: function(pitch, gain) {
        //console.log(pitch);
        //console.log(this.buffers.get(pitch));
        this.stopNote(pitch);
        let bufferSource = ctx.createBufferSource()
        const gainNode = ctx.createGain();
        gainNode.gain.setValueAtTime(gain, ctx.currentTime);
        bufferSource.buffer = this.buffers.get(pitch);
        bufferSource.connect(gainNode);
        gainNode.connect(ctx.destination);
        bufferSource.start();
        this.sources[pitch] = gainNode;
    },
    stopAllNotes: function() {
        this.sources.forEach((_, i) => this.stopNote(i));
    }
});

const sineBufInstrument = ctx => {
    const getBuffer = freq => {
        console.log(freq);
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
    console.log("qdsfqdf");
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
        stopNote: function(pitch) {
            if (pitch in sources && sources[pitch] !== null) {
                sources[pitch].gain.exponentialRampToValueAtTime(.01, ctx.currentTime+.2); 
            }
        },
        startNote: function(pitch, gain) {
            //console.log(buffersMap);
            //console.log(this.buffers.get(pitch));
            this.stopNote(pitch);
            if (pitch in buffersMap) {
                let bufferSource = ctx.createBufferSource()
                const gainNode = ctx.createGain();
                gainNode.gain.setValueAtTime(gain, ctx.currentTime);
                bufferSource.buffer = buffersMap[pitch];
                bufferSource.connect(gainNode);
                gainNode.connect(ctx.destination);
                bufferSource.start();
                sources[pitch] = gainNode;
            }
        },
        stopAllNotes: function() {
            sources.forEach((_, i) => this.stopNote(i));
        }
    };
};
