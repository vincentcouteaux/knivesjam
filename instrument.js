const midinotes = Immutable.List([...Array(71).keys()]).map(x => x+24);
const midi2freq = n => 440*(2**((n-69)/12));


const bufInstrument = (bufList, ctx) => ({
    buffers: bufList,
    sources: [],
    stopNote: function(pitch) {
        if (pitch in this.sources && this.sources[pitch] !== null) {
            this.sources[pitch].gain.setValueAtTime(0, ctx.currentTime); 
        }
    },
    startNote: function(pitch, gain) {
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
    return bufInstrument(buffers, ctx);
};
