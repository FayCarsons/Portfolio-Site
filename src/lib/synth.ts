import { randInt } from './utilities';

type EnvelopeShape = 'linear' | 'exponential';

interface VoiceConstructor {
  ctx: AudioContext;
  shape?: EnvelopeShape;
  attack?: number;
  decay?: number;
  outGain?: number;
  wave?: OscillatorType; // Osc waveshape
  modIdx?: number; // Modulation index/amount
  modWave?: OscillatorType; // Modulation Osc waveshape
  ratio?: number; // Primary - mod frequency ratio
}

interface SynthConstructor extends VoiceConstructor {
  voices: number;
}

class FmVoice {
  private osc: OscillatorNode;
  private vca: GainNode;
  private modOsc: OscillatorNode;
  private modVca: GainNode;
  private volume: GainNode;

  // Envelope
  private shape: EnvelopeShape;
  private attack: number;
  private decay: number;

  //  Params
  private modIdx: number; // Modulation index
  private ratio: number; // Fm ratio
  private outGain: number;

  constructor({
    ctx,
    shape,
    wave,
    attack,
    decay,
    outGain,
    modWave,
    modIdx,
    ratio,
  }: VoiceConstructor) {
    const osc = ctx.createOscillator();
    osc.type = wave ?? 'sine';
    osc.start();

    const vca = ctx.createGain();
    vca.gain.value = 0;

    const modOsc = ctx.createOscillator();
    modOsc.type = modWave ?? 'sine';
    modOsc.start();

    this.modIdx = modIdx ?? 0.1;
    const modVca = ctx.createGain();
    modVca.gain.value = this.modIdx;

    osc.connect(vca);

    modOsc.connect(modVca);
    modVca.connect(osc.frequency);

    this.outGain = outGain ?? 0.5;
    const volume = ctx.createGain();
    volume.gain.value = this.outGain;

    vca.connect(volume);
    volume.connect(ctx.destination);

    this.volume = volume;

    this.osc = osc;
    this.vca = vca;
    this.modOsc = modOsc;
    this.modVca = modVca;
    this.ratio = ratio ?? 2;

    this.shape = shape ?? 'linear';

    this.attack = attack ? attack / 1000 : 0.01;
    this.decay = decay ? decay / 1000 : 0.3;
  }

  midi_to_freq(note: number): number {
    let center = 440;
    return (center / 32) * 2 ** ((note - 9) / 12);
  }

  play(
    ctx: AudioContext,
    { note, velocity, attack, decay, modIdx }: NoteEvent,
  ) {
    const primary_freq = this.midi_to_freq(note);
    const mod_freq = primary_freq * this.ratio;
    if (attack) this.attack = attack / 1000;
    if (decay) this.decay = decay / 1000;
    console.log(this.decay);

    this.osc.frequency.value = primary_freq;
    this.modOsc.frequency.value = mod_freq;

    this.vca.gain.linearRampToValueAtTime(
      velocity / 127,
      ctx.currentTime + this.attack,
    );
    this.modVca.gain.linearRampToValueAtTime(
      (velocity / 127) * modIdx * 1000,
      ctx.currentTime + this.attack,
    );
    this.vca.gain.linearRampToValueAtTime(0, ctx.currentTime + this.decay);
    if (this.shape === 'linear')
      this.modVca.gain.linearRampToValueAtTime(
        0,
        ctx.currentTime + this.attack + this.decay,
      );
    else if (this.shape === 'exponential')
      this.modVca.gain.exponentialRampToValueAtTime(
        1e-16,
        ctx.currentTime + this.attack + this.decay,
      );
  }
}

interface NoteEvent {
  note: number; // Midi note
  velocity: number; // Volume
  attack?: number;
  decay?: number;
  modIdx: number;
}

class VoiceManager {
  private timestamps: number[];

  constructor(capacity: number) {
    this.timestamps = Array.from({ length: capacity });
  }

  use(idx: number): void {
    this.timestamps[idx] = Date.now();
  }

  get LRU(): number {
    const lru = [...this.timestamps]
      .map((n, i) => [n, i])
      .sort((a, b) => b[0] - a[0]);

    return lru[0][1];
  }
}

export class FmSynth {
  private voices: FmVoice[];
  private manager: VoiceManager;

  constructor({ voices, ...rest }: SynthConstructor) {
    this.voices = Array(voices)
      .fill(0)
      .map(_ => {
        return new FmVoice(rest);
      });

    this.manager = new VoiceManager(voices);
  }

  play(ctx: AudioContext, notes: NoteEvent | NoteEvent[]) {
    notes = Array.isArray(notes) ? notes : [notes];
    for (const note of notes) {
      const LRU = this.manager.LRU ?? randInt(0, this.voices.length);
      this.voices[LRU].play(ctx, note);
      this.manager.use(LRU);
    }
  }
}
