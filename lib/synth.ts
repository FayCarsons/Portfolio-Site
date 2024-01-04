import { buffer } from "stream/consumers";

type EnvelopeShape = "linear" | "exponential";

interface VoiceConstructor {
  ctx: AudioContext;
  shape?: EnvelopeShape;
  decay?: number;
  wave?: OscillatorType; // Osc waveshape
  mod_idx?: number; // Modulation index/amount
  mod_wave?: OscillatorType; // Modulation Osc waveshape
}

type Enumerate<
  N extends number,
  Acc extends number[] = [],
> = Acc["length"] extends N
  ? Acc[number]
  : Enumerate<N, [...Acc, Acc["length"]]>;

type IntRange<F extends number, T extends number> = Exclude<
  Enumerate<T>,
  Enumerate<F>
>;

interface SynthConstructor extends VoiceConstructor {
  voices: IntRange<1, 8>;
}

interface NoteEvent {
  pitch: IntRange<0, 127>; // Midi note
  gain: number; // Volume
  mod_idx: number;
}

class Voice {
  private osc: OscillatorNode;
  private vca: GainNode;
  private mod_osc: OscillatorNode;
  private mod_vca: GainNode;

  private shape: EnvelopeShape; // Shape of modulation env
  private decay: number;

  private mod_idx: number; // Modulation index/amount
  private out_gain: number;

  constructor({
    ctx,
    shape,
    wave,
    decay,
    mod_wave,
    mod_idx,
  }: VoiceConstructor) {
    const osc = ctx.createOscillator();
    osc.type = wave || "sine";
    osc.start();

    const vca = ctx.createGain();

    const mod_osc = ctx.createOscillator();
    mod_osc.type = mod_wave || "sine";
    mod_osc.start();

    this.mod_idx = mod_idx || 0.1;
    const mod_vca = ctx.createGain();
    mod_vca.gain.value = this.mod_idx;

    osc.connect(vca);
    vca.connect(ctx.destination);

    mod_osc.connect(mod_vca);
    mod_vca.connect(osc.frequency);

    this.osc = osc;
    this.vca = vca;
    this.mod_osc = mod_osc;
    this.mod_vca = mod_vca;

    this.shape = shape || "linear";
    this.out_gain = 1;
    this.decay = decay || 0.3;
  }

  play() {}
}
