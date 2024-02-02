import { init_shaders } from "../sketches/clouds/clouds";
import { FmSynth } from "../lib/synth";
import { rand_int, rand_nth } from "../lib/utilities";

init_shaders();

/* 
const CMAJ = [0, 2, 4, 7, 9, 11, 14];
const OFFSET = 48;

let playing = false;

const init = () => {
  if (playing) return;
  else playing = true;
  const ctx = new AudioContext();
  const synth = new FmSynth({
    shape: 'linear',
    voices: 8,
    ctx,
  });
  const make_note = (_: number) => {
    return {
      note: rand_nth(CMAJ) + OFFSET + 12 * rand_int(0, 3),
      velocity: rand_int(32, 128),
      mod_idx: Math.random(),
      attack: 20,
      decay: rand_int(1000, 5000),
    };
  };

  const play = () => {
    console.log("playing!");
    const num_notes = 1 + rand_int(0, 3);
    const notes = Array(num_notes).fill(0).map(make_note);
    console.log("note array: ", notes);
    synth.play(ctx, notes);
  };
  const metro = setInterval(play, 2000);
};

const dialog = document.getElementById("permission") as HTMLDialogElement;
const agree_button = document.getElementById("agree") as HTMLButtonElement;

agree_button.onclick = () => {
  console.log("yes");
  init();
  dialog.close();
};

const disagree_button = document.getElementById(
  "disagree"
) as HTMLButtonElement;
disagree_button.onclick = () => {
  console.log("no");
  dialog.close();
};

dialog.showModal();
*/