#!/usr/bin/env node

const readline = require("readline");

function mulberry32(seed) {
  let t = seed >>> 0;
  return function() {
    t += 0x6D2B79F5;
    let r = Math.imul(t ^ (t >>> 15), t | 1);
    r ^= r + Math.imul(r ^ (r >>> 7), r | 61);
    return ((r ^ (r >>> 14)) >>> 0) / 4294967296;
  };
}

const rng = mulberry32(1);

const moves = ["N", "S", "E", "W"];
let firstTick = true;

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
});

rl.on("line", (line) => {
  const data = JSON.parse(line);

  if (firstTick) {
    const { width, height } = data.config;
    console.error(`Random walker (Node.js) launching on a ${width}x${height} map`);
  }

  const move = moves[Math.floor(rng() * moves.length)];
  console.log(move);

  firstTick = false;
});
