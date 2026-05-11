#!/usr/bin/env node
/**
 * Flexible ASCII map generator using rot.js (Node only).
 * Adds options to border & fully-connect Cellular maps (and any map).
 *
 * Generators:
 *   arena | divided | eller | icey | cellular | uniform | digger | rogue
 *
 * Examples:
 *   node maze.js -g cellular -w 80 -h 40 --density 0.45 --steps 5
 *   node maze.js -g cellular --no-border            # allow open edges
 *   node maze.js -g digger   --no-connect           # skip connectivity fix
 *   node maze.js -g uniform  --opts '{"roomWidth":[3,9]}'
 */

const path = require('path');
let ROT;
try {
    ROT = require("rot-js");
} catch {
    const localRot = path.join(__dirname, "rot.min.js");
    ROT = require(localRot);
}

/* ==== BEGIN: contest PRNG adapter for rot.js (PCG32 XSH RR) ==== */
class PCG32 {
  constructor(seed = null, streamInitSeq = 54n) {
    this.MASK64 = (1n << 64n) - 1n;
    this.MASK32 = (1n << 32n) - 1n;
    this.MULT   = 6364136223846793005n;
    this.seed(seed, streamInitSeq);
  }
  seed(seed, streamInitSeq = 54n) {
    if (seed == null) seed = this._defaultTimeSeed();
    let initstate = BigInt.asUintN(64, BigInt(seed));
    let initseq   = BigInt.asUintN(64, BigInt(streamInitSeq));
    this.state = 0n;
    this.inc   = BigInt.asUintN(64, (initseq << 1n) | 1n); // ensure odd
    this._nextUint32();
    this.state = BigInt.asUintN(64, this.state + initstate);
    this._nextUint32();
  }
  _rotr32(x, r) {
    r &= 31n;
    return Number(((x >> r) | ((x << ((-r) & 31n)) & this.MASK32)) & this.MASK32) >>> 0;
  }
  _nextUint32() {
    const old = this.state;
    this.state = BigInt.asUintN(64, old * this.MULT + this.inc);
    const xorshifted = ((old >> 18n) ^ old) >> 27n;
    const rot = old >> 59n;
    return this._rotr32(xorshifted & this.MASK32, rot);
  }
  nextUint32() { return this._nextUint32(); }
  nextFloat() { return this._nextUint32() / 2**32; } // [0,1)
  int(n) { // unbiased int in [0,n)
    if (!(Number.isInteger(n) && n > 0)) throw new Error("n must be positive int");
    const t = (2**32) % n;
    for (;;) {
      const r = this._nextUint32() >>> 0;
      if (r >= t) return r % n;
    }
  }
  getState() { return { state: this.state.toString(), inc: this.inc.toString() }; }
  setState(s) {
    this.state = BigInt.asUintN(64, BigInt(s.state));
    this.inc   = BigInt.asUintN(64, BigInt(s.inc));
  }
  clone() { const c = new PCG32(0); c.setState(this.getState()); return c; }
  _defaultTimeSeed() {
    const nowMs = BigInt(Date.now() >>> 0);
    let hi = 0n;
    if (typeof process !== "undefined" && process.hrtime) {
      const [sec, ns] = process.hrtime();
      hi = (BigInt(sec) << 32n) ^ BigInt(ns >>> 0);
    } else if (typeof performance !== "undefined" && performance.now) {
      hi = BigInt(Math.floor(performance.now() * 1e6) >>> 0);
    }
    return BigInt.asUintN(64, (hi << 32n) ^ nowMs);
  }
}

/** Adapter that mimics ROT.RNG’s public API */
// --- Install adapter BEFORE any ROT usage (patch methods, don't replace object) ---
function _tryParseNumericSeed(x) {
  if (typeof x === "number" && Number.isFinite(x)) return x;
  if (typeof x !== "string") return null;
  const s = x.trim().toLowerCase().replace(/n$/, "");
  if (/^0x[0-9a-f]+$/.test(s))  return Number.parseInt(s, 16);
  if (/^[+-]?\d+$/.test(s))     return Number.parseInt(s, 10);
  return null;
}
function _hash64(str) { // FNV-1a 64-bit
  let h = 0xcbf29ce484222325n, p = 0x100000001b3n;
  for (let i = 0; i < str.length; i++) {
    h ^= BigInt(str.charCodeAt(i) & 0xff);
    h = (h * p) & ((1n << 64n) - 1n);
  }
  return h;
}
class RotPcgAdapter {
  constructor(seed = null) { this._pcg = new PCG32(null); this.setSeed(seed); }
  setSeed(seed) {
    if (seed == null) { this._pcg.seed(null); return this; }
    const n = _tryParseNumericSeed(seed);
    this._pcg.seed(n !== null ? n : _hash64(String(seed)));
    return this;
  }
  getUniform() { return this._pcg.nextFloat(); }
  getUniformInt(a, b) { return a + this._pcg.int(b - a + 1); }
  getNormal(mean = 0, stddev = 1) {
    let u = 0, v = 0;
    while (u === 0) u = this.getUniform();
    while (v === 0) v = this.getUniform();
    const z = Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
    return mean + stddev * z;
  }
  getWeightedValue(data) {
    let total = 0, entries = [];
    for (const k in data) { const w = data[k]; if (w > 0) { total += w; entries.push([k, w]); } }
    if (total <= 0) return null;
    let r = this.getUniform() * total;
    for (const [k, w] of entries) { if ((r -= w) < 0) return k; }
    return entries[entries.length - 1][0];
  }
  getState() { return this._pcg.getState(); }
  setState(s) { this._pcg.setState(s); return this; }
  clone()     { const c = new RotPcgAdapter(0); c.setState(this.getState()); return c; }
}

(function installContestRng() {
  // detect --seed 123 | -s 123 | --seed=123 | -s=123
  const argv = process.argv.slice(2);
  let cliSeed;
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a === "--seed" || a === "-s") { cliSeed = argv[i + 1] && !argv[i + 1].startsWith("-") ? argv[i + 1] : ""; break; }
    if (a.startsWith("--seed=")) { cliSeed = a.split("=",2)[1]; break; }
    if (a.startsWith("-s="))     { cliSeed = a.split("=",2)[1]; break; }
  }
  const seed = (cliSeed !== undefined) ? cliSeed
             : (process?.env?.CONTEST_SEED ?? null);

  // IMPORTANT: grab whatever ROT.RNG object rot.js exported and PATCH ITS METHODS
  const target = (typeof ROT !== "undefined" && ROT.RNG) ? ROT.RNG : (globalThis.ROT ??= {}, globalThis.ROT.RNG ??= {});
  const adapter = new RotPcgAdapter(seed);

  // Patch methods to delegate to our adapter (do not replace 'target' itself)
  target.setSeed          = s => { adapter.setSeed(s); return target; };
  target.getUniform       = () => adapter.getUniform();
  target.getUniformInt    = (a,b) => adapter.getUniformInt(a,b);
  target.getNormal        = (m,s) => adapter.getNormal(m,s);
  target.getWeightedValue = d => adapter.getWeightedValue(d);
  target.getState         = () => adapter.getState();
  target.setState         = s => { adapter.setState(s); return target; };
  target.clone            = () => adapter.clone();

  // Optional: debug
  const oldNext = adapter._pcg._nextUint32.bind(adapter._pcg);
  adapter._pcg._nextUint32 = () => { return oldNext(); };

  globalThis.__CONTEST_RNG__ = adapter;
})();
/* ==== END: contest PRNG adapter ==== */

/* ---------------- CLI ---------------- */
const args = process.argv.slice(2);
const getFlag = (name, short) => {
  const i = args.findIndex(a => a === `--${name}` || (short && a === `-${short}`));
  if (i === -1) return undefined;
  const nxt = args[i + 1];
  if (nxt === undefined || nxt.startsWith("-")) return true;
  return nxt;
};
const hasFlag = (name, short) => args.includes(`--${name}`) || (short && args.includes(`-${short}`));

if (hasFlag("help", "h") || hasFlag("usage", "?")) {
  console.log(`
ASCII map generator (rot.js)

Usage:
  node maze.js [--width 61] [--height 31] [--seed 123]
               [--generator cellular] [--opts '{"topology":4}']
               [--density 0.5] [--steps 4]
               [--no-border] [--no-connect]
               [--wall '#'] [--floor '.']

Flags:
  -g, --generator   arena|divided|eller|icey|cellular|uniform|digger|rogue (default: eller)
  -w, --width       map width (default: 41)
  -h, --height      map height (default: 21)
  -s, --seed        RNG seed (number or string)
      --opts        JSON passed to generator constructor
      --wall        wall character (default: █)
      --floor       floor character (default: space)
      --density     (cellular) initial fill [0..1] (default: 0.5)
      --steps       (cellular) automaton iterations (default: 4)
      --no-border   do NOT add a solid border (default is border ON for cellular)
      --no-connect  do NOT enforce full connectivity (default is ON)

Tips:
  - Odd dimensions often look better for mazes.
  - Use --seed to make outputs reproducible.
`);
  process.exit(0);
}

const width  = Math.max(5, parseInt(getFlag("width", "w"), 10) || 41);
const height = Math.max(5, parseInt(getFlag("height", "h"), 10) || 21);
const generator = (getFlag("generator", "g") || "eller").toLowerCase();
const seed = getFlag("seed", "s");
const wallChar  = (getFlag("wall")  || "█");
const floorChar = (getFlag("floor") || " ");

const optsJSON = getFlag("opts");
let ctorOpts = {};
if (typeof optsJSON === "string") {
  try { ctorOpts = JSON.parse(optsJSON); }
  catch { console.error("Could not parse --opts JSON. Using empty options."); }
}

// Cellular params
const density = clamp01(parseFloat(getFlag("density")) || 0.5);
const steps   = Math.max(0, parseInt(getFlag("steps"), 10) || 4);

// Behavior toggles (defaults tailored to your request)
const addBorder   = !hasFlag("no-border");   // default true
const doConnect   = !hasFlag("no-connect");  // default true

// (Kept for compatibility; adapter already installed above. This will just reseed it.)
if (seed !== undefined) {
  const n = Number(seed);
  ROT.RNG.setSeed(Number.isNaN(n) ? seed : n);
}

/* ---------------- Map selection ---------------- */
function buildMap(kind) {
  switch (kind) {
    case "arena":    return new ROT.Map.Arena(width, height, ctorOpts);
    case "divided":  return new ROT.Map.DividedMaze(width, height, ctorOpts);
    case "eller":    return new ROT.Map.EllerMaze(width, height, ctorOpts);
    case "icey":     return new ROT.Map.IceyMaze(width, height, ctorOpts);
    case "uniform":  return new ROT.Map.Uniform(width, height, ctorOpts);
    case "digger":   return new ROT.Map.Digger(width, height, ctorOpts);
    case "rogue":    return new ROT.Map.Rogue(width, height, ctorOpts);
    case "cellular": return new ROT.Map.Cellular(width, height, ctorOpts);
    default:
      console.error(`Unknown generator "${kind}". Use --help for options.`);
      process.exit(2);
  }
}

/* ---------------- Generate ---------------- */
const grid = Array.from({ length: height }, () => Array(width).fill(1)); // 1=wall, 0=floor
const write = (x, y, v) => { grid[y][x] = v; };

const map = buildMap(generator);

if (generator === "rogue") {
  // Use the safe wrapper instead of map.create directly
  const ok = createRogueSafe(width, height, ctorOpts, (x, y, v) => write(x, y, v));
  if (!ok) {
    // Fallback: use Digger (rogue-like dungeons) if Rogue keeps failing
    const dig = new ROT.Map.Digger(width, height, ctorOpts);
    dig.create((x, y, v) => write(x, y, v));
  }

  if (addBorder) addSolidBorder(grid);
  if (doConnect) ensureConnectivity(grid);

} else if (generator === "cellular") {
  // (keep your fixed cellular flow)
  if (typeof map.randomize === "function") map.randomize(density);
  for (let i = 0; i < steps; i++) map.create();
  map.create((x, y, v) => write(x, y, v));
  if (addBorder) addSolidBorder(grid);
  if (doConnect) ensureConnectivity(grid);

} else {
  // all other generators
  map.create((x, y, v) => write(x, y, v));
  if (addBorder) addSolidBorder(grid);
  if (doConnect) ensureConnectivity(grid);
}


/* ---------------- Output ---------------- */
for (let y = 0; y < height; y++) {
  let line = "";
  for (let x = 0; x < width; x++) line += grid[y][x] ? wallChar : floorChar;
  console.log(line);
}

/* ---------------- Helpers ---------------- */

function clamp01(n) { if (Number.isNaN(n)) return 0.5; return Math.max(0, Math.min(1, n)); }

function addSolidBorder(g) {
  const H = g.length, W = g[0].length;
  for (let x = 0; x < W; x++) { g[0][x] = 1; g[H-1][x] = 1; }
  for (let y = 0; y < H; y++) { g[y][0] = 1; g[y][W-1] = 1; }
}

// 1) Add this helper near your other helpers
function createRogueSafe(width, height, ctorOpts, write) {
  const candidates = [
    // Try a few proven-stable cell grids & room ranges
    { cellWidth: 3, cellHeight: 3, roomWidth: [3, 9], roomHeight: [3, 7] },
    { cellWidth: 3, cellHeight: 2, roomWidth: [3, 8], roomHeight: [3, 6] },
    { cellWidth: 2, cellHeight: 3, roomWidth: [3, 8], roomHeight: [3, 6] },
    { cellWidth: 4, cellHeight: 3, roomWidth: [3, 7], roomHeight: [3, 6] },
  ];

  // If user provided explicit Rogue opts, try those first
  const first = {};
  if (ctorOpts.cellWidth)  first.cellWidth  = ctorOpts.cellWidth;
  if (ctorOpts.cellHeight) first.cellHeight = ctorOpts.cellHeight;
  if (ctorOpts.roomWidth)  first.roomWidth  = ctorOpts.roomWidth;
  if (ctorOpts.roomHeight) first.roomHeight = ctorOpts.roomHeight;
  const tryList = Object.keys(first).length ? [first, ...candidates] : candidates;

  // Try each option set and up to a few seeds for each (keep user seed stable if set)
  const seedsToTry = [null, "rogue-fallback-1", "rogue-fallback-2"];

  for (const opts of tryList) {
    for (const s of seedsToTry) {
      const saved = ROT.RNG.getState && ROT.RNG.getState();
      if (s && ROT.RNG.setSeed) ROT.RNG.setSeed(s);
      try {
        const rogue = new ROT.Map.Rogue(width, height, { ...ctorOpts, ...opts });
        rogue.create((x, y, v) => write(x, y, v)); // may throw
        if (saved && ROT.RNG.setState) ROT.RNG.setState(saved);
        return true; // success
      } catch (e) {
        // restore RNG and try next combo
        if (saved && ROT.RNG.setState) ROT.RNG.setState(saved);
        continue;
      }
    }
  }
  return false; // all attempts failed
}


/**
 * Ensure the entire floor space is one connected component by carving
 * simple L-shaped corridors between components. Border is preserved.
 */
function ensureConnectivity(g) {
  const H = g.length, W = g[0].length;
  const {labels, components} = labelComponents(g);
  if (components.length <= 1) return;

  // Choose the largest component as the main region
  components.sort((a, b) => b.cells.length - a.cells.length);
  let main = components[0];

  for (let i = 1; i < components.length; i++) {
    const comp = components[i];
    // Find closest pair between 'main' and 'comp' (Manhattan distance)
    const {a, b} = closestPair(main.cells, comp.cells);
    carveTunnel(g, a.x, a.y, b.x, b.y);
    // Merge: relabel freshly carved tiles as part of main; recompute main cells quickly
    // (Simpler: relabel everything again)
    const relabeled = labelComponents(g);
    relabeled.components.sort((x, y) => y.cells.length - x.cells.length);
    main = relabeled.components[0];
  }
}

/** Label connected floor components (4-neighborhood). */
function labelComponents(g) {
  const H = g.length, W = g[0].length;
  const labels = Array.from({ length: H }, () => Array(W).fill(-1));
  const comps = [];
  let id = 0;

  for (let y = 0; y < H; y++) for (let x = 0; x < W; x++) {
    if (g[y][x] !== 0 || labels[y][x] !== -1) continue;
    // BFS flood fill
    const q = [{x, y}];
    labels[y][x] = id;
    const cells = [{x, y}];
    while (q.length) {
      const p = q.shift();
      for (const [nx, ny] of neighbors4(p.x, p.y, W, H)) {
        if (g[ny][nx] === 0 && labels[ny][nx] === -1) {
          labels[ny][nx] = id;
          q.push({x: nx, y: ny});
          cells.push({x: nx, y: ny});
        }
      }
    }
    comps.push({ id, cells });
    id++;
  }
  return { labels, components: comps };
}

function neighbors4(x, y, W, H) {
  const out = [];
  if (x > 0) out.push([x-1, y]);
  if (x < W-1) out.push([x+1, y]);
  if (y > 0) out.push([x, y-1]);
  if (y < H-1) out.push([x, y+1]);
  return out;
}

/** Find the closest pair of cells between two sets (O(n*m), fine for typical sizes). */
function closestPair(aCells, bCells) {
  let best = { d: Infinity, a: null, b: null };
  for (const a of aCells) {
    for (const b of bCells) {
      const d = Math.abs(a.x - b.x) + Math.abs(a.y - b.y);
      if (d < best.d) best = { d, a, b };
    }
  }
  return best;
}

/** Carve an L-shaped tunnel between two points, staying inside the border. */
function carveTunnel(g, x1, y1, x2, y2) {
  const H = g.length, W = g[0].length;
  const carve = (x, y) => {
    if (x <= 0 || y <= 0 || x >= W-1 || y >= H-1) return; // keep border solid
    g[y][x] = 0;
  };
  // Randomize bend direction a bit for variety
  const horizontalFirst = ROT.RNG.getUniform() < 0.5;
  if (horizontalFirst) {
    stepLine(x1, x2, x => carve(x, y1));
    stepLine(y1, y2, y => carve(x2, y));
  } else {
    stepLine(y1, y2, y => carve(x1, y));
    stepLine(x1, x2, x => carve(x, y2));
  }
}

function stepLine(a, b, visit) {
  const dir = a <= b ? 1 : -1;
  for (let v = a; v !== b; v += dir) visit(v);
  visit(b);
}
