#!/usr/bin/env node
/* ansi_render_to_video_runs.js — skia-canvas + ffmpeg (streaming, faster, run-batched)
* - Stream one buffer per source frame (no JS dup)
* - Generate PTS on input (-fflags +genpts); -framerate (input) + -r (output)
* - Cached fonts, skip default BG tiles
* - Row-wise style runs to minimize font/fillStyle churn
* - Prefer raw canvas buffer; fallback to getImageData
*/

const fs = require("fs");
const { spawn, execFileSync } = require("child_process");
const { parseArgs } = require("node:util");
const { Canvas, FontLibrary } = require("skia-canvas");

// --- helper: pick encoder ---
function pickEncoder(requested = "auto") {
    const want = (requested || "").toLowerCase();
    if (want === "x264" || want === "libx264") return { name: "libx264", kind: "cpu" };
    if (want === "h264_nvenc" || want === "nvenc_h264") return { name: "h264_nvenc", kind: "nvenc" };
    if (want === "hevc_nvenc" || want === "nvenc_hevc") return { name: "hevc_nvenc", kind: "nvenc" };

    try {
        const encs = execFileSync("ffmpeg", ["-hide_banner", "-encoders"], { encoding: "utf8" });
        if (/h264_nvenc/.test(encs)) return { name: "h264_nvenc", kind: "nvenc" };
        if (/hevc_nvenc/.test(encs)) return { name: "hevc_nvenc", kind: "nvenc" };
    } catch (_) { }
    return { name: "libx264", kind: "cpu" };
}

// ---------- CLI ----------
const { values, positionals } = parseArgs({
    options: {
        "font-regular": { type: "string", default: "../static/include/fonts/IBM_Plex_Mono/IBMPlexMono-Regular.ttf" },
        "font-bold": { type: "string", default: "../static/include/fonts/IBM_Plex_Mono/IBMPlexMono-Bold.ttf" },
        "emoji-font": { type: "string", default: "../static/include/fonts/Noto_Color_Emoji/NotoColorEmoji-Regular.ttf" },
        "fontname": { type: "string", default: "IBM Plex Mono" },
        "fontsize": { type: "string", default: "32" },
        "lineheight": { type: "string", default: "1.25" },
        "pad": { type: "string", default: "0" },
        "in-fps": { type: "string", default: "15" },
        "out-fps": { type: "string", default: "15" },
        "encoder": { type: "string", default: "nvenc" },
        "crf": { type: "string", default: "18" },
        "preset": { type: "string", default: "fast" },
        "keyint": { type: "string", default: "5" },
        "all-keyframes": { type: "boolean", default: false },
        "bframes": { type: "string", default: "0" },
    },
    allowPositionals: true,
});

if (positionals.length < 2) {
    console.error(
        "Usage: node ansi_render_to_video_runs.js <input.json> <out.mp4> " +
        "[--font-regular path.ttf] [--font-bold path.ttf] [--emoji-font path.ttf] " +
        "[--fontname Name] [--fontsize 24] [--lineheight 1.25] [--pad 0] " +
        "[--in-fps 15] [--out-fps 30] [--encoder auto|x264|h264_nvenc|hevc_nvenc] " +
        "[--crf 18] [--preset veryslow|p1..p7] [--keyint 5] [--all-keyframes] [--bframes 0]"
    );
    process.exit(1);
}

const INPUT = positionals[0];
const OUTPUT = positionals[1];
const FONT_REG = values["font-regular"];
const FONT_BOLD = values["font-bold"];
const EMOJI_TTF = values["emoji-font"];
const FONTNAME = values.fontname;
const FONT_SIZE = parseFloat(values.fontsize);
const LINE_H = parseFloat(values.lineheight);
const PAD = parseFloat(values.pad);
const IN_FPS = parseFloat(values["in-fps"]);
const OUT_FPS = parseFloat(values["out-fps"]);
const CRF = parseInt(values.crf, 10);
const PRESET = values.preset;

// Keyframe settings. Small GOPs make browser seeking much faster.
// --keyint 5 means one keyframe every 5 frames.
// --all-keyframes is equivalent to --keyint 1.
const KEYINT = values["all-keyframes"]
    ? 1
    : Math.max(1, parseInt(values.keyint, 10) || 5);
const BFRAMES = Math.max(0, parseInt(values.bframes, 10) || 0);

// ---------- Fonts ----------
if (FONT_REG) FontLibrary.use(FONTNAME, [{ path: FONT_REG, weight: 400 }]);
if (FONT_BOLD) FontLibrary.use(FONTNAME, [{ path: FONT_BOLD, weight: 700 }]);
// If only one font was provided, register for both weights to avoid fallback
if (!FONT_REG && !FONT_BOLD && values.font) {
    FontLibrary.use(FONTNAME, [{ path: values.font, weight: 400 }, { path: values.font, weight: 700 }]);
}
if (EMOJI_TTF) {
    FontLibrary.use("HG-Emoji", [
        { path: EMOJI_TTF, weight: 400 },
        { path: EMOJI_TTF, weight: 700 }, // map bold to same file
    ]);
}

// ---------- ANSI helpers ----------
const DEFAULT_FG = 0xd0d0d0, DEFAULT_BG = 0x000000;

const ANSI16 = {
    30: 0x000000, 31: 0xcd0000, 32: 0x00cd00, 33: 0xcdcd00,
    34: 0x1e90ff, 35: 0xcd00cd, 36: 0x00cdcd, 37: 0xe5e5e5,
    90: 0x7f7f7f, 91: 0xff0000, 92: 0x00ff00, 93: 0xffff00,
    94: 0x5c5cff, 95: 0xff00ff, 96: 0x00ffff, 97: 0xffffff
};
function xterm256(n) {
    if (n < 16) {
        const map = [0x000000, 0x800000, 0x008000, 0x808000, 0x000080, 0x800080, 0x008080, 0xc0c0c0, 0x808080, 0xff0000, 0x00ff00, 0xffff00, 0x0000ff, 0xff00ff, 0x00ffff, 0xffffff];
        return map[n];
    }
    if (n <= 231) {
        const c = n - 16, r = Math.floor(c / 36), g = Math.floor((c % 36) / 6), b = c % 6;
        const step = [0x00, 0x5f, 0x87, 0xaf, 0xd7, 0xff];
        return (step[r] << 16) | (step[g] << 8) | (step[b]);
    }
    const level = 8 + (n - 232) * 10;
    return (level << 16) | (level << 8) | level;
}
const graphemes = (s) => {
    if ('Segmenter' in Intl) {
        const seg = new Intl.Segmenter(undefined, { granularity: 'grapheme' });
        return Array.from(seg.segment(s), x => x.segment);
    }
    return Array.from(s);
};

// NOTE: now takes emojiWidths as a second argument and prefers Ruby’s mapping
function wcwidthGrapheme(g, emojiWidths) {
    const cp = g.codePointAt(0);
    if (cp == null) return 0;
    if (cp <= 0x1f || (cp >= 0x7f && cp <= 0x9f)) return 0;

    // 1) Trust Ruby’s emoji_widths if present
    if (emojiWidths) {
        if (Object.prototype.hasOwnProperty.call(emojiWidths, g)) {
            const w = emojiWidths[g];
            if (Number.isInteger(w)) return w;
        }
        // also try without VS-16, e.g. "⛏️" → "⛏"
        const base = g.replace(/\uFE0F/g, '');
        if (base !== g && Object.prototype.hasOwnProperty.call(emojiWidths, base)) {
            const w = emojiWidths[base];
            if (Number.isInteger(w)) return w;
        }
    }

    // 2) Fallback to heuristic
    const isEmoji = /\p{Extended_Pictographic}/u.test(g);
    const eastWide = (cp >= 0x1100 && (cp <= 0x115f || cp === 0x2329 || cp === 0x232a ||
        (cp >= 0x2e80 && cp <= 0xa4cf && cp !== 0x303f) || (cp >= 0xac00 && cp <= 0xd7a3) ||
        (cp >= 0xf900 && cp <= 0xfaff) || (cp >= 0xfe10 && cp <= 0xfe19) || (cp >= 0xfe30 && cp <= 0xfe6f) ||
        (cp >= 0xff00 && cp <= 0xff60) || (cp >= 0xffe0 && cp <= 0xffe6)));

        const isAstralEmoji = isEmoji && cp > 0xFFFF;
        return (eastWide || isAstralEmoji) ? 2 : 1;
    }
    function rgbToCss(v) { return `rgb(${v >> 16 & 255},${v >> 8 & 255},${v & 255})`; }

    function ansiToGrid(ansi, opts = {}) {
        const COLS = Number.isInteger(opts.cols) ? opts.cols : 80;
        const ROWS = Number.isInteger(opts.rows) ? opts.rows : 21;
        const defFG = opts.defaultFg ?? DEFAULT_FG;
        const defBG = opts.defaultBg ?? DEFAULT_BG;
        const emojiWidths = opts.emojiWidths || null;

        const grid = Array.from({ length: ROWS }, () => Array.from({ length: COLS }, () => ({
            ch: ' ', fg: defFG, bg: defBG, bold: false, width: 1
        })));
        let style = { fg: defFG, bg: defBG, bold: false };
        let r = 0, c = 0;

        function applySGR(params) {
            if (params.length === 0) { style = { fg: defFG, bg: defBG, bold: false }; return; }
            for (let i = 0; i < params.length; i++) {
                const p = params[i];
                if (p === 0) { style = { fg: defFG, bg: defBG, bold: false }; }
                else if (p === 1) { style.bold = true; }
                else if (p === 22) { style.bold = false; }
                else if ((p >= 30 && p <= 37) || (p >= 90 && p <= 97)) { style.fg = ANSI16[p] ?? style.fg; }
                else if ((p >= 40 && p <= 47) || (p >= 100 && p <= 107)) {
                    const bgCode = p - 10 + ((p >= 100) ? -60 : 0);
                    style.bg = ANSI16[bgCode] ?? style.bg;
                }
                else if (p === 38 || p === 48) {
                    const isFG = (p === 38); const mode = params[++i];
                    if (mode === 5) {
                        const idx = params[++i];
                        if (idx != null) {
                            const v = xterm256(idx);
                            isFG ? style.fg = v : style.bg = v;
                        }
                    } else if (mode === 2) {
                        const rr = params[++i], gg = params[++i], bb = params[++i];
                        if ([rr, gg, bb].every(v => Number.isInteger(v) && v >= 0 && v <= 255)) {
                            const v = (rr << 16) | (gg << 8) | bb;
                            isFG ? style.fg = v : style.bg = v;
                        }
                    }
                }
            }
        }

        function writeText(txt) {
            const lines = txt.split(/\r?\n/);
            for (let li = 0; li < lines.length; li++) {
                for (const g of graphemes(lines[li])) {
                    const w = wcwidthGrapheme(g, emojiWidths);
                    if (w === 0) continue;
                    if (c >= COLS) { r++; c = 0; }
                    if (r >= ROWS) return;
                    if (w === 2 && c === COLS - 1) { c++; continue; }
                    const cell = grid[r][c];
                    cell.ch = g; cell.fg = style.fg; cell.bg = style.bg; cell.bold = style.bold; cell.width = w;
                    if (w === 2) {
                        const follower = grid[r][c + 1];
                        follower.ch = ''; follower.fg = style.fg; follower.bg = style.bg; follower.bold = style.bold; follower.width = 0;
                    }
                    c += w;
                }
                if (li < lines.length - 1) { r++; c = 0; if (r >= ROWS) return; }
            }
        }

        const re = /\x1b\[([0-9;]*)m/g;
        let i = 0, m;
        while ((m = re.exec(ansi)) !== null) {
            if (m.index > i) writeText(ansi.slice(i, m.index));
            const params = m[1] === '' ? [] : m[1].split(';').map(n => parseInt(n, 10));
            applySGR(params);
            i = re.lastIndex;
            if (r >= ROWS) break;
        }
        if (i < ansi.length && r < ROWS) writeText(ansi.slice(i));
        return grid;
    }

    // ---------- metrics ----------
    function computeMetrics(ctx, fontFamily, fontSize, lineH) {
        ctx.font = `400 ${fontSize}px "HG-Emoji", "${fontFamily}", ui-monospace, Menlo, Consolas, "Liberation Mono", monospace`;
        const m = ctx.measureText("Mg");
        const ascent = m.actualBoundingBoxAscent ?? fontSize * 0.8;
        const descent = m.actualBoundingBoxDescent ?? fontSize * 0.2;
        const glyphH = ascent + descent;
        const cellH = fontSize * lineH;
        const yOffset = Math.max(0, (cellH - glyphH) / 2) - fontSize * 0.25;
        const cellW = ctx.measureText("M").width;
        return { cellW, cellH, yOffset };
    }

    // ---------- draw one grid (run-batched) ----------
    function drawGrid(ctx, grid, metrics, pad, fontFamily, fontSize) {
        const ROWS = grid.length;
        const COLS = ROWS ? grid[0].length : 0;
        const { cellW, cellH, yOffset } = metrics;

        // backgrounds — clear to black once (outside), draw only non-default segments
        for (let y = 0; y < ROWS; y++) {
            let x = 0;
            while (x < COLS) {
                const bg = grid[y][x].bg;
                if (bg === DEFAULT_BG) { x++; continue; }
                let len = 1;
                while ((x + len) < COLS && grid[y][x + len].bg === bg) len++;
                if (len > 0) {
                    if (ctx._lastFillStyle !== bg) {
                        ctx.fillStyle = rgbToCss(bg);
                        ctx._lastFillStyle = bg;
                    }
                    ctx.fillRect(pad + x * cellW, pad + y * cellH, len * cellW + 1, cellH + 1);
                }
                x += len;
            }
        }

        // glyphs — batch by (bold, fg) "style runs" per row; draw cells within a run
        ctx.textBaseline = "top";
        const fontNormal = `400 ${fontSize}px "${fontFamily}", "HG-Emoji", ui-monospace, Menlo, Consolas, "Liberation Mono", monospace`;
        const fontBold = `700 ${fontSize}px "${fontFamily}", "HG-Emoji", ui-monospace, Menlo, Consolas, "Liberation Mono", monospace`;

        for (let y = 0; y < ROWS; y++) {
            let x = 0;
            while (x < COLS) {
                // skip empties/followers fast
                while (x < COLS && !grid[y][x].ch) x++;
                if (x >= COLS) break;

                const startX = x;
                const base = grid[y][x];
                const runBold = base.bold;
                const runFG = base.fg;

                // extend run while cells share style and are visible (ch truthy)
                let endX = x;
                while (endX < COLS) {
                    const c = grid[y][endX];
                    if (!c.ch || c.bold !== runBold || c.fg !== runFG) break;
                    endX++;
                }

                // set style once per run
                ctx.font = runBold ? fontBold : fontNormal;
                ctx.fillStyle = rgbToCss(runFG);

                // draw cells in the run (still one fillText per grapheme to keep perfect cell alignment)
                for (let i = startX; i < endX; i++) {
                    const cell = grid[y][i];
                    if (!cell.ch) continue;
                    ctx.fillText(cell.ch, pad + i * cellW, pad + y * cellH + yOffset);
                }

                x = endX;
            }
        }
    }

    // ---------- main ----------
    (async function main() {
        const raw = fs.readFileSync(INPUT, "utf8");
        const data = JSON.parse(raw);
        const cols = data.width;
        const rows = data.height;
        const frames = data.frames;
        const emojiWidths = data.emoji_widths || null;

        if (!Array.isArray(frames) || frames.length === 0) {
            throw new Error("Input JSON must contain frames[]");
        }

        // Set up canvas & metrics once
        const probeCanvas = new Canvas(16, 16);
        const probeCtx = probeCanvas.getContext("2d");
        const metrics = computeMetrics(probeCtx, FONTNAME, FONT_SIZE, LINE_H);
        let W = Math.ceil(cols * metrics.cellW + PAD * 2);
        let H = Math.ceil(rows * metrics.cellH + PAD * 2);
        if (W % 2 !== 0) W++;
        if (H % 2 !== 0) H++;

        const canvas = new Canvas(W, H);
        const ctx = canvas.getContext("2d");

        const enc = pickEncoder(values.encoder);

        // Input: true framerate; generate PTS for raw pipe
        const ffIn = [
            "-y",
            "-fflags", "+genpts",
            "-f", "rawvideo",
            "-pix_fmt", "rgba",   // If your raw is BGRA, switch this to "bgra"
            "-s", `${W}x${H}`,
            "-framerate", String(IN_FPS),
            "-i", "-"
        ];

        // Output: request OUT_FPS on encoder side (no fps filter to avoid buffering)
        let ffOut;
        if (enc.kind === "nvenc") {
            const cq = Number.isFinite(+values.crf) ? +values.crf : 19;     // reuse --crf as NVENC CQ
            const presetNv = (/^p[1-7]$/.test(values.preset) ? values.preset : "p5");
            const codec = enc.name;                                         // h264_nvenc / hevc_nvenc
            const bFrameArgs = BFRAMES > 0
                ? ["-rc-lookahead", "20", "-b_ref_mode", "middle", "-bf", String(BFRAMES)]
                : ["-bf", "0"];

            ffOut = [
                "-an",
                "-r", String(OUT_FPS),
                "-c:v", codec,
                "-preset", presetNv,
                "-rc", "vbr",
                "-tune", "hq",
                "-multipass", "fullres",
                "-cq", String(cq),
                "-b:v", "0",
                "-spatial_aq", "1",
                "-temporal_aq", "1",

                // Browser scrubbing is fastest when keyframes are close together.
                // KEYINT=1 means every frame is a keyframe; KEYINT=5 is a good compromise.
                "-g", String(KEYINT),
                "-forced-idr", "1",
                ...bFrameArgs,

                "-pix_fmt", "yuv420p",
                "-movflags", "+faststart",
                OUTPUT
            ];
        } else {
            ffOut = [
                "-an",
                "-r", String(OUT_FPS),
                "-c:v", "libx264",
                "-preset", PRESET,
                "-crf", String(CRF),

                // Fixed, short GOP for fast browser seeking.
                "-g", String(KEYINT),
                "-keyint_min", String(KEYINT),
                "-sc_threshold", "0",
                "-bf", String(BFRAMES),

                "-pix_fmt", "yuv420p",
                "-movflags", "+faststart",
                OUTPUT
            ];
        }

        // const ff = spawn("ffmpeg", [...ffIn, ...ffOut], { stdio: ["pipe", "inherit", "inherit"] });
        // ff.on("error", (e) => { console.error("Failed to start ffmpeg:", e); process.exit(1); });

        const ffArgs = [...ffIn, ...ffOut];

        // Run: taskset -c 0 ffmpeg <args...>
        const ff = spawn("taskset", ["-c", "0", "ffmpeg", ...ffArgs], {
            stdio: ["pipe", "inherit", "inherit"],
        });

        ff.on("error", (e) => {
            console.error("Failed to start ffmpeg via taskset:", e);
            process.exit(1);
        });

        // Render → write once per source frame
        for (let i = 0; i < frames.length; i++) {
            const grid = ansiToGrid(frames[i].screen, { cols, rows, emojiWidths });

            // clear to background
            ctx._lastFillStyle = null; // reset cached fillStyle for BG pass
            ctx.fillStyle = "#000000";
            ctx.fillRect(0, 0, W, H);

            drawGrid(ctx, grid, metrics, PAD, FONTNAME, FONT_SIZE);

            // Prefer raw buffer; fallback to getImageData
            let buf = null;
            try {
                if (typeof canvas.toBuffer === "function") {
                    let rawMaybe = canvas.toBuffer("raw");     // may be Buffer/Uint8Array or Promise thereof
                    if (rawMaybe && typeof rawMaybe.then === "function") rawMaybe = await rawMaybe;
                    if (rawMaybe) {
                        buf = Buffer.isBuffer(rawMaybe)
                        ? rawMaybe
                        : Buffer.from(rawMaybe.buffer, rawMaybe.byteOffset, rawMaybe.byteLength);
                    }
                }
            } catch (_) { }
            if (!buf) {
                const img = ctx.getImageData(0, 0, W, H).data; // Uint8ClampedArray (RGBA)
                buf = Buffer.from(img.buffer, img.byteOffset, img.byteLength);
            }

            const ok = ff.stdin.write(buf);
            if (!ok) await new Promise(res => ff.stdin.once("drain", res));
            // Let the event loop breathe a tiny bit for smoother pipe backpressure
            await new Promise(res => setImmediate(res));
        }

        ff.stdin.end();
        ff.on("close", (code) => {
            if (code === 0) {
                console.log(`Wrote ${OUTPUT} at ${OUT_FPS} fps (rendered ${frames.length} frames @ ${IN_FPS}). Size: ${W}x${H}. Keyint: ${KEYINT}, B-frames: ${BFRAMES}`);
            } else {
                console.error("ffmpeg exited with code", code);
                process.exit(code || 1);
            }
        });
    })().catch((e) => {
        console.error(e);
        process.exit(1);
    });
