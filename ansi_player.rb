#!/usr/bin/env ruby
# frozen_string_literal: true

require "json"
require "zlib"
require_relative "include/key_input"

# Hidden Gems ANSI recording player (CLI)
#
# Compatible with JS widget format:
# {
#   "width": 80,
#   "height": 21,
#   "frames": [
#     { "screen": "\e[...m....", "stdin": {... or string ...} },
#     ...
#   ],
#   "emoji_widths": { ... }   // optional, ignored here
# }
#
# Controls:
#   Space        pause / play
#   ← / →        step back / forward
#   Home / End   jump to start / end
#   , / .        step back / forward (matches your JS bindings)
#   q            quit
#
class HgAnsiCliPlayer
    DEFAULT_FPS = 15.0

    def initialize(path, fps: DEFAULT_FPS, loop: false)
        @path = path
        @fps = fps.to_f
        @loop = loop

        @log = load_log(@path)
        @frames = @log.fetch("frames")
        raise "frames is empty" if @frames.empty?

        @idx = 0
        @paused = false
        @quit = false
        @last_tick = monotonic
    end

    def run
        setup_terminal
        draw(force_status: true)

        until @quit
            handle_key
            tick
            sleep 0.005
        end
    ensure
        restore_terminal
    end

    private

    # ---------- loading ----------

    def load_log(path)
        raw = Zlib::GzipReader.open(path, &:read)
        obj = JSON.parse(raw)

        unless obj.is_a?(Hash) && obj["frames"].is_a?(Array)
            raise "Unexpected log format. Expected object with key 'frames' (array)."
        end

        # Minimal sanity checks for compatibility
        obj["width"]  ||= 80
        obj["height"] ||= 21

        obj
    end

    # ---------- input ----------

    def handle_key
        key = KeyInput.get_key(@paused)
        return unless key

        case key
        when " "
            @paused = !@paused
            @last_tick = monotonic
        when "left", ","
            step(-1)
        when "right", "."
            step(+1)
        when "home"
            jump(0)
        when "end"
            jump(@frames.length - 1)
        when "q", "Q", "esc"
            @quit = true
        end
    end

    # ---------- playback ----------

    def tick
        return if @paused

        now = monotonic
        dt = now - @last_tick
        return if dt < (1.0 / @fps)

        @last_tick = now
        advance(+1)
    end

    def step(delta)
        @paused = true
        advance(delta)
    end

    def jump(i)
        @paused = true
        @idx = clamp(i)
        draw(force_status: true)
    end

    def advance(delta)
        next_idx = @idx + delta

        if next_idx >= @frames.length
            if @loop
                next_idx = 0
            else
                next_idx = @frames.length - 1
                @paused = true
            end
        elsif next_idx < 0
            next_idx = @loop ? (@frames.length - 1) : 0
        end

        @idx = next_idx
        draw
    end

    # ---------- rendering ----------

    def draw(force_status: false)
        frame = @frames[@idx]
        screen = frame["screen"]
        screen = screen.to_s

        STDOUT.write("\e[H")
        STDOUT.write(screen)
        STDOUT.flush
    end

    # ---------- terminal ----------

    def setup_terminal
        # clear, home, hide cursor
        STDOUT.write("\e[2J\e[H\e[?25l")
        STDOUT.flush
    end

    def restore_terminal
        STDOUT.write("\e[0m\e[?25h\n")
        STDOUT.flush
    end

    def terminal_size
        # Prefer actual terminal size if available
        if STDOUT.respond_to?(:winsize)
            r, c = STDOUT.winsize
            return [r, c] if r && c && r > 0 && c > 0
        end
        rows = (ENV["LINES"] || "24").to_i
        cols = (ENV["COLUMNS"] || "80").to_i
        [rows, cols]
    end

    # ---------- helpers ----------

    def monotonic
        Process.clock_gettime(Process::CLOCK_MONOTONIC)
    end

    def clamp(i)
        [[i, 0].max, @frames.length - 1].min
    end
end

# ---- CLI ----
if ARGV.empty?
    warn "Usage: ruby ansi_player.rb recording.json.gz [--fps N] [--loop]"
    exit 1
end

path = ARGV.shift
fps = HgAnsiCliPlayer::DEFAULT_FPS
looping = true
show_stdin = true

while (arg = ARGV.shift)
    case arg
    when "--fps"
        fps = Float(ARGV.shift || raise("Missing value for --fps"))
    when "--loop"
        looping = true
    else
        raise "Unknown argument: #{arg}"
    end
end

HgAnsiCliPlayer.new(path, fps: fps, loop: looping).run
