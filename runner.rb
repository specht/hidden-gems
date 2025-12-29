#!/usr/bin/env ruby

$LOAD_PATH.unshift File.expand_path("include/unicode-emoji-4.0.4/lib", __dir__)
$LOAD_PATH.unshift File.expand_path("include/unicode-display_width-3.1.5/lib", __dir__)
$LOAD_PATH.unshift File.expand_path("include/paint-2.3.0/lib", __dir__)

require './include/fov_angle.rb'
require './include/key_input.rb'
require './include/pcg32.rb'
require './include/timings.rb'
if Gem.win_platform?
    require 'fiddle/import'
end

require 'date'
require 'digest'
require 'fileutils'
require 'json'
require 'open3'
require 'optparse'
require 'paint'
require 'set'
require 'stringio'
require 'strscan'
require 'unicode/display_width'
require 'yaml'
require 'zlib'

SOFT_LIMIT            = 0.100
HARD_LIMIT            = 0.200
HARD_LIMIT_FIRST_TICK = 20.0
OVERTIME_BUDGET       = 1.5

OPTIONS_FOR_BOT = %w(stage_key width height generator max_ticks emit_signals vis_radius max_gems
                     gem_spawn_rate gem_ttl signal_radius signal_cutoff signal_noise
                     signal_quantization signal_fade enable_debug timeout_scale)

ANSI = /\e\[[0-9;:<>?]*[@-~]/

$timings = Timings.new

def median(array)
    return nil if array.empty?
    sorted = array.sort
    mid = sorted.length / 2
    if sorted.length.odd?
        sorted[mid]
    else
        (sorted[mid-1] + sorted[mid]) / 2.0
    end
end

def mean(array)
    return nil if array.empty?
    array.sum(0.0) / array.size
end

Paint.mode = 0xffffff

if Gem.win_platform?
    module Kernel32
        extend Fiddle::Importer
        dlload 'kernel32'
        extern 'void* GetStdHandle(int)'
        extern 'int FlushConsoleInputBuffer(void*)'
    end

    STD_INPUT_HANDLE = -10

    def flush_console_input_buffer
        h_in = Kernel32.GetStdHandle(STD_INPUT_HANDLE)
        Kernel32.FlushConsoleInputBuffer(h_in)
    end

    at_exit { flush_console_input_buffer }
end

class Runner

    UI_BACKGROUND_TOP = '#1d5479'
    UI_FOREGROUND_TOP = '#eeeeec'
    UI_BACKGROUND_BOTTOM = '#232626'
    UI_FOREGROUND_BOTTOM = '#d3d7cf'

    PORTAL_EMOJIS = ['🔴', '🔵', '🟢', '🟡']
    ANTENNA_EMOJI = '📡'
    GEM_EMOJI = '💎'
    ANNOUNCER_EMOJI = '🎙️'
    GEM_COLOR = '#238acc'
    FLOOR_COLOR = '#222728'
    WALL_COLOR = '#555753'
    COMMENT_SINGLE = [
        "Always curious, never standing still.",
        "Ready to chase the signal, no matter where it leads.",
        "A true explorer of the unknown.",
        "Quick on their feet, but will luck be on their side?",
        "Bringing energy, even when the path is unclear.",
        "Prepared to turn confusion into discovery.",
        "They may stumble, but they always keep going.",
        "Every step could be the one that finds the gem.",
        "Chaos, courage, and a hint of brilliance.",
        "Sometimes lost, sometimes lucky, always entertaining.",
        "They wander, they wobble, they win (sometimes).",
        "Expect the unexpected whenever this bot appears.",
    ]
    COMMENT_VERSUS = [
        "Two paths cross — only one will shine brighter.",
        "A duel of instincts begins!",
        "Both are eager, but who will read the maze better?",
        "Signals are flickering… tension is rising!",
        "Every step counts when rivals share the arena.",
        "Their strategies may differ, but the goal is the same.",
        "Brace yourselves, this maze is big enough for both… or is it?",
        "We’ve seen surprises before, and we’ll see them again.",
        "Who stumbles first, and who seizes the gem?",
        "The maze doesn’t care who wins, but we do.",
        "A clash of styles — randomness meets randomness!",
        "Head-to-head in the fog — anything can happen.",
    ]
    COMMENT_HYPE = [
        "It’s a showdown!",
        "Let’s rumble!",
        "Face-off in the maze!",
        "Here we go!",
        "Head to head!",
        "The maze decides!",
        "And they’re off!",
        "Gem hunters clash!",
        "Game on!",
        "Ready… set… scramble!",
        "The hunt is live!",
        "All signals point to battle!",
        "It’s anybody’s gem!",
    ]

    Bot = Struct.new(:stdin, :stdout, :stderr, :wait_thr)

    attr_accessor :round, :stage_title, :stage_key, :timeout_scale
    attr_reader :bots, :rng

    def initialize(seed:, width:, height:, generator:, max_ticks:,
                   vis_radius:, gem_spawn_rate:, gem_ttl:, max_gems:,
                   emit_signals:, signal_radius:, signal_quantization:,
                   signal_noise:, signal_cutoff:, signal_fade:, swap_bots:,
                   cache:, profile:, check_determinism:, use_docker:,
                   docker_workdirs:, rounds:, round_seeds:, verbose:,
                   max_tps:, announcer_enabled:, bot_chatter:,
                   ansi_log_path:, show_timings:, start_paused:,
                   highlight_color:, enable_debug:, timeout_scale:
                   )
        @seed = seed
        @width = width
        @height = height
        @generator = generator
        @max_ticks = max_ticks
        @vis_radius = vis_radius
        @gem_spawn_rate = gem_spawn_rate
        @gem_ttl = gem_ttl
        @max_gems = max_gems
        @emit_signals = emit_signals
        @signal_radius = signal_radius
        @signal_quantization = signal_quantization
        @signal_noise = signal_noise
        @signal_cutoff = signal_cutoff
        @signal_fade = signal_fade
        @swap_bots = swap_bots
        @cache = cache
        @profile = profile
        @check_determinism = check_determinism
        @use_docker = use_docker
        @docker_workdirs = docker_workdirs
        @rounds = rounds
        @round_seeds = round_seeds
        @verbose = verbose
        @max_tps = max_tps
        @bots = []
        @bots_io = []
        @gems = []
        @chatlog = []
        @stage_title = '(no stage)'
        @stage_key = '(no stage)'
        @announcer_enabled = announcer_enabled
        @bot_chatter = bot_chatter
        @ansi_log_path = ansi_log_path
        @ansi_log = []
        @show_timings = show_timings
        @start_paused = start_paused
        @highlight_color = highlight_color
        @enable_debug = enable_debug
        @timeout_scale = timeout_scale
        @faded_highlight_color = mix_rgb_hex(@highlight_color, '#000000', 0.25)
        @demo_mode = @ansi_log_path && File.basename(@ansi_log_path).include?('demo')

        param_rng = PCG32.new(@seed)
        [:width, :height, :max_ticks, :vis_radius, :gem_ttl, :max_gems,
         :signal_radius, :rounds].each do |_key|
            key = "@#{_key}".to_sym
            value = instance_variable_get(key)
            if value.is_a?(String) && value.include?('..')
                parts = value.split('..').map { |x| x.strip.to_i }
                new_value = parts[0] + param_rng.randrange(parts[1] - parts[0] + 1)
                instance_variable_set(key, new_value)
            end
        end
        [:gem_spawn_rate].each do |_key|
            key = "@#{_key}".to_sym
            value = instance_variable_get(key)
            if value.is_a?(String) && value.include?('..')
                parts = value.split('..').map { |x| x.strip.to_f }
                new_value = parts[0] + param_rng.next_float * (parts[1] - parts[0])
                new_value = (new_value * 100.0).floor() / 100.0
                instance_variable_set(key, new_value)
            end
        end
    end

    def options_hash
        OPTIONS_FOR_BOT.each_with_object({}) do |key, h|
            h[key.to_sym] = instance_variable_get("@#{key}")
        end
    end

    def gen_maze
        command = "node ./include/maze.js --width #{@width} --height #{@height} --generator #{@generator} --seed #{@seed} --wall \"#\" --floor \".\""
        maze = `#{command}`.strip.split("\n").map { |x| x.strip }.select do |line|
            line =~ /^[\.#]+$/
        end.map.with_index do |line, y|
            row = line.split('').map.with_index { |e, x| e == '#' ? (y << 16) | x : nil }
        end.flatten.reject { |x| x.nil? }
        Set.new(maze)
    end

    def atomic_gzip_write(path, string)
        dir = File.dirname(path)
        base = File.basename(path)
        tmp  = File.join(dir, ".#{base}.#{$$}.#{Thread.current.object_id}.tmp")

        FileUtils.mkdir_p(dir)
        File.open(tmp, File::WRONLY | File::CREAT | File::TRUNC, 0o644) do |f|
            gz = Zlib::GzipWriter.new(f)
            gz.write(string)
            gz.finish
            f.flush
            f.fsync
        end
        File.rename(tmp, path)
        File.open(dir) { |dfd| dfd.fsync rescue nil }
    end

    def safe_gzip_read(path)
        Zlib::GzipReader.open(path) { |gz| gz.read }
    rescue Zlib::GzipFile::Error, EOFError
        sleep 0.05
        Zlib::GzipReader.open(path) { |gz| gz.read }
    end

    def setup
        $timings.profile("setup") do
            @rng = PCG32.new(@seed)
            @maze = gen_maze()
            @floor_tiles = []
            @checksum = Digest::SHA256.hexdigest(@maze.to_a.sort.to_json)
            (0...@height).each do |y|
                (0...@width).each do |x|
                    offset = (y << 16) | x
                    unless @maze.include?(offset)
                        @floor_tiles << offset
                    end
                end
            end
            @rng.shuffle!(@floor_tiles)
            @floor_tiles_set = Set.new(@floor_tiles)
            @spawn_points = []
            @spawn_points << @floor_tiles.shift
            @spawn_points << @floor_tiles.shift
            @spawn_points.map! do |offset|
                [offset & 0xFFFF, offset >> 16]
            end
            @message_queue = Queue.new

            visibility_path = "cache/#{@checksum}.marshal.gz"

            if @cache && File.exist?(visibility_path)
                data = safe_gzip_read(visibility_path)
                @visibility = Marshal.load(data)
            else
                # pre-calculate visibility from each tile
                @visibility = {}
                # if @generator == 'arena'
                #     inner_field_visibility = nil
                #     (0...@height).each do |y|
                #         (0...@width).each do |x|
                #             offset = (y << 16) | x
                #             v = Set.new()
                #             unless @maze.include?(offset)
                #                 if inner_field_visibility.nil?
                #                     visible = FOVAngle.visible(@width, @height, @maze, x, y, radius: @vis_radius) { |xx, yy| @maze.include?((yy << 16) | xx) }
                #                     v = visible.to_a.map { |p| (p[1] << 16) | p[0] }.sort
                #                     inner_field_visibility = v
                #                 end
                #                 v = inner_field_visibility
                #             end
                #             @visibility[offset] = Set.new(v)
                #         end
                #     end
                # else
                    (0...@height).each do |y|
                        (0...@width).each do |x|
                            offset = (y << 16) | x
                            v = Set.new()
                            unless @maze.include?(offset)
                                v = FOVAngle.visible_packed(@width, @height, @maze, x, y, radius: @vis_radius)
                                v.sort!
                            end
                            @visibility[offset] = Set.new(v)
                        end
                    end
                # end

                if @cache
                    bytes = Marshal.dump(@visibility)
                    atomic_gzip_write(visibility_path, bytes)
                end
            end

            # pre-calculate gem spawns
            @gem_fel = []
            (0...@max_ticks).each do |t|
                if @rng.next_float() < @gem_spawn_rate
                    candidate_tiles = @floor_tiles_set.dup
                    position_offset = @rng.sample(candidate_tiles.to_a.sort)
                    ttl = @gem_ttl
                    @gem_fel << {:tick => t, :offset => position_offset, :ttl => ttl}
                end
            end
            # we're waiting to spawn the gem at @gem_fel_index
            @gem_fel_index = 0

            begin
                @terminal_height, @terminal_width = $stdout.winsize
            rescue
                @terminal_height = 24
                @terminal_width = 80
            end

            @tile_width = 2

            if @ansi_log_path
                @terminal_width = 100
                @terminal_height = @height + 2
            end

            @enable_chatlog = false
            @chatlog_position = nil
            @chatlog_width = 0
            @chatlog_height = 0

            if @verbose >= 2 || (!@ansi_log_path.nil?)
                # There are two possible places for the chat log:
                # - right side of the maze (if terminal is wide enough)
                # - below the maze (if terminal is high enough)
                if @terminal_width >= @width * @tile_width + 20
                    @enable_chatlog = true
                    @chatlog_position = :right
                    @chatlog_width = @terminal_width - @width * @tile_width - 2
                    @chatlog_height = @height
                elsif @terminal_height >= @height + 11
                    @enable_chatlog = true
                    @chatlog_position = :bottom
                    @chatlog_width = @terminal_width - 1
                    @chatlog_height = @terminal_height - @height - 1
                end
            end
            if @demo_mode
                @enable_chatlog = false
                @terminal_width = @width * @tile_width
            end
        end
    end

    def start_bot(_path, workdir, bot_args = [], &block)
        path = File.join(File.expand_path(_path), Gem.win_platform? ? 'start.bat' : 'start.sh')
        stdin, stdout, stderr, wait_thr = nil
        $timings.profile("launch bot") do
            if @use_docker
                args = [
                    'docker',
                    'run',
                    '--rm',
                    '-i',
                    '--network=none',
                    '--read-only',
                    '--pids-limit=256',
                    '--memory=512m',
                    '--memory-swap=512m',
                    '--cpus=1',
                    '--cap-drop=ALL',
                    '--security-opt=no-new-privileges',
                    '-u', '1000:1000',
                    "-v", "#{File.dirname(path)}:/src:ro",
                    "--tmpfs", "/home/runner/.cache:rw,nosuid,nodev,noexec,size=64m",
                    "--tmpfs", "/home/runner/.local:rw,nosuid,nodev,noexec,size=64m",
                    "--tmpfs", "/home/runner/.dart-tool:rw,nosuid,nodev,noexec,size=64m",
                    "--tmpfs", "/home/runner/.dotnet:rw,nosuid,nodev,noexec,size=64m",
                    "--tmpfs", "/home/runner/.nuget:rw,nosuid,nodev,noexec,size=64m",
                    "--tmpfs", "/tmp:rw,nosuid,nodev,noexec,size=64m",
                ]
                if workdir
                    args << '-v'
                    args << "#{File.expand_path(workdir)}:/app"
                else
                    args << "--tmpfs"
                    args << "/app:rw,nosuid,nodev,exec,size=256m,uid=1000,gid=1000,mode=1777"
                end
                args << 'hidden-gems-runner'
                args << '/launch.sh'
                args.concat(bot_args)
                stdin, stdout, stderr, wait_thr = Open3.popen3(*args)
            else
                spawn_opts = { chdir: File.dirname(path) }
                if Gem.win_platform?
                    spawn_opts[:new_pgroup] = true   # Windows
                else
                    spawn_opts[:pgroup] = true       # Linux/macOS
                end
                stdin, stdout, stderr, wait_thr = Open3.popen3([path, File.basename(path)], *bot_args, spawn_opts)
            end
        end
        stdin.sync = true
        stdout.sync = true
        stderr.sync = true
        err_thread = Thread.new do
            begin
                stderr.each_line do |line|
                    yield line if block_given?
                end
            rescue IOError
            end
        end
        Bot.new(stdin, stdout, stderr, wait_thr)
    end

    def kill_bot_process(bot_io)
        pid = bot_io.wait_thr.pid
        if Gem.win_platform?
            system("taskkill /PID #{pid} /T /F >NUL 2>&1")
        else
            begin
                if @use_docker
                    Process.kill('TERM', pid)
                else
                    Process.kill('TERM', -pid)
                end
            rescue Errno::ESRCH
            end
        end
    end

    def mix_rgb_hex(c1, c2, t)
        x = c1[1..].scan(/../).map { |h| h.to_i(16) }
        y = c2[1..].scan(/../).map { |h| h.to_i(16) }

        r = (x[0] + (y[0] - x[0]) * t).round.clamp(0, 255)
        g = (x[1] + (y[1] - x[1]) * t).round.clamp(0, 255)
        b = (x[2] + (y[2] - x[2]) * t).round.clamp(0, 255)

        format("#%02X%02X%02X", r, g, b)
    end

    def mul_rgb_hex(c1, c2)
        x = c1[1..].scan(/../).map { |h| h.to_i(16) }
        y = c2[1..].scan(/../).map { |h| h.to_i(16) }

        r = (x[0] * (y[0] / 255.0)).round.clamp(0, 255)
        g = (x[1] * (y[1] / 255.0)).round.clamp(0, 255)
        b = (x[2] * (y[2] / 255.0)).round.clamp(0, 255)

        format("#%02X%02X%02X", r, g, b)
    end

    def vwidth(str)
        Unicode::DisplayWidth.of(str.to_s, emoji: true, ambwidth: 1)
    end

    def sanitize_emoji(str)
        # Remove all characters with a width <= 0 from the string
        str.each_grapheme_cluster.select { |g| vwidth(g) > 0 }.join
    end

    def trim_ansi_to_width(str, max_width)
        ss      = StringScanner.new(str)
        out     = +""
        width   = 0

        graphemes = ->(s) { s.respond_to?(:each_grapheme_cluster) ? s.each_grapheme_cluster : s.each_char }

        until ss.eos?
            if ss.scan(ANSI)
                out << ss.matched
                next
            end

            start_pos = ss.pos
            char = ss.getch
            w = vwidth(char)
            break if width + w > max_width
            out << char
            width += w
        end

        [out, width]
    end

    # Split an overlong token into visual-width chunks, preserving all characters.
    def chunk_token(token, limit)
        return [token] if vwidth(token) <= limit || limit <= 0
        chunks = []
        buf = +""
        token.scan(/\X/) do |g| # \X = Unicode grapheme
            if vwidth(buf + g) > limit
                chunks << buf
                buf = g.dup
            else
                buf << g
            end
        end
        chunks << buf unless buf.empty?
        chunks
    end

    def wrap_entry(text, emoji, width, show_prefix: true)
        prefix = show_prefix ? "#{emoji} " : "  " # emoji only if show_prefix
        while vwidth(prefix) < 3
            prefix += " "
        end
        pwidth = vwidth(prefix)
        body_w = [width - pwidth, 0].max
        indent = " " * pwidth

        out = []
        line = +""

        tokens = text.scrub.split(/\s/).flat_map { |t| chunk_token(t, body_w) }
        tokens.each do |tok|
            if line.empty?
                line = tok.dup
            else
                if vwidth(line) + 1 + vwidth(tok) <= body_w
                    line << " " << tok
                else
                    out << line
                    line = tok.dup
                end
            end
        end
        out << line

        out.each_with_index.map { |l, i| (i == 0 ? prefix : indent) + l }
    end

    def render_chatlog(entries, width, height)
        return [] if height.to_i <= 0

        show_prefix = []
        prev_emoji = nil
        entries.each do |e|
            show_prefix << (e[:emoji] != prev_emoji)
            prev_emoji = e[:emoji]
        end

        need   = height
        chunks = []

        (entries.length - 1).downto(0) do |i|
            e = entries[i]
            lines = wrap_entry(e[:text], e[:emoji], width, show_prefix: show_prefix[i])

            if lines.length >= need
                chunks << lines.last(need)
                need = 0
                break
            else
                chunks << lines
                need -= lines.length
            end
        end

        result = chunks.reverse.flatten
        result.last(height)
    end


    def strip_ansi(str)
        str.gsub(/\e\[[0-9;]*[A-Za-z]/, '')
    end

    def render(tick, signal_level, paused)
        fg_top_mix = mix_rgb_hex(UI_FOREGROUND_TOP, UI_BACKGROUND_TOP, 0.5)
        fg_bottom_mix = mix_rgb_hex(UI_FOREGROUND_BOTTOM, UI_BACKGROUND_BOTTOM, 0.5)
        StringIO.open do |io|
            io.print "\033[H" if @verbose >= 2

            score_s = @bots.map { |x| "#{x[:emoji]} #{x[:disqualified_for] ? 0 : x[:score]}" }.join(' : ')

            $timings.profile("render: upper status bar") do
                status_line = [
                    [
                        Paint['  ', fg_bottom_mix, UI_BACKGROUND_TOP],
                        Paint['Stage: ', UI_FOREGROUND_TOP, UI_BACKGROUND_TOP, :bold],
                        Paint[@stage_key, UI_FOREGROUND_TOP, UI_BACKGROUND_TOP],
                    ],
                    [
                        Paint['Seed: ', UI_FOREGROUND_TOP, UI_BACKGROUND_TOP, :bold],
                        Paint[@seed.to_s(36), UI_FOREGROUND_TOP, UI_BACKGROUND_TOP],
                    ],
                    [
                        Paint['Tick: ', UI_FOREGROUND_TOP, UI_BACKGROUND_TOP, :bold],
                        Paint[sprintf("%#{(@max_ticks).to_s.size}d", tick), UI_FOREGROUND_TOP, UI_BACKGROUND_TOP],
                    ],
                    [
                        Paint['Score: ', UI_FOREGROUND_TOP, UI_BACKGROUND_TOP, :bold],
                        Paint[score_s, UI_FOREGROUND_TOP, UI_BACKGROUND_TOP],
                    ],
                ].map { |x| x.join('') }.join(Paint['  |  ', UI_FOREGROUND_TOP, UI_BACKGROUND_TOP])
                trimmed, vis = trim_ansi_to_width(status_line, @terminal_width)
                status_line = trimmed + Paint[' ' * [@terminal_width - vis, 0].max, UI_FOREGROUND_TOP, UI_BACKGROUND_TOP]
                io.puts status_line
            end

            paint_rng = PCG32.new(1234)

            bots_visible = @bots.map do |bot|
                @visibility[(bot[:position][1] << 16) | bot[:position][0]]
            end

            chat_lines = nil
            if @enable_chatlog
                chat_lines = render_chatlog(@chatlog, @chatlog_width, @chatlog_height)
            end

            bot_with_initiative = @tick % @bots.size
            @wall_color_cache ||= {}
            @fog_of_war_cache ||= {}
            @bg_fade ||= {}
            @bg_highlight ||= {}

            bot_highlights = {}

            if @enable_debug
                @bots.each.with_index do |x, i|
                    debug_json = ((@protocol[i][-2] || {})[:bots] || {})[:debug_json]
                    if debug_json
                        data = nil
                        begin
                            data = JSON.parse(debug_json)
                        rescue
                            next
                        end
                        if data['highlight']
                            data['highlight'].each do |_|
                                x = _[0]
                                y = _[1]
                                offset = (y << 16) | x
                                color = _[2]
                                bot_highlights[offset] ||= []
                                bot_highlights[offset] << color
                            end
                        end
                    end
                end
            end

            $timings.profile("render: main screen") do
                (0...@height).each do |y|
                    (0...@width).each do |x|
                        c = ' ' * @tile_width
                        bg = FLOOR_COLOR
                        offset = (y << 16) | x
                        if @maze.include?(offset)
                            unless @wall_color_cache.include?(offset)
                                @wall_color_cache[offset] = mix_rgb_hex(WALL_COLOR, '#000000', paint_rng.next_float() * 0.25)
                            end
                            bg = @wall_color_cache[offset]
                        end
                        $timings.profile("find bots") do
                            (0...@bots.size).each do |_k|
                                i = (_k + bot_with_initiative) % @bots.size
                                bot = @bots[i]
                                next if bot[:disqualified_for]
                                p = bot[:position]
                                if p[0] == x && p[1] == y
                                    c = @bots[i][:emoji]
                                    while vwidth(c) < @tile_width
                                        c += ' '
                                    end
                                end
                            end
                        end
                        $timings.profile("find gems") do
                            @gems.each.with_index do |p, i|
                                if p[:position][0] == x && p[:position][1] == y
                                    c = GEM_EMOJI
                                    while vwidth(c) < @tile_width
                                        c += ' '
                                    end
                                end
                                if @emit_signals
                                    if signal_level[i].include?((y << 16) | x)
                                        bg = mix_rgb_hex(GEM_COLOR, bg, 1.0 - signal_level[i][(y << 16) | x])
                                    end
                                end
                            end
                        end
                        highlight_color = nil
                        (0...@bots.size).each do |_k|
                            i = _k
                            bot = @bots[i]
                            next if bot[:disqualified_for]
                            if (@visibility[(bot[:position][1] << 16) | bot[:position][0]] || Set.new()).include?((y << 16) | x)
                                if @bots.size == 2
                                    if highlight_color.nil?
                                        highlight_color = @faded_highlight_color
                                    else
                                        highlight_color = @highlight_color
                                    end
                                else
                                    highlight_color = @highlight_color
                                end
                            end
                        end
                        @bg_highlight[offset] = highlight_color if highlight_color
                        if highlight_color.nil?
                            @bg_fade[offset] ||= 0.0
                            @bg_fade[offset] *= paused ? 0 : 0.5
                        else
                            @bg_fade[offset] = 1.0
                        end
                        cache_key_0 = "#{bg}/#000000/32"
                        @fog_of_war_cache[cache_key_0] ||= mix_rgb_hex(bg, '#000000', 0.5)
                        cache_key_1 = "#{bg}/#{@bg_highlight[offset] || '#ffffff'}/m"
                        @fog_of_war_cache[cache_key_1] ||= mul_rgb_hex(bg, @bg_highlight[offset] || '#ffffff')
                        cache_key_2 = "#{@fog_of_war_cache[cache_key_0]}/#{@fog_of_war_cache[cache_key_1]}/#{(@bg_fade[offset] * 64).round}"
                        @fog_of_war_cache[cache_key_2] ||= mix_rgb_hex(@fog_of_war_cache[cache_key_0], @fog_of_war_cache[cache_key_1], @bg_fade[offset])
                        bg = @fog_of_war_cache[cache_key_2]
                        if bot_highlights.include?(offset)
                            bot_highlights[offset].each do |color|
                                opacity = 32
                                if color.size == 9
                                    opacity = (color[7..8].to_i(16) * 63) / 255
                                    color = color[0..6]
                                end
                                cache_key_3 = "#{bg}/#{color}/#{opacity}"
                                @fog_of_war_cache[cache_key_3] ||= mix_rgb_hex(bg, color, opacity / 63.0)
                                bg = @fog_of_war_cache[cache_key_3]
                            end
                        end
                        io.print Paint[c, nil, bg]
                    end
                    if @enable_chatlog && @chatlog_position == :right
                        io.print ' '
                        s = chat_lines[y] || ''
                        s += ' ' * (@chatlog_width - vwidth(strip_ansi(s)))

                        io.print s
                    end
                    io.puts
                end
            end

            $timings.profile("render: lower status bar") do
                status_line = [
                    [
                        Paint['  ', fg_bottom_mix, UI_BACKGROUND_BOTTOM],
                        Paint['[Q]', UI_FOREGROUND_BOTTOM, UI_BACKGROUND_BOTTOM],
                        Paint[' Quit', fg_bottom_mix, UI_BACKGROUND_BOTTOM],
                    ],
                    [
                        Paint['[Space]', UI_FOREGROUND_BOTTOM, UI_BACKGROUND_BOTTOM],
                        Paint[' Pause', fg_bottom_mix, UI_BACKGROUND_BOTTOM],
                    ],
                    [
                        Paint['[←][→]', UI_FOREGROUND_BOTTOM, UI_BACKGROUND_BOTTOM],
                        Paint[' Step', fg_bottom_mix, UI_BACKGROUND_BOTTOM],
                    ],
                    [
                        Paint['[Home]', UI_FOREGROUND_BOTTOM, UI_BACKGROUND_BOTTOM],
                        Paint[' Rewind', fg_bottom_mix, UI_BACKGROUND_BOTTOM],
                    ],
                    [
                        Paint[@bots.map.with_index { |x, i| "#{x[:emoji]} #{((@protocol[i][-2] || {})[:bots] || {})[:response]}" }.join(' : '), UI_FOREGROUND_BOTTOM, UI_BACKGROUND_BOTTOM]
                    ],
                ].map { |x| x.join('') }.join(Paint['  |  ', fg_bottom_mix, UI_BACKGROUND_BOTTOM])
                trimmed, vis = trim_ansi_to_width(status_line, @terminal_width)
                status_line = trimmed + Paint[' ' * [@terminal_width - vis, 0].max, UI_FOREGROUND_BOTTOM, UI_BACKGROUND_BOTTOM]
                io.print status_line
            end

            if @enable_chatlog && @chatlog_position == :bottom
                chat_lines.each do |line|
                    io.puts line
                end
            end
            io.string
        end
    end

    def add_bot(path, bot_args = [])
        @bots << {:position => @spawn_points.shift, :score => 0, :name => "Botty McBotface", :emoji => '🤖', :overtime_used => 0.0, :disqualified_for => nil, :response_times => [], :stderr_log => []}
        yaml_path = File.join(File.expand_path(path), 'bot.yaml')
        if File.exist?(yaml_path)
            info = YAML.load(File.read(yaml_path))
            @bots.last[:name] = info['name'] if info['name'].is_a?(String)
            @bots.last[:emoji] = sanitize_emoji(info['emoji'].to_s) if info['emoji'].is_a?(String)
            if @bots.last[:name] && @bots.last[:name].length > 32
                raise "Error in bot.yaml: Bot name '#{@bots.last[:name]}' is too long (max 32 characters)"
            end
            if vwidth(@bots.last[:emoji]) > 2
                raise "Error in bot.yaml: emoji must be at most 2 characters wide (#{@bots.last[:emoji]} is #{vwidth(@bots.last[:emoji])} characters wide)"
            end
        end
        bot_index = @bots_io.size
        @bots_io << start_bot(path, @docker_workdirs[bot_index], bot_args) do |line|
            @message_queue << {:bot => bot_index, :line => line}
            @bots[bot_index][:stderr_log] << line
        end
    end

    def spawn_gem()
        spawn_data = @gem_fel[@gem_fel_index]
        @gem_fel_index += 1

        gem = {:position_offset => spawn_data[:offset], :ttl => spawn_data[:ttl]}
        gem[:position] = [gem[:position_offset] & 0xFFFF, gem[:position_offset] >> 16]

        # Attention: if there happens to be a gem or a bot already at this position,
        # let's find another position nearby.
        occupied_points = Set.new()
        @gems.each do |g|
            occupied_points << ((g[:position][1] << 16) | g[:position][0])
        end
        @bots.each do |b|
            occupied_points << ((b[:position][1] << 16) | b[:position][0])
        end

        dist_field = {}
        wavefront = Set.new()
        wavefront << [gem[:position][0], gem[:position][1]]
        dist_field[(gem[:position][1] << 16) | gem[:position][0]] = 0
        distance = 0
        good = false
        while (occupied_points.include?(gem[:position_offset])) && (!wavefront.empty?)
            new_wavefront = Set.new()
            wavefront.each do |p|
                px = p[0]
                py = p[1]
                candidates = [[-1, 0], [1, 0], [0, -1], [0, 1]]
                @rng.shuffle!(candidates)
                candidates.each do |d|
                    dx = px + d[0]
                    dy = py + d[1]
                    if dx >= 0 && dy >= 0 && dx < @width && dy < @height
                        offset = (dy << 16) | dx
                        if !dist_field.include?(offset) && !@maze.include?(offset)
                            dist_field[offset] = distance
                            new_wavefront << [dx, dy]
                            unless occupied_points.include?(offset)
                                gem[:position_offset] = offset
                                gem[:position] = [dx, dy]
                                good = true
                                break
                            end
                        end
                    end
                    break if good
                end
                break if good
            end
            break if good
            wavefront = new_wavefront
            distance += 1
        end

        # pre-calculate gem level
        level = {}
        wavefront = Set.new()
        wavefront << [gem[:position][0], gem[:position][1]]
        level[(gem[:position][1] << 16) | gem[:position][0]] = 1.0
        distance = 0
        while !wavefront.empty?
            new_wavefront = Set.new()
            wavefront.each do |p|
                px = p[0]
                py = p[1]
                [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |d|
                    dx = px + d[0]
                    dy = py + d[1]
                    if dx >= 0 && dy >= 0 && dx < @width && dy < @height
                        offset = (dy << 16) | dx
                        if !level.include?(offset) && !@maze.include?(offset)
                            l = Math.exp(-distance / @signal_radius)
                            if @signal_quantization > 0
                                l = ((l * @signal_quantization).to_i).to_f / @signal_quantization
                            end
                            l = 0.0 if l < @signal_cutoff
                            level[offset] = l
                            new_wavefront << [dx, dy]
                        end
                    end
                end
            end
            wavefront = new_wavefront
            distance += 1
        end
        gem[:level] = level

        @gems << gem
        return gem[:ttl]
    end

    def read_line_before_deadline(io, deadline_mono)
        buf = +""
        loop do
            now = Process.clock_gettime(Process::CLOCK_MONOTONIC)
            remaining = deadline_mono - now
            return [:hard_timeout, nil] if remaining <= 0

            begin
                chunk = io.read_nonblock(4096)
                return [:eof, nil] if chunk.nil?
                buf << chunk
                if (idx = buf.index("\n"))
                    return [:ok, buf[0..idx].strip]
                end
            rescue IO::WaitReadable
                ready = IO.select([io], nil, nil, remaining)
                return [:hard_timeout, nil] unless ready
            rescue EOFError
                return [:eof, nil]
            end
        end
    end

    def run
        trap("INT") do
            @bots_io.each { |b| b.stdin.close rescue nil }
            @bots_io.each { |b| kill_bot_process(b) }
            exit
        end

        print "\033[2J" if @verbose >= 2
        @tick = 0
        @tps = 0
        t0 = Time.now.to_f
        begin
            STDIN.echo = false
        rescue
        end
        results = @bots.map do |b|
             { :ticks_to_first_capture => nil }
        end
        @tiles_revealed = @bots.map do |b|
            Set.new()
        end
        ttl_spawned = 0

        @protocol = @bots.map { |b| [] }
        if @announcer_enabled
            @chatlog << {emoji: ANNOUNCER_EMOJI, text: "Welcome to Hidden Gems!" }
            @chatlog << {emoji: ANNOUNCER_EMOJI, text: "Today's stage is #{@stage_title} (v#{@stage_key.split('@').last})" }
            @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{@generator.capitalize} @ #{@width}x#{@height} with seed #{@seed.to_s(36)}" }
            if @bots.size == 1
                @chatlog << {emoji: ANNOUNCER_EMOJI, text: "All eyes on our lone contestant:" }
            else
                @chatlog << {emoji: ANNOUNCER_EMOJI, text: "Two bots enter the maze:" }
            end
            comments = COMMENT_SINGLE.dup
            
            @rng.shuffle!(comments)
            @bots.each.with_index do |bot, i|
                @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{bot[:name]} #{bot[:emoji]} -- #{comments.shift}" }
            end
            if @bots.size > 1
                @chatlog << {emoji: ANNOUNCER_EMOJI, text: @rng.sample(COMMENT_VERSUS) }
            end
        end
        frames = []
        paused = @start_paused
        break_on_tick = nil
        begin
            print "\033[?25l" if @verbose >= 2
            loop do
                break if break_on_tick && @tick >= break_on_tick
                tf0 = Time.now.to_f
                while frames.size <= @tick
                    running_tick = frames.size
                    until @message_queue.empty?
                        temp = @message_queue.pop(true) rescue nil
                        next if temp.nil?
                        if @bot_chatter
                            @chatlog << {emoji: @bots[temp[:bot]][:emoji], text: temp[:line].chomp}
                        end
                    end

                    (0...@bots.size).each do |i|
                        @protocol[i] << {}
                        @protocol[i].last[:tick] = @tick
                        @protocol[i].last[:rng_state] = @rng.snapshot
                    end

                    # STEP 1: Calculate signal levels at each tile
                    if @emit_signals
                        signal_level = @gems.map do |gem|
                            temp = if @signal_noise > 0.0
                                gem[:level].transform_values do |l|
                                    l += (@rng.next_float() - 0.5) * 2.0 * @signal_noise
                                    l = 0.0 if l < 0.0
                                    l = 1.0 if l > 1.0
                                    l
                                end
                            else
                                gem[:level]
                            end
                            if @signal_fade > 0
                                t = 1.0
                                gem_age = @gem_ttl - gem[:ttl]
                                if gem_age < @signal_fade
                                    t = (gem_age + 1).to_f / @signal_fade
                                elsif gem_age >= @gem_ttl - @signal_fade
                                    t = (@gem_ttl - gem_age).to_f / @signal_fade
                                end
                                t = 0.0 if t < 0.0
                                t = 1.0 if t > 1.0
                                if t < 1.0
                                    temp = temp.transform_values { |x| x * t }
                                end
                            end
                            temp
                        end
                    end

                    @bots.each.with_index do |bot, i|
                        bot_position = bot[:position]
                        @visibility[(bot_position[1] << 16) | bot_position[0]].each do |t|
                            @tiles_revealed[i] << t
                        end
                    end

                    if @tick == @max_ticks
                        if @announcer_enabled
                            if @bots.size == 1
                                @chatlog << {emoji: ANNOUNCER_EMOJI, text: "Mission complete after #{@max_ticks} ticks." }
                            else
                                if @bots[0][:score] != @bots[1][:score]
                                    @chatlog << {emoji: ANNOUNCER_EMOJI, text: "Duel ends after #{@max_ticks} ticks (one stands tall, one blames RNG)." }
                                else
                                    @chatlog << {emoji: ANNOUNCER_EMOJI, text: "Duel ends in a draw after #{@max_ticks} ticks." }
                                end
                            end
                        end
                    end

                    # STEP 2: RENDER
                    if @verbose >= 2 || @ansi_log_path
                        screen = nil
                        $timings.profile("render screen") do
                            screen = render(running_tick, signal_level, paused)
                        end
                        # @protocol.last[:screen] = screen
                        if @verbose >= 2
                            print screen
                        end
                        frames << screen
                        if @ansi_log_path
                            @ansi_log << {:screen => screen}
                        end
                    else
                        frames << nil
                    end
                    t1 = Time.now.to_f
                    @tps = (@tick.to_f / (t1 - t0)).round
                    if @verbose == 1
                        print "\rTick: #{@tick} @ #{@tps} tps"
                    end

                    if @bots.all? { |b| b[:disqualified_for] }
                        break_on_tick = @tick + 1
                    end

                    bot_with_initiative = @tick % @bots.size

                    # --- STEP 3: QUERY BOTS in parallel: send data to all, then read one line from each ---

                    next if @tick == @max_ticks
                    @stdout_buffers ||= Array.new(@bots.size) { "" }

                    bot_with_initiative = @tick % @bots.size

                    # 3a) PREPARE DATA for each bot
                    prepared = Array.new(@bots.size)
                    (0...@bots.size).each do |i|
                        bot = @bots[i]
                        next if bot[:disqualified_for]

                        data = {}
                        $timings.profile("prepare data") do
                            if @tick == 0
                                data[:config] = {}
                                OPTIONS_FOR_BOT.each do |key|
                                    data[:config][key.to_sym] = instance_variable_get("@#{key}")
                                end
                                bot_seed = Digest::SHA256.digest("#{@seed}/bot").unpack1('L<')
                                data[:config][:bot_seed] = bot_seed
                            end
                            data[:tick] = @tick
                            data[:bot] = bot[:position]
                            data[:wall] = []
                            data[:floor] = []
                            data[:initiative] = (bot_with_initiative == i)
                            data[:visible_gems] = []
                            vis_key = (bot[:position][1] << 16) | bot[:position][0]
                            @visibility[vis_key].each do |t|
                                key = @maze.include?(t) ? :wall : :floor
                                data[key] << [t & 0xFFFF, t >> 16]
                                @gems.each do |gem|
                                    if gem[:position_offset] == t
                                        data[:visible_gems] << { :position => gem[:position], :ttl => gem[:ttl] }
                                    end
                                end
                            end
                            if @emit_signals
                                level_sum = 0.0
                                @gems.each_with_index do |gem, gi|
                                    level_sum += (signal_level[gi][vis_key] || 0.0)
                                end
                                data[:signal_level] = format("%.6f", level_sum).to_f
                            end
                            if @bots.size > 1
                                data[:visible_bots] = []
                                (0...@bots.size).each do |j|
                                    next if j == i
                                    other = @bots[j]
                                    next if other[:disqualified_for]
                                    bx, by = other[:position]
                                    if @visibility[vis_key].include?((by << 16) | bx)
                                        data[:visible_bots] << { :position => other[:position], :emoji => other[:emoji] }
                                    end
                                end
                            end
                        end

                        prepared[i] = data
                        @protocol[i] ||= []
                        @protocol[i].last[:bots] ||= {}
                        @protocol[i].last[:bots][:data] = data
                        if @ansi_log_path && i == 0
                            @ansi_log.last[:stdin] = data
                        end
                    end

                    # 3b) WRITE PHASE: send to all eligible bots first
                    start_mono   = {}
                    deadline_mono = {}
                    write_limit = (@tick == 0 ? HARD_LIMIT_FIRST_TICK : HARD_LIMIT) * @timeout_scale
                    (0...@bots.size).each do |_k|
                        i = (_k + bot_with_initiative) % @bots.size
                        next if @bots[i][:disqualified_for] || prepared[i].nil?
                        start_mono[i]    = Process.clock_gettime(Process::CLOCK_MONOTONIC)
                        deadline_mono[i] = start_mono[i] + write_limit
                        $timings.profile("write to bot's stdin") do
                            begin
                                @bots_io[i].stdin.puts(prepared[i].to_json)
                                @bots_io[i].stdin.flush
                            rescue Errno::EPIPE
                                if @bots[i][:disqualified_for].nil?
                                    @bots[i][:disqualified_for] = 'terminated_unexpectedly'
                                    @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{@bots[i][:name]} has terminated unexpectedly!" } if @announcer_enabled
                                end
                            end
                        end
                    end

                    # 3c) READ PHASE: multiplex all stdout until we have one line per responding bot or they time out/EOF
                    pending = (0...@bots.size).select { |i| prepared[i] && @bots[i][:disqualified_for].nil? }
                    stdout_map = {}
                    read_ios = []
                    pending.each do |i|
                        io = @bots_io[i].stdout
                        read_ios << io
                        stdout_map[io] = i
                    end

                    responses = {} # i => line (String)
                    loop do
                        break if read_ios.empty?

                        # compute shortest remaining deadline for select timeout
                        now_mono = Process.clock_gettime(Process::CLOCK_MONOTONIC)
                        min_remaining = pending.map { |i| [deadline_mono[i] - now_mono, 0.0].max }.min
                        # if any already timed out, skip waiting
                        min_remaining = 0.0 if pending.any? { |i| now_mono >= deadline_mono[i] }

                        ready, = IO.select(read_ios, nil, nil, min_remaining)
                        now_mono = Process.clock_gettime(Process::CLOCK_MONOTONIC)

                        # Mark hard timeouts
                        pending.dup.each do |i|
                            if now_mono >= deadline_mono[i] && !responses.key?(i)
                                if @bots[i][:disqualified_for].nil?
                                    @bots[i][:disqualified_for] = "hard_timeout"
                                    elapsed = now_mono - start_mono[i]
                                    if @announcer_enabled
                                        @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{@bots[i][:name]} took too long to respond (#{(elapsed * 1000).to_i} ms) and has been terminated!" }
                                    end
                                end
                                io = @bots_io[i].stdout
                                read_ios.delete(io)
                                pending.delete(i)
                            end
                        end
                        break if read_ios.empty?

                        next if ready.nil? # nothing readable yet; loop will handle any new timeouts

                        ready.each do |io|
                            i = stdout_map[io]
                            next unless pending.include?(i)
                            chunk = io.read_nonblock(4096, exception: false)
                            case chunk
                            when :wait_readable
                                # no data yet
                            when nil
                                # EOF
                                if @bots[i][:disqualified_for].nil?
                                    @bots[i][:disqualified_for] = 'terminated_unexpectedly'
                                    @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{@bots[i][:name]} has terminated unexpectedly!" } if @announcer_enabled
                                end
                                read_ios.delete(io)
                                pending.delete(i)
                            else
                                @stdout_buffers[i] << chunk
                                # extract one line if present (keep remainder for next tick)
                                if (nl = @stdout_buffers[i].index("\n"))
                                    line = @stdout_buffers[i].slice!(0..nl).strip
                                    responses[i] = line
                                    read_ios.delete(io)
                                    pending.delete(i)
                                end
                            end
                        end
                    end

                    # 3d) PROCESS RESPONSES in initiative order
                    (0...@bots.size).each do |_k|
                        i = (_k + bot_with_initiative) % @bots.size
                        next if prepared[i].nil?
                        next if @bots[i][:disqualified_for]

                        line = responses[i]
                        if line.nil?
                            # already disqualified above (timeout/EOF)
                            next
                        end

                        elapsed = Process.clock_gettime(Process::CLOCK_MONOTONIC) - start_mono[i]
                        @bots[i][:response_times] << elapsed
                        overtime = (@tick == 0 || @check_determinism) ? 0.0 : (elapsed - SOFT_LIMIT * @timeout_scale)
                        @bots[i][:overtime_used] += overtime if overtime > 0.0
                        if @announcer_enabled && overtime.to_f > 0.0
                            @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{@bots[i][:name]} exceeded soft limit by #{(overtime * 1e3).to_i} ms (total overtime: #{(@bots[i][:overtime_used] * 1e3).to_i} ms)" }
                        end
                        if @bots[i][:overtime_used] > OVERTIME_BUDGET * @timeout_scale
                            if @bots[i][:disqualified_for].nil?
                                @bots[i][:disqualified_for] = 'overtime_budget_exceeded'
                                @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{@bots[i][:name]} has exceeded their overtime budget and is disqualified!" } if @announcer_enabled
                            end
                        end

                        command = (line.split(' ').first || '').strip
                        debug_json = line[command.length..-1]&.strip
                        @protocol[i].last[:bots][:response] = command
                        unless @check_determinism
                            @protocol[i].last[:bots][:debug_json] = debug_json
                        end

                        bot_position = @bots[i][:position]
                        if ['N','E','S','W'].include?(command)
                            dir = {'N'=>[0,-1],'E'=>[1,0],'S'=>[0,1],'W'=>[-1,0]}
                            dx = bot_position[0] + dir[command][0]
                            dy = bot_position[1] + dir[command][1]
                            if dx >= 0 && dy >= 0 && dx < @width && dy < @height
                                unless @maze.include?((dy << 16) | dx)
                                    target_occupied_by_bot = nil
                                    (0...@bots.size).each do |other|
                                        next if other == i
                                        next if @bots[other][:disqualified_for]
                                        if @bots[other][:position] == [dx, dy]
                                            target_occupied_by_bot = other
                                            break
                                        end
                                    end
                                    @bots[i][:position] = [dx, dy] if target_occupied_by_bot.nil?
                                end
                            end
                        elsif command == 'WAIT'
                            # no-op
                        else
                            # invalid command -> ignore
                        end
                    end

                    # STEP 4: COLLECT GEMS & DECAY GEMS
                    if @tick < @max_ticks
                        collected_gems = []
                        @gems.each.with_index do |gem, i|
                            collected_this_gem = false
                            (0...@bots.size).each do |_k|
                                k = (_k + bot_with_initiative) % @bots.size
                                bot = @bots[k]
                                next if bot[:disqualified_for]
                                if bot[:position] == gem[:position]
                                    collected_this_gem = true
                                    collected_gems << i
                                    bot[:score] += gem[:ttl]
                                    results[k][:ticks_to_first_capture] ||= @tick
                                    if @announcer_enabled
                                        @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{bot[:name]} scored a gem with #{gem[:ttl]} points!" }
                                    end
                                end
                                break if collected_this_gem
                            end
                        end
                        collected_gems.reverse.each do |i|
                            @gems.delete_at(i)
                        end
                        @gems.each.with_index do |gem, i|
                            gem[:ttl] -= 1
                        end
                        @gems.reject! do |gem|
                            gem[:ttl] <= 0
                        end
                    end

                    if @gems.size < @max_gems
                        if @gem_fel_index < @gem_fel.size
                            if @tick >= @gem_fel[@gem_fel_index][:tick]
                                ttl_spawned += spawn_gem()
                            end
                        end
                    end
                end
                if @verbose >= 2
                    print frames[@tick]
                end
                unless paused
                    @tick += 1
                    break if @tick > @max_ticks
                    if @verbose >= 2 && @max_tps > 0
                        loop do
                            tf1 = Time.now.to_f
                            break if tf1 - tf0 > 1.0 / @max_tps
                            sleep [(1.0 / @max_tps - tf1 + tf0), 0.0].max
                        end
                    end
                end
                unless @verbose < 2 || @check_determinism
                    begin
                        key = KeyInput.get_key(paused)
                        if key == 'q' || key == 'esc'
                            exit
                        elsif key == 'left'
                            @tick = [@tick - 1, 0].max
                            paused = true
                        elsif key == 'home'
                            @tick = 0
                            paused = true
                        # elsif key == 'end'
                        #     @tick = @max_ticks - 1
                        #     paused = true
                        elsif key == 'right'
                            @tick = [@tick + 1, @max_ticks].min
                            paused = true
                        elsif key == ' '
                            paused = !paused
                        end
                    rescue
                    end
                end
            end
            @bots_io.each do |b|
                kill_bot_process(b)
            end
        ensure
            print "\033[?25h" if @verbose >= 2
            begin
                STDIN.echo = true
            rescue
            end
        end
        # if a bot was disqualified, set their score to 0
        @bots.each do |bot|
            if bot[:disqualified_for]
                bot[:score] = 0
            end
        end

        if @verbose == 1
            puts
        end
        if @rounds == 1
            unless @ansi_log_path
                puts "Seed: #{@seed.to_s(36)} / Score: #{@bots.map { |x| x[:score]}.join(' / ')}"
            end
        else
            print "\rFinished round #{@round + 1} of #{@rounds}..."
        end
        @bots.each.with_index do |bot, i|
            results[i][:score] = bot[:score]
            results[i][:disqualified_for] = bot[:disqualified_for]
            first_response_time = (bot[:response_times].size > 0 ? (bot[:response_times].first * 1e9).to_i : nil)
            remaining_response_times = bot[:response_times].map { |x| (x * 1e9).to_i }
            remaining_response_times.shift if remaining_response_times.size > 0
            results[i][:response_time_stats] = {
                :first => first_response_time,
                :min => (remaining_response_times.size > 0 ? remaining_response_times.min : nil),
                :median => (remaining_response_times.size > 0 ? remaining_response_times.sort[remaining_response_times.size / 2] : nil),
                :max => (remaining_response_times.size > 0 ? remaining_response_times.max : nil),
            }
            results[i][:stderr_log] = bot[:stderr_log]
            if @profile
                results[i][:gem_utilization] = (ttl_spawned > 0 ? (bot[:score].to_f / ttl_spawned.to_f * 100.0 * 100).to_i.to_f / 100 : 0.0)
                results[i][:tile_coverage] = ((@tiles_revealed[i] & @floor_tiles_set).size.to_f / @floor_tiles_set.size.to_f * 100.0 * 100).to_i.to_f / 100
            end
            if @check_determinism
                results[i][:protocol_checksum] = Digest::SHA256.hexdigest(@protocol[i].to_json)
            end
        end
        if @ansi_log_path
            path = @ansi_log_path.sub('.json.gz', "-#{@seed.to_s(36)}.json.gz")
            emoji_widths = {}
            @bots.each do |bot|
                emoji_widths[bot[:emoji]] = vwidth(bot[:emoji])
            end
            Zlib::GzipWriter.open(path) do |f|
                data = {:width => @terminal_width, :height => @terminal_height, :frames => @ansi_log, :emoji_widths => emoji_widths}
                f.write(data.to_json)
            end
            path = @ansi_log_path.sub('.json.gz', "-#{@seed.to_s(36)}-poster.json.gz")
            Zlib::GzipWriter.open(path) do |f|
                data = {:width => @terminal_width, :height => @terminal_height, :frames => [@ansi_log.first], :emoji_widths => emoji_widths}
                f.write(data.to_json)
            end
        end
        results
    end
end

stages = YAML.load(File.read('stages.yaml'))

options = {
    stage: 'current',
    width: 19,
    height: 19,
    generator: 'arena',
    seed: rand(2 ** 32),
    max_ticks: 1000,
    vis_radius: 10,
    max_gems: 1,
    gem_spawn_rate: 0.05,
    gem_ttl: 300,
    signal_radius: 10.0,
    signal_cutoff: 0.0,
    signal_noise: 0.0,
    signal_quantization: 0,
    signal_fade: 0,
    swap_bots: false,
    verbose: 2,
    max_tps: 15,
    cache: false,
    emit_signals: false,
    profile: false,
    check_determinism: false,
    use_docker: false,
    docker_workdirs: [],
    rounds: 1,
    round_seeds: nil,
    announcer_enabled: true,
    bot_chatter: true,
    ansi_log_path: nil,
    show_timings: false,
    start_paused: false,
    highlight_color: '#ffffff',
    enable_debug: true,
    timeout_scale: 1.0,
}

unless ARGV.include?('--stage')
    ARGV.unshift('--stage', 'current')
end

GENERATORS = %w(arena divided eller icey cellular uniform digger rogue)
stage_title = nil
stage_key = nil
write_profile_json_path = nil

raw_argv = ARGV.dup
dashdash = raw_argv.index('--')

runner_argv = dashdash ? raw_argv[0...dashdash] : raw_argv
passthrough = dashdash ? raw_argv[(dashdash + 1)..] : []

OptionParser.new do |opts|
    opts.banner = <<~BANNER
    Usage: ./runner.rb [options] /path/to/bot1 [/path/to/bot2] [-- bot-args...]

    Bot arguments:
        Everything after '--' is passed to bots and will NOT be parsed by the runner.
        If you run two bots, use '--bot-args' to split arguments:

        ./runner.rb bot1 -- --foo 1 --bar
        ./runner.rb bot1 bot2 -- --bot-args --foo 1 --bot-args --bar 2

    BANNER

    opts.on('--stage STAGE', stages.keys + ['current'], "Stage (default: #{options[:stage]})") do |x|
        options[:stage] = x
        if options[:stage] == 'current'
            # find latest stage
            today = Date.today.strftime('%Y-%m-%d')
            options[:stage] = stages.keys.select do |k|
                today >= stages[k]['from']
            end.sort_by do |k|
                stages[k]['from']
            end.last
        end
        stage = stages[options[:stage]]
        stage_key = options[:stage]
        stage_title = stage['title']
        stage.each_pair do |_key, value|
            key = _key.to_sym
            next if key == :title || key == :from
            if value.is_a?(Integer) || value.is_a?(Float) || value.is_a?(String)
                options[key] = value
            elsif value == true || value == false
                options[key] = value
            end
        end
        options.delete(:stage)
    end
    opts.on("-sSEED", "--seed SEED", String, "Seed (default: random)") do |x|
        options[:seed] = x.to_i(36)
    end
    opts.on("-wWIDTH", "--width WIDTH", Integer, "Arena width (default: #{options[:width]})") do |x|
        options[:width] = x
    end
    opts.on("-hHEIGHT", "--height HEIGHT", Integer, "Arena height (default: #{options[:height]})") do |x|
        options[:height] = x
    end
    opts.on('-gGENERATOR', '--generator GENERATOR', GENERATORS,
        "Arena generator (default: #{options[:generator]})") do |x|
        options[:generator] = x
    end
    opts.on("-tTICKS", "--ticks TICKS", Integer, "Number of ticks (default: #{options[:max_ticks]})") do |x|
        options[:max_ticks] = x
    end
    opts.on("--vis-radius RADIUS", Integer, "Visibility radius (default: #{options[:vis_radius]})") do |x|
        options[:vis_radius] = x
    end
    opts.on("--gem-spawn N", Float, "Gem spawn probability (default: #{options[:gem_spawn_rate]})") do |x|
        options[:gem_spawn_rate] = x
    end
    opts.on("--gem-ttl TTL", Integer, "Gem TTL (default: #{options[:gem_ttl]})") do |x|
        options[:gem_ttl] = x
    end
    opts.on("--max-gems GEMS", Integer, "Max. number of gems (default: #{options[:max_gems]})") do |x|
        options[:max_gems] = x
    end
    opts.on("-e", "--[no-]emit-signals", "Enable gem signals (default: #{options[:emit_signals]})") do |x|
        options[:emit_signals] = x
    end
    opts.on("--signal-radius N", Float, "Gem signal radius (default: #{options[:signal_radius]})") do |x|
        options[:signal_radius] = x
    end
    opts.on("--signal-quantization N", Integer, "Gem signal quantization (default: #{options[:signal_quantization]})") do |x|
        options[:signal_quantization] = x
    end
    opts.on("--signal-noise N", Float, "Gem signal noise (default: #{options[:signal_noise]})") do |x|
        options[:signal_noise] = x
    end
    opts.on("--signal-cutoff N", Float, "Gem signal cutoff (default: #{options[:signal_cutoff]})") do |x|
        options[:signal_cutoff] = x
    end
    opts.on("--signal-fade N", Integer, "Gem signal fade (default: #{options[:signal_fade]})") do |x|
        options[:signal_fade] = x
    end
    opts.on("--[no-]swap-bots", "Swap starting positions (default: #{options[:swap_bots]})") do |x|
        options[:swap_bots] = x
    end
    opts.on("-c", "--[no-]cache", "Enable caching of pre-computed visibility (default: #{options[:cache]})") do |x|
        options[:cache] = x
    end
    opts.on("-p", "--[no-]profile", "Report KPIs (default: #{options[:profile]})") do |x|
        options[:profile] = x
        if x
            options[:rounds] = 20
            options[:verbose] = 0
        end
    end
    opts.on("--write-profile-json PATH", "Write profile results to JSON file") do |x|
        write_profile_json_path = x
    end
    opts.on("--[no-]check-determinism", "Check for deterministic behaviour and exit (default: #{options[:check_determinism]})") do |x|
        options[:check_determinism] = x
    end
    opts.on("-d", "--[no-]use-docker", "Use Docker to run bots (default: #{options[:use_docker]})") do |x|
        options[:use_docker] = x
    end
    opts.on("--docker-workdirs PATHS", "Specify empty persistent working directories for each bot") do |x|
        options[:docker_workdirs] = x.split(',').map { |s| s.strip }
    end
    opts.on("-rN", "--rounds N", Integer, "Rounds (default: #{options[:rounds]})") do |x|
        options[:rounds] = x
        options[:profile] = true
        options[:verbose] = 0
    end
    opts.on("--round-seeds SEEDS", String, "Round seeds (comma separated)") do |x|
        options[:round_seeds] = x.split(',').map { |s| s.strip }
        options[:rounds] = options[:round_seeds].size
    end
    opts.on("-vVERBOSE", "--verbose N", Integer, "Verbosity level (default: #{options[:verbose]})") do |x|
        options[:verbose] = x
    end
    opts.on("--max-tps N", Integer, "Max ticks/second (0 to disable, default: #{options[:max_tps]})") do |x|
        options[:max_tps] = x
    end
    opts.on("--[no-]announcer", "Add announcer to chat log (default: #{options[:announcer_enabled]})") do |x|
        options[:announcer_enabled] = x
    end
    opts.on("--[no-]bot-chatter", "Allow bots to add messages to chat log (default: #{options[:bot_chatter]})") do |x|
        options[:bot_chatter] = x
    end
    opts.on("--ansi-log-path PATH", "Write ANSI and stdin log to JSON file (ends in .json.gz)") do |x|
        options[:ansi_log_path] = x
    end
    opts.on("--[no-]show-timings", "Show timings after run (default: #{options[:show_timings]})") do |x|
        options[:show_timings] = x
    end
    opts.on("--[no-]start-paused", "Start the runner in paused mode (default: #{options[:start_paused]})") do |x|
        options[:start_paused] = x
    end
    opts.on("--highlight-color COLOR", String, "Highlight color (default: #{options[:highlight_color]})") do |x|
        options[:highlight_color] = x
        unless x =~ /\A#[0-9A-Fa-f]{6}\z/
            raise OptionParser::InvalidArgument, "Invalid highlight color '#{x}': expected HTML RGB format like #RRGGBB"
        end
        options[:highlight_color] = x.downcase
    end
    opts.on("--[no-]enable-debug", "Enable debugging commands from bot (default: #{options[:enable_debug]})") do |x|
        options[:enable_debug] = x
    end
    opts.on("--timeout-scale N", Float, "Timeout scale (default: #{options[:timeout_scale]}), 0 to disable timeouts") do |x|
        x = 3600 * 24 if x <= 1e-6
        options[:timeout_scale] = x
    end
end.parse!(runner_argv)

positional = runner_argv.dup

if positional.empty?
    positional = ["random-walker"]
end

if positional.size > 2
    STDERR.puts "Error: At most two bots can compete. If you meant bot args, put them after '--'."
    exit 1
end

bot_paths = positional.map { |x| File.expand_path(x) }

bot_args = [[], []]
current = 0
seen_markers = 0

passthrough.each do |tok|
    if tok == '--bot-args'
        seen_markers += 1
        current = seen_markers - 1
        if current > 1
            STDERR.puts "Error: '--bot-args' can be used at most twice (bot1 then bot2)."
            exit 1
        end
        next
    end
    bot_args[current] << tok
end

if bot_paths.size == 1 && bot_args[1].any?
    STDERR.puts "Error: bot2 args were provided, but only one bot path was given."
    exit 1
end

if options[:swap_bots]
    bot_paths.reverse!
    bot_args.reverse!
    options[:docker_workdirs].reverse!
end

if bot_paths.empty?
    bot_paths << "random-walker"
end

if bot_paths.size > 2
    STDERR.puts "Error: At most two bots can compete."
    exit 1
end

if options[:check_determinism]
    round_seed = Digest::SHA256.digest("#{options[:seed]}/check-determinism").unpack1('L<')
    seed_rng = PCG32.new(round_seed)
    seed = seed_rng.randrange(2 ** 32)
    options[:verbose] = 0
    bot_paths.each.with_index do |path, i|
        STDERR.puts "Checking determinism of bot at #{path}..."
        checksum = nil
        2.times do
            options[:seed] = seed
            runner = Runner.new(**options)
            runner.stage_title = stage_title if stage_title
            runner.stage_key = stage_key if stage_key
            # allow for more time for bot startup and responses during determinism check
            runner.timeout_scale = 10.0
            runner.setup
            runner.add_bot(path, bot_args[i] || [])
            results = runner.run
            if checksum.nil?
                checksum = results[0][:protocol_checksum]
            else
                if checksum != results[0][:protocol_checksum]
                    STDERR.puts "❌ Non-deterministic behaviour detected for bot at #{path}"
                    exit(1)
                end
            end
        end
        STDERR.puts "✅ Bot at #{path} is likely deterministic."
    end
    exit(0)
end

og_seed = options[:seed]

if options[:rounds] == 1
    if options[:round_seeds]
        options[:seed] = options[:round_seeds].first.to_i(36)
    end
    runner = Runner.new(**options)
    runner.stage_title = stage_title if stage_title
    runner.stage_key = stage_key if stage_key
    runner.setup
    bot_paths.each.with_index { |path, i| runner.add_bot(path, bot_args[i] || []) }
    results = runner.run

    if write_profile_json_path
        all_reports = []

        runner.bots.each_with_index do |bot, i|
            score = results[i][:score]
            gu    = results[i][:gem_utilization]
            tc    = results[i][:tile_coverage]
            ttfc  = results[i][:ticks_to_first_capture]
            disq  = results[i][:disqualified_for]
            rts   = results[i][:response_time_stats]
            stderr_log = results[i][:stderr_log]

            report = {}
            report[:timestamp] = Time.now.to_i
            report[:stage_key] = stage_key
            report[:stage_title] = stage_title
            report[:git_hash] = `git describe --always --dirty`.strip
            report[:seed] = og_seed.to_s(36)
            report[:name] = bot[:name]
            report[:emoji] = bot[:emoji]
            report[:total_score] = score

            # For single round, mean values = the round value; CV is not meaningful → nil
            report[:gem_utilization_mean] = gu if gu
            report[:gem_utilization_cv]   = nil
            report[:floor_coverage_mean]  = tc if tc

            round_entry = {
                :options => runner.options_hash(),
                :seed => options[:seed].to_s(36),
                :score => score,
                :gem_utilization => gu,
                :floor_coverage => tc,
                :ticks_to_first_capture => ttfc,
                :disqualified_for => disq,
                :response_time_stats => rts,
            }
            if disq
                round_entry[:stderr_log] = stderr_log
            end

            report[:rounds] = [round_entry]
            all_reports << report
        end

        File.open(write_profile_json_path, 'w') do |f|
            f.write(all_reports.to_json)
        end
    end
else
    round_seed = Digest::SHA256.digest("#{options[:seed]}/rounds").unpack1('L<')
    seed_rng = PCG32.new(round_seed)
    all_score = bot_paths.map { [] }
    all_utilization = bot_paths.map { [] }
    all_ttfc = bot_paths.map { [] }
    all_tc = bot_paths.map { [] }
    all_seed = []
    all_disqualified_for = bot_paths.map { [] }
    all_response_time_stats = bot_paths.map { [] }
    all_stderr_logs = bot_paths.map { [] }
    all_options = []

    bot_data = []

    options[:rounds].times do |i|
        if options[:round_seeds]
            options[:seed] = options[:round_seeds][i].to_i(36)
        else
            options[:seed] = seed_rng.randrange(2 ** 32)
        end
        all_seed << options[:seed]
        runner = Runner.new(**options)
        runner.round = i
        runner.stage_title = stage_title if stage_title
        runner.stage_key = stage_key if stage_key
        runner.setup
        bot_paths.each.with_index { |path, i| runner.add_bot(path, bot_args[i] || []) }
        if i == 0
            runner.bots.each.with_index do |bot, k|
                bot_data << {:name => bot[:name], :emoji => bot[:emoji]}
            end
        end
        all_options << runner.options_hash()
        results = runner.run
        (0...bot_paths.size).each do |k|
            all_score[k] << results[k][:score]
            all_utilization[k] << results[k][:gem_utilization]
            all_ttfc[k] << results[k][:ticks_to_first_capture]
            all_tc[k] << results[k][:tile_coverage]
            all_disqualified_for[k] << results[k][:disqualified_for]
            all_response_time_stats[k] << results[k][:response_time_stats]
            all_stderr_logs[k] << results[k][:stderr_log]
        end
    end
    puts

    all_reports = []
    bot_data.each.with_index do |data, i|
        puts "Results for #{data[:emoji]} #{data[:name]}"
        n     = all_utilization[i].size
        mean  = all_utilization[i].sum(0.0) / n
        var   = all_utilization[i].map { |x| (x - mean) ** 2 }.sum / n
        sd    = Math.sqrt(var)
        cv    = sd / mean * 100.0
        puts sprintf("Total Score     : %5d", all_score[i].sum)
        puts sprintf("Gem Utilization : %5.1f %%", mean)
        if cv.nan?
            puts sprintf("Chaos Factor    :     -")
        else
            puts sprintf("Chaos Factor    : %5.1f %%", cv)
        end
        puts sprintf("Floor Coverage  : %5.1f %%", mean(all_tc[i]))
        report = {}
        report[:timestamp] = Time.now.to_i
        report[:stage_key] = stage_key
        report[:stage_title] = stage_title
        report[:git_hash] = `git describe --always --dirty`.strip
        report[:seed] = og_seed.to_s(36)
        report[:name] = data[:name]
        report[:emoji] = data[:emoji]
        report[:total_score] = all_score[i].sum
        report[:gem_utilization_mean] = mean
        report[:gem_utilization_cv] = cv.nan? ? nil : cv
        report[:floor_coverage_mean] = mean(all_tc[i])
        report[:rounds] = all_score[i].map.with_index do |_, k|
            d = {
                :options => all_options[k],
                :seed => all_seed[k].to_s(36),
                :score => all_score[i][k],
                :gem_utilization => all_utilization[i][k],
                :floor_coverage => all_tc[i][k],
                :ticks_to_first_capture => all_ttfc[i][k],
                :disqualified_for => all_disqualified_for[i][k],
                :response_time_stats => all_response_time_stats[i][k],
            }
            if d[:disqualified_for]
                d[:stderr_log] = all_stderr_logs[i][k]
            end
            d
        end
        all_reports << report
    end
    if write_profile_json_path
        File.open(write_profile_json_path, 'w') do |f|
            f.write(all_reports.to_json)
        end
    end
end

if options[:show_timings]
    $timings.report
end
