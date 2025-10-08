#!/usr/bin/env ruby

$LOAD_PATH.unshift File.expand_path("include/unicode-emoji-4.0.4/lib", __dir__)
$LOAD_PATH.unshift File.expand_path("include/unicode-display_width-3.1.5/lib", __dir__)
$LOAD_PATH.unshift File.expand_path("include/paint-2.3.0/lib", __dir__)

require './include/fov_angle.rb'
require './include/pcg32.rb'
require './include/timings.rb'

require 'digest'
require 'fileutils'
require "io/console"
require 'json'
require 'open3'
require 'optparse'
require 'paint'
require 'set'
require 'stringio'
require 'unicode/display_width'
require 'yaml'
require 'zlib'

SOFT_LIMIT            = 0.100
HARD_LIMIT            = 0.200
HARD_LIMIT_FIRST_TICK = 20.0
OVERTIME_BUDGET       = 1.5

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

class Runner

    UI_BACKGROUND = '#143b86'
    UI_FOREGROUND = '#e7e6e1'

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

    attr_accessor :round, :stage_title, :stage_key
    attr_reader :bots, :rng

    def initialize(seed:, width:, height:, generator:, max_ticks:,
                   vis_radius:, gem_spawn_rate:, gem_ttl:, max_gems:,
                   emit_signals:, signal_radius:, signal_quantization:,
                   signal_noise:, signal_cutoff:, signal_fade:, swap_bots:,
                   cache:, profile:, check_determinism:, use_docker:,
                   rounds:, verbose:, max_tps:, announcer_enabled:,
                   ansi_log_path:, show_timings:
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
        @rounds = rounds
        @verbose = verbose
        @max_tps = max_tps
        @bots = []
        @bots_io = []
        @gems = []
        @chatlog = []
        @stage_title = '(no stage)'
        @stage_key = '(no stage)'
        @announcer_enabled = announcer_enabled
        @ansi_log_path = ansi_log_path
        @ansi_log = []
        @show_timings = show_timings
    end

    def gen_maze
        command = "node include/maze.js --width #{@width} --height #{@height} --generator #{@generator} --seed #{@seed} --wall \"#\" --floor \".\""
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
            @checksum = Digest::SHA256.hexdigest(@maze.to_json)
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
            if @swap_bots
                @spawn_points.reverse!
            end
            @message_queue = Queue.new

            visibility_path = "cache/#{@checksum}.marshal.gz"

            if @cache && File.exist?(visibility_path)
                data = safe_gzip_read(visibility_path)
                @visibility = Marshal.load(data)
            else
                # pre-calculate visibility from each tile
                @visibility = {}
                (0...@height).each do |y|
                    (0...@width).each do |x|
                        offset = (y << 16) | x
                        v = Set.new
                        unless @maze.include?(offset)
                            visible = FOVAngle.visible(@width, @height, @maze, x, y, radius: @vis_radius) { |xx, yy| @maze.include?((yy << 16) | xx) }
                            v = visible.to_a.map { |p| (p[1] << 16) | p[0] }.sort
                        end
                        @visibility[offset] = Set.new(v)
                    end
                end

                if @cache
                    bytes = Marshal.dump(@visibility)
                    atomic_gzip_write(visibility_path, bytes)
                end
            end

            begin
                @terminal_height, @terminal_width = $stdout.winsize
            rescue
                @terminal_height = 24
                @terminal_width = 80
            end

            @tile_width = 2

            if @ansi_log_path
                @terminal_width = @width * @tile_width
                @terminal_height = @height + 1
            end

            @enable_chatlog = false
            @chatlog_position = nil
            @chatlog_width = 0
            @chatlog_height = 0

            if @verbose >= 2 && @ansi_log_path.nil?
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
        end
    end

    def start_bot(_path, &block)
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
                    '--memory=256m',
                    '--memory-swap=256m',
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
                    "--tmpfs", "/app:rw,nosuid,nodev,exec,size=64m,uid=1000,gid=1000,mode=1777",
                    'hidden-gems-runner'
                ]
                # STDERR.puts args.join(' ')
                # exit
                stdin, stdout, stderr, wait_thr = Open3.popen3(*args)
            else
                stdin, stdout, stderr, wait_thr = Open3.popen3(path, chdir: File.dirname(path))
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

    def mix_rgb_hex(c1, c2, t)
        x = c1[1..].scan(/../).map { |h| h.to_i(16) }
        y = c2[1..].scan(/../).map { |h| h.to_i(16) }

        r = (x[0] + (y[0] - x[0]) * t).round.clamp(0, 255)
        g = (x[1] + (y[1] - x[1]) * t).round.clamp(0, 255)
        b = (x[2] + (y[2] - x[2]) * t).round.clamp(0, 255)

        format("#%02X%02X%02X", r, g, b)
    end

    def vwidth(str)
        Unicode::DisplayWidth.of(str.to_s, emoji: true, ambwidth: 1)
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

        tokens = text.scrub.split(/\s+/).flat_map { |t| chunk_token(t, body_w) }
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
        out << line unless line.empty?

        out.each_with_index.map { |l, i| (i == 0 ? prefix : indent) + l }
    end

    def render_chatlog(entries, width, height)
        lines = []
        prev_emoji = nil

        entries.each do |e|
            show_prefix = (e[:emoji] != prev_emoji)
            lines.concat(wrap_entry(e[:text], e[:emoji], width, show_prefix: show_prefix))
            prev_emoji = e[:emoji]
        end

        lines.last([height, 0].max).map { |l| l.ljust(width) }
    end

    def render(signal_level)
        StringIO.open do |io|
            io.print "\033[H" if @verbose >= 2

            score_s = @bots.map { |x| "#{x[:emoji]} #{x[:score]}" }.join(' : ')

            status_line = sprintf("  Stage: #{@stage_key}  │  Seed: #{@seed.to_s(36)}  │  Tick: %#{(@max_ticks - 1).to_s.size}d  │  Score: #{score_s}", @tick)
            status_line = status_line[0, @terminal_width] if status_line.size > @terminal_width
            status_line = status_line + ' ' * (@terminal_width - vwidth(status_line))

            io.puts Paint[status_line, UI_FOREGROUND, UI_BACKGROUND]

            paint_rng = PCG32.new(1234)

            bots_visible = @bots.map do |bot|
                @visibility[(bot[:position][1] << 16) | bot[:position][0]]
            end

            chat_lines = nil
            if @enable_chatlog
                chat_lines = render_chatlog(@chatlog, @chatlog_width, @chatlog_height)
            end

            (0...@height).each do |y|
                (0...@width).each do |x|
                    c = ' ' * @tile_width
                    bg = FLOOR_COLOR
                    if @maze.include?((y << 16) | x)
                        bg = mix_rgb_hex(WALL_COLOR, '#000000', paint_rng.next_float() * 0.25)
                    end
                    @bots.each.with_index do |bot, i|
                        next if bot[:disqualified_for]
                        p = bot[:position]
                        if p[0] == x && p[1] == y
                            c = @bots[i][:emoji]
                            while vwidth(c) < @tile_width
                                c += ' '
                            end
                        end
                    end
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
                    unless @tiles_revealed.any? { |s| s.include?((y << 16) | x) }
                        bg = mix_rgb_hex(bg, '#000000', 0.5)
                    end
                    io.print Paint[c, nil, bg]
                end
                if @enable_chatlog && @chatlog_position == :right
                    io.print ' '
                    io.print chat_lines[y]
                end
                io.puts unless (@ansi_log_path && (y == @height - 1))
            end
            if @enable_chatlog && @chatlog_position == :bottom
                chat_lines.each do |line|
                    io.puts line
                end
            end
            io.string
        end
    end

    def add_bot(path)
        @bots << {:position => @spawn_points.shift, :score => 0, :name => "Botty McBotface", :emoji => '🤖', :overtime_used => 0.0, :disqualified_for => nil, :stderr_lines => []}
        yaml_path = File.join(File.expand_path(path), 'bot.yaml')
        if File.exist?(yaml_path)
            info = YAML.load(File.read(yaml_path))
            @bots.last[:name] = info['name'] if info['name'].is_a?(String)
            @bots.last[:emoji] = info['emoji'] if info['emoji'].is_a?(String)
            if vwidth(@bots.last[:emoji]) > 2
                raise "Error in bot.yaml: emoji must be at most 2 characters wide (#{@bots.last[:emoji]} is #{vwidth(@bots.last[:emoji])} characters wide)"
            end
        end
        bot_index = @bots_io.size
        @bots_io << start_bot(path) do |line|
            @message_queue << {:bot => bot_index, :line => line}
        end
    end

    def add_gem()
        candidate_tiles = @floor_tiles_set.dup
        # don't spawn gem near bot
        @bots.each do |bot|
            candidate_tiles.delete((bot[:position][1] << 16) | bot[:position][0])
        end
        # don't spawn gem on another gem
        @gems.each do |gem|
            candidate_tiles.delete((gem[:position][1] << 16) | gem[:position][0])
        end
        return 0 if candidate_tiles.empty?
        gem = {:position_offset => @rng.sample(candidate_tiles.to_a.sort), :ttl => @gem_ttl}
        gem[:position] = [gem[:position_offset] & 0xFFFF, gem[:position_offset] >> 16]

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
            @bots_io.each do |b|
                b.wait_thr.join(0.2) or Process.kill(Gem.win_platform? ? "KILL" : "TERM", b.wait_thr.pid) rescue nil
            end
            exit
        end
        print "\033[2J" if @verbose >= 2
        @tick = -1
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
        begin
            print "\033[?25l" if @verbose >= 2
            loop do
                until @message_queue.empty?
                    temp = @message_queue.pop(true) rescue nil
                    next if temp.nil?
                    @chatlog << {emoji: @bots[temp[:bot]][:emoji], text: temp[:line].chomp}
                    @bots[temp[:bot]][:stderr_lines] << temp[:line].chomp
                end
                # STEP 0: Advance tick
                @tick += 1
                break if @tick >= @max_ticks

                (0...@bots.size).each do |i|
                    @protocol[i] << {}
                    @protocol[i].last[:tick] = @tick
                    @protocol[i].last[:rng_state] = @rng.snapshot
                end
                tf0 = Time.now.to_f

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

                # STEP 2: RENDER
                if @verbose >= 2 || @ansi_log_path
                    screen = render(signal_level)
                    # @protocol.last[:screen] = screen
                    if @verbose >= 2
                        print screen
                    end
                    if @ansi_log_path
                        @ansi_log << {:screen => screen}
                    end
                end
                t1 = Time.now.to_f
                @tps = (@tick.to_f / (t1 - t0)).round
                if @verbose == 1
                    print "\rTick: #{@tick} @ #{@tps} tps"
                end

                break if @bots.all? { |b| b[:disqualified_for] }

                bot_with_initiative = ((@tick + (@swap_bots ? 1 : 0)) % @bots.size)

                # STEP 3: QUERY BOTS: send data, get response, move but don't collect

                @bots.each.with_index do |bot, i|
                    next if bot[:disqualified_for]
                    bot_position = bot[:position]

                    data = {}
                    $timings.profile("prepare data") do
                        if @tick == 0
                            data[:config] = {}
                            %w(stage_key width height generator max_ticks emit_signals vis_radius max_gems
                            gem_spawn_rate gem_ttl signal_radius signal_cutoff signal_noise
                            signal_quantization signal_fade).each do |key|
                                data[:config][key.to_sym] = instance_variable_get("@#{key}")
                            end
                            bot_seed = Digest::SHA256.digest("#{@seed}/bot").unpack1('L<')
                            data[:config][:bot_seed] = bot_seed
                        end
                        data[:tick] = @tick
                        data[:bot] = bot_position
                        data[:wall] = []
                        data[:floor] = []
                        data[:initiative] = (bot_with_initiative == i)
                        data[:visible_gems] = []

                        @visibility[(bot_position[1] << 16) | bot_position[0]].each do |t|
                            key = @maze.include?(t) ? :wall : :floor
                            data[key] << [t & 0xFFFF, t >> 16]
                            @gems.each do |gem|
                                if gem[:position_offset] == t
                                    data[:visible_gems] << {:position => gem[:position], :ttl => gem[:ttl]}
                                end
                            end
                        end
                        if @emit_signals
                            level_sum = 0.0
                            @gems.each.with_index do |gem, i|
                                level_sum += signal_level[i][(bot_position[1] << 16) | bot_position[0]] || 0.0
                            end
                            data[:signal_level] = format("%.6f", level_sum).to_f
                        end
                    end

                    start_mono = Process.clock_gettime(Process::CLOCK_MONOTONIC)
                    deadline_mono = start_mono + (@tick == 0 ? HARD_LIMIT_FIRST_TICK : HARD_LIMIT)

                    if @ansi_log_path
                        @ansi_log.last[:stdin] = data
                    end
                    $timings.profile("write to bot's stdin") do
                        begin
                            @bots_io[i].stdin.puts(data.to_json)
                            @bots_io[i].stdin.flush
                        rescue Errno::EPIPE
                            # bot has terminated unexpectedly
                            if @bots[i][:disqualified_for].nil?
                                @bots[i][:disqualified_for] = 'terminated_unexpectedly'
                                if @announcer_enabled
                                    @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{bot[:name]} has terminated unexpectedly!" }
                                end
                            end
                        end
                    end

                    @protocol[i].last[:bots] ||= {}
                    @protocol[i].last[:bots][:data] = data

                    # line = @bots_io[i].stdout.gets.strip
                    status = line = nil
                    $timings.profile("read from bot's stdout") do
                        status, line = read_line_before_deadline(@bots_io[i].stdout, deadline_mono)
                    end
                    elapsed = Process.clock_gettime(Process::CLOCK_MONOTONIC) - start_mono
                    if status == :hard_timeout
                        if @bots[i][:disqualified_for].nil?
                            @bots[i][:disqualified_for] = "hard_timeout"
                            if @announcer_enabled
                                @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{bot[:name]} took too long to respond (#{(elapsed * 1000).to_i} ms) and has been terminated!" }
                            end
                        end
                    elsif status == :eof
                        if @bots[i][:disqualified_for].nil?
                            @bots[i][:disqualified_for] = 'terminated_unexpectedly'
                            if @announcer_enabled
                                @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{bot[:name]} has terminated unexpectedly!" }
                            end
                        end
                    elsif status == :ok
                        overtime = (@tick == 0) ? 0.0 : (elapsed - SOFT_LIMIT)
                        @bots[i][:overtime_used] += overtime if overtime > 0.0
                        if @announcer_enabled && overtime > 0.0
                            @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{bot[:name]} exceeded soft limit by #{(overtime * 1e3).to_i} ms (total overtime: #{(@bots[i][:overtime_used] * 1e3).to_i} ms)" }
                        end
                        if @bots[i][:overtime_used] > OVERTIME_BUDGET
                            if @bots[i][:disqualified_for].nil?
                                @bots[i][:disqualified_for] = 'overtime_budget_exceeded'
                                if @announcer_enabled
                                    @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{bot[:name]} has exceeded their overtime budget and is disqualified!" }
                                end
                            end
                        end

                        @protocol[i].last[:bots][:response] = line
                        command = line.split(' ').first
                        if ['N', 'E', 'S', 'W'].include?(command)
                            dir = {'N' => [0, -1], 'E' => [1, 0], 'S' => [0, 1], 'W' => [-1, 0]}
                            dx = bot_position[0] + dir[command][0]
                            dy = bot_position[1] + dir[command][1]
                            if dx >= 0 && dy >= 0 && dx < @width && dy < @height
                                unless @maze.include?((dy << 16) | dx)
                                    @bots[i][:position] = [dx, dy]
                                end
                            end
                        elsif command == 'WAIT'
                        else
                            # invalid command!
                        end
                    end
                end

                if @verbose >= 2 && @max_tps > 0
                    loop do
                        tf1 = Time.now.to_f
                        break if tf1 - tf0 > 1.0 / @max_tps
                        sleep [(1.0 / @max_tps - tf1 + tf0), 0.0].max
                    end
                end

                # STEP 4: COLLECT GEMS & DECAY GEMS
                collected_gems = []
                @gems.each.with_index do |gem, i|
                    (0...@bots.size).each do |_k|
                        next if @bots[_k][:disqualified_for]
                        k = (_k + bot_with_initiative) % @bots.size
                        bot = @bots[k]
                        if bot[:position] == gem[:position]
                            collected_gems << i
                            bot[:score] += gem[:ttl]
                            results[k][:ticks_to_first_capture] ||= @tick
                            if @announcer_enabled
                                @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{bot[:name]} scored a gem with #{gem[:ttl]} points!" }
                            end
                        end
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

                if @rng.next_float() < @gem_spawn_rate && @gems.size < @max_gems
                    ttl_spawned += add_gem()
                end
                begin
                    STDIN.raw do |stdin|
                        if IO.select([STDIN], nil, nil, 0)
                            key = stdin.getc
                            if key == "q"
                                exit
                            end
                        end
                    end
                rescue
                end
            end
            @bots_io.each do |b|
                begin
                    Process.kill(Gem.win_platform? ? "KILL" : "TERM", b.wait_thr.pid)
                rescue Errno::ESRCH
                end
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
            results[i][:stderr_lines] = bot[:stderr_lines]
            if @profile
                results[i][:gem_utilization] = (ttl_spawned > 0 ? (bot[:score].to_f / ttl_spawned.to_f * 100.0 * 100).to_i.to_f / 100 : 0.0)
                results[i][:tile_coverage] = ((@tiles_revealed[i] & @floor_tiles_set).size.to_f / @floor_tiles_set.size.to_f * 100.0 * 100).to_i.to_f / 100
            end
            if @check_determinism
                results[i][:protocol_checksum] = Digest::SHA256.hexdigest(@protocol[i].to_json)
            end
        end
        if @ansi_log_path
            File.open(@ansi_log_path, 'w') do |f|
                f.write(@ansi_log.to_json)
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
    rounds: 1,
    announcer_enabled: true,
    ansi_log_path: nil,
    show_timings: false,
}

unless ARGV.include?('--stage')
    ARGV.unshift('--stage', 'current')
end

GENERATORS = %w(arena divided eller icey cellular uniform digger rogue)
stage_title = nil
stage_key = nil
write_profile_json_path = nil
OptionParser.new do |opts|
    opts.banner = "Usage: ./runner.rb [options] /path/to/bot1 [/path/to/bot2]"

    opts.on('--stage STAGE', stages.keys,
        "Stage (default: #{options[:stage]})") do |x|
        options[:stage] = x
        options[:stage] = stages['current'] if options[:stage] == 'current'
        stage = stages[options[:stage]]
        stage_key = options[:stage]
        stage_title = stage['title']
        stage.each_pair do |_key, value|
            key = _key.to_sym
            next if key == :title
            if value.is_a? Integer
                options[key] = value
            elsif value.is_a? Float
                options[key] = value
            elsif value.is_a? String
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
    opts.on("-rN", "--rounds N", Integer, "Rounds (default: #{options[:rounds]})") do |x|
        options[:rounds] = x
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
    opts.on("--ansi-log-path PATH", "Write ANSI and stdin log to JSON file") do |x|
        options[:ansi_log_path] = x
    end
    opts.on("--[no-]show-timings", "Show timings after run (default: #{options[:show_timings]})") do |x|
        options[:show_timings] = x
    end
end.parse!

bot_paths = ARGV.map do |x|
    File.expand_path(x)
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
    bot_paths.each do |path|
        STDERR.puts "Checking determinism of bot at #{path}..."
        checksum = nil
        2.times do
            options[:seed] = seed
            runner = Runner.new(**options)
            runner.stage_title = stage_title if stage_title
            runner.stage_key = stage_key if stage_key
            runner.setup
            runner.add_bot(path)
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
    runner = Runner.new(**options)
    runner.stage_title = stage_title if stage_title
    runner.stage_key = stage_key if stage_key
    runner.setup
    bot_paths.each { |path| runner.add_bot(path) }
    runner.run
else
    round_seed = Digest::SHA256.digest("#{options[:seed]}/rounds").unpack1('L<')
    seed_rng = PCG32.new(round_seed)
    all_score = bot_paths.map { [] }
    all_utilization = bot_paths.map { [] }
    all_ttfc = bot_paths.map { [] }
    all_tc = bot_paths.map { [] }
    all_seed = []
    all_disqualified_for = bot_paths.map { [] }
    all_stderr_lines = bot_paths.map { [] }

    bot_data = []

    options[:rounds].times do |i|
        options[:seed] = seed_rng.randrange(2 ** 32)
        all_seed << options[:seed]
        runner = Runner.new(**options)
        runner.round = i
        runner.stage_title = stage_title if stage_title
        runner.stage_key = stage_key if stage_key
        runner.setup
        bot_paths.each { |path| runner.add_bot(path) }
        if i == 0
            runner.bots.each.with_index do |bot, k|
                bot_data << {:name => bot[:name], :emoji => bot[:emoji]}
            end
        end
        results = runner.run
        (0...bot_paths.size).each do |k|
            all_score[k] << results[k][:score]
            all_utilization[k] << results[k][:gem_utilization]
            all_ttfc[k] << results[k][:ticks_to_first_capture]
            all_tc[k] << results[k][:tile_coverage]
            all_disqualified_for[k] << results[k][:disqualified_for]
            all_stderr_lines[k] << results[k][:stderr_lines]
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
            {
                :seed => all_seed[k].to_s(36),
                :score => all_score[i][k],
                :gem_utilization => all_utilization[i][k],
                :floor_coverage => all_tc[i][k],
                :ticks_to_first_capture => all_ttfc[i][k],
                :disqualified_for => all_disqualified_for[i][k],
                :stderr_lines => all_stderr_lines[i][k],
            }
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