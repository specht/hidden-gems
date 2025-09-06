#!/usr/bin/env ruby

$LOAD_PATH.unshift File.expand_path("include/unicode-emoji-4.0.4/lib", __dir__)
$LOAD_PATH.unshift File.expand_path("include/unicode-display_width-3.1.5/lib", __dir__)
$LOAD_PATH.unshift File.expand_path("include/paint-2.3.0/lib", __dir__)

require './include/pcg32.rb'

require 'digest'
require 'fileutils'
require "io/console"
require 'json'
require 'open3'
require 'optparse'
require 'paint'
require 'set'
require 'unicode/display_width'
require 'yaml'
require 'zlib'

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

module FOVAngle
    module_function

    TAU = 2 * Math::PI
    EPS = 1e-12

    # Public: compute visible cells within radius (Euclidean).
    # grid[y][x]; origin (ox,oy); pass a block { |x,y| opaque? }
    # Returns a Set of [x,y].
    def visible(w, h, grid, ox, oy, radius:)
        raise ArgumentError, "need an opaque? block" unless block_given?

        return Set.new unless ox.between?(0, w-1) && oy.between?(0, h-1)

        r2 = radius * radius

        # Build list of candidate offsets within radius and in-bounds (exclude [0,0])
        cells = []
        min_dx = -[ox, radius].min
        max_dx = [w - 1 - ox, radius].min
        min_dy = -[oy, radius].min
        max_dy = [h - 1 - oy, radius].min

        (min_dy..max_dy).each do |dy|
            (min_dx..max_dx).each do |dx|
                next if dx.zero? && dy.zero?
                next unless dx * dx + dy * dy < r2
                cells << [dx, dy, dx * dx + dy * dy]
            end
        end

        # Sort by squared distance; we’ll process equal-distance groups together
        cells.sort_by! { |_, _, d2| d2 }

        vis = Set.new
        vis << [ox, oy]

        blocked = AngleUnion.new
        pending = [] # intervals to add after current distance group

        i = 0
        while i < cells.length
            j = i
            d2 = cells[i][2]
            j += 1 while j < cells.length && cells[j][2] == d2

            # For all cells at this exact distance, decide visibility using 'blocked' (nearer occluders only)
            (i...j).each do |k|
                dx, dy, _ = cells[k]
                x = ox + dx
                y = oy + dy

                # Compute the angular span(s) of this tile as seen from origin
                spans = tile_spans(dx, dy) # array of [start,end] in [0,TAU), non-wrapping
                any_uncovered = spans.any? { |s, e| !blocked.covered?(s, e) }

                if any_uncovered
                    vis << [x, y]
                    # If the tile is opaque, it blocks farther tiles. Queue its spans to add after this group.
                    if yield(x, y)
                        pending.concat(spans)
                    end
                end
            end

            # Commit occluders from this ring
            pending.each { |s, e| blocked.add(s, e) }
            pending.clear

            i = j
        end

        vis
    end

    # ---- Angle math helpers ----

    # Angular coverage of a tile centered at (dx,dy), with corners at (dx±0.5, dy±0.5)
    # Returns 1 or 2 non-wrapping intervals [start,end] in [0,TAU)
    def tile_spans(dx, dy)
        corners = [
            [dx - 0.5, dy - 0.5], [dx + 0.5, dy - 0.5],
            [dx + 0.5, dy + 0.5], [dx - 0.5, dy + 0.5]
        ]
        ang = corners.map { |x, y| norm_angle(Math.atan2(y, x)) }.sort
        # Find the largest gap; the tile’s minimal covering interval is the complement
        gaps = 4.times.map do |i|
            a = ang[i]
            b = ang[(i + 1) % 4]
            gap = b - a
            gap += TAU if gap < 0
            [gap, i]
        end
        _, imax = gaps.max_by { |g, _| g }
        start = ang[(imax + 1) % 4]
        finish = ang[imax]
        finish += TAU if finish < start
        # Split if it wraps
        if finish >= TAU
            [[start, TAU], [0.0, finish - TAU]]
        else
            [[start, finish]]
        end
    end

    def norm_angle(a)
        a %= TAU
        a += TAU if a < 0
        a
    end

    # Maintains a union of disjoint angular intervals on [0,TAU)
    class AngleUnion
        def initialize
            @iv = [] # array of [s,e], with 0 <= s < e <= TAU, non-overlapping, sorted by s
        end

        # Add [s,e] (non-wrapping). Use add_span for arbitrary spans.
        def add(s, e)
            i = 0
            while i < @iv.length && @iv[i][1] < s - EPS
                i += 1
            end
            ns = s
            ne = e
            while i < @iv.length && @iv[i][0] <= ne + EPS
                ns = [ns, @iv[i][0]].min
                ne = [ne, @iv[i][1]].max
                @iv.delete_at(i)
            end
            @iv.insert(i, [ns, ne])
        end

        # Convenience for arbitrary (possibly wrapping) span
        def add_span(s, e)
            if s <= e
                add(s, e)
            else
                add(s, TAU)
                add(0.0, e)
            end
        end

        # Return true if [s,e] is fully covered by the union
        def covered?(s, e)
            # Handle wrapping by splitting before query
            return covered?(s, TAU) && covered?(0.0, e) if s > e

            # Find first interval whose end reaches s
            i = 0
            i += 1 while i < @iv.length && @iv[i][1] < s - EPS
            return false if i >= @iv.length || @iv[i][0] > s + EPS

            cover = @iv[i][1]
            return true if cover >= e - EPS

            i += 1
            while i < @iv.length && @iv[i][0] <= cover + EPS
                cover = [cover, @iv[i][1]].max
                return true if cover >= e - EPS
                i += 1
            end
            false
        end
    end
end

class Runner

    UI_BACKGROUND = '#143b86'
    UI_FOREGROUND = '#e7e6e1'

    PORTAL_EMOJIS = ['🔴', '🔵', '🟢', '🟡']
    ANTENNA_EMOJI = '📡'
    GEM_EMOJI = '💎'
    GEM_COLOR = '#238acc'
    FLOOR_COLOR = '#222728'
    WALL_COLOR = '#555753'

    Bot = Struct.new(:stdin, :stdout, :stderr, :wait_thr)

    attr_accessor :round

    def initialize(seed:, width:, height:, generator:, max_ticks:,
                   vis_radius:, gem_spawn_rate:, gem_ttl:, max_gems:,
                   emit_signals:, signal_radius:, signal_quantization:,
                   signal_noise:, signal_cutoff:, signal_fade:, swap_bots:,
                   verbose:, max_tps:, cache:, rounds:, profile:
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
        @verbose = verbose
        @max_tps = max_tps
        @cache = cache
        @rounds = rounds
        @profile = profile
        @bots = []
        @bots_io = []
        @gems = []
    end

    def start_bot(_path, &block)
        path = File.join(File.expand_path(_path), 'start.sh')
        stdin, stdout, stderr, wait_thr = Open3.popen3(path, chdir: File.dirname(path))
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

    def gen_maze
        command = "node include/maze.js --width #{@width} --height #{@height} --generator #{@generator} --seed #{@seed} --wall \"#\" --floor \".\""
        maze = `#{command}`.strip.split("\n").map { |x| x.strip }.select do |line|
            line =~ /^[\.#]+$/
        end.map.with_index do |line, y|
            row = line.split('').map.with_index { |e, x| e == '#' ? (y << 16) | x : nil }
        end.flatten.reject { |x| x.nil? }
        Set.new(maze)
    end

    def mix_rgb_hex(c1, c2, t)
        x = c1[1..].scan(/../).map { |h| h.to_i(16) }
        y = c2[1..].scan(/../).map { |h| h.to_i(16) }

        r = (x[0] + (y[0] - x[0]) * t).round.clamp(0, 255)
        g = (x[1] + (y[1] - x[1]) * t).round.clamp(0, 255)
        b = (x[2] + (y[2] - x[2]) * t).round.clamp(0, 255)

        format("#%02X%02X%02X", r, g, b)
    end

    def render(signal_level)
        StringIO.open do |io|
            terminal_height, terminal_width = $stdout.winsize
            tile_width = 3

            io.print "\033[H" if @verbose >= 2

            status_line = sprintf("  Seed: #{@seed.to_s(36)}  │  Tick: %#{(@max_ticks - 1).to_s.size}d  │  %d tps  │  Score: #{@bots[0][:score]}", @tick, @tps)
            status_line = status_line + ' ' * (terminal_width - status_line.size)

            io.puts Paint[status_line, UI_FOREGROUND, UI_BACKGROUND]

            paint_rng = PCG32.new(1234)

            bots_visible = @bots.map do |bot|
                @visibility[(bot[:position][1] << 16) | bot[:position][0]]
            end

            (0...@height).each do |y|
                (0...@width).each do |x|
                    c = ' ' * tile_width
                    bg = FLOOR_COLOR
                    fg = WALL_COLOR
                    if @maze.include?((y << 16) | x)
                        c = '█' * tile_width
                        fg = mix_rgb_hex(WALL_COLOR, '#000000', paint_rng.next_float() * 0.25)
                    end
                    @bots.each.with_index do |bot, i|
                        p = bot[:position]
                        if p[0] == x && p[1] == y
                            c = @bots[i][:emoji]
                            while Unicode::DisplayWidth.of(c) < tile_width
                                c += ' '
                            end
                        end
                    end
                    @gems.each.with_index do |p, i|
                        if p[:position][0] == x && p[:position][1] == y
                            c = GEM_EMOJI
                            while Unicode::DisplayWidth.of(c) < tile_width
                                c += ' '
                            end
                        end
                        if @emit_signals
                            if signal_level[i].include?((y << 16) | x)
                                bg = mix_rgb_hex(GEM_COLOR, bg, 1.0 - signal_level[i][(y << 16) | x])
                            end
                        end
                    end
                    unless @tiles_revealed.include?((y << 16) | x)
                        fg = mix_rgb_hex(fg, '#000000', 0.5)
                        bg = mix_rgb_hex(bg, '#000000', 0.5)
                    end
                    io.print Paint[c, fg, bg]
                end
                io.puts
            end
            io.string
        end
    end

    def setup
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

        visibility_path = "cache/#{@checksum}.yaml.gz"
        if @cache && File.exist?(visibility_path)
            Zlib::GzipReader.open(visibility_path) do |gz|
                @visibility = YAML.load(gz.read, permitted_classes: [Set])
            end
        else
            # pre-calculate visibility from each tile
            @visibility = {}
            (0...@height).each do |y|
                (0...@width).each do |x|
                    offset = (y << 16) | x
                    v = Set.new()
                    unless @maze.include?(offset)
                        visible = FOVAngle.visible(@width, @height, @maze, x, y, radius: @vis_radius) { |x, y| @maze.include?((y << 16) | x) }
                        v = visible.to_a.map do |p|
                            (p[1] << 16) | p[0]
                        end.sort
                    end
                    @visibility[offset] = Set.new(v)
                end
            end
            if @cache
                FileUtils.mkpath(File.dirname(visibility_path))
                Zlib::GzipWriter.open(visibility_path) do |gz|
                    gz.write @visibility.to_yaml
                end
            end
        end

        @tiles_revealed = Set.new()
        @tiles_reported = Set.new()
    end

    def add_bot(path)
        @bots << {:position => @spawn_points.shift, :score => 0, :name => "Botty McBotface", :emoji => '🤖'}
        yaml_path = File.join(File.expand_path(path), 'bot.yaml')
        if File.exist?(yaml_path)
            info = YAML.load(File.read(yaml_path))
            @bots.last[:name] = info['name'] if info['name'].is_a?(String)
            @bots.last[:emoji] = info['emoji'] if info['emoji'].is_a?(String)
        end
        bot_index = @bots_io.size
        @bots_io << start_bot(path) do |line|
            if @verbose >= 2
                @message_queue << {:bot => bot_index, :line => line}
                STDERR.puts "Bot says: #{line}"
            end
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
        gem = {:position_offset => @rng.sample(candidate_tiles.to_a), :ttl => @gem_ttl}
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
        return @gem_ttl
    end

    def run
        trap("INT") do
            @bots_io.each { |b| b.stdin.close rescue nil }
            @bots_io.each do |b|
                b.wait_thr.join(0.2) or Process.kill("TERM", b.wait_thr.pid) rescue nil
            end
            exit
        end
        print "\033[2J" if @verbose >= 2
        @tick = -1
        @tps = 0
        t0 = Time.now.to_f
        STDIN.echo = false
        first_capture = nil
        spawned_ttl = 0
        @protocol = []
        begin
            print "\033[?25l" if @verbose >= 2
            loop do
                unless @protocol.empty?
                    until @message_queue.empty?
                        temp = @message_queue.pop(true) rescue nil
                        next if temp.nil?
                        @protocol.last[:messages] ||= []
                        @protocol.last[:messages] << temp
                    end
                end
                # STEP 0: Advance tick
                @tick += 1
                break if @tick >= @max_ticks

                @protocol << { }
                @protocol.last[:tick] = @tick
                @protocol.last[:rng_state] = @rng.snapshot
                # @protocol.last[:gems] = @gems
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

                bot_position = @bots[0][:position]
                @visibility[(bot_position[1] << 16) | bot_position[0]].each do |t|
                    @tiles_revealed << t
                end

                # STEP 2: RENDER
                if @verbose >= 2
                    screen = render(signal_level)
                    @protocol.last[:screen] = screen
                    print screen
                end
                t1 = Time.now.to_f
                @tps = (@tick.to_f / (t1 - t0)).round
                if @verbose == 1
                    print "\rTick: #{@tick} @ #{@tps} tps"
                end

                bot_with_initiative = ((@tick + (@swap_bots ? 1 : 0)) % @bots.size)

                # STEP 3: QUERY BOTS: send data, get response, move but don't collect

                @bots.each.with_index do |bot, i|
                    bot_position = bot[:position]

                    data = {}
                    if @tick == 0
                        data[:config] = {}
                        %w(width height generator max_ticks vis_radius max_gems
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

                    new_reported_tiles = Set.new()
                    this_round = (@visibility[(bot_position[1] << 16) | bot_position[0]] - @tiles_reported)
                    this_round.each do |t|
                        key = @maze.include?(t) ? :wall : :floor
                        data[key] << [t & 0xFFFF, t >> 16]
                        new_reported_tiles << t
                        @gems.each do |gem|
                            if gem[:position_offset] == t
                                data[:visible_gems] << {:position => gem[:position], :ttl => gem[:ttl]}
                            end
                        end
                    end
                    @tiles_reported |= new_reported_tiles
                    if @emit_signals
                        level_sum = 0.0
                        @gems.each.with_index do |gem, i|
                            level_sum += signal_level[i][(bot_position[1] << 16) | bot_position[0]] || 0.0
                        end
                        data[:signal_level] = format("%.6f", level_sum).to_f
                    end

                    @bots_io[i].stdin.puts(data.to_json)
                    @protocol.last[:bots] ||= {}
                    @protocol.last[:bots][:data] = data
                    line = @bots_io[i].stdout.gets.strip
                    @protocol.last[:bots][:response] = line
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
                        k = (_k + bot_with_initiative) % @bots.size
                        bot = @bots[k]
                        if bot[:position] == gem[:position]
                            collected_gems << i
                            bot[:score] += gem[:ttl]
                            first_capture ||= @tick
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
                    spawned_ttl += add_gem()
                end
                STDIN.raw do |stdin|
                    if IO.select([STDIN], nil, nil, 0)
                        key = stdin.getc
                        if key == "q"
                            # @tick = 0
                            exit
                        end
                    end
                end
            end
            @bots_io.each do |b|
                Process.kill("TERM", b.wait_thr.pid)
            end
        ensure
            print "\033[?25h" if @verbose >= 2
            STDIN.echo = true
        end
        if @verbose == 1
            puts
        end
        if @rounds == 1
            puts "Seed: #{@seed.to_s(36)} / Score: #{@bots.map { |x| x[:score]}.join(' / ')}"
        else
            print "\rFinished round #{@round + 1} of #{@rounds}..."
        end
        result = {}
        result[:score] = @bots.first[:score]
        if @profile
            result[:tile_coverage] = ((@tiles_revealed & @floor_tiles_set).size.to_f / @floor_tiles_set.size.to_f * 100.0 * 100).to_i.to_f / 100
            result[:ticks_to_first_capture] = first_capture
            if spawned_ttl > 0
                result[:gem_utilization] = (@bots.first[:score].to_f / spawned_ttl * 100.0 * 100).to_i.to_f / 100
            end
        end
        # STDERR.puts @protocol.to_yaml
        result
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
    rounds: 1,
    emit_signals: false,
    profile: false,
}

unless ARGV.include?('--stage')
    ARGV.unshift('--stage', 'current')
end

GENERATORS = %w(arena divided eller icey cellular uniform digger rogue)
OptionParser.new do |opts|
    opts.banner = "Usage: ./runner.rb [options]"

    opts.on('--stage STAGE', stages.keys,
        "Stage (default: #{options[:stage]})") do |x|
        options[:stage] = x
        options[:stage] = stages['current'] if options[:stage] == 'current'
        stage = stages[options[:stage]]
        stage.each_pair do |_key, value|
            key = _key.to_sym
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
    opts.on("-tTICKS", "--ticks TICKS", Integer, "Number of ticks (default: #{options[:ticks]})") do |x|
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
    opts.on("-vVERBOSE", "--verbose N", Integer, "Verbosity level (default: #{options[:verbose]})") do |x|
        options[:verbose] = x
    end
    opts.on("--max-tps N", Integer, "Max ticks/second (0 to disable, default: #{options[:max_tps]})") do |x|
        options[:max_tps] = x
    end
    opts.on("-c", "--[no-]cache", "Enable caching of pre-computed visibility (default: #{options[:cache]})") do |x|
        options[:cache] = x
    end
    opts.on("-rN", "--rounds N", Integer, "Rounds (default: #{options[:rounds]})") do |x|
        options[:rounds] = x
    end
    opts.on("-p", "--[no-]profile", "Report KPIs (default: #{options[:profile]})") do |x|
        options[:profile] = x
        if x
            options[:rounds] = 20
            options[:verbose] = 0
        end
    end
end.parse!

bot_paths = ARGV.map do |x|
    File.expand_path(x)
end

if bot_paths.empty?
    bot_paths << "random-walker/ruby"
end

if options[:rounds] == 1
    runner = Runner.new(**options)
    runner.setup
    bot_paths.each { |path| runner.add_bot(path) }
    runner.run
else
    round_seed = Digest::SHA256.digest("#{options[:seed]}/rounds").unpack1('L<')
    seed_rng = PCG32.new(round_seed)
    all_bu = []
    all_ttfc = []
    all_tc = []
    options[:rounds].times do |i|
        options[:seed] = seed_rng.randrange(2 ** 32)
        runner = Runner.new(**options)
        runner.round = i
        runner.setup
        bot_paths.each { |path| runner.add_bot(path) }
        result = runner.run
        gem_utilization = result[:gem_utilization]
        ticks_to_first_capture = result[:ticks_to_first_capture]
        tile_coverage = result[:tile_coverage]
        all_bu << gem_utilization if gem_utilization
        all_ttfc << ticks_to_first_capture if ticks_to_first_capture
        all_tc << tile_coverage
        # STDERR.puts result.to_json
    end
    puts
    n     = all_bu.size
    mean  = all_bu.sum(0.0) / n
    var   = all_bu.map { |x| (x - mean)**2 }.sum / n
    sd    = Math.sqrt(var)
    cv    = sd / mean * 100.0
    puts sprintf("Gem Utilization      : %5.1f %%", mean)
    puts sprintf("Relative Instability : %5.1f %%", cv)
    puts sprintf("Time to First Capture: %5.1f ticks", median(all_ttfc))
    puts sprintf("Capture Rate         : %5.1f %%", all_bu.size.to_f * 100 / options[:rounds])
    puts sprintf("Floor Tile Coverage  : %5.1f %%", mean(all_tc))
end
