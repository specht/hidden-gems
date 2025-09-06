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
    def visible(grid, ox, oy, radius:)
        raise ArgumentError, "need an opaque? block" unless block_given?

        h = grid.length
        w = grid[0].length
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
    BEACON_COLOR = '#238acc'
    FLOOR_COLOR = '#222728'
    WALL_COLOR = '#555753'

    Bot = Struct.new(:stdin, :stdout, :stderr, :wait_thr)

    attr_accessor :round

    def initialize(width:, height:, generator:, seed:, vis_radius:,
                   beacon_spawn:, beacon_radius:, beacon_quantization:,
                   beacon_noise:,beacon_cutoff:, beacon_ttl:, beacon_fade:,
                   max_beacons:, max_ticks:, swap_bots:, verbose:, max_tps:,
                   cache:, rounds:, beacon_icon:, emit_signals:, profile:
                   )
        @width = width
        @height = height
        @generator = generator
        @seed = seed
        @vis_radius = vis_radius
        @beacon_spawn = beacon_spawn
        @beacon_radius = beacon_radius
        @beacon_quantization = beacon_quantization
        @beacon_noise = beacon_noise
        @beacon_cutoff = beacon_cutoff
        @beacon_ttl = beacon_ttl
        @beacon_fade = beacon_fade
        @max_beacons = max_beacons
        @max_ticks = max_ticks
        @swap_bots = swap_bots
        @verbose = verbose
        @max_tps = max_tps
        @cache = cache
        @rounds = rounds
        @beacon_icon = beacon_icon
        @emit_signals = @emit_signals
        @profile = profile
        @bots = []
        @bots_io = []
        @beacons = []
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
        end.map do |line|
            row = line.split('').map { |x| x == '#' }
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

    def render(beacon_level)
        StringIO.open do |io|
            terminal_height, terminal_width = $stdout.winsize
            tile_width = 3

            io.print "\033[H" if @verbose >= 2

            status_line = sprintf("  Seed: #{@seed.to_s(36)}  │  Tick: %#{(@max_ticks - 1).to_s.size}d  │  %d tps  │  Score: #{@bots[0][:score]}", @tick, @tps)
            status_line = status_line + ' ' * (terminal_width - status_line.size)

            io.puts Paint[status_line, UI_FOREGROUND, UI_BACKGROUND]

            paint_rng = PCG32.new(1234)

            bots_visible = @bots.map do |bot|
                pos = bot[:position]
                @visibility["#{pos[0]}/#{pos[1]}"] || []
            end

            @maze.each.with_index do |row, y|
                row.each.with_index do |cell, x|
                    c = ' ' * tile_width
                    bg = FLOOR_COLOR
                    fg = WALL_COLOR
                    if cell
                        c = '█' * tile_width
                        fg = mix_rgb_hex(WALL_COLOR, '#000000', paint_rng.next_float() * 0.25)
                    end
                    @bots.each.with_index do |bot, i|
                        p = bot[:position]
                        if p[0] == x && p[1] == y
                            c = '🤔'
                            while Unicode::DisplayWidth.of(c) < tile_width
                                c += ' '
                            end
                        end
                    end
                    @beacons.each.with_index do |p, i|
                        if p[:position][0] == x && p[:position][1] == y
                            c = @beacon_icon
                            while Unicode::DisplayWidth.of(c) < tile_width
                                c += ' '
                            end
                            fg = BEACON_COLOR
                        end
                        if beacon_level[i].include?("#{x}/#{y}")
                            bg = mix_rgb_hex(BEACON_COLOR, bg, 1.0 - beacon_level[i]["#{x}/#{y}"])
                        end
                    end
                    unless @tiles_revealed.include?("#{x}/#{y}")
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
        @maze = gen_maze
        @floor_tiles = []
        @checksum = Digest::SHA256.hexdigest(@maze.to_json)
        @maze.each.with_index do |row, y|
            row.each.with_index do |cell, x|
                unless cell
                    @floor_tiles << [x, y]
                end
            end
        end
        @rng.shuffle!(@floor_tiles)
        @floor_tiles_set = Set.new(@floor_tiles)
        @spawn_points = []
        @spawn_points << @floor_tiles.shift
        @spawn_points << @floor_tiles.shift
        if @swap_bots
            @spawn_points.reverse!
        end
        @message_queue = Queue.new

        visibility_path = "cache/#{@checksum}.json.gz"
        if @cache && File.exist?(visibility_path)
            Zlib::GzipReader.open(visibility_path) do |gz|
                @visibility = JSON.parse(gz.read)
            end
        else
            # pre-calculate visibility from each tile
            @visibility = {}
            (0...@height).each do |y|
                (0...@width).each do |x|
                    next if @maze[y][x]
                    visible = FOVAngle.visible(@maze, x, y, radius: @vis_radius) { |x, y| @maze[y][x] }
                    @visibility["#{x}/#{y}"] = visible.to_a.map { |x| "#{x[0]}/#{x[1]}" }
                end
            end
            if @cache
                FileUtils.mkpath(File.dirname(visibility_path))
                Zlib::GzipWriter.open(visibility_path) do |gz|
                    gz.write @visibility.to_json
                end
            end
        end

        @tiles_revealed = Set.new()
    end

    def add_bot(path)
        @bots << {:position => @spawn_points.shift, :score => 0}
        bot_index = @bots_io.size
        @bots_io << start_bot(path) do |line|
            @message_queue << {:bot => bot_index, :line => line}
            STDERR.puts "Bot says: #{line}"
        end
    end

    def add_beacon()
        floor_tiles = Set.new()
        @maze.each.with_index do |row, y|
            row.each.with_index do |cell, x|
                unless cell
                    floor_tiles << [x, y]
                end
            end
        end
        # don't spawn beacon near bot
        @bots.each do |bot|
            floor_tiles.delete([bot[:position][0], bot[:position][1]])
        end
        # don't spawn beacon on another beacon
        @beacons.each do |beacon|
            floor_tiles.delete([beacon[:position][0], beacon[:position][1]])
        end
        return 0 if floor_tiles.empty?
        beacon = {:position => @rng.sample(floor_tiles.to_a), :ttl => @beacon_ttl}

        # pre-calculate beacon level
        level = {}
        wavefront = Set.new()
        wavefront << [beacon[:position][0], beacon[:position][1]]
        level["#{beacon[:position][0]}/#{beacon[:position][1]}"] = 1.0
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
                        if level["#{dx}/#{dy}"].nil? && !@maze[dy][dx]
                            l = Math.exp(-distance / @beacon_radius)
                            if @beacon_quantization > 0
                                l = ((l * @beacon_quantization).to_i).to_f / @beacon_quantization
                            end
                            l = 0.0 if l < @beacon_cutoff
                            level["#{dx}/#{dy}"] = l
                            new_wavefront << [dx, dy]
                        end
                    end
                end
            end
            wavefront = new_wavefront
            distance += 1
        end
        beacon[:level] = level

        @beacons << beacon
        return @beacon_ttl
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
                # STEP 5: Advance tick
                @tick += 1
                break if @tick >= @max_ticks

                @protocol << { }
                @protocol.last[:tick] = @tick
                @protocol.last[:rng_state] = @rng.snapshot
                # @protocol.last[:beacons] = @beacons
                tf0 = Time.now.to_f

                # STEP 1: Calculate beacon levels at each tile
                beacon_level = @beacons.map do |beacon|
                    temp = if @beacon_noise > 0.0
                        beacon[:level].transform_values do |l|
                            l += (@rng.next_float() - 0.5) * 2.0 * @beacon_noise
                            l = 0.0 if l < 0.0
                            l = 1.0 if l > 1.0
                            l
                        end
                    else
                        beacon[:level]
                    end
                    if @beacon_fade > 0
                        t = 1.0
                        beacon_age = @beacon_ttl - beacon[:ttl]
                        if beacon_age < @beacon_fade
                            t = (beacon_age + 1).to_f / @beacon_fade
                        elsif beacon_age >= @beacon_ttl - @beacon_fade
                            t = (@beacon_ttl - beacon_age).to_f / @beacon_fade
                        end
                        t = 0.0 if t < 0.0
                        t = 1.0 if t > 1.0
                        if t < 1.0
                            temp = temp.transform_values { |x| x * t }
                        end
                    end
                    temp
                end

                bot_position = @bots[0][:position]
                (@visibility["#{bot_position[0]}/#{bot_position[1]}"] || []).each do |t|
                    @tiles_revealed << t
                end

                # STEP 2: RENDER
                screen = render(beacon_level) if @verbose >= 2
                @protocol.last[:screen] = screen
                print screen
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
                        %w(width height generator max_ticks vis_radius max_beacons
                        beacon_spawn beacon_ttl beacon_radius beacon_cutoff beacon_noise
                        beacon_quantization beacon_fade).each do |key|
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
                    data[:visible_beacons] = []
                    (@visibility["#{bot_position[0]}/#{bot_position[1]}"] || []).each do |t|
                        _ = t.split('/').map { |x| x.to_i }
                        tx = _[0]
                        ty = _[1]
                        key = @maze[ty][tx] ? :wall : :floor
                        data[key] << _
                        @beacons.each do |beacon|
                            if beacon[:position] == _
                                data[:visible_beacons] << {:position => _, :ttl => beacon[:ttl]}
                            end
                        end
                    end
                    level_sum = 0.0
                    @beacons.each.with_index do |beacon, i|
                        level_sum += beacon_level[i]["#{bot_position[0]}/#{bot_position[1]}"] || 0.0
                    end
                    # level_sum = 1.0 if level_sum > 1.0
                    data[:beacon_level] = format("%.6f", level_sum).to_f
                    # STDERR.puts data.to_json
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
                            unless (@maze[dy][dx])
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

                # STEP 4: COLLECT BEACONS & DECAY BEACONS
                collected_beacons = []
                @beacons.each.with_index do |beacon, i|
                    (0...@bots.size).each do |_k|
                        k = (_k + bot_with_initiative) % @bots.size
                        bot = @bots[k]
                        if bot[:position] == beacon[:position]
                            collected_beacons << i
                            bot[:score] += beacon[:ttl]
                            first_capture ||= @tick
                        end
                    end
                end
                collected_beacons.reverse.each do |i|
                    @beacons.delete_at(i)
                end
                @beacons.each.with_index do |beacon, i|
                    beacon[:ttl] -= 1
                end
                @beacons.reject! do |beacon|
                    beacon[:ttl] <= 0
                end

                if @rng.next_float() < @beacon_spawn && @beacons.size < @max_beacons
                    spawned_ttl += add_beacon()
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
            temp = @floor_tiles_set.map { |x| x.map { |y| y.to_s }.join('/') }
            result[:tile_coverage] = ((@tiles_revealed & temp).size.to_f / temp.size.to_f * 100.0 * 100).to_i.to_f / 100
            result[:ticks_to_first_capture] = first_capture
            if spawned_ttl > 0
                result[:beacon_utilization] = (@bots.first[:score].to_f / spawned_ttl * 100.0 * 100).to_i.to_f / 100
            end
        end
        # STDERR.puts @protocol.to_yaml
        result
    end
end

stages = YAML.load(File.read('stages.yaml'))
options = {
    stage: nil,
    width: 19,
    height: 19,
    generator: 'cellular',
    seed: rand(2 ** 32),
    max_ticks: 1000,
    vis_radius: 5,
    max_beacons: 1,
    beacon_spawn: 0.05,
    beacon_ttl: 300,
    beacon_radius: 10.0,
    beacon_cutoff: 0.0,
    beacon_noise: 0.0,
    beacon_quantization: 0,
    beacon_fade: 0,
    swap_bots: false,
    verbose: 2,
    max_tps: 15,
    cache: false,
    rounds: 1,
    emit_signals: true,
    beacon_icon: "💎",
    profile: false,
}
GENERATORS = %w(arena divided eller icey cellular uniform digger rogue)
OptionParser.new do |opts|
    opts.banner = "Usage: ./runner.rb [options]"

    opts.on('-sSTAGE', '--stage STAGE', stages.keys.reject { |x| x == 'current' },
        "Stage (default: none)") do |x|
        options[:stage] = x
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
    opts.on("-sSEED", "--seed SEED", String, "Seed (default: random)") do |x|
        options[:seed] = x.to_i(36)
    end
    opts.on("-tTICKS", "--ticks TICKS", Integer, "Number of ticks (default: #{options[:ticks]})") do |x|
        options[:max_ticks] = x
    end
    opts.on("--vis-radius RADIUS", Integer, "Visibility radius (default: #{options[:vis_radius]})") do |x|
        options[:vis_radius] = x
    end
    opts.on("--max-beacons BEACONS", Integer, "Max. number of beacons (default: #{options[:max_beacons]})") do |x|
        options[:max_beacons] = x
    end
    opts.on("--beacon-spawn N", Float, "Beacon spawn probability (default: #{options[:beacon_spawn]})") do |x|
        options[:beacon_spawn] = x
    end
    opts.on("--beacon-ttl TTL", Integer, "Beacon TTL (default: #{options[:beacon_ttl]})") do |x|
        options[:beacon_ttl] = x
    end
    opts.on("--beacon-radius N", Float, "Beacon radius (default: #{options[:beacon_radius]})") do |x|
        options[:beacon_radius] = x
    end
    opts.on("--beacon-cutoff N", Float, "Beacon cutoff (default: #{options[:beacon_cutoff]})") do |x|
        options[:beacon_cutoff] = x
    end
    opts.on("--beacon-noise N", Float, "Beacon noise (default: #{options[:beacon_noise]})") do |x|
        options[:beacon_noise] = x
    end
    opts.on("--beacon-quantization N", Integer, "Beacon quantization (default: #{options[:beacon_quantization]})") do |x|
        options[:beacon_quantization] = x
    end
    opts.on("--beacon-fade N", Integer, "Beacon fade (default: #{options[:beacon_fade]})") do |x|
        options[:beacon_fade] = x
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

# Apply stage if given
unless options[:stage].nil?
    stage = stages[options[:stage]]
    stage.each_pair do |_key, value|
        key = _key.to_sym
        if value.is_a? Integer
            options[key] = value
        elsif value.is_a? Float
            options[key] = value
        elsif value.is_a? String
            options[key] = value
        end
    end
end
options.delete(:stage)

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
        beacon_utilization = result[:beacon_utilization]
        ticks_to_first_capture = result[:ticks_to_first_capture]
        tile_coverage = result[:tile_coverage]
        all_bu << beacon_utilization if beacon_utilization
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
    puts sprintf("Beacon Utilization   : %5.1f %%", mean)
    puts sprintf("Relative Instability : %5.1f %%", cv)
    puts sprintf("Time to First Capture: %5.1f ticks", median(all_ttfc))
    puts sprintf("Capture Rate         : %5.1f %%", all_bu.size.to_f * 100 / options[:rounds])
    puts sprintf("Floor Tile Coverage  : %5.1f %%", mean(all_tc))
end
