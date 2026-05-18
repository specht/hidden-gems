#!/usr/bin/env ruby

$LOAD_PATH.unshift File.expand_path("include/unicode-emoji-4.0.4/lib", __dir__)
$LOAD_PATH.unshift File.expand_path("include/unicode-display_width-3.1.5/lib", __dir__)
$LOAD_PATH.unshift File.expand_path("include/paint-2.3.0/lib", __dir__)

$LOAD_PATH.unshift File.expand_path(__dir__)
require 'include/fov_angle'
require 'include/key_input'
require 'include/pcg32'
require 'include/timings'
if Gem.win_platform?
    require 'fiddle/import'
end

require 'date'
require 'digest'
require 'fileutils'
require 'io/console'
require 'json'
require 'open3'
require 'openssl'
require 'optparse'
require 'paint'
require 'set'
require 'shellwords'
require 'stringio'
require 'strscan'
require 'unicode/display_width'
require 'yaml'
require 'zlib'

SOFT_LIMIT            = 0.100
HARD_LIMIT            = 0.200
HARD_LIMIT_FIRST_TICK = 30.0
OVERTIME_BUDGET       = 1.5

OPTIONS_FOR_BOT = %w(stage_key width height generator max_ticks emit_signals
                     emit_signal_channels vis_radius max_gems gem_spawn_rate
                     gem_ttl swarm_gem_ttl max_antennas max_portals signal_radius
                     signal_cutoff signal_noise signal_quantization
                     signal_fade enable_debug timeout_scale
                     instances_per_team comm_bytes swarm_gems
                     swarm_gem_chance swarm_node_count swarm_required_nodes
                     swarm_node_distance swarm_score_two_nodes
                     swarm_score_three_nodes)

ANSI = /\e\[[0-9;:<>?]*[@-~]/

GAUGE = "⠀⡀⣀⣄⣤⣦⣶⣷⣿"
GAUGE_COLORS = ['#ea2830', '#80bc42', '#55beed', '#fad31c', '#f384ae', '#00a8a8', '#7b67ae']
GEM_CHANNEL_COOLDOWN = 1

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

def sanitize_utf8_values(obj, replacement: "�")
    case obj
    when String
        return obj if obj.encoding == Encoding::UTF_8 && obj.valid_encoding?
        obj.encode(Encoding::UTF_8, invalid: :replace, undef: :replace, replace: replacement)

    when Array
        obj.map { |v| sanitize_utf8_values(v, replacement: replacement) }

    when Hash
        obj.transform_values { |v| sanitize_utf8_values(v, replacement: replacement) }

    else
        obj
    end
end

class Runner

    UI_BACKGROUND_TOP = '#1d5479'
    UI_FOREGROUND_TOP = '#eeeeec'
    UI_BACKGROUND_BOTTOM = '#232626'
    UI_FOREGROUND_BOTTOM = '#d3d7cf'

    PORTAL_EMOJIS = %w(🔴 🟠 🟡 🟢 🔵 🟣 ⚪ ⚫)
    ANTENNA_EMOJI = '📡'
    GEM_EMOJI = '💎'
    SWARM_GEM_EMOJI = '🔮'
    SWARM_NODE_EMOJI = '🔸'
    ANNOUNCER_EMOJI = '🎙️'
    GEM_COLOR = '#238acc'
    SWARM_GEM_COLOR = '#d946ef'
    SWARM_NODE_COLOR = '#facc15'
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

    attr_accessor :round, :stage_title, :stage_key, :timeout_scale, :team_count
    attr_reader :bots, :rng, :interactive_mode

    def initialize(seed:, width:, height:, generator:, max_ticks:,
                   vis_radius:, gem_spawn_rate:, gem_ttl:, max_gems:,
                   max_antennas:, max_portals:, emit_signals:,
                   emit_signal_channels:, signal_radius:,
                   signal_quantization:, signal_noise:,
                   signal_cutoff:, signal_fade:, swap_bots:, cache:,
                   profile:, check_determinism:, use_docker:,
                   docker_workdirs:, rounds:, round_seeds:, verbose:,
                   max_tps:, announcer_enabled:, bot_chatter:,
                   ansi_log_path:, write_highlights:, write_stdin:,
                   show_timings:, start_paused:, contest_mode:,
                   highlight_color:, enable_debug:, timeout_scale:,
                   instances_per_team:, comm_bytes:,
                   swarm_gems: false, swarm_gem_chance: 0.0,
                   swarm_gem_ttl: nil,
                   swarm_node_count: 3, swarm_required_nodes: 2,
                   swarm_node_distance: 5,
                   swarm_score_two_nodes: 3.0,
                   swarm_score_three_nodes: 5.0
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
        @max_antennas = max_antennas
        @max_portals = max_portals
        @emit_signals = emit_signals
        @emit_signal_channels = emit_signal_channels
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
        @bot_id_for_offset = {}
        @gems = []
        @gems_spawned = 0
        @gem_id_for_offset = {}
        @channel_blocked_until = []
        @chatlog = []
        @stage_title = '(no stage)'
        @stage_key = '(no stage)'
        @announcer_enabled = announcer_enabled
        @bot_chatter = bot_chatter
        @ansi_log_path = ansi_log_path
        @write_highlights = write_highlights
        @write_stdin = write_stdin
        @ansi_log = []
        @show_timings = show_timings
        @paused = false
        @pause_requested = false
        @start_paused = start_paused
        @contest_mode = contest_mode
        @highlight_color = highlight_color
        @enable_debug = enable_debug
        @timeout_scale = timeout_scale
        @team_count = 1
        @instances_per_team = [instances_per_team.to_i, 1].max
        @comm_bytes = [comm_bytes.to_i, 0].max
        @swarm_gems = !!swarm_gems
        @swarm_gem_chance = swarm_gem_chance
        @swarm_gem_ttl = swarm_gem_ttl
        @swarm_node_count = swarm_node_count
        @swarm_required_nodes = swarm_required_nodes
        @swarm_node_distance = swarm_node_distance
        @swarm_score_two_nodes = swarm_score_two_nodes
        @swarm_score_three_nodes = swarm_score_three_nodes
        @swarm_node_by_offset = {}
        @bot_index_by_team_slot = {}
        @faded_highlight_color = mix_rgb_hex(@highlight_color, '#000000', 0.25)
        @demo_mode = @ansi_log_path && File.basename(@ansi_log_path).include?('demo')
        @interactive_mode = false

        param_rng = PCG32.new(@seed)
        [:width, :height, :max_ticks, :vis_radius, :gem_ttl, :swarm_gem_ttl,
         :max_gems, :max_antennas, :max_portals, :signal_radius, :signal_fade,
         :rounds, :swarm_node_count, :swarm_required_nodes,
         :swarm_node_distance].each do |_key|
            key = "@#{_key}".to_sym
            value = instance_variable_get(key)
            if value.is_a?(String) && value.include?('..')
                parts = value.split('..').map { |x| x.strip.to_i }
                new_value = parts[0] + param_rng.randrange(parts[1] - parts[0] + 1)
                instance_variable_set(key, new_value)
            end
        end
        [:gem_spawn_rate, :signal_noise, :swarm_gem_chance,
         :swarm_score_two_nodes, :swarm_score_three_nodes].each do |_key|
            key = "@#{_key}".to_sym
            value = instance_variable_get(key)
            if value.is_a?(String) && value.include?('..')
                parts = value.split('..').map { |x| x.strip.to_f }
                instance_variable_set(key, parts[0] + param_rng.next_float * (parts[1] - parts[0]))
            end
        end

        @swarm_gem_ttl = @gem_ttl if @swarm_gem_ttl.nil?
        @swarm_gem_ttl = [@swarm_gem_ttl.to_i, 1].max

        @swarm_gem_chance = [[@swarm_gem_chance.to_f, 0.0].max, 1.0].min
        @swarm_node_count = [@swarm_node_count.to_i, 0].max
        @swarm_required_nodes = [@swarm_required_nodes.to_i, 0].max
        @swarm_required_nodes = [@swarm_required_nodes, @swarm_node_count].min
        @swarm_node_distance = [@swarm_node_distance.to_i, 1].max
        @swarm_score_two_nodes = [@swarm_score_two_nodes.to_f, 1.0].max
        @swarm_score_three_nodes = [@swarm_score_three_nodes.to_f, @swarm_score_two_nodes].max
    end

    def options_hash
        OPTIONS_FOR_BOT.each_with_object({}) do |key, h|
            h[key.to_sym] = instance_variable_get("@#{key}")
        end
    end

    def gen_maze

        maze_script = File.expand_path("include/maze.js", __dir__)

        run_generator = lambda do |width, height, generator, seed|
            command = [
                "node",
                Shellwords.escape(maze_script),
                "--width", width.to_s,
                "--height", height.to_s,
                "--generator", Shellwords.escape(generator.to_s),
                "--seed", Shellwords.escape(seed.to_s),
                "--wall", Shellwords.escape("#"),
                "--floor", Shellwords.escape(".")
            ].join(" ")

            `#{command}`.strip.split("\n").map(&:strip).select do |line|
                line =~ /^[\.#]+$/
            end
        end

        if @generator == 'mixed'
            inner_segment_width = 0.15

            raise "Mixed maze needs width >= 12 and height >= 7" if @width < 12 || @height < 7

            inner_width = @width - 2
            inner_height = @height - 2

            door_count = @mixed_door_count || 4
            door_count = [[door_count.to_i, 1].max, inner_height].min

            # We reserve two vertical separator columns:
            #
            #   [left cellular] [separator] [eller] [separator] [right cellular]
            #
            usable_width = inner_width - 2

            # Wider cellular sides, smaller Eller center.
            w1 = (usable_width * (0.5 - inner_segment_width * 0.5)).round
            w3 = (usable_width * (0.5 - inner_segment_width * 0.5)).round
            w2 = usable_width - w1 - w3

            # Defensive fallback for very small maps.
            if w2 < 3
                w2 = 3
                remaining = usable_width - w2
                w1 = remaining / 2
                w3 = remaining - w1
            end

            widths = [w1, w2, w3]

            parts = [
                run_generator.call(widths[0] + 2, inner_height + 2, 'cellular', "#{@seed}-mixed-cellular-a"),
                run_generator.call(widths[1] + 2, inner_height + 2, 'eller',    "#{@seed}-mixed-eller"),
                run_generator.call(widths[2] + 2, inner_height + 2, 'cellular', "#{@seed}-mixed-cellular-b")
            ]

            # Remove temporary borders.
            left_part = parts[0][1...-1].map do |line|
                line[1...-1].chars
            end

            eller_part = parts[1][1...-1].map do |line|
                line[1...-1].chars
            end

            right_part = parts[2][1...-1].map do |line|
                line[1...-1].chars
            end

            # Pick roughly evenly spaced doorway rows.
            door_rows = door_count.times.map do |i|
                (((i + 1) * (inner_height + 1)) / (door_count + 1).to_f).round - 1
            end

            door_rows = door_rows.map do |y|
                [[y, 0].max, inner_height - 1].min
            end.uniq

            # Defensive fill-up if rounding caused duplicates.
            y = 0
            while door_rows.size < door_count && y < inner_height
                door_rows << y unless door_rows.include?(y)
                y += 1
            end

            door_rows = door_rows.sort

            # Helper: carve from a given cell to the nearest existing floor tile
            # inside one submaze part.
            #
            # part is an array of char arrays.
            #
            # This is intentionally local to the submaze. It prevents separator openings
            # from ending immediately in a cellular wall blob.
            carve_to_nearest_floor = lambda do |part, start_x, start_y|
                h = part.size
                w = part[0].size

                return if start_x < 0 || start_y < 0 || start_x >= w || start_y >= h

                # If this cell is already floor, there is nothing to repair.
                return if part[start_y][start_x] == '.'

                seen = Array.new(h) { Array.new(w, false) }
                parent = Array.new(h) { Array.new(w) }

                queue = [[start_x, start_y]]
                seen[start_y][start_x] = true
                target = nil

                until queue.empty?
                    x, y = queue.shift

                    if part[y][x] == '.'
                        target = [x, y]
                        break
                    end

                    [[1, 0], [-1, 0], [0, 1], [0, -1]].each do |dx, dy|
                        nx = x + dx
                        ny = y + dy

                        next if nx < 0 || ny < 0 || nx >= w || ny >= h
                        next if seen[ny][nx]

                        seen[ny][nx] = true
                        parent[ny][nx] = [x, y]
                        queue << [nx, ny]
                    end
                end

                # If there was no floor at all, just carve the starting tile.
                unless target
                    part[start_y][start_x] = '.'
                    return
                end

                # Carve from the found floor back to the requested doorway cell.
                x, y = target

                while [x, y] != [start_x, start_y]
                    part[y][x] = '.'
                    x, y = parent[y][x]
                end

                part[start_y][start_x] = '.'
            end

            # Make doorway rows real on both sides.
            #
            # Left separator:
            #   left_part right edge <-> separator <-> eller_part left edge
            #
            # Right separator:
            #   eller_part right edge <-> separator <-> right_part left edge
            door_rows.each do |y|
                carve_to_nearest_floor.call(left_part,  w1 - 1, y)
                carve_to_nearest_floor.call(eller_part, 0,      y)

                carve_to_nearest_floor.call(eller_part, w2 - 1, y)
                carve_to_nearest_floor.call(right_part, 0,      y)
            end

            # Stitch the parts together with two separator walls.
            inner = inner_height.times.map do |y|
                left_sep = door_rows.include?(y) ? '.' : '#'
                right_sep = door_rows.include?(y) ? '.' : '#'

                left_part[y].join + left_sep + eller_part[y].join + right_sep + right_part[y].join
            end

            # Convert to mutable boolean grid:
            #
            #   true  = floor
            #   false = wall
            #
            grid = inner.map do |line|
                line.chars.map { |c| c == '.' }
            end

            inside_bounds = lambda do |x, y|
                x >= 0 && y >= 0 && x < inner_width && y < inner_height
            end

            flood_fill = lambda do |start_x, start_y|
                visited = Array.new(inner_height) { Array.new(inner_width, false) }
                queue = [[start_x, start_y]]
                visited[start_y][start_x] = true

                until queue.empty?
                    x, y = queue.shift

                    [[1, 0], [-1, 0], [0, 1], [0, -1]].each do |dx, dy|
                        nx = x + dx
                        ny = y + dy

                        next unless inside_bounds.call(nx, ny)
                        next if visited[ny][nx]
                        next unless grid[ny][nx]

                        visited[ny][nx] = true
                        queue << [nx, ny]
                    end
                end

                visited
            end

            find_first_floor = lambda do
                inner_height.times do |y|
                    inner_width.times do |x|
                        return [x, y] if grid[y][x]
                    end
                end

                nil
            end

            find_unvisited_floor = lambda do |visited|
                inner_height.times do |y|
                    inner_width.times do |x|
                        return [x, y] if grid[y][x] && !visited[y][x]
                    end
                end

                nil
            end

            carve_path_to_visited = lambda do |start_x, start_y, visited|
                seen = Array.new(inner_height) { Array.new(inner_width, false) }
                parent = Array.new(inner_height) { Array.new(inner_width) }

                queue = [[start_x, start_y]]
                seen[start_y][start_x] = true
                target = nil

                until queue.empty?
                    x, y = queue.shift

                    if visited[y][x]
                        target = [x, y]
                        break
                    end

                    [[1, 0], [-1, 0], [0, 1], [0, -1]].each do |dx, dy|
                        nx = x + dx
                        ny = y + dy

                        next unless inside_bounds.call(nx, ny)
                        next if seen[ny][nx]

                        seen[ny][nx] = true
                        parent[ny][nx] = [x, y]
                        queue << [nx, ny]
                    end
                end

                return unless target

                x, y = target

                while [x, y] != [start_x, start_y]
                    grid[y][x] = true
                    x, y = parent[y][x]
                end

                grid[start_y][start_x] = true
            end

            start = find_first_floor.call

            unless start
                grid[0][0] = true
                start = [0, 0]
            end

            # Make sure all floor areas are globally connected.
            loop do
                visited = flood_fill.call(start[0], start[1])
                unvisited = find_unvisited_floor.call(visited)

                break unless unvisited

                carve_path_to_visited.call(unvisited[0], unvisited[1], visited)
            end

        # Extra anti-lock-in pass:
        #
        # For an arena with several bots, tiny one-tile pockets and narrow dead ends
        # can be annoying. This pass can open some constrained floor tiles a little.
        #
        # Tuning:
        #
        #   @mixed_anti_lock_in_passes
        #       Number of passes over the map.
        #       0 disables this feature.
        #
        #   @mixed_anti_lock_in_chance
        #       Chance per detected dead end / near-dead-end to carve one neighboring wall.
        #       0.0 disables carving.
        #       1.0 carves every detected candidate.
        #
        anti_lock_in_passes = @mixed_anti_lock_in_passes || 1
        anti_lock_in_chance = @mixed_anti_lock_in_chance || 0.35

        anti_lock_in_passes = [anti_lock_in_passes.to_i, 0].max
        anti_lock_in_chance = [[anti_lock_in_chance.to_f, 0.0].max, 1.0].min

        anti_lock_in_passes.times do
            changes = []

            (1...(inner_height - 1)).each do |y|
                (1...(inner_width - 1)).each do |x|
                    next unless grid[y][x]

                    open_neighbors = 0

                    [[1, 0], [-1, 0], [0, 1], [0, -1]].each do |dx, dy|
                        open_neighbors += 1 if grid[y + dy][x + dx]
                    end

                    # Only target true dead ends.
                    # If this is still too much, keep this at <= 0.
                    # If you want slightly more opening, use <= 1.
                    next unless open_neighbors <= 1

                    next if rand >= anti_lock_in_chance

                    candidates = []

                    [[1, 0], [-1, 0], [0, 1], [0, -1]].each do |dx, dy|
                        nx = x + dx
                        ny = y + dy

                        next unless inside_bounds.call(nx, ny)
                        next if grid[ny][nx]

                        candidates << [nx, ny]
                    end

                    changes << candidates.sample if candidates.any?
                end
            end

            changes.compact.each do |x, y|
                grid[y][x] = true
            end
        end

            # Re-run connectivity after anti-lock-in carving.
            start = find_first_floor.call

            loop do
                visited = flood_fill.call(start[0], start[1])
                unvisited = find_unvisited_floor.call(visited)

                break unless unvisited

                carve_path_to_visited.call(unvisited[0], unvisited[1], visited)
            end

            # Add one final outer border.
            maze_lines = []
            maze_lines << "#" * @width

            grid.each do |row|
                maze_lines << "#" + row.map { |floor| floor ? "." : "#" }.join + "#"
            end

            maze_lines << "#" * @width
        else
            maze_lines = run_generator.call(@width, @height, @generator, @seed)
        end

        walls = maze_lines.map.with_index do |line, y|
            line.split('').map.with_index do |e, x|
                e == '#' ? (y << 16) | x : nil
            end
        end.flatten.reject(&:nil?)

        Set.new(walls)
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
            spawn_count = @team_count * @instances_per_team
            if @floor_tiles.size < spawn_count
                raise "Not enough floor tiles for #{spawn_count} bot spawn points"
            end
            @spawn_points = @floor_tiles.shift(spawn_count).map do |offset|
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
                    @gem_fel << {:tick => t, :offset => position_offset}
                end
            end
            # we're waiting to spawn the gem at @gem_fel_index
            @gem_fel_index = 0

            @channel_blocked_until = []
            (0...@max_gems).each do |c|
                @channel_blocked_until[c] = 0
            end

            begin
                @terminal_height, @terminal_width = $stdout.winsize
            rescue
                @terminal_height = 24
                @terminal_width = 80
            end

            @tile_width = 2

            if @ansi_log_path
                stage_index = @stage_key.split('-')[1].to_i
                @terminal_width = stage_index < 2 ? 100 : 150
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

    def read_interactive_command(prompt = "Your move> ")
        buffer = +""

        print prompt
        STDOUT.flush

        loop do
            ch = STDIN.getch

            case ch
            when "\r", "\n"
                puts
                cmd = buffer.strip
                return cmd.empty? ? "WAIT" : cmd.upcase

            when "\u0003" # Ctrl-C
                raise Interrupt

            when "\u007F", "\b" # Backspace on Unix/Windows
                unless buffer.empty?
                    buffer.chop!
                    print "\b \b"
                    STDOUT.flush
                end

            when "\e" # ANSI escape sequence: arrows on Linux/macOS, sometimes Windows terminals too
                begin
                    ch2 = STDIN.read_nonblock(1)
                    if ch2 == "["
                        ch3 = STDIN.read_nonblock(1)
                        case ch3
                        when "A" then puts; return "N"
                        when "B" then puts; return "S"
                        when "C" then puts; return "E"
                        when "D" then puts; return "W"
                        end
                    end
                rescue IO::WaitReadable, EOFError
                end

            when "\u0000", "\u00E0" # Windows extended keys
                begin
                    ch2 = STDIN.getch
                    case ch2
                    when "H" then puts; return "N" # up
                    when "P" then puts; return "S" # down
                    when "M" then puts; return "E" # right
                    when "K" then puts; return "W" # left
                    end
                rescue EOFError
                end

            else
                # Allow regular typed commands like WAIT, PAN, P1N, json, etc.
                if ch >= " " && ch != "\e"
                    buffer << ch
                    print ch
                    STDOUT.flush
                end
            end
        end
    end

    def start_interactive_bot(bot_index)
        bot_stdin_r, bot_stdin_w = IO.pipe
        bot_stdout_r, bot_stdout_w = IO.pipe

        thread = Thread.new do
            loop do
                line = bot_stdin_r.gets
                break if line.nil?

                begin
                    parsed = JSON.parse(line)
                    compact = parsed.to_json
                rescue
                    compact = line
                end

                puts
                puts "=== INTERACTIVE BOT #{bot_index} INPUT ==="
                puts compact
                puts "=== END INPUT ==="
                puts "Attention: Don't hold keys pressed, don't press multiple keys at once. You may use arrow keys to navigate."

                response = STDIN.raw { read_interactive_command("Your move> ") }
                break if response.nil?

                bot_stdout_w.puts(response.strip.upcase)
                bot_stdout_w.flush
            end
        end

        stdin  = bot_stdin_w
        stdout = bot_stdout_r
        stderr = StringIO.new
        wait_thr = Struct.new(:pid).new(thread.object_id)

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

    def team_bot_indices(team)
        (0...@bots.size).select { |i| @bots[i][:team] == team }
    end

    def team_bots(team)
        team_bot_indices(team).map { |i| @bots[i] }
    end

    def team_representative(team)
        team_bots(team).first || { :name => "Team #{team + 1}", :emoji => '🤖' }
    end

    def team_label(team, with_name: false)
        bot = team_representative(team)
        if with_name
            "#{bot[:name]} #{bot[:emoji]}"
        else
            bot[:emoji]
        end
    end

    def team_score(team)
        team_bots(team).sum { |bot| bot[:disqualified_for] ? 0 : bot[:score] }
    end

    def score_summary
        if @instances_per_team > 1
            (0...@team_count).map { |team| "#{team_label(team)} #{team_score(team)}" }.join(' : ')
        else
            @bots.map { |bot| "#{bot[:emoji]} #{bot[:disqualified_for] ? 0 : bot[:score]}" }.join(' : ')
        end
    end

    def team_disqualified_for(team)
        reasons = team_bots(team).map { |bot| bot[:disqualified_for] }.compact.uniq
        reasons.empty? ? nil : reasons.join(',')
    end

    def team_stderr_log(team)
        lines = []
        team_bot_indices(team).each do |i|
            bot = @bots[i]
            slot = bot[:slot] || 0
            bot[:stderr_log].each do |line|
                lines << "[slot #{slot}] #{line}"
            end
        end
        lines
    end

    def team_tile_coverage(team)
        seen = Set.new
        team_bot_indices(team).each do |i|
            seen.merge(@tiles_revealed[i] || Set.new)
        end
        return 0.0 if @floor_tiles_set.empty?
        ((seen & @floor_tiles_set).size.to_f / @floor_tiles_set.size.to_f * 100.0 * 100).to_i.to_f / 100
    end

    def response_time_stats_for_bots(bots)
        response_times = bots.flat_map { |bot| bot[:response_times] || [] }
        first_response_time = response_times.size > 0 ? (response_times.first * 1e9).to_i : nil
        remaining_response_times = response_times.map { |x| (x * 1e9).to_i }
        remaining_response_times.shift if remaining_response_times.size > 0

        {
            :first => first_response_time,
            :min => (remaining_response_times.size > 0 ? remaining_response_times.min : nil),
            :median => (remaining_response_times.size > 0 ? remaining_response_times.sort[remaining_response_times.size / 2] : nil),
            :max => (remaining_response_times.size > 0 ? remaining_response_times.max : nil),
        }
    end

    def team_protocol_checksum(team)
        protocols = team_bot_indices(team).map do |i|
            {
                :slot => @bots[i][:slot],
                :protocol => @protocol[i]
            }
        end
        Digest::SHA256.hexdigest(protocols.to_json)
    end

    def build_team_results(ttl_spawned, ticks_to_first_capture_by_team)
        (0...@team_count).map do |team|
            bots = team_bots(team)
            score = team_score(team)
            result = {
                :score => score,
                :ticks_to_first_capture => ticks_to_first_capture_by_team[team],
                :disqualified_for => team_disqualified_for(team),
                :response_time_stats => response_time_stats_for_bots(bots),
                :stderr_log => team_stderr_log(team),
            }
            if @profile
                result[:gem_utilization] = (ttl_spawned > 0 ? (score.to_f / ttl_spawned.to_f * 100.0 * 100).to_i.to_f / 100 : 0.0)
                result[:tile_coverage] = team_tile_coverage(team)
            end
            if @check_determinism
                result[:protocol_checksum] = team_protocol_checksum(team)
            end
            result
        end
    end

    def team_order_for_tick(tick)
        teams = (0...@team_count).to_a
        return teams if teams.empty?
        teams.rotate(tick % teams.size)
    end

    def bot_batches_for_tick(tick)
        if @instances_per_team > 1
            team_order = team_order_for_tick(tick)
            (0...@instances_per_team).map do |slot|
                team_order.map { |team| @bot_index_by_team_slot[[team, slot]] }.compact
            end
        else
            return [[]] if @bots.empty?
            [(0...@bots.size).to_a.rotate(tick % @bots.size)]
        end
    end

    def parse_response_extra(json_text)
        return nil if json_text.nil? || json_text.empty?
        JSON.parse(json_text)
    rescue JSON::ParserError
        nil
    end

    def comm_byte(value)
        value.to_i.clamp(0, 255)
    rescue
        0
    end

    def apply_comm_write(bot_index, extra)
        return unless @comm_bytes > 0
        return unless extra.is_a?(Hash)

        team = @bots[bot_index][:team]
        buffer = @team_buffers[team]
        return unless buffer

        if extra['comm'].is_a?(Array)
            extra['comm'].first(@comm_bytes).each_with_index do |value, index|
                buffer[index] = comm_byte(value)
            end
        end

        if extra['comm_write'].is_a?(Hash)
            extra['comm_write'].each do |key, value|
                begin
                    index = Integer(key)
                rescue ArgumentError, TypeError
                    next
                end
                next if index < 0 || index >= @comm_bytes
                buffer[index] = comm_byte(value)
            end
        end
    end

    def gem_kind(gem)
        gem[:kind] || :regular
    end

    def swarm_gem?(gem)
        gem_kind(gem) == :swarm
    end

    def gem_emoji(gem)
        swarm_gem?(gem) ? SWARM_GEM_EMOJI : GEM_EMOJI
    end

    def gem_color(gem)
        swarm_gem?(gem) ? SWARM_GEM_COLOR : GEM_COLOR
    end

    def occupied_swarm_node_offsets(gem)
        occupied = Set.new
        return occupied unless swarm_gem?(gem)

        (gem[:nodes] || []).each do |node|
            node_position = node[:position]
            if @bots.any? { |bot| !bot[:disqualified_for] && bot[:position] == node_position }
                occupied << node[:offset]
            end
        end

        occupied
    end

    def occupied_swarm_node_count(gem)
        occupied_swarm_node_offsets(gem).size
    end

    def swarm_score_multiplier(occupied_nodes)
        return nil if occupied_nodes < @swarm_required_nodes
        return @swarm_score_three_nodes.to_f if occupied_nodes >= @swarm_node_count
        @swarm_score_two_nodes.to_f
    end

    def gem_score(gem, occupied_nodes = nil)
        unless swarm_gem?(gem)
            return gem[:ttl]
        end

        occupied_nodes = occupied_swarm_node_count(gem) if occupied_nodes.nil?
        multiplier = swarm_score_multiplier(occupied_nodes)
        return nil if multiplier.nil?

        (gem[:ttl].to_f * multiplier).round
    end

    def max_gem_score(gem)
        if swarm_gem?(gem)
            (gem[:ttl].to_f * @swarm_score_three_nodes.to_f).round
        else
            gem[:ttl]
        end
    end

    def rebuild_gem_offsets
        @gem_id_for_offset.clear
        @swarm_node_by_offset.clear

        @gems.each_with_index do |gem, gem_index|
            @gem_id_for_offset[gem[:position_offset]] = gem_index

            next unless swarm_gem?(gem)

            (gem[:nodes] || []).each_with_index do |node, node_index|
                @swarm_node_by_offset[node[:offset]] = [gem_index, node_index]
            end
        end
    end

    def occupied_offsets_for_spawn(placed_antennas)
        occupied_points = Set.new

        @gems.each do |g|
            occupied_points << g[:position_offset]
            if swarm_gem?(g)
                (g[:nodes] || []).each do |node|
                    occupied_points << node[:offset]
                end
            end
        end

        @bots.each do |b|
            occupied_points << ((b[:position][1] << 16) | b[:position][0])
        end

        placed_antennas.each do |a|
            a.each do |offset|
                occupied_points << offset
            end
        end

        occupied_points
    end

    def place_swarm_nodes_for_gem(gem, occupied_points)
        return [] unless @swarm_gems
        return [] if @swarm_node_count <= 0

        gx = gem[:position][0]
        gy = gem[:position][1]
        start_offset = gem[:position_offset]
        target_distance = [@swarm_node_distance.to_i, 1].max
        min_node_distance = [[(target_distance / 2.0).ceil, 2].max, target_distance].min

        seen = Set.new
        queue = [[gx, gy, 0]]
        seen << start_offset
        candidates = []

        until queue.empty?
            x, y, dist = queue.shift
            offset = (y << 16) | x

            if offset != start_offset && !occupied_points.include?(offset)
                candidates << {
                    :position => [x, y],
                    :offset => offset,
                    :distance => dist,
                    :distance_error => (dist - target_distance).abs
                }
            end

            [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |dx, dy|
                nx = x + dx
                ny = y + dy
                next if nx < 0 || ny < 0 || nx >= @width || ny >= @height

                noffset = (ny << 16) | nx
                next if seen.include?(noffset)
                next if @maze.include?(noffset)

                seen << noffset
                queue << [nx, ny, dist + 1]
            end
        end

        @rng.shuffle!(candidates)
        candidates.sort_by! { |c| [c[:distance_error], c[:distance]] }

        nodes = []
        candidates.each do |candidate|
            next if nodes.any? do |node|
                (node[:position][0] - candidate[:position][0]).abs +
                    (node[:position][1] - candidate[:position][1]).abs < min_node_distance
            end

            nodes << candidate
            break if nodes.size >= @swarm_node_count
        end

        if nodes.size < @swarm_node_count
            candidates.each do |candidate|
                next if nodes.any? { |node| node[:offset] == candidate[:offset] }

                nodes << candidate
                break if nodes.size >= @swarm_node_count
            end
        end

        return [] if nodes.size < @swarm_node_count

        nodes.each do |node|
            node.delete(:distance)
            node.delete(:distance_error)
        end

        nodes
    end

    def collect_gems_for_initiative_order(initiative_order, ticks_to_first_capture_by_team, signal_level)
        return if @tick >= @max_ticks
        return if @gems.empty?

        collected_gems = []
        @gems.each.with_index do |gem, i|
            collected_this_gem = false
            initiative_order.each do |k|
                bot = @bots[k]
                next if bot.nil? || bot[:disqualified_for]
                next unless bot[:position] == gem[:position]

                occupied_nodes = nil
                multiplier = nil
                points = nil

                if swarm_gem?(gem)
                    occupied_nodes = occupied_swarm_node_count(gem)
                    multiplier = swarm_score_multiplier(occupied_nodes)
                    next if multiplier.nil?
                    points = gem_score(gem, occupied_nodes)
                else
                    points = gem_score(gem)
                end

                collected_this_gem = true
                collected_gems << i
                bot[:score] += points
                ticks_to_first_capture_by_team[bot[:team]] ||= @tick

                scorer = @instances_per_team > 1 ? team_label(bot[:team], with_name: true) : "#{bot[:name]} #{bot[:emoji]}"
                if @announcer_enabled
                    if swarm_gem?(gem)
                        node_text = "#{occupied_nodes}/#{@swarm_node_count} resonance nodes"
                        if occupied_nodes >= @swarm_node_count
                            @chatlog << {emoji: ANNOUNCER_EMOJI, text: "Perfect resonance! #{scorer} scored a Swarm Gem with #{node_text}: #{points} points!" }
                        else
                            @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{scorer} scored a Swarm Gem with #{node_text}: #{points} points!" }
                        end
                    else
                        @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{scorer} scored a gem with #{points} points!" }
                    end
                end

                event = {
                    tick: @tick,
                    type: 'gem_collected',
                    gem_type: gem_kind(gem).to_s,
                    bot: k,
                    team: bot[:team],
                    delta: points,
                    new_score: bot[:score],
                    new_team_score: team_score(bot[:team]),
                    position: gem[:position],
                    id: gem[:id]
                }

                if swarm_gem?(gem)
                    event[:occupied_nodes] = occupied_nodes
                    event[:required_nodes] = @swarm_required_nodes
                    event[:node_count] = @swarm_node_count
                    event[:multiplier] = multiplier
                    event[:nodes] = (gem[:nodes] || []).map { |node| node[:position] }
                end

                if @team_count == 2
                    other_team = 1 - bot[:team]
                    other_bot = team_bots(other_team).find { |b| b[:disqualified_for].nil? }
                    event[:other_bot] = other_bot[:position] if other_bot
                end
                @events << event

                break
            end
        end

        collected_gems.reverse.each do |i|
            if @emit_signal_channels
                @channel_blocked_until[@gems[i][:channel]] = @tick + GEM_CHANNEL_COOLDOWN
            end
            @gems.delete_at(i)
            signal_level.delete_at(i) if signal_level
        end
        rebuild_gem_offsets
    end

    def decay_gems
        return if @tick >= @max_ticks

        @gems.each do |gem|
            gem[:ttl] -= 1
            if gem[:ttl] <= 0
                event = { tick: @tick, type: 'gem_expired', gem_type: gem_kind(gem).to_s, position: gem[:position], id: gem[:id] }
                if swarm_gem?(gem)
                    event[:nodes] = (gem[:nodes] || []).map { |node| node[:position] }
                    event[:required_nodes] = @swarm_required_nodes
                    event[:node_count] = @swarm_node_count
                end
                @events << event
            end
        end

        @gems.each do |gem|
            if gem[:ttl] <= 0
                if @emit_signal_channels
                    @channel_blocked_until[gem[:channel]] = @tick + GEM_CHANNEL_COOLDOWN
                end
            end
        end

        @gems.reject! do |gem|
            gem[:ttl] <= 0
        end
        rebuild_gem_offsets
    end

    def announce_competitors
        @chatlog << {emoji: ANNOUNCER_EMOJI, text: "Welcome to Hidden Gems!" }
        @chatlog << {emoji: ANNOUNCER_EMOJI, text: "Today's stage is #{@stage_title} (v#{@stage_key.split('@').last})" }
        @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{@generator.capitalize} @ #{@width}x#{@height} with seed #{@seed.to_s(36)}" }

        comments = COMMENT_SINGLE.dup
        @rng.shuffle!(comments)

        if @instances_per_team > 1
            if @team_count == 1
                @chatlog << {emoji: ANNOUNCER_EMOJI, text: "All eyes on our lone squad:" }
            elsif @team_count == 2
                @chatlog << {emoji: ANNOUNCER_EMOJI, text: "Two squads enter the maze:" }
            else
                @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{@team_count} squads enter the maze:" }
            end

            (0...@team_count).each do |team|
                @chatlog << {
                    emoji: ANNOUNCER_EMOJI,
                    text: "#{team_label(team, with_name: true)} deploys #{@instances_per_team} bot instances -- #{comments.shift}"
                }
            end
        else
            if @bots.size == 1
                @chatlog << {emoji: ANNOUNCER_EMOJI, text: "All eyes on our lone contestant:" }
            else
                @chatlog << {emoji: ANNOUNCER_EMOJI, text: "Two bots enter the maze:" }
            end

            @bots.each do |bot|
                @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{bot[:name]} #{bot[:emoji]} -- #{comments.shift}" }
            end
        end

        if @team_count > 1
            @chatlog << {emoji: ANNOUNCER_EMOJI, text: @rng.sample(COMMENT_VERSUS) }
        end
    end

    def render(tick, signal_level, paused, placed_antennas, placed_portals)
        fg_top_mix = mix_rgb_hex(UI_FOREGROUND_TOP, UI_BACKGROUND_TOP, 0.5)
        fg_bottom_mix = mix_rgb_hex(UI_FOREGROUND_BOTTOM, UI_BACKGROUND_BOTTOM, 0.5)
        StringIO.open do |io|
            io.print "\033[H" if @verbose >= 2

            score_s = score_summary

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

            bot_highlights = []

            portal_override = {}
            placed_portals.each.with_index do |portals_for_bot, bot_index|
                portals_for_bot.each.with_index do |portal_pair, portal_index|
                    portal_pair.each do |portal|
                        portal_override[portal[0]] = PORTAL_EMOJIS[(bot_index * 4 + portal_index) % PORTAL_EMOJIS.size]
                    end
                end
            end

            swarm_node_override = {}
            @gems.each_with_index do |gem, gem_index|
                next unless swarm_gem?(gem)

                occupied_nodes = occupied_swarm_node_offsets(gem)
                (gem[:nodes] || []).each_with_index do |node, node_index|
                    swarm_node_override[node[:offset]] = {
                        gem_index: gem_index,
                        node_index: node_index,
                        occupied: occupied_nodes.include?(node[:offset])
                    }
                end
            end

            if @enable_debug
                @bots.each.with_index do |x, i|
                    bot_highlights[i] = {}
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
                                if x >= 0 && x < @width && y >= 0 && y < @height
                                    offset = (y << 16) | x
                                    color = _[2]
                                    bot_highlights[i][offset] ||= []
                                    bot_highlights[i][offset] << color
                                end
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
                        have_antenna = placed_antennas.any? { |x| x.include?(offset) }
                        if @maze.include?(offset) && !have_antenna
                            unless @wall_color_cache.include?(offset)
                                @wall_color_cache[offset] = mix_rgb_hex(WALL_COLOR, '#000000', paint_rng.next_float() * 0.25)
                            end
                            bg = @wall_color_cache[offset]
                        end
                        node_info = swarm_node_override[offset]
                        # if node_info && !(@maze.include?(offset) && !have_antenna)
                        #     node_mix = node_info[:occupied] ? 0.35 : 0.45
                        #     bg = mix_rgb_hex(SWARM_NODE_COLOR, bg, node_mix)
                        # end
                        $timings.profile("find bots") do
                            if @bot_id_for_offset.include?(offset)
                                bot_index = @bot_id_for_offset[offset]
                                bot = @bots[bot_index]
                                next if bot[:disqualified_for]
                                c = bot[:emoji]
                                while vwidth(c) < @tile_width
                                    c += ' '
                                end
                            end
                        end

                        $timings.profile("find gems") do
                            if @gem_id_for_offset.include?(offset)
                                gem_index = @gem_id_for_offset[offset]
                                gem = @gems[gem_index]
                                c = gem_emoji(gem)
                                while vwidth(c) < @tile_width
                                    c += ' '
                                end
                            elsif node_info && !@bot_id_for_offset.include?(offset)
                                c = SWARM_NODE_EMOJI
                                while vwidth(c) < @tile_width
                                    c += ' '
                                end
                            end
                            if @emit_signals
                                @gems.each.with_index do |p, i|
                                    if signal_level[i].include?((y << 16) | x)
                                        unless @maze.include?((y << 16) | x) && !have_antenna
                                            level = signal_level[i][(y << 16) | x]
                                            # clamp signal level for rendering
                                            level = 0.0 if level < 0.0
                                            level = 1.0 if level > 1.0
                                            bg = mix_rgb_hex(gem_color(p), bg, 1.0 - level)
                                        end
                                    end
                                end
                            end
                        end
                        if have_antenna
                            c = ANTENNA_EMOJI
                            while vwidth(c) < @tile_width
                                c += ' '
                            end
                        end
                        if portal_override.include?(offset)
                            c = portal_override[offset]
                            while vwidth(c) < @tile_width
                                c += ' '
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
                        old_bg = bg
                        (0...@bots.size).each do |_k|
                            if @ansi_log_path && @write_highlights
                                bg = old_bg
                            end
                            if (bot_highlights[_k] || {}).include?(offset)
                                bot_highlights[_k][offset].each do |color|
                                    opacity = 32
                                    if color.size == 9
                                        opacity = (color[7..8].to_i(16) * 63) / 255
                                        color = color[0..6]
                                    end
                                    cache_key_3 = "#{bg}/#{color}/#{opacity}"
                                    @fog_of_war_cache[cache_key_3] ||= mix_rgb_hex(bg, color, opacity / 63.0)
                                    bg = @fog_of_war_cache[cache_key_3]
                                end
                                if @ansi_log_path && @write_highlights
                                    ((@protocol[_k][-2] || {})[:bots] || {})[:highlight] ||= []
                                    ((@protocol[_k][-2] || {})[:bots] || {})[:highlight] << [x, y, bg]
                                end
                            end
                        end
                        if @ansi_log_path && @write_highlights
                            bg = old_bg
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
                status_line_parts = [
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
                ]
                bot_line = @bots.map.with_index do |x, i|
                    parts = []
                    if (i > 0)
                        parts << Paint[' : ', fg_bottom_mix, UI_BACKGROUND_BOTTOM]
                    end
                    parts << Paint["#{x[:emoji]}", UI_FOREGROUND_BOTTOM, UI_BACKGROUND_BOTTOM]
                    if @emit_signal_channels
                        (0...@max_gems).each.with_index do |c, ci|
                            signal_level = ((((@protocol[i][-2] || {})[:bots] || {})[:data] || {})[:channels] || [])[c] || 0.0
                            signal_level = 0.0 if signal_level < 0.0
                            signal_level = 1.0 if signal_level > 1.0
                            signal_level = signal_level ** 0.5
                            gauge_character = GAUGE[[(signal_level * (GAUGE.size - 1)).round, GAUGE.size - 1].min]
                            parts << Paint[" #{gauge_character}", GAUGE_COLORS[ci % GAUGE_COLORS.size], UI_BACKGROUND_BOTTOM]
                        end
                    end
                    parts << Paint[" #{((@protocol[i][-2] || {})[:bots] || {})[:response]}", UI_FOREGROUND_BOTTOM, UI_BACKGROUND_BOTTOM]
                    parts
                end
                status_line_parts << bot_line
                status_line = status_line_parts.map { |x| x.join('') }.join(Paint['  |  ', fg_bottom_mix, UI_BACKGROUND_BOTTOM])
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

    def add_bot(path, bot_args = [], team: @bots.size, slot: 0)
        @bots << {
            :team => team,
            :slot => slot,
            :position => @spawn_points.shift,
            :score => 0,
            :name => "Botty McBotface",
            :emoji => '🤖',
            :overtime_used => 0.0,
            :disqualified_for => nil,
            :response_times => [],
            :stderr_log => []
        }
        @bot_id_for_offset[@bots.last[:position][1] << 16 | @bots.last[:position][0]] = @bots.size - 1

        bot_index = @bots_io.size
        @bot_index_by_team_slot[[team, slot]] = bot_index

        if File.basename(path) == "interactive"
            @interactive_mode = true
            @bots.last[:name] = "Interactive"
            @bots.last[:emoji] = '🤖'

            # Disable timeouts entirely
            @timeout_scale = 1e9

            @bots_io << start_interactive_bot(bot_index)
            return
        end

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

        @bots_io << start_bot(path, @docker_workdirs[bot_index], bot_args) do |line|
            @message_queue << {:bot => bot_index, :line => line}
            @bots[bot_index][:stderr_log] << line
        end
    end

    def add_team(path, team_index, bot_args = [])
        if File.basename(path) == "interactive" && @instances_per_team > 1
            raise "Interactive mode only supports one instance per team"
        end

        (0...@instances_per_team).each do |slot|
            add_bot(path, bot_args, team: team_index, slot: slot)
        end
    end

    def spawn_gem(channel, placed_antennas)
        spawn_data = @gem_fel[@gem_fel_index]
        @gem_fel_index += 1

        gem = {:position_offset => spawn_data[:offset], :ttl => @gem_ttl, :initial_ttl => @gem_ttl, :kind => :regular}
        gem[:position] = [gem[:position_offset] & 0xFFFF, gem[:position_offset] >> 16]

        # Attention: if there happens to be a gem, a swarm node, a bot, or an
        # antenna already at this position, let's find another position nearby.
        occupied_points = occupied_offsets_for_spawn(placed_antennas)

        dist_field = {}
        wavefront = Set.new()
        wavefront << [gem[:position][0], gem[:position][1]]
        dist_field[(gem[:position][1] << 16) | gem[:position][0]] = 0
        distance = 0
        good = false
        $timings.profile("spawn gem: find free tile") do
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
        end

        if @swarm_gems && @swarm_node_count > 0 && @swarm_required_nodes > 0 && @rng.next_float < @swarm_gem_chance
            node_occupied_points = occupied_points.dup
            node_occupied_points << gem[:position_offset]
            nodes = []
            $timings.profile("spawn gem: place swarm nodes") do
                nodes = place_swarm_nodes_for_gem(gem, node_occupied_points)
            end
            if nodes.size == @swarm_node_count
                gem[:kind] = :swarm
                gem[:ttl] = @swarm_gem_ttl
                gem[:initial_ttl] = @swarm_gem_ttl
                gem[:nodes] = nodes
            end
        end

        $timings.profile("spawn gem: precalculate signal level") do
            # pre-calculate gem level
            level = {}
            seen = {}
            wavefront = Set.new()
            gx = gem[:position][0]
            gy = gem[:position][1]
            wavefront << [gx, gy]
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
                            if !seen.include?(offset)
                                dist = Math.sqrt((dx - gx) * (dx - gx) + (dy - gy) * (dy - gy))
                                r = @signal_radius.to_f
                                l = 1.0 / (1.0 + (dist / r) * (dist / r))
                                if @signal_quantization > 0
                                    q = @signal_quantization.to_f
                                    l = ((l * q).floor).to_f / q
                                end
                                if l >= @signal_cutoff.to_f
                                    level[offset] = l
                                    seen[offset] = true
                                    new_wavefront << [dx, dy]
                                end
                            end
                        end
                    end
                end
                wavefront = new_wavefront
            end
            gem[:level] = level
        end

        @gems_spawned += 1
        gem[:id] = @gems_spawned - 1
        if @emit_signal_channels
            gem[:channel] = channel
            @channel_blocked_until[channel] = @tick + gem[:ttl] + GEM_CHANNEL_COOLDOWN
        end
        @gems << gem
        rebuild_gem_offsets
        max_gem_score(gem)
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

    def varint_size(n)
        size = 1
        while n >= 0x80
            n >>= 7
            size += 1
        end
        size
    end

    def write_varint(io, n)
        # unsigned LEB128
        while n >= 0x80
            io << ((n & 0x7f) | 0x80)
            n >>= 7
        end
        io << (n & 0x7f)
    end

    def sparse_payload(entries, width)
        # entries: [x, y, idx]
        linear = entries.map { |x, y, idx| [y * width + x, idx] }
        linear.sort_by!(&:first)

        out = [linear.length]
        prev_i = 0
        linear.each do |i, idx|
            out << (i - prev_i)
            out << idx
            prev_i = i
        end
        out
    end

    def sparse_bytes(payload)
        payload.sum { |n| varint_size(n) }
    end

    def rle_payload(entries, width, height)
        # Build a full index grid with 0=transparent.
        grid = Array.new(height) { Array.new(width, 0) }
        entries.each { |x, y, idx| grid[y][x] = idx }

        out = []
        grid.each do |row|
            prev = row[0]
            run_len = 1

            row[1..].each do |v|
                if v == prev
                    run_len += 1
                else
                    out << run_len
                    out << prev
                    prev = v
                    run_len = 1
                end
            end

            out << run_len
            out << prev
        end
        out
    end

    def rle_bytes(payload)
        payload.sum { |n| varint_size(n) }
    end

    def encode_overlay(entries, width, height)
        return nil if entries.nil? || entries.empty?

        sp = sparse_payload(entries, width)
        sp_bytes = sparse_bytes(sp)

        rl = rle_payload(entries, width, height)
        rl_bytes = rle_bytes(rl)

        if sp_bytes <= rl_bytes
            { type: :sparse, bytes: sp_bytes, data: sp }
        else
            { type: :rle, bytes: rl_bytes, data: rl }
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
        unless @interactive_mode
            begin
                STDIN.echo = false
            rescue
            end
        end
        ticks_to_first_capture_by_team = Array.new(@team_count)
        @tiles_revealed = @bots.map do |b|
            Set.new()
        end
        ttl_spawned = 0
        color_to_index = {'#00000000' => 0}
        index_to_color = ['#00000000']
        @team_buffers = Array.new(@team_count) { Array.new(@comm_bytes, 0) }

        antenna_stock = @bots.map { |b| @max_antennas }
        placed_antennas = @bots.map { |b| [] }
        # placed_portals:
        # - for each bot:
        #  - for each portal index:
        #    - list with 0, 1 or 2 entries of:
        #      - offset of placed portal
        #      - offset the portal was placed from
        placed_portals = @bots.map { |b| (0...@max_portals).map { |i| [] } }

        @events = []
        @events << {
            :type => 'match_start',
            :teams => (0...@team_count).map do |team|
                bot = team_representative(team)
                { name: bot[:name], emoji: bot[:emoji], score: 0 }
            end,
            :bots => @bots.map { |b| { name: b[:name], emoji: b[:emoji], team: b[:team], slot: b[:slot], position: b[:position] } }
        }
        @protocol = @bots.map { |b| [] }
        announce_competitors if @announcer_enabled
        frames = []
        @paused = @start_paused
        break_on_tick = nil
        begin
            print "\033[?25l" if @verbose >= 2 && !@interactive_mode
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
                            # fade first
                            temp = gem[:level]
                            if @signal_fade > 0
                                t = 1.0
                                initial_ttl = gem[:initial_ttl] || (swarm_gem?(gem) ? @swarm_gem_ttl : @gem_ttl)
                                gem_age = initial_ttl - gem[:ttl]
                                if gem_age < @signal_fade
                                    t = (gem_age + 1).to_f / @signal_fade
                                elsif gem_age >= initial_ttl - @signal_fade
                                    t = (initial_ttl - gem_age).to_f / @signal_fade
                                end
                                t = 0.0 if t < 0.0
                                t = 1.0 if t > 1.0
                                if t < 1.0
                                    temp = temp.transform_values { |x| x * t }
                                end
                            end
                            # add noise after fading
                            if @signal_noise > 0.0
                                temp = temp.transform_values do |l|
                                    if @stage_key < 'stage-3@0.2'
                                        l += (@rng.next_float() - 0.5) * 2.0 * @signal_noise
                                    else
                                        l += @rng.rand_normal(0, @signal_noise * 0.577350)
                                    end
                                    # no clamping of raw noisy signals, we'll only clamp for rendering later
                                    # l = 0.0 if l < 0.0
                                    # l = 1.0 if l > 1.0
                                    l
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
                            if @team_count == 1
                                @chatlog << {emoji: ANNOUNCER_EMOJI, text: "Mission complete after #{@max_ticks} ticks." }
                            else
                                team_scores = (0...@team_count).map { |team| team_score(team) }
                                if team_scores.uniq.size == 1
                                    @chatlog << {emoji: ANNOUNCER_EMOJI, text: "Duel ends in a draw after #{@max_ticks} ticks." }
                                else
                                    @chatlog << {emoji: ANNOUNCER_EMOJI, text: "Duel ends after #{@max_ticks} ticks (one squad stands tall, one blames RNG)." }
                                end
                            end
                        end
                    end

                    # STEP 2: RENDER
                    if @verbose >= 2 || @ansi_log_path
                        screen = nil
                        $timings.profile("render screen") do
                            screen = render(running_tick, signal_level, @paused, placed_antennas, placed_portals)
                        end
                        # @protocol.last[:screen] = screen
                        if @verbose >= 2
                            if @interactive_mode
                                print "\033[2J\033[H"
                            end
                            print screen
                        end
                        frames << screen
                        if @ansi_log_path
                            log_entry = {:screen => screen}
                            if @write_highlights
                                log_entry[:highlight] = []
                                @bots.each.with_index do |bot, i|
                                    stream = ((@protocol[i][-2] || {})[:bots] || {})[:highlight] || []
                                    highlight_indices = []
                                    begin
                                        highlight_indices = stream.map do |entry|
                                            x = entry[0]
                                            y = entry[1]
                                            color = entry[2].downcase
                                            unless index_to_color.include?(color)
                                                color_to_index[color] = index_to_color.size
                                                index_to_color << color
                                            end
                                            index = color_to_index[color]
                                            [x, y, index]
                                        end
                                    end
                                    log_entry[:highlight] << encode_overlay(highlight_indices, @width, @height)
                                end
                            end
                            @ansi_log << log_entry
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
                    if @pause_requested && !@paused
                        @paused = true
                        @pause_requested = false
                    end

                    # --- STEP 3: QUERY BOTS ---
                    #
                    # Normal stages keep the old model: all bots receive the same
                    # start-of-tick snapshot and responses are resolved by initiative.
                    #
                    # Multi-instance team stages are split into slot batches:
                    #   A0/B0, then A1/B1, then A2/B2, ...
                    # Bots in one batch receive the same world snapshot. After the
                    # batch is resolved, later slots see the updated world and the
                    # updated team communication buffer.

                    next if @tick == @max_ticks
                    @stdout_buffers ||= Array.new(@bots.size) { "" }

                    bot_batches = bot_batches_for_tick(@tick)
                    bot_batches.each do |batch_order|
                        batch_order = batch_order.select { |i| !i.nil? && i >= 0 && i < @bots.size }
                        next if batch_order.empty?

                        # 3a) PREPARE DATA for this initiative batch
                        prepared = {}
                        batch_order.each do |i|
                            bot = @bots[i]
                            next if bot[:disqualified_for]

                            data = {}
                            $timings.profile("prepare data") do
                                if @tick == 0
                                    data[:config] = {}
                                    OPTIONS_FOR_BOT.each do |key|
                                        data[:config][key.to_sym] = instance_variable_get("@#{key}")
                                    end
                                    dk = OpenSSL::PKCS5.pbkdf2_hmac("#{@seed}/bot/#{i}", "scrim:v1:#{@seed}", 200_000, 4, "sha256")
                                    bot_seed = dk.unpack1("L<")
                                    data[:config][:bot_seed] = bot_seed
                                    # data[:config][:team] = bot[:team]
                                    data[:config][:instance] = bot[:slot]
                                    data[:config][:team_count] = @team_count
                                    data[:config][:instances_per_team] = @instances_per_team
                                    data[:config][:comm_bytes] = @comm_bytes
                                end
                                data[:tick] = @tick
                                data[:team] = bot[:team]
                                data[:slot] = bot[:slot]
                                data[:team_size] = @instances_per_team
                                data[:phase] = bot[:slot]
                                data[:team_buffer] = @team_buffers[bot[:team]].dup if @comm_bytes > 0
                                data[:bot] = bot[:position]
                                data[:wall] = []
                                data[:floor] = []
                                if @max_antennas > 0
                                    data[:antennas] = []
                                end
                                if @max_portals > 0
                                    data[:portals] = []
                                    data[:portal_stubs] = []
                                end

                                data[:initiative] = (batch_order.first == i)
                                data[:visible_gems] = []
                                data[:visible_swarm_nodes] = [] if @swarm_gems
                                vis_key = (bot[:position][1] << 16) | bot[:position][0]
                                @visibility[vis_key].each do |t|
                                    key = @maze.include?(t) ? :wall : :floor
                                    data[key] << [t & 0xFFFF, t >> 16]
                                    if @max_antennas > 0
                                        if placed_antennas.any? { |x| x.include?(t) }
                                            data[:antennas] << [t & 0xFFFF, t >> 16]
                                        end
                                    end
                                    if @max_portals > 0
                                        placed_portals.each.with_index do |portals_for_bot, bot_index|
                                            portals_for_bot.each.with_index do |portal_pair, portal_index|
                                                portal_pair.each do |portal|
                                                    if portal[0] == t
                                                        if portal_pair.size == 2
                                                            data[:portals] << [t & 0xFFFF, t >> 16]
                                                        else
                                                            data[:portal_stubs] << [t & 0xFFFF, t >> 16]
                                                        end
                                                    end
                                                end
                                            end
                                        end
                                    end
                                    @gems.each do |gem|
                                        if gem[:position_offset] == t
                                            entry = { :position => gem[:position], :ttl => gem[:ttl] }
                                            if swarm_gem?(gem)
                                                entry[:type] = 'swarm'
                                                entry[:nodes] = (gem[:nodes] || []).map { |node| node[:position] }
                                                entry[:required_nodes] = @swarm_required_nodes
                                                entry[:occupied_nodes] = occupied_swarm_node_count(gem)
                                                entry[:score_two_nodes] = @swarm_score_two_nodes
                                                entry[:score_three_nodes] = @swarm_score_three_nodes
                                            end
                                            data[:visible_gems] << entry
                                        end
                                    end

                                    if @swarm_gems && @swarm_node_by_offset.include?(t)
                                        gem_index, node_index = @swarm_node_by_offset[t]
                                        gem = @gems[gem_index]
                                        if gem
                                            node = (gem[:nodes] || [])[node_index]
                                            if node
                                                data[:visible_swarm_nodes] << {
                                                    :position => node[:position],
                                                    :gem_position => gem[:position],
                                                    :gem_id => gem[:id],
                                                    :occupied => @bots.any? { |b| !b[:disqualified_for] && b[:position] == node[:position] }
                                                }
                                            end
                                        end
                                    end
                                end
                                if @emit_signals
                                    level_sum = 0.0
                                    @gems.each_with_index do |gem, gi|
                                        level_sum += (signal_level[gi][vis_key] || 0.0)
                                    end
                                    data[:signal_level] = format("%.6f", level_sum).to_f
                                    if @emit_signal_channels
                                        data[:channels] = (0...@max_gems).map { |c| 0.0 }
                                        @gems.each.with_index do |g, gi|
                                            channel = g[:channel]
                                            level = signal_level[gi][vis_key] || 0.0
                                            data[:channels][channel] = format("%.6f", level).to_f
                                        end
                                    end
                                    if @max_antennas > 0
                                        data[:antenna_signals] = []
                                        placed_antennas[i].each do |a|
                                            ax = a & 0xFFFF
                                            ay = a >> 16
                                            signal = signal_level.each_with_index.map { |l, gi| l[a] || 0.0 }.sum
                                            data[:antenna_signals] << { :position => [ax, ay], :signal => format("%.6f", signal).to_f }
                                        end
                                    end
                                end
                                if @bots.size > 1
                                    data[:visible_bots] = []
                                    (0...@bots.size).each do |j|
                                        next if j == i
                                        other = @bots[j]
                                        next if other[:disqualified_for]
                                        bx, by = other[:position]
                                        if @visibility[vis_key].include?((by << 16) | bx)
                                            entry = {
                                                :position => other[:position],
                                                :emoji => other[:emoji],
                                                # :team => other[:team],
                                                :teammate => (other[:team] == bot[:team])
                                            }
                                            if entry[:teammate]
                                                entry[:instance] = other[:slot]
                                            end
                                            data[:visible_bots] << entry
                                        end
                                    end
                                end
                            end

                            prepared[i] = data
                            @protocol[i] ||= []
                            @protocol[i].last[:bots] ||= {}
                            @protocol[i].last[:bots][:data] = data
                            if @ansi_log_path && @write_stdin
                                @ansi_log.last[:stdin] = data
                            end
                        end

                        # puts prepared.to_json

                        # 3b) WRITE PHASE: send this batch to all eligible bots first
                        start_mono   = {}
                        deadline_mono = {}
                        received_mono = {}
                        write_limit = (@tick == 0 ? HARD_LIMIT_FIRST_TICK : HARD_LIMIT) * @timeout_scale
                        batch_order.each do |i|
                            next if @bots[i][:disqualified_for] || prepared[i].nil?
                            $timings.profile("write to bot's stdin") do
                                begin
                                    @bots_io[i].stdin.puts(prepared[i].to_json)
                                    @bots_io[i].stdin.flush
                                    # Start the response timer only after the input has actually been delivered.
                                    start_mono[i]    = Process.clock_gettime(Process::CLOCK_MONOTONIC)
                                    deadline_mono[i] = start_mono[i] + write_limit
                                rescue Errno::EPIPE
                                    if @bots[i][:disqualified_for].nil?
                                        @bots[i][:disqualified_for] = 'terminated_unexpectedly'
                                        @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{@bots[i][:name]} has terminated unexpectedly!" } if @announcer_enabled
                                        @events << { tick: @tick, type: 'terminated_unexpectedly', bot: i }
                                    end
                                end
                            end
                        end

                        # 3c) READ PHASE: multiplex this batch until we have one line per responding bot or they time out/EOF
                        pending = batch_order.select { |i| prepared[i] && @bots[i][:disqualified_for].nil? }
                        stdout_map = {}
                        read_ios = []
                        pending.each do |i|
                            io = @bots_io[i].stdout
                            read_ios << io
                            stdout_map[io] = i
                        end

                        responses = {} # i => line (String)
                        $timings.profile("read responses") do
                            loop do
                                break if read_ios.empty?

                                # compute shortest remaining deadline for select timeout
                                now_mono = Process.clock_gettime(Process::CLOCK_MONOTONIC)
                                min_remaining = pending.map { |i| [deadline_mono[i] - now_mono, 0.0].max }.min
                                # if any already timed out, skip waiting
                                min_remaining = 0.0 if pending.any? { |i| now_mono >= deadline_mono[i] }

                                ready, = IO.select(read_ios, nil, nil, min_remaining)
                                ready ||= []

                                # Always drain any ready stdout first; otherwise a bot can be falsely hard-timed-out
                                # simply because the runner was busy with other bots.
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
                                            @events << { tick: @tick, type: 'terminated_unexpectedly', bot: i }
                                        end
                                        read_ios.delete(io)
                                        pending.delete(i)
                                    else
                                        @stdout_buffers[i] << chunk
                                        # extract one line if present (keep remainder for next tick)
                                        if (nl = @stdout_buffers[i].index("\n"))
                                            line = @stdout_buffers[i].slice!(0..nl).strip
                                            responses[i] = line
                                            received_mono[i] = Process.clock_gettime(Process::CLOCK_MONOTONIC)
                                            read_ios.delete(io)
                                            pending.delete(i)
                                        end
                                    end
                                end

                                # Now mark hard timeouts for bots still pending without a full line.
                                now_mono = Process.clock_gettime(Process::CLOCK_MONOTONIC)
                                pending.dup.each do |i|
                                    if now_mono >= deadline_mono[i] && !responses.key?(i)
                                        if @bots[i][:disqualified_for].nil?
                                            @bots[i][:disqualified_for] = "hard_timeout"
                                            elapsed = now_mono - start_mono[i]
                                            if @announcer_enabled
                                                @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{@bots[i][:name]} took too long to respond (#{(elapsed * 1000).to_i} ms) and has been terminated!" }
                                                @events << { tick: @tick, type: 'hard_timeout', bot: i }
                                            end
                                        end
                                        io = @bots_io[i].stdout
                                        read_ios.delete(io)
                                        pending.delete(i)
                                    end
                                end
                            end
                        end

                        # 3d) PROCESS RESPONSES in initiative order for this batch
                        $timings.profile("process bot responses") do
                            batch_order.each do |i|
                                next if prepared[i].nil?
                                next if @bots[i][:disqualified_for]

                                line = responses[i]
                                if line.nil?
                                    # already disqualified above (timeout/EOF)
                                    next
                                end

                                # Use the moment we actually received this bot's newline-terminated response.
                                # Otherwise a slow opponent inflates everyone's "elapsed" (and overtime).
                                recv_t = received_mono[i] || Process.clock_gettime(Process::CLOCK_MONOTONIC)
                                elapsed = recv_t - start_mono[i]
                                @bots[i][:response_times] << elapsed
                                overtime = (@tick == 0 || @check_determinism) ? 0.0 : (elapsed - SOFT_LIMIT * @timeout_scale)
                                @bots[i][:overtime_used] += overtime if overtime > 0.0
                                if @announcer_enabled && overtime.to_f > 0.0
                                    @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{@bots[i][:name]} exceeded soft limit by #{(overtime * 1e3).to_i} ms (total overtime: #{(@bots[i][:overtime_used] * 1e3).to_i} ms)" }
                                    @events << { tick: @tick, type: 'overtime', bot: i, overtime: overtime.to_f, total_overtime: @bots[i][:overtime_used].to_f }
                                end
                                if @bots[i][:overtime_used] > OVERTIME_BUDGET * @timeout_scale
                                    if @bots[i][:disqualified_for].nil?
                                        @bots[i][:disqualified_for] = 'overtime_budget_exceeded'
                                        @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{@bots[i][:name]} has exceeded their overtime budget and is disqualified!" } if @announcer_enabled
                                        @events << { tick: @tick, type: 'overtime_budget_exceeded', bot: i }
                                    end
                                end

                                command = (line.split(' ').first || '').strip
                                debug_json = line[command.length..-1]&.strip
                                extra_data = parse_response_extra(debug_json)
                                @protocol[i].last[:bots][:response] = command
                                if (!@check_determinism) && (!@contest_mode)
                                    @protocol[i].last[:bots][:debug_json] = debug_json
                                    if @verbose >= 2
                                        begin
                                            debug_data = extra_data || JSON.parse(debug_json)
                                            if debug_data['command'] == 'pause'
                                                @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{@bots[i][:name]} requested a pause." }
                                                @pause_requested = true
                                            end
                                        rescue
                                        end
                                    end
                                end

                                bot_position = @bots[i][:position]
                                prev_bot_position = bot_position.dup
                                dir = {'N'=>[0,-1],'E'=>[1,0],'S'=>[0,1],'W'=>[-1,0]}
                                if command =~ /^[NESW]$/
                                    # move NESW
                                    dx = bot_position[0] + dir[command][0]
                                    dy = bot_position[1] + dir[command][1]
                                    if dx >= 0 && dy >= 0 && dx < @width && dy < @height
                                        if (!@maze.include?((dy << 16) | dx))
                                            target_occupied_by_bot = nil
                                            (0...@bots.size).each do |other|
                                                next if other == i
                                                next if @bots[other][:disqualified_for]
                                                if @bots[other][:position] == [dx, dy]
                                                    target_occupied_by_bot = other
                                                    break
                                                end
                                            end
                                            if target_occupied_by_bot.nil?
                                                old_offset = (bot_position[1] << 16) | bot_position[0]
                                                @bot_id_for_offset.delete(old_offset)
                                                @bots[i][:position] = [dx, dy]
                                                new_offset = (dy << 16) | dx
                                                @bot_id_for_offset[new_offset] = i
                                            end
                                        elsif @max_portals > 0
                                            # check for portal
                                            placed_portals.each_with_index do |portals_for_bot, bot_index|
                                                portals_for_bot.each_with_index do |portal_pair, portal_index|
                                                    portal_pair.each.with_index do |portal, portal_half_index|
                                                        if portal[0] == ((dy << 16) | dx)
                                                            # teleport to other half if exists, otherwise ignore move
                                                            if portal_pair.size == 2
                                                                other_half = portal_pair[1 - portal_half_index]
                                                                other_half_x = other_half[1] & 0xFFFF
                                                                other_half_y = other_half[1] >> 16
                                                                target_occupied_by_bot = nil
                                                                (0...@bots.size).each do |other|
                                                                    next if other == i
                                                                    next if @bots[other][:disqualified_for]
                                                                    if @bots[other][:position] == [other_half_x, other_half_y]
                                                                        target_occupied_by_bot = other
                                                                        break
                                                                    end
                                                                end
                                                                old_offset = (@bots[i][:position][1] << 16) | @bots[i][:position][0]
                                                                @bot_id_for_offset.delete(old_offset)
                                                                @bots[i][:position] = [other_half_x, other_half_y] if target_occupied_by_bot.nil?
                                                                new_offset = (other_half_y << 16) | other_half_x
                                                                @bot_id_for_offset[new_offset] = i
                                                            end
                                                        end
                                                    end
                                                end
                                            end
                                        end
                                    end
                                elsif @max_antennas > 0 && command =~ /^PA[NESW]$/
                                    # Place Antenna NESW
                                    dx = bot_position[0] + dir[command[2]][0]
                                    dy = bot_position[1] + dir[command[2]][1]
                                    if dx >= 0 && dy >= 0 && dx < @width && dy < @height
                                        offset = (dy << 16) | dx
                                        target_occupied_by_bot = nil
                                        (0...@bots.size).each do |other|
                                            next if other == i
                                            next if @bots[other][:disqualified_for]
                                            if @bots[other][:position] == [dx, dy]
                                                target_occupied_by_bot = other
                                                break
                                            end
                                        end
                                        target_occupied_by_gem = nil
                                        @gems.each_with_index do |gem, gi|
                                            if gem[:position] == [dx, dy]
                                                target_occupied_by_gem = gi
                                                break
                                            end
                                        end
                                        target_occupied_by_antenna = placed_antennas.any? { |x| x.include?(offset) }
                                        if @maze.include?(offset) && target_occupied_by_bot.nil? && target_occupied_by_gem.nil? && !target_occupied_by_antenna && antenna_stock[i] > 0
                                            antenna_stock[i] -= 1
                                            placed_antennas[i] << offset
                                            @maze << offset
                                            if @announcer_enabled
                                                @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{@bots[i][:name]} placed an antenna (#{antenna_stock[i]} antenna#{antenna_stock[i] == 1 ? '' : 's'} remaining)." }
                                                @events << { tick: @tick, type: 'antenna_placed', bot: i, position: [dx, dy], remaining_stock: antenna_stock[i] }
                                            end
                                        end
                                    end
                                elsif @max_portals > 0 && command =~ /^P[1-4][NESW]$/
                                    # Place Portal 1-4 NESW
                                    portal_id = command[1].to_i - 1
                                    if portal_id >= 0 && portal_id < @max_portals
                                        dx = bot_position[0] + dir[command[2]][0]
                                        dy = bot_position[1] + dir[command[2]][1]
                                        if dx >= 0 && dy >= 0 && dx < @width && dy < @height
                                            offset = (dy << 16) | dx
                                            # target must be on wall
                                            if @maze.include?(offset)
                                                # target must not be part of another portal (complete or incomplete)
                                                occupied = false
                                                placed_portals.each do |portals_for_bot|
                                                    portals_for_bot.each do |portal_pair|
                                                        portal_pair.each do |portal|
                                                            if portal.include?(offset)
                                                                occupied = true
                                                                break
                                                            end
                                                        end
                                                    end
                                                    break if occupied
                                                end
                                                unless occupied
                                                    # each portal can have at most 2 placements (entrance and exit)
                                                    if placed_portals[i][portal_id].size < 2
                                                        # prepare entry with portal offset and bot position at time of placement
                                                        entry = [offset, (bot_position[1] << 16) | bot_position[0]]
                                                        placed_portals[i][portal_id] << entry
                                                        if placed_portals[i][portal_id].size == 1
                                                            @events << { tick: @tick, type: 'portal_half_placed', bot: i, portal_id: portal_id, position: [dx, dy] }
                                                        else
                                                            @events << { tick: @tick, type: 'portal_completed', bot: i, portal_id: portal_id, position: [dx, dy] }
                                                        end
                                                        if @announcer_enabled
                                                            if placed_portals[i][portal_id].size == 1
                                                                @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{@bots[i][:name]} placed the first half of portal #{portal_id + 1}." }
                                                            else
                                                                @chatlog << {emoji: ANNOUNCER_EMOJI, text: "#{@bots[i][:name]} completed portal #{portal_id + 1}." }
                                                            end
                                                        end
                                                    end
                                                end

                                            end
                                        end
                                    end
                                elsif command == 'WAIT'
                                    # no-op
                                else
                                    # invalid command -> ignore
                                end

                                apply_comm_write(i, extra_data)
                                @events << { tick: @tick, type: 'bot_moved', bot: i, from: prev_bot_position, to: @bots[i][:position], command: command[0, 16] }
                            end
                        end

                        # Collect immediately after this batch has been resolved.
                        # Later slot batches therefore no longer see gems that were
                        # already collected by earlier slot pairs in the same tick.
                        collect_gems_for_initiative_order(batch_order, ticks_to_first_capture_by_team, signal_level)
                    end

                    # STEP 4: DECAY GEMS
                    decay_gems

                    if @gems.size < @max_gems
                        if @gem_fel_index < @gem_fel.size
                            if @tick >= @gem_fel[@gem_fel_index][:tick]
                                can_spawn_channels = (0...@max_gems).select do |channel|
                                    @tick >= @channel_blocked_until[channel]
                                end
                                unless can_spawn_channels.empty?
                                    ttl_spawned += spawn_gem(@rng.sample(can_spawn_channels), placed_antennas)
                                    spawned_gem = @gems.last
                                    spawn_event = {
                                        tick: @tick,
                                        type: 'gem_spawned',
                                        gem_type: gem_kind(spawned_gem).to_s,
                                        position: spawned_gem[:position],
                                        ttl: spawned_gem[:ttl],
                                        id: spawned_gem[:id]
                                    }
                                    if swarm_gem?(spawned_gem)
                                        spawn_event[:nodes] = (spawned_gem[:nodes] || []).map { |node| node[:position] }
                                        spawn_event[:required_nodes] = @swarm_required_nodes
                                        spawn_event[:node_count] = @swarm_node_count
                                        spawn_event[:score_two_nodes] = @swarm_score_two_nodes
                                        spawn_event[:score_three_nodes] = @swarm_score_three_nodes
                                        # if @announcer_enabled
                                        #     @chatlog << {emoji: ANNOUNCER_EMOJI, text: "A Swarm Gem appeared. #{@swarm_node_count} Resonance Nodes begin to glow." }
                                        # end
                                    end
                                    @events << spawn_event
                                end
                            end
                        end
                    end
                end
                if @verbose >= 2
                    print frames[@tick]
                end
                unless @paused
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
                        key = KeyInput.get_key(@paused)
                        if key == 'q' || key == 'esc'
                            exit
                        elsif key == 'left'
                            @tick = [@tick - 1, 0].max
                            @paused = true
                        elsif key == 'home'
                            @tick = 0
                            @paused = true
                        # elsif key == 'end'
                        #     @tick = @max_ticks - 1
                        #     @paused = true
                        elsif key == 'right'
                            @tick = [@tick + 1, @max_ticks].min
                            @paused = true
                        elsif key == ' '
                            @paused = !@paused
                        end
                    rescue
                    end
                end
            end
            @bots_io.each do |b|
                kill_bot_process(b)
            end
        ensure
            print "\033[?25h" if @verbose >= 2 && !@interactive_mode
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
                puts "Seed: #{@seed.to_s(36)} / Score: #{score_summary}"
            end
        else
            print "\rFinished round #{@round + 1} of #{@rounds}..."
        end
        results = build_team_results(ttl_spawned, ticks_to_first_capture_by_team)
        if @ansi_log_path
            path = @ansi_log_path.sub('.json.gz', "-#{@seed.to_s(36)}.json.gz")
            emoji_widths = {}
            @bots.each do |bot|
                emoji_widths[bot[:emoji]] = vwidth(bot[:emoji])
            end
            Zlib::GzipWriter.open(path) do |f|
                data = {:width => @terminal_width, :height => @terminal_height, :frames => @ansi_log, :emoji_widths => emoji_widths}
                if @write_highlights
                    data[:index_to_color] = index_to_color
                end
                f.write(data.to_json)
            end
            path = @ansi_log_path.sub('.json.gz', "-#{@seed.to_s(36)}-poster.json.gz")
            Zlib::GzipWriter.open(path) do |f|
                data = {:width => @terminal_width, :height => @terminal_height, :frames => [@ansi_log.first], :emoji_widths => emoji_widths}
                f.write(data.to_json)
            end
        end
        return results, @events
    end
end

stages = YAML.load_file(File.join(__dir__, 'stages.yaml'))

options = {
    stage: 'current',
    width: 19,
    height: 19,
    generator: 'arena',
    seed: rand(2 ** 48),
    max_ticks: 1000,
    vis_radius: 10,
    max_gems: 1,
    gem_spawn_rate: 0.05,
    gem_ttl: 300,
    max_antennas: 0,
    max_portals: 0,
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
    emit_signal_channels: false,
    profile: false,
    check_determinism: false,
    use_docker: false,
    docker_workdirs: [],
    rounds: 1,
    round_seeds: nil,
    announcer_enabled: true,
    bot_chatter: true,
    ansi_log_path: nil,
    write_highlights: false,
    write_stdin: false,
    show_timings: false,
    start_paused: false,
    contest_mode: false,
    highlight_color: '#ffffff',
    enable_debug: true,
    timeout_scale: 1.0,
    instances_per_team: 1,
    comm_bytes: 0,
    swarm_gems: false,
    swarm_gem_chance: 0.0,
    swarm_gem_ttl: nil,
    swarm_node_count: 3,
    swarm_required_nodes: 2,
    swarm_node_distance: 5,
    swarm_score_two_nodes: 3.0,
    swarm_score_three_nodes: 5.0,
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
    opts.on("--max-antennas N", Integer, "Max. number of antennas per bot (default: #{options[:max_antennas]})") do |x|
        options[:max_antennas] = x
    end
    opts.on("--max-portals N", Integer, "Max. number of portals per bot (default: #{options[:max_portals]})") do |x|
        options[:max_portals] = x
    end
    opts.on("--instances-per-team N", Integer, "Number of spawned copies per submitted bot (default: #{options[:instances_per_team]})") do |x|
        options[:instances_per_team] = [x, 1].max
    end
    opts.on("--comm-bytes N", Integer, "Team communication buffer size in bytes (default: #{options[:comm_bytes]})") do |x|
        options[:comm_bytes] = [x, 0].max
    end
    opts.on("--[no-]swarm-gems", "Enable swarm gems (default: #{options[:swarm_gems]})") do |x|
        options[:swarm_gems] = x
    end
    opts.on("--swarm-gem-chance N", Float, "Chance that a spawned gem becomes a swarm gem (default: #{options[:swarm_gem_chance]})") do |x|
        options[:swarm_gem_chance] = [[x, 0.0].max, 1.0].min
    end
    opts.on("--swarm-gem-ttl N", Integer, "TTL for swarm gems; defaults to gem_ttl when omitted") do |x|
        options[:swarm_gem_ttl] = [x, 1].max
    end
    opts.on("--swarm-node-count N", Integer, "Number of resonance nodes per swarm gem (default: #{options[:swarm_node_count]})") do |x|
        options[:swarm_node_count] = [x, 0].max
    end
    opts.on("--swarm-required-nodes N", Integer, "Required occupied resonance nodes (default: #{options[:swarm_required_nodes]})") do |x|
        options[:swarm_required_nodes] = [x, 0].max
    end
    opts.on("--swarm-node-distance N", Integer, "Preferred BFS distance of resonance nodes from the swarm gem (default: #{options[:swarm_node_distance]})") do |x|
        options[:swarm_node_distance] = [x, 1].max
    end
    opts.on("--swarm-score-two-nodes N", Float, "Score multiplier when the required nodes are occupied (default: #{options[:swarm_score_two_nodes]})") do |x|
        options[:swarm_score_two_nodes] = [x, 1.0].max
    end
    opts.on("--swarm-score-three-nodes N", Float, "Score multiplier when all nodes are occupied (default: #{options[:swarm_score_three_nodes]})") do |x|
        options[:swarm_score_three_nodes] = [x, 1.0].max
    end
    opts.on("-e", "--[no-]emit-signals", "Enable gem signals (default: #{options[:emit_signals]})") do |x|
        options[:emit_signals] = x
    end
    opts.on("--[no-]emit-signal-channels", "Enable gem signal channels (default: #{options[:emit_signal_channels]})") do |x|
        options[:emit_signal_channels] = x
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
    opts.on("--[no-]write-highlights", "Write highlight overlays to ANSI log (default: #{options[:write_highlights]})") do |x|
        options[:write_highlights] = x
    end
    opts.on("--[no-]write-stdin", "Write bot stdin data to ANSI log (default: #{options[:write_stdin]})") do |x|
        options[:write_stdin] = x
    end
    opts.on("--[no-]show-timings", "Show timings after run (default: #{options[:show_timings]})") do |x|
        options[:show_timings] = x
    end
    opts.on("--[no-]start-paused", "Start the runner in paused mode (default: #{options[:start_paused]})") do |x|
        options[:start_paused] = x
    end
    opts.on("--[no-]contest-mode", "Enable contest mode (disables bot debugging, default: #{options[:contest_mode]})") do |x|
        options[:contest_mode] = x
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
    positional = [File.expand_path("random-walker", __dir__)]
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
    bot_paths << File.expand_path("random-walker", __dir__)
end

if bot_paths.size > 2
    STDERR.puts "Error: At most two bots can compete."
    exit 1
end


if options[:check_determinism]
    round_seed = Digest::SHA256.digest("#{options[:seed]}/check-determinism").unpack1('L<')
    seed_rng = PCG32.new(round_seed)
    seed = seed_rng.randrange(2 ** 48)
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
            runner.add_team(path, 0, bot_args[i] || [])
            results, events = runner.run
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
    runner.team_count = bot_paths.size
    runner.setup
    bot_paths.each.with_index { |path, i| runner.add_team(path, i, bot_args[i] || []) }
    results, events = runner.run

    if write_profile_json_path
        all_reports = []

        (0...bot_paths.size).each do |i|
            bot = runner.team_representative(i)
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
            begin
                report[:git_hash] = `git -C "#{__dir__}" describe --always --dirty`.strip
            rescue
            end
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
            f.write(sanitize_utf8_values(all_reports).to_json)
        end

        events_path = write_profile_json_path.sub(/\.json\z/, "-events.json.gz")
        FileUtils.mkdir_p(File.dirname(events_path))
        Zlib::GzipWriter.open(events_path) do |gz|
            gz.write({ events: [events] }.to_json)
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
    all_events = []

    bot_data = []

    options[:rounds].times do |i|
        if options[:round_seeds]
            options[:seed] = options[:round_seeds][i].to_i(36)
        else
            options[:seed] = seed_rng.randrange(2 ** 48)
        end
        all_seed << options[:seed]
        runner = Runner.new(**options)
        runner.round = i
        runner.stage_title = stage_title if stage_title
        runner.stage_key = stage_key if stage_key
        runner.team_count = bot_paths.size
        runner.setup
        bot_paths.each.with_index { |path, i| runner.add_team(path, i, bot_args[i] || []) }
        if i == 0
            (0...bot_paths.size).each do |team|
                bot = runner.team_representative(team)
                bot_data << {:name => bot[:name], :emoji => bot[:emoji]}
            end
        end
        all_options << runner.options_hash()
        results, events = runner.run
        all_events << events
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

    puts "Ran on seed #{og_seed.to_s(36)} with #{options[:rounds]} rounds."

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
        begin
            report[:git_hash] = `git -C "#{__dir__}" describe --always --dirty`
        rescue
        end
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
        events_path = write_profile_json_path.sub(/\.json\z/, "-events.json.gz")
        FileUtils.mkdir_p(File.dirname(events_path))
        Zlib::GzipWriter.open(events_path) do |gz|
            gz.write({ events: sanitize_utf8_values(all_events) }.to_json)
        end
    end
end

if options[:show_timings]
    $timings.report
end